# ===========================================================================
# NEON My Little Inverts — server.R
# v2 flow. The picker map is the primary selector; the by-name panel + browse
# list are fallbacks. ONE shared load_site() serves all three, and a pendingSite
# bridge keeps the sidebar dropdowns in sync with what the map loaded (the
# site-picker map contract). All metrics are read from the precomputed bundle.
# Density is a within-site standardized index; richness/Chao1 are grayed where
# small_n. No biotic-index / pass-fail anywhere.
# ===========================================================================
server <- function(input, output, session) {
  is_dark <- function() identical(input$colorMode, "dark")
  plotly_theme <- function(p, legend = TRUE) {
    dark <- is_dark(); ink <- if (dark) "#e4f6f7" else "#102a33"
    grid <- if (dark) "rgba(228,246,247,0.09)" else "rgba(16,42,51,0.07)"; zero <- if (dark) "rgba(228,246,247,0.20)" else "rgba(16,42,51,0.14)"
    lin <- if (dark) "#1f4248" else "#cfe4e6"; legc <- if (dark) "#b9dde0" else "#274a54"
    p %>% plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = ink, family = "Rubik"),
      xaxis = list(gridcolor = grid, zerolinecolor = zero, linecolor = lin),
      yaxis = list(gridcolor = grid, zerolinecolor = zero, linecolor = lin),
      legend = list(bgcolor = "rgba(0,0,0,0)", orientation = "h", y = -0.2, font = list(color = legc)),
      margin = list(l = 55, r = 30, t = 48, b = 44),
      hoverlabel = list(bgcolor = if (dark) "rgba(11,42,48,0.97)" else "rgba(16,42,51,0.95)", bordercolor = "#2bb7c4", font = list(color = "#fff", family = "Rubik", size = 13))) %>%
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  }
  note_plot <- function(msg, icon = "\U0001F990") plotly::plot_ly(type="scatter", mode="markers") %>%
    plotly::layout(paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)", xaxis=list(visible=FALSE), yaxis=list(visible=FALSE),
      annotations=list(list(text=paste0(icon,"<br>",msg), showarrow=FALSE, font=list(color=if(is_dark())"#8db4ba" else "#5d7c84", size=15), align="center"))) %>%
    plotly::config(displayModeBar = FALSE)

  rv <- reactiveValues(bundle=NULL, meta=NULL, bouts=NULL, taxa=NULL, samples=NULL,
                       label=NULL, site=NULL, sp=NULL, pendingSite=NULL, reach=NULL)

  # session-scoped delighter state (no racing observers; each fires at most once)
  celebrated  <- reactiveVal(FALSE)   # the EPT confetti fires AT MOST ONCE per session
  carryTab    <- reactiveVal(NULL)    # the tab to restore after a site change
  recentCodes <- reactiveVal(character(0))  # the recents ring, newest first (from localStorage)

  # ---- selectors + the sidebar-sync bridge --------------------------------
  observe({ ch <- inv_state_choices(); updateSelectInput(session, "stateSel", choices = ch, selected = if ("AZ" %in% ch) "AZ" else NULL) })
  observeEvent(input$stateSel, {
    sites <- inv_sites_in_state(input$stateSel)
    # honour a pending map pick: keep the dropdown ON the loaded site, not sites[[1]]
    sel <- if (!is.null(rv$pendingSite) && rv$pendingSite %in% sites) rv$pendingSite else if (length(sites)) sites[[1]] else NULL
    rv$pendingSite <- NULL
    updateSelectInput(session, "site", choices = sites, selected = sel)
  }, ignoreInit = FALSE)
  output$siteBio <- renderUI({ req(input$site); b <- site_bio(input$site); if (is.null(b)) return(NULL); div(class="site-bio", bs_icon("info-circle-fill"), span(b)) })

  output$siteCards <- renderUI({
    if (is.null(SITE_INDEX) || !nrow(site_table)) return(NULL)
    div(class="site-cards", lapply(seq_len(nrow(site_table)), function(i){ r <- site_table[i,]
      tags$a(class="site-card", href="#",
        onclick=sprintf("smtLoadStart('%s · loading…');Shiny.setInputValue('siteExplore','%s',{priority:'event'});return false;", gsub("'","",r$name), r$site),
        div(class="sc-emoji","\U0001F990"),
        div(class="sc-body", div(class="sc-name", tags$b(r$site), sprintf(" · %s", r$name)),
          div(class="sc-meta", sprintf("%s · %s · %s taxa · %s EPT taxa", r$state, TYPE_LAB[r$type] %||% r$type, r$richness %||% "—", r$ept_richness %||% "—")))) }))
  })
  shinyjs::hide("mainTabsWrap")

  # ---- ingest a bundle into the reactive state ----------------------------
  ingest <- function(b, label) {
    if (is.null(b) || is.null(b$bouts) || !nrow(b$bouts)) {
      session$sendCustomMessage("loadDone", list())
      showNotification(HTML("That site isn't bundled. Run <code>Rscript scripts/build_inv_data.R</code> to populate <code>data/</code>."), type = "error", duration = 12)
      return(invisible())
    }
    b$meta$name <- b$meta$name %||% site_name(b$meta$site)   # fill the NA name from metadata
    rv$bundle <- b; rv$meta <- b$meta; rv$bouts <- b$bouts; rv$taxa <- taxa_board(b$taxa)
    rv$samples <- b$samples; rv$label <- label; rv$site <- b$meta$site; rv$sp <- NULL; rv$reach <- NULL
    shinyjs::show("mainTabsWrap"); shinyjs::show("spPickerWrap"); shinyjs::hide("splash")
    tb <- rv$taxa
    ch <- setNames(tb$scientificName, sprintf("%s · %s", tb$scientificName, ifelse(tb$class=="EPT","EPT", tb$order %||% "other")))
    updateSelectizeInput(session, "spSel", choices = c("Pick a taxon…"="", ch), selected = "", server = TRUE)
    # CARRY STATE across a site change: keep the user on the tab they were on when
    # the new site supports it (all sites share the same tab set, so it always
    # does). carryTab() is set by the "change site" handler before teardown; the
    # cold/deep-link/restore paths leave it NULL and we land on Overview.
    land <- carryTab(); if (is.null(land) || !nzchar(land)) land <- "overview"
    carryTab(NULL)
    nav_select("tabs", land)
    # DEEP-LINK + RESTORE persistence — fire ONLY here, on a successful load (never
    # on a reactive tick). The address bar becomes a shareable ?site= link; the
    # localStorage handler writes the resume target + the recents ring in one place.
    updateQueryString(paste0("?site=", utils::URLencode(rv$site, reserved = TRUE)), mode = "replace")
    session$sendCustomMessage("invSaveSite", list(site = rv$site))
    # CELEBRATION — leashed + honesty-gated. Fire AT MOST ONCE per session, ONLY
    # when the loaded site is a STREAM/RIVER (lakes are EPT-poor by nature, so
    # celebrating low/zero EPT there is dishonest) AND its %EPT clears the network
    # top-quartile bar. EPT_CELEBRATE_THRESHOLD is the 75th-percentile-of-
    # stream/river pct_ept_ind from the bundled site_index (34.8% rounded to 34;
    # this captures 8 of 27 stream/river sites ≈ the top quartile). Reduced-motion
    # is already honored inside rodentConfetti().
    if (!isTRUE(celebrated())) {
      typ <- b$meta$aquaticSiteType %||% NA_character_
      ept <- suppressWarnings(as.numeric(b$meta$pct_ept_ind %||% NA))
      if (!is.na(typ) && typ %in% c("stream", "river") &&
          is.finite(ept) && ept >= EPT_CELEBRATE_THRESHOLD) {
        celebrated(TRUE)
        session$sendCustomMessage("confetti", list(big = ept >= 40))  # extra burst for a standout
      }
    }
    session$sendCustomMessage("countUp", list()); session$sendCustomMessage("loadDone", list())
    invisible(TRUE)
  }

  # ONE shared loader used by the map Explore, the by-name Load, and the browse list
  load_site <- function(site){ if (is.null(site)||site=="") { session$sendCustomMessage("loadDone", list()); return() }
    b <- load_site_bundle(site); if (is.null(b)) { session$sendCustomMessage("loadDone", list()); showNotification("That site isn't bundled.", type="error"); return() }
    row <- site_table[site_table$site==site,]
    # keep the by-name dropdowns in sync with the loaded site (the contract):
    # set pendingSite, then cascade the state selector so its observer honours it.
    if (nrow(row) && !is.na(row$state) && !identical(input$stateSel, row$state)) {
      rv$pendingSite <- site; updateSelectInput(session, "stateSel", selected = row$state)
    } else if (nrow(row)) {
      updateSelectInput(session, "site", selected = site)
    }
    ingest(b, sprintf("%s · %s", site, if (nrow(row)) row$name else site)) }

  observeEvent(input$loadBtn, load_site(input$site))
  observeEvent(input$siteExplore, load_site(input$siteExplore))   # map "Explore" + browse cards
  # pincards.js routes a cross-site "Open this site" chip to input$pickSite — alias it
  # to the same shared loader (it raises the overlay client-side before firing).
  observeEvent(input$pickSite, load_site(input$pickSite))
  # "About this site" -> instant modal, no load
  observeEvent(input$siteInfo, {
    code <- input$siteInfo; row <- site_table[site_table$site == code, ]
    if (!nrow(row)) return()
    si <- SITE_INDEX[SITE_INDEX$site == code, ]
    showModal(modalDialog(easyClose = TRUE, title = tagList(bs_icon("water"), sprintf(" %s · %s", code, row$name)),
      tags$p(class = "site-bio", bs_icon("info-circle-fill"), span(site_bio(code) %||% "")),
      tags$ul(
        tags$li(HTML(sprintf("<b>%s</b> · NEON %s · %s", TYPE_LAB[row$type] %||% row$type, row$domain, row$state))),
        tags$li(HTML(sprintf("<b>%s</b> taxa caught · <b>%s</b> EPT taxa · <b>%.0f%%</b> EPT individuals", si$richness %||% "—", si$ept_richness %||% "—", si$pct_ept_ind %||% NA))),
        tags$li(HTML(sprintf("<b>%s</b> bouts over <b>%s</b>", si$n_bouts %||% "—", if (!is.na(si$year_min)) sprintf("%d–%d", si$year_min, si$year_max) else "—")))),
      footer = tagList(
        actionButton("siteInfoExplore", tagList(bs_icon("water"), " Explore this site"), class = "btn-primary",
                     onclick = sprintf("smtLoadStart('%s · loading…');", gsub("'","",row$name))),
        modalButton("Close"))))
  })
  observeEvent(input$siteInfoExplore, { removeModal(); load_site(input$siteInfo) })

  # "Change site" -> back to the picker map
  observeEvent(input$changeSite, {
    carryTab(input$tabs)   # remember the active tab so the next load lands back on it
    rv$bundle <- NULL; rv$meta <- NULL; rv$bouts <- NULL; rv$taxa <- NULL; rv$samples <- NULL
    rv$label <- NULL; rv$site <- NULL; rv$sp <- NULL; rv$reach <- NULL
    shinyjs::hide("mainTabsWrap"); shinyjs::hide("spPickerWrap"); shinyjs::show("splash")
    updateQueryString("?", mode = "replace")   # don't carry a stale deep link onto the splash
    session$sendCustomMessage("kickMaps", list())
  })

  # ---- delighters: deep-link + restore startup resolver -------------------
  # Fires ONCE on connect (input$invLastSite is set in the shiny:connected JS, by
  # which point clientData$url_search is ready). STRICT PRECEDENCE in ONE if/else:
  #   (A) URL ?site=CODE  -> route a valid code through load_site()
  #   (B) else localStorage last-site -> route it through load_site()
  #   (C) else stay on the splash
  # Restore is NON-STICKY: "change site" still re-shows the picker, so the splash
  # is always one tap away.
  valid_site <- function(code) !is.null(code) && length(code) == 1 &&
    nzchar(code) && code %in% site_table$site
  observeEvent(input$invLastSite, once = TRUE, ignoreNULL = FALSE, {
    q <- tryCatch(parseQueryString(session$clientData$url_search %||% ""),
                  error = function(e) list())
    target <- NULL
    if (valid_site(q$site)) {
      target <- q$site                          # (A) URL deep link wins
    } else {
      raw <- input$invLastSite                  # (B) else localStorage resume
      if (valid_site(raw)) target <- raw
    }
    if (is.null(target)) return(invisible())    # (C) nothing valid -> splash stays
    session$sendCustomMessage("smtLoadStart", list(label = paste0(target, " · loading…")))
    load_site(target)
  })

  # ---- delighters: recents strip ------------------------------------------
  # The recents ring arrives as a comma-joined code string from localStorage
  # (read on connect, and re-pushed by the invSaveSite handler after each load).
  # Keep only codes that are actually bundled; render zero-effort tap-chips.
  observeEvent(input$invRecents, ignoreNULL = FALSE, {
    raw <- input$invRecents %||% ""
    codes <- trimws(unlist(strsplit(raw, ",", fixed = TRUE)))
    codes <- codes[nzchar(codes) & codes %in% site_table$site]
    recentCodes(unique(codes)[seq_len(min(4L, length(unique(codes))))])
  })
  output$recentsStrip <- renderUI({
    codes <- recentCodes()
    if (!length(codes)) return(NULL)
    # zero-effort tap-chips. Each raises the overlay client-side then fires ONE
    # shared input (recentPick) — the same single-input mechanism the map popup
    # uses — so there is no per-code observer to stack as the ring changes.
    div(class = "recents-strip",
      tags$span(class = "recents-lab", bs_icon("clock-history"), " Recently viewed:"),
      lapply(codes, function(code) {
        row <- site_table[site_table$site == code, ]
        nm  <- if (nrow(row)) gsub("'", "", row$name) else code
        tags$a(href = "#", class = "recent-chip",
          title = if (nrow(row)) row$name else code,
          onclick = sprintf("smtLoadStart('%s · loading…');Shiny.setInputValue('recentPick','%s',{priority:'event'});return false;", nm, code),
          code)
      }))
  })
  observeEvent(input$recentPick, load_site(input$recentPick))   # routes through the SAME shared loader

  # ---- taxon selection ----------------------------------------------------
  pick_taxon <- function(sci, navigate=FALSE){ if (is.null(sci)||is.na(sci)||sci=="") return()
    if (is.null(rv$taxa) || !(sci %in% rv$taxa$scientificName)) return()
    rv$sp <- sci; if (!identical(input$spSel, sci)) updateSelectizeInput(session, "spSel", selected=sci); if (navigate) nav_select("tabs","species") }
  observeEvent(input$spSel, if (nzchar(input$spSel %||% "")) pick_taxon(input$spSel, navigate=TRUE), ignoreInit=TRUE)
  observeEvent(input$qcCardRequest, if (nzchar(input$qcCardRequest %||% "")) pick_taxon(input$qcCardRequest, navigate=TRUE), ignoreInit=TRUE)
  observeEvent(input$surpriseBtn, { req(rv$taxa); pick_taxon(sample(rv$taxa$scientificName, 1), navigate=TRUE) })
  observeEvent(input$goPulse, nav_select("tabs","pulse")); observeEvent(input$goBoard, nav_select("tabs","board"))
  observeEvent(input$goDiversity, nav_select("tabs","diversity")); observeEvent(input$goCross, nav_select("tabs","cross"))
  observeEvent(input$goMap, nav_select("tabs","map"))
  observeEvent(input$goSearch, nav_select("tabs","search"))
  # a cross-site dot's "Open this site" chip routes through siteExplore too
  observeEvent(input$goSpFromCard, nav_select("tabs","species"))

  # ---- Search the network -------------------------------------------------
  # Queries the bundled SEARCH_INDEX (loaded once at boot) in memory. The
  # "Go to this site" buttons reuse the SAME load path as the map / browse list:
  # smtLoadStart() raises the overlay client-side, then siteExplore loads the
  # bundle (instant) and lands on the Overview. A small DT button helper:
  go_btn <- function(code) sprintf(
    "<button class='dt-go-btn' onclick=\"smtLoadStart('%s · loading…');Shiny.setInputValue('siteExplore','%s',{priority:'event'});return false;\">Go to site &rarr;</button>",
    code, code)

  # populate the autocomplete from the index (server-side for the 1,500+ taxa)
  observe({
    ch <- search_taxon_choices()
    updateSelectizeInput(session, "searchTaxon",
      choices = c("Type a taxon name…" = "", ch), selected = "", server = TRUE)
  })

  # -- MODE A: find a taxon -> every site where it occurs --------------------
  taxon_hits <- reactive({
    sci <- input$searchTaxon %||% ""; if (!nzchar(sci) || is.null(SEARCH_TAXA)) return(NULL)
    h <- SEARCH_TAXA[SEARCH_TAXA$scientificName == sci, , drop = FALSE]
    if (!nrow(h)) return(h)
    nm <- site_name_vec(h$site)
    data.frame(
      Site = h$site,
      Name = nm,
      `EPT` = ifelse(h$is_ept, "EPT", "—"),
      `Mean density (ind/m2)` = round(h$mean_density, 1),
      `Ubiquity (% samples)` = round(h$ubiquity),
      `Years` = ifelse(is.na(h$year_min), "—", paste0(h$year_min, "–", h$year_max)),
      Open = vapply(h$site, go_btn, character(1)),
      check.names = FALSE, stringsAsFactors = FALSE)[order(-h$mean_density), ]
  })

  output$searchTaxonCaption <- renderUI({
    sci <- input$searchTaxon %||% ""
    if (!nzchar(sci)) return(div(class = "search-empty", bs_icon("search"),
      " Pick a taxon above to see every site it was found at."))
    h <- taxon_hits(); n <- if (is.null(h)) 0 else nrow(h)
    ept <- !is.null(SEARCH_TAXA) && any(SEARCH_TAXA$scientificName == sci & SEARCH_TAXA$is_ept)
    if (n == 0) return(div(class = "search-empty", bs_icon("emoji-frown"),
      sprintf(" No sites in the index list %s.", sci)))
    tagList(
      div(class = "search-count",
        tags$b(sprintf("%s", sci)), if (ept) glow_badge("EPT", "#0e8f9c"),
        sprintf(" found at %d of %d sites", n, length(BUNDLED))),
      div(class = "search-note", bs_icon("info-circle"),
        " Mean density is a within-site index (individuals per m", tags$sup("2"), "), not an absolute ranking. Ubiquity is the share of that site's samples the taxon shows up on."))
  })

  output$searchTaxonTbl <- DT::renderDT({
    h <- taxon_hits(); validate(need(!is.null(h) && nrow(h) > 0, ""))
    DT::datatable(h, rownames = FALSE, escape = FALSE, selection = "none",
      options = list(pageLength = 12, dom = "tip", order = list(),
        columnDefs = list(list(orderable = FALSE, targets = ncol(h) - 1))),
      class = "compact stripe hover")
  })

  # -- MODE B: threshold query -> the sites that clear it --------------------
  thresh_hits <- reactive({
    if (is.null(SEARCH_SITES)) return(NULL)
    metric <- input$threshMetric %||% "ept_richness"; v <- suppressWarnings(as.numeric(input$threshValue))
    if (is.na(v)) v <- 0
    s <- SEARCH_SITES; val <- s[[metric]]
    keep <- !is.na(val) & val > v
    s <- s[keep, , drop = FALSE]; val <- val[keep]
    if (!nrow(s)) return(s)
    metlab <- if (metric == "pct_ept_ind") "%EPT (individuals)" else "EPT richness"
    out <- data.frame(
      Site = s$site,
      Name = site_name_vec(s$site),
      Type = TYPE_LAB[s$aquaticSiteType] %||% s$aquaticSiteType,
      `Metric` = if (metric == "pct_ept_ind") sprintf("%.1f%%", val) else as.character(round(val)),
      `EPT richness` = s$ept_richness,
      `%EPT` = round(s$pct_ept_ind, 1),
      `Richness` = s$richness,
      `Bouts` = s$n_bouts,
      Open = vapply(s$site, go_btn, character(1)),
      check.names = FALSE, stringsAsFactors = FALSE)
    names(out)[names(out) == "Metric"] <- metlab
    attr(out, "metlab") <- metlab
    out[order(-val), ]
  })

  output$searchThreshCaption <- renderUI({
    metric <- input$threshMetric %||% "ept_richness"; v <- suppressWarnings(as.numeric(input$threshValue))
    if (is.na(v)) v <- 0
    h <- thresh_hits(); n <- if (is.null(h)) 0 else nrow(h)
    lab <- if (metric == "pct_ept_ind") sprintf("%%EPT greater than %g%%", v) else sprintf("EPT richness greater than %g", v)
    if (n == 0) return(div(class = "search-empty", bs_icon("emoji-frown"),
      sprintf(" No sites clear %s.", lab)))
    tagList(
      div(class = "search-count",
        sprintf("%d of %d sites have ", n, length(BUNDLED)), tags$b(lab)),
      div(class = "search-note", bs_icon("info-circle"),
        " Space-for-time across different waters, confounded by water type and habitat. Lakes are naturally EPT-poor, a low value is the ecosystem, not impairment."))
  })

  output$searchThreshTbl <- DT::renderDT({
    h <- thresh_hits(); validate(need(!is.null(h) && nrow(h) > 0, ""))
    DT::datatable(h, rownames = FALSE, escape = FALSE, selection = "none",
      options = list(pageLength = 12, dom = "tip", order = list(),
        columnDefs = list(list(orderable = FALSE, targets = ncol(h) - 1))),
      class = "compact stripe hover")
  })

  # ---- hero band ----------------------------------------------------------
  output$heroStats <- renderUI({
    sv <- site_vectors(rv$meta); if (is.null(sv)) return(NULL)
    hero <- function(v,l,suf="",icon,tone,info=NULL,gray=FALSE) div(class=paste0("hero-stat hero-",tone, if (gray) " hero-grayed" else ""),
      div(class="hs-icon", bs_icon(icon)),
      div(if (gray) div(class="hs-v", "n/a") else div(class="hs-v count-up", `data-target`=v, `data-suffix`=suf, "0"),
          div(class="hs-l", l, if (!is.null(info)) info)))
    rar_gray <- sv$small_n || is.na(sv$rarefied)
    div(class="hero-band",
      div(class="hero-title", bs_icon("water"), tags$b(rv$label),
        actionLink("changeSite", tagList(bs_icon("arrow-left-circle"), " change site"), class = "hero-change"),
        downloadLink("reportPdf", tagList(bs_icon("file-earmark-pdf"), " report (PDF)"), class = "hero-report"),
        downloadLink("reportCsv", tagList(bs_icon("file-earmark-spreadsheet"), " data (CSV)"), class = "hero-report")),
      div(class="hero-grid",
        hero(sv$richness, "taxa", icon="bug-fill", tone="navy",
          info=info_pop("Taxa", p("The number of distinct ", tags$b("taxa"), " found here across all bouts. Benthic sampling misses rare taxa, so the true total is higher (see the Chao1 estimate)."))),
        hero(sv$ept_richness, "EPT taxa", icon="award", tone="terra",
          info=info_pop("EPT richness", p("Number of ", tags$b("mayfly, stonefly, and caddisfly"), " taxa, the pollution-sensitive groups. Lakes are naturally EPT-poor, so low EPT in a lake is normal, not impairment."))),
        hero(sv$pct_ept_ind, "% EPT", suf="%", icon="droplet-half", tone="pine",
          info=info_pop("EPT share", p("Share of estimated individuals that are EPT (mayfly / stonefly / caddisfly). A descriptive clean-water signal, ", tags$b("not"), " a pass/fail score. Beside it, ", tags$b(sprintf("%.0f%%", sv$pct_ept_taxa %||% NA)), " of the taxa are EPT."))),
        hero(if (rar_gray) NA else sv$rarefied, "rarefied richness", icon="bar-chart-steps", tone="gold", gray=rar_gray,
          info=info_pop("Standardized richness", p("Richness rarefied to 100 individuals (Hurlbert 1971), comparable across effort. ", if (rar_gray) tags$b("Suppressed here: insufficient count for standardized richness.") else "")))))
  })

  # ---- Overview: density board (top taxa) ---------------------------------
  output$topBar <- renderPlotly({
    tb <- rv$taxa; req(tb); tb <- head(tb[order(-tb$mean_density),], 16)
    tb$lab <- factor(tb$scientificName, levels = rev(tb$scientificName))
    plot_ly(tb, x=~mean_density, y=~lab, type="bar", orientation="h", marker=list(color=ept_col(tb$class)),
      text=~ifelse(class=="EPT","EPT","other"), customdata=~ubiquity,
      hovertemplate="%{y}<br>%{x:.1f} /m² · on %{customdata}% of samples · %{text}<extra></extra>") %>%
      plotly_theme(legend=FALSE) %>% plotly::layout(showlegend=FALSE, xaxis=list(title="Mean density (individuals / m², log)", type="log"), yaxis=list(title=""), margin=list(l=190, t=34),
        annotations=list(list(text=sprintf("at <b>%s</b> · this site only · colour = EPT vs other · log scale", rv$site %||% "this site"), x=0, y=1.07, xref="paper", yref="paper", showarrow=FALSE, xanchor="left", font=list(color=if(is_dark())"#8db4ba" else "#5d7c84", size=11))))
  })
  output$overviewInsight <- renderUI({
    tb <- rv$taxa; req(tb); top <- tb[which.max(tb$mean_density),]; ubi <- tb[which.max(tb$ubiquity),]
    n_ept <- sum(tb$class == "EPT")
    insight_banner("droplet-half", tone="navy", HTML(sprintf("<b><i>%s</i></b> is the densest taxon here (%.0f /m²); <b><i>%s</i></b> is the most widespread (on %.0f%% of samples). The site holds <span class='ci-hero'>%d</span> taxa, <b>%d</b> of them EPT.",
      top$scientificName, top$mean_density, ubi$scientificName, ubi$ubiquity, nrow(tb), n_ept)))
  })
  output$siteInsights <- renderUI({
    sv <- site_vectors(rv$meta); req(sv); tb <- rv$taxa; meta <- rv$meta
    yr <- year_label(meta)
    pts <- c(
      sprintf("Over <b>%s</b>, NEON ran <b>%s</b> collection bouts (<b>%s</b> samples) at this %s and found <b>%d</b> taxa, an estimated <b>%s</b> individuals.",
        yr %||% "its record", fmt_int(sv$n_bouts), fmt_int(sv$n_samples), TYPE_LAB[sv$type] %||% sv$type %||% "site", sv$richness, fmt_int(sv$total_ind)),
      sprintf("EPT (mayflies, stoneflies, caddisflies) make up <b>%.0f%%</b> of individuals and <b>%.0f%%</b> of taxa (<b>%d</b> EPT taxa). %s",
        sv$pct_ept_ind, sv$pct_ept_taxa, sv$ept_richness,
        if (identical(sv$type, "lake")) "This is a lake, which is naturally EPT-poor, so read low EPT as the ecosystem, not impairment." else "Higher EPT generally tracks cleaner, cooler, better-oxygenated water, within this one site."),
      sprintf("The densest taxon is <b><i>%s</i></b>. Midges (Chironomidae) are <b>%.0f%%</b> and worms (Oligochaeta) <b>%.0f%%</b> of individuals, the more tolerant groups.",
        sv$top_taxon, sv$pct_chiro, sv$pct_oligo))
    if (!sv$small_n && !is.na(sv$chao1))
      pts <- c(pts, sprintf("Sampling found <b>%d</b> taxa; <b>Chao1</b> estimates at least <b>%.0f</b> (±%.0f) really use the site, so roughly <b>%.0f</b> remain undetected.",
        sv$richness, sv$chao1, sv$chao1_se %||% 0, max(0, round(sv$chao1 - sv$richness))))
    else
      pts <- c(pts, "Standardized richness (rarefied / Chao1) is suppressed at this site because the count is too small to estimate it honestly.")
    pts <- c(pts, "Remember: density is a <b>within-site standardized index</b>, not a population, and these are descriptive metrics, never a pass/fail score. Open any taxon's profile for its data-quality flags.")
    tags$ul(class="insight-list", lapply(pts, function(t) tags$li(HTML(t))))
  })

  # ---- The EPT Pulse (signature) ------------------------------------------
  output$pulsePlot <- renderPlotly({
    bs <- bout_series(rv$bouts); req(bs); if (nrow(bs) < 1) return(note_plot("No bouts to draw"))
    muted <- if (is_dark()) "#8db4ba" else "#5d7c84"
    # marker symbol = habitat; colour = sampler type; greyed where flagged
    habs <- sort(unique(bs$habitatType)); symset <- c("circle","square","diamond","triangle-up","cross","star","pentagon","hexagon")
    bs$sym <- symset[(match(bs$habitatType, habs) - 1) %% length(symset) + 1]
    samps <- sort(unique(bs$samplerType)); palset <- c("#0e8f9c","#2f7daa","#e0a13b","#b06a4a","#5a8f3e","#9c5d18")
    bs$scol <- palset[(match(bs$samplerType, samps) - 1) %% length(palset) + 1]
    bs$scol[bs$flagged] <- "rgba(148,167,173,0.45)"
    bs$ept <- num(bs$pct_ept_ind); bs$dens <- num(bs$density_m2)
    bs$card <- inv_bout_card(format(bs$collectDate, "%b %Y"), format(bs$collectDate, "%Y-%m-%d"),
      sprintf("EPT <b>%.0f%%</b> · density <b>%s</b>/m²<br/>%s · %s sampler · %d samples%s",
        bs$ept, ifelse(is.na(bs$dens), "—", format(round(bs$dens), big.mark = ",")),
        bs$habitatType, bs$samplerType, bs$n_samples, ifelse(bs$flagged, " · flagged", "")))
    p <- plot_ly()
    # density bars (secondary axis), faint
    p <- p %>% add_trace(x=~bs$collectDate, y=~bs$dens, type="bar", name="density (/m²)", yaxis="y2",
      marker=list(color=if (is_dark()) "rgba(95,208,218,0.20)" else "rgba(14,143,156,0.14)"),
      hovertemplate="%{x|%b %Y}<br>%{y:.0f} /m²<extra></extra>")
    # %EPT line + per-bout markers
    p <- p %>% add_trace(x=~bs$collectDate, y=~bs$ept, type="scatter", mode="lines", name="%EPT",
      line=list(color=DDL$teal, width=2.5), hoverinfo="skip", showlegend=TRUE)
    for (i in seq_len(nrow(bs))) {
      p <- p %>% add_trace(x=bs$collectDate[i], y=bs$ept[i], type="scatter", mode="markers", showlegend=FALSE,
        customdata=list(bs$card[i]),
        marker=list(symbol=bs$sym[i], color=bs$scol[i], size=12, line=list(color="#fff", width=1)),
        hovertemplate=sprintf("%s · %s<br>%%EPT %.1f%% · %.0f /m²<br>%s sampler · %d samples%s<extra></extra>",
          format(bs$collectDate[i], "%b %Y"), bs$habitatType[i], bs$ept[i], bs$dens[i], bs$samplerType[i], bs$n_samples[i],
          if (bs$flagged[i]) " · flagged" else ""))
    }
    # legend KEY proxies (one NA point each, nothing plots) so a screenshot
    # self-decodes the double encoding: marker SHAPE = habitat, COLOUR = sampler.
    keymut <- "#9aa6aa"; hi <- 0L
    for (h in habs) { hi <- hi + 1L
      p <- p %>% add_trace(x=as.Date(NA), y=NA, type="scatter", mode="markers", name=h, legendgroup="hab",
        legendgrouptitle=list(text="Habitat (shape)"), hoverinfo="skip", showlegend=TRUE,
        marker=list(symbol=symset[(hi - 1) %% length(symset) + 1], color=keymut, size=11, line=list(color="#fff", width=1))) }
    si <- 0L
    for (s in samps) { si <- si + 1L
      p <- p %>% add_trace(x=as.Date(NA), y=NA, type="scatter", mode="markers", name=s, legendgroup="samp",
        legendgrouptitle=list(text="Sampler (colour)"), hoverinfo="skip", showlegend=TRUE,
        marker=list(symbol="circle", color=palset[(si - 1) %% length(palset) + 1], size=11, line=list(color="#fff", width=1))) }
    p %>% plotly_theme() %>% plotly::layout(
      xaxis=list(title="Collection bout"), yaxis=list(title="% EPT (individuals)", rangemode="tozero", ticksuffix="%"),
      yaxis2=list(title="density (/m²)", overlaying="y", side="right", rangemode="tozero", showgrid=FALSE),
      margin=list(l=56, r=70, t=44, b=78), legend=list(y=-0.26, font=list(size=10.5)),
      annotations=list(list(text=sprintf("at <b>%s</b> · greyed = flagged bout · habitat + sampler key below", rv$site %||% "this site"), x=0, y=1.1, xref="paper", yref="paper", showarrow=FALSE, xanchor="left", font=list(color=muted, size=11))))
  })
  output$pulseInsight <- renderUI({
    bs <- bout_series(rv$bouts); req(bs)
    ept <- num(bs$pct_ept_ind); ept <- ept[is.finite(ept)]
    if (!length(ept)) return(NULL)
    trend <- if (length(ept) >= 6) { fit <- suppressWarnings(stats::cor(seq_along(ept), ept, method="spearman"))
      if (is.finite(fit) && fit > 0.3) "drifting up over the record" else if (is.finite(fit) && fit < -0.3) "drifting down over the record" else "fairly steady across bouts"
    } else "too short a series to call a trend"
    insight_banner("activity", tone="navy", HTML(sprintf("%%EPT runs about <b>%.0f%%</b> on average here and is <b>%s</b>. %s",
      mean(ept), trend, if (identical(rv$meta$aquaticSiteType, "lake")) "As a lake, this site is EPT-poor by nature." else "EPT is the clean-water signal in this product.")))
  })
  output$pulseCsv <- downloadHandler(
    filename = function() sprintf("NEON-Inverts_bouts_%s_%s.csv", rv$site %||% "site", format(Sys.Date(),"%Y%m%d")),
    content = function(file){ bs <- rv$bouts
      if (is.null(bs) || !nrow(bs)) { utils::write.csv(data.frame(note="No bouts for this site."), file, row.names=FALSE); return() }
      keep <- intersect(c("eventID","siteID","collectDate","year","n_samples","habitatType","samplerType",
                          "mixed_habitat","mixed_sampler","density_m2","richness","ept_richness","pct_ept_ind",
                          "pct_ept_taxa","hill_q1","hill_q2","pct_dominant","pct_chironomidae","pct_oligochaeta",
                          "total_individuals","chao1","chao1_se","rarefied_richness","small_n"), names(bs))
      out <- bs[, keep, drop=FALSE]
      out$DENSITY_NOTE <- "density_m2 = within-site standardized index (individuals/m2); descriptive, not a pass/fail score"
      utils::write.csv(out, file, row.names=FALSE, na="") }, contentType="text/csv")

  # ---- density trend + Chao1 (Pulse tab) ----------------------------------
  output$densityPlot <- renderPlotly({
    bs <- bout_series(rv$bouts); req(bs); bs$dens <- num(bs$density_m2); bs <- bs[is.finite(bs$dens), , drop=FALSE]
    if (!nrow(bs)) return(note_plot("No density data (no benthic area)"))
    bs$card <- inv_bout_card(format(bs$collectDate, "%b %Y"), format(bs$collectDate, "%Y-%m-%d"),
      sprintf("density <b>%s</b>/m²", format(round(bs$dens), big.mark = ",")))
    plot_ly(bs, x=~collectDate, y=~dens, type="scatter", mode="lines+markers", line=list(color=DDL$teal, width=2.5),
      customdata=as.list(bs$card), marker=list(color=DDL$teal, size=7), hovertemplate="%{x|%b %Y}<br>%{y:.0f} /m²<extra></extra>") %>%
      plotly_theme(legend=FALSE) %>% plotly::layout(xaxis=list(title="Collection bout"), yaxis=list(title="Density (individuals / m²)", rangemode="tozero"))
  })
  output$densityInsight <- renderUI({
    bs <- bout_series(rv$bouts); req(bs); d <- num(bs$density_m2); d <- d[is.finite(d)]; req(length(d))
    insight_banner("graph-up", tone="pine", HTML(sprintf("Density runs from <b>%s</b> to <b>%s</b> /m² across bouts (median <span class='ci-hero'>%s</span>). It's heavy-tailed, so read the direction, not the exact value.",
      fmt_int(min(d)), fmt_int(max(d)), fmt_int(stats::median(d)))))
  })
  output$chaoBanner <- renderUI({
    sv <- site_vectors(rv$meta); req(sv)
    if (sv$small_n || is.na(sv$chao1))
      return(insight_banner("calculator", tone="gold", HTML(sprintf("Sampling found <b>%d</b> taxa. A standardized (Chao1) estimate is <b>suppressed</b> here: the count is too small to estimate richness honestly.", sv$richness))))
    se <- sv$chao1_se %||% 0
    insight_banner("calculator", tone="gold", HTML(sprintf("Sampling found <b>%d</b> taxa. <b>Chao1</b> estimates <span class='ci-hero'>%.0f</span> (±%.0f) really use the site, so roughly <b>%.0f</b> remain undetected. A bias-corrected <b>minimum</b> (Chao 1984).",
      sv$richness, sv$chao1, se, max(0, round(sv$chao1 - sv$richness)))),
      info_pop("Chao1", p("Benthic sampling misses rare and patchy taxa, so Chao1 is a lower bound on true richness. The ±is the standard error.")))
  })

  # ---- Diversity + composition --------------------------------------------
  output$diversityPlot <- renderPlotly({
    bs <- bout_series(rv$bouts); req(bs); bs$collectDate <- as.Date(bs$collectDate)
    rr <- num(bs$rarefied_richness); h1 <- num(bs$hill_q1)
    if (all(is.na(rr)) && all(is.na(h1))) return(note_plot("Standardized richness suppressed (insufficient count)"))
    card <- inv_bout_card(format(bs$collectDate, "%b %Y"), format(bs$collectDate, "%Y-%m-%d"),
      sprintf("rarefied richness <b>%s</b> (to 100)<br/>Hill q1 <b>%s</b> common taxa",
        ifelse(is.na(rr), "—", round(rr)), ifelse(is.na(h1), "—", sprintf("%.1f", h1))))
    p <- plot_ly()
    p <- p %>% add_trace(x=bs$collectDate, y=rr, type="scatter", mode="lines+markers", name="rarefied richness (to 100)",
      customdata=as.list(card), line=list(color=DDL$teal, width=2.5), marker=list(color=DDL$teal, size=7), connectgaps=FALSE,
      hovertemplate="%{x|%b %Y}<br>%{y:.0f} taxa (rarefied)<extra></extra>")
    p <- p %>% add_trace(x=bs$collectDate, y=h1, type="scatter", mode="lines+markers", name="Hill q1 (common taxa)",
      customdata=as.list(card), line=list(color=DDL$aqua, width=2, dash="dot"), marker=list(color=DDL$aqua, size=6), connectgaps=FALSE,
      hovertemplate="%{x|%b %Y}<br>%{y:.1f} effective common taxa<extra></extra>")
    p %>% plotly_theme() %>% plotly::layout(xaxis=list(title="Collection bout"), yaxis=list(title="Effective # taxa", rangemode="tozero"))
  })
  output$divInsight <- renderUI({
    bs <- bout_series(rv$bouts); req(bs); rr <- num(bs$rarefied_richness); rr <- rr[is.finite(rr)]
    if (!length(rr)) return(insight_banner("bar-chart-steps", tone="navy", HTML("Standardized richness is <b>suppressed</b> at this site (insufficient count per bout).")))
    insight_banner("bar-chart-steps", tone="navy", HTML(sprintf("Rarefied richness (to 100 individuals) averages <span class='ci-hero'>%.0f</span> taxa per bout, range <b>%.0f</b> to <b>%.0f</b>. Standardized so a bigger sample doesn't look richer just for being bigger.",
      mean(rr), min(rr), max(rr))))
  })
  output$compPlot <- renderPlotly({
    cl <- composition_long(rv$bouts); req(cl); if (!nrow(cl)) return(note_plot("No composition data"))
    # 100% stacked AREA on an evenly-spaced CATEGORICAL bout axis: a filled band
    # reads composition-share-over-a-sequence far better than thin bars marooned
    # in the gaps of an irregular date axis. EPT is the bottom anchor; the more
    # tolerant midge/worm groups and "other" stack above it. (Vera)
    cl$collectDate <- as.Date(cl$collectDate)
    bouts <- sort(unique(cl$collectDate))
    blab  <- make.unique(format(bouts, "%b %Y"))             # unique label even if two bouts share a month
    cl$bx <- factor(blab[match(cl$collectDate, bouts)], levels = blab)
    p <- plot_ly()
    for (comp in levels(cl$component)) {                      # EPT -> Chironomidae -> Oligochaeta -> other (anchor->top)
      sub <- cl[cl$component == comp, ]; sub <- sub[order(sub$collectDate), ]
      p <- p %>% add_trace(x=sub$bx, y=sub$share, type="scatter", mode="lines", stackgroup="one",
        name=comp, line=list(width=0.5, color=comp_col(comp)), fillcolor=comp_col(comp),
        hovertemplate=sprintf("%s · %%{y:.0f}%% · %%{x}<extra></extra>", comp)) }
    # clickable pin markers per bout (on the EPT-band top = the EPT share); the
    # card carries the exact 4-way % breakdown so bouts can be pinned + compared.
    bw <- tidyr::pivot_wider(cl[, c("collectDate","bx","component","share")],
                             names_from = component, values_from = share)
    bw <- bw[order(bw$collectDate), ]
    getc <- function(nm) if (nm %in% names(bw)) num(bw[[nm]]) else rep(0, nrow(bw))
    e <- getc("EPT"); ch <- getc("Chironomidae"); ol <- getc("Oligochaeta"); ot <- getc("other")
    bw$card <- inv_bout_card(format(as.Date(bw$collectDate), "%b %Y"), format(as.Date(bw$collectDate), "%Y-%m-%d"),
      sprintf("EPT <b>%.0f%%</b> · Chironomidae <b>%.0f%%</b><br/>Oligochaeta <b>%.0f%%</b> · other <b>%.0f%%</b>", e, ch, ol, ot))
    p <- p %>% add_trace(x=bw$bx, y=e, type="scatter", mode="markers", name="bout", customdata=as.list(bw$card),
      marker=list(color="#0a6f7a", size=7, line=list(color="#fff", width=1)), showlegend=FALSE,
      hovertemplate="%{x}<br>EPT %{y:.0f}% — tap to pin all four shares<extra></extra>")
    p %>% plotly_theme() %>% plotly::layout(
      xaxis=list(title="Collection bout (in order)", type="category"),
      yaxis=list(title="% of individuals", range=c(0,100), ticksuffix="%"))
  })

  # ---- Taxa Board (flagship pin-card scatter) -----------------------------
  output$taxaScatter <- renderPlotly({
    tb <- rv$taxa; req(tb)
    tb$dens <- num(tb$mean_density); tb <- tb[is.finite(tb$dens) & tb$dens > 0, , drop=FALSE]
    tb$reliable <- tb$ubiquity >= 5
    tb$col <- ept_col(tb$class); tb$col[!tb$reliable] <- "rgba(148,167,173,0.4)"
    tb$tip <- paste0("<span class='smt-pin-emoji'>\U0001F990</span> <b><em>", tb$scientificName, "</em></b><br/>",
      "<em>", ifelse(is.na(tb$order),"order n/a",tb$order), ifelse(tb$class=="EPT"," · EPT",""), "</em><br/>",
      "<span class='smt-pin-stats'>", round(tb$dens), " /m² · on ", tb$ubiquity, "% of samples<br/>",
      round(tb$total_est), " individuals (est.)</span>",
      ifelse(tb$reliable, "", "<br/><span class='smt-pin-rar' style='color:#9fe1e7'>⚠ few samples</span>"),
      "<br/><span class='smt-open' role='button' tabindex='0' data-tag='", tb$scientificName, "'>\U0001F50E Open taxon profile &rarr;</span>",
      "<br/><em class='smt-pin-hint'>Tap the dot to pin this card</em>")
    qcol <- if (is_dark()) "#8db4ba" else "#5d7c84"; muted <- qcol
    p <- plot_ly()
    for (cl in c("EPT","other")) { sub <- tb[tb$class %in% cl, ]; if (!nrow(sub)) next
      p <- p %>% add_trace(data=sub, x=~ubiquity, y=~dens, type="scatter", mode="markers", name=cl,
        customdata=~tip, marker=list(color=sub$col, size=12, opacity=0.82, line=list(color="#fff", width=0.5)),
        text=~scientificName, hovertemplate="<b>%{text}</b><br>%{x}% of samples · %{y:.0f}/m²<extra></extra>") }
    mx <- stats::median(tb$ubiquity); my <- stats::median(tb$dens[tb$reliable])
    # median crosshair: ubiquity median on the linear x, density median on the LOG
    # y (so the shape coord must be log10(my) — same axis lesson as the pin anchor).
    shp <- list(list(type="line", xref="x", yref="paper", x0=mx, x1=mx, y0=0, y1=1, line=list(color=qcol, dash="dot", width=1)))
    if (is.finite(my) && my > 0) shp <- c(shp, list(list(type="line", yref="y", xref="paper", x0=0, x1=1, y0=log10(my), y1=log10(my), line=list(color=qcol, dash="dot", width=1))))
    if (!is.null(rv$sp)) { ir <- tb[tb$scientificName == rv$sp, ]
      if (nrow(ir)==1) p <- p %>% add_trace(x=ir$ubiquity, y=ir$dens, type="scatter", mode="markers", name="★ viewing", customdata=ir$tip, showlegend=TRUE,
        marker=list(symbol="diamond", size=18, color="#0a6f7a", line=list(color="#fff", width=1.6)), hovertemplate=paste0("viewing ", ir$scientificName, "<extra></extra>")) }
    p %>% plotly_theme() %>% plotly::layout(xaxis=list(title="Ubiquity (% of samples present)"), yaxis=list(title="Mean density (individuals / m², log)", type="log"),
      shapes=shp,
      annotations=list(list(text=sprintf("at <b>%s</b> (this site) · each dot is a taxon · density (within-site index, log) × ubiquity · colour = EPT vs other", rv$site %||% "this site"), x=0, y=1.06, xref="paper", yref="paper", showarrow=FALSE, xanchor="left", font=list(color=muted, size=11))),
      hovermode="closest")
  })
  output$spCardSlot <- renderUI({
    if (is.null(rv$sp)) return(div(class="qc-empty", div(class="qc-empty-icon","\U0001F990"), h4("Tap a dot above to pin a taxon card here"),
      p("Each dot is a taxon. Tap one to pin its card, then open its full profile — or use the taxon picker above the tabs.")))
    r <- rv$taxa[rv$taxa$scientificName == rv$sp,]; if (!nrow(r)) return(NULL)
    div(class="lab-sel", span(class="ls-emoji","\U0001F50E"),
      div(class="ls-body", div(class="ls-id", tags$b(em(r$scientificName)), sprintf(" · %.0f /m² · %.0f%% of samples", r$mean_density, r$ubiquity)),
        div(class="ls-dom", sprintf("%s%s", r$order %||% "order n/a", if (r$class=="EPT") " · EPT" else ""))),
      actionButton("goSpFromCard", tagList(bs_icon("arrows-fullscreen"), " Open full profile"), class="btn-outline-dark btn-sm"))
  })

  # ---- Cross-site gradient (Across the country) ---------------------------
  output$crossGradient <- renderPlotly({
    g <- CROSS_SITE; if (is.null(g) || !nrow(g)) return(note_plot("Cross-site table unavailable", "\U0001F30D"))
    g <- as.data.frame(g)
    m <- neon_sites[match(g$site, neon_sites$site), ]
    g$name <- m$name; g$state <- m$state
    g$type <- ifelse(is.na(g$aquaticSiteType), m$type, g$aquaticSiteType)
    metric <- input$crossMetric %||% "ept_richness"; xvar <- input$crossX %||% "lat"
    is_log <- identical(metric, "density_m2")
    ylab <- switch(metric,
      ept_richness = "EPT richness (# taxa)", pct_ept_ind = "EPT share (% of individuals)",
      richness = "Observed richness (# taxa)", rarefied_richness = "Rarefied richness (to 100 ind.)",
      density_m2 = "Density index (individuals / m², log)", hill_q1 = "Common-taxa diversity (Hill q1)", metric)
    xlab <- if (identical(xvar,"elevation")) "Elevation (m)" else "Latitude (°N)"
    g$xx <- num(g[[xvar]]); g$yy <- num(g[[metric]])
    g$eff <- num(g$n_bouts); g$eff[is.na(g$eff)] <- 1
    g <- g[is.finite(g$xx) & is.finite(g$yy) & (!is_log | g$yy > 0), ]; if (!nrow(g)) return(note_plot("No sites with this combination", "\U0001F30D"))
    g$tip <- paste0("<span class='smt-pin-emoji'>\U0001F990</span> <b>", g$site, " · ", g$name, "</b><br/>",
      "<em>", TYPE_LAB[g$type] %||% g$type, " · ", g$state, "</em><br/>",
      "<span class='smt-pin-stats'>", g$richness, " taxa · ", g$ept_richness, " EPT · ", round(g$pct_ept_ind), "% EPT<br/>",
      round(g$density_m2), " /m² (index)</span>",
      "<br/><span class='smt-open' role='button' tabindex='0' data-action='site' data-tag='", g$site, "'>\U0001F990 Open this site &rarr;</span>",
      "<br/><em class='smt-pin-hint'>Tap the dot to pin this card</em>")
    sref <- 2 * max(g$eff, na.rm=TRUE) / (26^2); muted <- if (is_dark()) "#8db4ba" else "#5d7c84"
    p <- plot_ly()
    for (ty in c("stream","river","lake")) { sub <- g[g$type %in% ty, ]; if (!nrow(sub)) next
      p <- p %>% add_trace(data=sub, x=~xx, y=~yy, type="scatter", mode="markers", name=unname(TYPE_LAB[ty]),
        customdata=~tip, text=~paste0(site, " · ", name),
        marker=list(color=type_col(ty), size=sub$eff, sizemode="area", sizeref=sref, sizemin=6, opacity=0.82, line=list(color="#fff", width=0.6)),
        hovertemplate="%{text}<br>%{x:.1f} · %{y:.1f}<extra></extra>") }
    if (!is.null(rv$site)) { ir <- g[g$site == rv$site, ]
      if (nrow(ir)==1) p <- p %>% add_trace(x=ir$xx, y=ir$yy, type="scatter", mode="markers", name="★ viewing", customdata=ir$tip,
        marker=list(symbol="diamond", size=18, color="#0a6f7a", line=list(color="#fff", width=1.6)), hovertemplate=paste0("viewing ", ir$site, "<extra></extra>")) }
    sc <- spearman_ci(g$xx, g$yy)
    ci_str <- if (!is.na(sc$lo)) sprintf(", 95%% CI [%.2f, %.2f], n = %d", sc$lo, sc$hi, sc$n) else sprintf(", n = %d", sc$n)
    ann <- list(
      list(text = sprintf("Every dot is one of %d NEON aquatic sites · %s × %s · dot size = bouts", nrow(g), xlab, tolower(ylab)),
           x=0, y=1.13, xref="paper", yref="paper", showarrow=FALSE, xanchor="left", font=list(color=muted, size=11)),
      list(text = sprintf("Spearman ρ = %.2f%s · space-for-time (34 places, not one site changing), correlational, confounded by water type &amp; habitat", ifelse(is.na(sc$rho),0,sc$rho), ci_str),
           x=0, y=1.065, xref="paper", yref="paper", showarrow=FALSE, xanchor="left", font=list(color=muted, size=10.5)))
    p %>% plotly_theme() %>% plotly::layout(xaxis=list(title=list(text=xlab, standoff=10)),
      yaxis=list(title=ylab, type=if (is_log) "log" else "linear", rangemode=if (is_log) "normal" else "tozero"),
      annotations=ann, hovermode="closest", margin=list(l=60, r=30, t=92, b=52))
  })
  output$crossSiteCsv <- downloadHandler(
    filename = function() sprintf("NEON-Inverts_cross-site_%s.csv", format(Sys.Date(),"%Y%m%d")),
    content = function(file){ g <- CROSS_SITE
      if (is.null(g) || !nrow(g)) { utils::write.csv(data.frame(note="Cross-site table unavailable."), file, row.names=FALSE); return() }
      g <- as.data.frame(g); m <- neon_sites[match(g$site, neon_sites$site), ]; g$name <- m$name; g$state <- m$state
      keep <- intersect(c("site","name","state","aquaticSiteType","lat","lng","elevation","n_bouts","n_samples",
                          "year_min","year_max","density_m2","richness","ept_richness","pct_ept_ind","pct_ept_taxa",
                          "hill_q1","rarefied_richness","chao1","pct_chironomidae","top_taxon"), names(g))
      out <- g[, keep, drop=FALSE]
      out$DENSITY_NOTE <- "density_m2 = within-site standardized index; compare sites by direction, not raw value. Lakes are naturally EPT-poor."
      utils::write.csv(out, file, row.names=FALSE, na="") }, contentType="text/csv")
  output$taxaCsv <- downloadHandler(
    filename = function() sprintf("NEON-Inverts_taxa_%s_%s.csv", rv$site %||% "site", format(Sys.Date(),"%Y%m%d")),
    content = function(file){ tb <- rv$bundle$taxa
      if (is.null(tb) || !nrow(tb)) { utils::write.csv(data.frame(note="No taxa for this site."), file, row.names=FALSE); return() }
      keep <- intersect(c("acceptedTaxonID","scientificName","order","family","is_ept","mean_density","total_est","n_samples_present","ubiquity"), names(tb))
      out <- cbind(site = rv$site %||% NA_character_, tb[, keep, drop=FALSE])
      utils::write.csv(out, file, row.names=FALSE, na="") }, contentType="text/csv")

  # ---- Taxon Profile (downloadable card + QC flags) -----------------------
  output$taxonDensityPlot <- renderPlotly({
    sci <- rv$sp; req(sci); req(rv$taxa)
    # rank-in-context: this taxon's mean density against the site's densest taxa,
    # ALL in the SAME unit (individuals / m²), so the comparison is honest — the
    # old version stacked density / ubiquity / total individuals on one log axis,
    # three different units that read as comparable but aren't. (Vera)
    tb <- rv$taxa; tb$dens <- num(tb$mean_density); tb <- tb[is.finite(tb$dens) & tb$dens > 0, , drop=FALSE]
    req(nrow(tb))
    topn <- head(tb[order(-tb$dens), , drop=FALSE], 8)
    if (!(sci %in% topn$scientificName)) topn <- rbind(topn, tb[tb$scientificName == sci, , drop=FALSE])
    topn <- topn[order(topn$dens), , drop=FALSE]
    topn$lab <- factor(topn$scientificName, levels = topn$scientificName)
    topn$col <- ifelse(topn$scientificName == sci, "#0a6f7a", "rgba(148,167,173,0.5)")
    plot_ly(topn, x=~dens, y=~lab, type="bar", orientation="h", marker=list(color=topn$col),
            text=~ifelse(scientificName == sci, " · this taxon", ""),
            hovertemplate="%{y}<br>%{x:.0f} /m²%{text}<extra></extra>") %>%
      plotly_theme(legend=FALSE) %>% plotly::layout(
        xaxis=list(title="mean density (individuals / m², log)", type="log"),
        yaxis=list(title=""), margin=list(l=132, r=12, t=8, b=38),
        annotations=list(list(text="this taxon (teal) vs the site's densest taxa — same unit", x=0, y=1.08, xref="paper", yref="paper", showarrow=FALSE, xanchor="left", font=list(color=if(is_dark())"#8db4ba" else "#5d7c84", size=10.5))))
  })
  qc <- reactive({ req(rv$bundle); inv_qc(rv$bundle) })
  qc_icon <- function(level) switch(level, high = "exclamation-octagon-fill", warn = "exclamation-triangle-fill", info = "info-circle-fill", "check-circle-fill")

  output$speciesProfile <- renderUI({
    if (is.null(rv$sp)) return(div(class="qc-empty", div(class="qc-empty-icon","\U0001F990"), h4("Pick a taxon to open its profile"),
      p("Use the Taxa Board (tap a dot → “Open taxon profile”) or the taxon picker above the tabs.")))
    r <- rv$taxa[rv$taxa$scientificName == rv$sp,]; req(nrow(r)==1)
    tile <- function(v,l) div(class="qc-tile", div(class="qc-tile-v", v), div(class="qc-tile-l", l))
    qf <- qc()$flags
    qc_block <- tagList(
      div(class="qc-section-h", bs_icon("clipboard-check"), " Site data-quality review flags ",
        tags$span(class="qcf-sub","· verify, not errors")),
      if (length(qf)) tagList(
        div(class="qc-flags", lapply(qf, function(f) div(
          class = paste0("qc-flag qc-flag-", f$level, " qc-flag-click"), role = "button", tabindex = "0",
          onclick = sprintf("Shiny.setInputValue('invQcInspect','%s',{priority:'event'})", f$key),
          bs_icon(qc_icon(f$level)),
          div(class="qcf-body",
            div(class="qcf-title", f$title, tags$span(class="qcf-n", f$n)),
            div(class="qcf-detail", f$detail)),
          tags$span(class="qcf-go", bs_icon("chevron-right"))))),
        div(class="qcf-hint", bs_icon("hand-index-thumb"), " tap a flag to list the exact samples behind it"))
      else div(class="qc-flag qc-flag-ok", bs_icon("check-circle-fill"),
        div(class="qcf-body", div(class="qcf-title","No data-quality flags for this site"),
          div(class="qcf-detail","Benthic area, subsample fractions, dominance, and identification all look consistent, nothing to verify."))))
    body <- div(id="qcCardNode", class="qc-card", `data-short`=gsub("[^A-Za-z]","",substr(r$scientificName,1,20)),
      div(class="qc-head", span(class="qc-emoji","\U0001F50E"),
        div(div(class="qc-id", em(r$scientificName)), div(class="qc-sci", sprintf("%s%s", r$order %||% "order n/a", if (r$class=="EPT") " · EPT (clean-water group)" else ""))),
        div(class="qc-head-badges", glow_badge(paste0(round(r$total_est), " individuals"), DDL$sky))),
      div(class="qc-tiles",
        tile(round(r$mean_density), "/m² (density)"), tile(paste0(r$ubiquity,"%"), "of samples"),
        tile(r$n_samples_present, "samples present"), tile(r$family %||% "—", "family"),
        tile(if (r$class=="EPT") "yes" else "no", "EPT"), tile(round(r$total_est), "individuals")),
      div(class="qc-section-h", bs_icon("bar-chart"), " This taxon at a glance"),
      plotlyOutput("taxonDensityPlot", height="220px"),
      qc_block,
      p(class="qc-cap-note", style="margin-top:8px", bs_icon("info-circle"),
        " Density is a within-site standardized index (individuals / m²), not a population. Counts are subsample estimates scaled to the whole sample. QC flags are site-level (the bundle does not carry per-taxon collection records)."))
    div(div(class="plot-profile-wrap", body), div(class="qc-toolbar",
      tags$button(class="smt-snap-btn", type="button", onclick="smtSaveQcCard()", bsicons::bs_icon("download"), " Save taxon card (PNG)"),
      downloadButton("taxaCsv2", "Download taxa records (CSV)", class="smt-clear-btn"),
      if (length(qf)) downloadButton("qcReportCsv", "Download QC report (CSV)", class="smt-clear-btn"),
      downloadButton("codebookCsv", "Download column codebook (CSV)", class="smt-clear-btn")),
      uiOutput("invQcInspector"))
  })
  output$taxaCsv2 <- downloadHandler(
    filename = function() sprintf("NEON-Inverts_taxa_%s_%s.csv", rv$site %||% "site", format(Sys.Date(),"%Y%m%d")),
    content = function(file){ tb <- rv$bundle$taxa
      if (is.null(tb) || !nrow(tb)) { utils::write.csv(data.frame(note="No taxa."), file, row.names=FALSE); return() }
      out <- cbind(site = rv$site %||% NA_character_, as.data.frame(tb))
      utils::write.csv(out, file, row.names=FALSE, na="") }, contentType="text/csv")

  output$invQcInspector <- renderUI({
    key <- input$invQcInspect; q <- qc(); req(!is.null(key), key %in% names(q$sets))
    st <- q$sets[[key]]; req(!is.null(st), nrow(st))
    f <- Filter(function(x) x$key == key, q$flags)[[1]]
    show <- names(st)
    head_n <- min(nrow(st), 200L); sv <- st[seq_len(head_n), show, drop=FALSE]
    div(class="qc-inspector",
      div(class="qci-head", bs_icon(qc_icon(f$level)), tags$b(sprintf(" %s · %d record%s", f$title, f$n, if (f$n==1) "" else "s")),
        downloadButton("qcSubsetCsv", "Download these", class="btn-outline-dark btn-sm qci-dl")),
      div(class="qc-cap-scroll", tags$table(class="inspect-tbl",
        tags$thead(tags$tr(lapply(show, tags$th))),
        tags$tbody(lapply(seq_len(nrow(sv)), function(i)
          tags$tr(lapply(show, function(cc) tags$td(format(sv[[cc]][i]))))) ))),
      if (nrow(st) > head_n) p(class="qc-cap-note", sprintf("Showing first %d of %d. Download for the full list.", head_n, nrow(st))))
  })
  output$qcSubsetCsv <- downloadHandler(
    filename = function() sprintf("NEON-Inverts_QC-%s_%s_%s.csv", input$invQcInspect %||% "flag", rv$site %||% "site", format(Sys.Date(),"%Y%m%d")),
    content = function(file){ q <- qc(); st <- q$sets[[input$invQcInspect]]; req(!is.null(st))
      st <- cbind(site = rv$site %||% NA_character_, flag = input$invQcInspect %||% NA_character_, as.data.frame(st))
      utils::write.csv(st, file, row.names=FALSE, na="") }, contentType="text/csv")
  output$qcReportCsv <- downloadHandler(
    filename = function() sprintf("NEON-Inverts_QC-report_%s_%s.csv", rv$site %||% "site", format(Sys.Date(),"%Y%m%d")),
    content = function(file){ rep <- inv_qc_report(rv$bundle)
      if (is.null(rep)) rep <- data.frame(note="No data-quality flags for this site.")
      rep <- cbind(site = rv$site %||% NA_character_, rep)
      utils::write.csv(rep, file, row.names=FALSE, na="") }, contentType="text/csv")
  output$codebookCsv <- downloadHandler(
    filename = function() sprintf("NEON-Inverts_codebook_%s.csv", format(Sys.Date(),"%Y%m%d")),
    content = function(file) utils::write.csv(inv_codebook(), file, row.names=FALSE, na=""),
    contentType="text/csv")

  # ---- within-site Map (the sampled reach) --------------------------------
  output$siteMap <- leaflet::renderLeaflet({
    pts <- sample_points(rv$samples); req(pts)
    rr <- range(pts$density_m2, na.rm=TRUE)
    pts$radius <- if (is.finite(diff(rr)) && diff(rr) > 0) 10 + 16*(pts$density_m2 - rr[1])/diff(rr) else 13
    pts$radius[is.na(pts$radius)] <- 11
    m <- leaflet::leaflet(pts) %>%
      leaflet::addProviderTiles(input$view %||% "Esri.WorldTopoMap") %>%
      leaflet::addScaleBar(position="bottomleft", options=leaflet::scaleBarOptions(imperial=TRUE, maxWidth=140)) %>%
      leaflet::addCircleMarkers(lng=~lng, lat=~lat, radius=~radius, fillColor=DDL$teal, color=DDL$teal2,
        weight=1.5, fillOpacity=0.85, layerId=~namedLocation,
        label=~lapply(sprintf("<div style='font-family:Rubik,sans-serif'><b>%s</b><br>%s · %s sampler · %d samples · %s /m²</div>",
          namedLocation, modal_habitat %||% "habitat n/a", modal_sampler %||% "sampler n/a", n_samples,
          ifelse(is.na(density_m2), "—", as.character(round(density_m2)))), htmltools::HTML),
        popup=site_reach_popup(pts, rv$meta, rv$site)) %>%
      leaflet::addControl(position="topright", html=htmltools::HTML(sprintf(
        "<div style='font-family:Rubik,sans-serif;background:#fff;padding:6px 9px;border-radius:8px;box-shadow:0 1px 4px rgba(0,0,0,.15);max-width:230px'><b>%s</b><br><span style='color:#5d7c84;font-size:12px'>marker size = density (within-site index) · tap for details</span></div>",
        rv$label %||% rv$site %||% "Sampled reach")))
    # fit the view to the station(s): a single fixed reach (the usual NEON case)
    # gets a reach-scale zoom so it isn't a lone dot at world view; multiple named
    # stations get bounds over them all.
    if (nrow(pts) == 1) m <- m %>% leaflet::setView(pts$lng[1], pts$lat[1], zoom = 15)
    else m <- m %>% leaflet::fitBounds(min(pts$lng), min(pts$lat), max(pts$lng), max(pts$lat), options = list(padding = c(40, 40)))
    m
  })
  observeEvent(input$siteMap_marker_click, { id <- input$siteMap_marker_click$id; if (!is.null(id)) rv$reach <- id })
  # the click->detail payoff: a single-reach site shows its one station's detail
  # immediately; a multi-station site prompts a tap, then shows the tapped one.
  output$reachPanel <- renderUI({
    if (is.null(rv$samples)) return(NULL)
    pts <- sample_points(rv$samples); req(pts); n <- nrow(pts)
    sel <- if (!is.null(rv$reach) && rv$reach %in% pts$namedLocation) rv$reach else if (n == 1) pts$namedLocation[1] else NULL
    if (is.null(sel))
      return(div(class="grid-empty", bs_icon("hand-index-thumb"),
        span(sprintf(" This site has %d sampled stations. Tap a marker for its habitat, sampler, and density.", n))))
    r <- pts[pts$namedLocation == sel, ][1, ]
    div(class="grid-empty", bs_icon("geo-alt-fill"),
      span(HTML(sprintf(" <b>%s</b> — %s · %s sampler · <b>%s</b> samples · density <b>%s</b>/m²%s.",
        r$namedLocation, r$modal_habitat %||% "habitat n/a", r$modal_sampler %||% "sampler n/a",
        as.integer(r$n_samples), ifelse(is.na(r$density_m2), "—", as.character(round(r$density_m2))),
        if (n == 1) " · the one fixed reach NEON samples here" else ""))))
  })

  # ---- Splash: national site picker (the contract) ------------------------
  site_popup_html <- function(r, si) {
    sprintf("<div style='font-family:Rubik,sans-serif;min-width:230px'><b>%s · %s</b><br><span style='color:#5d7c84'>%s · %s</span><br><b>%s</b> taxa · <b>%s</b> EPT · <b>%.0f%%</b> EPT individuals<br><div style='margin-top:7px;display:flex;gap:7px;flex-wrap:wrap'><a href='#' class='btn btn-sm btn-primary' style='font-weight:700' onclick=\"smtLoadStart('%s · loading…');Shiny.setInputValue('siteExplore','%s',{priority:'event'});return false;\">Explore this site &rarr;</a><a href='#' class='btn btn-sm btn-outline-secondary' onclick=\"Shiny.setInputValue('siteInfo','%s',{priority:'event'});return false;\">About this site</a></div></div>",
      r$site, r$name, TYPE_LAB[r$type] %||% r$type, r$state,
      si$richness %||% "—", si$ept_richness %||% "—", si$pct_ept_ind %||% NA,
      gsub("'","", r$name), r$site, r$site)
  }
  output$nationalPicker <- leaflet::renderLeaflet({
    d <- site_table
    if (is.null(d) || !nrow(d)) {
      nd <- neon_sites
      return(leaflet::leaflet(nd) %>% leaflet::addProviderTiles("CartoDB.Positron") %>% leaflet::setView(-96, 41, 3) %>%
        leaflet::addCircleMarkers(lng=~lng, lat=~lat, radius=6, fillColor=~type_col(type), color="#fff", weight=1, fillOpacity=0.35,
          label=~lapply(sprintf("<b>%s</b> · %s<br><span style='color:#9c5d18'>data not built yet</span>", site, name), htmltools::HTML)))
    }
    d$eff <- num(d$n_bouts); d$eff[is.na(d$eff)] <- min(d$eff, na.rm=TRUE)
    rr <- range(d$eff, na.rm=TRUE); d$rad <- 6 + 12*(d$eff - rr[1])/max(1, diff(rr))
    pops <- vapply(seq_len(nrow(d)), function(i){ si <- SITE_INDEX[SITE_INDEX$site == d$site[i], ]; site_popup_html(d[i,], si) }, character(1))
    leaflet::leaflet(d) %>% leaflet::addProviderTiles("CartoDB.Positron") %>%
      leaflet::fitBounds(min(d$lng, na.rm=TRUE), min(d$lat, na.rm=TRUE), max(d$lng, na.rm=TRUE), max(d$lat, na.rm=TRUE)) %>%  # frame all 34 incl. AK + PR (was a fixed CONUS setView that clipped them)
      leaflet::addCircleMarkers(lng=~lng, lat=~lat, radius=~rad, fillColor=~type_col(type), color="#fff", weight=1, fillOpacity=0.85,
        label=~lapply(sprintf("<b>%s</b> · %s<br>%s · %s EPT taxa", site, name, TYPE_LAB[type] %||% type, ept_richness), htmltools::HTML),
        popup=pops, popupOptions=leaflet::popupOptions(maxWidth=300, minWidth=230, autoPan=TRUE, closeOnClick=FALSE)) %>%
      leaflet::addLegend("bottomright", colors=unname(TYPE_COL), labels=unname(TYPE_LAB), title="Water type", opacity=0.9)
  })

  # ---- Site report (top-bar downloads) ------------------------------------
  # PDF: a one-page printable/shareable site card (base-graphics, no pandoc dep).
  output$reportPdf <- downloadHandler(
    filename = function() sprintf("NEON-Inverts_site-report_%s_%s.pdf", rv$site %||% "site", format(Sys.Date(),"%Y%m%d")),
    content = function(file) inv_report_pdf(file, rv$meta, rv$site, rv$label),
    contentType = "application/pdf")
  # CSV: the machine-readable one-row-per-metric report (+ codebook companion).
  output$reportCsv <- downloadHandler(
    filename = function() sprintf("NEON-Inverts_site-report_%s_%s.csv", rv$site %||% "site", format(Sys.Date(),"%Y%m%d")),
    content = function(file){
      sv <- site_vectors(rv$meta)
      if (is.null(sv)) { utils::write.csv(data.frame(note="No site loaded."), file, row.names=FALSE); return() }
      m <- function(metric, value, note="") data.frame(metric=metric, value=as.character(value), note=note, stringsAsFactors=FALSE)
      rows <- list(
        m("site", rv$site %||% NA, "NEON aquatic site code"),
        m("site_label", rv$label %||% NA, ""),
        m("aquatic_type", sv$type, "lake / river / stream — lakes are naturally EPT-poor"),
        m("years_sampled", year_label(rv$meta), ""),
        m("bouts", sv$n_bouts, "collection bouts"),
        m("samples", sv$n_samples, "benthic samples"),
        m("taxa_richness", sv$richness, "distinct taxa found"),
        m("ept_richness", sv$ept_richness, "mayfly/stonefly/caddisfly taxa"),
        m("pct_ept_individuals", sv$pct_ept_ind, "EPT share of individuals — descriptive, not a pass/fail"),
        m("pct_ept_taxa", sv$pct_ept_taxa, "EPT share of taxa"),
        m("density_index_per_m2", sv$density, "within-site standardized density index, NOT a population"),
        m("hill_q1", sv$hill_q1, "effective number of common taxa"),
        m("rarefied_richness_to_100", if (sv$small_n) "suppressed (small n)" else sv$rarefied, "Hurlbert 1971 rarefaction"),
        m("chao1", if (sv$small_n || is.na(sv$chao1)) "suppressed (small n)" else sv$chao1, "asymptotic richness (Chao 1984)"),
        m("pct_chironomidae", sv$pct_chiro, "midge share — tolerance surrogate"),
        m("pct_oligochaeta", sv$pct_oligo, "worm share — tolerance surrogate"),
        m("top_taxon", sv$top_taxon, "densest taxon"),
        m("DISCLAIMER", "descriptive bioassessment", INV_DISCLAIMER))
      out <- do.call(rbind, rows)
      utils::write.csv(out, file, row.names=FALSE, na="") }, contentType="text/csv")

  # ---- About + help -------------------------------------------------------
  output$aboutPanel <- renderUI({
    div(class="about-wrap",
      div(class="about-card", h4("\U0001F990 What this is"),
        p("An (unofficial) explorer for NEON's ", tags$b("Macroinvertebrate collection"), " (", tags$code("DP1.20120.001"), "). At each aquatic site NEON scoops the stream or lake bottom with a fixed-area sampler, then sorts and identifies the small animals living there, the insect larvae, worms, snails, and crustaceans that fish eat and that breathe the water directly.")),
      div(class="about-card", h4(bs_icon("droplet-half"), " Density is an index, not a count"),
        p("Each sample covers a known area of bottom, so a count becomes a ", tags$b("density"), " (individuals per square metre). Big samples are subsampled and scaled up, so the number is an ", tags$b("estimate"), ". We call it a ", tags$b("within-site standardized density index"), ": good for comparing bouts at one site (within a habitat and sampler type), never an absolute population.")),
      div(class="about-card", h4(bs_icon("award"), " EPT: the clean-water bugs"),
        p(tags$b("EPT"), " is mayflies (Ephemeroptera), stoneflies (Plecoptera), and caddisflies (Trichoptera). These groups need clean, cool, well-oxygenated water, so a higher EPT share generally tracks better conditions, ", tags$b("within a site"), ". Midges and worms tend to tolerate more, so a midge- or worm-heavy community is often the more stressed one."),
        p("This is descriptive. NEON sites have ", tags$b("no calibrated reference condition"), " and no state biotic index, so the app never gives a pass/fail score, a good/fair/poor rating, or an aquatic-life-use call. (Method: EPA Rapid Bioassessment, Barbour et al. 1999.)")),
      div(class="about-card", h4(bs_icon("water"), " Lakes vs streams"),
        p("Lakes are naturally ", tags$b("EPT-poor"), ", because stoneflies and most mayflies want flowing, oxygen-rich riffles. A low EPT share at a lake is the ecosystem, not impairment, and lakes are not directly comparable to streams on EPT metrics.")),
      div(class="about-card", h4(bs_icon("calculator"), " How many taxa?"),
        p(tags$b("Chao1"), " (Chao 1984) estimates how many taxa use the site beyond those found. ", tags$b("Rarefied richness"), " (Hurlbert 1971) standardizes richness to a common 100 individuals so a bigger sample doesn't look richer. Both are suppressed where the count is too small to estimate honestly.")),
      div(class="about-card bio-links-block",
        div(class="bio-links-title", "Explore the NEON series"),
        div(class="sib-grid", lapply(SUITE_REGISTRY, function(s) {
          tags$a(class=paste0("sib-card", if (identical(s$dpid, NEON_DPID)) " is-self" else ""), href=s$url, target="_blank",
            div(class="sib-emoji", s$emoji),
            div(div(class="sib-name", s$name), div(class="sib-tag", s$tag))) }))),
      div(class="about-card", h4(bs_icon("patch-check"), " Data attribution & license"),
        p(class="caveat",
          "Built with data from the National Ecological Observatory Network (NEON), a U.S. National Science Foundation program operated by Battelle. NEON data are provided under a Creative Commons Attribution 4.0 International (CC BY 4.0) license (",
          tags$a(href="https://creativecommons.org/licenses/by/4.0/", target="_blank", "creativecommons.org/licenses/by/4.0"),
          "). This app aggregates and derives summary metrics from the raw NEON data products; the underlying measurements are unaltered. It is an independent, unofficial tool and is not endorsed by NEON, Battelle, or the NSF.")),
      div(class="about-card", h4(bs_icon("envelope"), " Desert Data Labs"),
        p(bs_icon("envelope"), " ", tags$a(href="mailto:desertdatalabs@gmail.com","desertdatalabs@gmail.com"), " · ",
          tags$a(href="https://data.neonscience.org/data-products/DP1.20120.001", target="_blank", "NEON data product"))))
  })
  observeEvent(input$help, showModal(modalDialog(easyClose=TRUE, title=tagList(bs_icon("question-circle"), " How it works"),
    tags$ul(
      tags$li(HTML("Pick a <b>site</b>: tap a dot on the map, or pick one by name in the panel below the map.")),
      tags$li(HTML("<b>The EPT Pulse</b> · the clean-water signal (mayflies / stoneflies / caddisflies) and density over time, bout by bout.")),
      tags$li(HTML("<b>Taxa Board</b> · every taxon by density × ubiquity; <b>tap one</b> to pin its card, then “Open taxon profile”.")),
      tags$li(HTML("<b>Diversity</b> · standardized richness and the composition stack. <b>Across the country</b> · all 34 sites on a gradient.")),
      tags$li(HTML("Density is a <b>within-site standardized index</b>, not a population, and these are descriptive metrics, never a pass/fail."))),
    footer=modalButton("Got it"))))
}
