# ===========================================================================
# NEON My Little Inverts — report_pdf.R
# A one-page printable site report drawn with the BASE graphics device — no
# rmarkdown / pandoc / LaTeX dependency, so it ships on the lean Connect Cloud
# deploy (manifest stays minimal). The shareable/printable companion to the CSV
# report (output$reportCsv): branded riffle-teal header, a key-metrics grid, the
# site story, and the honesty contract + method citation. Drawing is device-
# agnostic (.inv_report_draw) so the same layout renders to PDF (the download)
# or PNG (visual QA). Honesty grain unchanged: density is a within-site index,
# no IBI / pass-fail; rarefied/Chao1 suppressed where small_n.
# ===========================================================================

# fit `s` to `maxw` USER units by measuring real glyph widths (strwidth), so the
# layout never depends on a hand-guessed character count.
.wrap_w <- function(s, maxw, cex) {
  words <- strsplit(s, "\\s+")[[1]]; words <- words[nzchar(words)]
  if (!length(words)) return("")
  lines <- character(0); cur <- ""
  for (w in words) {
    test <- if (nzchar(cur)) paste(cur, w) else w
    if (graphics::strwidth(test, cex = cex, units = "user") > maxw && nzchar(cur)) {
      lines <- c(lines, cur); cur <- w
    } else cur <- test
  }
  if (nzchar(cur)) lines <- c(lines, cur)
  lines
}

# device-agnostic page draw (a device must already be open)
.inv_report_draw <- function(meta, site_code, label) {
  sv <- site_vectors(meta); if (is.null(sv)) sv <- list()
  nm <- neon_sites[neon_sites$site == site_code, ]; has_nm <- nrow(nm) > 0

  teal <- "#0e8f9c"; teal2 <- "#0a6f7a"; aqua <- "#2bb7c4"
  ink  <- "#102a33"; ink2 <- "#274a54"; muted <- "#5d7c84"
  gold <- "#9c5d18"; line <- "#cfe4e6"; tile_bg <- "#eef6f7"

  fmt_n <- function(x) if (is.null(x) || length(x) != 1 || is.na(x)) "—" else format(round(as.numeric(x)), big.mark = ",", trim = TRUE)
  pct   <- function(x) if (is.null(x) || length(x) != 1 || is.na(x)) "—" else sprintf("%.0f%%", as.numeric(x))

  op <- graphics::par(mar = c(0, 0, 0, 0), xpd = NA); on.exit(graphics::par(op), add = TRUE)
  graphics::plot.new(); graphics::plot.window(xlim = c(0, 100), ylim = c(0, 130))
  L <- 8; R <- 92

  txt <- function(x, y, s, cex = 1, col = ink, font = 1, adj = 0) graphics::text(x, y, s, cex = cex, col = col, font = font, adj = adj)
  para <- function(x, y, s, maxw, cex = 0.9, col = ink2, lh = 2.6, font = 1) {
    wr <- .wrap_w(s, maxw, cex)
    for (i in seq_along(wr)) txt(x, y - (i - 1) * lh, wr[i], cex = cex, col = col, font = font)
    y - length(wr) * lh
  }

  # ---- header band ----
  graphics::rect(0, 119, 100, 130, col = teal2, border = NA)
  graphics::rect(0, 117.7, 100, 119, col = aqua, border = NA)
  txt(L, 125.2, "NEON · My Little Inverts", cex = 1.7, col = "#ffffff", font = 2)
  txt(L, 121.4, "Aquatic macroinvertebrate site report · descriptive bioassessment", cex = 0.9, col = "#d6f3f5")
  txt(R, 125.2, format(Sys.Date(), "%d %b %Y"), cex = 0.95, col = "#d6f3f5", adj = 1)
  txt(R, 121.6, "DP1.20120.001", cex = 0.82, col = "#bfe7ea", adj = 1)

  # ---- site identity ----
  txt(L, 113, label %||% (if (has_nm) nm$name[1] else site_code), cex = 1.5, col = ink, font = 2)
  typ <- TYPE_LAB[sv$type] %||% sv$type %||% "site"
  yl  <- year_label(meta)
  sub <- sprintf("%s · %s%s", typ,
                 if (has_nm) sprintf("NEON %s · %s", nm$domain[1], nm$state[1]) else site_code,
                 if (!is.null(yl) && !is.na(yl)) paste0(" · ", yl) else "")
  txt(L, 109, sub, cex = 0.98, col = teal2)
  ybio <- if (has_nm && !is.na(nm$bio[1])) para(L, 105, nm$bio[1], maxw = R - L, cex = 0.9, col = muted, lh = 2.5) else 105

  # ---- metrics grid (3 x 2) ----
  gy <- 95; gh <- 11; gw <- (R - L) / 3
  cells <- list(
    list(v = fmt_n(sv$richness),     l = "taxa found"),
    list(v = fmt_n(sv$ept_richness), l = "EPT taxa (clean-water)"),
    list(v = pct(sv$pct_ept_ind),    l = "EPT share of individuals"),
    list(v = fmt_n(sv$density),      l = "density index / m²"),
    list(v = if (isTRUE(sv$small_n) || is.null(sv$rarefied) || is.na(sv$rarefied)) "n/a" else fmt_n(sv$rarefied), l = "rarefied richness (to 100)"),
    list(v = sprintf("%s / %s", fmt_n(sv$n_bouts), fmt_n(sv$n_samples)), l = "bouts / samples"))
  for (i in seq_along(cells)) {
    ci <- (i - 1) %% 3; ri <- (i - 1) %/% 3
    x0 <- L + ci * gw; yt <- gy - ri * (gh + 2.5)
    graphics::rect(x0, yt - gh, x0 + gw - 2.5, yt, col = tile_bg, border = line)
    graphics::rect(x0, yt - 0.9, x0 + gw - 2.5, yt, col = teal, border = NA)
    txt(x0 + 2.6, yt - 5.2, cells[[i]]$v, cex = 1.45, col = ink, font = 2)
    txt(x0 + 2.6, yt - 8.8, cells[[i]]$l, cex = 0.7, col = muted)
  }

  # ---- the story ----
  sy <- gy - 2 * (gh + 2.5) - 5
  txt(L, sy, "THE STORY SO FAR", cex = 0.8, col = teal2, font = 2); sy <- sy - 3.2
  bullets <- c(
    sprintf("Over %s, NEON ran %s collection bouts (%s samples) at this %s and found %s taxa, an estimated %s individuals.",
            yl %||% "its record", fmt_n(sv$n_bouts), fmt_n(sv$n_samples), tolower(typ), fmt_n(sv$richness), fmt_n(sv$total_ind)),
    sprintf("EPT (mayflies, stoneflies, caddisflies) make up %s of individuals and %s of taxa (%s EPT taxa).%s",
            pct(sv$pct_ept_ind), pct(sv$pct_ept_taxa), fmt_n(sv$ept_richness),
            if (identical(sv$type, "lake")) " This is a lake, naturally EPT-poor — read low EPT as the ecosystem, not impairment." else ""),
    if (!isTRUE(sv$small_n) && !is.null(sv$chao1) && !is.na(sv$chao1))
      sprintf("Sampling found %s taxa; Chao1 estimates at least %s really use the site, so roughly %s remain undetected.",
              fmt_n(sv$richness), fmt_n(sv$chao1), fmt_n(max(0, round(as.numeric(sv$chao1) - as.numeric(sv$richness)))))
    else "Standardized richness (rarefied / Chao1) is suppressed here: the count is too small to estimate it honestly.",
    sprintf("Densest taxon: %s. Midges (Chironomidae) are %s and worms (Oligochaeta) %s of individuals — the more tolerant groups.",
            sv$top_taxon %||% "—", pct(sv$pct_chiro), pct(sv$pct_oligo)))
  for (b in bullets) {
    graphics::points(L + 0.7, sy - 0.9, pch = 19, cex = 0.45, col = teal)
    sy <- para(L + 2.6, sy, b, maxw = R - (L + 2.6), cex = 0.88, col = ink2, lh = 2.5)
    sy <- sy - 1.6
  }

  # ---- honesty contract box ----
  bt <- 24
  graphics::rect(L, 6, R, bt, col = "#fdf3e2", border = "#f1dcb0")
  graphics::rect(L, 6, L + 0.9, bt, col = gold, border = NA)
  txt(L + 3, bt - 3, "HOW TO READ THESE NUMBERS", cex = 0.78, col = gold, font = 2)
  contract <- paste(
    "Density is a within-site standardized index (individuals / m²), valid for trends within one site, within a habitat and sampler type — never an absolute population.",
    INV_DISCLAIMER)
  para(L + 3, bt - 6.4, contract, maxw = R - (L + 3) - 1, cex = 0.8, col = "#6b4a16", lh = 2.3)

  # ---- footer ----
  txt(L, 3, "Built by Desert Data Labs · Tucson, AZ", cex = 0.78, col = muted)
  txt(R, 3, "Not affiliated with NEON, Battelle, or the NSF", cex = 0.74, col = muted, adj = 1)
  invisible(TRUE)
}

# the download target: open a US-Letter PDF and draw the page.
inv_report_pdf <- function(file, meta, site_code, label) {
  grDevices::pdf(file, width = 8.5, height = 11, bg = "#f8fdfd", pointsize = 11)
  on.exit(grDevices::dev.off(), add = TRUE)
  if (is.null(meta)) {
    graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new()
    graphics::text(0.5, 0.5, "No site loaded.", cex = 1.2, col = "#5d7c84")
  } else .inv_report_draw(meta, site_code, label)
  invisible(file)
}
