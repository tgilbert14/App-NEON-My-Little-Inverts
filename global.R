# ===========================================================================
# NEON My Little Inverts — global.R
# A NEONize sibling (Desert Data Labs) for the benthic Macroinvertebrate
# collection (DP1.20120.001). Chrome + bundling spine + pin-card interaction
# ported from the prior siblings (Mosquito Pulse is the v2 reference); the
# analysis layer is benthic-invertebrate native.
#
# Honesty grain: the abundance axis is DENSITY (individuals / m2), a WITHIN-SITE
# standardized density index, never an absolute population. There is NO biotic
# index / IBI / pass-fail — NEON aquatic sites have no calibrated reference
# condition. Lakes are naturally EPT-poor; low EPT is not impairment.
# ===========================================================================
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(bsicons)
  library(dplyr); library(tidyr); library(stringr); library(tibble)
  library(plotly); library(leaflet); library(DT)
  library(shinyjs); library(shinycssloaders); library(RColorBrewer); library(htmltools)
})
source("R/site_metadata.R", local = FALSE)
source("R/inv_helpers.R",   local = FALSE)

NEON_DPID <- "DP1.20120.001"   # Macroinvertebrate collection
# neonUtilities is referenced by a COMPUTED name so the rsconnect static scan
# never pins it into the manifest (the deploy is bundle-only + lean; no live fetch).
.NEON_PKG <- paste0("neon", "Utilities")
LIVE_FETCH <- (Sys.getenv("INV_LIVE", "0") != "0") && requireNamespace(.NEON_PKG, quietly = TRUE)

SITE_DIR  <- "data/sites"
DEMO_PATH <- "data-sample/demo.rds"
DEMO_META <- list(site = "SYCA", label = "SYCA · Sycamore Creek · demo")

# read_bundle(): defensive — NULL on missing/corrupt or an empty bundle, never
# crash boot. A valid invert bundle is a list carrying non-empty $bouts.
read_bundle <- function(f) {
  if (!file.exists(f)) return(NULL)
  out <- tryCatch(readRDS(f), error = function(e) { warning(sprintf("read_bundle('%s'): %s", f, conditionMessage(e))); NULL })
  if (is.null(out) || is.null(out$bouts) || !nrow(out$bouts)) NULL else out
}
load_site_bundle <- function(site) read_bundle(file.path(SITE_DIR, paste0(site, ".rds")))
load_demo <- function() { b <- load_site_bundle(DEMO_META$site); if (!is.null(b)) b else read_bundle(DEMO_PATH) }

SITE_INDEX <- tryCatch(readRDS("data/site_index.rds"), error = function(e) NULL)
CROSS_SITE <- tryCatch(readRDS("data/cross_site.rds"), error = function(e) SITE_INDEX)
BUNDLED <- if (!is.null(SITE_INDEX)) SITE_INDEX$site else character(0)

# ---------------------------------------------------------------------------
# "Search the network" index — one small precomputed .rds loaded ONCE at boot
# (built by scripts/build_search_index.R from the committed bundles, never a live
# fetch). $taxa = tidy one-row-per-(taxon, site) occurrence table; $sites = the
# site-level metric table for the threshold query. Searches filter this in
# memory, so the fast bundled load is preserved.
# ---------------------------------------------------------------------------
SEARCH_INDEX <- tryCatch(readRDS("data/search_index.rds"), error = function(e) NULL)
SEARCH_TAXA  <- if (!is.null(SEARCH_INDEX)) SEARCH_INDEX$taxa  else NULL
SEARCH_SITES <- if (!is.null(SEARCH_INDEX)) SEARCH_INDEX$sites else NULL

# the autocomplete vocabulary: one entry per distinct taxon (display -> code-ish
# key uses the scientificName itself; EPT-flagged in the label so the user sees it)
search_taxon_choices <- function() {
  if (is.null(SEARCH_TAXA) || !nrow(SEARCH_TAXA)) return(NULL)
  u <- SEARCH_TAXA[!duplicated(SEARCH_TAXA$scientificName), c("scientificName","is_ept","order")]
  u <- u[order(u$scientificName), ]
  lab <- ifelse(u$is_ept, paste0(u$scientificName, "  · EPT"), u$scientificName)
  setNames(u$scientificName, lab)
}

# join the NEON site metadata (name / state / domain / bio) onto the index. The
# bundle's meta$name is NA-filled at build time, so the app supplies it here.
site_table <- if (length(BUNDLED)) {
  m <- neon_sites[match(BUNDLED, neon_sites$site), ]
  keep <- intersect(c("lat","lng","aquaticSiteType","n_bouts","n_samples","richness","ept_richness",
                      "pct_ept_ind","density_m2","hill_q1","rarefied_richness","top_taxon"), names(SITE_INDEX))
  out <- cbind(m, SITE_INDEX[match(m$site, SITE_INDEX$site), keep])
  # fall back to the index's aquaticSiteType where the metadata table is missing one
  out$type <- ifelse(is.na(out$type), out$aquaticSiteType, out$type)
  out[order(out$name), ]
} else neon_sites[0, ]

NO_DATA <- is.null(SITE_INDEX) || !length(BUNDLED) || !nrow(site_table)

# state choices for the by-name select panel
inv_state_choices <- function() {
  st <- sort(unique(site_table$state)); if (!length(st)) return(NULL)
  setNames(st, sprintf("%s (%d)", state_names[st] %||% st, as.integer(table(site_table$state)[st])))
}
inv_sites_in_state <- function(stt) {
  rows <- site_table[site_table$state == stt, ]; rows <- rows[order(rows$name), ]
  if (!nrow(rows)) return(character(0))
  setNames(rows$site, sprintf("%s · %s", rows$site, rows$name))
}

# ---------------------------------------------------------------------------
# "Riffle & Teal" palette (Vera). A clean water-teal/aqua primary on a cool
# paper page, a kingfisher-blue secondary, and a reserved coral for the high QC
# flag. OLD key names (navy / cardinal / gold / sky / green) are kept and
# REMAPPED so the shared chrome (server.R's DDL$… references, styles.css token
# names) re-themes for free. The DATA palettes (EPT / aquaticSiteType) are LOCKED
# below — they encode data, never theme, and are never aliased to a CSS token.
# ---------------------------------------------------------------------------
DDL <- list(
  paper = "#f8fdfd", bg = "#eef6f7",
  ink = "#102a33", ink2 = "#274a54", muted = "#5d7c84", line = "#cfe4e6",
  teal = "#0e8f9c", teal2 = "#0a6f7a", aqua = "#2bb7c4",
  coral = "#e0524d", coral_ink = "#9c3531",
  # legacy aliases -> riffle-teal, so shared code paths stay on-theme
  navy = "#123640", navy2 = "#1f5560", cardinal = "#0e8f9c",
  gold = "#e08a2b", gold2 = "#9c5d18", sky = "#2bb7c4",
  green = "#3f9e6e", green2 = "#2f7d56", terra = "#0e8f9c", rust = "#0e8f9c")

# ---- LOCKED DATA palettes (data, never theme; never read from var(--…)) ----
# EPT vs other — the headline composition encoding. EPT (sensitive groups) owns
# the brand teal; everything else is a neutral slate.
EPT_COL <- c(EPT = "#0e8f9c", other = "#94a7ad")
ept_col <- function(c) { out <- unname(EPT_COL[c]); ifelse(is.na(out), unname(EPT_COL["other"]), out) }
# aquatic site type — the picker-map + cross-site colour. Fixed legend order.
TYPE_COL <- c(stream = "#0e8f9c", river = "#2f7daa", lake = "#5a8f3e")
type_col <- function(t) { out <- unname(TYPE_COL[t]); ifelse(is.na(out), "#94a7ad", out) }
TYPE_LAB <- c(stream = "Stream", river = "River", lake = "Lake")
# composition stack — EPT teal, midge amber, worm rust, other slate
COMP_COL <- c(EPT = "#0e8f9c", Chironomidae = "#e0a13b", Oligochaeta = "#b06a4a", other = "#aab9bd")
comp_col <- function(c) { out <- unname(COMP_COL[c]); ifelse(is.na(out), "#aab9bd", out) }

app_theme <- bs_theme(version = 5, bg = "#f8fdfd", fg = DDL$ink,
  primary = DDL$teal, secondary = DDL$aqua, success = DDL$green, info = DDL$sky,
  warning = DDL$gold, danger = DDL$coral,
  base_font = font_google("Rubik"), heading_font = font_google("Rubik"), "border-radius" = "10px")

asset_url <- function(path) { f <- file.path("www", path)
  v <- if (file.exists(f)) as.integer(as.numeric(file.mtime(f))) else 0L; sprintf("%s?v=%s", path, v) }
spin <- function(x, img = NULL) shinycssloaders::withSpinner(x, color = DDL$teal, type = 6)
info_pop <- function(title, ..., placement = "auto")
  bslib::popover(tags$span(class = "info-dot", bsicons::bs_icon("info-circle")), ..., title = title, placement = placement)
insight_banner <- function(icon, ..., tone = "navy")
  div(class = paste("chart-insight", paste0("ci-", tone)), bsicons::bs_icon(icon), div(class = "ci-text", ...))
glow_badge <- function(label, color = "#0e8f9c", glow = color)
  span(class = "glow-badge", style = sprintf("color:#fff; background:%s; border-color:%s;", color, color), label)
card_head <- function(icon, title, ...)
  bslib::card_header(class = "with-info", bsicons::bs_icon(icon), tags$span(class = "ch-title", " ", title), ...)
fmt_int <- function(x) format(round(as.numeric(x)), big.mark = ",", trim = TRUE)

# ---------------------------------------------------------------------------
# The suite registry (ONE source of truth, mirrored in docs/index.html). Every
# sibling links to every other; the About "Explore the NEON series" block renders
# this. is-self is flagged by matching dpid to NEON_DPID. Keep in sync when an app
# ships. URLs are the github.io showcase covers (the public front doors).
# ---------------------------------------------------------------------------
SUITE_REGISTRY <- list(
  list(name = "Small Mammal Tracker",  emoji = "\U0001F42D", tag = "tagged rodents, mark-recapture", dpid = "DP1.10072.001", url = "https://tgilbert14.github.io/NEON-Small-Mammal-Tracker-App/"),
  list(name = "Plant Diversity",       emoji = "\U0001F33F", tag = "plots, richness, expected-vs-observed", dpid = "DP1.10058.001", url = "https://tgilbert14.github.io/NEON-Plant-Diversity/"),
  list(name = "Breeding Birds",        emoji = "\U0001F426", tag = "point counts, rarefied richness", dpid = "DP1.10003.001", url = "https://tgilbert14.github.io/NEON-Breeding-Birds/"),
  list(name = "Plant Phenology",       emoji = "\U0001F33C", tag = "leaf-out and flowering timing", dpid = "DP1.10055.001", url = "https://tgilbert14.github.io/NEON-Plant-Phenology-Explorer/"),
  list(name = "Vegetation Structure",  emoji = "\U0001F332", tag = "tree size, basal area, standing stock", dpid = "DP1.10098.001", url = "https://tgilbert14.github.io/NEON-Vegetation-Structure-Explorer/"),
  list(name = "Ground Beetle Tracker", emoji = "\U0001FAB2", tag = "pitfall carabids by site", dpid = "DP1.10022.001", url = "https://tgilbert14.github.io/NEON-Ground-Beetle-Tracker/"),
  list(name = "Water Chemistry",       emoji = "\U0001F4A7", tag = "stream chemistry and conductivity", dpid = "DP1.20093.001", url = "https://tgilbert14.github.io/NEON-WaterChemistry-Analyte-Viewer-App/"),
  list(name = "Mosquito Pulse",        emoji = "\U0001F99F", tag = "CO2-trap mosquitoes, the monsoon pulse", dpid = "DP1.10043.001", url = "https://tgilbert14.github.io/NEON-Mosquito-Pulse/"),
  list(name = "My Little Inverts",     emoji = "\U0001FAB2", tag = "stream and lake bottom-dwellers, EPT", dpid = "DP1.20120.001", url = "https://tgilbert14.github.io/NEON-My-Little-Inverts/"),
  list(name = "Driver Cascade",        emoji = "\U0001F30E", tag = "cross-product synthesis, the master view", dpid = "cascade", url = "https://tgilbert14.github.io/NEON-Driver-Cascade/"))

# ---------------------------------------------------------------------------
# The app mascot — "Riffle," a friendly round mayfly nymph (the EPT poster-bug)
# in the riffle-teal accent. Flat, no gradient, no id (safely reusable as the
# loading spinner, the splash guide, and the celebration hop). Tails are classed
# mascot-ear-l/r so the CSS can wiggle them; eyes blink via mascot-eyes.
# ---------------------------------------------------------------------------
MASCOT_CRITTER <- htmltools::HTML(paste0(
  '<svg class="mascot" viewBox="0 0 120 120" aria-hidden="true">',
  # antennae
  '<g stroke="#2bb7c4" stroke-width="3" stroke-linecap="round" fill="none">',
  '<path d="M52,42 Q47,28 51,20"/><path d="M68,42 Q73,28 69,20"/>',
  '<circle cx="51" cy="20" r="2.5" fill="#7fe0e8" stroke="none"/><circle cx="69" cy="20" r="2.5" fill="#7fe0e8" stroke="none"/></g>',
  # three tails (cerci) — the mayfly signature, the wiggly "ears"
  '<g class="mascot-ear-l" stroke="#0a6f7a" stroke-width="3" stroke-linecap="round" fill="none">',
  '<path d="M48,98 q-10,14 -22,18"/><path d="M52,100 q-6,16 -14,22"/></g>',
  '<g class="mascot-ear-r" stroke="#0a6f7a" stroke-width="3" stroke-linecap="round" fill="none">',
  '<path d="M72,98 q10,14 22,18"/><path d="M68,100 q6,16 14,22"/></g>',
  # middle tail
  '<path d="M60,100 q0,18 0,22" stroke="#0a6f7a" stroke-width="3" stroke-linecap="round" fill="none"/>',
  # legs
  '<g stroke="#0a6f7a" stroke-width="2.6" stroke-linecap="round" fill="none">',
  '<path d="M44,72 q-16,2 -24,-6"/><path d="M44,82 q-15,8 -26,8"/>',
  '<path d="M76,72 q16,2 24,-6"/><path d="M76,82 q15,8 26,8"/></g>',
  # gill plates along the abdomen (faint teal frills)
  '<g fill="#7fe0e8" fill-opacity=".55"><ellipse cx="40" cy="62" rx="6" ry="9"/><ellipse cx="80" cy="62" rx="6" ry="9"/>',
  '<ellipse cx="42" cy="78" rx="5.5" ry="8"/><ellipse cx="78" cy="78" rx="5.5" ry="8"/></g>',
  # body (head + segmented abdomen)
  '<ellipse cx="60" cy="58" rx="20" ry="18" fill="#0e8f9c"/>',
  '<ellipse cx="60" cy="80" rx="16" ry="20" fill="#0e8f9c"/>',
  '<ellipse cx="60" cy="80" rx="9" ry="15" fill="#36b3bd"/>',
  # blush
  '<g fill="#ff9ec4" opacity=".28"><ellipse cx="48" cy="60" rx="6.5" ry="4.5"/><ellipse cx="72" cy="60" rx="6.5" ry="4.5"/></g>',
  # eyes
  '<g class="mascot-eyes"><circle cx="52" cy="55" r="6.5" fill="#0b2a30"/><circle cx="68" cy="55" r="6.5" fill="#0b2a30"/>',
  '<circle cx="50.3" cy="52.8" r="2.3" fill="#ffffff"/><circle cx="66.3" cy="52.8" r="2.3" fill="#ffffff"/></g>',
  '</svg>'))
