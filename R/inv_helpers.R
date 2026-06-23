# ===========================================================================
# NEON My Little Inverts — inv_helpers.R
# Benthic-macroinvertebrate analyses on DP1.20120.001. The honesty backbone: the
# abundance axis is DENSITY = individuals / m2 (estimatedTotalCount / benthicArea),
# a WITHIN-SITE standardized density index, never an absolute population. Metrics
# are PRECOMPUTED at build time (scripts/build_inv_data.R) per sample -> bout ->
# site, so these helpers mostly read + shape the bundle. NO biotic-index / IBI /
# pass-fail score: NEON aquatic sites have no calibrated reference condition.
# (Method: EPA RBP, Barbour et al. 1999; Chao 1984; Hurlbert 1971; Hill numbers.)
# See docs/neonize-playbook.md + docs/DATA-TAKEAWAYS.md.
# ===========================================================================
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
num <- function(x) suppressWarnings(as.numeric(x))
mode_chr <- function(x){ x <- x[!is.na(x)]; if (!length(x)) return(NA_character_); names(sort(table(x), decreasing = TRUE))[1] }

# The disclaimer line carried verbatim near every metric (Aquatics science spec).
INV_DISCLAIMER <- paste0(
  "These are descriptive bioassessment metrics, not a regulatory determination. ",
  "NEON sites have no calibrated reference condition or state biotic index, so the app ",
  "shows within-site trends and cross-site direction only, never a pass/fail score or an ",
  "aquatic-life-use call. (Method: EPA RBP, Barbour et al. 1999.)")

# ---------------------------------------------------------------------------
# site_vectors(): the hero stat band, read straight from meta (precomputed).
# density is the within-site standardized density index (individuals / m2).
# rarefied_richness / chao1 are NA where meta$small_n (insufficient count) — the
# UI grays those out rather than printing false precision.
# ---------------------------------------------------------------------------
site_vectors <- function(meta) {
  if (is.null(meta)) return(NULL)
  list(
    richness     = meta$richness %||% NA_real_,
    ept_richness = meta$ept_richness %||% NA_real_,
    pct_ept_ind  = meta$pct_ept_ind %||% NA_real_,
    pct_ept_taxa = meta$pct_ept_taxa %||% NA_real_,
    density      = meta$density_m2 %||% NA_real_,
    hill_q1      = meta$hill_q1 %||% NA_real_,
    rarefied     = if (isTRUE(meta$small_n)) NA_real_ else (meta$rarefied_richness %||% NA_real_),
    chao1        = if (isTRUE(meta$small_n)) NA_real_ else (meta$chao1 %||% NA_real_),
    chao1_se     = if (isTRUE(meta$small_n)) NA_real_ else (meta$chao1_se %||% NA_real_),
    n_bouts      = meta$n_bouts %||% NA_integer_,
    n_samples    = meta$n_samples %||% NA_integer_,
    total_ind    = meta$total_individuals %||% NA_real_,
    pct_chiro    = meta$pct_chironomidae %||% NA_real_,
    pct_oligo    = meta$pct_oligochaeta %||% NA_real_,
    pct_dom      = meta$pct_dominant %||% NA_real_,
    small_n      = isTRUE(meta$small_n),
    top_taxon    = meta$top_taxon %||% NA_character_,
    type         = meta$aquaticSiteType %||% NA_character_)
}

# year label for a site (from the bouts or meta)
year_label <- function(meta) {
  y0 <- meta$year_min; y1 <- meta$year_max
  if (is.na(y0) || is.na(y1)) return(NA_character_)
  if (y0 == y1) as.character(y0) else sprintf("%d–%d", y0, y1)
}

# ---------------------------------------------------------------------------
# bout_series(): the EPT Pulse signature feed. One row per bout, ready to plot:
# %EPT (pct_ept_ind) and density over time, with habitatType / samplerType /
# small_n carried so the chart can facet, colour, and gray flagged bouts.
# Bouts where a metric is NA (e.g. density with no benthicArea) are kept; the
# plot drops them per-trace so the trajectory never invents a value.
# ---------------------------------------------------------------------------
bout_series <- function(bouts) {
  if (is.null(bouts) || !nrow(bouts)) return(NULL)
  b <- bouts
  b$collectDate <- as.Date(b$collectDate)
  b <- b[order(b$collectDate), , drop = FALSE]
  b$flagged <- isTRUE_vec(b$small_n) | isTRUE_vec(b$mixed_habitat) | isTRUE_vec(b$mixed_sampler)
  b
}
isTRUE_vec <- function(x) { x <- as.logical(x); x[is.na(x)] <- FALSE; x }

# ---------------------------------------------------------------------------
# taxa_board(): the density-vs-ubiquity board. Reads the precomputed taxa table
# (one row per taxon: mean_density, ubiquity, is_ept). The signature scatter axes
# are x = mean_density (log), y = ubiquity, colour = EPT vs other.
# ---------------------------------------------------------------------------
taxa_board <- function(taxa) {
  if (is.null(taxa) || !nrow(taxa)) return(NULL)
  t <- taxa
  t$class <- ifelse(isTRUE_vec(t$is_ept), "EPT", "other")
  t$scientificName <- ifelse(is.na(t$scientificName) | !nzchar(t$scientificName),
                             t$acceptedTaxonID, t$scientificName)
  t[order(-t$mean_density), , drop = FALSE]
}

# composition stack across bouts: %EPT / %Chironomidae / %Oligochaeta / %other
composition_long <- function(bouts) {
  if (is.null(bouts) || !nrow(bouts)) return(NULL)
  b <- bouts; b$collectDate <- as.Date(b$collectDate); b <- b[order(b$collectDate), , drop = FALSE]
  ept   <- num(b$pct_ept_ind);   ept[is.na(ept)]   <- 0
  chiro <- num(b$pct_chironomidae); chiro[is.na(chiro)] <- 0
  oligo <- num(b$pct_oligochaeta);  oligo[is.na(oligo)]  <- 0
  other <- pmax(0, 100 - ept - chiro - oligo)
  data.frame(
    eventID = rep(b$eventID, 4), collectDate = rep(b$collectDate, 4),
    component = factor(rep(c("EPT", "Chironomidae", "Oligochaeta", "other"), each = nrow(b)),
                       levels = c("EPT", "Chironomidae", "Oligochaeta", "other")),
    share = c(ept, chiro, oligo, other), stringsAsFactors = FALSE)
}

# pin-card HTML for a single bout/point on the time-series charts (EPT Pulse,
# density, richness, composition). Vectorized: pass parallel vectors of title /
# tag / stats_html and get one card per point for a trace's customdata. The
# hidden data-tag drives the pin dedupe + the clear-on-data-change signature.
# (No "open profile" chip — a bout isn't a navigable entity, just a pinnable read.)
inv_bout_card <- function(title, tag, stats_html) paste0(
  "<span class='smt-pin-emoji'>\U0001F990</span> <b>", title, "</b>",
  "<span class='smt-tag' data-tag='", tag, "' style='display:none'></span><br/>",
  "<span class='smt-pin-stats'>", stats_html, "</span>",
  "<br/><em class='smt-pin-hint'>Tap the point to pin this card</em>")

# within-site sample-location map feed. NEON aquatic sites sample a single reach,
# so lat/lng are usually one point; we summarise per namedLocation (the reach /
# habitat unit) so multiple stations show separately where they exist.
sample_points <- function(samples) {
  if (is.null(samples) || !nrow(samples)) return(NULL)
  s <- samples
  s$lat <- num(s$lat); s$lng <- num(s$lng); s$density_m2 <- num(s$density_m2)
  s <- s[is.finite(s$lat) & is.finite(s$lng), , drop = FALSE]
  if (!nrow(s)) return(NULL)
  agg <- s %>% dplyr::group_by(.data$namedLocation) %>%
    dplyr::summarise(
      lat = stats::median(.data$lat, na.rm = TRUE),
      lng = stats::median(.data$lng, na.rm = TRUE),
      n_samples = dplyr::n(),
      density_m2 = round(stats::median(.data$density_m2, na.rm = TRUE), 1),
      modal_habitat = mode_chr(.data$habitatType),
      modal_sampler = mode_chr(.data$samplerType),
      .groups = "drop")
  agg$density_m2[!is.finite(agg$density_m2)] <- NA_real_
  agg
}

# rich within-site map popup: site context (water type, domain, years, richness,
# EPT, elevation when present) from meta + neon_sites, plus THIS station's
# habitat / sampler / samples / density, plus a NEON portal deep link. Uses only
# bundled data (no live calls). One HTML string per row of pts. (Atlas)
site_reach_popup <- function(pts, meta, site_code) {
  if (is.null(pts) || !nrow(pts)) return(character(0))
  nm     <- neon_sites[neon_sites$site == site_code, ]
  sname  <- if (nrow(nm)) nm$name[1]   else site_code
  state  <- if (nrow(nm)) nm$state[1]  else ""
  domain <- if (nrow(nm)) nm$domain[1] else ""
  typ    <- TYPE_LAB[meta$aquaticSiteType] %||% meta$aquaticSiteType %||% "site"
  yrs    <- year_label(meta) %||% "—"
  elev   <- suppressWarnings(as.numeric(meta$elevation %||% NA))
  elev_s <- if (is.finite(elev)) sprintf(" · %s m elev", format(round(elev), big.mark = ",")) else ""
  url    <- sprintf("https://www.neonscience.org/field-sites/%s", tolower(site_code))
  rich   <- meta$richness %||% NA; ept <- meta$pct_ept_ind %||% NA; nb <- meta$n_bouts %||% NA
  vapply(seq_len(nrow(pts)), function(i) sprintf(
    paste0(
      "<div style='font-family:Rubik,sans-serif;min-width:236px'>",
      "<b>%s</b> · %s, %s<br>",
      "<span style='color:#5d7c84'>%s · NEON %s%s</span><br>",
      "<span style='color:#5d7c84;font-size:11px'>Reach: %s</span><br>",
      "<b style='color:#0a6f7a'>%s</b> · %s sampler<br>",
      "<b>%s</b> samples · <b>%s</b> bouts · %s<br>",
      "density <b>%s</b>/m² · richness <b>%s</b> · EPT <b>%s%%</b>",
      "<div style='margin-top:7px'><a href='%s' target='_blank' rel='noopener' ",
      "class='btn btn-sm btn-outline-secondary'>View on NEON portal →</a></div></div>"),
    sname, site_code, state,
    typ, domain, elev_s,
    pts$namedLocation[i] %||% "—",
    pts$modal_habitat[i] %||% "habitat n/a", pts$modal_sampler[i] %||% "sampler n/a",
    as.integer(pts$n_samples[i]), if (is.na(nb)) "—" else as.integer(nb), yrs,
    ifelse(is.na(pts$density_m2[i]), "—", as.character(round(pts$density_m2[i]))),
    if (is.na(rich)) "—" else as.character(rich),
    if (is.na(ept)) "—" else sprintf("%.0f", ept), url),
    character(1))
}

# ---------------------------------------------------------------------------
# inv_qc(): the suite-standard data-quality flag system, built from the 8
# precomputed per-site counts (meta$qc) PLUS the exact offending qc_samples rows
# behind each, so the UI can list them (clickable) + download a per-flag CSV.
#   high = the data can't be standardized      (no_density: missing benthicArea)
#   warn = worth a look                        (heavy_subsmp, single_dom, coarse_tax, mixed_sampler)
#   info = a note                              (ept_unclass, mixed_habitat, low_count)
# Flagged rows are RETAINED, never deleted ("verify, not wrong"). Thresholds are
# the build-time defaults (subsample < 5%, dominance > 80%, coarse share > 30%).
# Returns list(flags = ranked list, sets = named list of offending-row frames).
# ---------------------------------------------------------------------------
inv_qc <- function(bundle) {
  out <- list(flags = list(), sets = list())
  if (is.null(bundle)) return(out)
  qc <- bundle$meta$qc; sq <- bundle$qc_samples; bts <- bundle$bouts
  if (is.null(qc)) return(out)
  cnt <- function(k) { v <- suppressWarnings(as.integer(qc[[k]])); if (length(v) && !is.na(v)) v else 0L }

  add <- function(level, title, key, n, detail, rows = NULL) {
    if (is.null(n) || is.na(n) || n <= 0) return(invisible())
    out$flags[[length(out$flags) + 1L]] <<- list(level = level, title = title, key = key, n = as.integer(n), detail = detail)
    out$sets[[key]] <<- rows
  }

  # the qc_samples columns to surface in the inspector tables
  show <- intersect(c("sampleID","eventID","collectDate","year","habitatType","samplerType",
                      "benthicArea","density_m2","n_taxa","total_est","min_subsample","pct_dom",
                      "coarse_share","ept_unclass","sampleCondition","namedLocation"), names(sq))
  sub <- function(idx) if (is.null(idx) || !length(which(idx))) NULL else sq[which(idx), show, drop = FALSE]

  no_density <- is.na(num(sq$benthicArea)) | num(sq$benthicArea) <= 0
  add("high", "No benthic area (can't standardize density)", "no_density", cnt("no_density"),
      "These samples have no usable benthic area, so individuals can't be turned into a density (individuals / m2) and are dropped from the density index. A sample with no sampled area is unusable as an effort denominator.",
      sub(no_density))

  heavy <- num(sq$min_subsample) < 5
  add("warn", "Heavy subsampling (a small fraction picked)", "heavy_subsmp", cnt("heavy_subsmp"),
      "A small fraction of the sample (under 5%) was sorted and scaled up to the whole sample, so the estimated total carries wide uncertainty. The count is an estimate, not a tally.",
      sub(heavy))

  single <- num(sq$pct_dom) > 80
  add("warn", "One taxon dominates the sample", "single_dom", cnt("single_dom"),
      "A single taxon makes up more than 80% of the estimated individuals in these samples. That can be real (a bloom of one midge), but it also flags a possible mis-sort or expansion artifact worth a look.",
      sub(single))

  coarse <- num(sq$coarse_share) > 30
  add("warn", "Coarse identification (much not to species/genus)", "coarse_tax", cnt("coarse_tax"),
      "More than 30% of the estimated individuals in these samples are identified only to family or coarser. Coarse IDs can't enter species-level richness, so they are noted here for transparency.",
      sub(coarse))

  unclass <- isTRUE_vec(sq$ept_unclass)
  add("info", "Unclassified order present", "ept_unclass", cnt("ept_unclass"),
      "Some individuals in these samples have no order recorded, so they can't be tested for EPT membership. They are kept in the density total but excluded from the EPT metrics.",
      sub(unclass))

  # mixed sampler / habitat are site-level booleans (1/0); offending rows = the bouts that mix
  if (cnt("mixed_sampler") > 0 && !is.null(bts)) {
    mb <- bts[isTRUE_vec(bts$mixed_sampler), intersect(c("eventID","collectDate","year","n_samples","habitatType","samplerType"), names(bts)), drop = FALSE]
    add("warn", "Mixed sampler types within a bout", "mixed_sampler", if (nrow(mb)) nrow(mb) else 1L,
        "This site uses more than one sampler type (e.g. Surber, core, kicknet) across its history, and some bouts mix them. Different samplers sweight habitats differently, so compare density within a sampler type, not across.",
        if (nrow(mb)) mb else NULL)
  }
  if (cnt("mixed_habitat") > 0 && !is.null(bts)) {
    mh <- bts[isTRUE_vec(bts$mixed_habitat), intersect(c("eventID","collectDate","year","n_samples","habitatType","samplerType"), names(bts)), drop = FALSE]
    add("info", "Mixed habitat types within a bout", "mixed_habitat", if (nrow(mh)) nrow(mh) else 1L,
        "Some bouts at this site combine more than one habitat (riffle, run, pool, ...). Habitat strongly shapes the community, so a bout-to-bout change can partly reflect which habitat was sampled.",
        if (nrow(mh)) mh else NULL)
  }
  if (cnt("low_count") > 0 && !is.null(bts)) {
    lb <- bts[isTRUE_vec(bts$small_n), intersect(c("eventID","collectDate","year","n_samples","total_individuals","richness"), names(bts)), drop = FALSE]
    add("info", "Low-count bouts (richness suppressed)", "low_count", if (nrow(lb)) nrow(lb) else cnt("low_count"),
        "These bouts have too few individuals or samples (under 100 individuals or 3 samples) for a stable standardized richness, so their rarefied richness and Chao1 are suppressed rather than shown with false precision.",
        if (nrow(lb)) lb else NULL)
  }
  out
}

# tidy combined QC report (one frame across all flags) for the full-report CSV
inv_qc_report <- function(bundle) {
  q <- inv_qc(bundle); if (!length(q$sets)) return(NULL)
  parts <- list()
  for (k in names(q$sets)) {
    st <- q$sets[[k]]; if (is.null(st) || !nrow(st)) next
    st <- as.data.frame(st); st$flag <- k
    parts[[length(parts) + 1L]] <- st
  }
  if (!length(parts)) return(NULL)
  # bind by common columns (the flag-specific frames differ in shape)
  cols <- Reduce(union, lapply(parts, names))
  parts <- lapply(parts, function(d) { for (c in setdiff(cols, names(d))) d[[c]] <- NA; d[, cols, drop = FALSE] })
  do.call(rbind, c(parts, list(make.row.names = FALSE)))
}

# ---------------------------------------------------------------------------
# cross-site inference: Spearman rho + Fisher-z CI for a gradient (one dot per
# site). Space-for-time, correlational — the caption says so. Returns a list the
# annotation builder consumes.
# ---------------------------------------------------------------------------
spearman_ci <- function(x, y) {
  ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]; n <- length(x)
  if (n < 3) return(list(rho = NA_real_, lo = NA_real_, hi = NA_real_, n = n))
  rho <- suppressWarnings(stats::cor(x, y, method = "spearman"))
  if (!is.finite(rho) || abs(rho) >= 1 || n < 5) return(list(rho = rho, lo = NA_real_, hi = NA_real_, n = n))
  z <- atanh(rho); se <- 1.03 / sqrt(n - 3)
  list(rho = rho, lo = tanh(z - 1.96 * se), hi = tanh(z + 1.96 * se), n = n)
}

# ---------------------------------------------------------------------------
# inv_codebook(): machine-readable column dictionary for every CSV download.
# Built by documenting the actual export keep-vectors (units, NA-semantics, the
# density-index caveat). Concatenated from the per-export column groups.
# ---------------------------------------------------------------------------
inv_codebook <- function() {
  bout <- data.frame(
    column = c("eventID","siteID","collectDate","year","n_samples","habitatType","samplerType",
               "mixed_habitat","mixed_sampler","density_m2","richness","ept_richness","pct_ept_ind",
               "pct_ept_taxa","hill_q1","hill_q2","pct_dominant","pct_chironomidae","pct_oligochaeta",
               "total_individuals","chao1","chao1_se","rarefied_richness","small_n"),
    units = c("","","date","year","# samples","","","logical","logical","individuals / m2","# taxa",
              "# EPT taxa","% of individuals","% of taxa","effective # taxa","effective # taxa",
              "%","%","%","# individuals (est.)","# taxa","# taxa","# taxa","logical"),
    description = c(
      "NEON collection-bout identifier (site + collect date). One bout = one site visit's set of samples.",
      "NEON 4-letter aquatic site code.",
      "Date the bout's samples were collected (earliest sample date).",
      "Calendar year of the bout.",
      "Number of benthic samples taken in the bout.",
      "Dominant habitat sampled in the bout (riffle, run, pool, ...). Habitat strongly shapes the community.",
      "Dominant sampler type used (Surber, core, kicknet, ...). Different samplers weight habitats differently; compare within a sampler type.",
      "TRUE if the bout combined more than one habitat type.",
      "TRUE if the bout combined more than one sampler type.",
      "DENSITY = estimated individuals / m2 sampled (estimatedTotalCount / benthicArea), averaged over the bout's samples. A WITHIN-SITE standardized density index, NOT an absolute population. NA where no usable benthic area.",
      "Observed taxon richness (distinct accepted taxa) in the bout.",
      "Number of EPT (Ephemeroptera, Plecoptera, Trichoptera) taxa in the bout.",
      "EPT share of estimated individuals (the lead %EPT metric). Lakes are naturally EPT-poor; low EPT is not impairment.",
      "EPT share of taxa (the richness-weighted companion to pct_ept_ind).",
      "Hill number q=1 (exp Shannon): effective number of common taxa.",
      "Hill number q=2 (inverse Simpson): effective number of dominant taxa.",
      "Share of estimated individuals in the single most abundant taxon.",
      "Chironomidae (non-biting midge) share of estimated individuals — a tolerance / composition surrogate.",
      "Oligochaeta (aquatic worm) share of estimated individuals — a tolerance / composition surrogate.",
      "Estimated total individuals in the bout (subsample counts scaled to the whole sample).",
      "Chao1 asymptotic richness estimate (bias-corrected; Chao 1984). NA where small_n (insufficient count).",
      "Standard error of the Chao1 estimate. NA where small_n.",
      "Richness rarefied to 100 individuals (Hurlbert 1971), comparable across bouts of different effort. NA where small_n.",
      "TRUE if the bout has under 100 individuals or under 3 samples; standardized richness (rarefied / Chao1) is suppressed."),
    stringsAsFactors = FALSE)

  taxa <- data.frame(
    column = c("acceptedTaxonID","scientificName","order","family","is_ept","mean_density",
               "total_est","n_samples_present","ubiquity"),
    units = c("","","","","logical","individuals / m2","# individuals (est.)","# samples","% of samples"),
    description = c(
      "NEON accepted taxon ID.",
      "Accepted scientific name of the taxon.",
      "Taxonomic order.",
      "Taxonomic family.",
      "TRUE if the taxon is EPT (Ephemeroptera, Plecoptera, or Trichoptera) — the pollution-sensitive groups.",
      "Mean density of the taxon across the samples it appears in (individuals / m2). The board's x-axis (log scale).",
      "Estimated total individuals of the taxon across the site (subsample counts scaled up).",
      "Number of samples the taxon was found in.",
      "Ubiquity = % of the site's samples the taxon was found in. The board's y-axis."),
    stringsAsFactors = FALSE)

  cross <- data.frame(
    column = c("site","aquaticSiteType","lat","lng","elevation","n_bouts","n_samples","year_min",
               "year_max","density_m2","richness","ept_richness","pct_ept_ind","pct_ept_taxa","hill_q1",
               "rarefied_richness","chao1","pct_chironomidae","top_taxon"),
    units = c("code","lake/river/stream","°","°","m","# bouts","# samples","year","year",
              "individuals / m2","# taxa","# EPT taxa","% of individuals","% of taxa","effective # taxa",
              "# taxa","# taxa","%",""),
    description = c(
      "Cross-site CSV: NEON 4-letter aquatic site code.",
      "Cross-site CSV: aquatic site type. Lakes are naturally EPT-poor; do not read low lake EPT as impairment.",
      "Cross-site CSV: reach latitude.",
      "Cross-site CSV: reach longitude.",
      "Cross-site CSV: reach elevation (m).",
      "Cross-site CSV: number of collection bouts.",
      "Cross-site CSV: number of benthic samples.",
      "Cross-site CSV: first year sampled.",
      "Cross-site CSV: last year sampled.",
      "Cross-site CSV: site mean density index (individuals / m2). Within-site index; compare sites by direction, not raw value.",
      "Cross-site CSV: site-pooled observed richness.",
      "Cross-site CSV: site-pooled EPT richness.",
      "Cross-site CSV: site EPT share of individuals.",
      "Cross-site CSV: site EPT share of taxa.",
      "Cross-site CSV: site Hill q1 (effective common taxa).",
      "Cross-site CSV: site richness rarefied to 100 individuals (NA where small_n).",
      "Cross-site CSV: site Chao1 asymptotic richness (NA where small_n).",
      "Cross-site CSV: site Chironomidae share of individuals.",
      "Cross-site CSV: the site's top taxon by mean density."),
    stringsAsFactors = FALSE)

  caveat <- data.frame(
    column = "DENSITY_NOTE", units = "",
    description = paste0("density_m2 is a WITHIN-SITE standardized density index (individuals / m2), ",
                         "valid for trends within a site, within a habitat type and sampler type. Across-site ",
                         "differences reflect habitat and method as much as biology. ", INV_DISCLAIMER),
    stringsAsFactors = FALSE)

  rbind(bout, taxa, cross, caveat)
}
