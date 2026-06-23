# ===========================================================================
# build_inv_data.R  —  the SINGLE BUILDER for NEON My Little Inverts
# (DP1.20120.001, Macroinvertebrate collection).
#
# Reads the fetched raw stack (../inverts-data-fetch/DP1.20120.001_all.rds) and
# precomputes everything the app loads at boot — NO live fetch at runtime.
#
# Implements the Aquatics science spec (Barbour et al. 1999 RBP; NEON aquatic
# macroinvertebrate tutorial; Chao 1984; Hurlbert 1971 rarefaction):
#   * THE honest unit = density (individuals/m2), estimatedTotalCount/benthicArea,
#     joined on sampleID (NEVER collectDate), aggregated sample -> bout -> site.
#   * Metrics: richness, EPT richness, %EPT (individuals + taxa), Hill q0/q1/q2,
#     composition surrogates (%Chironomidae/%Oligochaeta/%dominant), rarefied
#     richness + Chao1 with a hard small-n gate.
#   * NO biotic-index score / pass-fail (no calibrated reference condition).
#   * 9 QC flags.
#
# Outputs: data/sites/<SITE>.rds  (list: samples, bouts, taxa, meta)
#          data/site_index.rds    (one row per site)
#          data/cross_site.rds     (cross-site gradient table)
#          data-sample/demo.rds    (the default site bundle)
#
# Run:  "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" scripts/build_inv_data.R
# ===========================================================================
suppressWarnings(suppressMessages({ library(dplyr) }))

RAW <- "../inverts-data-fetch/DP1.20120.001_all.rds"
stopifnot(file.exists(RAW))
d  <- readRDS(RAW)
fd <- d$inv_fieldData
tp <- d$inv_taxonomyProcessed

dir.create("data/sites", recursive = TRUE, showWarnings = FALSE)
dir.create("data-sample", showWarnings = FALSE)

EPT_ORDERS <- c("Ephemeroptera", "Plecoptera", "Trichoptera")
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a
mode_chr <- function(x){ x <- x[!is.na(x)]; if(!length(x)) return(NA_character_); names(sort(table(x), decreasing = TRUE))[1] }

# ---- per-sample field context (sampleID is the join key) ------------------
fld <- fd %>%
  dplyr::transmute(
    sampleID, siteID, eventID,
    collectDate = as.Date(substr(as.character(collectDate), 1, 10)),
    year = as.integer(format(collectDate, "%Y")),
    namedLocation, lat = decimalLatitude, lng = decimalLongitude,
    elevation = suppressWarnings(as.numeric(elevation)),
    aquaticSiteType, habitatType, samplerType,
    benthicArea = suppressWarnings(as.numeric(benthicArea)),
    sampleCondition) %>%
  dplyr::filter(!is.na(sampleID))

# ---- collapse taxonomy to one row per (sampleID, taxon) --------------------
# (a sample x taxon can span several size-class rows; pool the expanded counts)
tax <- tp %>%
  dplyr::filter(!is.na(sampleID), !is.na(acceptedTaxonID)) %>%
  dplyr::group_by(sampleID, acceptedTaxonID) %>%
  dplyr::summarise(
    scientificName = mode_chr(scientificName),
    taxonRank      = mode_chr(taxonRank),
    order          = mode_chr(order),
    family         = mode_chr(family),
    class          = mode_chr(class),
    subclass       = mode_chr(subclass),
    individualCount   = sum(suppressWarnings(as.numeric(individualCount)),   na.rm = TRUE),
    estimatedTotalCount = sum(suppressWarnings(as.numeric(estimatedTotalCount)), na.rm = TRUE),
    subsamplePercent  = suppressWarnings(min(as.numeric(subsamplePercent), na.rm = TRUE)),
    .groups = "drop") %>%
  dplyr::mutate(
    subsamplePercent = ifelse(is.finite(subsamplePercent), subsamplePercent, NA_real_),
    is_ept = order %in% EPT_ORDERS,
    # QC-2: an expansion can never SHRINK the count; fall back to individualCount
    est = ifelse(is.na(estimatedTotalCount) | estimatedTotalCount < individualCount,
                 individualCount, estimatedTotalCount))

# join the field context onto every taxon row
obs <- tax %>% dplyr::inner_join(fld, by = "sampleID")

# ---- per-sample totals + density ------------------------------------------
samp <- obs %>%
  dplyr::group_by(sampleID, siteID, eventID, collectDate, year, habitatType, samplerType,
                  benthicArea, lat, lng, elevation, aquaticSiteType, namedLocation, sampleCondition) %>%
  dplyr::summarise(
    total_est = sum(est, na.rm = TRUE),
    n_taxa    = dplyr::n_distinct(acceptedTaxonID),
    .groups = "drop") %>%
  dplyr::mutate(density_m2 = ifelse(!is.na(benthicArea) & benthicArea > 0, total_est / benthicArea, NA_real_))

# ---------------------------------------------------------------------------
# estimators (vegan-free, precomputed once so the deploy stays lean)
# ---------------------------------------------------------------------------
# Chao1 (bias-corrected, Chao 1984): S_obs + f1(f1-1) / (2(f2+1))
chao1 <- function(counts){
  counts <- round(counts[counts > 0]); S <- length(counts)
  f1 <- sum(counts == 1); f2 <- sum(counts == 2)
  est <- S + (f1 * (f1 - 1)) / (2 * (f2 + 1))
  v   <- f1*(f1-1)/(2*(f2+1)) + f1*(2*f1-1)^2/(4*(f2+1)^2) + f1^2*f2*(f1-1)^2/(4*(f2+1)^4)
  se  <- suppressWarnings(sqrt(v))
  list(chao1 = est, se = ifelse(is.finite(se), se, NA_real_), S_obs = S, f1 = f1, f2 = f2)
}
# individual-based rarefaction to m (Hurlbert 1971): E[S_m]
rarefy_to <- function(counts, m){
  counts <- round(counts[counts > 0]); N <- sum(counts)
  if (N < m || m < 1) return(NA_real_)
  sum(1 - exp(lchoose(N - counts, m) - lchoose(N, m)))
}
# Hill numbers on proportions (q = 0 richness, 1 = expShannon, 2 = invSimpson)
hill <- function(counts){
  p <- counts[counts > 0]; p <- p / sum(p)
  q0 <- length(p)
  q1 <- exp(-sum(p * log(p)))
  q2 <- 1 / sum(p^2)
  c(q0 = q0, q1 = q1, q2 = q2)
}

MIN_IND <- 100L   # small-n gate: pooled individuals
MIN_SAMP <- 3L    # small-n gate: samples per unit

# assemblage metrics on a pooled (taxon -> est) vector + n_samples
assemblage <- function(df, n_samples){
  agg <- df %>% dplyr::group_by(acceptedTaxonID, is_ept, family, class) %>%
    dplyr::summarise(est = sum(est, na.rm = TRUE), .groups = "drop")
  est <- agg$est; tot <- sum(est)
  S   <- nrow(agg)
  S_ept <- sum(agg$is_ept)
  pct_ept_ind  <- if (tot > 0) 100 * sum(est[agg$is_ept]) / tot else NA_real_
  pct_ept_taxa <- if (S   > 0) 100 * S_ept / S else NA_real_
  hl <- hill(est)
  pct_dom   <- if (tot > 0) 100 * max(est) / tot else NA_real_
  pct_chiro <- if (tot > 0) 100 * sum(est[agg$family %in% "Chironomidae"]) / tot else NA_real_
  pct_oligo <- if (tot > 0) 100 * sum(est[agg$class  %in% "Oligochaeta"])  / tot else NA_real_
  small_n   <- (round(tot) < MIN_IND) || (n_samples < MIN_SAMP)
  ch <- chao1(est)
  rar_to <- max(MIN_IND, 0)                                  # rarefy to 100 individuals (the gate floor)
  data.frame(
    richness = S, ept_richness = S_ept,
    pct_ept_ind = round(pct_ept_ind, 1), pct_ept_taxa = round(pct_ept_taxa, 1),
    hill_q1 = round(hl["q1"], 2), hill_q2 = round(hl["q2"], 2),
    pct_dominant = round(pct_dom, 1), pct_chironomidae = round(pct_chiro, 1),
    pct_oligochaeta = round(pct_oligo, 1),
    total_individuals = round(tot),
    chao1 = if (small_n) NA_real_ else round(ch$chao1, 1),
    chao1_se = if (small_n) NA_real_ else round(ch$se, 1),
    rarefied_richness = if (small_n) NA_real_ else round(rarefy_to(est, rar_to), 1),
    small_n = small_n, row.names = NULL)
}

# ---- per-bout metrics (eventID is the bout) -------------------------------
bout_ids <- unique(obs$eventID[!is.na(obs$eventID)])
bouts <- do.call(rbind, lapply(bout_ids, function(ev){
  o  <- obs[obs$eventID == ev, , drop = FALSE]
  ss <- samp[samp$eventID == ev, , drop = FALSE]
  ns <- nrow(ss)
  am <- assemblage(o, ns)
  data.frame(
    eventID = ev, siteID = o$siteID[1],
    collectDate = min(ss$collectDate, na.rm = TRUE), year = o$year[1],
    n_samples = ns,
    habitatType = mode_chr(ss$habitatType), samplerType = mode_chr(ss$samplerType),
    mixed_habitat = dplyr::n_distinct(ss$habitatType) > 1,
    mixed_sampler = dplyr::n_distinct(ss$samplerType) > 1,
    density_m2 = round(mean(ss$density_m2, na.rm = TRUE), 1),
    am, row.names = NULL)
}))
bouts <- bouts[order(bouts$siteID, bouts$collectDate), ]

# ---------------------------------------------------------------------------
# QC flags (per the spec) — counted per site, listed per sample for the inspector
# ---------------------------------------------------------------------------
samp_qc <- samp %>% dplyr::left_join(
  obs %>% dplyr::group_by(sampleID) %>%
    dplyr::summarise(min_subsample = suppressWarnings(min(subsamplePercent, na.rm = TRUE)),
                     pct_dom = { e <- tapply(est, acceptedTaxonID, sum); if (sum(e)>0) 100*max(e)/sum(e) else NA_real_ },
                     coarse_share = { e <- tapply(est, taxonRank %in% c("genus","species","subspecies","speciesGroup","subgenus"), sum)
                                      tot <- sum(e); if (tot>0) 100*(tot - sum(e[names(e)=="TRUE"]))/tot else NA_real_ },
                     ept_unclass = any(is.na(order) & est > 0),
                     .groups = "drop"),
  by = "sampleID") %>%
  dplyr::mutate(min_subsample = ifelse(is.finite(min_subsample), min_subsample, NA_real_))

# ---- assemble per-site bundles + the site index ---------------------------
sites <- sort(unique(fld$siteID))
idx_rows <- list()
for (s in sites) {
  o  <- obs[obs$siteID == s, , drop = FALSE]
  sb <- bouts[bouts$siteID == s, , drop = FALSE]
  sp <- samp[samp$siteID == s, , drop = FALSE]
  sq <- samp_qc[samp_qc$siteID == s, , drop = FALSE]
  if (!nrow(o) || !nrow(sb)) next

  # site-pooled assemblage (all samples) for the headline richness/EPT
  site_am <- assemblage(o, nrow(sp))

  # taxa board: per-taxon mean density across samples + ubiquity
  tax_dens <- o %>%
    dplyr::mutate(dens = ifelse(!is.na(benthicArea) & benthicArea > 0, est / benthicArea, NA_real_)) %>%
    dplyr::group_by(acceptedTaxonID, scientificName, order, family, is_ept) %>%
    dplyr::summarise(mean_density = round(mean(dens, na.rm = TRUE), 2),
                     total_est = round(sum(est, na.rm = TRUE)),
                     n_samples_present = dplyr::n_distinct(sampleID), .groups = "drop") %>%
    dplyr::mutate(ubiquity = round(100 * n_samples_present / dplyr::n_distinct(sp$sampleID))) %>%
    dplyr::arrange(dplyr::desc(mean_density))

  modal_hab <- mode_chr(sp$habitatType); modal_samp <- mode_chr(sp$samplerType)
  yrs <- range(sp$year, na.rm = TRUE)
  qc <- list(
    no_density   = sum(is.na(sq$benthicArea) | sq$benthicArea <= 0),
    heavy_subsmp = sum(sq$min_subsample < 5, na.rm = TRUE),
    single_dom   = sum(sq$pct_dom > 80, na.rm = TRUE),
    coarse_tax   = sum(sq$coarse_share > 30, na.rm = TRUE),
    ept_unclass  = sum(sq$ept_unclass, na.rm = TRUE),
    mixed_sampler= as.integer(dplyr::n_distinct(sp$samplerType) > 1),
    mixed_habitat= as.integer(dplyr::n_distinct(sp$habitatType) > 1),
    low_count    = sum(sb$small_n, na.rm = TRUE))

  meta <- list(
    site = s, name = NA_character_,                       # name filled from site_metadata in the app
    aquaticSiteType = mode_chr(sp$aquaticSiteType),
    lat = round(mean(sp$lat, na.rm = TRUE), 4), lng = round(mean(sp$lng, na.rm = TRUE), 4),
    elevation = round(mean(sp$elevation, na.rm = TRUE)),
    n_bouts = nrow(sb), n_samples = nrow(sp),
    year_min = yrs[1], year_max = yrs[2],
    modal_habitat = modal_hab, modal_sampler = modal_samp,
    density_m2 = round(mean(sb$density_m2, na.rm = TRUE), 1),
    richness = site_am$richness, ept_richness = site_am$ept_richness,
    pct_ept_ind = site_am$pct_ept_ind, pct_ept_taxa = site_am$pct_ept_taxa,
    hill_q1 = site_am$hill_q1, pct_dominant = site_am$pct_dominant,
    pct_chironomidae = site_am$pct_chironomidae, pct_oligochaeta = site_am$pct_oligochaeta,
    rarefied_richness = site_am$rarefied_richness, chao1 = site_am$chao1, chao1_se = site_am$chao1_se,
    total_individuals = site_am$total_individuals, small_n = site_am$small_n,
    top_taxon = tax_dens$scientificName[1], qc = qc, built = as.character(Sys.Date()))

  saveRDS(list(samples = sp, bouts = sb, taxa = tax_dens, qc_samples = sq, meta = meta),
          file.path("data/sites", paste0(s, ".rds")))

  idx_rows[[s]] <- data.frame(
    site = s, aquaticSiteType = meta$aquaticSiteType, lat = meta$lat, lng = meta$lng,
    elevation = meta$elevation, n_bouts = meta$n_bouts, n_samples = meta$n_samples,
    year_min = meta$year_min, year_max = meta$year_max,
    density_m2 = meta$density_m2, richness = meta$richness, ept_richness = meta$ept_richness,
    pct_ept_ind = meta$pct_ept_ind, pct_ept_taxa = meta$pct_ept_taxa, hill_q1 = meta$hill_q1,
    rarefied_richness = meta$rarefied_richness, chao1 = meta$chao1,
    pct_chironomidae = meta$pct_chironomidae, top_taxon = meta$top_taxon, row.names = NULL)
}
site_index <- do.call(rbind, idx_rows)
saveRDS(site_index, "data/site_index.rds")
saveRDS(site_index, "data/cross_site.rds")   # cross-site gradient = the same one-row-per-site table

# demo = SYCA (Sycamore Creek, AZ — the suite's house default + the original app's site)
demo_site <- if (file.exists("data/sites/SYCA.rds")) "SYCA" else site_index$site[which.max(site_index$n_bouts)]
file.copy(file.path("data/sites", paste0(demo_site, ".rds")), "data-sample/demo.rds", overwrite = TRUE)

cat(sprintf("Built %d site bundles. demo = %s.\n", nrow(site_index), demo_site))
cat(sprintf("Sites with EPT: %d/%d. Density range: %.1f - %.1f /m2.\n",
            sum(site_index$ept_richness > 0), nrow(site_index),
            min(site_index$density_m2, na.rm = TRUE), max(site_index$density_m2, na.rm = TRUE)))
print(utils::head(site_index[order(-site_index$ept_richness),
        c("site","aquaticSiteType","n_bouts","density_m2","richness","ept_richness","pct_ept_ind","rarefied_richness","chao1")], 8))
