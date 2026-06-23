# ===========================================================================
# build_cross_site.R — rebuild data/site_index.rds + data/cross_site.rds from
# the COMMITTED per-site bundles (data/sites/*.rds), with NO fetch. This is the
# fast, no-op-safe rebuild the monthly refresh CI runs on the skip_download path
# (and after a re-bundle). The metrics are already precomputed in each bundle's
# meta, so this only re-assembles the one-row-per-site index.
#
#   Rscript scripts/build_cross_site.R
# ===========================================================================
SITE_DIR <- "data/sites"
files <- list.files(SITE_DIR, pattern = "\\.rds$", full.names = TRUE)
stopifnot(length(files) > 0)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

rows <- list()
for (f in files) {
  b <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(b) || is.null(b$meta)) next
  m <- b$meta
  rows[[m$site]] <- data.frame(
    site = m$site, aquaticSiteType = m$aquaticSiteType %||% NA_character_,
    lat = m$lat %||% NA_real_, lng = m$lng %||% NA_real_, elevation = m$elevation %||% NA_real_,
    n_bouts = m$n_bouts %||% NA_integer_, n_samples = m$n_samples %||% NA_integer_,
    year_min = m$year_min %||% NA_integer_, year_max = m$year_max %||% NA_integer_,
    density_m2 = m$density_m2 %||% NA_real_, richness = m$richness %||% NA_real_,
    ept_richness = m$ept_richness %||% NA_real_, pct_ept_ind = m$pct_ept_ind %||% NA_real_,
    pct_ept_taxa = m$pct_ept_taxa %||% NA_real_, hill_q1 = m$hill_q1 %||% NA_real_,
    rarefied_richness = m$rarefied_richness %||% NA_real_, chao1 = m$chao1 %||% NA_real_,
    pct_chironomidae = m$pct_chironomidae %||% NA_real_, top_taxon = m$top_taxon %||% NA_character_,
    row.names = NULL, stringsAsFactors = FALSE)
}
site_index <- do.call(rbind, rows)
site_index <- site_index[order(site_index$site), ]

saveRDS(site_index, "data/site_index.rds")
saveRDS(site_index, "data/cross_site.rds")

# keep the demo bundle pointed at SYCA (the house default), refresh-safe
demo_site <- if (file.exists(file.path(SITE_DIR, "SYCA.rds"))) "SYCA" else site_index$site[which.max(site_index$n_bouts)]
dir.create("data-sample", showWarnings = FALSE)
file.copy(file.path(SITE_DIR, paste0(demo_site, ".rds")), "data-sample/demo.rds", overwrite = TRUE)

cat(sprintf("Rebuilt site_index + cross_site from %d bundles. demo = %s.\n", nrow(site_index), demo_site))
