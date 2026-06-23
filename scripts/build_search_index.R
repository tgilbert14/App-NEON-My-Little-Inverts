# ===========================================================================
# build_search_index.R — build the SMALL, PRECOMPUTED "Search the network"
# index from the COMMITTED bundles (data/sites/*.rds + data/site_index.rds).
# NO live fetch. Writes data/search_index.rds, one small file the app loads once
# at boot (like site_index) and filters in memory — instant search, no I/O.
#
# The index is a list:
#   $taxa  — tidy taxon-occurrence: one row per (acceptedTaxonID, site) with the
#            display scientificName, order/family, the EPT flag, the site's mean
#            density for that taxon (individuals/m2 — the app's honest within-site
#            index), ubiquity (% of the site's samples it shows up on), and the
#            site's year_min/year_max. This powers FIND-A-TAXON.
#   $sites — the site-level metric table (reused from site_index) for the
#            THRESHOLD query (EPT richness > X, %EPT > X%).
#   $built — build date stamp.
#
# Run:  "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" scripts/build_search_index.R
# ===========================================================================
suppressWarnings(suppressMessages({ library(dplyr) }))

SITE_DIR <- "data/sites"
files <- list.files(SITE_DIR, pattern = "\\.rds$", full.names = TRUE)
stopifnot(length(files) > 0)

site_index <- readRDS("data/site_index.rds")
EPT_ORDERS <- c("Ephemeroptera", "Plecoptera", "Trichoptera")

# ---- tidy taxon-occurrence table: one row per (taxon, site) ----------------
rows <- list()
for (f in files) {
  b <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(b) || is.null(b$taxa) || !nrow(b$taxa) || is.null(b$meta)) next
  m <- b$meta
  tx <- as.data.frame(b$taxa)
  # EPT from the taxon's order (the bundle carries is_ept already; recompute as a
  # guard so a stale flag can never mislabel the search result).
  is_ept <- (tx$order %in% EPT_ORDERS)
  rows[[m$site]] <- data.frame(
    acceptedTaxonID = tx$acceptedTaxonID,
    scientificName  = tx$scientificName,
    order           = ifelse(is.na(tx$order)  | tx$order  == "", NA_character_, tx$order),
    family          = ifelse(is.na(tx$family) | tx$family == "", NA_character_, tx$family),
    is_ept          = is_ept,
    site            = m$site,
    mean_density    = round(as.numeric(tx$mean_density), 2),   # individuals/m2, within-site index
    ubiquity        = as.numeric(tx$ubiquity),                 # % of samples present
    n_samples_present = as.integer(tx$n_samples_present),
    year_min        = as.integer(m$year_min),
    year_max        = as.integer(m$year_max),
    row.names = NULL, stringsAsFactors = FALSE)
}
taxa <- do.call(rbind, rows)
stopifnot(!is.null(taxa), nrow(taxa) > 0)

# a clean display name (some scientificName are blank -> fall back to the code)
taxa$scientificName <- ifelse(is.na(taxa$scientificName) | taxa$scientificName == "",
                              taxa$acceptedTaxonID, taxa$scientificName)
taxa <- taxa[order(taxa$scientificName, taxa$site), ]
rownames(taxa) <- NULL

# ---- site-level metric table for the THRESHOLD query -----------------------
# Reuse site_index (already one row per site); keep only what the query + jump need.
keep <- intersect(c("site","aquaticSiteType","lat","lng","elevation","n_bouts","n_samples",
                    "year_min","year_max","density_m2","richness","ept_richness",
                    "pct_ept_ind","pct_ept_taxa","hill_q1","rarefied_richness","top_taxon"),
                  names(site_index))
sites <- site_index[, keep, drop = FALSE]
rownames(sites) <- NULL

search_index <- list(taxa = taxa, sites = sites, built = as.character(Sys.Date()))
saveRDS(search_index, "data/search_index.rds", compress = "xz")

sz <- file.info("data/search_index.rds")$size
cat(sprintf("Built search_index.rds: %d taxon-occurrence rows across %d sites, %d distinct taxa. %.1f KB.\n",
            nrow(taxa), length(unique(taxa$site)), length(unique(taxa$acceptedTaxonID)), sz / 1024))
cat(sprintf("EPT occurrence rows: %d. Sites with EPT richness > 10: %d.\n",
            sum(taxa$is_ept), sum(sites$ept_richness > 10, na.rm = TRUE)))
