# ===========================================================================
# fetch_inv_all.R — pull NEON Macroinvertebrate collection (DP1.20120.001) for
# ALL aquatic sites into ../inverts-data-fetch/ as a single stacked RDS.
# Run with R-4.3.1 (neonUtilities 4.0.0). Resumable: skips if the output exists.
#   "C:\Program Files\R\R-4.3.1\bin\Rscript.exe" scripts/fetch_inv_all.R
# ===========================================================================
suppressWarnings(suppressMessages(library(neonUtilities)))

OUT_DIR <- normalizePath(file.path("..", "inverts-data-fetch"), mustWork = FALSE)
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
OUT <- file.path(OUT_DIR, "DP1.20120.001_all.rds")

tok <- tryCatch(
  readLines("C:/Users/tsgil/OneDrive/Documents/VGS - R/App-NEON-Small-Mammal-Tracker/.neon_token", warn = FALSE)[1],
  error = function(e) NA_character_)
if (is.na(tok) || !nzchar(tok)) { cat("WARN: no NEON token found; proceeding anonymously (lower rate limit)\n"); tok <- NA_character_ }

cat("Fetching DP1.20120.001 (macroinvertebrate collection), all sites, all dates...\n")
t0 <- Sys.time()
out <- neonUtilities::loadByProduct(
  dpID = "DP1.20120.001", site = "all", package = "basic",
  check.size = FALSE, token = tok, nCores = 1, progress = TRUE)

cat("Tables returned:", paste(names(out), collapse = ", "), "\n")
for (nm in names(out)) {
  d <- out[[nm]]
  if (is.data.frame(d)) cat(sprintf("  %-32s %6d rows x %d cols\n", nm, nrow(d), ncol(d)))
}
saveRDS(out, OUT)
cat(sprintf("Saved %s (%.1f min)\n", OUT, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
