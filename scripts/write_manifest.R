# ===========================================================================
# write_manifest.R — (re)generate manifest.json for Posit Connect Cloud, then
# CHECK it. Connect Cloud reads the committed manifest, so a stale manifest
# restores the OLD package set; regenerate + commit after any dependency or data
# change.
#
#   Rscript scripts/write_manifest.R
#
# CONTRACT (this is a CHECK-ONLY guard, never a re-serializer):
#   * Run rsconnect::writeManifest() to write the CANONICAL manifest.json
#     (rsconnect's own format — has the top-level "users" key + per-file
#     "checksum"). NEVER re-serialize manifest.json with jsonlite afterwards:
#     that reorders keys + drops the canonical fields, and Connect rejects it as
#     invalid.
#   * READ the written manifest and stop() ONLY if neonUtilities or arrow leak in
#     (those would make the deploy heavy / break it). neonUtilities is referenced
#     by a computed name in global.R, so the static scan should not pick it up.
#   * data.table is a LEGIT plotly dependency and MUST stay — do not flag it.
# ===========================================================================
if (!requireNamespace("rsconnect", quietly = TRUE)) stop("install.packages('rsconnect') first")
if (!requireNamespace("jsonlite",  quietly = TRUE)) stop("install.packages('jsonlite') first")

rsconnect::writeManifest(
  appDir = ".",                   # ui.R + server.R + global.R -> detected as a Shiny app
  appFiles = c(
    "global.R", "ui.R", "server.R",
    list.files("R", full.names = TRUE),
    list.files("www", full.names = TRUE),
    list.files("data", recursive = TRUE, full.names = TRUE),
    list.files("data-sample", full.names = TRUE)
  )
)

# READ-ONLY verification (no re-serialize — the file on disk stays canonical)
m <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
pkgs <- names(m$packages)
cat(sprintf("manifest.json written: %d packages.\n", length(pkgs)))

has_users    <- "users" %in% names(m)
has_checksum <- length(m$files) > 0 && all(vapply(m$files, function(f) "checksum" %in% names(f), logical(1)))
cat(sprintf("canonical format: users key %s · per-file checksum %s\n",
            if (has_users) "present" else "MISSING", if (has_checksum) "present" else "MISSING"))

leak <- pkgs[grepl("neonUtilities|^arrow$", pkgs, ignore.case = TRUE)]
if (length(leak)) {
  stop(sprintf("Heavy package(s) leaked into the manifest: %s. Check the global.R computed-name guard / appFiles.", paste(leak, collapse = ", ")))
}
cat("Good: neonUtilities / arrow are NOT in the manifest (lean deploy).\n")
if ("data.table" %in% pkgs) cat("Note: data.table present (a legit plotly dependency — kept).\n")

# ---- pin terra to the last release before the GDAL-3.8 multidim code (1.8-54) ----
# terra >= 1.8-54 ships gdal_multidimensional.cpp using a GDAL 3.8 call unguarded in
# releases, so it FAILS to compile against Connect Cloud's GDAL 3.4.1. Connect compiles
# from source regardless of repo. 1.8-50 is the last release before 1.8-54: it compiles
# on 3.4.1 and still satisfies raster's terra (>= 1.8-5). terra/raster are install-only
# (leaflet -> raster -> terra; app never calls terra) -> zero runtime impact. Also pin
# the repo to the RSPM jammy binary mirror for suite consistency.
#
# IMPORTANT: this is a TEXT-ONLY edit (readLines/gsub/writeLines). Per this script's
# CONTRACT we must NEVER re-serialize the manifest with jsonlite — that drops the
# canonical "users" key + per-file "checksum" and Connect rejects it. The line-level
# substitutions below preserve the canonical rsconnect format untouched, and the gate
# directly below re-validates those fields after this runs.
if (!is.null(m$packages$terra) &&
    !identical(m$packages$terra$description$Version, "1.8-50")) {
  old_ver <- m$packages$terra$description$Version
  mtxt <- readLines("manifest.json", warn = FALSE)
  # Replace the terra Version and RemoteSha lines only (both carry the bare version
  # string surrounded by quotes, which is unique to terra's block in this manifest).
  mtxt <- gsub(sprintf('"%s"', old_ver), '"1.8-50"', mtxt, fixed = TRUE)
  writeLines(mtxt, "manifest.json")
  m <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
  has_users    <- "users" %in% names(m)
  has_checksum <- length(m$files) > 0 && all(vapply(m$files, function(f) "checksum" %in% names(f), logical(1)))
  cat(sprintf("Pinned terra %s -> 1.8-50 (text edit; canonical format preserved).\n", old_ver))
}
# Swap any CRAN/RSPM-latest repo URLs to the RSPM jammy binary mirror (text-only).
{
  mtxt <- readLines("manifest.json", warn = FALSE)
  before <- mtxt
  mtxt <- gsub("https://cloud.r-project.org", "https://packagemanager.posit.co/cran/__linux__/jammy/latest", mtxt, fixed = TRUE)
  mtxt <- gsub("https://packagemanager.posit.co/cran/latest", "https://packagemanager.posit.co/cran/__linux__/jammy/latest", mtxt, fixed = TRUE)
  if (!identical(before, mtxt)) {
    writeLines(mtxt, "manifest.json")
    m <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
    has_users    <- "users" %in% names(m)
    has_checksum <- length(m$files) > 0 && all(vapply(m$files, function(f) "checksum" %in% names(f), logical(1)))
    cat("Swapped package repo URLs to the RSPM jammy binary mirror.\n")
  }
}

if (!has_users || !has_checksum) stop("manifest.json is missing canonical rsconnect fields — do NOT hand-edit / re-serialize it.")
cat("Manifest OK.\n")
