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
if (!has_users || !has_checksum) stop("manifest.json is missing canonical rsconnect fields — do NOT hand-edit / re-serialize it.")
cat("Manifest OK.\n")
