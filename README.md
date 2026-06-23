# NEON My Little Inverts

An (unofficial) R/Shiny explorer for NEON's **Macroinvertebrate collection**
(`DP1.20120.001`) — the small animals living on the bottom of NEON's streams,
rivers, and lakes (insect larvae, worms, snails, crustaceans). Part of the
Desert Data Labs **NEON series**. Pick one of **34 aquatic sites** and read its
community: how dense it is, how many kinds of animal live there, and how big a
share are the **EPT** groups (mayflies, stoneflies, caddisflies) that need
clean, well-oxygenated water.

This is a from-scratch rebuild of a 2020 single-site prototype; it now covers
all 34 sites, runs on the v2 suite chrome, and ships the suite-standard QC and
honesty layers.

## The honesty contract (read before reading any number)

- **Density is a within-site standardized index** (individuals / m², =
  estimatedTotalCount / benthicArea), **not an absolute population**. It is valid
  for comparing bouts within one site, within a habitat type and sampler type.
  Across-site differences reflect habitat and sampling method as much as biology.
- **No biotic-index / IBI / pass-fail score, no good–fair–poor, no
  impairment or aquatic-life-use call.** NEON aquatic sites have **no calibrated
  reference condition** and no state biotic index, so the app shows within-site
  trends and cross-site *direction* only. (Method: EPA Rapid Bioassessment,
  Barbour et al. 1999.)
- **Lakes are naturally EPT-poor.** Low EPT at a lake is the ecosystem, not
  impairment, and lakes are not directly comparable to streams on EPT metrics.
- **Rarefied richness and Chao1 are suppressed** where the count is too small to
  standardize honestly (< 100 individuals or < 3 samples).

## What's in the app

- A national **picker map** (34 sites, sized by survey effort, coloured by water
  type) with an Explore / About popup, plus a by-name select panel and a browse
  list — all routed through one shared loader, so the sidebar stays in sync.
- The **EPT Pulse** (the signature): each bout's %EPT and density over time,
  marker = habitat, colour = sampler type, flagged bouts greyed.
- A **Taxa Board** pin-card scatter (density × ubiquity, tap to pin a card,
  download with pins baked in), a **Diversity** tab (rarefied richness + the
  composition stack), an **Across the country** cross-site gradient (Spearman ρ
  with CI, space-for-time caveat), a within-site **Map** of the sampled reach,
  and a **Taxon Profile** with a downloadable card.
- A **clickable + downloadable QC inspector** (the 8 site-level checks ranked
  high / warn / info, each opening the offending sample rows + a per-flag CSV).
- Downloads everywhere: per-bout metrics CSV, taxa board CSV, cross-site table
  CSV, a column **codebook**, and a site **report** — a printable one-page
  **PDF** (branded, base-graphics, no pandoc/LaTeX dependency) plus the
  machine-readable one-row-per-metric **CSV**.

## Data pipeline

```
scripts/fetch_inv_all.R     # ONE-TIME / on-demand: pull DP1.20120.001 (all sites)
                            #   -> ../inverts-data-fetch/DP1.20120.001_all.rds
                            #   run with R-4.3.1 (neonUtilities; R-4.5.2 crashes on loadByProduct)
scripts/build_inv_data.R    # the SINGLE BUILDER: raw stack -> data/sites/<SITE>.rds (34),
                            #   data/site_index.rds, data/cross_site.rds, data-sample/demo.rds.
                            #   All metrics (density, richness, EPT, Hill, Chao1, rarefaction,
                            #   composition surrogates, 8 QC counts) are precomputed here.
scripts/build_cross_site.R  # rebuild the index + cross-site table from the committed
                            #   bundles (no fetch) — the fast monthly-refresh rebuild.
scripts/write_manifest.R    # (re)write + CHECK manifest.json for Connect Cloud (lean: no
                            #   neonUtilities / arrow; data.table kept).
```

The deployed app makes **zero** live NEON calls — it reads the committed
`data/sites/*.rds` bundles at boot. `neonUtilities` is referenced by a computed
name in `global.R` so the rsconnect scanner never pins it into the manifest.

## Deploy (Posit Connect Cloud, git-backed)

Connect Cloud watches this repo's `main` branch; **a push IS the deploy**. No
shinyapps secrets, no `rsconnect/`, no `deploy.R`. `.github/workflows/refresh-data.yml`
self-refreshes monthly (first Saturday, off-peak AZ) and pushes the rebuilt
bundle directly to `main`.

**One-time owner setup (the agent can't do these):** create the Connect Cloud
app from this repo, set `APP_URL` in `docs/index.html`, and leave `main`
**unprotected** so the github-actions bot can push the monthly refresh.

## Run locally

```r
# from the app directory, in R-4.5.2 (runs the app fine; only the FETCH needs 4.3.1)
shiny::runApp(".")
```

Data: NEON Macroinvertebrate collection (`DP1.20120.001`). Not affiliated with
NEON, Battelle, or the NSF. An educational data-exploration tool by Desert Data
Labs (Tucson, AZ).
