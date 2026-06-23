# NEON My Little Inverts — data takeaways (recomputed from the bundles)

`DP1.20120.001` Macroinvertebrate collection. Every number below was recomputed
directly from `data/site_index.rds` / `data/sites/*.rds` (the audit script
`_audit.R`), not copied from prose. 34 aquatic sites, 830 bouts, 6,430 benthic
samples, 2014–2024.

## The headline, audited

Water type is the dominant structure in this product — far stronger than any
geographic gradient:

| Water type | mean %EPT (individuals) | mean EPT taxa | mean richness |
|---|---|---|---|
| stream | **28.0%** | 77 | 292 |
| river  | 9.4%  | 44 | 267 |
| lake   | **6.2%** | 28 | 227 |

**Lakes are genuinely EPT-poor in the data** (6.2% EPT individuals, 4–14% across
the 7 lakes), so the app's "low lake EPT is the ecosystem, not impairment"
caption is grounded, not boilerplate. Stoneflies and most mayflies want flowing,
oxygen-rich riffles; lakes don't have them. **Streams and lakes are not directly
comparable on EPT metrics** — the cross-site tab colours by water type so this is
always visible.

## The geographic gradient is weak — so don't sell one

- EPT richness ~ **latitude**: Spearman ρ ≈ **−0.01** (no gradient at all).
- EPT richness ~ **elevation**: ρ ≈ **+0.21** (weak; cold high-elevation streams
  trend richer in EPT).
- %EPT ~ latitude: ρ ≈ −0.08 (none).

LESSON applied: the cross-site x-axis **defaults to elevation** (the only axis
with any signal), and the caption frames it as space-for-time and **confounded
by water type**. A latitude default would imply a gradient the data does not
show. The real read on that tab is the stream/river/lake colour split, not the
x-axis slope.

## Richest EPT sites (all cold/clean streams)

HOPB (MA, 133 EPT taxa), LECO (TN, 126), POSE (VA, 126), MCRA (OR, 122), MART
(WA, 119). These are exactly the forested, cool, well-oxygenated streams EPT
favour — a good sanity check that the EPT computation behaves.

## Density is heavy-tailed and is an INDEX

Site mean density ranges from ~1,500 to ~48,000 /m² across the 34 sites — orders
of magnitude. It is `estimatedTotalCount / benthicArea`, a **within-site
standardized density index**, not a population: it depends on sampler type and
habitat as much as on biology. The app log-scales it on the density board and the
cross-site tab, and labels it an index everywhere.

## small_n suppression

At the **site** level, 0 of 34 sites are small_n-suppressed (every site has
plenty of individuals pooled). At the **bout** level, a handful of low-count
bouts per site are flagged (the `low_count` QC count) and their rarefied richness
/ Chao1 are suppressed rather than shown with false precision.

## QC flag base rates (a real signal, not dead code)

The 8 site-level QC counts fire at honest rates on real data — e.g. SYCA:
`single_dom` 7, `coarse_tax` 47, `ept_unclass` 68, plus the mixed-method and
low-count notes. `no_density` (HIGH) is rare (SYCA = 1). These are "verify, not
wrong" flags surfaced on the taxon profile, each clickable to the offending
sample rows. No flag is hardwired to fire zero times.
