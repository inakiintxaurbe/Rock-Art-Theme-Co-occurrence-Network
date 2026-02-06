# Rock-Art-Theme-Co-occurrence-Network
**Created by**: Iñaki Intxaurbe Alberdi  

*Department of Graphic Design and Engineering Projects*  

*(Universidad del País Vasco/Euskal Herriko Unibertsitatea)*  

*PACEA UMR 5199* *(Université du Bordeaux)*  

**e-mail**: inaki.intxaurbe@ehu.eus; inaki.intxaurbe@u-bordeaux.fr; inaki.intxaurbe@gmail.com  

**ORCID**: https://orcid.org/0000-0003-3643-3177  

**Date**: 2026-01-04  

*Copyright (C) 2026 Iñaki Intxaurbe*

---

## Overview

This repository contains an R script designed to generate a theme co-occurrence network from Paleolithic cave art data. The resulting networks are exported in formats compatible with Gephi for further exploration, visualization, and network analysis.

The script downloads an Excel dataset from another GitHub repository (https://github.com/inakiintxaurbe/spatial-organization-patterns-related-to-magdalenian-cave-art), processes thematic information at the panel level, and produces weighted networks based on shared thematic occurrences.

Repository structure:
- `Code/` → R script(s)
- `Graphs/` → exported graphs to gephi (see below)

---

## Purpose of the Script

1) Downloads an Excel dataset from a GitHub URL (**editable in the script**).  
2) Extracts Panel IDs from GU codes (e.g. `S.E.II.01` → `S.E.II`).  
3) Builds multiple networks:
   - **Theme–Theme co-occurrence** (binary; by shared panels)
   - **Theme–Theme weighted** (counts motif repetition inside panels)
   - **Jaccard-normalised** Theme–Theme associations
   - **Filtered** network (by min weight + min Jaccard)
   - **MST** (minimum spanning tree) from the filtered network
   - **Panel–Theme bipartite** networks (binary and count-weighted)
4) Computes additional orientation / inclination statistics and exports them as CSV.

---

## Method (formal definitions)

Let \(P=\{p_1,\dots,p_n\}\) be the set of panels and \(T_p\) the set of themes present in panel \(p\).

### Binary thematic co-occurrence (panel sharing)

Binary co-occurrence between themes *i* and *j* is the number of panels where both occur:

$$
C_{ij} = \sum_{p=1}^{n} \mathbf{1}\left(i \in T_p \;\wedge\; j \in T_p\right)
$$

where $\mathbf{1}(\cdot)$ denotes the indicator function. This captures structural association between themes independently of their repetition.

### Weighted co-occurrence (motif repetition within panels)

Let $n_{ip}$ and $n_{jp}$ be the number of occurrences of themes *i* and *j* in panel *p*.  
Weighted co-occurrence is defined as:

$$
W_{ij} = \sum_{p=1}^{n} \left(n_{ip} \cdot n_{jp}\right)
$$

This emphasises panels where themes are repeatedly associated and allows comparison between combinatorial vs. frequency-driven patterns.

### Jaccard normalisation

Associations are normalised using the Jaccard similarity index:

$$
J(i,j) = \frac{C_{ij}}{C_i + C_j - C_{ij}}
$$

where $C_i$ and $C_j$ correspond to the number of panels in which each theme occurs (Jaccard, 1901).  
This reduces bias from highly recurrent motifs and facilitates comparisons across themes with different overall frequencies.

---

## How to run (RStudio)

1. Open the script in `Code/` (RStudio recommended).
2. (Optional) Change the dataset URL in the script:
   ```r
   xlsx_url <- "https://raw.githubusercontent.com/.../Table_DATA.xlsx"
   ```
3. Run the script. It will create an output folder named:
   ```text
   gephi_exports/
   ```

---

## Outputs generated (updated)

All files below are created inside `gephi_exports/`:

| File | Description |
|---|---|
| `gephi_nodes_theme.csv` | Theme nodes (frequency = number of panels per theme) |
| `gephi_edges_theme_weighted_jaccard.csv` | Theme–Theme edges with `Weight = shared_panels` and `Jaccard` |
| `theme_cooc_weight_and_jaccard.graphml` | Theme–Theme network (GraphML; includes weight + Jaccard) |
| `edges_theme_weighted_by_repetition.csv` | Theme–Theme edges with `Weight = Σ(n_ip·n_jp)` |
| `theme_weighted_by_repetition.graphml` | Weighted-by-repetition Theme–Theme network (GraphML) |
| `bip_nodes_panel_theme_1.csv` | Bipartite nodes (binary: presence/absence) |
| `bip_edges_panel_theme_1.csv` | Bipartite edges (binary: presence/absence) |
| `panel_theme_bipartite_1.graphml` | Bipartite (binary) network (GraphML) |
| `bip_nodes_panel_theme_2.csv` | Bipartite nodes (count-weighted) |
| `bip_edges_panel_theme_2.csv` | Bipartite edges with `Weight = n` (motif multiplicity) |
| `panel_theme_bipartite_2.graphml` | Bipartite (count-weighted) network (GraphML) |
| `edges_theme_filtered.csv` | Filtered Theme–Theme edges (thresholds below) |
| `theme_filtered.graphml` | Filtered Theme–Theme network (GraphML) |
| `edges_theme_MST.csv` | MST edges from filtered network |
| `theme_MST.graphml` | MST network (GraphML) |
| `stats_theme_x_orient_global.csv` | Global Theme × Orientation chi-square + Cramer's V |
| `confrontation_horses.csv` | Horse “confrontation” (binomial test) results |
| `per_theme_right_left_binom.csv` | Per-theme left/right binomial tests + BH correction |
| `stats_theme_x_incl_residuals.csv` | Theme × Inclination: standardised residuals |

### Filtering thresholds (editable in the script)

```r
min_weight  <- 3
min_jaccard <- 0.12
```

---

## Figures (Graphs/)

The folder `Graphs/` contains ready-to-use figures derived from the exports:

1. **Non-filtered theme co-occurrence networks**
   - (1a) Frequency-weighted
   - (1b) Jaccard-normalised
2. **Filtered** theme network
3. **MST** (minimum spanning tree)
4. **Panel–Theme bipartite networks**
   - (4a) Binary bipartite (presence/absence)
   - (4b) Count-weighted bipartite (motif multiplicity)

---

## Requirements

- **R** (>= 4.0 recommended)
- Packages used: `readxl`, `dplyr`, `stringr`, `tidyr`, `purrr`, `igraph`, `readr`
- **Gephi** (optional; for visualisation and interactive exploration)

---

## Data source

The script downloads the dataset automatically from:

- https://github.com/inakiintxaurbe/spatial-organization-patterns-related-to-magdalenian-cave-art

You can replace the URL with any **equivalent Excel table** with the same structure (same sheet name and column names expected by the script).

---

## License

AGPL-3.0 *(Citation of the author and repository is **mandatory**)*
