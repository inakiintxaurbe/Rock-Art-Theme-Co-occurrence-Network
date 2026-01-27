# Rock-Art-Theme-Co-occurrence-Network
**Created by**: Iñaki Intxaurbe Alberdi

*Department of Graphic Design and Engineering Projects*

*(Universidad del País Vasco/Euskal Herriko Unibertsitatea)*

*PACEA UMR 5199*

*(Université du Bordeaux)*

**e-mail**: inaki.intxaurbe@ehu.eus; inaki.intxaurbe@u-bordeaux.fr; inaki.intxaurbe@gmail.com

**ORDICD nº**: https://orcid.org/0000-0003-3643-3177

**Date**: 2026-01-04

*Copyright (C) 2026  Iñaki Intxaurbe*

## Overview

This repository contains an R script designed to generate a theme co-occurrence network from Paleolithic cave art data. The resulting networks are exported in formats compatible with Gephi for further exploration, visualization, and network analysis.

The script downloads an Excel dataset from another GitHub repository (https://github.com/inakiintxaurbe/spatial-organization-patterns-related-to-magdalenian-cave-art), processes thematic information at the panel level, and produces weighted networks based on shared thematic occurrences.

## Purpose of the Script

The main objectives of this script are:

--> Automatically download an Excel data table from a GitHub repository

--> Extract **panels** from GU codes

--> Compute **theme** co-occurrence per panel (*weighted by frequency*)

--> Generate multiple network representations:

--------> Theme–Theme co-occurrence network

--------> Theme–Theme network with *Jaccard* similarity

--------> Panel–Theme bipartite network (optional)

--> Export all outputs in formats ready for their edition in **Gephi**.

## Output Structure

The script automatically creates a directory named:

```gephi_exports/```

The following files are generated inside this folder:

| File                                     | Description                                  |
| ---------------------------------------- | -------------------------------------------- |
| `gephi_nodes_theme.csv`                  | Theme node list                              |
| `gephi_edges_theme_cooc.csv`             | Theme–Theme edges (weighted co-occurrence)   |
| `gephi_edges_theme_weighted_jaccard.csv` | Theme–Theme edges with Jaccard index         |
| `gephi_nodes_panel_theme_bipartite.csv`  | Nodes for Panel–Theme bipartite network      |
| `gephi_edges_panel_theme_bipartite.csv`  | Edges for Panel–Theme bipartite network      |
| `theme_cooc_network.graphml`             | Network file ready to open directly in Gephi |
| `nodes_theme.csv`                        | It's used to filter the network              |
| `bip_edges_panel_theme.csv`              | Panel-Theme edges for global network         |
| `bip_nodes_panel_theme.csv`              | Panel-Theme nodes for global network         |

The dataset is downloaded automatically from the following GitHub repository:

```https://github.com/inakiintxaurbe/spatial-organization-patterns-related-to-magdalenian-cave-art```

## Requirements
--> Software

--------> **R** (version 4.0 or higher recommended)

--------> **Gephi** (optional, for network visualization)


## Methodological Notes

**Panels** are defined as the first three hierarchical levels of the GU code
(e.g. ```S.E.II.01``` → ```S.E.II```)

Theme co-occurrence is calculated based on **shared presence within panels**.

Edge weights represent the number of panels in which two themes co-occur

The **Jaccard similarity index** is calculated as:

<img width="475" height="119" alt="image" src="https://github.com/user-attachments/assets/065e9a93-dc0c-48dc-b4b4-fa742a72854d" />

where <img width="42" height="35" alt="image" src="https://github.com/user-attachments/assets/cf5c8891-542b-4287-9884-f45234c1c37f" /> and <img width="40" height="33" alt="image" src="https://github.com/user-attachments/assets/4c68f1ac-53d2-48e3-8daa-2b180b76f6d7" /> are the number of panels in which each theme appears.
