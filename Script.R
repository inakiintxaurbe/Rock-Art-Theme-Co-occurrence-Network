#   THEME CO-OCCURRENCE NETWORKS IN PALAEOLITHIC ROCK ART
#
# --> Download Table_DATA.xlsx from GitHub 
#   (https://github.com/inakiintxaurbe/spatial-organization-patterns-related-to-magdalenian-cave-art) 
#   // THIS CAN BE CHANGED IN ORDER TO PUT THE LINK TO ANOTHER DATA BASE WITH THE SAME STRUCTURE
# --> Extract Panel from GU code (e.g. S.E.II.01 -> S.E.II)
# --> Calculate Theme co-occurrence per panel (Weighted)
# --> Export CSVs to Gephi
# --> Save it in the SAME folder where this script is located.
#
#   Author: Iñaki Intxaurbe Alberdi 
#   Department of Graphic Design and Engineering Projects
#   (Universidad del País Vasco/Euskal Herriko Unibertsitatea)
#   PACEA UMR 5199
#   (Université du Bordeaux)
#   Date: 2026-01-04
#   Copyright (C) 2026 Iñaki Intxaurbe





# Install packages (if necessary) --------------------

pkgs <- c("readxl", "dplyr", "stringr", "tidyr", "purrr", "igraph", "readr")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(igraph)
library(readr)


out_dir <- file.path(getwd(), "gephi_exports")
if (!dir.exists(out_dir)) dir.create(out_dir)


# V GitHub URL -> THE URL CAN BE CHANGED TO ANOTHER LINK CONTAINING A DATABASE WITH THE SAME STRUCTURE --------------------------------------------------------------------------

xlsx_url <- "https://raw.githubusercontent.com/inakiintxaurbe/spatial-organization-patterns-related-to-magdalenian-cave-art/master/2%20STATISTICS/Table_DATA.xlsx" # <-- HERE !!!

# Ʌ GitHub URL -> THE URL CAN BE CHANGED TO ANOTHER LINK CONTAINING A DATABASE WITH THE SAME STRUCTURE --------------------------------------------------------------------------

tmpfile <- file.path(tempdir(), "Table_DATA.xlsx")
download.file(xlsx_url, tmpfile, mode = "wb")

dat <- readxl::read_excel(tmpfile, sheet = "FAMD_and_HCPC")

dat2 <- dat %>%
  transmute(
    GU    = as.character(GU),
    Theme = as.character(Theme)
  ) %>%
  filter(!is.na(GU), GU != "", !is.na(Theme), Theme != "")

# Extract Panel from GU: "X.Y.Z.W" -> "X.Y.Z" -------------------------------------------------
#    E.g.: S.E.II.01 -> S.E.II ----------------------------------------------------------------

dat2 <- dat2 %>%
  mutate(Panel = sub("^([^\\.]+\\.[^\\.]+\\.[^\\.]+).*", "\\1", GU))

# Remove duplicates within the same panel (if a theme appears multiple times in that panel) <-- LEHENENGO ANALISIAN, BIGARRENIAN HAU KENDUKO DA TEMATIKEN PISUAK KALKULATEKO PANEL BAKOTZIAN
dat_panel_theme <- dat2 %>%
  distinct(Panel, Theme)




# Edge list Theme-Theme (co-ocurrencia per panel)
#    weight = nº of panels wher co-ocurrence


panel_to_edges <- function(df_panel) {
  th <- sort(unique(df_panel$Theme))
  if (length(th) < 2) {
    return(tibble(Source = character(), Target = character(), Weight = integer()))
  }
  cmb <- t(combn(th, 2))
  tibble(Source = cmb[, 1], Target = cmb[, 2], Weight = 1L)
}

edges <- dat_panel_theme %>%
  group_by(Panel) %>%
  group_modify(~ panel_to_edges(.x)) %>%
  ungroup() %>%
  group_by(Source, Target) %>%
  summarise(Weight = sum(Weight), .groups = "drop") %>%
  arrange(desc(Weight))




# 6) Node list (Themes)
#    PanelFreq = nº of panels where appears each theme


nodes <- dat_panel_theme %>%
  group_by(Theme) %>%
  summarise(
    Id = first(Theme),
    Label = first(Theme),
    PanelFreq = n_distinct(Panel),
    .groups = "drop"
  ) %>%
  select(Id, Label, PanelFreq) %>%
  arrange(desc(PanelFreq))




# Jaccard metric for each pair of Themes
#    J = shared_panels / (nx + ny - shared_panels)



theme_freq <- dat_panel_theme %>%
  count(Theme, name = "n_panels")

shared <- dat_panel_theme %>%
  inner_join(dat_panel_theme, by = "Panel") %>%
  filter(Theme.x < Theme.y) %>%
  count(Theme.x, Theme.y, name = "shared_panels")

edges_jaccard <- shared %>%
  left_join(theme_freq, by = c("Theme.x" = "Theme")) %>%
  rename(nx = n_panels) %>%
  left_join(theme_freq, by = c("Theme.y" = "Theme")) %>%
  rename(ny = n_panels) %>%
  mutate(jaccard = shared_panels / (nx + ny - shared_panels)) %>%
  transmute(
    Source  = Theme.x,
    Target  = Theme.y,
    Weight  = shared_panels,  
    Jaccard = jaccard
  ) %>%
  arrange(desc(Weight), desc(Jaccard))




# Export to Gephi (CSV) / Gephi-ra exportatu (CSV-a)


out_nodes <- file.path(out_dir, "gephi_nodes_theme.csv")
out_edges <- file.path(out_dir, "gephi_edges_theme_cooc.csv")
out_edges_j <- file.path(out_dir, "gephi_edges_theme_weighted_jaccard.csv")

write_csv(nodes, out_nodes)
write_csv(edges, out_edges)
write_csv(edges_jaccard, out_edges_j)




# Bipartite Panel-Theme (to see panels as nodes) / Panel-gaika bipartitoa (panelak nodo gisa ikusteko)


bip_edges <- dat_panel_theme %>%
  count(Panel, Theme, name = "Weight") %>%
  transmute(Source = Panel, Target = Theme, Weight = Weight)

bip_nodes <- bind_rows(
  tibble(Id = unique(dat_panel_theme$Panel), Label = unique(dat_panel_theme$Panel), Type = "Panel"),
  tibble(Id = unique(dat_panel_theme$Theme), Label = unique(dat_panel_theme$Theme), Type = "Theme")
)

out_bip_nodes <- file.path(out_dir, "gephi_nodes_panel_theme_bipartite.csv")
out_bip_edges <- file.path(out_dir, "gephi_edges_panel_theme_bipartite.csv")

write_csv(bip_nodes, out_bip_nodes)
write_csv(bip_edges, out_bip_edges)




# Export GEXF (To open in Gephi directly / zuzenian Gephi-n zabaltzeko)


g <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
E(g)$Weight <- edges$Weight

out_graphml <- file.path(out_dir, "theme_cooc_network.graphml")
write_graph(g, out_graphml, format = "graphml")







