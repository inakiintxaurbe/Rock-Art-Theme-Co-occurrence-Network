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

# Install packages (if necessary) ----------------------------------------------------------------------------------------------------------------------------------------------

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

# Extract Panel from GU: "X.Y.Z.W" -> "X.Y.Z" ----------------------------------------------------------------------------------------------------------------------------------
#    E.g.: S.E.II.01 -> S.E.II -------------------------------------------------------------------------------------------------------------------------------------------------

dat2 <- dat2 %>%
  mutate(Panel = sub("^([^\\.]+\\.[^\\.]+\\.[^\\.]+).*", "\\1", GU))

# Remove duplicates (for Jaccard, filtered, comparisons) and takes into account for ponderated co-occurrence network analysis
dat_panel_theme_1 <- dat2 %>% distinct(Panel, Theme)
dat_panel_theme_2   <- dat2 %>% count(Panel, Theme, name = "n")

# Co-occurrence network considering Jaccard (shared_panels / (nx + ny - shared_panels)) -----------------------------------------------------------------------------
# Preponderating
nodes <- dat_panel_theme_1 %>%
  group_by(Theme) %>%
  summarise(
    Id = first(Theme),
    Label = first(Theme),
    PanelFreq = n_distinct(Panel),
    .groups = "drop"
  ) %>%
  select(Id, Label, PanelFreq) %>%
  arrange(desc(PanelFreq))

theme_freq <- dat_panel_theme_1 %>%
  count(Theme, name = "n_panels")

shared <- dat_panel_theme_1 %>%
  inner_join(dat_panel_theme_1, by = "Panel") %>%
  filter(Theme.x < Theme.y) %>%
  count(Theme.x, Theme.y, name = "shared_panels")

edges <- shared %>%
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

out_nodes <- file.path(out_dir, "gephi_nodes_theme.csv")
out_edges_j <- file.path(out_dir, "gephi_edges_theme_weighted_jaccard.csv")

write_csv(nodes, out_nodes)
write_csv(edges, out_edges_j)

g <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)

write_graph(g, file.path(out_dir, "theme_cooc_weight_and_jaccard.graphml"), format = "graphml")

# Ponderating (takes into account the repetition of themes in each panel)
edges_tt_weighted <- dat_panel_theme_2 %>%
  inner_join(., ., by = "Panel") %>%
  filter(Theme.x < Theme.y) %>%
  mutate(Weight = n.x * n.y) %>%
  group_by(Theme.x, Theme.y) %>%
  summarise(Weight = sum(Weight), .groups = "drop") %>%
  transmute(Source = Theme.x, Target = Theme.y, Weight) %>%
  arrange(desc(Weight))

write_csv(edges_tt_weighted, file.path(out_dir, "edges_theme_weighted_by_repetition.csv"))
write_graph(
  graph_from_data_frame(edges_tt_weighted, directed = FALSE, vertices = nodes),
  file.path(out_dir, "theme_weighted_by_repetition.graphml"),
  format = "graphml"
)

# Global network Panel-Theme ---------------------------------------------------------------------------------------------------------------------------------------
# Preponderating

bip_edges_1 <- dat_panel_theme_1 %>%
  transmute(Source = Panel, Target = Theme)

bip_nodes_1 <- bind_rows(
  tibble(Id = unique(dat_panel_theme_1$Panel), Label = unique(dat_panel_theme_1$Panel), Type = "Panel"),
  tibble(Id = unique(dat_panel_theme_1$Theme), Label = unique(dat_panel_theme_1$Theme), Type = "Theme")
)

write_csv(bip_nodes_1, file.path(out_dir, "bip_nodes_panel_theme_1.csv"))
write_csv(bip_edges_1, file.path(out_dir, "bip_edges_panel_theme_1.csv"))

g_bip_1 <- graph_from_data_frame(bip_edges_1, directed=FALSE, vertices=bip_nodes_1 %>% rename(name=Id))
V(g_bip_1)$Type <- bip_nodes_1$Type[match(V(g_bip_1)$name, bip_nodes_1$Id)]
write_graph(g_bip_1, file.path(out_dir, "panel_theme_bipartite_1.graphml"), format="graphml")

# Ponderating (takes into account the repetition of themes in each panel)

bip_edges_2 <- dat_panel_theme_2 %>%
  transmute(Source = Panel, Target = Theme, Weight = n)

bip_nodes_2 <- bind_rows(
  tibble(Id = unique(dat_panel_theme_2$Panel), Label = unique(dat_panel_theme_2$Panel), Type = "Panel"),
  tibble(Id = unique(dat_panel_theme_2$Theme), Label = unique(dat_panel_theme_2$Theme), Type = "Theme")
)

write_csv(bip_nodes_2, file.path(out_dir, "bip_nodes_panel_theme_2.csv"))
write_csv(bip_edges_2, file.path(out_dir, "bip_edges_panel_theme_2.csv"))

g_bip_2 <- graph_from_data_frame(bip_edges_2, directed=FALSE, vertices=bip_nodes_2 %>% rename(name=Id))
V(g_bip_2)$Type <- bip_nodes_2$Type[match(V(g_bip_2)$name, bip_nodes_2$Id)]
write_graph(g_bip_2, file.path(out_dir, "panel_theme_bipartite_2.graphml"), format="graphml")

# V Filtered network (change the thresholds) -----------------------------------------------------------------------------------------------------------------------

min_weight  <- 3      # <-- HERE !!!
min_jaccard <- 0.12   # <-- HERE !!!

# Ʌ Filtered network (change the thresholds) -----------------------------------------------------------------------------------------------------------------------

edges_filt <- edges %>%
  filter(Weight >= min_weight, Jaccard >= min_jaccard)

g_filt <- graph_from_data_frame(edges_filt, directed=FALSE, vertices=nodes)

write_csv(edges_filt, file.path(out_dir, "edges_theme_filtered.csv"))
write_graph(g_filt, file.path(out_dir, "theme_filtered.graphml"), format="graphml")

# MST (Minimum Spanning Tree) --------------------------------------------------------------------------------------------------------------------------------------

E(g_filt)$cost <- 1 / E(g_filt)$Weight

mst_g <- igraph::mst(g_filt, weights = E(g_filt)$cost)

mst_edges <- igraph::as_data_frame(mst_g, what = "edges") %>%
  dplyr::rename(Source = from, Target = to) %>%
  dplyr::left_join(
    edges_filt,
    by = c("Source", "Target")
  )

write_csv(mst_edges, file.path(out_dir, "edges_theme_MST.csv"))
write_graph(mst_g, file.path(out_dir, "theme_MST.graphml"), format = "graphml")


# EGO NETWORKS: Bison vs Ibex vs Horse ----------------------------------------------------------------------------------------------------------------------------

ego_export <- function(g, center, order = 1) {
  
  sg <- igraph::make_ego_graph(
    g,
    order = order,
    nodes = center,
    mode = "all"
  )[[1]]
  
  edges <- igraph::as_data_frame(sg, what = "edges") %>%
    dplyr::rename(Source = from, Target = to)
  
  write_graph(
    sg,
    file.path(out_dir, paste0("ego_", center, ".graphml")),
    format = "graphml"
  )
  
  write_csv(
    edges,
    file.path(out_dir, paste0("ego_", center, "_edges.csv"))
  )
}

ego_export(g_filt, "Bison", order=1)
ego_export(g_filt, "Ibex",  order=1)
ego_export(g_filt, "Horse", order=1)

# LATERALITY AND INCLINATION OF THEMES ----------------------------------------------------------------------------------------------------------------------------

df_fig <- dat %>%
  transmute(
    GU     = as.character(GU),
    Panel  = sub("^([^\\.]+\\.[^\\.]+\\.[^\\.]+).*", "\\1", as.character(GU)),
    Theme  = as.character(Theme),
    Format = as.character(Format),
    Orient = as.character(Orient),
    Incl   = as.character(Incl)
  ) %>%
  filter(
    Format != "NF_F",
    Orient != "NF_O",
    Incl   != "NF_I"
  )

# Theme x Orient (globals: chi-square and Cramer's V)

tab_or <- table(df_fig$Theme, df_fig$Orient)
chi_or <- suppressWarnings(chisq.test(tab_or))

cramers_v <- function(tab) {
  chi <- suppressWarnings(chisq.test(tab)$statistic)
  n <- sum(tab)
  r <- nrow(tab); k <- ncol(tab)
  as.numeric(sqrt(chi / (n * (min(r-1, k-1)))))
}

or_summary <- tibble(
  Test = "Theme x Orient",
  ChiSquare = as.numeric(chi_or$statistic),
  df = as.numeric(chi_or$parameter),
  p_value = as.numeric(chi_or$p.value),
  CramersV = cramers_v(tab_or)
)

write_csv(or_summary, file.path(out_dir, "stats_theme_x_orient_global.csv"))


# Theme x Orient (per-theme: binomials probabilities and BH correction)

per_theme_lr <- df_fig %>%
  count(Theme, Orient) %>%
  tidyr::pivot_wider(names_from = Orient, values_from = n, values_fill = 0) %>%
  mutate(
    n_total = Left + Right,
    prop_right = Right / n_total
  ) %>%
  rowwise() %>%
  mutate(
    p_binom = binom.test(Right, n_total, p = 0.5)$p.value,
    direction = case_when(
      prop_right > 0.5 ~ "Right",
      prop_right < 0.5 ~ "Left",
      prop_right == 0.5 ~ "Equal"
    )
  ) %>%
  ungroup() %>%
  mutate(
    p_adj_BH = p.adjust(p_binom, method = "BH"),
    sig_Binom = if_else(p_binom < 0.05, "*", ""),
    sig_BH = if_else(p_adj_BH < 0.05, "**", "")
  ) %>%
  arrange(p_adj_BH, desc(n_total))

write_csv(per_theme_lr, file.path(out_dir, "per_theme_right_left_binom.csv"))

# Theme x Incl (residuals)

tab_in <- table(df_fig$Theme, df_fig$Incl)
chi_in <- suppressWarnings(chisq.test(tab_in))

incl_residuals <- as.data.frame(as.table(chi_in$stdres)) %>%
  dplyr::rename(
    Theme = Var1,
    Incl = Var2,
    std_resid = Freq
  ) %>%
  dplyr::arrange(dplyr::desc(abs(std_resid)))

write_csv(incl_residuals,file.path(out_dir, "stats_theme_x_incl_residuals.csv"))

