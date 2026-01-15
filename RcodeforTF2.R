## ================================
## 0) Prep (Windows-friendly)
## ================================
options(repos = c(CRAN = "https://cran.rstudio.com"))
dir.create("C:/R/rlibs", recursive = TRUE, showWarnings = FALSE)
.libPaths(c("C:/R/rlibs", .libPaths()))

rdeps <- c(
  "reticulate","tidyverse","lubridate","tictoc","patchwork",
  "sf","mapview","leaflet.extras2","ggplot2","readr","tidyr","dplyr",
  "ggthemes","forcats","webshot2","htmlwidgets","stringr"
)
to_install <- setdiff(rdeps, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, type = "binary")

library(reticulate)
library(tidyverse)
library(lubridate)
library(ggplot2)

## ================================
## 1) (Optional) Python deps for geemap, ee, etc.
## ================================
try(reticulate::install_miniconda(), silent = TRUE)
try(
  reticulate::py_install(
    packages = c("earthengine-api","geemap","osdatahub","OSGridConverter","numpy"),
    pip = TRUE
  ),
  silent = TRUE
)
try(reticulate::import("geemap", delay_load = TRUE), silent = TRUE)

## ================================
## 2) tinyforestR
## ================================
if (!requireNamespace("tinyforestR", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes", type = "binary")
  remotes::install_github("julianflowers/tinyForestR",
                          dependencies = TRUE, build_vignettes = FALSE, upgrade = "never")
}
library(tinyforestR)

## ================================
## 3) Get Tiny Forest data & save
## ================================
library(tictoc)
tic("Fetching Tiny Forest data")
out <- tinyforestR::get_tf_data()
toc()

tf <- if (is.list(out) && !is.null(out$tidy_tf)) out$tidy_tf else out
dir.create("data", showWarnings = FALSE)
readr::write_csv(tf, "data/tf_data.csv")
message("Saved: ", normalizePath("data/tf_data.csv"))

## ================================
## 4) Prepare data for analysis
## ================================
needed_cols <- c("tf_id","trees","plant_date","lon","lat","area")
missing <- setdiff(needed_cols, names(tf))
if (length(missing)) warning("Missing columns in tf: ", paste(missing, collapse = ", "))

tf <- tf %>%
  mutate(
    plant_date = as.Date(plant_date),
    year = lubridate::year(plant_date),
    n_trees = dplyr::case_when(
      is.na(trees) ~ NA_integer_,
      trimws(trees) == "" ~ NA_integer_,
      TRUE ~ vapply(strsplit(trees, "\\|"), function(x) length(unique(trimws(x))), integer(1))
    )
  )


## ================================
## 0) Prep
## ================================
options(repos = c(CRAN = "https://cran.rstudio.com"))
dir.create("C:/R/rlibs", recursive = TRUE, showWarnings = FALSE)
.libPaths(c("C:/R/rlibs", .libPaths()))

# R deps (install if missing)
rdeps <- c(
  "reticulate","needs","tidyverse","lubridate","tictoc","patchwork",
  "sf","mapview","leaflet.extras2","ggplot2","readr","tidyr","dplyr",
  "ggthemes","forcats","webshot2","htmlwidgets"
)
to_install <- setdiff(rdeps, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, type = "binary")

library(reticulate)

## ================================
## 1) Python: install required modules into whatever Python reticulate found
##    (removed RETICULATE_PYTHON after init — it was a no-op)
## ================================
# If no Python is available, quietly set up miniconda (idempotent & Windows-friendly)
try(reticulate::install_miniconda(), silent = TRUE)

reticulate::py_install(
  packages = c("earthengine-api", "geemap", "osdatahub", "OSGridConverter", "numpy"),
  pip = TRUE
)

# Sanity import test (delay_load keeps session fast)
reticulate::import("ee", delay_load = TRUE)
reticulate::import("geemap", delay_load = TRUE)
reticulate::import("osdatahub", delay_load = TRUE)
reticulate::import("OSGridConverter", delay_load = TRUE)

## ================================
## 2) Load tinyforestR
## ================================
if (!requireNamespace("tinyforestR", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes", type = "binary")
  }
  remotes::install_github(
    "julianflowers/tinyForestR",
    dependencies = TRUE, build_vignettes = FALSE, upgrade = "never"
  )
}
library(tinyforestR)

## ================================
## 3) Get Tiny Forest data & save
## ================================
library(tictoc)
library(tidyverse)
library(lubridate)

tic("Fetching Tiny Forest data")
out <- tinyforestR::get_tf_data()
toc()

# Handle both return styles (list with $tidy_tf or tibble directly)
tf <- if (is.list(out) && !is.null(out$tidy_tf)) out$tidy_tf else out

dir.create("data", showWarnings = FALSE)
readr::write_csv(tf, "data/tf_data.csv")
message("Saved: ", normalizePath("data/tf_data.csv"))

## ================================
## 4) Prepare data for analysis
## ================================
# Ensure expected columns exist
needed_cols <- c("tf_id","trees","plant_date","lon","lat","area")
missing <- setdiff(needed_cols, names(tf))
if (length(missing)) {
  warning("Missing columns in tf: ", paste(missing, collapse = ", "))
}

tf <- tf %>%
  mutate(
    plant_date = as.Date(plant_date),
    year = lubridate::year(plant_date),
    # Count unique species from "A | B | C"
    n_trees = dplyr::case_when(
      is.na(trees) ~ NA_integer_,
      trimws(trees) == "" ~ NA_integer_,
      TRUE ~ vapply(strsplit(trees, "\\|"), function(x) length(unique(trimws(x))), integer(1))
    )
  )

## ================================
## 5) Create plots
## ================================
library(patchwork)

dir.create("figures", showWarnings = FALSE)

# 1) TFs planted per year
p_year <- tf %>%
  distinct(tf_id, year, .keep_all = TRUE) %>%
  count(year, name = "n") %>%
  filter(!is.na(year)) %>%
  ggplot(aes(year, n)) +
  geom_col() +
  labs(title = "Tiny Forests planted per year", x = NULL, y = "Count") +
  theme_bw(base_size = 12) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    axis.title = element_text(face = "bold"),
    axis.text  = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text  = element_text(face = "bold")
  )

ggsave("figures/01_forests_per_year.png", p_year, width = 8, height = 6, dpi = 300, bg = "white")
print(p_year)

# 2) Area distribution by year
p_area <- tf %>%
  distinct(tf_id, year, area, .keep_all = FALSE) %>%
  filter(!is.na(year), !is.na(area)) %>%
  ggplot(aes(factor(year), area)) +
  geom_boxplot() +
  labs(title = "Tiny Forest area by year", x = "Year", y = "Area (m²)") +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title  = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text  = element_text(face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
  )

ggsave("figures/02_area_by_year.png", p_area, width = 8, height = 6, dpi = 300, bg = "white")
print(p_area)

# 3) Distribution of tree species per site
p_trees <- tf %>%
  group_by(tf_id) %>%
  summarise(n_trees = dplyr::first(na.omit(n_trees)), .groups = "drop") %>%
  filter(!is.na(n_trees)) %>%
  ggplot(aes(n_trees)) +
  geom_histogram(bins = 25, boundary = 0) +
  labs(title = "Distribution of tree species per site",
       x = "Number of tree species", y = "Sites") +
  theme_bw(base_size = 12) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    axis.title = element_text(face = "bold"),
    axis.text  = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text  = element_text(face = "bold")
  )

ggsave("figures/03_species_distribution.png", p_trees, width = 8, height = 6, dpi = 300, bg = "white")
print(p_trees)

combined_plot <- p_year + p_area + p_trees
ggsave("figures/00_combined_plots.png", combined_plot, width = 12, height = 10, dpi = 300, bg = "white")
print(combined_plot)

## ================================
## 6) Create UK map
## ================================
library(sf)
library(mapview)

# UK bounding box
lon_min <- -8.8; lon_max <- 2.2
lat_min <- 49.5; lat_max <- 60.9

pts_uk <- tf %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  filter(dplyr::between(lon, lon_min, lon_max),
         dplyr::between(lat, lat_min, lat_max)) %>%
  distinct(tf_id, .keep_all = TRUE) %>%
  mutate(year = factor(year)) %>%
  sf::st_as_sf(coords = c("lon","lat"), crs = 4326)

# Snapshot reliability tweaks
mapviewOptions(fgb = FALSE)  # helps mapshot stability on Windows

# Interactive map
lf <- mapview(
  pts_uk,
  zcol = "year",
  legend = TRUE,
  basemaps = "CartoDB.Positron"   # 'basemaps' is the current arg name
)
print(lf)

# Save map as PNG for Word (try webshot2 then fallback)
saved <- FALSE

# Method 1: webshot2 (uses installed Chrome/Edge)
try({
  library(webshot2)
  mapview::mapshot(
    lf,
    file = "figures/tinyforest_uk_map.png",
    vwidth = 2400, vheight = 1600
  )
  message("✓ Map saved using webshot2: ", normalizePath("figures/tinyforest_uk_map.png"))
  saved <- TRUE
}, silent = TRUE)

# Method 2: PhantomJS (classic webshot)
if (!saved) {
  try({
    if (!requireNamespace("webshot", quietly = TRUE)) install.packages("webshot", type = "binary")
    library(webshot)
    # Install PhantomJS if missing (idempotent)
    try(webshot::install_phantomjs(), silent = TRUE)
    mapview::mapshot(
      lf,
      file = "figures/tinyforest_uk_map.png",
      vwidth = 2400, vheight = 1600
    )
    message("✓ Map saved using webshot: ", normalizePath("figures/tinyforest_uk_map.png"))
    saved <- TRUE
  }, silent = TRUE)
}

# Method 3: Save HTML as a sure fallback
if (!saved) {
  htmlwidgets::saveWidget(lf@map, "figures/tinyforest_uk_map.html", selfcontained = TRUE)
  message("✓ Map saved as HTML: ", normalizePath("figures/tinyforest_uk_map.html"))
}

# Static backup for Word/PDF with fixed UK extent
library(ggplot2)
static_map <- ggplot(pts_uk) +
  geom_sf(aes(color = year), size = 2, alpha = 0.7, show.legend = TRUE) +
  coord_sf(
    xlim = c(lon_min, lon_max),
    ylim = c(lat_min, lat_max),
    expand = FALSE
  ) +
  labs(
    title = "Tiny Forests Locations in the UK",
    subtitle = paste0("Total sites: ", nrow(pts_uk)),
    color = "Year Planted"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    axis.title = element_text(face = "bold"),
    axis.text  = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text  = element_text(face = "bold"),
    legend.position = "right"
  )

ggsave("figures/05_static_uk_map.png", static_map, width = 10, height = 12, dpi = 300, bg = "white")
message("✓ Static map saved: figures/05_static_uk_map.png")

## ================================
## 7) Locations: Tree species analysis
## ================================
library(forcats)
library(stringr)

species_plot <- tf %>%
  filter(!is.na(trees), trees != "") %>%
  tidyr::separate_rows(trees, sep = "\\|") %>%
  mutate(trees = str_squish(trees)) %>%
  filter(trees != "") %>%
  distinct(tf_id, trees) %>%
  count(trees, sort = TRUE, name = "n") %>%
  slice_max(order_by = n, n = 25, with_ties = FALSE) %>%
  mutate(trees = fct_reorder(trees, n)) %>%
  ggplot(aes(n, trees)) +
  geom_col() +
  labs(
    x = "Number of sites", y = NULL,
    title = "25 most commonly planted tree species"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title.position = "plot",
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    axis.title = element_text(face = "bold"),
    axis.text  = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text  = element_text(face = "bold")
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, .05)))  # avoids clipping bars at 0/right

# Outer frame & white background for crisp export
species_plot_framed <- species_plot +
  theme(
    plot.background = element_rect(colour = "black", fill = "white", linewidth = 1),
    plot.margin     = margin(10, 12, 10, 14)
  )

ggsave(
  "figures/04_top_species.png",
  species_plot_framed,
  width = 10, height = 8, dpi = 300, bg = "white"
)
print(species_plot_framed)
