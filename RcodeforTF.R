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
rdeps <- c("reticulate","needs","tidyverse","lubridate","tictoc","patchwork",
           "sf","mapview","leaflet.extras2","ggplot2","readr","tidyr","dplyr",
           "ggthemes","forcats","webshot2")
to_install <- setdiff(rdeps, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, type = "binary")

library(reticulate)

## ================================
## 1) Python: use reticulate's current env + install required modules
## ================================
py <- reticulate::py_config()$python
Sys.setenv(RETICULATE_PYTHON = py)

# Install Python deps
reticulate::py_install(
  packages = c("earthengine-api", "geemap", "osdatahub", "OSGridConverter", "numpy"),
  pip = TRUE
)

# Sanity import test
import("ee", delay_load = TRUE)
import("geemap", delay_load = TRUE)
import("osdatahub", delay_load = TRUE)
import("OSGridConverter", delay_load = TRUE)

## ================================
## 2) Load tinyforestR
## ================================
if (!requireNamespace("tinyforestR", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes", type = "binary")
  }
  remotes::install_github("julianflowers/tinyForestR",
                          dependencies = TRUE, build_vignettes = FALSE, upgrade = "never")
}
library(tinyforestR)

## ================================
## 3) Get Tiny Forest data & save
## ================================
library(tictoc)
library(tidyverse)
library(lubridate)

tic("Fetching Tiny Forest data")
out <- tinyforestR::get_tf_data()  # returns a list
toc()

# Extract the tidy dataframe
tf <- out$tidy_tf

# Save raw data
dir.create("data", showWarnings = FALSE)
readr::write_csv(tf, "data/tf_data.csv")
message("Saved: ", normalizePath("data/tf_data.csv"))

## ================================
## 4) Prepare data for analysis
## ================================
tf <- tf %>%
  mutate(
    date = as.Date(plant_date),
    year = lubridate::year(date),
    # Count species from the "A | B | C" string
    n_trees = ifelse(
      is.na(trees) | trimws(trees) == "",
      NA_integer_,
      sapply(strsplit(trees, "\\|"), function(x) length(unique(trimws(x))))
    )
  )

## ================================
## 5) Create plots
## ================================
library(patchwork)

# Create figures directory
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
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold"))

# Save
#ggsave("figures/01_forests_per_year.png", p_year, width = 8, height = 6, dpi = 300)
print(p_year)

# 2) Area distribution by year
p_area <- tf %>%
  distinct(tf_id, year, area, .keep_all = FALSE) %>%
  filter(!is.na(year), !is.na(area)) %>%
  ggplot(aes(factor(year), area)) +
  geom_boxplot() +
  labs(title = "Tiny Forest area by year", x = "Year", y = "Area (m²)") +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))

# Save
#ggsave("figures/02_area_by_year.png", p_area, width = 8, height = 6, dpi = 300)
print(p_area)

# 3) Distribution of tree species per site
p_trees <- tf %>%
  group_by(tf_id) %>%
  summarise(n_trees = dplyr::first(na.omit(n_trees)), .groups = "drop") %>%
  filter(!is.na(n_trees)) %>%
  ggplot(aes(n_trees)) +
  geom_histogram(bins = 45) +
  labs(title = "Distribution of tree species per site",
       x = "Number of tree species", y = "Sites") +
  theme_bw(base_size = 12) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold"))

# Save
#ggsave("figures/03_species_distribution.png", p_trees, width = 8, height = 6, dpi = 300)
print(p_trees)

# Display combined plot
combined_plot <- p_year + p_area + p_trees
ggsave("figures/00_combined_plots.png", combined_plot, width = 12, height = 10, dpi = 300)
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
  filter(between(lon, lon_min, lon_max),
         between(lat, lat_min, lat_max)) %>%
  distinct(tf_id, .keep_all = TRUE) %>%
  mutate(year = factor(year)) %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326)

# Create interactive map
lf <- mapview(pts_uk, zcol = "year", map.types = "CartoDB.Positron", legend = TRUE)
print(lf)

# Save map as PNG for Word document
# Install required packages
if (!requireNamespace("webshot", quietly = TRUE)) {
  install.packages("webshot", type = "binary")
  webshot::install_phantomjs()  # Install PhantomJS for webshot
}
if (!requireNamespace("webshot2", quietly = TRUE)) {
  install.packages("webshot2", type = "binary")
}

# Try multiple methods to save the map
saved <- FALSE

# Method 1: Try webshot2 first (uses Chrome)
if (!saved) {
  tryCatch({
    library(webshot2)
    mapview::mapshot(lf, file = "figures/tinyforest_uk_map.png", 
                     vwidth = 2400, vheight = 1600)
    message("✓ Map saved using webshot2: ", normalizePath("figures/tinyforest_uk_map.png"))
    saved <- TRUE
  }, error = function(e) {
    message("webshot2 failed, trying alternative method...")
  })
}

# Method 2: Try original webshot (uses PhantomJS)
if (!saved) {
  tryCatch({
    library(webshot)
    mapview::mapshot(lf, file = "figures/tinyforest_uk_map.png", 
                     vwidth = 2400, vheight = 1600)
    message("✓ Map saved using webshot: ", normalizePath("figures/tinyforest_uk_map.png"))
    saved <- TRUE
  }, error = function(e) {
    message("webshot failed, trying HTML method...")
  })
}

# Method 3: Save as HTML (always works)
if (!saved) {
  library(htmlwidgets)
  saveWidget(lf@map, "figures/tinyforest_uk_map.html", selfcontained = TRUE)
  message("✓ Map saved as HTML: ", normalizePath("figures/tinyforest_uk_map.html"))
  message("  → Open the HTML file in your browser, then:")
  message("  → Press Ctrl+Shift+S (or Cmd+Shift+S on Mac) to screenshot")
  message("  → Or right-click → 'Save image as...' if available")
  message("  → Then insert the image into your Word document")
}

# Also create a static ggplot map as backup for Word
library(ggplot2)
library(sf)

static_map <- ggplot(pts_uk) +
  geom_sf(aes(color = year), size = 2, alpha = 0.7) +
  labs(title = "Tiny Forests Locations in UK",
       subtitle = paste0("Total sites: ", nrow(pts_uk)),
       color = "Year Planted") +
  theme_bw(base_size = 12) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold"),
        legend.position = "right")

ggsave("figures/05_static_uk_map.png", static_map, width = 10, height = 12, dpi = 300)
message("✓ Static map saved: figures/05_static_uk_map.png")
message("  → This PNG can be directly inserted into Word!")

## ================================
## 7) Locations: Tree species analysis
## ================================
library(forcats)

species_plot <- tf %>%
  filter(!is.na(trees), trees != "") %>%
  # Explode the "A | B | C" string into rows
  separate_rows(trees, sep = "\\|") %>%
  mutate(trees = str_squish(trees)) %>%
  # Count species once per site (avoid duplicates)
  distinct(tf_id, trees) %>%
  count(trees, sort = TRUE) %>%
  slice_max(n, n = 25, with_ties = FALSE) %>%
  mutate(trees = fct_reorder(trees, n)) %>%
  ggplot(aes(n, trees)) +
  geom_col() +
  labs(
    x = "Number of sites", y = NULL,
    title = "25 most commonly planted tree species"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title.position = "plot",
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold"))

# Save
ggsave("figures/04_top_species.png", species_plot, width = 10, height = 8, dpi = 300)
print(species_plot)

#Another way 
library(ggplot2)

species_plot <- tf %>%
  dplyr::filter(!is.na(trees) & trees != "") %>%
  tidyr::separate_rows(trees, sep = "\\|") %>%
  dplyr::mutate(trees = stringr::str_squish(trees)) %>%
  dplyr::filter(trees != "") %>%
  dplyr::distinct(tf_id, trees) %>%
  dplyr::count(trees, name = "n", sort = TRUE) %>%
  dplyr::slice_max(order_by = n, n = 25, with_ties = FALSE) %>%
  dplyr::mutate(trees = forcats::fct_reorder(trees, n)) %>%
  ggplot2::ggplot(ggplot2::aes(x = n, y = trees)) +
  ggplot2::geom_col() +
  ggplot2::labs(
    x = "Number of sites", y = NULL,
    title = "25 most commonly planted tree species"
  ) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(
    plot.title.position = "plot",
    axis.title   = ggplot2::element_text(face = "bold"),
    axis.text.x  = ggplot2::element_text(face = "bold"),
    axis.text.y  = ggplot2::element_text(face = "bold"),  # species names bold
    legend.title = ggplot2::element_text(face = "bold"),
    legend.text  = ggplot2::element_text(face = "bold"),
    panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1)
  )

# Add OUTER frame too
species_plot_framed <- species_plot +
  theme(
    # inner panel box
    panel.border   = element_rect(colour = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", colour = NA),
    
    # outer frame (white fill so it isn't transparent)
    plot.background = element_rect(colour = "black", fill = "white", linewidth = 1),
    plot.margin     = margin(10, 12, 10, 14)  # a bit more room for labels
  )

ggsave(
  "figures/04_top_species.png",
  species_plot_framed,
  width = 10, height = 8, dpi = 300,
  bg = "white"   # <- forces a white background in the PNG
)


