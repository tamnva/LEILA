# ====================================================================================
# SELECT ONE UNIQUE REPRESENTATIVE WELL PER CATCHMENT
# ====================================================================================
# Purpose
# - Start from the list of candidate representative wells per catchment (rep_wells_all.csv).
# - Rank all possible catchment-well combinations according to the following criteria:
#     1) Whether the well is inside the catchment
#     2) Difference in hydraulic conductivity class (dK)
#     3) If the same well could fit multiple catchments,
#        priority is given to the catchment where the dominant
#        hydraulic conductivity class covers a larger area.
#     4) Whether the well is classified as "natural"
#     5) Similarity to natural wells based on DTW distance
#
# Goal
# - Assign only ONE representative well to each catchment (if candidates exist).
# - Ensure that the same well is not assigned to more than one catchment.
#
# Outputs:
# selected_well_per_catchment_unique.csv
# ==============================================================================

rm(list = ls())

# ===============================
# 1. Libraries
# ===============================
library(sf)
library(dplyr)
library(readr)
library(units)
library(leaflet)

# ===============================
# 2. Inputs/outputs
# ===============================
base_dir <- "C:/Users/nguyenta/Documents/LEILA/working_code_documentation/data/representative_wells"
in_dir   <- file.path(base_dir, "Inputs")
out_dir  <- file.path(base_dir, "Outputs")
well_path   <- file.path(in_dir, "gw_meta_monthly_geo_qc.txt")


if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ===============================
# 3. Paths
# ===============================
# Candidates from Script 01
rep_wells_path <- file.path(out_dir, "rep_wells_all.csv")

# DTW similarity results: @fromTam: How to get this data
# neighbors_path <- file.path(in_dir, "wells_neighbors.csv")
neighbors_path <- list.files(file.path(in_dir, "wells_neighbors"), full.names = TRUE) 

# Catchments shapefile (for dominant K share)
catch_path <- file.path(out_dir, "sub_intersect.rds")

# study catchments
# csv_selected_path <- file.path(in_dir, "selected_well_per_catchment_final.csv")

# Final output
out_file <- file.path(out_dir, "selected_well_per_catchment_unique.csv")

# ===============================
# 4. Load data
# ===============================
cat("Loading input files...\n")

rep_wells_all <- read_csv(rep_wells_path, show_col_types = FALSE)
#neigh <- read_csv(neighbors_path, show_col_types = FALSE)
neigh <- bind_rows(
  lapply(X = neighbors_path, read_delim, delim = ";", show_col_types = FALSE)
  ) %>%
  select(point_UFZ.ID, neighbor_UFZ.ID, DTW_dist) %>% 
  mutate(has_neighbor = TRUE)

# Make sure 1 well only compare to 1 another near natural well
neigh <- neigh[-which(duplicated(neigh$neighbor_UFZ.ID)),]

catchments  <- bind_rows(readRDS(catch_path))

#selected_catchments_df <- read_csv(csv_selected_path, show_col_types = FALSE)
#final_catchment_ids <- unique(selected_catchments_df$catch_id)
final_catchment_ids <- unique(catchments$gauge_id)

# Read well data
wells <- read.csv(well_path, sep = ";", header = TRUE) %>% as_tibble()
wells <- st_as_sf(wells, coords = c("x_EPSG25832", "y_EPSG25832"), crs = 25832)
wells <- st_transform(wells, 4326)

# ===============================
# 5. Filter candidates to the study catchments
# ===============================
rep_wells_filtered <- rep_wells_all %>%
  mutate(catch_id = as.character(catch_id),
         well_id  = as.character(well_id)) %>%
  filter(catch_id %in% as.character(final_catchment_ids))

cat("Catchments in rep_wells_all:", length(unique(rep_wells_all$catch_id)), "\n")
cat("Catchments after filtering:", length(unique(rep_wells_filtered$catch_id)), "\n")

# ===============================
# 6. Calculate dominant K-class share per catchment
# ===============================
cat("Calculating dominant K-class area share...\n")

catch_dominant_share <- st_make_valid(catchments) %>%
  rename(catch_id = gauge_id, K_code = L_KF) %>%
  mutate(area_piece = st_area(.)) %>%
  st_drop_geometry() %>%
  group_by(catch_id, K_code) %>%
  summarise(area = sum(area_piece), .groups = "drop") %>%
  group_by(catch_id) %>%
  mutate(
    total_area = sum(area),
    share = as.numeric(area / total_area)
  ) %>%
  slice_max(order_by = share, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(catch_id, dominant_K_code = K_code, dominant_K_share = share) %>%
  mutate(catch_id = as.character(catch_id))

# ===============================
# 7.  Natural wells + nearest DTW
# ===============================
# "Natural" wells are the wells listed in the left column of the neighbors table
natural_wells <- unique(neigh$`point_UFZ.ID`) %>% as.character()

# For each well (as neighbor), get the nearest DTW distance to any natural well
min_dtw_to_natural <- neigh %>%
  rename(neighbor = `neighbor_UFZ.ID`, DTW = `DTW_dist`) %>%
  mutate(
    neighbor = as.character(neighbor)
  ) %>%
  group_by(neighbor) %>%
  summarise(nearest_dtw_to_natural = min(DTW, na.rm = TRUE), .groups = "drop")

# ===============================
# 8. Build ranking table for all candidate catchment-well pairs
# ===============================
ranked_pairs <- rep_wells_filtered %>%
  mutate(
    inside_priority = if_else(tolower(match_type) == "inside", 1L, 0L),
    is_natural      = if_else(well_id %in% natural_wells, 1L, 0L)
  ) %>%
  left_join(min_dtw_to_natural, by = c("well_id" = "neighbor")) %>%
  mutate(
    nearest_dtw_to_natural = if_else(is_natural == 1L, 0, nearest_dtw_to_natural),
    nearest_dtw_to_natural = if_else(is.na(nearest_dtw_to_natural), Inf, nearest_dtw_to_natural)
  ) %>%
  left_join(catch_dominant_share, by = "catch_id")

# ===============================
# 9. Rank pairs (best options first)
# ===============================
ranked_pairs <- ranked_pairs %>%
  arrange(
    desc(inside_priority),        # inside
    dK,                           # smaller K difference first
    desc(dominant_K_share),       # if a well fits multiple catchments, prefer the catchment with stronger dominant K
    desc(is_natural),             # natural wells first
    nearest_dtw_to_natural,       # smaller DTW distance first
    catch_id,                     # stable tie-breaker
    well_id
  )

# ===============================
# 10. Assign unique wells (one well per catchment & no duplicates across catchments)
# ===============================
assigned_catchments <- character(0)
assigned_wells      <- character(0)
final_assignments   <- list()

for (i in seq_len(nrow(ranked_pairs))) {
  current_catch_id <- ranked_pairs$catch_id[i]
  current_well_id  <- ranked_pairs$well_id[i]
  
  if (!(current_catch_id %in% assigned_catchments) && !(current_well_id %in% assigned_wells)) {
    final_assignments[[current_catch_id]] <- ranked_pairs[i, ]
    assigned_catchments <- c(assigned_catchments, current_catch_id)
    assigned_wells      <- c(assigned_wells, current_well_id)
  }
}

selected_unique_wells <- bind_rows(final_assignments)

# ===============================
# 11. Checks and export
# ===============================
cat("\n=== ASSIGNMENT SUMMARY ===\n")
cat("Catchments in final list:", length(final_catchment_ids), "\n")
cat("Catchments assigned a well:", nrow(selected_unique_wells), "\n")
cat("Unique wells assigned:", length(unique(selected_unique_wells$well_id)), "\n\n")

cat("Preview (first rows):\n")
print(
  head(
    selected_unique_wells %>%
      select(catch_id, well_id, match_type, dK, dominant_K_share, is_natural)
  )
)

write_csv(selected_unique_wells, out_file)
cat("\nSaved:\n - ", out_file, "\n", sep = "")

# Plot near natural wells
selected_unique_wells <- read.csv(out_file)

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = wells %>% filter(UFZ.ID %in% unique(selected_unique_wells$well_id)),
                   radius = 2, fillOpacity = 0.7, stroke = FALSE)
