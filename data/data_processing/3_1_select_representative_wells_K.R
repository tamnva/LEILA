# ====================================================================================
# SELECTION OF REPRESENTATIVE WELLS BASED ON HYDRAULIC CONDUCTIVITY (K CLASSES)
# ====================================================================================
# Purpose
# - Identify, for each catchment, a dominant hydraulic conductivity (K) class
#   based on the largest area share in the catchment.
# - Find all representative wells per catchment:
#     1) Prefer wells inside the catchment whose K class is matching/close to the dominant class
#     2) If no suitable inside well exists, search for wells within a 5 km buffer
# - Export:
#     Outputs/rep_wells_all.csv
# Notes
# - K codes 1 to 12 are treated as valid classes.
# - K codes 0, 11, 99 are excluded unless no valid alternative exists
#   when determining the dominant class.
# ====================================================================================

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
# 2. Input and output folders
# ===============================
base_dir <- "C:/Users/nguyenta/Documents/LEILA/working_code_documentation/data/representative_wells"

in_dir  <- file.path(base_dir, "Inputs")
out_dir <- file.path(base_dir, "Outputs")

# Create output folder if it does not exist
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ===============================
# 3. Input files
# ===============================
# Shapefiles
catch_path  <- file.path(in_dir, "CAMELS_DE_catchments.shp")
huek_path   <- file.path(in_dir, "HUEK200_TR.shp")
well_path   <- file.path(in_dir, "gw_meta_monthly_geo_qc.txt")


# ===============================
# 4. Read shapefiles
# ===============================
cat("Reading shapefiles...\n")
catchments <- st_read(catch_path, quiet = TRUE)
huek       <- st_read(huek_path)

wells <- read.csv(well_path, sep = ";", header = TRUE) %>% as_tibble()
wells <- st_as_sf(wells, coords = c("x_EPSG25832", "y_EPSG25832"), crs = 25832)
wells <- st_transform(wells, st_crs(catchments))

# Assign, transform coordinate, and visualize
st_crs(huek) <- 25832
huek         <- st_transform(huek, st_crs(catchments))

# Make invalid geometry valid
catchments <- st_make_valid(catchments)
huek <- st_make_valid(huek)

# Intersect catchments and hydrogeological units
sub_intersect <- list()
for (id in catchments$gauge_id){
  message("Instersecting subcatchment = ", id)
  sub           <- catchments %>% filter(gauge_id == id)
  intersect     <- st_intersects(sub, huek)[[1]]
  sub_intersect[[id]] <- st_intersection(sub, huek[intersect,])
}

saveRDS(sub_intersect, file.path(out_dir, "sub_intersect.rds"))
# sub_intersect <- readRDS(file.path(out_dir, "sub_intersect.rds"))
# leaflet(sub_intersect[[50]]) %>% addTiles() %>% addPolygons()

# catchments <- do.call(rbind, sub_intersect)

# ===============================
# 5. Basic checks (unique IDs)
# ===============================
no_of_catchments <- length(unique(catchments$gauge_id))
no_of_wells      <- length(unique(wells$UFZ.ID))

cat("Unique catchments:", no_of_catchments, "\n")
cat("Unique wells:", no_of_wells, "\n")

# ===============================
# 6. Rename fields to standard names
# ===============================
# catchments and wells :
# - gauge_id -> catch_id
# - L_KF      -> K_code (K class code in catchment polygons)

for (i in 1:length(sub_intersect)){
  sub_intersect[[i]] <- sub_intersect[[i]] %>% 
    rename(K_code = L_KF)
}
wells <- wells %>% rename(K_code = L_KF, well_id = UFZ.ID)


# ===============================
# 7. K class labels and validity rules
# ===============================
k_labels <- c(
  `0`  = "No data",
  `1`  = "Very high",
  `2`  = "High",
  `3`  = "Medium",
  `4`  = "Moderately low",
  `5`  = "Low",
  `6`  = "Very low",
  `7`  = "Extremely low",
  `8`  = "Very high to high",
  `9`  = "Medium to moderately low",
  `10` = "Low to extremely low",
  `11` = "Highly variable",
  `12` = "Moderately low to low",
  `99` = "Surface water"
)

# Valid codes for selection (exclude 0, 11, 99)
valid_k <- as.character(1:12)

# Target CRS for area calculations and buffering
target_crs <- 3035

# Ensure catchments are in the target CRS before area computations
for (i in 1:length(sub_intersect)){
  sub_intersect[[i]] <- st_transform(sub_intersect[[i]], target_crs)
}

# ===============================
# 8. Determine dominant K class per catchment (largest area share)
# ===============================
# Step A: compute area share by (catchment, K_code)
area_tbl <- list()
for (i in 1:length(sub_intersect)){
  area_tbl[[i]] <- sub_intersect[[i]] %>%
    mutate(area_piece = st_area(.)) %>%
    st_drop_geometry() %>%
    group_by(catch_id, K_code) %>%
    summarise(area = sum(area_piece), .groups = "drop") %>%
    group_by(catch_id) %>%
    mutate(
      total_area = sum(area),
      share = as.numeric(area / total_area)
    )
}

area_tbl <- do.call(rbind, area_tbl)

# Step B: pick the K_code with maximum share, preferring valid classes
catch_dominant_tbl <- area_tbl %>%
  group_by(gauge_id) %>%
  group_modify(~{
    df <- .x
    if (any(df$K_code %in% valid_k)) {
      df <- dplyr::filter(df, K_code %in% valid_k)
    }
    dplyr::slice_max(df, order_by = share, n = 1, with_ties = FALSE)
  }) %>%
  ungroup()

# Step D: join dominant K back to dissolved geometry (no need step C)
catch_dominant <- left_join(catchments, catch_dominant_tbl, by = "catch_id") %>%
  mutate(K_description = k_labels[as.character(K_code)])

# ===============================
# 9. Deduplicate wells and add labels
# ===============================
wells <- wells %>%
  distinct(well_id, .keep_all = TRUE) %>%
  mutate(K_description = k_labels[as.character(K_code)])

# ===============================
# 10. Map K_code to a numeric index (for distance in class space)
# ===============================
# This index lets us compute "class distance" like abs(K_index_w - K_index_c).
# Mixed classes are mapped to midpoints.
k_index_map <- c(
  `0`=NA, `1`=1.0, `2`=2.0, `3`=3.0, `4`=4.0, `5`=5.0, `6`=6.0, `7`=7.0,
  `8`=1.5, `9`=3.5, `10`=6.0, `11`=NA, `12`=4.5, `99`=NA
)

catch_dominant <- catch_dominant %>%
  mutate(K_index_c = unname(k_index_map[as.character(K_code)]))

wells <- wells %>%
  mutate(K_index_w = unname(k_index_map[as.character(K_code)]))

# ================================================
# 11. Filter wells to keep only valid K classes
# ================================================
wells_valid <- wells %>%
  filter(!is.na(K_index_w))

cat("Valid wells: ", nrow(wells_valid), " / ", nrow(wells), "\n")

# ===============================
# 12. Prepare for spatial matching
# ===============================
catch_dominant <- st_transform(catch_dominant, target_crs)
wells_valid    <- st_transform(wells_valid,    target_crs)

# Use planar geometry operations (important for buffering and within)
sf::sf_use_s2(FALSE)

thr_class <- 1        # maximum allowed difference in K index (class threshold)
radius_m  <- 5000     # search radius for "nearby" wells if no inside match exists (5 km)

catch_small <- catch_dominant %>% select(catch_id, K_index_c)
well_small  <- wells_valid    %>% select(well_id, K_index_w)

# Optional quick check (if original L_KF exists in wells)
# sum(wells$L_KF == 11, na.rm = TRUE)

# ===============================
# 13. Match wells inside catchments (interior wells)
# ===============================
pairs_interior <- st_join(well_small, catch_small, join = st_within, left = FALSE) %>%
  st_drop_geometry() %>%
  mutate(dK_in = abs(K_index_w - K_index_c))

best_interior <- pairs_interior %>%
  group_by(catch_id) %>%
  summarise(min_dK_in = suppressWarnings(min(dK_in, na.rm = TRUE)), .groups = "drop")

# ===============================
# 14. If needed, match wells within 5 km buffer (adjacent wells)
# ===============================
ct_no_interior <- setdiff(catch_small$catch_id, unique(pairs_interior$catch_id))

ct_interior_no_match <- best_interior %>%
  filter(!(is.finite(min_dK_in) & min_dK_in <= thr_class)) %>%
  pull(catch_id)

ct_need_adj <- union(ct_no_interior, ct_interior_no_match)

if (length(ct_need_adj) > 0) {
  catch_need <- catch_small %>% filter(catch_id %in% ct_need_adj)
  catch_need_buf <- st_buffer(catch_need, dist = radius_m)
  
  pairs_adjacent <- st_join(well_small, catch_need_buf, join = st_within, left = FALSE) %>%
    st_drop_geometry() %>%
    mutate(dK_adj = abs(K_index_w - K_index_c))
  
  # Remove wells already counted as interior for that catchment
  if (nrow(pairs_interior) > 0 && nrow(pairs_adjacent) > 0) {
    pairs_adjacent <- anti_join(
      pairs_adjacent,
      pairs_interior %>% select(catch_id, well_id),
      by = c("catch_id", "well_id")
    )
  }
  
  best_adjacent <- pairs_adjacent %>%
    group_by(catch_id) %>%
    summarise(min_dK_adj = suppressWarnings(min(dK_adj, na.rm = TRUE)), .groups = "drop")
} else {
  best_adjacent <- tibble(catch_id = character(), min_dK_adj = numeric())
}

# ===============================
# 15. Catchment-level classification summary
# ===============================
summary_ct <- catch_small %>%
  st_drop_geometry() %>%
  left_join(best_interior, by = "catch_id") %>%
  left_join(best_adjacent, by = "catch_id") %>%
  left_join(
    catch_dominant %>% st_drop_geometry() %>% select(catch_id, K_code, K_description),
    by = "catch_id"
  ) %>%
  mutate(
    has_good_interior = is.finite(min_dK_in) & (min_dK_in <= thr_class),
    has_good_adjacent = !has_good_interior & is.finite(min_dK_adj) & (min_dK_adj <= thr_class),
    classification = case_when(
      has_good_interior ~ "Representative well inside catchment",
      has_good_adjacent ~ "Representative well within 5 km",
      TRUE              ~ "No representative well"
    )
  ) %>%
  distinct(catch_id, .keep_all = TRUE)

# ===============================
# 16. Headline counts
# ===============================
counts <- summary_ct %>%
  summarise(
    total_catchments           = n(),
    with_representative_inside = sum(classification == "Representative well inside catchment"),
    with_representative_nearby = sum(classification == "Representative well within 5 km"),
    no_representative          = sum(classification == "No representative well")
  )

cat("\n=== HEADLINE NUMBERS ===\n")
print(counts)

# ===============================
# 17. Export catchments with no representative well
# ===============================
out_ids <- summary_ct %>%
  filter(classification == "No representative well") %>%
  select(catch_id, K_code, K_description)

out_file <- file.path(out_dir, "catchments_needing_investigation_final.csv")
write.csv(out_ids, out_file, row.names = FALSE, fileEncoding = "UTF-8")

cat("\nCSV file saved to:\n", out_file, "\n")

# ===============================
# 18. Build table of ALL representative wells per catchment
# ===============================
# Interior candidates within class threshold
cand_interior <- pairs_interior %>%
  filter(is.finite(dK_in), dK_in <= thr_class) %>%
  transmute(catch_id, well_id, match_type = "inside", dK = dK_in)

# Adjacent candidates within class threshold (only if computed)
cand_adjacent <- if (exists("pairs_adjacent")) {
  pairs_adjacent %>%
    filter(is.finite(dK_adj), dK_adj <= thr_class) %>%
    transmute(catch_id, well_id, match_type = "outside", dK = dK_adj)
} else {
  tibble(catch_id = character(), well_id = character(),
         match_type = character(), dK = numeric())
}

rep_wells_all <- bind_rows(cand_interior, cand_adjacent)

# Join descriptive attributes for auditing
rep_wells_all <- rep_wells_all %>%
  left_join(
    wells %>% st_drop_geometry() %>%
      select(well_id, K_code_well = K_code, K_desc_well = K_description, K_index_w),
    by = "well_id"
  ) %>%
  left_join(
    catch_dominant %>% st_drop_geometry() %>%
      select(catch_id, K_code_cat = K_code, K_desc_cat = K_description, K_index_c),
    by = "catch_id"
  ) %>%
  arrange(catch_id, match_type, well_id)

# Add a numeric catchment index for easier reading
rep_wells_all <- rep_wells_all %>%
  mutate(catch_no = as.numeric(factor(catch_id, levels = unique(catch_id)))) %>%
  relocate(catch_no, .before = catch_id)

cat("\n=== Representative wells per catchment (all) ===\n")
print(head(rep_wells_all, 10))

# ===============================
# 19. Summary table per catchment (counts of inside/outside candidates)
# ===============================
summary_reps <- rep_wells_all %>%
  group_by(catch_id) %>%
  summarise(
    catch_no  = first(catch_no),
    n_inside  = sum(match_type == "inside"),
    n_outside = sum(match_type == "outside"),
    n_total   = n(),
    .groups = "drop"
  ) %>%
  arrange(catch_no)

cat("\n=== Summary of representative wells per catchment ===\n")
print(summary_reps)

# ===============================
# 20. Export outputs
# ===============================
write_csv(rep_wells_all, file.path(out_dir, "rep_wells_all.csv"))

