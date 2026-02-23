#' Find overlapping area 
#'
#' @param polygons multi-polygons object; shape file of all catchments
#' @param cover_polygons multi-polygon object, e.g., natrate polluted area
#' 
#' @return data frame showing the areal percentages of polygons are covered by 
#' cover_polygons. 
#'
#' @export

overlappingArea <- function(polygons, cover_polygons){
  
  cover_polygons <- st_transform(cover_polygons, 32633)
  polygons <- st_transform(polygons, 32633)
  
  polygons$int_area <- st_area(polygons)
  
  intersections <- st_intersection(polygons, cover_polygons)
  intersections$int_area <- st_area(intersections)
  
  coverage <- intersections %>%
    st_drop_geometry() %>%
    group_by(gauge_id) %>%
    summarise(total_int_area = sum(int_area))
  
  result <- polygons %>%
    st_drop_geometry() %>%
    left_join(coverage, by = "gauge_id") %>%
    mutate(
      total_int_area = ifelse(is.na(total_int_area), 0, total_int_area),
      percent_cover = as.numeric(total_int_area / int_area) * 100
    ) %>%
    select(gauge_id, percent_cover) 
  
  return(result)
  
}
