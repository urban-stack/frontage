library(tidyverse)
library(sf)

fun_bearing = function(my_line) {
  
  if(st_is_longlat(my_line)) {
    stop("Please use a projected coordinate system")
  }
  
  coords <- st_coordinates(my_line) |>
    as_tibble() |>
    group_by(L1) |>
    summarise(height = max(X) - min(X),
              width = max(Y) - min(Y)) |>
    mutate(bearing = atan(width/height)*180/pi)
  
  coords$bearing
}

fn_min_diff <- function(list_nums, num) {
  min(abs(num - unlist(list_nums)))
}

fn_label_sides <- function(my_parcels,
                        my_streets,
                        close_threshold = 3,
                        parallel_threshold = 15) {
  
  edges <- my_parcels |>
    st_segments(progress = FALSE) 
  
  edges$edge_id <- seq(1:nrow(edges))
  
  shared <- st_equals(edges, edges, remove_self = TRUE) |>
    sapply(length)
  
  edges$unique = (shared == 0)
  
  street_segs <- st_segments(streets, progress = FALSE)
  
  close_street_index <- st_nn(edges, street_segs, 
                              k = 2, progress = FALSE)
  
  close_street_index_df <- matrix(unlist(close_street_index),
                                  ncol=2,
                                  byrow=TRUE) |>
    as_tibble() 
  
  close_street_1 <- street_segs[close_street_index_df$V1,] 
  close_street_2 <- street_segs[close_street_index_df$V2,] 
  
  edges$street_bearing_1 <- fun_bearing(st_transform(close_street_1, 2812))
  edges$street_bearing_2 <- fun_bearing(st_transform(close_street_2, 2812))
  edges$edge_bearing <- fun_bearing(st_transform(edges, 2812))
  
  edges_labeled <- edges |>
    mutate(angle_1 = abs(edge_bearing - street_bearing_1),
           angle_2 = abs(edge_bearing - street_bearing_2)) |>
    mutate(dist_to_para_st = ifelse(angle_1 < parallel_threshold,
                                    st_distance(edges, 
                                                close_street_1, 
                                                by_element = TRUE),
                                    st_distance(edges, 
                                                close_street_2, 
                                                by_element = TRUE))) |>
    group_by(PID) |>
    mutate(min_dist = min(dist_to_para_st)) |>
    mutate(diff = dist_to_para_st - min_dist) |>
    mutate(front = diff <= close_threshold &
             unique) |>
    mutate(nfronts = sum(front),
           front_bearings = list(edge_bearing[front])) |>
    ungroup() |>
    mutate(angle_from_front = mapply(fn_min_diff, front_bearings, edge_bearing)) |>
    mutate(side = case_when(front ~ "front",
                            angle_from_front < parallel_threshold ~ "rear",
                            TRUE ~ "side")) |>
    select(PID, side) |>
    group_by(PID, side) |>
    summarise(geometry = st_union(result))
}
