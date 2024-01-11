fn_label_culdsides <- function(my_parcels,
                           my_streets,
                           close_threshold = 3,
                           parallel_threshold = 15,
                           culd_buffer = 25,
                           my_crs = 2812) {
edges <- my_parcels |>
  st_segments(progress = FALSE) |>
  st_transform(my_crs)

  
edges$edge_id <- seq(1:nrow(edges))
  
shared <- st_equals(edges, edges, remove_self = TRUE) |>
  sapply(length)
  
  edges$unique = (shared == 0)
  
#segmenting the streets
brokenstreets <- my_streets |>
    st_segments()|>
    st_as_sf()|>
    st_transform(my_crs)
  
brokenstreets$id <- seq(1:nrow(brokenstreets))
  
#finding end points of the street segments
end_points <- st_cast(brokenstreets, "POINT")
  
#identifying unique end points
shared <- st_equals(end_points, end_points, remove_self = TRUE) %>%
    sapply(length)
  
  end_points$unique <- (shared == 0)
  
#creating a logical vector identifying groups where there is >0 unique end points
  endvec <- end_points |>
    group_by(id)|>
    mutate(end = ifelse(sum(unique) > 0, TRUE, FALSE))
  
#removing every other row so there is an equal amount of rows in endvec and broken streets
  endvec = endvec[seq(1, nrow(endvec), 2), ]
  
#adding this information back into the streets data set
brokenstreets$end <- endvec$end
  
brokenstreets <- brokenstreets|>
    mutate(endval = ifelse(end == TRUE, 0, 1))
  
#creating a buffer around the culdesac ends
end_orb <- end_points|>
    filter(unique == TRUE)|>
    st_buffer(culd_buffer)
  
#labeling the lot edges that are within culdesac buffer 
overlap_logvec <- st_intersects(edges, end_orb)
  
result_tibble <- tibble(
    overlap = as.numeric(overlap_logvec),
    edge_id = edges$edge_id)
  
edges <- edges|>
    left_join(result_tibble)|>
    mutate(culdnear = ifelse(is.na(overlap), FALSE, TRUE))|>
    mutate(culdfront = ifelse(culdnear & unique, TRUE, FALSE))|>
    select(!overlap)
  
#labeling which edges meet my conditions for front and side, marking all that don't as rear
culdedges_labeled.1 <- edges|>
    mutate(side.1 = case_when(culdfront ~ "front",
                              !unique ~ "side"))|>
mutate(side = ifelse(is.na(side.1), "rear", side.1))

edges <- culdedges_labeled.1 |>
  st_transform(2812)|>
  group_by(PID)|>
  mutate(endlot = ifelse(sum(culdfront) > 0, TRUE, FALSE))|>
  filter(!endlot)|>
  ungroup()

streets <- my_streets|>
  st_transform(2812)

#using the helpers code to identify the street bearings
close_street_index <- st_nn(edges, streets, 
                            k = 2, progress = FALSE)

close_street_index_df <- matrix(unlist(close_street_index),
                                ncol=2,
                                byrow=TRUE) |>
  as_tibble() 

close_street_1 <- streets[close_street_index_df$V1,] 
close_street_2 <- streets[close_street_index_df$V2,]

edges$street_bearing_1 <- fun_bearing(st_transform(close_street_1, 2812))
edges$street_bearing_2 <- fun_bearing(st_transform(close_street_2, 2812))
edges$edge_bearing <- fun_bearing(st_transform(edges, 2812))


#running a function to label edges with original logic 
culdedges_labeled.2 <- edges |>
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
           unique)|> mutate(nfronts = sum(front),
                            front_bearings = list(edge_bearing[front])) |>
  ungroup() |>
  mutate(angle_from_front = mapply(fn_min_diff, front_bearings, edge_bearing)) |>
  mutate(side = case_when(front ~ "front",
                          angle_from_front < parallel_threshold ~ "rear",
                          !unique ~ "side"))|>
  select(PID, side)|>
  group_by(PID, side) |>
  summarise(geometry = st_union(result))

#combining them
culdedges_labeled.3 <- culdedges_labeled.1|>
  st_transform(2812)|>
  group_by(PID)|>
  mutate(endlot = ifelse(sum(culdfront) > 0, TRUE, FALSE))|>
  filter(endlot)|>
  ungroup()|>
  select(PID, side)|>
  group_by(PID, side) |>
  summarise(geometry = st_union(result))

culdedges_labelled <- bind_rows(culdedges_labeled.2, culdedges_labeled.3)
}
  