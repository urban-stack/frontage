fn_label_culdsides <- function(my_parcels,
                           my_streets,
                           my_crs,
                           close_threshold = 3,
                           parallel_threshold = 15,
                           culd_buffer = 25) {

#dividing parcels into individual edges with a unique edge_id
edges <- my_parcels |>
  st_transform(my_crs) |>
  st_segments(progress = FALSE) 

  
edges$edge_id <- seq(1:nrow(edges))

#adding a column 'unique' that =TRUE when the segment is apart of two lots 
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
  
edges$overlap <- as.logical(apply(overlap_logvec, 1, any))

edges <- edges|>
    mutate(culdnear = ifelse(!overlap, FALSE, TRUE))|>
    mutate(culdfront = ifelse(culdnear & unique, TRUE, FALSE))|>
    select(!overlap)
  
#labeling which edges meet my conditions for front and side, marking all that don't as rear
culdedges_labeled.1 <- edges|>
    mutate(side.1 = case_when(culdfront ~ "front",
                              !unique ~ "side"))|>
mutate(side = ifelse(is.na(side.1), "rear", side.1))

#filtering for parcels that are out of the cul de sac buffer
edges <- culdedges_labeled.1 |>
  st_transform(2812)|>
  group_by(PID)|>
  mutate(endlot = ifelse(sum(culdfront) > 0, TRUE, FALSE))|>
  filter(!endlot)|>
  ungroup()

#creating an index of the two nearest streets to each edge
close_street_index <- st_nn(edges, brokenstreets, 
                            k = 2, progress = FALSE)

 close_street_index_df <- matrix(unlist(close_street_index),
                                ncol=2,
                                byrow=TRUE) |>
  as_tibble() 

#creating a tibble of street segments identified in  close_street_index
close_street_1 <- brokenstreets[close_street_index_df$V1,] 
close_street_2 <- brokenstreets[close_street_index_df$V2,]

#using fun_bearing to identify the angle of the two nearest streets and adding that to edges tibble
edges$street_bearing_1 <- fun_bearing(st_transform(close_street_1, my_crs))
edges$street_bearing_2 <- fun_bearing(st_transform(close_street_2, my_crs))
edges$edge_bearing <- fun_bearing(st_transform(edges, my_crs))


# this long pipe labels the edges
# front = edge is parallel to the street nearest to the parcel
# rear = parallel to front but not nearest to nearest street
# side = everything else 
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
                          angle_from_front < parallel_threshold & unique~ "rear",
                          !unique ~ "side",
                          TRUE ~ "side"))|>
  select(PID, side)|>
  group_by(PID, side) |>
  summarise(geometry = st_union(result))

#combining the labelled edges 
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
  

fn_iron_edges <- function(my_edges,
                          my_crs,
                          parallel_threshold = 5) {

edges <- my_edges|>
  st_transform(my_crs)|>
  st_segments(progress = FALSE)
  
close_edge_index <- st_nn(edges, edges, 
                              k = 2, progress = FALSE)
  
close_edge_index_df <- matrix(unlist(close_edge_index),
                                  ncol=2,
                                  byrow=TRUE) |>
    as_tibble() 
  
#creating a tibble of edges segments identified in  close_edge_index
close_edge_1 <- edges[close_edge_index_df$V1,] 
close_edge_2 <- edges[close_edge_index_df$V2,]
  
#using fun_bearing to identify the angle of the two nearest streets and adding that to edges tibble
edges$neighbor_bearing_1 <- fun_bearing(close_edge_1)
edges$neighbor_bearing_2 <- fun_bearing(close_edge_2)
edges$edge_bearing <- fun_bearing(edges)

edges$neighbor_side_1 <- close_edge_1$side
edges$neighbor_side_2 <- close_edge_2$side

edges <- edges |>
  mutate(middleline = ifelse(abs(edge_bearing - neighbor_bearing_1) < parallel_threshold &
                        abs(edge_bearing - neighbor_bearing_2) < parallel_threshold,
                         TRUE, FALSE))|>
  mutate(discontinuous = ifelse(middleline & (side != neighbor_side_1 | side != neighbor_side_2),
                             TRUE, FALSE))|>
  mutate(side = case_when(discontinuous & neighbor_side_1 == neighbor_side_2 ~ neighbor_side_1,
                              TRUE ~ side))|>
  select(PID, side) |>
  group_by(PID, side) |>
  summarise(geometry = st_union(result))
} 
  