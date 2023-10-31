library(tidyverse)
library(sf)
library(here)
library(nngeo)
library(units)

here("code",
     "helpers.R") |>
  source()

parcels <- here("data",
                "County_Parcels.geojson") |>
  st_read() |> 
  st_transform(2812)

streets <- here("data",
                "Hennepin_County_Street_Centerlines.geojson") |>
  st_read() |> 
  st_transform(2812)

edges_labeled <- fn_label_sides(parcels, streets)

