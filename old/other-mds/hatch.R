library(cartography)

census_tracts <- read_sf("/srv/data/penn/francesca/census-tracts.shp") %>%
  st_transform(4326)

# match tract IDs and merge in diff in % black
census_tracts <- census_tracts |>
  merge(map_visual,
        by.x = "TRACTCE10",
        by.y = "tract")

#plot census tracts
plot(census_tracts["ratio_buckets"],
     pal = viridisLite::inferno(5, direction = -1),
     key.pos = 1,
     key.width = lcm(1.3),
     key.length = 0.5,
     main = "",
     reset = FALSE)

cartography::hatchedLayer(st_geometry(census_tracts[is.na(census_tracts$ratio),]),
                          "left2right",
                          col = "black",
                          density = 7,
                          add = TRUE)