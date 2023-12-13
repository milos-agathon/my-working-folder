# 	|￣￣￣￣￣￣|
# 	| START!    |
# 	|           |
# 	|           |
# 	|           |
# 	| ＿＿＿＿＿_|
# 	(\__/) ||
# 	(•ㅅ•) ||
# 	/ 　 づ

# 1. PACKAGES

libs <- c(
    "tidyverse", "osmdata",
    "dodgr", "sf", "maptiles",
    "tidyterra"
)

installed_libs <- libs %in% rownames(
    installed.packages()
)

if (any(installed_libs == F)) {
    install.packages(
        libs[!installed_libs],
        dependencies = T
    )
}

invisible(
    lapply(
        libs, library,
        character.only = T
    )
)

# 2. BOUNDING BOX
# 20.417487,44.800668,20.484438,44.832196

xmin <- 20.4
xmax <- 20.48
ymin <- 44.8
ymax <- 44.8

belgrade_bbox <- c(
    xmin = xmin, ymin = ymin,
    xmax = xmax, ymax = ymax
) |>
    sf::st_bbox(crs = 4326) |>
    sf::st_as_sfc(crs = 4326)

# 3. GET STREET LAYER

belgrade_streets <- maptiles::get_tiles(
    belgrade_bbox,
    provider = "CartoDB.Positron",
    zoom = 16
)

p1 <- ggplot() +
    tidyterra::geom_spatraster_rgb(
        data = belgrade_streets
    )

ggsave("streets.png", p1)

# 4. GET FUEL STATIONS

belgrade_amenities <- osmdata::opq(
    bbox = belgrade_bbox,
    timeout = 180,
    memsize = 104857600
) |>
    osmdata::add_osm_feature(
        key = "amenity",
        value = "fuel"
    ) |>
    osmdata::osmdata_sf()

belgrade_fuel_pts <- belgrade_amenities[c("osm_points")]
belgrade_fuel <- do.call(rbind, belgrade_fuel_pts) |>
    dplyr::select("osm_id", "geometry")

ggplot() +
    tidyterra::geom_spatraster_rgb(
        data = belgrade_streets
    ) +
    geom_sf(
        data = belgrade_fuel,
        color = "blue",
        inherit.aes = F
    )

# 5. GET OSM ROADS

belgrade_roads <- osmdata::opq(
    bbox = belgrade_bbox,
    timeout = 180,
    memsize = 104857600
) |>
    osmdata::add_osm_feature(
        key = "highway"
    ) |>
    osmdata::osmdata_sf()

belgrade_hways <- belgrade_roads$osm_lines

ggplot() +
    tidyterra::geom_spatraster_rgb(
        data = belgrade_streets
    ) +
    geom_sf(
        data = belgrade_fuel,
        color = "blue",
        inherit.aes = F
    ) +
    geom_sf(
        data = belgrade_hways,
        color = "black",
        size = .15,
        alpha = .5,
        inherit.aes = F
    ) +
    theme_void()

# 6. DECOMPOSE ROADS INTO DISTINCT EDGES

g <- dodgr::weight_streetnet(
    belgrade_hways,
    wt_profile = "motorcar",
    type_col = "highway"
)

head(g)

# 7. CALCULATE PATHS

v <- dodgr::dodgr_vertices(g)

xy <- v[sample(
    nrow(v),
    size = 1
)]

to <- sf::st_coordinates(belgrade_fuel)

paths <- dodgr::dodgr_paths(
    graph = g,
    from = xy,
    to = to
)

# 8. MATCH PATHS AND TURN THEM TO SF

paths_sf <- lapply(
    paths, function(x) {
        lapply(
            x, function(y) {
                path_xy <- v[match(
                    y, v$id
                ), ]
                sf::st_linestring(
                    as.matrix(
                        path_xy[, c("x", "y")]
                    )
                ) |>
                    sf::st_sfc(crs = 4326)
            }
        )
    }
)

paths_sf <- lapply(
    paths_sf, function(x) {
        do.call(
            rbind, x
        )
    }
)

paths_sf <- do.call(rbind, paths_sf)

paths_sf <- sf::st_sf(
    from = rep(
        names(paths),
        each = nrow(xy)
    ),
    to = rep(
        names(paths),
        times = nrow(xy)
    ),
    geometry = paths_sf[, 1],
    crs = 4326
)

ggplot() +
    tidyterra::geom_spatraster_rgb(
        data = belgrade_streets
    ) +
    geom_sf(
        data = belgrade_fuel,
        color = "blue",
        inherit.aes = F
    ) +
    geom_point(
        data = xy,
        aes(x = x, y = y),
        color = "red",
        inherit.aes = F
    ) +
    geom_sf(
        data = paths_sf,
        color = "black",
        alpha = .65,
        size = .25,
        inherit.aes = F
    ) +
    theme_void()

# 9. WHAT IS THE SHORTEST?

# compute distances
route_distances <- dodgr::dodgr_dists(graph = g, from = xy, to = to, shortest = T)

# convert to data frame and transpose
route_distances_df <- t(as.data.frame(as.matrix(route_distances)))

# get the shortest route distance and edge id
shortest_route_distance_df <- route_distances_df %>%
  as.data.frame() %>%
  rownames_to_column("edge_id") %>%
  mutate(distance = as.numeric(`8951126837`)) %>%
  arrange(distance) %>%
  distinct(edge_id, distance, .keep_all = T) %>%
  slice_head(n = 1)

# get all paths and convert to data frame
df_graph_edges <- as.data.frame(as.matrix(unlist(paths))) %>%
  rownames_to_column("id") %>%
  rename(edge_id = V1)

# find shortest path among all paths
shortest_line <- df_graph_edges %>%
  filter(str_detect(id, shortest_route_distance_df$edge_id))

# fetch shortest line from street network
gsf <- dodgr::dodgr_to_sf(g)
shortest_path_sf <- gsf %>%
  filter(to_id %in% unique(shortest_line$edge_id)) %>%
  st_intersection(paths_sf)

p5 <- ggplot() +
    tidyterra::geom_spatraster_rgb(
        data = belgrade_streets
    ) +
    geom_sf(
        data = bg_p,
        color = "blue",
        inherit.aes = F
    ) +
    geom_point(
        data = xy,
        aes(x = x, y = y),
        color = "red",
        inherit.aes = F
    ) +
    geom_sf(
        data = paths_sf,
        color = "black",
        alpha = .5,
        size = .15,
        inherit.aes = F
    ) +
    geom_sf(
        data = shortest_path_sf,
        color = "red",
        size = .8,
        inherit.aes = F
    ) +
    theme_void()

ggsave("final_shortest.png", p5)
