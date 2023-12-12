#######################################################
# COMPUTE AND MAP SHORTEST DRIVING DISTANCE           #
# Milos Popovic                                       #
# 2021-06-20                                          #
#######################################################

# 	|￣￣￣￣￣￣|
# 	| START!   |
# 	|          |
# 	|          |
# 	|          |
# 	| ＿＿＿＿＿ |
# 	(\__/) ||
# 	(•ㅅ•) ||
# 	/ 　 づ

# 1. PACKAGES
# libraries we need
libs <- c(
  "tidyverse", "osmdata", "dodgr",
  "sf", "classInt", "geosphere", "maptiles"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())

if (any(installed_libs == FALSE)) {
  install.packages(libs[!installed_libs], dependencies = T)
}

# load libraries
invisible(lapply(libs, library, character.only = TRUE))

# 2. BOUNDING BOX

# 20.417487,44.800668,20.484438,44.832196

xmin <- 20.41
xmax <- 20.48
ymin <- 44.8
ymax <- 44.83

belgrade_bbox <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax) |>
  sf::st_bbox(crs = 4326) |>
  sf::st_as_sfc(
    crs = 4326
  )

# 3. STREET LAYER

# define Belgrade's bounding box based on ggmap's bounding box
belgrade_streets <- maptiles::get_tiles(
  belgrade_bbox,
  provider = "CartoDB.Positron",
  zoom = 16
)

ggplot() +
  tidyterra::geom_spatraster_rgb(
    data = belgrade_streets
  )

# 4. FUEL STATIONS
bg_amen <- osmdata::opq(
  bbox = belgrade_bbox, timeout = 180, memsize = 104857600
) |>
  add_osm_feature(
    key = "amenity",
    value = "fuel"
  ) |>
  osmdata_sf(quiet = FALSE)
bg_pts <- bg_amen[c("osm_points")] # filter only points
bg_p <- do.call(rbind, bg_pts) |> # turn from list to sf object
  select("osm_id", "geometry")

ggplot() +
  tidyterra::geom_spatraster_rgb(
    data = belgrade_streets
  ) +
  geom_sf(data = bg_p, color = "red", inherit.aes = F)

# 5. OSM ROADS
bg_way <- opq(
  bbox = belgrade_bbox,
  timeout = 120,
  memsize = 104857600
) |>
  add_osm_feature(
    key = "highway"
  ) |>
  osmdata_sf(quiet = T)

bg_r <- bg_way$osm_lines

ggplot() +
  tidyterra::geom_spatraster_rgb(
    data = belgrade_streets
  ) +
  geom_sf(
    data = bg_r,
    inherit.aes = F,
    col = "#3875D9",
    size = .15
  ) +
  geom_sf(
    data = bg_p,
    inherit.aes = F,
    col = "#d94496",
    alpha = .6,
    size = 1.5
  ) +
  coord_sf(
    crs = 4326,
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax)
  ) +
  theme_void()


# 6. APARTMENTS

bg_apartments <- osmdata::opq(
  bbox = belgrade_bbox, timeout = 180, memsize = 104857600
) |>
  add_osm_feature(
    key = "building",
    value = "apartments"
  ) |>
  osmdata_sf(quiet = FALSE)

bg_apartments_pts <- bg_apartments[c("osm_points")] # filter only points
bg_apartments_pts <- do.call(
  rbind, bg_apartments_pts
) |> # turn from list to sf object
  select("osm_id", "geometry") |>
  dplyr::sample_n(1)

ggplot() +
  tidyterra::geom_spatraster_rgb(
    data = belgrade_streets
  ) +
  geom_sf(
    data = bg_apartments_pts,
    color = "red",
    inherit.aes = F
  )

# 7. ROADS TO EDGES
g <- dodgr::weight_streetnet(
  bg_r,
  wt_profile = "motorcar",
  type_col = "highway"
)
dim(g)
head(g)

# 8. ORIGIN, DESTINATION, SHORTEST DISTANCE
from <- sf::st_coordinates(bg_apartments_pts)
to <- sf::st_coordinates(bg_p)

route_distances <- dodgr::dodgr_dists(
  graph = g, from = from,
  to = to, shortest = T
)

lines_df <- data.frame(
  from_lon = from[, 1],
  from_lat = from[, 2],
  to_lon = to[, 1],
  to_lat = to[, 2],
  distance = as.vector(route_distances)
) |>
  dplyr::arrange(distance)

head(lines_df)
nrow(lines_df)
head(graph_undir)

paths_sf <- dodgr_flows_aggregate(
  graph = g,
  from = from,
  to = to,
  flows = matrix(1, nrow(from), nrow(to))
) |>
  merge_directed_graph() |>
  dodgr_to_sf()

ggplot() +
  tidyterra::geom_spatraster_rgb(
    data = belgrade_streets
  ) +
  geom_sf(
    data = paths_sf,
    inherit.aes = F,
    col = "black",
    alpha = .65,
    size = .15
  ) +
  geom_point(
    data = lines_df,
    aes(x = from_lon, y = from_lat),
    inherit.aes = F,
    col = "blue",
    size = 1.5
  ) +
  geom_point(
    data = lines_df,
    aes(x = to_lon, y = to_lat),
    inherit.aes = F,
    col = "red",
    size = 1.5
  ) +
  coord_sf(
    crs = 4326,
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax)
  ) +
  theme_void()

# 9. SHORTEST ROUTE EDGES

route_distances_df <-
  route_distances |>
  as.matrix() |>
  as.data.frame() |>
  t()

route_distances_df <- cbind(
  edge_id = rownames(route_distances_df),
  route_distances_df
) |>
  as.data.frame() |>
  dplyr::rename(
    distance = `2097635499`
  ) |>
  dplyr::mutate(
    distance = as.numeric(distance)
  ) |>
  dplyr::arrange(distance) |>
  dplyr::distinct(
    edge_id, distance,
    .keep_all = T
  ) |>
  dplyr::slice_head(n = 1)

rownames(route_distances_df) <- NULL
names(route_distances_df)[2] <- "distance"

head(route_distances_df)

route_results <- list()

for (i in 1:nrow(route_distances)) {
  list_of_graph_edges <- dodgr::dodgr_paths(
    g,
    from = from,
    to = to,
    vertices = FALSE
  )
  # [[1]][[1]]

  route_results[[i]] <- list_of_graph_edges
}

str(list_of_graph_edges)
class(shortest_line)

df_graph_edges <- unlist(
  list_of_graph_edges
) |>
  as.matrix() |>
  as.data.frame()

df_graph_edges <- cbind(id = rownames(df_graph_edges), df_graph_edges)
rownames(df_graph_edges) <- NULL
names(df_graph_edges)[2] <- "edge_id"


shortest_line <- df_graph_edges |>
  dplyr::filter(
    stringr::str_detect(
      id, route_distances_df$edge_id
    )
  ) |>
  dplyr::mutate(edge_id = as.character(edge_id))

unique(shortest_line$edge_id)
unique(paths_sf$geom_num)

gsf <- dodgr::dodgr_to_sf(g)

shortest_path_sf <- gsf |>
  dplyr::select(edge_id) |>
  dplyr::filter(
    edge_id %in% unique(shortest_line$edge_id)
  )

unique(gsf$edge_id)


ggplot() +
  tidyterra::geom_spatraster_rgb(
    data = belgrade_streets
  ) +
  # geom_sf(
  #   data = shortest_path_sf,
  #   inherit.aes = F,
  #   col = "purple",
  #   size = .3
  # ) +
  geom_sf(
    data = paths_sf,
    inherit.aes = F,
    col = "black",
    alpha = .25,
    size = .1
  ) +
  geom_point(
    data = lines_df,
    aes(x = from_lon, y = from_lat),
    inherit.aes = F,
    col = "purple",
    size = 1.5
  ) +
  geom_point(
    data = lines_df,
    aes(x = to_lon, y = to_lat),
    inherit.aes = F,
    col = "red",
    size = 1.5
  ) +
  coord_sf(
    crs = 4326,
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax)
  ) +
  theme_void()



names(dists) <- c("from_id", "to_id", "distance")


head(dists)
nrow(dists)

d <- dodgr::dodgr_flows_si(
  graph = g, from = from,
  to = to
)

str(d)
nrow(d)
head(d)
unique(d$from_id)

graph_undir <- merge_directed_graph(d) |>
  dodgr_to_sf()

head(graph_undir)
nrow(graph_undir)

# 	|￣￣￣￣￣￣ |
#    	| END        |
#    	| REPLICATION|
#   	|            |
#   |            |
#    | ＿＿＿＿＿__|
#    	(\__/) ||
#    	(•ㅅ•) ||
#    	/ 　 づ


net <- weight_streetnet(hampi)
v <- dodgr_vertices(net)
head(v)
set.seed(1)
xy <- v[sample(nrow(v), size = 10), ]
head(xy)
p <- dodgr_paths(graph = net, from = xy, to = xy)
head(p)
class(v$id)
# convert paths to sf-format:
p_sf <- lapply(p, function(i) {
  lapply(i, function(j) {
    path_ij <- v[match(j, v$id), ]
    st_linestring(as.matrix(path_ij[, c("x", "y")])) |>
      st_sfc(crs = 4326)
  })
})

# Then unlist to convert to single 'sfc' object:
p_sf <- lapply(p_sf, function(i) do.call(rbind, i))
p_sf <- do.call(rbind, p_sf)

# add 'from' and 'to' columns:
p_sf <- st_sf(
  from = rep(names(p), each = nrow(xy)),
  to = rep(names(p), times = nrow(xy)),
  geometry = p_sf[, 1],
  crs = 4326
)
