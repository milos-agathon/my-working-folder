#######################################################
# COMPUTE AND MAP SHORTEST DRIVING DISTANCE           #
# Milos Popovic                                       #
# 2021-06-20                                          #
#######################################################

# 	|￣￣￣￣￣￣|
# 	| BEGIN      |
# 	| REPLICATION|
# 	|            |
# 	|            |
# 	| ＿＿＿＿＿__|
# 	(\__/) ||
# 	(•ㅅ•) ||
# 	/ 　 づ

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

# download official 2021 Serbian census circles

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

# fetch Belgrade's petrol stations
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

# get Belgrade's paved roads
bg_way <- opq(bbox = belgrade_bbox, timeout = 120, memsize = 104857600) |>
  add_osm_feature(
    key = "highway"
  ) |>
  osmdata_sf(quiet = T)

bg_r <- bg_way$osm_lines

# let's peek into the locations of ATMs/banks and roads
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



# apartments

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
  geom_sf(data = bg_apartments_pts, color = "red", inherit.aes = F)



# decompose roads into distinct edges
g <- dodgr::weight_streetnet(
  bg_r,
  wt_profile = "motorcar",
  type_col = "highway"
)
dim(g)
head(g)

# define origin, destination and compute distance

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

# This is where we will put our results
# route_results <- list()

# for (i in 1:nrow(route_distances)) {
#   list_of_graph_edges <- dodgr_paths(g,
#     from = from,
#     to = to,
#     vertices = FALSE
#   )
#   # [[1]][[1]]

#   route_results[[i]] <- list_of_graph_edges
# }

# nrow(route_distances)
# length(route_results)
# head(route_results)
# head(g)

# unique(route_results)
# shortest to dist
# 9804675161

unique(paths_sf$to_id)

shortest_path <- paths_sf |>
  select(from_id, to_id) |>
  dplyr::filter(
    to_id == 9804675161 |
    from_id == 9804675161
  )

ggplot() +
  tidyterra::geom_spatraster_rgb(
    data = belgrade_streets
  ) +
  geom_sf(
    data = shortest_path,
    inherit.aes = F,
    col = "purple",
    size = .15
  ) +
  geom_sf(
    data = paths_sf,
    inherit.aes = F,
    col = "black",
    alpha = .45,
    size = .15
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


df <- apply(dists, 1, FUN = min, na.rm = TRUE) %>% as.data.frame()

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

df <- apply(
  d, 1,
  FUN = min, na.rm = TRUE
) |>
  as.data.frame()

df <- cbind(id = rownames(df), df)
rownames(df) <- NULL
names(df)[2] <- "dist"

head(df)
nrow(df)



ggplot() +
  tidyterra::geom_spatraster_rgb(
    data = belgrade_streets
  ) +
  geom_sf(
    data = graph_undir,
    inherit.aes = F,
    col = "#d94496",
    alpha = .6,
    size = 1.5
  ) +
  geom_sf(
    data = bg_apartments_pts,
    inherit.aes = F,
    col = "green",
    size = 1.5
  ) +
  geom_sf(
    data = bg_p,
    inherit.aes = F,
    col = "purple",
    size = 1.5
  ) +
  coord_sf(
    crs = 4326,
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax)
  ) +
  theme_void()








b <- sf::st_sf(data.frame(pkb, df))
head(bg_apartments)
names(b)[14] <- "dist"

summary(b$dist)
nrow(subset(b, dist == "Inf"))

# calculate the aerial distance from every centroid to the closest station
b$id <- 1:max(nrow(b)) # create id
binf <- subset(b, dist == "Inf") # subset rows with infinite values
cinf <- subset(cen, id %in% binf$id) # filter coords by rows with infinite values
c <- st_coordinates(cinf) # get coordinates
c <- as.data.frame(cbind(c, cinf$id)) # convert into df
names(c) <- c("long", "lat", "id") # choose intuitive names

# function to find the shortest aerial distance
min_dist <- function(loc) {
  sd <- c[c$id == loc, ]
  sd1 <- distGeo(sd[, 1:2], to[, 1:2])
  sd2 <- data.frame(
    id = loc,
    dist = min(sd1)
  )
  return(sd2)
}
dist_mat <- dplyr::bind_rows(lapply(c$id, min_dist))

# plug new distances back into "b" and create "bb"
# use if_else to replace infinite values with aerial distance
bb <- b |>
  left_join(dist_mat, by = "id") |>
  mutate(dist = if_else(is.infinite(dist.x), dist.y, dist.x))

# let's find a natural interval with quantile breaks
ni <- classIntervals(bb$dist,
  n = 8,
  style = "quantile"
)$brks

# this function uses above intervals to create categories
labels <- c()
for (i in 1:length(ni)) {
  labels <- c(labels, paste0(
    round(ni[i], 0),
    "–",
    round(ni[i + 1], 0)
  ))
}
labels <- labels[1:length(labels) - 1]

# finally, carve out the categorical variable based on the breaks and labels
bb$cat <- cut(bb$dist,
  breaks = ni,
  labels = labels,
  include.lowest = T
)
levels(bb$cat) # let's check how many levels it has(8)

# plot
p <- ggplot() +
  geom_sf(data = bb, aes(fill = cat), color = NA, size = 0) +
  coord_sf(crs = 4326, datum = NA) +
  scale_fill_manual(
    name = "meters",
    values = c(
      "#ffffca", "#b9e0ad", "#72c099", "#109c99",
      "#117581", "#154f68", "#122c4e", "#0c0636"
    ),
    labels = c(
      "0–427", "427–601", "601–755",
      "755–903", "903–1066", "1066–1288",
      "1288–1758", ">1758"
    ),
    drop = F
  ) +
  guides(fill = guide_legend(
    direction = "horizontal",
    keyheight = unit(1.15, units = "mm"),
    keywidth = unit(20, units = "mm"),
    title.position = "top",
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  )) +
  theme_minimal() +
  theme(
    text = element_text(family = "Georgia"),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(size = 9, color = "white", hjust = .7, vjust = 200),
    axis.title.y = element_blank(),
    legend.position = c(.5, -.015),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 11, color = "grey20"),
    panel.grid.major = element_line(color = "white", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(t = 0, r = 0, b = 0, l = 0), "lines"), # added these narrower margins to enlarge map
    plot.title = element_text(face = "bold", size = 17, color = "#095169", hjust = .5, vjust = -2),
    plot.subtitle = element_text(size = 16, color = "#53ba83", hjust = .5, vjust = -2),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank()
  ) +
  labs(
    x = "©2021 Milos Popovic https://milospopovic.net\n Data: OSM Geofabrik",
    title = "Shortest driving distance to a petrol station in Belgrade",
    subtitle = "at census circle level",
    caption = ""
  )

# 	|￣￣￣￣￣￣ |
#    	| END        |
#    	| REPLICATION|
#   	|            |
#   |            |
#    | ＿＿＿＿＿__|
#    	(\__/) ||
#    	(•ㅅ•) ||
#    	/ 　 づ
