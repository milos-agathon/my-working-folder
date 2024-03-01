setwd("/Users/mpopovic3/Downloads/")

# 1. PACKAGES

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
    tidyverse, sf,
    anyflights, igraph,
    tidygraph, ggraph, tidygraph
)

sf::sf_use_s2(F)

# 2. US STATES

states <- map_data("state")
states_sf <- sf::st_as_sf(
    states,
    coords = c("long", "lat"),
    crs = 4326
) |>
    dplyr::group_by(group) |>
    dplyr::summarise(
        geometry = sf::st_combine(geometry)
    ) |>
    sf::st_cast("POLYGON") |>
    sf::st_transform("ESRI:102003")

plot(sf::st_geometry(states_sf))

# 3. AIRPORTS

us_airports <- anyflights::get_airports() |>
    dplyr::select(1, 3:4)
head(us_airports)

# transform to NAD83
us_airports_sf <- us_airports |>
    sf::st_as_sf(
        crs = 4326,
        coords = c("lon", "lat"),
        remove = FALSE
    ) |>
    sf::st_transform(crs = "ESRI:102003") |>
    sf::st_intersection(
        states_sf
    )

plot(sf::st_geometry(states_sf))
plot(sf::st_geometry(us_airports_sf), add = TRUE)

coords <- st_coordinates(us_airports_sf)

us_airports_df <- us_airports_sf |>
    sf::st_drop_geometry() |>
    dplyr::select(1) |>
    dplyr::mutate(
        lon = coords[, 1],
        lat = coords[, 2]
    )

head(us_airports_df)

# 4. FLIGHTS

# all flights
station <- unique(us_airports_df$faa)

us_flights <- anyflights::get_flights(
    station = station,
    year = 2023,
    month = 11
)

# distinct edge list

us_flights_distinct <- us_flights |>
    dplyr::select("origin", "dest") |>
    dplyr::distinct()

# vertices matching edge list

vertices <- dplyr::filter(
    us_airports_df,
    faa %in% us_flights_distinct$origin &
        faa %in% us_flights_distinct$dest
)

# edge list matching vertices

us_flights_distinct <- dplyr::filter(
    us_flights_distinct,
    origin %in% vertices$faa &
        dest %in% vertices$faa
)

# c(us_flights_distinct$origin, us_flights_distinct$dest) %in% vertices$faa

# 5. NETWORK GRAPH

g <- igraph::graph_from_data_frame(
    d = us_flights_distinct,
    directed = TRUE,
    vertices = vertices
)

gr <- tidygraph::as_tbl_graph(g)

# 6. MAP

ggraph::ggraph(
    gr,
    x = lon,
    y = lat
) +
    geom_sf(
        data = states_sf,
        fill = "grey10",
        color = "white",
        linewidth = .3
    ) +
    geom_edge_bundle_path(
        color = "#FA57B1",
        width = .05,
        alpha = .4
    ) +
    coord_sf(
        crs = sf::st_crs(states_sf)
    ) +
    theme_void()
