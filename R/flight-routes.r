setwd("/Users/mpopovic3/Downloads/")

libs <- c(
    "tidyverse", "sf", "giscoR",
    "openSkies", "anyflights",
    "ggraph", "tidygraph", "edgebundle"
)

installed_libs <- libs %in% rownames(
    installed.packages()
)

if (any(installed_libs == F)) {
    install.packages(
        libs[!installed_libs]
    )
}

invisible(lapply(
    libs, library,
    character.only = T
))

sf::sf_use_s2(F)

# 1. US STATES

states <- map_data("state")
states_sf <- sf::st_as_sf(
    states,
    coords = c("long", "lat"),
    crs = 4326
) |>
    group_by(group) |>
    summarise(geometry = st_combine(geometry)) |>
    st_cast("POLYGON") |>
    sf::st_transform("ESRI:102003")

plot(sf::st_geometry(states_sf))

# 2. AIRPORTS

url <- "https://davidmegginson.github.io/ourairports-data/airports.csv"

airports <- readr::read_csv(url)
head(airports)

us_airports <- dplyr::filter(
    airports,
    iso_country == "US"
) |>
    dplyr::select(
        4:6, 15
    ) |>
    na.omit()

names(us_airports)
head(us_airports)
nrow(us_airports)

us_airports <- anyflights::get_airports() |>
    dplyr::select(1, 3:4)
head(us_airports)

# transform to NAD83
us_airports_sf <- us_airports %>%
    sf::st_as_sf(
        crs = 4326,
        coords = c("lon", "lat"),
        remove = F
    ) |>
    # sf::st_transform(crs = "ESRI:102003") |>
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

# 3. FLIGHTS

# all flights
station <- unique(us_airports_df$faa)

us_flights <- anyflights::get_flights(
    station = station,
    year = 2023,
    month = 11
)

# distinct dyads

us_flights_distinct <- us_flights |>
    dplyr::select("origin", "dest") |>
    dplyr::distinct()

# vertices matching dyads

vertices <- dplyr::filter(
    us_airports_df,
    faa %in% us_flights_distinct$origin &
        faa %in% us_flights_distinct$dest
)

# dyads matching vertices

us_flights_distinct <- dplyr::filter(
    us_flights_distinct,
    origin %in% vertices$faa &
        dest %in% vertices$faa
)

c(us_flights_distinct$origin, us_flights_distinct$dest) %in% vertices$faa


g <- igraph::graph_from_data_frame(
    us_flights_distinct,
    directed = TRUE,
    vertices = vertices
)

gr <- tidygraph::as_tbl_graph(g)

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
