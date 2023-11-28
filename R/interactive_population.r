setwd("D:/kontur")

# define libraries we need
libs <- c(
    "tidyverse", "saqgetr",
    "terra", "sf",
    "rayshader", "geodata",
    "deckgl", "kriging"
)

# install missing libraries
installed_libraries <- libs %in% rownames(installed.packages())
if (any(installed_libraries == F)) {
    installed.packages(libs[!installed_libraries])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

options(timeout = 300)
url <- "https://geodata-eu-central-1-kontur-public.s3.amazonaws.com/kontur_datasets/kontur_population_NL_20231101.gpkg.gz"

download.file(
    url = url,
    destfile = basename(url),
    mode = "wb"
)
R.utils::gunzip(basename(url), remove = F)

pop_df <- sf::st_read(
    dsn = "D:/kontur/kontur_population_NL_20231101.gpkg"
) |>
    sf::st_transform(crs = "EPSG:4326")

props <- list(
    pickable = TRUE,
    stroked = TRUE,
    filled = TRUE,
    wireframe = TRUE,
    lineWidthMinPixels = 1,
    getPolygon = ~geom,
    getElevation = deckgl::JS("d => d.population"),
    getFillColor = deckgl::JS("d => [d.population / 60, 140, 0]"),
    getLineColor = ~color,
    getLineWidth = 1,
    tooltip = "Population: {{population}}"
)

pal <- scales::col_quantile(
    "viridis", pop_df$population,
    n = 6
)

pop_df$color <- pal(
    pop_df$population
)

deckgl::deckgl(
    latitude = 52.252365,
    longitude = 5.3350558,
    zoom = 11, pitch = 45
) |>
    deckgl::add_polygon_layer(
        data = pop_df,
        getPolygon = JS("d => d.geom.coordinates"),
        opacity = .1,
        stroked = FALSE,
        filled = TRUE,
        extruded = TRUE,
        wireframe = TRUE,
        fp64 = TRUE,
        getElevation = ~population,
        getFillColor = ~color,
        getLineColor = ~color
    ) |>
    deckgl::add_basemap(deckgl::use_carto_style())
