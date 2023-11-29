setwd("/Users/mpopovic3/Downloads/kontur")

# define libraries we need
libs <- c(
    "R.utils", "sf",
    "scales", "deckgl"
)

# install missing libraries
installed_libraries <- libs %in% rownames(installed.packages())
if (any(installed_libraries == F)) {
    install.packages(libs[!installed_libraries])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

options(timeout = 300)
url <- "https://geodata-eu-central-1-kontur-public.s3.amazonaws.com/kontur_datasets/kontur_population_BE_20231101.gpkg.gz"
filename <- basename(url)

download.file(
    url = url,
    destfile = basename(url),
    mode = "wb"
)

R.utils::gunzip(basename(url), remove = F)

pop_df <- sf::st_read(
    dsn = gsub(
        pattern = ".gz",
        replacement = "",
        x = filename
    )
) |>
    sf::st_transform(crs = "EPSG:4326")

plot(sf::st_geometry(pop_df))

pal <- scales::col_quantile(
    "viridis", pop_df$population,
    n = 6
)

pop_df$color <- pal(
    pop_df$population
)

deckgl::does_it_work()

properties <- list(
    pickable = T,
    stroked = F,
    filled = T,
    extruded = T,
    wireframe = T,
    fp64 = T,
    elevationScale = 1,
    lineWidthMinPixels = 1,
    getFillColor = ~color,
    getElevation = ~population,
    getLineColor = ~color,
    getLineWidth = 1,
    getPolygon = deckgl::JS("d => d.geom.coordinates"),
    tooltip = "Population: {{population}}",
    opacity = .8
)

deckgl::deckgl(
    latitude = 52.252365,
    longitude = 5.3350558,
    zoom = 11, pitch = 45
) |>
    deckgl::add_polygon_layer(
        data = pop_df,
        properties = properties
    ) |>
    deckgl::add_legend_pal(pal, title = "Population") |>
    deckgl::add_basemap(deckgl::use_carto_style())