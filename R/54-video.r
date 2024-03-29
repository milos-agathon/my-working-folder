# 1. LIBRARIES

install.packages("httr")
# install.packages("devtools")
devtools::install_github("eliocamp/ggnewscale")

pacman::p_load(
    rstac,
    sf,
    terra,
    arcgislayers,
    tidyverse,
    terrainr
)

# 2. CITY BOUNDARIES

url1 <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/World_Urban_Areas/FeatureServer/0"

city_data <- arcgislayers::arc_open(
    url1
)

city_sf <- arcgislayers::arc_select(
    city_data,
    fields = "NAME",
    where = "NAME = 'São Paulo'",
    crs = 4326
)

plot(
    sf::st_geometry(
        city_sf
    )
)

city_bbox <- sf::st_bbox(city_sf)

main_dir <- getwd()

# 3. ESA Land cover - query and collections
ms_query <- rstac::stac(
    "https://planetarycomputer.microsoft.com/api/stac/v1"
)

ms_query

ms_collections <- ms_query |>
    rstac::collections() |>
    rstac::get_request()

print(ms_collections, n = 123)

collections <- "esa-worldcover"

ms_esa_query <- rstac::stac_search(
    q = ms_query,
    collections = collections,
    # ids = "India_123121303_2023-04-25",
    datetime = "2021-01-01T00:00:00Z/2021-12-31T23:59:59Z",
    bbox = city_bbox,
    limit = 100
) |>
    rstac::get_request()

# 4. ESA Land cover - data download

ms_query_signin <- rstac::items_sign(
    ms_esa_query,
    rstac::sign_planetary_computer()
)

ms_query_signin

rstac::assets_download(
    items = ms_query_signin,
    asset_names = "map",
    output_dir = main_dir,
    overwrite = TRUE
)

# 5. ESA Land cover - load data

version <- c("v100", "v200")
year <- c("2020", "2021")
asset_name <- "map"


data_dirs <- paste0(
    main_dir, "/",
    collections, "/",
    version, "/",
    year, "/",
    asset_name
)

raster_files <- list.files(
    data_dirs,
    full.names = TRUE
)

land_cover_rasters <- lapply(
    raster_files,
    terra::rast
)

city_land_cover_rasters <- lapply(
    land_cover_rasters,
    function(x) {
        terra::crop(
            x,
            terra::vect(city_sf),
            snap = "in"
        )
    }
)

terra::plot(city_land_cover_rasters[[1]])

raster_color_table <- do.call(
    data.frame,
    terra::coltab(city_land_cover_rasters[[1]])
)

head(raster_color_table)

hex_code <- ggtern::rgb2hex(
    r = raster_color_table[, 2],
    g = raster_color_table[, 3],
    b = raster_color_table[, 4]
)

# 7 ASSIGN COLORS TO RASTER
values <- sort(unique(terra::values(city_land_cover_rasters[[1]])))
new_values <- Map(`+`, values, 1) |> unlist()
cols <- hex_code[new_values]

from <- values
to <- t(col2rgb(cols))

land_cover_new <- terra::subst(
    city_land_cover_rasters[[1]],
    from = from,
    to = to,
    names = cols
)

dem <- elevatr::get_elev_raster(
    locations = city_sf,
    z = 11,
    clip = "bbox"
) |>
    terra::rast()

slope <- terra::terrain(dem, "slope", unit = "radians", neighbors = 8)
aspect <- terra::terrain(dem, "aspect", unit = "radians", neighbors = 8)
hillshade <- terra::shade(
    slope = slope,
    aspect = aspect,
    angle = 45,
    direction = 315,
    normalize = TRUE
)

hillshade_df <- as.data.frame(
            hillshade,
            xy = TRUE
        )

lc_resampled <- terra::resample(
    x = land_cover_new,
    y = dem,
    method = "near"
)

lc_df <- as.data.frame(
            lc_resampled,
            xy = TRUE
        )

names(lc_df)[3] <- "land_cover"

class(lc_resampled)
terra::plotRGB(lc_resampled)

ggplot() +
    # geom_raster(
    #     data = hillshade_df,
    #     aes(x, y, fill = hillshade),
    #     show.legend = FALSE
    # ) +
    # scale_fill_distiller(palette = "Greys") +
    geom_spatial_rgb(
    data = lc_resampled,
    aes(
      x = x,
      y = y,
      r = red,
      g = green,
      b = blue
    )
  ) +
    theme_void()






city_land_cover_forest <- lapply(
    land_cover_rasters,
    function(x) {
        terra::ifel(
            x == 10,
            1,
            0
        )
    }
)

terra::plot(city_land_cover_forest[[2]])

city_forest_cover_change <-
    city_land_cover_forest[[2]] - city_land_cover_forest[[1]]







terra::plot(forest_cover_change_resampled)
