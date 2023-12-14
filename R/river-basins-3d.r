###################################################
#                3D river basin maps with R
#                 Milos Popovic
#                 2023/11/21
###################################################

setwd("D:/hydroshed/poland")

libs <- c(
    "tidyverse", "sf", "giscoR",
    "elevatr", "terra", "rayshader"
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

# 1. COUNTRY SF
#---------------

get_bbox <- function() {
    bb <- sf::st_sfc(
        sf::st_polygon(list(
            cbind(
                c(-5.207520, 9.656982, 9.656982, -5.207520, -5.207520),
                c(41.335576, 41.335576, 51.179343, 51.179343, 41.335576)
            )
        )),
        crs = "EPSG:4326"
    )

    return(bb)
}

bb <- get_bbox()
plot(sf::st_geometry(bb))

country_sf <- giscoR::gisco_get_countries(
    resolution = "1",
    country = "IT"
) |>
    sf::st_intersection(bb)

plot(sf::st_geometry(country_sf))

country_bbox <- sf::st_bbox(
    country_sf
)

# 2. GET RIVERS
#--------------

url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_eu_shp.zip"

download.file(
    url = url,
    destfile = basename(url),
    mode = "wb"
)

unzip(basename(url))
filename <- list.files(
    path = "HydroRIVERS_v10_eu_shp",
    pattern = ".shp",
    full.names = T
)

print(country_bbox)

bbox_wkt <- "POLYGON((
    6.627429 35.49304,
    6.627429 47.09175,
    18.51931 47.09175,
    18.51931 35.49304,
    6.627429 35.49304
))
"

country_rivers <- sf::st_read(
    filename,
    wkt_filter = bbox_wkt
)

plot(sf::st_geometry(country_sf), col = "red")
plot(sf::st_geometry(country_rivers), add = T)

# 3. GET BASINS
#---------------

url <- "https://data.hydrosheds.org/file/HydroBASINS/standard/hybas_eu_lev04_v1c.zip"

download.file(
    url = url,
    destfile = basename(url),
    mode = "wb"
)

list.files()

unzip(basename(url))

country_basin <- sf::st_read(
    "hybas_eu_lev04_v1c.shp"
) |>
    sf::st_intersection(country_sf) |>
    dplyr::select(HYBAS_ID)

# 4. CLIP RIVERS TO BASINS
#-------------------------

country_river_basin <- sf::st_intersection(
    country_rivers,
    country_basin
)

unique(country_river_basin$HYBAS_ID)

# 5. PALETTE
#-----------

palette_basin <- hcl.colors(
    n = 10,
    palette = "Dark 3"
) |>
    sample()

palette_rivers <- colorspace::darken(
    palette_basin, .7
)

names(palette_rivers) <- unique(
    country_river_basin$HYBAS_ID
)

pal_basin <- as.data.frame(
    palette_rivers
) |>
    tibble::rownames_to_column(
        "HYBAS_ID"
    ) |>
    dplyr::mutate(
        HYBAS_ID = as.numeric(HYBAS_ID)
    )

country_river_basin_pal <- country_river_basin |>
    dplyr::left_join(
        pal_basin,
        by = "HYBAS_ID"
    )

country_basin_pal <- country_basin |>
    dplyr::filter(
        HYBAS_ID %in% unique(country_river_basin$HYBAS_ID)
    ) |>
    sf::st_transform(
        crs = crs_lambert
    ) |>
    dplyr::mutate(
        HYBAS_ID = as.factor(HYBAS_ID)
    )

names(palette_basin) <- unique(
    country_basin_pal$HYBAS_ID
)

# 6. WIDTH
#----------

unique(country_river_basin_pal$ORD_FLOW)

country_river_width <- country_river_basin_pal |>
    dplyr::mutate(
        width = as.numeric(
            ORD_FLOW
        ),
        width = dplyr::case_when(
            width == 3 ~ 14,
            width == 4 ~ 12,
            width == 5 ~ 10,
            width == 6 ~ 8,
            width == 7 ~ 6,
            width == 8 ~ 4,
            TRUE ~ 0
        )
    ) |>
    sf::st_as_sf() |>
    sf::st_transform(crs = crs_lambert)

# 7. DEM
#-------

elevation_raster <- elevatr::get_elev_raster(
    locations = country_sf,
    z = 8, clip = "locations"
) |>
    terra::rast() |>
    terra::project(crs_lambert)

elevation_matrix <- rayshader::raster_to_matrix(
    elevation_raster
)

# 8. RENDER SCENE
#----------------

h <- nrow(elevation_raster)
w <- ncol(elevation_raster)

elevation_matrix |>
    rayshader::height_shade(
        texture = colorRampPalette(
            c(
                "grey90",
                "grey60"
            )
        )(256)
    ) |>
    rayshader::add_overlay(
        rayshader::generate_polygon_overlay(
            geometry = country_basin_pal,
            extent = elevation_raster,
            heightmap = elevation_matrix,
            linecolor = palette_basin,
            palette = palette_basin,
            data_column_fill = "HYBAS_ID"
        ),
        alphalayer = .6 # REDO
    ) |>
    rayshader::add_overlay( # : missing
        rayshader::generate_line_overlay(
            geometry = country_river_width,
            extent = elevation_raster,
            heightmap = elevation_matrix,
            color = country_river_width$palette_rivers,
            linewidth = country_river_width$width,
            data_column_width = "width"
        ),
        alphalayer = 1
    ) |>
    rayshader::plot_3d(
        elevation_matrix,
        zscale = 17,
        solid = F,
        shadow = F, # REDO
        shadow_darkness = 1, # REDO
        background = "white",
        windowsize = c(
            w / 8, h / 8
        ),
        zoom = .7,
        phi = 87,
        theta = 0
    )

# REDO
# rayshader::render_camera(
#     phi = 87,
#     theta = 0,
#     zoom = .7
# )


# 9. RENDER OBJECT
#-----------------


u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/limpopo_golf_course_4k.hdr"

download.file(
    url = u,
    destfile = basename(u),
    mode = "wb"
)

rayshader::render_highquality(
    filename = "italy-3d-river-basins.png",
    preview = T,
    light = F,
    environment_light = "limpopo_golf_course_4k.hdr",
    rotate_env = 0,
    intensity_env = .85,
    ground_material = rayrender::diffuse(
        color = "grey10"
    ),
    interactive = F,
    parallel = T,
    width = w,
    height = h
)

# ©2023 Milos Popovic (https://milospopovic.net)
# Data: ©World Wildlife Fund, Inc. (2006-2013) HydroSHEDS database http://www.hydrosheds.org
