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

country_sf <- giscoR::gisco_get_countries(
    country = "BR",
    resolution = "1"
)

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
    path = "HydroRIVERS_v10_sa_shp",
    pattern = ".shp",
    full.names = T
)

print(country_bbox)

bbox_wkt <- "POLYGON((
    -73.984396 -33.750400,
    -73.984396  5.264468,
    -28.848437  5.264468,
    -28.848437 -33.750400,
    -73.984396 -33.750400
))
"

country_rivers <- sf::st_read(
    filename,
    wkt_filter = bbox_wkt
)

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
    "hybas_sa_lev03_v1c.shp"
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
    n = 12,
    palette = "Dark 3"
) |>
    sample()

pal_basin <- colorRampPalette(
    palette_basin
)(14)

palette_rivers <- colorspace::darken(
    pal_basin, .6
)

names(palette_rivers) <- unique(
    country_river_basin$HYBAS_ID
)

pal_basin_df <- as.data.frame(
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
        pal_basin_df,
        by = "HYBAS_ID"
    )

crs_lambert <-
    "+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"

country_basin_pal <- country_basin |>
    dplyr::filter(
        HYBAS_ID %in% unique(
            country_river_basin$HYBAS_ID
        )
    ) |>
    sf::st_transform(
        crs = crs_lambert
    ) |>
    dplyr::mutate(
        HYBAS_ID = as.factor(HYBAS_ID)
    )

names(pal_basin) <- unique(
    country_basin_pal$HYBAS_ID
)

# 6. WIDTH
#----------

sort(unique(country_river_basin_pal$ORD_FLOW))

country_river_width <- country_river_basin_pal |>
    dplyr::mutate(
        width = as.numeric(
            ORD_FLOW
        ),
        width = dplyr::case_when(
            width == 1 ~ 16,
            width == 2 ~ 14,
            width == 3 ~ 12,
            width == 4 ~ 10,
            width == 5 ~ 8,
            width == 6 ~ 6,
            width == 7 ~ 4,
            width == 8 ~ 3,
            width == 9 ~ 1,
            TRUE ~ 0
        )
    ) |>
    sf::st_as_sf() |>
    sf::st_transform(crs = crs_lambert)

# 7. DEM
#-------

elevation_raster <- elevatr::get_elev_raster(
    locations = country_sf,
    z = 6, clip = "locations"
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
            linecolor = pal_basin,
            palette = pal_basin,
            data_column_fill = "HYBAS_ID"
        ),
        alphalayer = .75 # REDO
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
        zscale = 12,
        solid = F,
        shadow = F, # REDO
        shadow_darkness = 1, # REDO
        background = "white",
        windowsize = c(
            w / 5, h / 5
        ),
        zoom = .7,
        phi = 85,
        theta = 0
    )

# REDO
rayshader::render_camera(
    zoom = .58
)


# 9. RENDER OBJECT
#-----------------


u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/limpopo_golf_course_4k.hdr"

download.file(
    url = u,
    destfile = basename(u),
    mode = "wb"
)

rayshader::render_highquality(
    filename = "brazil-3d-river-basins2.png",
    preview = T,
    light = F,
    environment_light = "limpopo_golf_course_4k.hdr",
    rotate_env = 0,
    intensity_env = 1,
    ground_material = rayrender::diffuse(
        color = "grey10"
    ),
    interactive = F,
    parallel = T,
    width = w * 1.25,
    height = h * 1.25
)

# ©2023 Milos Popovic (https://milospopovic.net)
# Data: ©World Wildlife Fund, Inc. (2006-2013) HydroSHEDS database http://www.hydrosheds.org

sessionInfo()
unzip("rezultati_izbora_2023_csv.zip")