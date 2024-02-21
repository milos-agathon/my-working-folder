###################################################
#                3D river basin maps with R
#                 Milos Popovic
#                 2023/11/21
###################################################
setwd("/Users/mpopovic3/Downloads")

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

get_country_admin1 <- function() {
    main_path <- getwd()
    country_sf <- geodata::gadm(
        country = "ITA",
        level = 1,
        path = main_path
    ) |>
        sf::st_as_sf()

    return(country_sf)
}

country_sf <- get_country_admin1()
print(country_sf, n = 20)

north <- c(
    "Emilia-Romagna", "Friuli-Venezia Giulia",
    "Lombardia", "Liguria", "Trentino-Alto Adige",
    "Piemonte", "Valle d'Aosta", "Veneto"
)

north_sf <- country_sf |>
    dplyr::filter(NAME_1 %in% north)

plot(sf::st_geometry(north_sf))

# pdf(
#     file = "italy-regions.pdf",
#     width = 5.74 * 8,
#     height = 6.96 * 8,
# )
# plot(sf::st_geometry(country_sf))
# dev.off()

# map <- ggplot() +
#     geom_sf(
#         data = country_sf,
#         color = "black",
#         fill = NA,
#         size = 5
#     ) +
#     theme_void()

# print(map)

# ggsave(
#     filename = "italy_regions.png",
#     width = 5.74 * 1.25,
#     height = 6.96 * 1.25,
#     dpi = 600,
#     bg = NULL,
#     map
# )

# png(
#     file = "italy-regions.png",
#     width = 5.74 * 1000,
#     height = 6.96 * 1000,
# )
# plot(sf::st_geometry(country_sf))
# dev.off()

# 2. PALETTE
#-----------

palette_basin <- hcl.colors(
    n = 10,
    palette = "Dark 3"
) |>
    sample()

# pal_basin <- palette_basin

pal_basin <- colorRampPalette(
    palette_basin
)(20)

# 3. DEM
#-------

elevation_raster <- elevatr::get_elev_raster(
    locations = north_sf,
    z = 8, clip = "locations"
)

crs_lambert <-
    "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_frfs"

elevation_raster_lambert <- elevation_raster |>
    terra::rast() |>
    terra::project(crs_lambert)

elevation_matrix <- rayshader::raster_to_matrix(
    elevation_raster_lambert
)

nord <- north_sf |>
    sf::st_transform(crs = crs_lambert) |>
    dplyr::mutate(
        NAME_1 = as.factor(NAME_1)
    ) 
nord_line <- nord |>
    sf::st_cast("MULTILINESTRING")

pal <- rep("#114A60", length(
    unique(
        nord$NAME_1
    )
))

names(pal) <- unique(
    nord$NAME_1
)

# 4. RENDER SCENE
#----------------

h <- nrow(elevation_raster_lambert)
w <- ncol(elevation_raster_lambert)

elevation_matrix |>
    rayshader::height_shade(
        texture = colorRampPalette(
            c(
                "grey80",
                "grey40"
            )
        )(256)
    ) |>
    # rayshader::add_overlay(
    #     rayshader::generate_line_overlay(
    #         geometry = nord_line,
    #         extent = elevation_raster_lambert,
    #         heightmap = elevation_matrix,
    #         color = "black",
    #         linewidth = 10
    #     ),
    #     alphalayer = 1
    # ) |>
    rayshader::add_overlay(
        rayshader::generate_polygon_overlay(
            geometry = nord,
            extent = elevation_raster_lambert,
            heightmap = elevation_matrix,
            linecolor = "black",
            linewidth = 7,
            palette = pal,
            data_column_fill = "NAME_1"
        ),
        alphalayer = 1 # REDO
    ) |>
    rayshader::plot_3d(
        elevation_matrix,
        zscale = 15,
        solid = F,
        shadow = F, # REDO
        shadow_darkness = 1, # REDO
        background = "white",
        windowsize = c(
            800, 800
        ),
        zoom = .7,
        phi = 85,
        theta = 0
    )

# REDO
rayshader::render_camera(
    zoom = .9,
    phi = 85,
    theta = 0
)

# 9. RENDER OBJECT
#-----------------


# u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/limpopo_golf_course_4k.hdr"

# download.file(
#     url = u,
#     destfile = basename(u),
#     mode = "wb"
# )

rayshader::render_highquality(
    filename = "nord-3d-light.png",
    preview = T,
    light = T,
    environment_light = "limpopo_golf_course_4k.hdr",
    rotate_env = 0,
    intensity_env = 8,
    # ground_material = rayrender::diffuse(
    #     color = "grey10"
    # ),
    interactive = F,
    parallel = T,
    width = 4000,
    height = 4000
)

# ©2024 Milos Popovic (https://milospopovic.net)
# Data: ©World Wildlife Fund, Inc. (2006-2013) HydroSHEDS database http://www.hydrosheds.org
