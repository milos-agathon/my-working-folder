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

not_north <- country_sf$NAME_1[!country_sf$NAME_1 %in% north]

center <- c(
    "Abruzzo", "Lazio",
    "Marche", "Toscana",
    "Umbria"
)

not_center <- country_sf$NAME_1[!country_sf$NAME_1 %in% center]

south <- c(
    "Apulia", "Basilicata",
    "Calabria", "Campania",
    "Molise", "Sardegna",
    "Sicily"
)
not_south <- country_sf$NAME_1[!country_sf$NAME_1 %in% south]

# 3. DEM
#-------

elevation_raster <- elevatr::get_elev_raster(
    locations = country_sf,
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

country <- country_sf |>
    sf::st_transform(crs = crs_lambert) |>
    dplyr::mutate(
        NAME_1 = as.factor(NAME_1)
    )

north_sf <- country |>
    dplyr::filter(
        NAME_1 %in% north
    )

center_sf <- country |>
    dplyr::filter(
        NAME_1 %in% center
    )

south_sf <- country |>
    dplyr::filter(
        NAME_1 %in% south
    )

# 4. PALETTE
#----------

# north
pal_north <- c(
    rep("#114A60", length(
        north
    )),
    rep("grey60", length(
        not_north
    ))
)

region_names <- c(
    north,
    not_north
)

names(pal_north) <- unique(
    region_names
)

# center
pal_center <- c(
    rep("#2596be", length(
        center
    )),
    rep("grey60", length(
        not_center
    ))
)

region_names <- c(
    center,
    not_center
)

names(pal_center) <- unique(
    region_names
)

# south
pal_south <- c(
    rep("#84c0b0", length(
        south
    )),
    rep("grey60", length(
        not_south
    ))
)

region_names <- c(
    south,
    not_south
)

names(pal_south) <- unique(
    region_names
)

# total
regions_names <- c(
    north,
    center,
    south
)

pal_total <- c(
    rep("#114A60", length(
        north
    )),
    rep("#2596be", length(
        center
    )),
    rep("#84c0b0", length(
        south
    ))
)

names(pal_total) <- unique(
    region_names
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
    rayshader::add_overlay(
        rayshader::generate_polygon_overlay(
            geometry = south_sf, # change here
            extent = elevation_raster_lambert,
            heightmap = elevation_matrix,
            linecolor = "white",
            linewidth = 12,
            palette = pal_south, # change
            data_column_fill = "NAME_1"
        ),
        alphalayer = 1 # REDO
    ) |>
    rayshader::plot_3d(
        elevation_matrix,
        zscale = 25,
        solid = F,
        shadow = F, # REDO
        shadow_darkness = 1, # REDO
        background = "white",
        windowsize = c(
            w / 8, h / 8
        ),
        zoom = .7,
        phi = 89,
        theta = 0
    )

# REDO
rayshader::render_camera(
    zoom = .61,
    phi = 89,
    theta = 0
)

# 9. RENDER OBJECT
#-----------------

rayshader::render_highquality(
    filename = "south-3d.png",
    preview = T,
    light = T,
    environment_light = "limpopo_golf_course_4k.hdr",
    rotate_env = 0,
    intensity_env = .4,
    # ground_material = rayrender::diffuse(
    #     color = "grey10"
    # ),
    interactive = F,
    parallel = T,
    width = w,
    height = h
)

# ©2024 Milos Popovic (https://milospopovic.net)
# Data: ©World Wildlife Fund, Inc. (2006-2013) HydroSHEDS database http://www.hydrosheds.org

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
