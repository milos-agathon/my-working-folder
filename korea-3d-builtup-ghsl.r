setwd("D:/ghsl-builtup")
# libraries we need
libs <- c(
    "terra", "tidyverse", "sf",
    "rayshader"
)
# devtools::install_github("tylermorganwall/rayshader")
# install.packages("rayshader")
# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
    install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# Montserrat font
sysfonts::font_add_google("Montserrat", "Montserrat")
showtext::showtext_auto()

# define longlat projection
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crsLAEA <-
    "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"


pop <- terra::rast(
    "GHS_BUILT_S_E2020_GLOBE_R2023A_54009_100_V1_0.tif"
)

crs_mollweide <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0"

country_sf <- giscoR::gisco_get_countries(
    country = "KR",
    resolution = "1"
)

country <- country_sf |>
    sf::st_transform(crs_mollweide)

plot(sf::st_geometry(country))

korea_pop <- terra::crop(
    pop,
    terra::vect(country),
    snap = "in",
    mask = T
)

crs_korea <- "EPSG:5179"

korea_pop_reproj <- korea_pop |>
    terra::aggregate(fact = 2) |>
    terra::project(crs_korea)

terra::plot(
    korea_pop_reproj
)

pop_mat <- rayshader::raster_to_matrix(
    korea_pop_reproj
)

cols <- c(
    "#0000cd", "#862ae3",
    "#ea36eb", "#ffa08a", "#faea48"
)
texture <- colorRampPalette(
    cols, bias = 2,
    )(256)
pie(rep(1,256), col = texture)

pop_mat |>
    rayshader::height_shade(
        texture = texture
    ) |>
    rayshader::add_shadow(
        rayshader::lamb_shade(
            pop_mat,
            zscale = 50,
            sunaltitude = 90,
            sunangle = 315
        ),
        max_darken = .95
    ) |>
    rayshader::add_shadow(
        rayshader::ambient_shade(
            pop_mat
        ),
        max_darken = .95
    ) |>
    rayshader::add_shadow(
        rayshader::texture_shade(
            pop_mat,
            detail = 1,
            contrast = 40,
            brightness = 80
        ),
        max_darken = .95
    ) |>
    rayshader::plot_3d(
        pop_mat,
        zscale = 1,
        solid = F,
        shadow = T,
        shadowdepth = 0,
        shadow_darkness = 1,
        offset_edges = F,
        background = "white", # cols[1]
        sunangle = 315, # 225
        windowsize = c(800, 800),
        zoom = .75,
        phi = 80,
        theta = 0
    )

rayshader::render_camera(
    phi = 89,
    zoom = .45,
    theta = 0
)

# rayshader::render_highquality(
#     filename = "korea-urban-ghsl.png",
#     preview = T,
#     light = T,
#     lightdirection = c(
#         315, 310, 315, 310
#     ),
#     lightintensity = c(
#         1500, 1750, 700, 800
#     ),
#     lightaltitude = c(
#         5, 5, 90, 90
#     ),
#     interactive = F,
#     # ground_material = rayrender::microfacet(
#     #     roughness = .6
#     # ),
#     ground_material = rayrender::diffuse(
#         color = "white"
#     ),
#     parallel = T,
#     width = 8000, height = 8000
# )

rayshader::render_highquality(
    filename = "korea-urban-ghsl.png",
    preview = T,
    light = T,
    lightdirection = 315,
    lightintensity = 1500,
    lightaltitude = 90,
    interactive = F,
    # ground_material = rayrender::microfacet(
    #     roughness = .6
    # ),
    # ground_material = rayrender::diffuse(
    #     color = "white"
    # ),
    parallel = T,
    width = 8000, height = 8000
)