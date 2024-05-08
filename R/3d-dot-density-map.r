#############################################
# 3D forest height with R
# Milos Popovic 2023/07/18
#############################################
setwd("/Users/mpopovic3/Downloads/")

# 1. PACKAGES
#------------

pacman::p_load(
    arcgis,
    geodata,
    tidyverse,
    elevatr,
    terra,
    sf, rayshader,
    dots
)

url <- "https://services1.arcgis.com/ZGrptGlLV2IILABw/arcgis/rest/services/Pop_Admin1/FeatureServer/0"

data <- arcgislayers::arc_open(
    url
)

crs <- "+proj=tmerc +lat_0=0 +lon_0=6 +k=1 +x_0=2500000 +y_0=0 +ellps=bessel +units=m +no_defs +type=crs"

admin1_population <- arcgislayers::arc_select(
    data,
    fields = c("HASC_1", "ISO2", "Population"),
    where = "ISO2 = 'DE'"
) |>
    sf::st_drop_geometry()

country_admin1_sf <- geodata::gadm(
    country = "DEU",
    level = 1,
    path = getwd()
) |>
    sf::st_as_sf()

country_admin1_population <- dplyr::left_join(
    country_admin1_sf,
    admin1_population,
    by = "HASC_1"
) |> sf::st_transform(crs = crs)


sf::sf_use_s2(FALSE)

population_dots <- dots::dots_points(
    shp = country_admin1_population,
    col = "Population",
    engine = engine_sf_random,
    divisor = 100000
)

ggplot() +
    geom_sf(
        data = country_admin1_population,
        aes(
            fill = Population
        ),
        color = "white",
        size = .5
    ) +
    geom_sf(
        data = population_dots,
        color = "red"
    ) +
    theme_void()

dem <- elevatr::get_elev_raster(
    locations = country_admin1_sf,
    z = 7,
    clip = "bbox",
    expand = 3
)

dem_reproj <- dem |>
    terra::rast() |>
    terra::project(crs)

terra::plot(dem_reproj)
plot(sf::st_geometry(country_admin1_population), add = TRUE)

dem_matrix <- dem_reproj |>
    rayshader::raster_to_matrix()

width <- nrow(dem_matrix)
height <- ncol(dem_matrix)

dem_matrix |>
    rayshader::height_shade(
        texture = colorRampPalette(
            "white"
        )(128)
    ) |>
    rayshader::add_overlay(
        rayshader::generate_polygon_overlay(
            geometry = country_admin1_population,
            palette = "#569884",
            linecolor = "#2FCD9E",
            linewidth = 10,
            extent = dem_reproj,
            heightmap = dem_matrix
        ),
        alphalayer = 1
    ) |>
    rayshader::plot_3d(
        dem_matrix,
        zscale = 10,
        solid = FALSE,
        shadow = TRUE,
        shadow_darkness = 1,
        background = "white",
        windowsize = c(height / 8, width / 8),
        zoom = .65,
        phi = 89,
        theta = 0
    )

rayshader::render_camera(
    zoom = .4
)

coords <- sf::st_coordinates(population_dots)
long <- coords[, "X"]
lat <- coords[, "Y"]
altitude <- terra::extract(
    x = dem_reproj,
    y = terra::vect(population_dots),
    fun = min,
    na.rm = TRUE
)

altitude <- altitude[, 2]

rayshader::render_points(
    lat = lat,
    long = long,
    altitude = altitude,
    extent = dem_reproj,
    heightmap = dem_matrix,
    zscale = 5,
    size = 5,
    color = "#EDE237"
)

rayshader::render_camera(
    zoom = .325
)

# 7. RENDER OBJECT
#-----------------

rayshader::render_highquality(
    filename = "3d-dot-density-de.png",
    preview = TRUE,
    light = TRUE,
    environment_light = "/Users/mpopovic3/Downloads/lebombo_4k.hdr",
    intensity_env = .8,
    point_radius = 5,
    point_material = rayrender::glossy,
    point_material_args = list(
        gloss = .4,
        reflectance = .1
    ),
    interactive = FALSE,
    parallel = TRUE,
    width = width / 2,
    height = height / 2
)
