
setwd("/Users/mpopovic3/Downloads/")

# 1. PACKAGES

libs <- c(
    "terra",
    "geodata",
    "sf",
    "tidyverse",
    "elevatr",
    "rayshader",
    "ceramic"
)

installed_libraries <- libs %in% rownames(
    installed.packages()
)

if (any(installed_libraries == F)) {
    install.packages(
        libs[!installed_libraries]
    )
}

invisible(
    lapply(
        libs, library,
        character.only = T
    )
)

# 2. DATA

url <- "https://wri-dataportal-prod.s3.amazonaws.com/manual/global_power_plant_database_v_1_3.zip"

filename <- basename(url)

download.file(
    url = url,
    destfile = filename,
    mode = "wb"
)

unzip(filename)

power_plant_df <- read.csv(
    "global_power_plant_database.csv"
)

head(power_plant_df)


# 3. FILTER COUNTRY DATA

country_power_plant_df <- power_plant_df |>
    dplyr::filter(country == "NLD") |>
    dplyr::select(
        name, capacity_mw,
        primary_fuel,
        latitude, longitude
    )

summary(as.factor(country_power_plant_df$primary_fuel))


country_nuclear_sf <- country_power_plant_df |>
    # dplyr::filter(primary_fuel == "Solar") |>
    sf::st_as_sf(
        coords = c("longitude", "latitude"),
        crs = 4326
    )

plot(sf::st_geometry(country_nuclear_sf))

# 4. COUNTRY BORDERS

country_sf <- geodata::gadm(
    country = "NLD",
    level = 0,
    path = getwd()
) |>
    sf::st_as_sf()

plot(sf::st_geometry(country_sf))

# 5. DIGITAL ELEVATION MODEL

elev <- elevatr::get_elev_raster(
    locations = country_sf,
    z = 8, clip = "locations"
)

crs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

elev_lambert <- elev |>
    terra::rast() |>
    terra::project(crs)

elmat <- elev_lambert |>
    rayshader::raster_to_matrix()

elmat[elmat < 0] <- 0
elmat[is.na(elmat)] <- min(elmat, na.rm = T)

topo <- ceramic::cc_location(
    country_sf,
    dimension = 2000
    )
topo_lambert <- terra::project(topo, crs)
terra::plotRGB(topo_lambert)

img_name <- "topo.png"
terra::writeRaster(
    x = topo_lambert,
    filename = img_name,
    overwrite = T,
    NAflag = 255
)

topo_img <- png::readPNG(img_name)

# 6. only power plants within borders

country_points <- sf::st_intersection(
    country_nuclear_sf,
    country_sf
    ) |>
    sf::st_transform(
        crs = crs
    )

plot(sf::st_geometry(country_points))

# 7. RENDER SCENE
#----------------

h <- nrow(elmat)
w <- ncol(elmat)

elmat |>
    rayshader::height_shade(
        texture = colorRampPalette(
            c(
                "grey90",
                "grey60"
            )
        )(256)
    ) |>
    # rayshader::add_overlay(
    #     overlay = topo_img,
    #     alphalayer = 1
    # ) |>
    rayshader::plot_3d(
        elmat,
        zscale = 5,
        solid = F,
        shadow = T,
        shadow_darkness = 1,
        background = "white",
        windowsize = c(
            600, 600
        ),
        zoom = .65,
        phi = 85,
        theta = 0
    )

rayshader::render_camera(
    phi = 87,
    zoom = .6
)

# 10. RENDER POINTS
#-----------------

coords <- sf::st_coordinates(country_points)
long <- coords[, "X"]
lat <- coords[, "Y"]

rayshader::render_points(
    lat = lat,
    long = long,
    extent = elev_lambert,
    heightmap = elmat,
    zscale = 1,
    size = 5,
    color = "#F59F07"
)

imgname <- "3d_power_plant_netherlands.png"

rayshader::render_highquality(
    # filename = imgname,
    preview = T,
    light = F,
    point_radius = 7,
    point_material = rayrender::light,
    point_material_args = list(
        color = "#F59F07",
        #"#20F5F5",
        gradient_color = "#F5D014",
        intensity = 60
    ),
    clamp_value = 2,
    interactive = F,
    parallel = T,
    min_variance = 0,
    sample_method = "sobol"
)


# ©2024 Milos Popovic (https://milospopovic.net)
# Data: Sentinel-2 10m Land Use/Land Cover - Esri, Impact Observatory, and Microsoft
