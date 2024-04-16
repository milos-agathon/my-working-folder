# STEP 1
pacman::p_load(rgee, reticulate)

# STEP 3
reticulate::py_discover_config()

# STEP 4
rgee::ee_install_set_pyenv(
    py_path = "C:/Users/milos/AppData/Local/r-miniconda/envs/r-reticulate/python.exe", # PLEASE SET YOUR OWN PATH
    py_env = "rgee"
)

# STEP 5
rgee::ee_check()
rgee::ee_install_upgrade()

rgee::ee_clean_user_credentials(
    user = "milos.agathon@gmail" # PLEASE SET YOUR OWN CREDENTIALS
)

# START HERE
# initialize Earth Engine
rgee::ee_Initialize(
    user = "milos.agathon@gmail" # PLEASE SET YOUR OWN CREDENTIALS
)

pacman::p_load(sf, giscoR, elevatr, rayshader, terra)
crs_lambert <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_frfs"

country_sf <- geodata::gadm(
    country = "ITA",
    level = 2,
    path = getwd()
) |>
    sf::st_as_sf()

sort(unique(country_sf$NAME_2))

region_sf <- subset(country_sf, NAME_2 == "Torino")

plot(sf::st_geometry(region_sf))

region_bbox <- sf::st_bbox(
    region_sf
)

region_bounds <- ee$Geometry$Rectangle(
    c(
        west = region_bbox[["xmin"]],
        south = region_bbox[["ymin"]],
        east = region_bbox[["xmax"]],
        north = region_bbox[["ymax"]]
    ),
    geodetic = TRUE,
    proj = "EPSG:4326"
)


image <- ee$ImageCollection(
    "users/gena/global-hand/hand-100"
)$
    filterBounds(
    region_bounds
)

rgee::ee_print(image)


region_image <- image$select("b1")$toBands()
rgee::ee_print(region_image)
region_image$bandNames()$getInfo()


rivers_image <- region_image$select(
    c(
        "HAND_100_cell_01358_b1",
        "HAND_100_cell_01359_b1"
    )
)

rivers_country <- rgee::ee_as_rast(
    image = rivers_image,
    region = region_bounds,
    maxPixels = 2e9
)

terra::plot(rivers_country)

rivers_mosaic <- terra::rast(rivers_country)
x <- terra::concats(rivers_country, rivers_mosaic)
terra::plot(rivers_mosaic)

river_matrix <- rayshader::raster_to_matrix(rivers_country)

height <- nrow(river_matrix)
width <- ncol(river_matrix)

texture <- colorRampPalette(
    hcl.colors(
        n = 5,
        palette = "Geyser"
    )
)(512)

river_matrix |>
    rayshader::height_shade(
        texture = texture
    ) |>
    rayshader::plot_3d(
        river_matrix,
        zscale = 10,
        solid = FALSE,
        shadow = TRUE,
        shadow_darkness = 1,
        background = "white",
        windowsize = c(
            600, 600
        ),
        zoom = .515,
        phi = 87,
        theta = 0
    )

rayshader::render_camera(
    phi = 89,
    zoom = .65,
    theta = 0
)









# Get earthquake data
rivers_data <- ee$FeatureCollection(
    "projects/sat-io/open-datasets/GloRiC/GloRiC_v10"
)$
    filterBounds(
    region_bounds
)

rgee::ee_print(rivers_data)

rivers_italy <- rgee::ee_as_sf(
    rivers_data,
    maxFeatures = 15000
) |>
    sf::st_transform(
        crs_lambert
    )

plot(sf::st_geometry(rivers_italy))


# Get elevation data

elev <- elevatr::get_elev_raster(
    locations = region_sf,
    z = 8, clip = "locations"
)

elev_lambert <- elev |>
    terra::rast() |>
    terra::project(
        crs_lambert
    )

elmat <- rayshader::raster_to_matrix(elev_lambert)

# Render scene

h <- nrow(elev_lambert)
w <- ncol(elev_lambert)

elmat |>
    rayshader::height_shade(
        texture = colorRampPalette(
            c(
                "grey90",
                "grey60",
                "grey40"
            )
        )(512)
    ) |>
    rayshader::add_overlay(
        rayshader::generate_point_overlay(
            earthquake_region,
            color = "#7b00ff",
            size = 5,
            extent = elev_lambert,
            heightmap = elmat
        ),
        alphalayer = .85
    ) |>
    rayshader::plot_3d(
        elmat,
        zscale = 17,
        solid = F,
        shadow = T,
        shadow_darkness = 1,
        background = "white",
        windowsize = c(
            w / 8, h / 8
        ),
        zoom = .515,
        phi = 87,
        theta = 0
    )

rayshader::render_camera(
    phi = 89,
    zoom = .55,
    theta = 0
)

# Render object

rayshader::render_highquality(
    filename = "balkan-earthquakes.png",
    preview = T,
    light = F,
    environment_light = "D:/forest2019/air_museum_playground_4k.hdr",
    intensity_env = .7,
    rotate_env = 90,
    interactive = F,
    parallel = T,
    width = w,
    height = h
)

# ©2023 Milos Popovic (https://milospopovic.net)
# Data: U.S. Geological Survey (USGS). 2023. Earthquake Hazards Program (EHP)
