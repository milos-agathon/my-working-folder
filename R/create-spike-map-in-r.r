#######################################################
#                 Making crisp spike maps with R
#                 Milos Popovic
#                 2023/03/12
########################################################
# install rayshader & rayrender from the source
# devtools::install_github("tylermorganwall/rayshader")
# devtools::install_github("tylermorganwall/rayrender")

setwd("C:/Users/milos/Downloads")
# libraries we need
libs <- c(
    "tidyverse", "R.utils",
    "httr", "sf", "stars",
    "rayshader", "car",
    "rayimage"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
    install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

### 1. DOWNLOAD & UNZIP DATA
### ------------------------
url <-
    "https://geodata-eu-central-1-kontur-public.s3.amazonaws.com/kontur_datasets/kontur_population_BE_20220630.gpkg.gz"

file_name <- "belgium-population.gpkg.gz"


get_population_data <- function() {
    res <- httr::GET(
        url,
        write_disk(file_name),
        progress()
    )

    R.utils::gunzip(file_name, remove = F)
}

get_population_data()

### 2. LOAD DATA
### -------------
load_file_name <- gsub(".gz", "", file_name)
# Lambert projection
crs_longlat <- "+proj=longlat +datum=WGS84 +no_defs"
crs_lambert <-
    "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

get_population_data <- function() {
    pop_df <- sf::st_read(
        load_file_name
    ) |>
        sf::st_transform(crs = 4326)
}

pop_sf <- get_population_data()

### 3. SHP TO RASTER
### ----------------

b <- sf::st_bbox(pop_sf)

get_raster_size <- function() {
    height <- sf::st_distance(
        sf::st_point(c(b[["xmin"]], b[["ymin"]])),
        sf::st_point(c(b[["xmin"]], b[["ymax"]]))
    )
    width <- sf::st_distance(
        sf::st_point(c(b[["xmin"]], b[["ymin"]])),
        sf::st_point(c(b[["xmax"]], b[["ymin"]]))
    )

    if (height > width) {
        height_ratio <- 1
        width_ratio <- width / height
    } else {
        width_ratio <- 1
        height_ratio <- height / width
    }

    return(list(width_ratio, height_ratio))
}
width_ratio <- get_raster_size()[[1]]
height_ratio <- get_raster_size()[[2]]

size <- 5000
width <- round((size * width_ratio), 0)
height <- round((size * height_ratio), 0)

get_population_raster <- function() {
    pop_rast <- stars::st_rasterize(
        pop_sf |>
            dplyr::select(population, geom),
        nx = width, ny = height
    )

    return(pop_rast)
}

pop_rast <- get_population_raster()
# plot(pop_rast)

pop_raster <- pop_rast |>
    as("Raster")

pop_mat <- rayshader::raster_to_matrix(pop_raster)

cols <- c(
    "#140b34", "#84206b",
    "#e55c30", "#f6d746"
)


texture <- grDevices::colorRampPalette(
    cols, bias = 2)(512)

h <- nrow(pop_raster)
w <- ncol(pop_raster)

pop_mat |>
    rayshader::height_shade(
        texture = texture
    ) |>
    rayshader::add_shadow(
        rayshader::lamb_shade(
            pop_mat,
            zscale = 50,
            sunaltitude = 90,
            sunangle = 225
        ),
        max_darken = .25
    ) |>
    rayshader::add_shadow(
        rayshader::ambient_shade(
            pop_mat
        ),
        max_darken = .25
    ) |>
    rayshader::add_shadow(
        rayshader::texture_shade(
            pop_mat,
            detail = .95,
            contrast = 80,
            brightness = 100
        ),
        max_darken = .1
    ) |>
    rayshader::plot_3d(
        pop_mat,
        zscale = 5,
        solid = F,
        shadowdepth = 0,
        shadow = T,
        shadow_darkness = 1,
        background = "white", # cols[1]
        sunangle = 225, # 225
        windowsize = c(w / 10, h / 10),
        zoom = .4,
        phi = 30,
        theta = -30
    )

rayshader::render_highquality(
    filename = "belgium_population_2022.png",
    preview = T,
    light = T,
    lightdirection = 225, # 210
    lightintensity = 1750, # 2000
    lightaltitude = 90, # 90
    interactive = F,
    parallel = T,
    width = w, height = h
)