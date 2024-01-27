#############################################
# Sex balance with R
# Milos Popovic 2024/01/09
#############################################
setwd("/Users/mpopovic3/Downloads/sex-ratio")

remotes::install_github("ropensci/osmdata")

libs <- c(
   "terra", "tidyverse", "osmdata"
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

# 1. CITY BOUNDARIES
#----------------------
place_name <- "Paris, France"

city_border <- osmdata::getbb(
    place_name = place_name,
    format_out = "sf_polygon",
    limit = 1,
    featuretype = "settlement"
) |>
    sf::st_set_crs(4326) 

plot(sf::st_geometry(city_border))

# 2. DATA
#--------

options(timeout = 999)

urls <- c(
    "https://data.humdata.org/dataset/da0f4294-57ea-473d-98b8-315f1135f793/resource/2cddeeda-3263-4c23-973d-9b931b9c3066/download/fra_men_geotiff.zip",
    "https://data.humdata.org/dataset/da0f4294-57ea-473d-98b8-315f1135f793/resource/fc23467b-63ca-4013-8100-98cb68fc9336/download/fra_women_geotiff.zip"
)

for (url in urls) {
    download.file(url,
        destfile = basename(url),
        mode = "wb"
    )
}

main_dir <- getwd()

zip_files <- list.files(
    path = main_dir,
    pattern = ".zip",
    full.names = T
)

lapply(
    zip_files,
    unzip
)

raster_files <- list.files(
    path = main_dir,
    pattern = ".tif$",
    full.names = T
)

raster_list <- lapply(
    raster_files,
    terra::rast
)

# 3 CROP DATA

city_rasters <- lapply(
    raster_list,
    function(x) {
        terra::crop(
            x,
            terra::vect(
                city_border
            ),
            snap = "in",
            mask = T
        )
    }
)

# 4. CALCULATE HUMAN SEX RATIO
#-----------------------------

sex_ratio <- (
    (100 * city_rasters[[1]]) / city_rasters[[2]]
)

terra::plot(sex_ratio)
plot(sf::st_geometry(country_sf), add = T)

crs_europe <- "EPSG:3035"

terra::plot(sex_ratio_agg)

# 3. GET STREET LAYER

city_bbox <- 
    sf::st_bbox(city_border) |>
    sf::st_as_sfc(crs = 4326)

layer <- maptiles::get_tiles(
    city_bbox,
    provider = "CartoDB.Positron",
    zoom = 12,
    crop = T,
    project = F,
    forceDownload = T
)

sex_ratio_reproj <- terra::project(
    sex_ratio, terra::crs(layer)
)

terra::plot(layer, smooth = T)

# 5. RASTER TO DATAFRAME
#-----------------------
sex_ratio_df <- sex_ratio_reproj |>
    as.data.frame(xy = T)

names(sex_ratio_df)
names(sex_ratio_df)[3] <- "ratio"

# 6. BREAKS
#----------
summary(sex_ratio_df$ratio)
min_val <- min(sex_ratio_df$ratio)
max_val <- max(sex_ratio_df$ratio)
limits <- c(min_val, max_val)

breaks <- classInt::classIntervals(
    var = sex_ratio_df$ratio,
    n = 6,
    style = "equal"
)$brks

p2 <-
    ggplot(data = sex_ratio_df) +
    tidyterra::geom_spatraster_rgb(
        data = layer
    ) +
    geom_sf(
        data = city_border,
        fill = NA,
        color = "white",
        size = 1
    ) +
    geom_tile(
        aes(
            x = x, y = y,
            fill = ratio
        ),
        na.rm = T
    ) +
    scale_fill_gradient2(
        name = "",
        low = "#2686A0",
        mid = "#EDEAC2",
        high = "#A36B2B",
        midpoint = 100,
        limits = limits,
        breaks = round(breaks, 0),
        na.value = "white",
        guide = "colourbar"
    ) +
    # coord_sf(crs = crs_europe) +
    guides(
        fill = guide_colorbar(
            direction = "vertical",
            title.position = "top",
            label.position = "right",
            title.hjust = .5,
            title.vjust = 4,
            label.hjust = 0,
            nrow = 1,
            drop = F
        )
    ) +
    theme_void() +
    theme(
        legend.position = c(.1, .85),
        legend.title = element_text(
            size = 14, color = "grey10"
        ),
        legend.text = element_text(
            size = 12, color = "grey10"
        ),
        plot.margin = unit(
            c(
                t = 0, r = 0,
                b = 0, l = 0
            ), "lines"
        )
    )
# print(p)

w <- ncol(sex_ratio_reproj)
h <- nrow(sex_ratio_reproj)

ggsave(
    "paris-sex-ratio-layer.png",
    p2,
    width = w * 5,
    height = h * 5,
    units = "px",
    bg = "white"
)
