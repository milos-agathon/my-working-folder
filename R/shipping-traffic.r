setwd("/Users/mpopovic3/Downloads/shipping-traffic")

# STEP 1: install and load packages
pacman::p_load(
    tidyverse, terra, sf,
    giscoR, ggnewscale
)

# STEP 2: download, unzip and load traffic data
# https://datacatalogfiles.worldbank.org/ddh-published/0037580/DR0045406/shipdensity_global.zip?versionId=2023-01-18T20:40:41.1120158Z
url <- "https://datacatalogfiles.worldbank.org/ddh-published/0037580/DR0045406/shipdensity_global.zip"
destfile <- basename(url)

options(timeout = 999)

download.file(
    url = url,
    destfile = destfile,
    mode = "wb"
)

decompress_file <- function(directory, file, .file_cache = FALSE) {
    if (.file_cache == TRUE) {
        print("decompression skipped")
    } else {

        # Set working directory for decompression
        # simplifies unzip directory location behavior
        wd <- getwd()
        setwd(directory)

        # Run decompression
        decompression <-
            system2("unzip",
                args = c(
                    "-o", # include override flag
                    file
                ),
                stdout = TRUE
            )

        # uncomment to delete archive once decompressed
        file.remove(file)

        # Reset working directory
        setwd(wd)
        rm(wd)

        # Test for success criteria
        # change the search depending on
        # your implementation
        if (grepl("Warning message", tail(decompression, 1))) {
            print(decompression)
        }
    }
}

decompress_file(
    directory = getwd(),
    file = destfile
)

rastfile <- gsub(".zip", ".tif", destfile)
global_traffic <- terra::rast(rastfile)


# STEP 3: Select the area of interest and crop
# -11.188846,47.889154,10.937619,55.462093

xmin <- -11.188846
xmax <- 10.937619
ymin <- 47.889154
ymax <- 55.462093

bounding_box <- sf::st_sfc(
    sf::st_polygon(list(cbind(
        c(xmin, xmax, xmax, xmin, xmin),
        c(ymin, ymin, ymax, ymax, ymin)
    ))),
    crs = 4326
)

shipping_traffic <- terra::crop(
    x = global_traffic,
    y = bounding_box,
    snap = "in"
)

shipping_traffic_clean <- terra::ifel(
    shipping_traffic == 0,
    NA,
    shipping_traffic
)

terra::plot(shipping_traffic_clean)

# STEP 4: Get nightlight data

u <- "https://eogdata.mines.edu/nighttime_light/annual/v22/2022/VNL_v22_npp-j01_2022_global_vcmslcfg_c202303062300.average_masked.dat.tif.gz"
filename <- basename(u)

download.file(
    url = u,
    destfile = filename,
    mode = "wb"
)

path_to_nightlight <- list.files(
    path = getwd(),
    pattern = filename,
    full.names = T
)

nightlight <- terra::rast(
    paste0("/vsigzip/", path_to_nightlight)
)

nightlight_region <- terra::crop(
    x = nightlight,
    y = bounding_box,
    snap = "in"
)

nightlight_clean <- terra::ifel(
    nightlight_region == 0,
    NA,
    nightlight_region
)

nightlight_resampled <- terra::resample(
    x = nightlight_region,
    y = shipping_traffic_clean,
    method = "bilinear"
)



terra::plot(nightlight_resampled)

# STEP 5: Map

nightlight_cols <- c("black", "#1f4762", "#FFD966", "white")
# nightlight_cols <- c("black", "grey80", "white")

nightlight_pal <- colorRampPalette(
    nightlight_cols,
    bias = 8
)(512)

shipping_traffic_cols <- hcl.colors(
    n = 5,
    palette = "Blues"
)

scales::show_col(
    shipping_traffic_cols, ncol = 5,
    labels = TRUE
)

shipping_traffic_pal <- colorRampPalette(
    shipping_traffic_cols[1:4]
)(512)

nightlight_df <- as.data.frame(
    nightlight_resampled,
    xy = TRUE,
    na.rm = TRUE
)

names(nightlight_df)[3] <- "nightlight_value"

shipping_traffic_df <- as.data.frame(
    shipping_traffic_clean,
    xy = TRUE,
    na.rm = TRUE
)

head(shipping_traffic_df)
head(nightlight_df)

map <- ggplot() +
geom_tile(
    data = nightlight_df,
            aes(
                x = x,
                y = y,
                fill = nightlight_value
            )
        ) +
    # tidyterra::geom_spatraster(
    #     data = nightlight_resampled
    # ) +
    scale_fill_gradientn(
        colours = nightlight_pal
    ) +
    # start a new scale
    ggnewscale::new_scale_fill() +
    geom_raster(
    data = shipping_traffic_df,
            aes(
                x = x,
                y = y,
                fill = shipdensity_global
            )
        ) +
    # tidyterra::geom_spatraster(
    #     data = shipping_traffic_clean
    # ) +
    scale_fill_gradientn(
        colours = shipping_traffic_pal,
        breaks = traffic_breaks
    ) +
    theme_void() +
    theme(
        legend.position = "none"
    )

ggsave(
    filename = "shipping-traffic-Blues.png",
    plot = map,
    width = 7,
    height = 7
)