#############################################
# Sex balance with R
# Milos Popovic 2024/01/09
#############################################
setwd("/Users/mpopovic3/Downloads/sex-ratio")

libs <- c(
    "giscoR", "terra", "elevatr",
    "tidyverse", "rayshader", "magick"
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

# 1. COUNTRY BOUNDARIES
#----------------------

main_dir <- getwd()

country_sf <- geodata::gadm(
    country = "BE",
    level = 1,
    path = main_dir
) |>
    sf::st_as_sf() |>
    dplyr::filter(
        NAME_1 == "Bruxelles"
    )

plot(sf::st_geometry(country_sf))

unique(country_sf$NAME_1)

# 2. DATA
#--------

urls <- c(
    "https://data.humdata.org/dataset/1979ecab-8630-41f3-a826-b2dc71fe0bc4/resource/989ce23c-0218-4da9-bf77-c1ad94a7257f/download/bel_men_2020_geotiff.zip",
    "https://data.humdata.org/dataset/1979ecab-8630-41f3-a826-b2dc71fe0bc4/resource/7ee111b4-084d-4850-a177-c3b7bf2bdd75/download/bel_women_2020_geotiff.zip"
)

for (url in urls) {
    download.file(url,
        destfile = basename(url),
        mode = "wb"
    )
}

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

# 3 CROP SWISS DATA

country_rasters <- lapply(
    raster_list,
    function(x) {
        terra::crop(
            x,
            terra::vect(
                country_sf
            ),
            snap = "in",
            mask = T
        )
    }
)

# 4. CALCULATE HUMAN SEX RATIO
#-----------------------------

# sex_ratio <- terra::lapp(
#     terra::sds(
#         list(
#             raster_list[[1]], raster_list[[2]]
#             )),
#     fun = function(males, females){
#         return((100 * males) / females)
#     }
# )

sex_ratio <- (
    (100 * country_rasters[[1]]) / country_rasters[[2]]
)

terra::plot(sex_ratio)
plot(sf::st_geometry(country_sf), add = T)

crs_europe <- "EPSG:3035"



sex_ratio_agg <- terra::aggregate(
    x = sex_ratio_reproj,
    fact = 2,
    fun = "mean"
)

terra::plot(sex_ratio_agg)



c("#2686A0", "#EDEAC2", "#A36B2B")

p <- ggplot(sex_ratio_df) +
    tidyterra::geom_spatraster_rgb(
        data = lay
    ) +
    geom_sf(
        data = country_sf,
        fill = NA,
        color = "grey10",
        size = 1
    ) +
    geom_tile(
        data = sex_ratio_df,
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
    coord_sf(crs = crs_europe) +
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
    theme_minimal() +
    theme(
        axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = c(.1, .85),
        legend.title = element_text(
            size = 12, color = "grey10"
        ),
        legend.text = element_text(
            size = 10, color = "grey10"
        ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        legend.background = element_rect(
            fill = "white", color = NA
        ),
        panel.border = element_blank(),
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
    "brussels-sex-ratio-layer.png",
    p,
    width = w * 5,
    height = h * 5,
    units = "px",
    bg = "white"
)

rayshader::plot_gg(
    ggobj = p,
    width = w / 100,
    height = h / 100,
    windowsize = c(w, h),
    scale = 200,
    solid = F,
    shadow = T,
    shadowcolor = "white",
    # shadowwidth = 0,
    shadow_intensity = 1,
    zoom = .5,
    phi = 30,
    theta = -30,
    multicore = T
)

rayshader::render_camera(
    phi = 87,
    zoom = .58,
    theta = 0
)

rayshader::render_highquality(
    # filename = "brussels-sex-balance-3d.png",
    preview = T,
    interactive = F,
    parallel = T,
    light = F,
    environment_light = "/Users/mpopovic3/Downloads/brown_photostudio_02_4k.hdr",
    intensity_env = 1.25,
    rotate_env = 90,
    width = w,
    height = h,
    sample_method = "sobol"
)

bb <- country_sf |>
    sf::st_bbox(crs = 4326) |>
    sf::st_as_sfc(crs = 4326) |>
    sf::st_transform(crs = crs_europe)

# 3. GET STREET LAYER

layer <- maptiles::get_tiles(
    bb,
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
maptiles::plot_tiles(layer)

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

cols <- hcl.colors(
    n = 3,
    palette = "Earth",
    rev = T
)

p2 <-
    ggplot(data = sex_ratio_df) +
    tidyterra::geom_spatraster_rgb(
        data = layer,
        smooth = T
    ) +
    geom_sf(
        data = country_sf,
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
    "brussels-sex-ratio-layer.png",
    p2,
    width = w * 5,
    height = h * 5,
    units = "px",
    bg = "white"
)
