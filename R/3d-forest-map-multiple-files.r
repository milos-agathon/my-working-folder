#############################################
# 3D forest map with R
# Milos Popovic 2023/04/23
#############################################
setwd("/Users/mpopovic3/Downloads/russia")

# define libraries we need
libs <- c(
    "tidyverse", "terra", "sf",
    "giscoR", "rayshader", "geodata"
)

# install missing libraries
installed_libraries <- libs %in% rownames(installed.packages())
if (any(installed_libraries == F)) {
    installed.packages(libs[!installed_libraries])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# 1. GET COUNTRY MAP
#-------------------
# Lambert projection
crs_longlat <- "+proj=longlat +datum=WGS84 +no_frfs"
sf::sf_use_s2(F)

country_borders <- giscoR::gisco_get_countries(
    country = "RUS",
    resolution = "10"
)

# 2. GET FOREST COVER
#--------------------

urls <- c(
    "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/E040N80/E040N80_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif",
    "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/E060N80/E060N80_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif",
    "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/E080N80/E080N80_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif",
    "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/E100N80/E100N80_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif",
    "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/E120N80/E120N80_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif",
    "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/E140N80/E140N80_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif",
    "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/E160N80/E160N80_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif",
    "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/W180N80/W180N80_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif",
    "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/E040N60/E040N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif",
    "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/E060N60/E060N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif",
    "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/E080N60/E080N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif",
    "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/E100N60/E100N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif",
    "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/E120N60/E120N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif",
    "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/E140N60/E140N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif",
    "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/E160N60/E160N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif"
)

for (url in urls) {
    download.file(
        url,
        destfile = basename(url),
        mode = "wb"
    )
}

raster_files <- basename(urls)

# 3. LOAD forest COVER
#---------------------

forest_cover <- lapply(raster_files, terra::rast)

forest_rasters <- lapply(
    forest_cover,
    function(x) {
        terra::crop(
            x,
            terra::vect(country_borders)
        )
    }
)

forest_cover_mosaic <- do.call(
    terra::mosaic,
    forest_rasters
)

forest_cover_agg <- forest_cover_mosaic |>
    terra::crop(
        terra::vect(country_borders),
        snap = "in",
        mask = TRUE
    )

crs_lambert <-
    "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=105 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

forest_cover_reproj <- forest_cover_agg |>
    terra::aggregate(fact = 5) |>
    terra::project(crs_lambert)

terra::plot(
    forest_cover_reproj
)

# 4. RASTER TO DATAFRAME
#-----------------------
forest_cover_df <- forest_cover_reproj |>
    as.data.frame(xy = T)

names(forest_cover_df)
names(forest_cover_df)[3] <- "percent_cover"

# 5. BREAKS
#----------
summary(forest_cover_df$percent_cover)
min_val <- min(forest_cover_df$percent_cover)
max_val <- max(forest_cover_df$percent_cover)
limits <- c(min_val, max_val)

breaks <- seq(from = min_val, to = max_val, by = 20)

# 6. COLORS
#----------

cols <- rev(
    c(
        "#205544", "#6daa55",
        "#fbe06e", "#ffd3af",
        "#fefed3"
    )
)

texture <- colorRampPalette(cols, bias = 2)(256)

# 7. GGPLOT2
#-----------
p <- ggplot(forest_cover_df) +
    geom_raster(
        aes(
            x = x, y = y,
            fill = percent_cover
        )
    ) +
    scale_fill_gradientn(
        name = "% of area",
        colours = texture,
        breaks = breaks,
        limits = limits
    ) +
    coord_sf(crs = crs_lambert) +
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
        legend.position = c(.9, .2),
        legend.title = element_text(
            size = 14, color = "grey10"
        ),
        legend.text = element_text(
            size = 12, color = "grey10"
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

# ggsave(
#     "france-forest-cover1.png",
#     width = 7, height = 7,
#     p
# )

# 9. RAYSHADER
#-------------

h <- nrow(forest_cover_reproj)
w <- ncol(forest_cover_reproj)

rayshader::plot_gg(
    ggobj = p,
    width = w / 1000,
    height = h / 1000,
    scale = 200,
    solid = F,
    shadow = T,
    shadowcolor = "white",
    shadowwidth = 0,
    shadow_intensity = 1,
    sunangle = 315,
    window.size = c(800, 800),
    zoom = .5,
    phi = 30,
    theta = -30,
    multicore = T
)

rayshader::render_camera(
    phi = 89,
    zoom = .85,
    theta = 0
)

# 9. RENDER OBJECT
#-----------------

rayshader::render_highquality(
    filename = "argentina-forest-cover-2019.png",
    preview = T,
    interactive = F,
    parallel = T,
    light = F,
    environment_light = "/Users/mpopovic3/Downloads/air_museum_playground_4k.hdr",
    intensity_env = 1.25,
    rotate_env = 90,
    ground_material =
        rayrender::microfacet(
            roughness = .6
        ),
    width = 4000,
    height = 4000
)

# ©2024 Milos Popovic (https://milospopovic.net)
# Data:  Copernicus Global Land Service: Land Cover 100m
