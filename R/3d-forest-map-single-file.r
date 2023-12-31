#############################################
# 3D forest map with R
# Milos Popovic 2023/04/23
#############################################
setwd("D:/forest2019")

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
crs_lambert <-
    "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_frfs"

sf::sf_use_s2(F)

country_borders <- giscoR::gisco_get_countries(
    resolution = "1",
    country = "BE"
)

plot(sf::st_geometry(country_borders))

# 2. GET FOREST COVER
#--------------------

start_url <- "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/"
var_url <- c(
    "E000N60/E000N60"
)
end <- "_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif"

urls <- paste0(start_url, var_url, end)

for (url in urls) {
    download.file(url, destfile = basename(url), mode = "wb")
}

raster_files <-
    "E000N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif"


# 3. LOAD forest COVER
#---------------------

forest_cover <- terra::rast(
    raster_files
    )

forest_rasters <-
        terra::crop(
            forest_cover,
            terra::vect(country_borders),
            snap = "in",
            mask = T
        ) |>
    terra::project(crs_lambert)

terra::plot(forest_rasters)

# 9. RAYSHADER
#-------------

cols <- rev(
    c(
        "#205544", "#6daa55",
        "#fbe06e", "#ffd3af",
        "#fefed3"
    )
)


texture <- colorRampPalette(cols, bias = 2)(256)

# 5. RASTER TO DATAFRAME
#-----------------------
forest_cover_df <- forest_rasters |>
    as.data.frame(xy = T)

names(forest_cover_df)
names(forest_cover_df)[3] <- "percent_cover"

# 6. BREAKS
#----------
summary(forest_cover_df$percent_cover)
min_val <- min(forest_cover_df$percent_cover)
max_val <- max(forest_cover_df$percent_cover)
limits <- c(min_val, max_val)

breaks <- seq(from = min_val, to = max_val, by = 20)

# 8. GGPLOT2
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
        legend.position = c(.1, .2),
        legend.title = element_text(
            size = 11, color = "grey10"
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

# 9. RAYSHADER
#-------------

w <- ncol(forest_rasters)
h <- nrow(forest_rasters)

rayshader::plot_gg(
    ggobj = p,
    width = w / 500,
    height = h / 500,
    windowsize = c(w / 5, h / 5),
    scale = 75,
    solid = F,
    shadow = T,
    shadowcolor = "white",
    shadowwidth = 0,
    shadow_intensity = 1,
    zoom = .5,
    phi = 30,
    theta = -30,
    multicore = T
)

rayshader::render_camera(
    phi = 87,
    zoom = .625,
    theta = 0
)

rayshader::render_highquality(
    filename = "belgium-3d-forest.png",
    preview = T,
    interactive = F,
    parallel = T,
    light = F,
    environment_light = "D:/forest2019/air_museum_playground_4k.hdr",
    intensity_env = 2,
    rotate_env = 90,
    ground_material =
        rayrender::microfacet(
            roughness = .6
        ),
    width = w,
    height = h
)

# ©2023 Milos Popovic (https://milospopovic.net) | Data:  Copernicus Global Land Service: Land Cover 100m