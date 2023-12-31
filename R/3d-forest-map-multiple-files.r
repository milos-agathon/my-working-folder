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
sf::sf_use_s2(F)
crs_lambert <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"


get_bbox <- function() {
    bb <- sf::st_sfc(
        sf::st_polygon(list(
            cbind(
                c(-5.207520, 9.656982, 9.656982, -5.207520, -5.207520),
                c(41.335576, 41.335576, 51.179343, 51.179343, 41.335576)
            )
        )),
        crs = "EPSG:4326"
    )

    return(bb)
}

bb <- get_bbox()

country_borders <- giscoR::gisco_get_countries(
    resolution = "1",
    country = "FR"
) |>
    sf::st_intersection(bb)

plot(sf::st_geometry(country_borders))

# 2. GET FOREST COVER
#--------------------

raster_files <- c(
    "W020N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif",
    "E000N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif"
)

# 3. LOAD forest COVER
#---------------------

forest_cover <- lapply(raster_files, terra::rast)

forest_rasters <- lapply(
    forest_cover,
    function(x) {
        terra::crop(
            x,
            terra::vect(country_borders),
            snap = "in",
            mask = T
        )
    }
)

forest_cover_mosaic <- do.call(
    terra::mosaic,
    forest_rasters
)

forest_cover_fr <- forest_cover_mosaic |>
    terra::project(crs_lambert) |>
    terra::aggregate(fact = 2)

terra::plot(
    forest_cover_fr
)

# 4. RASTER TO DATAFRAME
#-----------------------
forest_cover_df <- forest_cover_fr |>
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
        legend.position = c(.1, .4),
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

# ggsave(
#     "france-forest-cover1.png",
#     width = 7, height = 7,
#     p
# )

# 9. RAYSHADER
#-------------

h <- nrow(forest_cover_fr)
w <- ncol(forest_cover_fr)

rayshader::plot_gg(
    ggobj = p,
    width = w / 1000,
    height = h / 1000,
    scale = 150,
    solid = F,
    shadow = T,
    shadowcolor = "white",
    shadowwidth = 0,
    shadow_intensity = 1,
    sunangle = 315,
    window.size = c(w / 10, h / 10),
    zoom = .5,
    phi = 30,
    theta = -30,
    multicore = T
)

rayshader::render_camera(
    phi = 85,
    zoom = .65,
    theta = 0
)

# 9. RENDER OBJECT
#-----------------

rayshader::render_highquality(
    filename = "france-forest-cover-2019c.png",
    preview = T,
    interactive = F,
    parallel = T,
    light = F,
    environment_light = "D:/forest2019/air_museum_playground_4k.hdr",
    intensity_env = 1.85,
    rotate_env = 90,
    ground_material =
        rayrender::microfacet(
            roughness = .6
        ),
    width = w,
    height = h
)

# ©2023 Milos Popovic (https://milospopovic.net) 
# Data:  Copernicus Global Land Service: Land Cover 100m
