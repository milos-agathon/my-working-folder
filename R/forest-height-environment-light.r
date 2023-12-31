#############################################
# 3D forest height with R
# Milos Popovic 2023/07/18
#############################################
setwd("D:/tree-height/korea")
libs <- c(
    "tidyverse", "sf", "geodata",
    "terra", "classInt", "rayshader"
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
    libs,
    library,
    character.only = T
))

# 1. DOWNLOAD ETH DATA
#---------------------
urls <- c(
    "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N36E123_Map.tif",
    "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N36E126_Map.tif",
    "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N36E129_Map.tif",
    "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N33E123_Map.tif",
    "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N33E126_Map.tif",
    "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N33E129_Map.tif"
)

for (url in urls) {
    download.file(
        url,
        destfile = basename(gsub(".*ETH_", "", url)),
        mode = "wb"
    )
}

raster_files <-
    list.files(
        path = getwd(),
        pattern = "Map",
        full.names = T
    )

# 2. country POLYGON
#--------------------
sf::sf_use_s2(F)
get_country_borders <- function() {
    country_borders <- giscoR::gisco_get_countries(
        resolution = "1",
        country = "KOR"
    )

    return(country_borders)
}

country_sf <- get_country_borders()


plot(sf::st_geometry(
    country_sf
))

# 3. LOAD FOREST HEIGHT
#----------------------
forest_height_list <- lapply(
    raster_files,
    terra::rast
)

forest_height_rasters <- lapply(
    forest_height_list,
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

forest_height_mosaic <- do.call(
    terra::mosaic,
    forest_height_rasters
)

crs_lambert <-
    "+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"

forest_height_country <- forest_height_mosaic |>
    terra::aggregate(
        fact = 5
    ) |>
    terra::project(crs_lambert)

terra::plot(forest_height_country)

# 4. RASTER TO DATAFRAME
#-----------------------
forest_height_country_df <- forest_height_country |>
    as.data.frame(
        xy = T
    )

head(forest_height_country_df)
names(forest_height_country_df)[3] <- "height"

# 5. BREAKS
#----------

breaks <- classInt::classIntervals(
    forest_height_country_df$height,
    n = 4,
    style = "equal"
)$brks

# 6. COLORS
#----------
cols <-
    c(
        "white", "#ffd3af", "#fbe06e",
        "#6daa55", "#205544"
    )

texture <- colorRampPalette(
    cols,
    bias = 2
)(512)

# pie(rep(1, 512), col = texture)

# 7. GGPLOT2
#-----------
p <- ggplot(
    forest_height_country_df
) +
    geom_raster(
        aes(
            x = x,
            y = y,
            fill = height
        )
    ) +
    scale_fill_gradientn(
        name = "height (m)",
        colors = texture,
        breaks = round(breaks, 0),
        limits = c(
            0,
            max(
                round(
                    forest_height_country_df$height
                ), 0
            )
        )
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
        legend.position = c(.01, .3),
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

# 8. RENDER SCENE
#----------------

h <- nrow(forest_height_country)
w <- ncol(forest_height_country)

rayshader::plot_gg(
    ggobj = p,
    width = w / 2000,
    height = h / 2000,
    scale = 150,
    solid = F,
    shadow = T,
    shadowcolor = "white",
    shadowwidth = 0,
    shadow_intensity = 1,
    sunangle = 315,
    window.size = c(w / 20, h / 20),
    zoom = .5,
    phi = 30,
    theta = -30,
    multicore = T
)

rayshader::render_camera(
    phi = 70,
    zoom = .65,
    theta = 0
)

# 9. RENDER OBJECT
#-----------------

rayshader::render_highquality(
    filename = "korea-forest-height-2020.png",
    preview = T,
    interactive = F,
    parallel = T,
    light = F,
    environment_light = "D:/forest2019/air_museum_playground_4k.hdr",
    intensity_env = 1,
    rotate_env = 90,
    ground_material =
        rayrender::microfacet(
            roughness = .6
        ),
    width = w / 2,
    height = h / 2
)

# ©2023 Milos Popovic (https://milospopovic.net)
# ETH Global Canopy Height 10m 2020 version1 derived from Sentinel-2 and GEDI
