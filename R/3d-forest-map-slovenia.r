#############################################
# 3D forest map with R
# Milos Popovic 2023/04/23
#############################################
setwd("/Users/mpopovic3/Downloads")


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
crs_albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 
                          +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

get_country_admin1 <- function() {
    main_path <- getwd()
    country_admin1 <- geodata::gadm(
        country = "SVN",
        level = 0,
        path = main_path
    ) |>
        sf::st_as_sf()

    return(country_admin1)
}

country_borders <- get_country_admin1()


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

# 9. RAYSHADER
#-------------

cols <- rev(
    c(
        "#205544", "#6daa55", 
        "#fbe06e", "#ffd3af", 
        "#fefed3")
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

# 7. COLORS
#----------


# 8. GGPLOT2
#-----------

p <- ggplot(forest_cover_df) +
    geom_raster(
        aes(
            x = x, y = y, fill = percent_cover
        )) +
    # geom_sf(data = country_borders,
    #     fill = "transparent", color = "black", size = .2) +
    scale_fill_gradientn(
        name = "% of area",
        colours = texture,
        breaks = breaks,
        limits = limits
    ) +
    coord_sf(crs = crs_lambert) +
    guides(
        fill = guide_colorbar(
            direction = "horizontal",
            title.position = "top",
            label.position = "bottom",
            nrow = 1,
            byrow = T
        )
    ) +
    theme_minimal() +
    theme(
        axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        legend.title = element_text(size = 5, color = "grey10"),
        legend.text = element_text(size = 3, color = "grey10"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "white", size = 0),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        plot.margin = unit(c(t = 0, r = 0, b = 0, l = 0), "lines")
    ) +
    labs(
        title = "",
        subtitle = "",
        caption = "",
        x = "",
        y = ""
    )

# print(p)

# 9. RAYSHADER
#-------------

w <- ncol(forest_rasters)
h <- nrow(forest_rasters)

rayshader::plot_gg(
    ggobj = p,
    multicore = T,
    width = w / 1000,
    height = h / 1000,
    windowsize = c(800, 800),
    scale = 50,
    solid = F,
    shadow = T,
    shadow_intensity = 1,
    offset_edges = F,
    sunangle = 135,
    phi = 30,
    theta = -30
)

rayshader::render_camera(
    phi = 85,
    zoom = .45,
    theta = 0
)

rayshader::render_highquality(
    filename = "slovenia-forest.png",
    preview = T,
    light = T,
    lightdirection = 315, # 210
    lightintensity = 1100, # 2000
    lightaltitude = 85, # 90
    interactive = F,
    ground_material = rayrender::microfacet(
        roughness = .6
    ),
    parallel = T,
    width = 5000, height = 5000
)

# ©2023 Milos Popovic (https://milospopovic.net) | Data:  Copernicus Global Land Service: Land Cover 100m

rayshader::render_snapshot(
    "img-high-contrast-high-light.png"
)

