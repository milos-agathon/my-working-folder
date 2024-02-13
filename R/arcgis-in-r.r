# 1. PACKAGES
#------------

setwd("/Users/mpopovic3/Downloads")

# remotes::install_github(
#     "r-arcgis/arcgis",
#     dependencies = TRUE,
#     force = TRUE
# )

libs <- c(
    "arcgis",
    "tidyverse",
    "giscoR",
    "sf",
    "classInt"
)

installed_libs <- libs %in% rownames(
    installed.packages()
)

if (any(installed_libs == F)) {
    install.packages(
        libs[!installed_libs]
    )
}

invisible(
    lapply(
        libs, library,
        character.only = T
    )
)

# 2. CITY BOUNDARIES
#-------------------

furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/World_Urban_Areas/FeatureServer/0"

city_data <- arcgislayers::arc_open(furl)
city_data

city_sf <- arcgislayers::arc_select(
    city_data,
    fields = c("NAME", "ISO_CC"),
    where = "NAME = 'Agra'",
    crs = 4326
)

plot(sf::st_geometry(city_sf))

city_bbox <- sf::st_bbox(
    city_sf
)

# 3. LANDSAT IMAGE
#-----------------

url <- "https://landsat2.arcgis.com/arcgis/rest/services/Landsat8_Views/ImageServer"

landsat_data <- arcgislayers::arc_open(url)

city_raster <- arcgislayers::arc_raster(
    x = landsat_data,
    xmin = city_bbox[["xmin"]],
    xmax = city_bbox[["xmax"]],
    ymin = city_bbox[["ymin"]],
    ymax = city_bbox[["ymax"]],
    crs = sf::st_crs(city_sf),
    width = 2000,
    height = 2000
)

terra::plot(city_raster[[5]])
names(city_raster)

# 4. PLOT LANDSAT IMAGE
#----------------------

vals <- terra::values(city_raster[[2:4]])
min_val <- min(vals)
max_val <- max(vals)

p1 <- ggplot() +
    tidyterra::geom_spatraster_rgb(
        data = city_raster,
        r = 4,
        g = 3,
        b = 2,
        alpha = 1,
        max_col_value = max_val,
    ) +
    theme_void() +
    theme(
        plot.margin = unit(
            c(
                t = 1, r = -1.75,
                b = -1.75, l = -1.75
            ),
            "lines"
        )
    )

ggsave(
    filename = "agra_cols.png",
    width = 7, height = 7, dpi = 600,
    device = "png", bg = "white", p1
)

# 5. NDVI
#--------

red <- city_raster[[4]]
nir <- city_raster[[5]]

ndvi <- (nir - red) / (nir + red)
terra::plot(ndvi)
terra::summary(ndvi)

ndvi_clamped <- terra::clamp(
    x = ndvi,
    lower = 0,
    upper = 1,
    values = TRUE
)

ndvi_vals <- terra::values(ndvi_clamped)

breaks <- classInt::classIntervals(
    ndvi_vals,
    n = 7,
    style = "equal"
)$brks

colors <- hcl.colors(
    n = length(breaks),
    palette = "Green-Brown",
    rev = T
)

# 6. 2D MAP OF NDVI
#------------------

p2 <- ggplot() +
    tidyterra::geom_spatraster(
        data = ndvi_clamped
    ) +
    scale_fill_gradientn(
        name = "NDVI",
        colors = colors,
        breaks = breaks,
        limits = c(
            min(ndvi_vals),
            max(ndvi_vals)
        ),
        labels = round(breaks, 2)
    ) +
    guides(
        fill = guide_colorbar(
            direction = "horizontal",
            barheight = unit(1.5, "mm"),
            barwidth = unit(100, "mm"),
            title.position = "top",
            label.position = "bottom",
            title.hjust = .5,
            label.hjust = .5,
            nrow = 1,
            byrow = T
        )
    ) +
    theme_void() +
    theme(
        legend.position = "top",
        plot.margin = unit(
            c(
                t = .25, r = -1,
                b = -1, l = -1
            ),
            "lines"
        )
    )

ggsave(
    filename = "agra_ndvi.png",
    width = 7, height = 7, dpi = 600,
    device = "png", bg = "white", p2
)

# 7. CLASSIFY NDVI VALUES
#------------------------

ndvi_class <- terra::classify(
    ndvi,
    breaks,
    include.lowest = TRUE
)

terra::plot(ndvi_class)
hcl.pals("diverging")

values <- unique(sort(terra::values(ndvi_class)))

ndvi_colors <- hcl.colors(
    n = length(values),
    palette = "Green-Brown",
    rev = T
)

terra::coltab(ndvi_class) <- data.frame(
    values = values,
    colors = ndvi_colors
)

# 8. RESAMPLE
#------------

elev <- elevatr::get_elev_raster(
    locations = city_sf,
    z = 14, clip = "bbox"
)

elev_3857 <- elev |>
    terra::rast() |>
    terra::project(
        terra::crs(ndvi_class)
    )

ndvi_img <- terra::resample(
    ndvi_class,
    elev_3857,
    "near"
)

img_file <- "agra_ndvi.png"

terra::writeRaster(
    x = ndvi_img,
    filename = img_file,
    overwrite = TRUE
)

# 9. RENDER SCENE
#----------------

img <- png::readPNG(img_file)

elmat <- rayshader::raster_to_matrix(
    elev_3857
)

h <- nrow(elev_3857)
w <- ncol(elev_3857)

elmat |>
    rayshader::height_shade(
        texture = colorRampPalette(
            "white"
        )(512)
    ) |>
    rayshader::add_overlay(
        img,
        alphalayer = 1,
        alphacolor = "white"
    ) |>
    rayshader::plot_3d(
        elmat,
        zscale = 3,
        solid = F,
        shadow = F,
        shadow_darkness = 1,
        background = "white",
        windowsize = c(
            w / 8, h / 8
        ),
        zoom = .5,
        phi = 87,
        theta = 0
    )

rayshader::render_camera(
    zoom = .5,
    phi = 85
)

# 10. RENDER OBJECT
#----------------

rayshader::render_highquality(
    filename = "3d-ndvi-agra.png",
    preview = T,
    light = F,
    environment_light = "/Users/mpopovic3/Downloads/photo_studio_loft_hall_4k.hdr",
    intensity_env = 1.5,
    rotate_env = 90,
    # ground_material = rayrender::diffuse(
    #     color = "black"
    # ),
    interactive = F,
    parallel = T,
    width = w, height = h
)
