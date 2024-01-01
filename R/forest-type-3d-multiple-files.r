#############################################
# 3D forest type maps with R
# Milos Popovic 2023/10/19
#############################################
setwd("D:/forest2019")

libs <- c(
    "giscoR", "terra", "elevatr",
    "png", "rayshader", "magick"
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

get_country_admin1 <- function() {
    main_path <- getwd()
    country_admin1 <- geodata::gadm(
        country = c(
            "AND",
            "ESP", "PRT"
        ),
        level = 1,
        path = main_path
    ) |>
        sf::st_as_sf()

    return(country_admin1)
}

country_admin1 <- get_country_admin1()

country_sf <- country_admin1 |>
    dplyr::filter(
        !NAME_1 %in% c(
            "Islas Canarias",
            "Azores", "Madeira",
            "Islas Baleares",
            "Ceuta y Melilla"
        )
    ) |>
    sf::st_as_sf() |>
    sf::st_union()

plot(sf::st_geometry(country_sf))

# 2. FOREST TYPE RASTER
#----------------------

raster_files <- c(
    "E000N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Forest-Type-layer_EPSG-4326.tif",
    "W020N40_PROBAV_LC100_global_v3.0.1_2019-nrt_Forest-Type-layer_EPSG-4326.tif",
    "W020N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Forest-Type-layer_EPSG-4326.tif",
    "E000N40_PROBAV_LC100_global_v3.0.1_2019-nrt_Forest-Type-layer_EPSG-4326.tif"
)

forest_cover_list <- lapply(
    raster_files,
    terra::rast
)

# 3. CROP FOREST TYPE RASTER
#---------------------------

crs_lambert <-
    "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_frfs"

forest_cover_rasters <- lapply(
    forest_cover_list,
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

forest_cover_mosaic <- do.call(
    terra::mosaic,
    forest_cover_rasters
)


# 4. FOREST TYPE RASTER TO IMAGE
#-------------------------------

cols <- c(
    # "#073b4c",
    "grey40",
    "#21b7f7",
    "#f6aa08",
    "#e30f44"
)

vals <- terra::values(
    forest_cover_mosaic,
    dataframe = T
)

names(vals)[1] <- "value"
sort(unique(vals$value))

from <- c(0:1, 4:5)
to <- t(col2rgb(
    cols
))

forest_terra <- na.omit(
    forest_cover_mosaic
)

elev <- elevatr::get_elev_raster(
    locations = country_sf,
    z = 8, clip = "locations"
)

elev_lambert <- elev |>
    terra::rast() |>
    terra::project(crs_lambert)

forest_terra_resampled <- terra::resample(
    x = forest_terra,
    y = terra::rast(elev),
    method = "near"
)

forest_type_image <- terra::subst(
    forest_terra_resampled,
    from,
    to,
    names = cols
) |>
    terra::project(crs_lambert)

terra::plotRGB(forest_type_image)

img_file <- "iberia-forest-image-resampled.png"
terra::writeRaster(
    forest_terra, # forest_type_image,
    img_file,
    overwrite = T,
    NAflag = 255
)

img <- png::readPNG(img_file)

# 5. COUNTRY ELEVATION RASTER
#----------------------------

elmat <- rayshader::raster_to_matrix(
    elev_lambert
)

# 6. RENDER SCENE
#----------------

h <- nrow(elev_lambert)
w <- ncol(elev_lambert)

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
    # rayshader::add_shadow(
    #     rayshader::lamb_shade(
    #         elmat,
    #         zscale = 50,
    #         sunaltitude = 90,
    #         sunangle = 315,
    #     ), max_darken = .25
    # ) |>
    # rayshader::add_shadow(
    #     rayshader::texture_shade(
    #         elmat,
    #         detail = .95,
    #         brightness = 90, #warn
    #         contrast = 80,
    #     ), max_darken = .1
    # ) |>
    rayshader::plot_3d(
        elmat,
        zscale = 12,
        solid = F,
        shadow = T,
        shadow_darkness = 1,
        background = "white",
        windowsize = c(
            w / 5, h / 5
        ),
        zoom = .5,
        phi = 85,
        theta = 0
    )

rayshader::render_camera(
    phi = 85,
    zoom = .6,
    theta = 0
)

# 7. RENDER OBJECT
#-----------------

filename <- "iberia-forest-type-3d.png"

rayshader::render_highquality(
    filename = filename,
    preview = T,
    light = F,
    environment_light = "air_museum_playground_4k.hdr",
    intensity_env = 2,
    rotate_env = 90,
    interactive = F,
    parallel = T,
    width = w * 2,
    height = h * 2
)

# 8. MAKE LEGEND
#---------------

png("my_legend.png")
par(family = "mono")
plot(
    NULL,
    xaxt = "n",
    yaxt = "n", bty = "n",
    ylab = "", xlab = "",
    xlim = 0:1, ylim = 0:1,
    xaxs = "i", yaxs = "i"
)
legend(
    "center",
    legend = c(
        "Unknown",
        "Evergreen needle leaf",
        "Deciduous broad leaf",
        "Mixed"
    ),
    pch = 16,
    pt.cex = 3,
    cex = 1.5,
    bty = "n",
    col = cols
)
dev.off()

# 9. FINAL MAP
#-------------

forest_img <- magick::image_read(
    filename
)

my_legend <- magick::image_read(
    "my_legend.png"
)

my_legend_scaled <- magick::image_scale(
    magick::image_background(
        my_legend, "none"
    ), 3000
) |>
    magick::image_transparent("white")

p <- magick::image_composite(
    magick::image_scale(
        forest_img,
        "x7000"
    ),
    my_legend_scaled,
    gravity = "southwest",
    offset = "+200+0"
)

magick::image_write(
    p,
    "iberia-final-map.png"
)
