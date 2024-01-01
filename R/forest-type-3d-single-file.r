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

country_sf <- giscoR::gisco_get_countries(
    country = "KOR",
    resolution = "1"
)

plot(sf::st_geometry(country_sf))

# 2. FOREST TYPE RASTER
#----------------------

urls <- c(
    "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/E000N60/E020N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Forest-Type-layer_EPSG-4326.tif",
    "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/E020N60/E040N40_PROBAV_LC100_global_v3.0.1_2019-nrt_Forest-Type-layer_EPSG-4326.tif",
    "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/E020N60/E040N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Forest-Type-layer_EPSG-4326.tif",
    "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/E020N60/E020N40_PROBAV_LC100_global_v3.0.1_2019-nrt_Forest-Type-layer_EPSG-4326.tif"
)

for (url in urls) {
    download.file(
        url = url,
        destfile = basename(url),
        mode = "wb"
    )
}

forest_type_list <-
    terra::rast(
        "E120N40_PROBAV_LC100_global_v3.0.1_2019-nrt_Forest-Type-layer_EPSG-4326.tif"
    )

# 3. CROP FOREST TYPE RASTER
#---------------------------

forest_type <-
    terra::crop(
        forest_type_list,
        terra::vect(
            country_sf
        ),
        snap = "in",
        mask = T
    )

crs_lambert <-
    "+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"


vals <- terra::values(
    forest_type,
    dataframe = T
)

names(vals)
names(vals)[1] <- "value"
unique(vals$value)


# 4. FOREST TYPE RASTER TO IMAGE
#-------------------------------

cols <- c(
    "grey70",
    "#f6aa08",
    "#33A02C",
    "#89099d",
    "#21b7f7",
    "#e30f44"
)

from <- c(0:5)
to <- t(col2rgb(
    cols
))

forest_terra <- na.omit(
    forest_type
)

elev <- elevatr::get_elev_raster(
    locations = country_sf,
    z = 9, clip = "locations"
)

elev_lambert <- elev |>
    terra::rast()

forest_terra_resampled <- terra::resample(
    x = forest_terra,
    y = elev_lambert,
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

img_file <- "korea-forest-image-resampled.png"
terra::writeRaster(
    forest_type_image,
    img_file,
    overwrite = T,
    NAflag = 255
)

img <- png::readPNG(img_file)

# 5. COUNTRY ELEVATION RASTER
#----------------------------

elev_lambert <- terra::project(
    elev_lambert,
    crs_lambert
)

elmat <- elev_lambert |>
    rayshader::raster_to_matrix()

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
    rayshader::plot_3d(
        elmat,
        zscale = 10,
        solid = F,
        shadow = F,
        shadow_darkness = 1,
        background = "white",
        windowsize = c(
            w / 5, h / 5
        ),
        zoom = .5,
        phi = 87,
        theta = 0
    )

rayshader::render_camera(
    phi = 87,
    zoom = .675,
    theta = 0
)

# 7. RENDER OBJECT
#-----------------

filename <- "korea-forest-type-3d.png"

rayshader::render_highquality(
    filename = filename,
    preview = T,
    light = F,
    environment_light = "air_museum_playground_4k.hdr",
    intensity_env = .85,
    rotate_env = 90,
    interactive = F,
    parallel = T,
    width = w * 1.5,
    height = h * 1.5
)

# 8. MAKE LEGEND
#---------------

png("my_legend.png", width = 800, height = 800)
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
    ), 4000
) |>
    magick::image_transparent("white")

p <- magick::image_composite(
    magick::image_scale(
        forest_img,
        "x5000"
    ),
    my_legend_scaled,
    gravity = "north",
    offset = "+2000+2800"
)

magick::image_write(
    p,
    "korea-final-map.png"
)

# ©2023 Milos Popovic (https://milospopovic.net) | Data:  Copernicus Global Land Service: Land Cover 100m
