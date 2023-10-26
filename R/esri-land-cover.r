setwd("D:/esri-land-cover/lulc2022")
# define libraries we need
libs <- c(
    "terra",
    "sf",
    "giscoR",
    "ggtern"
)

# remotes::install_github("wmgeolab/rgeoboundaries")

# install missing libraries
installed_libraries <-
    libs %in% rownames(
        installed.packages()
    )
if (
    any(
        installed_libraries
        == F
    )
) {
    install.packages(
        libs[
            !installed_libraries
        ]
    )
}

# load libraries
invisible(
    lapply(
        libs, library,
        character.only = T
    )
)

r1 <- terra::rast(
    "33T_20220101-20230101.tif"
)

crs_lambert <-
    "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_frfs"

crs <- "+proj=longlat +datum=WGS84 +no_defs"

get_country <- function() {
    country <-
        giscoR::gisco_get_countries(
            country = "BA",
            epsg = "4326",
            resolution = "1"
        )

    return(country)
}

country <- get_country() 

country_sf <- country |>
    sf::st_transform(crs = crs_lambert)

country1 <- country_sf |> 
    sf::st_transform(
        crs = crs(r1)
    )

r1c <- terra::crop(
    r1,
    terra::vect(country1),
    snap = "in",
    mask = T
) |>
terra::aggregate(fact = 5) |>
terra::project(crs_lambert)

terra::plot(r1c)

r2 <- terra::rast(
    "34T_20220101-20230101.tif"
)

country2 <- country_sf |> 
    sf::st_transform(
        crs = crs(r2)
    )

r2c <- terra::crop(
    r2,
    terra::vect(country2),
    snap = "in",
    mask = T
) |>
terra::aggregate(fact = 5) |>
terra::project(crs_lambert)

writeRaster(r1c, "r1laea.tif", overwrite=TRUE)
writeRaster(r2c, "r2laea.tif", overwrite = TRUE)
r_list <- c("r1laea.tif", "r2laea.tif")
v <- vrt(r_list, "test_laea.vrt", overwrite = TRUE)
terra::plot(v)

# vals <- terra::values(
#     v,
#     mat = F, 
#     dataframe = T)
# tail(vals)

# names(vals)[1] <- "value"
# unique(vals$value)

ct <- do.call(
    data.frame,
        terra::coltab(r1)  
    )
hex_code <- ggtern::rgb2hex(
    r = ct[,2],
    g = ct[,3],
    b = ct[,4]
    # ct[,2:4],
    # maxColorValue = 11
) # convert to hex

cols <- hex_code[c(2:3, 5:6, 8:12)]
from <- c(1:2, 4:5, 7:11)
to <- t(col2rgb(cols))
lc <- na.omit(v)

lc_ras <- terra::subst(
    lc, from, to,
    names = cols
) |>
terra::project(
    crs_lambert
) |>
terra::crop(
    terra::vect(country_sf),
    snap = "in",
    mask = T
)

elev <- elevatr::get_elev_raster(
    locations = country,
    z = 9, clip = "locations"
) 

elev_lambert <- elev |> 
    terra::rast() |>
    terra::project(crs_lambert)

lc_resampled <- terra::resample(
    x = lc_ras,
    y = elev_lambert,
    method = "near" 
)

terra::plotRGB(lc_ras)
terra::plotRGB(lc_resampled)

plot(
    sf::st_geometry(country_sf)
)
terra::plot(elev, add = T)

img_file <- "lc_bih_laea.png"
terra::writeRaster(
    lc_resampled, img_file, 
    overwrite = T,
    NAflag = 255
    )

img <- png::readPNG(img_file)

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
            cols[9]
        )(512)
    ) |>
    rayshader::add_overlay(
        img,
        alphalayer = 1,
        # alphacolor = "white",
        rescale_original = T
    ) |>
    rayshader::add_shadow(
        rayshader::lamb_shade(
            elmat,
            zscale = 50,
            sunaltitude = 90,
            sunangle = 315,
        ), max_darken = .25
    ) |>
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
        zscale = 15,
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
    phi = 80,
    zoom = .58,
    theta = 0
)

# 7. RENDER OBJECT
#-----------------
filename <- "land-cover-bih-3d-b.png"
rayshader::render_highquality(
    filename = filename,
    preview = T,
    light = F,
    environment_light = "D:/forest2019/air_museum_playground_4k.hdr",
    intensity_env = 2,
    rotate_env = 90,
    interactive = F,
    parallel = T,
    width = w * 1.75, 
    height = h * 1.75
)

# 8. MAKE LEGEND
#---------------
legend_name <- "lc_legend.png"
png(legend_name)
par(family = "mono")
plot(
    NULL, xaxt = "n",
    yaxt = "n", bty = "n",
    ylab = "", xlab = "",
    xlim = 0:1, ylim = 0:1,
    xaxs = "i", yaxs = "i"
)
legend(
    "center",
    legend = c(
        "Water",
        "Trees",
        "Crops",
        "Built area",
        "Rangeland"
    ),
    pch = 15,
    cex = 2,
    pt.cex = .1,
    bty = "n",
    col = c(cols[c(1:2, 4:5)], "white"),
    fill = c(cols[c(1:2, 4:5)], "white"),
    border = "grey20"
)
dev.off()

# 9. FINAL MAP
#-------------

lc_img <- magick::image_read(
    filename
)

my_legend <- magick::image_read(
    legend_name
)

my_legend_scaled <- magick::image_scale(
    magick::image_background(
    my_legend, "none"), 2000
) |>
magick::image_transparent("white")

p <- magick::image_composite(
    magick::image_scale(
        lc_img,
        "x4000"
    ),
    my_legend_scaled,
    offset = "+100+0",
    gravity = "southwest"
)

magick::image_write(
    p,
    "bih-lc-final-map.png"
)


 

