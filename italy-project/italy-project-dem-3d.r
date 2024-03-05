setwd("D:/ghsl-population")

# 1. PACKAGES

libs <- c(
    "terra",
    "geodata",
    "sf",
    "tidyverse",
    "elevatr",
    "rayshader"
)

installed_libraries <-
    libs %in% rownames(
        installed.packages()
    )

if (
    any(
        installed_libraries == F
    )
) {
    install.packages(
        libs[
            !installed_libraries
        ],
        dependencies = T
    )
}

invisible(
    lapply(
        libs, library,
        character.only = T
    )
)

# 2. DATA

crs_lambert <-
    "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_frfs"

country_sf <- giscoR::gisco_get_countries(
    country = "IT",
    resolution = "1"
)

file_names <- c(
    "GHS_POP_E2010_GLOBE_R2023A_4326_3ss_V1_0.tif",
    "GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0.tif"
)

pop_rasters <- lapply(
    file_names,
    terra::rast
)

# 3. CROP
#--------

country_pop_rasters <-
    lapply(
        pop_rasters,
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

pop_change <- (
    (country_pop_rasters[[2]] - country_pop_rasters[[1]])
)

pop_change[pop_change < 0] <- -1
pop_change[pop_change == 0] <- 0
pop_change[pop_change > 0] <- 1

terra::plot(pop_change)

cols <- rev(c(
    "#08771e",
    "grey80",
    "#b3096c"
))


from <- c(-1:1)
to <- t(col2rgb(
    cols
))

pop_change_image <- terra::subst(
    pop_change,
    from,
    to,
    names = cols
)

terra::plotRGB(pop_change_image)

elev <- elevatr::get_elev_raster(
    locations = country_sf,
    z = 8, clip = "locations"
)

elev_lambert <- elev |>
    terra::rast()

pop_resampled <- terra::resample(
    x = pop_change_image,
    y = elev_lambert,
    method = "near"
) |>
    terra::project(crs_lambert)

terra::plotRGB(pop_resampled)

img_file <- "italy-pop-image-resampled.png"

terra::writeRaster(
    pop_resampled,
    img_file,
    overwrite = T,
    NAflag = 255
)

img <- png::readPNG(img_file)

elev_reproj <- terra::project(
    elev_lambert,
    crs_lambert
)

elmat <- elev_reproj |>
    rayshader::raster_to_matrix()

# TOWN NAMES

towns <- c(
    "Milano", "Torino", "Roma",
    "Napoli"
)

centroids <- read_csv("D:/alps/cities.csv") |>
    dplyr::filter(city %in% towns) |>
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326) |>
    sf::st_transform(crs_lambert)

h <- nrow(elev_reproj)
w <- ncol(elev_reproj)

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
    rayshader::add_overlay(
        rayshader::generate_label_overlay(
            centroids,
            extent = elev_reproj,
            text_size = 8,
            point_size = 0,
            color = "black",
            halo_color = "white",
            halo_expand = 10,
            halo_blur = 20,
            halo_alpha = .8,
            seed = 123,
            heightmap = elmat,
            data_label_column = "city"
        )
    ) |>
    rayshader::plot_3d(
        elmat,
        zscale = 25,
        solid = F,
        shadow = T,
        shadow_darkness = 1,
        background = "white",
        windowsize = c(
            w / 8, h / 8
        ),
        zoom = .525,
        phi = 89,
        theta = 0
    )

start_time <- Sys.time()
filename <- "italy-population-change-2010-2020.png"

rayshader::render_highquality(
    filename = filename,
    preview = T,
    light = F,
    environment_light = "D:/forest2019/air_museum_playground_4k.hdr",
    intensity_env = .6,
    rotate_env = 90,
    interactive = F,
    parallel = T,
    width = w,
    height = h
)

end_time <- Sys.time()
end_time - start_time

legend_name <- "population_legend.png"
png(legend_name, width = 800, height = 800)
par(family = "sans")

plot(
    NULL,
    xaxt = "n",
    yaxt = "n",
    bty = "n",
    ylab = "",
    xlab = "",
    xlim = 0:1,
    ylim = 0:1,
    xaxs = "i",
    yaxs = "i"
)
legend(
    "center",
    legend = rev(c(
        "Growth",
        "Uninhabited",
        "Decline"
    )),
    pch = 16,
    cex = 3,
    pt.cex = 4,
    bty = "n",
    col = cols,
    border = "grey20"
)
dev.off()

# filename <- "land-cover-bih-3d-b.png"

lc_img <- magick::image_read(
    filename
)

my_legend <- magick::image_read(
    legend_name
)

my_legend_scaled <- magick::image_scale(
    magick::image_background(
        my_legend, "none"
    ), 3000
) |>
    magick::image_transparent("white")

p <- magick::image_composite(
    magick::image_scale(
        lc_img, "x5000"
    ),
    my_legend_scaled,
    gravity = "southeast",
    offset = "+2500+2000"
)

magick::image_write(
    p, "3d-italy-population-change-2010-2020.png"
)

# LAND COVER

setwd("D:/esri-land-cover/lulc2022")

cols <- c(
    "#419bdf", "#397d49", "#7a87c6",
    "#e49635", "#c4281b", "#a59b8f",
    "#a8ebff", "#616161", "#e3e2c3"
)

country_sf <- giscoR::gisco_get_countries(
    resolution = "1",
    country = "IT"
)

elev <- elevatr::get_elev_raster(
    locations = country_sf,
    z = 8, clip = "locations"
)

crs_lambert <-
    "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_frfs"

img_file <- "land_cover_italy.png"

img <- png::readPNG(img_file)

# 9. RENDER SCENE
#----------------

elev_lambert <- elev |>
    terra::rast() |>
    terra::project(crs_lambert)

elmat <- rayshader::raster_to_matrix(
    elev_lambert
)

h <- nrow(elev_lambert)
w <- ncol(elev_lambert)

elmat |>
    rayshader::height_shade(
        texture = colorRampPalette(
            cols[9]
        )(256)
    ) |>
    rayshader::add_overlay(
        img,
        alphalayer = 1
    ) |>
    rayshader::add_overlay(
        rayshader::generate_label_overlay(
            centroids,
            extent = elev_lambert,
            text_size = 8,
            point_size = 2,
            color = "black",
            halo_color = "white",
            halo_expand = 10,
            halo_blur = 20,
            halo_alpha = .8,
            seed = 123,
            heightmap = elmat,
            data_label_column = "city"
        )
    ) |>
    rayshader::plot_3d(
        elmat,
        zscale = 25,
        solid = F,
        shadow = T,
        shadow_darkness = 1,
        background = "white",
        windowsize = c(
            w / 8, h / 8
        ),
        zoom = .525,
        phi = 89,
        theta = 0
    )

# rayshader::render_camera(
#     phi = 87,
#     zoom = .525
# )

# 10. RENDER OBJECT
#-----------------

filename <- "3d_land_cover_italy.png"

rayshader::render_highquality(
    filename = filename,
    preview = T,
    light = T,
    environment_light = "D:/forest2019/air_museum_playground_4k.hdr",
    intensity_env = .5,
    rotate_env = 90,
    interactive = F,
    parallel = T,
    width = w,
    height = h
)

# 11. PUT EVERYTHING TOGETHER


legend_name <- "italy_land_cover_legend.png"
png(legend_name, width = 1200, height = 1200)
par(family = "mono")

plot(
    NULL,
    xaxt = "n",
    yaxt = "n",
    bty = "n",
    ylab = "",
    xlab = "",
    xlim = 0:1,
    ylim = 0:1,
    xaxs = "i",
    yaxs = "i"
)
legend(
    "center",
    legend = c(
        "Water",
        "Trees",
        "Flooded Vegetation",
        "Crops",
        "Built area",
        "Bare ground",
        "Rangeland"
    ),
    pch = 16,
    cex = 2.5,
    pt.cex = 3.5,
    bty = "n",
    col = c(cols[c(1:6, 9)]),
    border = "grey20"
)
dev.off()

# filename <- "land-cover-bih-3d-b.png"

lc_img <- magick::image_read(
    filename
)

my_legend <- magick::image_read(
    legend_name
)

my_legend_scaled <- magick::image_scale(
    magick::image_background(
        my_legend, "none"
    ), 4200
) |>
    magick::image_transparent("white")

p <- magick::image_composite(
    magick::image_scale(
        lc_img, "x5000"
    ),
    my_legend_scaled,
    gravity = "southwest",
    offset = "+0+2000"
)

magick::image_write(
    p, "3d-italy-land-cover.png"
)

# ©2024 Milos Popovic (https://milospopovic.net) | Data:  Sentinel-2 10m Land Use/Land Cover – Esri, Impact Observatory, and Microsoft

# CO2 EMMISSIONS

setwd("D:/alps")

co2_names <- list.files(
    path = getwd(),
    pattern = "CO2",
    full.names = T
)

co2_rasters <- lapply(
    co2_names,
    terra::rast
)

# 3. CROP
#--------

country_co2_rasters <-
    lapply(
        co2_rasters,
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

co2_change <- (
    (country_co2_rasters[[2]] - country_co2_rasters[[1]])
)

co2_change[co2_change < 0] <- -1
# co2_change[co2_change == 0] <- 0
co2_change[co2_change > 0] <- 1

terra::plot(co2_change)

cols <- rev(c(
    "#EB38A0",
    "#38DDEB"
))

from <- c(-1, 1)
to <- t(col2rgb(
    cols
))

co2_change_image <- terra::subst(
    co2_change,
    from,
    to,
    names = cols
)

co2_resampled <- terra::resample(
    x = co2_change_image,
    y = elev_lambert,
    method = "near"
) |>
    terra::project(crs_lambert)

terra::plotRGB(co2_resampled)

img_file <- "co2-image-resampled_italy.png"

terra::writeRaster(
    co2_resampled,
    img_file,
    overwrite = T,
    NAflag = 255
)

img <- png::readPNG(img_file)

h <- nrow(elev_reproj)
w <- ncol(elev_reproj)

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
    rayshader::add_overlay(
        rayshader::generate_label_overlay(
            centroids,
            extent = elev_reproj,
            text_size = 8,
            point_size = 0,
            color = "black",
            halo_color = "white",
            halo_expand = 10,
            halo_blur = 20,
            halo_alpha = .8,
            seed = 123,
            heightmap = elmat,
            data_label_column = "city"
        )
    ) |>
    rayshader::plot_3d(
        elmat,
        zscale = 25,
        solid = F,
        shadow = T,
        shadow_darkness = 1,
        background = "white",
        windowsize = c(
            w / 8, h / 8
        ),
        zoom = .525,
        phi = 89,
        theta = 0
    )

start_time <- Sys.time()
filename <- "italy-CO2-change-1992-2022.png"

rayshader::render_highquality(
    filename = filename,
    preview = T,
    light = F,
    environment_light = "D:/forest2019/air_museum_playground_4k.hdr",
    intensity_env = .6,
    rotate_env = 90,
    interactive = F,
    parallel = T,
    width = w,
    height = h
)

end_time <- Sys.time()
end_time - start_time

legend_name <- "co2_legend.png"
png(legend_name, width = 800, height = 800)
par(family = "sans")

plot(
    NULL,
    xaxt = "n",
    yaxt = "n",
    bty = "n",
    ylab = "",
    xlab = "",
    xlim = 0:1,
    ylim = 0:1,
    xaxs = "i",
    yaxs = "i"
)
legend(
    "center",
    legend = rev(c(
        "Increase",
        "Decrease"
    )),
    pch = 16,
    cex = 3,
    pt.cex = 4,
    bty = "n",
    col = cols,
    border = "grey20"
)
dev.off()

lc_img <- magick::image_read(
    filename
)

my_legend <- magick::image_read(
    legend_name
)

my_legend_scaled <- magick::image_scale(
    magick::image_background(
        my_legend, "none"
    ), 3000
) |>
    magick::image_transparent("white")

p <- magick::image_composite(
    magick::image_scale(
        lc_img, "x5000"
    ),
    my_legend_scaled,
    gravity = "southeast",
    offset = "+2500+2000"
)

magick::image_write(
    p, "3d-co2-italy.png"
)
