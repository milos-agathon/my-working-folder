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
        dependencies = TRUE
    )
}

invisible(
    lapply(
        libs, library,
        character.only = TRUE
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
                mask = TRUE
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
    overwrite = TRUE,
    NAflag = 255
)

img <- png::readPNG("D:/alps/italy-pop-image-resampled.png")

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
            c(
                "grey90",
                "grey50"
            )
        )(512)
    ) |>
    rayshader::add_overlay(
        img,
        alphalayer = 1,
        alphacolor = "white"
    ) |>
    # rayshader::add_overlay(
    #     rayshader::generate_label_overlay(
    #         centroids,
    #         extent = elev_reproj,
    #         text_size = 8,
    #         point_size = 0,
    #         color = "black",
    #         halo_color = "white",
    #         halo_expand = 8,
    #         halo_blur = 17,
    #         halo_alpha = .9,
    #         seed = 123,
    #         heightmap = elmat,
    #         data_label_column = "city"
    #     )
    # ) |>
    rayshader::plot_3d(
        elmat,
        zscale = 25,
        solid = FALSE,
        shadow = FALSE,
        shadow_darkness = 1,
        background = "white",
        windowsize = c(
            w / 8, h / 8
        ),
        zoom = .62,
        phi = 89,
        theta = 0
    )

# rayshader::render_camera(
#     zoom = .62
# )

start_time <- Sys.time()
filename <- "italy-population-change.png"

rayshader::render_highquality(
    filename = filename,
    preview = TRUE,
    light = FALSE,
    environment_light = "D:/forest2019/air_museum_playground_4k.hdr",
    intensity_env = .6,
    rotate_env = 90,
    interactive = FALSE,
    parallel = TRUE,
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
        "Crescita",
        "Aree Disabitate",
        "Diminuzione"
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
    p, "3d-italy-population-change.png"
)

# Variazione della Popolazione (2010-2020)

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
            c(
                "grey90",
                "grey50"
            )
        )(512)
    ) |>
    rayshader::add_overlay(
        img,
        alphalayer = 1,
        alphacolor = "white"
    ) |>
    # rayshader::add_overlay(
    #     rayshader::generate_label_overlay(
    #         centroids,
    #         extent = elev_lambert,
    #         text_size = 8,
    #         point_size = 0,
    #         color = "black",
    #         halo_color = "white",
    #         halo_expand = 8,
    #         halo_blur = 17,
    #         halo_alpha = .9,
    #         seed = 123,
    #         heightmap = elmat,
    #         data_label_column = "city"
    #     )
    # ) |>
    rayshader::plot_3d(
        elmat,
        zscale = 25,
        solid = FALSE,
        shadow = FALSE,
        shadow_darkness = 1,
        background = "white",
        windowsize = c(
            w / 8, h / 8
        ),
        zoom = .62,
        phi = 89,
        theta = 0
    )

# rayshader::render_camera(
#     zoom = .62
# )

start_time <- Sys.time()
filename <- "italy-land-cover.png"

rayshader::render_highquality(
    filename = filename,
    preview = TRUE,
    light = FALSE,
    environment_light = "D:/forest2019/air_museum_playground_4k.hdr",
    intensity_env = .6,
    rotate_env = 90,
    interactive = FALSE,
    parallel = TRUE,
    width = w,
    height = h
)

end_time <- Sys.time()
end_time - start_time

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
        "Acqua",
        "Foreste",
        "Vegetazione delle Aree Alluvionali",
        "Superficie Coltivata",
        "Aree Urbane",
        "Terreno Scoperto",
        "Pascoli"
    ),
    pch = 16,
    cex = 2.5,
    pt.cex = 3.5,
    bty = "n",
    col = c(cols[c(1:6, 9)]),
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
    gravity = "southwest",
    offset = "+1700+2100"
)

magick::image_write(
    p, "3d-italy-land-cover.png"
)

# Utilizzo del Suolo - 2022

# CO2 EMMISSIONS

setwd("D:/alps")

co2_names <- list.files(
    path = getwd(),
    pattern = "CO2_",
    full.names = TRUE
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
                mask = TRUE
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
    overwrite = TRUE,
    NAflag = 255
)

img <- png::readPNG(img_file)

h <- nrow(elev_reproj)
w <- ncol(elev_reproj)

elmat |>
    rayshader::height_shade(
        texture = colorRampPalette(
            c(
                "grey90",
                "grey50"
            )
        )(512)
    ) |>
    rayshader::add_overlay(
        img,
        alphalayer = 1,
        alphacolor = "white"
    ) |>
    # rayshader::add_overlay(
    #     rayshader::generate_label_overlay(
    #         centroids,
    #         extent = elev_reproj,
    #         text_size = 8,
    #         point_size = 0,
    #         color = "black",
    #         halo_color = "white",
    #         halo_expand = 8,
    #         halo_blur = 17,
    #         halo_alpha = .9,
    #         seed = 123,
    #         heightmap = elmat,
    #         data_label_column = "city"
    #     )
    # ) |>
    rayshader::plot_3d(
        elmat,
        zscale = 25,
        solid = FALSE,
        shadow = FALSE,
        shadow_darkness = 1,
        background = "white",
        windowsize = c(
            w / 8, h / 8
        ),
        zoom = .62,
        phi = 89,
        theta = 0
    )

# rayshader::render_camera(
#     zoom = .62
# )

start_time <- Sys.time()
filename <- "italy-co2-change.png"

rayshader::render_highquality(
    filename = filename,
    preview = TRUE,
    light = FALSE,
    environment_light = "D:/forest2019/air_museum_playground_4k.hdr",
    intensity_env = .6,
    rotate_env = 90,
    interactive = FALSE,
    parallel = TRUE,
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
        "Crescita",
        "Diminuzione"
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
    p, "3d-co2-change-italy.png"
)

#  Variazioni nelle emissioni di CO2(1992-2022)

# FOREST COVER IN 2019

raster_files <- c(
    "D:/forest2019/E000N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif",
    "D:/forest2019/E000N40_PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif"
)

forest_cover_list <- lapply(
    raster_files,
    terra::rast
)

forest_cover_rasters <- lapply(
    forest_cover_list,
    function(x) {
        terra::crop(
            x,
            terra::vect(
                country_sf
            ),
            snap = "in",
            mask = TRUE
        )
    }
)

forest_cover_mosaic <- do.call(
    terra::mosaic,
    forest_cover_rasters
)

forest_cover_single <- forest_cover_mosaic

forest_cover_single[
    forest_cover_single == 111 |
        forest_cover_single == 112 |
        forest_cover_single == 113 |
        forest_cover_single == 114 |
        forest_cover_single == 115 |
        forest_cover_single == 116 |
        forest_cover_single == 121 |
        forest_cover_single == 122 |
        forest_cover_single == 123 |
        forest_cover_single == 124 |
        forest_cover_single == 125 |
        forest_cover_single == 126
] <- 1

forest_cover_binary <- terra::ifel(
    forest_cover_single == 1,
    1,
    0
)

terra::plot(forest_cover_binary)

# cols <- "#205544"
cols <- "#107C11" # "#03ac13"

from <- 1
to <- t(
    col2rgb(cols)
)
forest_cover_binary <-
    na.omit(
        forest_cover_binary
    )

forest_cover_col <-
    terra::subst(
        forest_cover_binary,
        from = from,
        to = to,
        names = cols
    )

terra::plotRGB(forest_cover_col)

elev_lambert <- elev |>
    terra::rast()

forest_cover_resampled <- terra::resample(
    x = forest_cover_col,
    y = elev_lambert,
    method = "near"
) |>
    terra::project(crs_lambert)

terra::plotRGB(forest_cover_resampled)

img_file <- "forest_cover-resampled_italy.png"

terra::writeRaster(
    forest_cover_resampled,
    img_file,
    overwrite = TRUE,
    NAflag = 0
)

img <- png::readPNG(img_file)

h <- nrow(elev_reproj)
w <- ncol(elev_reproj)

elmat |>
    rayshader::height_shade(
        texture = colorRampPalette(
            c(
                "grey90",
                "grey50"
            )
        )(512)
    ) |>
    rayshader::add_overlay(
        img,
        alphalayer = 1,
        alphacolor = "white"
    ) |>
    # rayshader::add_overlay(
    #     rayshader::generate_label_overlay(
    #         centroids,
    #         extent = elev_reproj,
    #         text_size = 8,
    #         point_size = 0,
    #         color = "black",
    #         halo_color = "white",
    #         halo_expand = 8,
    #         halo_blur = 17,
    #         halo_alpha = .9,
    #         seed = 123,
    #         heightmap = elmat,
    #         data_label_column = "city"
    #     )
    # ) |>
    rayshader::plot_3d(
        elmat,
        zscale = 25,
        solid = FALSE,
        shadow = FALSE,
        shadow_darkness = 1,
        background = "white",
        windowsize = c(
            w / 8, h / 8
        ),
        zoom = .62,
        phi = 89,
        theta = 0
    )

start_time <- Sys.time()
filename <- "italy-forest-cover.png"

rayshader::render_highquality(
    filename = filename,
    preview = TRUE,
    light = FALSE,
    environment_light = "D:/forest2019/air_museum_playground_4k.hdr",
    intensity_env = .6,
    rotate_env = 90,
    interactive = FALSE,
    parallel = TRUE,
    width = w,
    height = h
)

end_time <- Sys.time()
end_time - start_time

legend_name <- "forest_legend.png"
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

# hospitals

hospitals <- read.csv(
    "http://www.datiopen.it/export/csv/Mappa-degli-ospedali-in-Italia.csv",
    sep = ";"
)

hospitals_sf <- hospitals |>
    sf::st_as_sf(
        coords = c(
            "Longitudine",
            "Latitudine"
        ),
        crs = 4326
    ) |>
    sf::st_transform(crs = crs_lambert)


h <- nrow(elev_reproj)
w <- ncol(elev_reproj)

elmat |>
    rayshader::height_shade(
        texture = colorRampPalette(
            c(
                "grey90",
                "grey50"
            )
        )(512)
    ) |>
    rayshader::add_overlay(
        rayshader::generate_point_overlay(
            hospitals_sf,
            color = "red",
            size = 12,
            extent = elev_reproj,
            heightmap = elmat
        )
    ) |>
    rayshader::add_overlay(
        rayshader::generate_label_overlay(
            centroids,
            extent = elev_reproj,
            text_size = 8,
            point_size = 0,
            color = "black",
            halo_color = "white",
            halo_expand = 8,
            halo_blur = 17,
            halo_alpha = .9,
            seed = 123,
            heightmap = elmat,
            data_label_column = "city"
        )
    ) |>
    rayshader::plot_3d(
        elmat,
        zscale = 25,
        solid = FALSE,
        shadow = FALSE,
        shadow_darkness = 1,
        background = "white",
        windowsize = c(
            w / 8, h / 8
        ),
        zoom = .62,
        phi = 89,
        theta = 0
    )

start_time <- Sys.time()
filename <- "3d-hospitals-italy.png"

rayshader::render_highquality(
    filename = filename,
    preview = TRUE,
    light = FALSE,
    environment_light = "D:/forest2019/air_museum_playground_4k.hdr",
    intensity_env = .6,
    rotate_env = 90,
    interactive = FALSE,
    parallel = TRUE,
    width = w,
    height = h
)

end_time <- Sys.time()
end_time - start_time

