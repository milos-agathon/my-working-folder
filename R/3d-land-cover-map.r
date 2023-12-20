
setwd("/Users/mpopovic3/Downloads/india")

# 1. PACKAGES

libs <- c(
    "terra",
    "giscoR",
    "sf",
    "tidyverse",
    "ggtern",
    "elevatr",
    "png",
    "rayshader",
    "magick"
)

installed_libraries <- libs %in% rownames(
    installed.packages()
)

if(any(installed_libraries == F)){
    install.packages(
        libs[!installed_libraries]
    )
}

invisible(
    lapply(
        libs, library, character.only = T
    )
)

# 2. COUNTRY BORDERS

# 1. GET INDIAN STATES

url <- "https://github.com/AnujTiwari/India-State-and-Country-Shapefile-Updated-Jan-2020/archive/refs/heads/master.zip"
download.file(
    url,
    basename(url),
    mode = "wb"
)

unzip("master.zip")

country_sf <- sf::st_read(
    "India-State-and-Country-Shapefile-Updated-Jan-2020-master/India_State_Boundary.shp"
) |>
sf::st_union()

plot(sf::st_geometry(country_sf))

# 3 DOWNLOAD ESRI LAND COVER TILES

urls <- c(
    "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/42S_20220101-20230101.tif",
    "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/43S_20220101-20230101.tif",
    "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/44S_20220101-20230101.tif",
    "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/42R_20220101-20230101.tif",
    "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/43R_20220101-20230101.tif",
    "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/44R_20220101-20230101.tif",
    "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/45R_20220101-20230101.tif",
    "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/46R_20220101-20230101.tif",
    "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/47R_20220101-20230101.tif",
    "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/42Q_20220101-20230101.tif",
    "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/43Q_20220101-20230101.tif",
    "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/44Q_20220101-20230101.tif",
    "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/45Q_20220101-20230101.tif",
    "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/46Q_20220101-20230101.tif",
    "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/43P_20220101-20230101.tif",
    "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/44P_20220101-20230101.tif",
    "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/46P_20220101-20230101.tif",
    "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/46N_20220101-20230101.tif"
)

for(url in urls){
    download.file(
        url = url,
        destfile = basename(url),
        mode = "wb"
    )
}

# 4 LOAD TILES

raster_files <- list.files(
    path = getwd(),
    pattern = "20230101.tif$",
    full.names = T
)

crs <- "+proj=lcc +lat_1=32.5 +lat_0=32.5 +lon_0=68 +k_0=0.99878641 +x_0=2743195.5 +y_0=914398.5 +a=6377299.151 +rf=300.8017255 +towgs84=295,736,257,0,0,0,0 +units=m +no_defs +type=crs"

for(raster in raster_files){
    rasters <- terra::rast(raster)

    country <- country_sf |>
        sf::st_transform(
            crs = terra::crs(
                rasters
            )
        )

    land_cover <- terra::crop(
        rasters,
        terra::vect(
            country
        ),
        snap = "in",
        mask = T
    ) |>
    terra::aggregate(
        fact = 5,
        fun = "modal"
    ) |>
    terra::project(crs)

    terra::writeRaster(
        land_cover,
        paste0(
            raster,
            "_india",
            ".tif"
        )
    )
}

# 5 LOAD VIRTUAL LAYER

r_list <- list.files(
    path = getwd(),
    pattern = "_india.tif",
    full.names = T
)

land_cover_vrt <- terra::vrt(
    r_list,
    "india_land_cover_vrt.vrt",
    overwrite = T
)

# 6 FETCH ORIGINAL COLORS

ras <- terra::rast(
    raster_files[[1]]
)

raster_color_table <- do.call(
    data.frame,
    terra::coltab(ras)
)

head(raster_color_table)

hex_code <- ggtern::rgb2hex(
    r = raster_color_table[,2],
    g = raster_color_table[,3],
    b = raster_color_table[,4]
)

# 7 ASSIGN COLORS TO RASTER

cols <- hex_code[c(2:3, 5:6, 8:12)]

from <- c(1:2, 4:5, 7:11)
to <- t(col2rgb(cols))
land_cover_vrt <- na.omit(land_cover_vrt)

land_cover_india <- terra::subst(
    land_cover_vrt,
    from = from,
    to = to,
    names = cols
)

terra::plotRGB(land_cover_india)

# 8 DIGITAL ELEVATION MODEL

elev <- elevatr::get_elev_raster(
    locations = country_sf,
    z = 7, clip = "locations"
)

elev_lambert <- elev |>
    terra::rast() |>
    terra::project(crs)


land_cover_india_resampled <- terra::resample(
    x = land_cover_india,
    y = elev_lambert,
    method = "near"
) |>
terra::project(crs)

terra::plotRGB(land_cover_india_resampled)

img_file <- "land_cover_india.png"

terra::writeRaster(
    land_cover_india_resampled,
    img_file,
    overwrite = T,
    NAflag = 255
)

img <- png::readPNG(img_file)

# 9. RENDER SCENE
#----------------



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
    zoom = .55
)

# 10. RENDER OBJECT
#-----------------

filename <- "3d_land_cover_india-dark.png"

rayshader::render_highquality(
    filename = filename,
    preview = T,
    light = F,
    environment_light = "air_museum_playground_4k.hdr",
    intensity_env = .85,
    rotate_env = 90,
    interactive = F,
    parallel = T,
    width = w,
    height = h
)

# 11. PUT EVERYTHING TOGETHER

c(
    "#419bdf", "#397d49", "#7a87c6", 
    "#e49635", "#c4281b", "#a59b8f", 
    "#a8ebff", "#616161", "#e3e2c3"
)

legend_name <- "land_cover_legend.png"
png(legend_name)
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
        "Crops",
        "Built area",
        "Rangeland"
    ),
    pch = 15,
    cex = 2,
    pt.cex = 1,
    bty = "n",
    col = c(cols[c(1:2, 4:5, 9)]),
    fill = c(cols[c(1:2, 4:5, 9)]),
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
    ), 2500
)

p <- magick::image_composite(
    magick::image_scale(
        lc_img, "x7000" 
    ),
    my_legend_scaled,
    gravity = "southwest",
    offset = "+100+0"
)

magick::image_write(
    p, "3d_india_land_cover_final.png"
)
