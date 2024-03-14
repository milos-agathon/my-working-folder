#############################################
# 3D built-up map with R
# Milos Popovic 2023/04/23
#############################################

setwd("/Users/mpopovic3/Downloads")

# 1. PACKAGES

if (
    !require("pacman")
) {
    install.packages("pacman")
}

pacman::p_load(
    geodata,
    elevatr,
    terra, sf,
    rayshader,
    tidyverse,
    magick
)

# 2. GET COUNTRY MAP
#-------------------

# all regions
regions_sf <- geodata::gadm(
    country = "ITA",
    level = 1,
    path = getwd()
) |>
    sf::st_as_sf()

plot(sf::st_geometry(regions_sf))
print(regions_sf, n = 20)

# region of interest
region_sf <- subset(regions_sf, NAME_1 == "Sicily")
plot(sf::st_geometry(region_sf))

# country
country_sf <- sf::st_union(regions_sf)
plot(sf::st_geometry(country_sf))

# 3. DEM
#-------

elev <- elevatr::get_elev_raster(
    locations = region_sf,
    z = 9, clip = "locations"
)

crs_lambert <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

elev_lambert <- elev |>
    terra::rast() |>
    terra::project(crs_lambert)

elmat <- rayshader::raster_to_matrix(
    elev_lambert
)

# 4. RENDER SCENE
#----------------

colors <- hcl.colors(
    n = 7,
    palette = "Zissou 1"
)

texture <- colorRampPalette(
    colors
)(512)

h <- nrow(elev_lambert)
w <- ncol(elev_lambert)

elmat |>
    rayshader::height_shade(
        texture = texture
    ) |>
    rayshader::plot_3d(
        elmat,
        zscale = 50,
        solid = FALSE,
        shadow = TRUE,
        shadow_darkness = 1,
        background = "white",
        windowsize = c(
            w / 6, h / 6
        ),
        zoom = .5,
        phi = 87,
        theta = 0
    )

rayshader::render_camera(
    phi = 89,
    zoom = .7
)

# 5. RENDER OBJECT
#-----------------

u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/photo_studio_loft_hall_4k.hdr"
# u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/lebombo_4k.hdr"
hdri_file <- basename(u)

download.file(
    url = u,
    destfile = hdri_file,
    mode = "wb"
)

filename <- "3d-dem-sicily.png"

rayshader::render_highquality(
    filename = filename,
    preview = TRUE,
    light = TRUE,
    environment_light = hdri_file,
    intensity_env = 1,
    interactive = F,
    width = w,
    height = h
)

# 6. BOUNDING BOX
#----------------

region_bbox <- sf::st_as_sfc(
    sf::st_bbox(
        region_sf
    )
)

country_bbox <- sf::st_as_sfc(
    sf::st_bbox(
        country_sf
    )
)

# 7. INSET MAP
#-------------

inset_map <- ggplot() +
    geom_sf(
        data = country_sf,
        color = "white",
        fill = "grey80"
    ) +
    geom_sf(
        data = country_bbox,
        fill = "transparent",
        color = "grey80",
        linewidth = 1
    ) +
    geom_sf(
        data = region_bbox,
        fill = "transparent",
        color = "red",
        linewidth = 1.5
    ) +
    theme_void()

print(inset_map)

file_name <- "inset_map.png"

ggsave(
    filename = file_name,
    plot = inset_map,
    width = 6,
    height = 8,
    units = "in"
)

# 8. PUT MAPS TOGETHER
#---------------------

inset_map_img <- magick::image_read(file_name)
region_dem_img <- magick::image_read(filename)

inset_map_rescaled_img <- magick::image_scale(
    inset_map_img, "x1200"
)

final_img <- magick::image_composite(
    region_dem_img,
    inset_map_rescaled_img,
    gravity = "southeast",
    offset = "+0+0"
)

magick::image_write(
    final_img,
    "final_img.png"
)
