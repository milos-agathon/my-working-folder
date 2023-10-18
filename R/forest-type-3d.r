#############################################
# 3D forest height with R
# Milos Popovic 2023/07/18
#############################################
setwd("/Users/mpopovic3/Downloads")

# devtools::install_version(
#     "ggplot2", version = "3.3.6",
#     repos = "http://cran.us.r-project.org")

libs <- c(
    "tidyverse", "sf", "geodata",
    "terra", "classInt", "rayshader",
    "terrainr"
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

forest_type <- terra::rast(
    "E000N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Forest-Type-layer_EPSG-4326.tif"
)

# lc <- terra::rast("E000N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif")
# lcc <- terra::crop(
#     forest_type,
#     country_sf,
#     snap = "in",
#     mask = T
# )

# terra::plot(lc)
# coltb <- terra::coltab(lc)
# values(lc) <- as.integer(values(lc))
# coltb$value <- as.integer(coltb$value)
# coltab(lc) <- coltb

# val <- values(lc)
# unique(val)

ggplot() +
    tidyterra::geom_spatraster(data = lcc) +
    scale_fill_viridis_c()

# 1. COUNTRY SF
#--------------

country_sf <- giscoR::gisco_get_countries(
    country = "CH",
    resolution = "1"
)

# 2. DOWNLOAD ETH DATA
#---------------------

# raster_files <-
#     list.files(
#         path = getwd(),
#         pattern = ".tif$",
#         full.names = T
#     )

# 3. LOAD FOREST
#---------------

# forest_list <- lapply(
#     raster_files,
#     terra::rast
# )

# country_forest_list <- lapply(
#     forest_list,
#     function(x) {
#         terra::crop(
#             x,
#             terra::vect(
#                 country_sf
#             ),
#             snap = "in",
#             mask = T
#         )
#     }
# )

# forest_mosaic <- do.call(
#     terra::mosaic,
#     country_forest_list
# ) |>
#     terra::aggregate(
#         fact = 10
#     )

# terra::plot(forest_mosaic)
# plot(sf::st_geometry(country_sf), add = T)


from <- 0:5
cols <- hcl.colors(n = 6, palette = "viridis")
to <- t(col2rgb(cols))
# to <- t(col2rgb(c("red", "blue", "green")))

r <- terra::subst(
    lcc, from, to,
    names = cols
)

img_file <- "ch_forest.png"
writeRaster(r, img_file, overwrite=TRUE)
img <- png::readPNG(img_file)

terra::plotRGB(r)
ar <- as.array(r)
class(ar)
str(ar)
# mat <- rayshader::raster_to_matrix(r)

# raster::writeRaster(
#     x = forest_height_mosaic,
#     filename = "iberia-forest-height.tif"
# )

elev <- elevatr::get_elev_raster(
    locations = country_sf,
    z = 9, clip = "locations"
)

elmat <- rayshader::raster_to_matrix(elev)

h <- nrow(r)
w <- ncol(r)

elmat |>
    rayshader::height_shade() |>
    rayshader::add_overlay(
        img,
        alphalayer = .99
    ) |>
    rayshader::plot_3d(
        elmat,
        zscale = 5,
        solid = F,
        shadowdepth = 0,
        shadow = T,
        shadow_darkness = 1,
        background = "white", # cols[1]
        sunangle = 225, # 225
        windowsize = c(w / 10, h / 10),
        zoom = .65,
        phi = 80,
        theta = 0
    )

rayshader::render_camera(
    phi = 80,
    zoom = .625,
    theta = 5
)



# GGPLOT2
#--------

elev_df <- elev |>
    as.data.frame(
        xy = T
    )

head(elev_df)
names(elev_df)[3] <- "dem"
class(elev_df$dem)
rdf <- r |>
    as.data.frame(
        xy = T
    )

head(rdf)
names(rdf)[3] <- "forest"

ggplot() +
    terrainr::geom_spatial_rgb(
        data = rdf,
        mapping = aes(
            x = x,
            y = y,
            r = red,
            g = green,
            b = blue
        )
    ) +
    geom_raster(
        data = elev_df,
        aes(
            x = x,
            y = y,
            alpha = dem
        )
    ) +
    scale_alpha_gradientn(
        name = "height (m)",
        colors = hcl.colors(8, "Grays")
    ) +
    guides(
        fill = guide_legend(
            direction = "vertical",
            keyheight = unit(5, "mm"),
            keywidth = unit(5, "mm"),
            title.position = "top",
            label.position = "right",
            title.hjust = .5,
            label.hjust = .5,
            ncol = 1,
            byrow = F
        )
    ) +
    theme_minimal() +
    theme(
        axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = c(0.9, .3),
        legend.title = element_text(
            size = 11, color = "grey10"
        ),
        legend.text = element_text(
            size = 10, color = "grey10"
        ),
        panel.grid.major = element_line(
            color = "white"
        ),
        panel.grid.minor = element_line(
            color = "white"
        ),
        plot.background = element_rect(
            fill = "white",
            color = NA
        ),
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











# 4. RASTER TO DATAFRAME
#-----------------------

forest_height_iberia_df <- forest_height_iberia |>
    as.data.frame(
        xy = T
    )

head(forest_height_iberia_df)
names(forest_height_iberia_df)[3] <- "height"

# 5. BREAKS
#----------

breaks <- classInt::classIntervals(
    forest_height_iberia_df$height,
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
)(6)

# 7. GGPLOT2
#-----------

p <- ggplot(
    forest_height_iberia_df
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
        breaks = round(breaks, 0)
    ) +
    coord_sf(crs = crs_lambert) +
    guides(
        fill = guide_legend(
            direction = "vertical",
            keyheight = unit(5, "mm"),
            keywidth = unit(5, "mm"),
            title.position = "top",
            label.position = "right",
            title.hjust = .5,
            label.hjust = .5,
            ncol = 1,
            byrow = F
        )
    ) +
    theme_minimal() +
    theme(
        axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = c(0.9, .3),
        legend.title = element_text(
            size = 11, color = "grey10"
        ),
        legend.text = element_text(
            size = 10, color = "grey10"
        ),
        panel.grid.major = element_line(
            color = "white"
        ),
        panel.grid.minor = element_line(
            color = "white"
        ),
        plot.background = element_rect(
            fill = "white",
            color = NA
        ),
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

h <- nrow(forest_height_iberia)
w <- ncol(forest_height_iberia)

rayshader::plot_gg(
    ggobj = p,
    width = w / 1000,
    height = h / 1000,
    scale = 150,
    solid = F,
    # solidcolor = "white",
    # solidlinecolor = "white",
    # soliddepth = 0,
    shadow = T,
    shadowcolor = "white",
    shadow_intensity = 1,
    background = "white",
    offset_edges = F,
    sunangle = 315,
    window.size = c(800, 800),
    zoom = .5,
    phi = 30,
    theta = -30,
    multicore = T
)

rayshader::render_camera(
    phi = 80,
    zoom = .625,
    theta = 5
)

# 9. RENDER OBJECT
#-----------------

rayshader::render_highquality(
    filename = "iberia-forest-height-2020.png",
    preview = T,
    parallel = T,
    interactive = F,
    light = T,
    lightdirection = c(
        315, 320, 315, 320
    ),
    lightintensity = c(
        1200, 1750, 600, 700
    ),
    lightaltitude = c(
        15, 15, 80, 80
    ),
    ground_material =
        rayrender::microfacet(
            roughness = .6
        ),
    width = 5000,
    height = 5000
)
