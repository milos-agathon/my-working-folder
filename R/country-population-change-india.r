####################################################
#                 Let's map uninhabited areas with R
#                 Milos Popovic
#                 2023/08/07
####################################################
# R.home("bin")
# install.packages("jsonlite")
setwd("D:/ghsl-population")

libs <- c(
    "tidyverse", "terra",
    "sf", "giscoR"
)

installed_libs <-
    libs %in% rownames(
        installed.packages()
    )

if (
    any(installed_libs == F)
) {
    install.packages(
        libs[!installed_libs]
    )
}

invisible(
    lapply(
        libs, library,
        character.only = TRUE
    )
)

# 1. DOWNLOAD GHSL DATA
#----------------------

urls <- c(
    "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E1990_GLOBE_R2023A_4326_30ss/V1-0/GHS_POP_E1990_GLOBE_R2023A_4326_3ss_V1_0.zip",
    "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2020_GLOBE_R2023A_4326_30ss/V1-0/GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0.zip"
)

options(timeout = 300)

# file_name <- "GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0.zip"
for (url in urls) {
    download.file(
        url = url,
        path = getwd(),
        destfile = basename(url)
    )
}

lapply(basename(urls), unzip, overwrite = T)

# 2. LOAD GHSL DATA
#----------------------

file_names <- list.files(
    path = getwd(),
    pattern = "3ss_V1_0.tif$",
    full.names = T
)

pop_rasters <- lapply(
    file_names,
    terra::rast
)

# 3. COUNTRY
#------------

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
    sf::st_union() |>
    sf::st_transform(crs = 4326)

# 4. CROP
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

# 5. CALCULATE % CHANGE
#-----------------------

pop_change <- (
    country_pop_rasters[[2]] - country_pop_rasters[[1]]
)

crs_lambert <-
    "+proj=lcc +lat_1=32.5 +lat_0=32.5 +lon_0=68 +k_0=0.99878641 +x_0=2743195.5 +y_0=914398.5 +a=6377299.151 +rf=300.8017255 +towgs84=295,736,257,0,0,0,0 +units=m +no_defs +type=crs"

pop_change_agg <- pop_change |>
terra::project(crs_lambert) |>
    terra::aggregate(fact = 3)

# 6. RASTER TO DATAFRAME
#-----------------------

country_pop_df <- as.data.frame(
    pop_change_agg,
    xy = T,
    na.rm = T
)

names(country_pop_df)[3] <- "perc_change"

country_pop_df <- country_pop_df |>
    dplyr::mutate(
        cat = dplyr::case_when(
            perc_change == 0 ~ "Uninhabited",
            perc_change > 0 ~ "Growth",
            perc_change < 0 ~ "Decline",
            TRUE ~ NA
        )
    )

country_pop_df$cat <- factor(
    country_pop_df$cat,
    levels = c(
        "Growth", "Uninhabited", "Decline" 
    )
)

# 7. MAP
#-------

cols <- c(
    "#018f1d",
    "grey80",
    "#eb389f"
    
)

p <- ggplot() +
    geom_raster(
        data = country_pop_df,
        aes(
            x = x,
            y = y,
            fill = cat
        )
    ) +
    geom_sf(
        data = country_sf,
        fill = "transparent",
        color = "grey80", size = .5
    ) +
    scale_fill_manual(
        name = "",
        values = cols
    ) +
    guides(
        fill = guide_legend(
            direction = "horizontal",
            keyheight = unit(4.5, "mm"),
            keywidth = unit(45, "mm"),
            label.position = "bottom",
            label.hjust = .5,
            nrow = 1,
            byrow = T,
            drop = F
        )
    ) +
    coord_sf(
        crs = crs_lambert
    ) +
    theme_void() +
    theme(
        legend.position = c(.5, .95),
        legend.title = element_text(
            size = 35, color = "grey20"
        ),
        legend.text = element_text(
            size = 30, color = "grey20"
        ),
        plot.caption = element_text(
            size = 20, color = "grey40",
            hjust = .5, vjust = 10
        ),
        plot.margin = unit(
            c(
                t = .5, b = -3,
                l = -3, r = -3
            ), "lines"
        )
    ) +
    labs(
        caption = ""
    )

w <- ncol(pop_change_agg)
h <- nrow(pop_change_agg)

ggsave(
    "india-population-change.png",
    p,
    width = 5000,
    height = 5000,
    units = "px",
    bg = "white"
)

# ©2023 Milos Popovic (https://milospopovic.net)
# Data: Global Human Settlement Layer - population grid (R2023) at 3 arcsec
