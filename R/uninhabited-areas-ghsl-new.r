####################################################
#                 Let's map uninhabited areas with R
#                 Milos Popovic
#                 2023/08/07
####################################################
# R.home("bin")
# install.packages("jsonlite")
setwd("D:/ghsl-population")

libs <- c(
    "terra", "tidyverse", "sf",
    "rayshader"
)

installed_libs <- libs %in% rownames(
    installed.packages()
)

if(any(installed_libs == F)){
    install.packages(
        libs[!installed_libs]
    )
}

invisible(
    lapply(
        libs, library, character.only = T
    )
)

# 1. DOWNLOAD GHSL DATA
#----------------------

url <- 
"https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2020_GLOBE_R2023A_4326_30ss/V1-0/GHS_POP_E2020_GLOBE_R2023A_4326_30ss_V1_0.zip"

file_name <- "GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0.zip"

download.file(
    url = url,
    path = getwd(),
    destfile = file_name
)

# 2. LOAD GHSL DATA
#----------------------

unzip(file_name)
raster_name <- gsub(
    ".zip", ".tif",
    file_name
)

pop <- terra::rast(raster_name)

# 3. country SHAPEFILE
#-----------------------

get_country_borders <- function(){
    country <- giscoR::gisco_get_countries(
        country = "MN",
        resolution = "1"
    )

    return(country)
}

country <- get_country_borders()
plot(sf::st_geometry(country))

# 4. CROP country GHSL
#-----------------------
crs_lambert <-
    "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_frfs"

country_pop <- terra::crop(
    pop,
    terra::vect(country),
    snap = "in",
    mask = T
) 

country_pop_agg <- country_pop |>
terra::aggregate(fact = 2)
# terra::project(crs_lambert)

# 5. RASTER TO DATAFRAME
#-----------------------

country_pop_df <- as.data.frame(
    country_pop_agg,
    xy = T, na.rm = T
)


head(country_pop_df)

names(country_pop_df)[3] <- "val"
country_pop_df <- country_pop_df |>
    dplyr::mutate(
        cat = dplyr::if_else(
            val > 0, "Yes", "No"
        )
    )

country_pop_df$cat <- as.factor(
    country_pop_df$cat
)

# 6. MAP
#-------

cols <- c("#0a1c29", "#edc241")

p <- ggplot() +
    # geom_sf(
    #     data = country, fill = NA,
    #     color = "#2b7ab3", size = .15
    # ) +
    geom_raster(
        data = country_pop_df,
        aes(x = x,
            y = y,
            fill = cat
        )
    ) +
    scale_fill_manual(
        name = "Are there any people?",
        values = cols,
        na.value = "#0a1c29"
    ) +
    guides(
        fill = guide_legend(
            direction = "horizontal",
            keyheight = unit(5, "mm"),
            keywidth = unit(20, "mm"),
            label.position = "bottom",
            label.hjust = .5,
            nrow = 1,
            byrow = T
        )
    ) +
    # coord_sf(crs = crs_lambert) +
    theme_void() +
    theme(
        legend.position = "top",
        legend.title = element_text(
            size = 26, color = "grey10"
        ),
        legend.text = element_text(
            size = 24, color = "grey10"
        ),
        plot.caption = element_text(
            size = 20, color = "grey10",
            hjust = .25, vjust = 15
        ),
        plot.margin = unit(
            c(
                t = 0, b = -1,
                l = -1, r = -1
            ), "lines"
        )
    ) +
    labs(
        title = "",
        caption = "Data: Global Human Settlement Layer at 3 arcsec"
    )

w <- ncol(country_pop)
h <- nrow(country_pop)

ggsave(
    "mongolia-uninhabited.png",
    p, width = w / 5, height = h / 5,
    units = "px", bg = "white"
)