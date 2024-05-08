#################################
# Map temperature anomaly with R
# Milos Popovic 2024-05-03
#################################

setwd("/Users/mpopovic3/Downloads/temperature")


# 1. PACKAGES
#------------

pacman::p_load(
    KrigR,
    giscoR,
    terra,
    elevatr,
    tidyverse,
    sf
)



# 2. QUERY TEMPERATURE DATA
#---------------------------
url <- "https://data.giss.nasa.gov/pub/gistemp/gistemp1200_GHCNv4_ERSSTv5.nc.gz"
destfile <- basename(url)

download.file(
    url = url,
    destfile = destfile,
    mode = "wb"
)

R.utils::gunzip(
    filename = destfile
)

temp_anomaly <- terra::rast(
    gsub(
        ".gz",
        "",
        destfile
    )
)

# 3. WORLD SHAPEFILE
#----------------

world_sf <- giscoR::gisco_get_countries()

plot(sf::st_geometry(world_sf))

# 4. JANUARY 2024 ANOMALY
#------------------------

temp_anomaly_jan2024 <- terra::subset(
    temp_anomaly,
    time(temp_anomaly) == as.Date("2024-01-15")
)

terra::plot(temp_anomaly_jan2024)
plot(sf::st_geometry(world_sf), add = TRUE)

robinson_proj <- "+proj=robin +over"
temp_anomaly_jan2024_proj <- terra::project(
    temp_anomaly_jan2024,
    robinson_proj
)

world_sf_proj <- world_sf |>
    sf::st_transform(
        crs = robinson_proj
    ) |>
    sf::st_union()

world_bb <- sf::st_union(
    sf::st_make_grid(
        sf::st_bbox(
            world_sf
        ),
        n = 100
    )
) |>
    sf::st_transform(crs = robinson_proj)

temp_anomaly_jan2024_proj <- terra::crop(
    temp_anomaly_jan2024_proj,
    world_bb
)

terra::plot(temp_anomaly_jan2024_proj)
plot(sf::st_geometry(world_sf_proj), add = TRUE)
plot(sf::st_geometry(world_bb), add = TRUE)

# 5. MAP MARCH 2024 ANOMALY
#--------------------------
breaks <- classInt::classIntervals(
    terra::values(
        temp_anomaly_jan2024_proj
    ),
    n = 7,
    style = "equal"
)$brks

theme_for_the_win <- function() {
    theme_void() +
        theme(
            legend.position = "right",
            legend.title = element_text(
                size = 20, color = "grey20"
            ),
            legend.text = element_text(
                size = 15, color = "grey20"
            ),
            plot.title = element_text(
                size = 30, color = "grey40",
                hjust = .5, vjust = -1
            ),
            plot.caption = element_text(
                size = 10, color = "grey40",
                hjust = .5, vjust = 10
            ),
            plot.margin = unit(
                c(
                    t = -1, b = -1,
                    l = -1, r = .5
                ), "lines"
            )
        )
}

map1 <- ggplot() +
    tidyterra::geom_spatraster(
        data = temp_anomaly_jan2024_proj
    ) +
    geom_sf(
        data = world_sf_proj,
        color = "grey20",
        linewidth = .5,
        fill = "transparent"
    ) +
    geom_sf(
        data = world_bb,
        color = "grey20",
        linewidth = .5,
        fill = "transparent"
    ) +
    scale_fill_gradient2(
        name = "°C",
        low = "#009B9F",
        mid = "white",
        high = "#C75DAA",
        midpoint = 0,
        breaks = breaks,
        labels = round(breaks, 0),
        na.value = "white",
    ) +
    guides(
        fill = guide_colorbar(
            direction = "vertical",
            barheight = unit(60, units = "mm"),
            barwidth = unit(3, units = "mm"),
            title.position = "top",
            title.hjust = .5,
            label.hjust = 0,
            byrow = FALSE,
            label.position = "left"
        )
    ) +
    theme_for_the_win() +
    labs(
        title = "Temperature anomaly for January 2024",
        caption = "©2024 Milos Popovic (https://milospopovic.net\nData: Land-Ocean Temperature Index, ERSSTv5, 1200km smoothing"
    )

w <- ncol(temp_anomaly_jan2024_proj)
h <- nrow(temp_anomaly_jan2024_proj)

ggsave(
    "temperature_anomaly_jan2024.png",
    map1,
    width = w * 20,
    height = h * 20,
    units = "px",
    bg = "white"
)

# 6. 2023 ANOMALY
#----------------

temp_anomaly_2023 <- temp_anomaly |>
    terra::subset(
        time(temp_anomaly) >= as.Date("2023-01-15") &
            time(temp_anomaly) <= as.Date("2023-12-15")
    ) |>
    terra::app(fun = mean)

temp_anomaly_2023_proj <- terra::project(
    temp_anomaly_2023,
    robinson_proj
)

breaks <- classInt::classIntervals(
    terra::values(
        temp_anomaly_2023_proj
    ),
    n = 5,
    style = "equal"
)$brks

map2 <- ggplot() +
    tidyterra::geom_spatraster(
        data = temp_anomaly_2023_proj
    ) +
    geom_sf(
        data = world_sf_proj,
        color = "grey20",
        linewidth = .5,
        fill = "transparent"
    ) +
    geom_sf(
        data = world_bb,
        color = "grey20",
        linewidth = .5,
        fill = "transparent"
    ) +
    scale_fill_gradient2(
        name = "°C",
        low = "#009B9F",
        mid = "white",
        high = "#C75DAA",
        midpoint = 0,
        breaks = breaks,
        labels = round(breaks, 0),
        na.value = "white",
    ) +
    guides(
        fill = guide_colorbar(
            direction = "vertical",
            barheight = unit(60, units = "mm"),
            barwidth = unit(3, units = "mm"),
            title.position = "top",
            title.hjust = .5,
            label.hjust = 0,
            byrow = FALSE,
            label.position = "left"
        )
    ) +
    theme_for_the_win() +
    labs(
        title = "Temperature anomaly for 2023",
        caption = "©2024 Milos Popovic (https://milospopovic.net\nData: Land-Ocean Temperature Index, ERSSTv5, 1200km smoothing"
    )

w <- ncol(temp_anomaly_2023_proj)
h <- nrow(temp_anomaly_2023_proj)

ggsave(
    "temperature_anomaly_2023.png",
    map2,
    width = w * 20,
    height = h * 20,
    units = "px",
    bg = "white"
)
















# MONTHLY AVERAGES

url <- "https://downloads.psl.noaa.gov/Datasets/ghcncams/air.mon.mean.nc"

destfile <- basename(url)

download.file(
    url = url,
    destfile = destfile,
    mode = "wb"
)

temp_monthly_mean <- terra::rast(
    destfile
)

temp_monthly_mean <- temp_monthly_mean - 273.15

temp_monthly_mean <- terra::rotate(temp_monthly_mean)

temp_annual_mean <- temp_monthly_mean |>
    terra::subset(
        time(temp_monthly_mean) >= as.Date("1991-01-01") &
            time(temp_monthly_mean) <= as.Date("2020-12-01")
    ) |>
    terra::app(fun = mean)

temp_2023_mean <- temp_monthly_mean |>
    terra::subset(
        time(temp_monthly_mean) >= as.Date("2023-01-01") &
            time(temp_monthly_mean) <= as.Date("2023-12-01")
    ) |>
    terra::app(fun = mean)

temp_anomaly <- temp_2023_mean - temp_annual_mean
temp_anomaly_proj <- terra::project(
    temp_anomaly, robinson_proj
)

terra::plot(
    temp_anomaly_proj
)
plot(sf::st_geometry(world_sf_proj), add = TRUE)

# What is the temperature anomaly for a specific month?

temp_jan <- raster::subset(
    temp_monthly_mean,
    format.Date(
        time(
            temp_monthly_mean
        ), "%m"
    ) == "03"
)

temp_jan_mean <- temp_jan |>
    terra::subset(
        time(temp_jan) >= as.Date("1991-03-01") &
            time(temp_jan) <= as.Date("2020-03-01")
    ) |>
    terra::app(fun = mean)


temp_2024_jan_mean <- temp_jan |>
    terra::subset(
        time(temp_jan) == as.Date("2024-03-01")
    ) |>
    terra::app(fun = mean)

temp_jan_anomaly <- temp_2024_jan_mean - temp_jan_mean
temp_jan_anomaly_proj <- terra::project(
    temp_jan_anomaly, robinson_proj
)

terra::plot(
    temp_jan_anomaly_proj
)
plot(sf::st_geometry(world_sf_proj), add = TRUE)
