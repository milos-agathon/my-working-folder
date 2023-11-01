######################################
# Let's visualize night lights with R
# Milos Popovic 2023/08/20
######################################

setwd("/Users/mpopovic3/Downloads/nightlight/nl")

# define libraries we need
libs <- c(
    "tidyverse", "terra", "sf",
    "giscoR"
)

# install missing libraries
installed_libraries <- libs %in% rownames(installed.packages())
if (any(installed_libraries == F)) {
    installed.packages(libs[!installed_libraries])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# 1. GET COUNTRY MAP
#-------------------

globe_lights <- terra::rast(
    "VNL_v22_npp-j01_2022_global_vcmslcfg_c202303062300.average_masked.dat.tif"
)

get_bounding_box_europe <- function() {
    xmin <- -23.6600
    xmax <- 65.5500
    ymin <- 26.5000
    ymax <- 72.0500

    bb <- sf::st_sfc(
        sf::st_polygon(list(cbind(
            c(xmin, xmax, xmax, xmin, xmin),
            c(ymin, ymin, ymax, ymax, ymin)
        ))),
        crs = 4326
    )

    return(bb)
}
bb <- get_bounding_box_europe()

# list of European countries
europeList <- function() {
    urlfile <- "https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv"
    iso2 <- read.csv(urlfile) |>
        filter(region == "Europe" |
            alpha.2 == "TR" |
            alpha.2 == "CY" |
            alpha.2 == "GE") |> # filter Europe, Cyprus and Turkey
        select("alpha.2") |>
        rename(CNTR_ID = alpha.2)
    return(iso2)
}

countries <- europeList()

# load national map of Europe
europeMap <- function() {
    europe <- giscoR::gisco_get_countries(
        year = "2016",
        epsg = "4326",
        resolution = "3",
        region = c("Europe", "Asia")
    ) |>
        mutate(FID = recode(FID, "UK" = "GB")) |>
        mutate(FID = recode(FID, "EL" = "GR"))

    eur <- europe |>
        dplyr::filter(FID %in% countries$CNTR_ID)
    return(eur)
}
eur_countries <- europeMap() |>
    sf::st_crop(sf::st_bbox(bb))

eur <- europeMap() |>
    sf::st_crop(sf::st_bbox(bb)) |>
    sf::st_union()

plot(sf::st_geometry(eur))

country_lights_list <-
    terra::crop(
        globe_lights,
        terra::vect(
            eur
        ),
        snap = "in",
        mask = T
    )

crs_lambert <-
    "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_frfs"

country_lights_reproj <-
    project(
        x = country_lights_list,
        y = crs_lambert
    )

country_lights_reproj2 <-
    terra::ifel(
        country_lights_reproj <= 0,
        NA,
        country_lights_reproj
    ) |>
    terra::aggregate(fact = 2)

# 2. NC TO DATAFRAME
#-------------------
country_lights_df <-
    as.data.frame(
        country_lights_reproj2,
        xy = T,
        na.rm = T
    )

str(country_lights_df)
names(country_lights_df) <- c("x", "y", "value")
head(country_lights_df)

# vmin <- min(country_lights_long$value)
# vmax <- max(country_lights_long$value)

# breaks <- classInt::classIntervals(
#     country_lights_long$value,
#     n = 5,
#     style = "equal"
# )$brks

cols <- c(
    "#1f4762", "#DBBA58",
    "#FFD966",
    "#FFFE66", "white")

    
pal <- colorRampPalette(cols, bias = 8)(512)
w <- ncol(country_lights_reproj2)
h <- nrow(country_lights_reproj2)

xmin <- -10.6600
xmax <- 38.5500
ymin <- 34.5000
ymax <- 71.0500

b <- sf::st_sfc(
    sf::st_polygon(list(cbind(
        c(xmin, xmax, xmax, xmin, xmin),
        c(ymin, ymin, ymax, ymax, ymin)
    ))),
    crs = 4326
) |>
    sf::st_transform(crs_lambert) |>
    sf::st_bbox()

map <- ggplot(
    data = country_lights_df
) +
    geom_tile(aes(
        x = x, y = y, fill = value
    )) +
    # geom_sf(
    #     data = eur_countries,
    #     fill = "transparent",
    #     color = "#10acdc", size = .001
    # ) +
    scale_fill_gradientn(
        name = "",
        colours = pal
    ) +
    coord_sf(
        crs = crs_lambert,
        xlim = c(b["xmin"], b["xmax"]),
        ylim = c(b["ymin"], b["ymax"]),
    ) +
    guides(
        fill = guide_colorbar(
            direction = "vertical",
            keyheight = unit(1.5, units = "mm"),
            keywidth = unit(10, units = "mm"),
            title.position = "top",
            label.position = "right",
            title.hjust = .5,
            label.hjust = .1,
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
        axis.ticks = element_blank(),
        legend.position = "none",
        legend.title = element_text(
            size = 11, color = "grey10"
        ),
        legend.text = element_text(
            size = 10, color = "grey10"
        ),
        plot.title = element_text(
            size = 20, color = "grey10",
            hjust = 0, vjust = -3
        ),
        plot.subtitle = element_text(
            size = 40, color = "#c43c4e",
            hjust = 0, vjust = -1
        ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(
            c(t = 1, r = -2, b = -1, l = -2), "lines"
        ),
        plot.background = element_rect(
            fill = "#182833",
            color = NA
        ),
        legend.background = element_rect(
            fill = "#182833", color = NA
        ),
    ) +
    labs(
        x = "",
        y = "",
        title = "",
        subtitle = ""
    )

ggsave(
    filename = "eur_nightlight_2022.png",
    width = 6000, height = 6000, dpi = 600,
    units = "px", device = "png", bg = "#182833", map
)