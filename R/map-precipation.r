# 1. PACKAGES

if (
    !require(
        "pacman"
    )
) {
    install.packages(
        "pacman"
    )
}

pacman::p_load(
    nasapower,
    giscoR,
    sf,
    tidyverse,
    rayshader
)

# 2. BOUNDING BOX

country_sf <- giscoR::gisco_get_countries(
    country = "IT",
    resolution = "1"
)

plot(sf::st_geometry(country_sf))

country_bbox <- sf::st_bbox(
    country_sf
)

xmin <- country_bbox[["xmin"]]
ymin <- country_bbox[["ymin"]]
xmax <- country_bbox[["xmax"]]
ymax <- country_bbox[["ymax"]]

# 3. PRECIPATION DATA

round_down <- function(par, num) num * floor(par / num)
round_up <- function(par, num) num * ceiling(par / num)


xmin_ymin <- lapply(
    list(xmin, ymin),
    function(x) {
        round_down(
            par = x,
            num = 5
        )
    }
)

xmax_ymax <- lapply(
    list(xmax, ymax),
    function(x) {
        round_down(
            par = x,
            num = 5
        )
    }
)

flag <- 1
for (x in seq(
    xmin_ymin[[1]],
    xmax_ymax[[1]] - 5, 5
)
) {
    for (y in seq(
        xmin_ymin[[2]],
        xmax_ymax[[2]] - 5, 5
    )
    ) {
        precipation_country_temp <- nasapower::get_power(
            community = "AG",
            pars = "PRECTOTCORR", # https://power.larc.nasa.gov/ RESOURCES
            lonlat = c(x, (y - 5), (x + 5), y),
            temporal_api = "CLIMATOLOGY"
        )
        if (flag == 1) {
            precipation_country <- precipation_country_temp
            flag <- 0
        } else {
            precipation_country <- rbind(
                precipation_country,
                precipation_country_temp
            )
        }
    }
}

tail(precipation_country)
nrow(precipation_country)

min(precipation_country$LON)
min(precipation_country$LAT)
min(precipation_country$LON)
min(precipation_country$LON)


# 4. 2D CONTOUR MAP

map <- ggplot(
    data = precipation_country
) +
    geom_sf(
        data = country_sf,
        color = "white",
        size = 1
    ) +
    geom_tile(aes(
        x = LON,
        y = LAT,
        fill = ANN
    )) +
    geom_contour(
        aes(
            x = LON,
            y = LAT,
            z = ANN
        ),
        color = "black"
    ) +
    # coord_sf(crs = crs_lambert) +
    guides(
        fill = guide_colorbar(
            direction = "vertical",
            barheight = unit(20, "mm"),
            barwidth = unit(5, "mm"),
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
        legend.position = "right",
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
            fill = "white", color = NA
        ),
        legend.background = element_rect(
            fill = "white", color = NA
        ),
        panel.border = element_rect(
            fill = NA, color = "white"
        ),
        plot.margin = unit(
            c(
                t = 0, r = 0,
                b = 0, l = 0
            ), "lines"
        )
    )

print(map)


# 5. 3D CONTOUR MAP
x <- 5 # 10
y <- 35 #40

c(x, (y - 5), (x + 5), y)