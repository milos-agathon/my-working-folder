setwd("/Users/mpopovic3/Downloads")
remotes::install_github("eurostat/restatapi")

# libraries we need
libs <- c(
    "tidyverse", "sf",
    "classInt", "giscoR",
    "restatapi"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
    install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# sysfonts::font_add_google("Montserrat", "Montserrat")
# showtext::showtext_auto()

# 1. GET europe SF DATA
#----------------------
nuts3 <- gisco_get_nuts(
    year = "2021",
    epsg = "4326",
    resolution = "3",
    nuts_level = "3"
)

# plot(st_geometry(nuts3))

cntrys <- giscoR::gisco_get_countries(
    year = "2020",
    epsg = "4326",
    resolution = "3",
    region = c("Europe", "Asia")
)
names(cntrys)

non_eu_list <- c(
    "AM", "AZ", "BA",
    "BY", "GE", "MD",
    "RU", "UA", "XK"
)

eu_list <- c(unique(nuts3$CNTR_CODE), "XK")

eu <- cntrys |>
    filter(CNTR_ID %in% eu_list)

non_eu <- cntrys |>
    filter(CNTR_ID %in% non_eu_list)

# plot(st_geometry(non_eu))

bb <- st_sfc(
    st_polygon(list(cbind(
        c(-10.6600, 45, 45, -10.6600, -10.6600),
        c(34.5000, 34.5000, 71.0500, 71.0500, 34.5000)
    ))),
    crs = 4326
)

# get NUTS2-level data on NEETs

indic_df <- restatapi::get_eurostat_data(
    id = "demo_r_pjanind3",
    filters = c("PC_FM", "PC"),
    date_filter = c(2021:2022),
    exact_match = T,
    label = F
)

head(indic_df)

indic_filtered_df <- indic_df |>
    dplyr::select(
        1, 4, 5
    ) |>
    dplyr::rename(
        "NUTS_ID" = "geo"
    )

# convert to wide format
indic_wide <- tidyr::pivot_wider(
    indic_filtered_df,
    names_from = time,
    values_from = values
)

head(indic_wide)
# Replace values of 2019 with missing values by values of 2018
df <- indic_wide |>
    dplyr::mutate(
        values = if_else(is.na(`2022`),
            `2021`,
            `2022`
        )
    ) |>
    dplyr::select(NUTS_ID, values)

# merge shp and data.frame
d <- nuts3 |>
    dplyr::left_join(df, by = "NUTS_ID")
head(d)

# let's find a natural interval with pretty breaks
ni <- classInt::classIntervals(
    d$values,
    n = 6,
    style = "equal"
)$brks

# transform the shp into Lambert projection
crs_lambert <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"
laeabb <- sf::st_transform(
    bb,
    crs = crs_lambert
)
b <- sf::st_bbox(laeabb)


# cols <- hcl.colors(
#     n = 6,
#     palette = "viridis",
#     rev = T
# )

cols <- c(
    "#003851", "#007894",
    "#52bcda", "#f4a116",
    "#e6681d", "#d4111e"
)
pal <- colorRampPalette(cols, bias = 1.5)(8)

breaks <- c(88, 95, 100, 105, 110, 115, 120, 125)

brk <- ni |>
    append(max(d$values)) |>
    head(-1)

breaks <- c(min(d$values), brk) |>
    tail(-1)

# map
p <- ggplot(data = d) +
    geom_sf(
        data = filter(eu, CNTR_ID == "RS"),
        color = "white", size = 0.15,
        fill = "#bfbfbf"
    ) +
    geom_sf(
        mapping = aes(
            fill = values,
        ),
        color = NA, size = 0
    ) +
    geom_sf(
        data = eu, color = "white",
        size = 0.125, fill = "transparent"
    ) +
    geom_sf(
        data = non_eu, color = "white",
        size = 0.125, fill = "#bfbfbf"
    ) +
    coord_sf(
        crs = crs_lambert,
        xlim = c(b["xmin"], b["xmax"]),
        ylim = c(b["ymin"], b["ymax"])
    ) +
    scale_fill_gradientn(
        name = "Women per 100 men",
        colors = pal,
        breaks = breaks,
        limits = c(
            88,
            125
        ),
        #     min(d$values, na.rm = T),
        #     max(d$values, na.rm = T)
        # ),
        labels = round(breaks, 0),
        na.value = "grey80",
    ) +
    # scale_fill_gradientn(
    #     name = "Women per 100 men",
    #     colors = pal,
    #     breaks = breaks,
    #     limits = c(
    #         min(d$values),
    #         max(d$values)
    #     ),
    #     labels = round(breaks, 0),
    #     na.value = "#bfbfbf"
    # ) +
    guides(
        fill = guide_colorbar(
            direction = "horizontal",
            barheight = unit(2.5, units = "mm"),
            barwidth = unit(50, units = "mm"),
            title.position = "top",
            title.hjust = .5,
            label.hjust = .5,
            nrow = 1,
            byrow = T,
            reverse = F,
            label.position = "bottom"
        )
    ) +
    theme_void() +
    theme(
        # text = element_text(family = "Montserrat"),
        legend.position = c(.35, .95),
        legend.text = element_text(
            size = 10, color = "grey20"
        ),
        legend.title = element_text(
            size = 11, color = "grey20"
        ),
        legend.spacing.y = unit(.25, "cm"),
        plot.title = element_text(
            face = "bold", size = 26,
            color = "grey40", hjust = .5,
            vjust = 5
        ),
        plot.caption = element_text(
            size = 10,
            color = "grey20",
            hjust = .5, vjust = 1
        ),
        axis.title.x = element_text(
            size = 10, color = "grey60",
            hjust = 0.5, vjust = 3
        ),
        plot.margin = unit(
            c(t = 0, r = -2, b = -2, l = -2), "lines"
        )
    ) +
    labs(
        x = "©2024 Milos Popovic (https://milospopovic.net) Data: Eurostat",
        title = "Human sex ratio in 2022"
    )

ggsave(
    filename = "eur_sex_ratio_2022b.png",
    width = 7, height = 7.5, dpi = 600,
    device = "png", bg = "white", p
)
