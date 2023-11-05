setwd("/Users/mpopovic3/Downloads")

# libraries we need
libs <- c(
    "tidyverse", "sf", "classInt", "giscoR", "eurostat"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
    install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

sysfonts::font_add_google("Montserrat", "Montserrat")
showtext::showtext_auto()

# define longlat projection
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# 1. GET europe SF DATA
#----------------------
nuts2 <- gisco_get_nuts(
    year = "2021",
    epsg = "4326",
    resolution = "3",
    nuts_level = "2"
)

# plot(st_geometry(nuts3))

cntrys <- giscoR::gisco_get_countries(
    year = "2020",
    epsg = "4326",
    resolution = "3",
    region = c("Europe", "Asia")
)
names(cntrys)

non_eu_list <- c("AR", "AZ", "BA", "BY", "GE", "MD", "RU", "UA", "XK")
eu_list <- c(unique(nuts2$CNTR_CODE), "XK")

eu <- cntrys %>%
    filter(CNTR_ID %in% eu_list)

non_eu <- cntrys %>%
    filter(CNTR_ID %in% non_eu_list)

# plot(st_geometry(non_eu))

bb <- st_sfc(
    st_polygon(list(cbind(
        c(-10.6600, 36.5500, 36.5500, -10.6600, -10.6600),
        c(34.5000, 34.5000, 71.0500, 71.0500, 34.5000)
    ))),
    crs = crsLONGLAT
)

# get NUTS2-level data on NEETs
indic <- eurostat::get_eurostat("htec_emp_reg2",
    time_format = "num"
) %>%
    dplyr::filter(
        nace_r2 == "C" &
            unit == "PC_EMP" &
            sex == "T" &
            time >= 2020
    ) %>%
    dplyr::select(geo, time, values)

names(indic)[1] <- "NUTS_ID"

# convert to wide format
iwide <- tidyr::pivot_wider(indic, names_from = time, values_from = values)

# Replace values of 2019 with missing values by values of 2018
df <- iwide %>%
    mutate(values = if_else(is.na(`2021`), `2020`, `2021`)) %>%
    select(NUTS_ID, values)

# merge shp and data.frame
d <- nuts2 %>% left_join(df, by = "NUTS_ID")
head(d)
# let's find a natural interval with pretty breaks
ni <- classIntervals(d$values,
    n = 6,
    style = "fisher"
)$brks

# this function uses above intervals to create categories
labels <- c()
for (i in 1:length(ni)) {
    labels <- c(labels, paste0(
        round(ni[i], 0),
        "-",
        round(ni[i + 1], 0)
    ))
}
labels <- labels[1:length(labels) - 1]

# finally, carve out the categorical variable
# based on the breaks and labels above
d$cat <- cut(d$values,
    breaks = ni,
    labels = labels,
    include.lowest = T
)
levels(d$cat) # let's check how many levels it has (7)

# label NAs, too
lvl <- levels(d$cat)
lvl[length(lvl) + 1] <- "No data"
d$cat <- factor(d$cat, levels = lvl)
d$cat[is.na(d$cat)] <- "No data"
levels(d$cat)

# transform the shp into Lambert projection
crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"
laeabb <- st_transform(bb, crs = crsLAEA)
b <- st_bbox(laeabb)

# map
p <- ggplot() +
    geom_sf(
        data = filter(eu, CNTR_ID == "RS"),
        color = "white", size = 0.15, fill = "grey80"
    ) +
    geom_sf(data = d, aes(fill = cat), color = NA, size = 0) +
    geom_sf(data = eu, color = "white", size = 0.125, fill = "transparent") +
    geom_sf(
        data = non_eu, color = "white", size = 0.125, fill = "grey80"
    ) +
    coord_sf(
        crs = crsLAEA,
        xlim = c(b["xmin"], b["xmax"]),
        ylim = c(b["ymin"], b["ymax"])
    ) +
    scale_fill_manual(
        name = "",
        values = rev(c(
            "grey80", "#355070", "#735a77",
            "#a86377", "#d17075", "#e48b7d",
            "#eaac8b"
        )),
        drop = F
    ) +
    guides(fill = guide_legend(
        direction = "horizontal",
        keyheight = unit(1.5, units = "mm"),
        keywidth = unit(15, units = "mm"),
        title.position = "top",
        title.hjust = .5,
        label.hjust = .5,
        nrow = 1,
        byrow = T,
        reverse = F,
        label.position = "bottom"
    )) +
    theme_minimal() +
    theme(
        text = element_text(family = "Montserrat"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(
            size = 50, color = "grey60", hjust = 0.5, vjust = 10
        ),
        axis.title.y = element_blank(),
        legend.position = c(.35, .975),
        legend.text = element_text(size = 60, color = "grey20"),
        legend.title = element_text(size = 70, color = "grey20"),
        legend.spacing.y = unit(0.25, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(
            face = "bold", size = 100, color = "#b56576", hjust = .5
        ),
        plot.caption = element_text(
            size = 50, color = "grey20", hjust = .5, vjust = 1
        ),
        plot.subtitle = element_text(size = 60, color = "#eaac8b", hjust = .5),
        plot.margin = unit(
            c(t = 1, r = -2, b = -1, l = -2), "lines"
        ),
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
        panel.border = element_blank()
    ) +
    labs(
        x = "©2022 Milos Popovic (https://milospopovic.net) Data: Eurostat",
        y = NULL,
        title = "Employment in manufacturing in 2021*",
        subtitle = "% of employed people",
        caption = "* The data for Montenegro, N. Macedonia and Turkey is from 2020"
    )

ggsave(
    filename = "eur_manufacturing_2021.png",
    width = 7, height = 8.5, dpi = 600,
    device = "png", bg = "white", p
)
