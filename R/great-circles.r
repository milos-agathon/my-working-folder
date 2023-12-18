setwd("D:/Downloads")

# install.packages("devtools")
devtools::install_github("ropensci/comtradr@main")

# libraries we need
libs <- c(
    "tidyverse", "sf", "giscoR", "classInt",
    "sfheaders", "comtradr", "CoordinateCleaner"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)) {
    install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = TRUE))

# 1. GET TRADE DATA
#---------

comtradr::set_primary_comtrade_key(
    "a45012dc37944445ad90eb6e81a51cd4"
)

# Fetch all wheat related commodity codes from the Comtrade commodities DB.
# This vector of codes will get passed to the API query.
wheat_codes <- comtradr::ct_commodity_lookup(
    "wheat",
    return_code = F,
    return_char = T
)

# query from Comtrade API
# grab text and return a dataframe

# You can request a maximum interval of twelve years from the API
wheat_exports <- comtradr::ct_get_data(
    commodity_code = "1001",
    flow_direction = "export",
    reporter = "UKR",
    partner = "all",
    start_date = 2022,
    end_date = 2022
)

# Inspect the return data
str(wheat_exports)
head(wheat_exports)

wheat_df <- wheat_exports |>
    dplyr::select(
        partner_iso,
        net_wgt
    )

# 2. FETCH CAPITALS
#---------
data(countryref)

capitals <- countryref |>
    dplyr::filter(!is.na(capital)) |>
    dplyr::group_by(iso3, capital) |>
    dplyr::summarise_at(vars(capital.lon, capital.lat), max) |>
    dplyr::rename(long = capital.lon, lat = capital.lat)

head(capitals)

# 3. CREATE STARTING/ENDING POINTS
#---------------------------------

end_coords <- wheat_df |>
    dplyr::left_join(
        capitals,
        by = c("partner_iso" = "iso3")
    ) |>
    dplyr::select(
        partner_iso, net_wgt,
        long, lat
    ) |>
    na.omit()

start_coords <- capitals |>
    dplyr::filter(iso3 == "UKR") |>
    dplyr::group_by(iso3) |>
    dplyr::slice(
        rep(
            1:max(
                nrow(end_coords)
            ),
            each = max(nrow(end_coords))
        )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(long, lat)

# 4. GENERATE GREAT CIRCLES
#--------------------------

start_coords$linestring_id <-
    end_coords$linestring_id <-
    seq_len(nrow(start_coords))

wheat_lines_sf <- sfheaders::sf_linestring(
    dplyr::bind_rows(
        start_coords, end_coords
    ) |>
        dplyr::arrange(linestring_id),
    x = "long", y = "lat",
    linestring_id = "linestring_id"
) |>
    sf::st_set_crs(4326)

wheat_lines_sf <- cbind(
    wheat_lines_sf,
    end_coords[, c("partner_iso", "net_wgt")]
)

plot(sf::st_geometry(wheat_lines_sf))

# 5. RETURN WORLD SPATIAL OBJECT
#-------------------------------

world_shp <- giscoR::gisco_get_countries(
    year = "2016",
    epsg = "4326",
    resolution = "10"
) |>
    subset(NAME_ENGL != "Antarctica") # get rid of Antarctica

# 6. MAP
#---------

crsROBINSON <- "+proj=robin +lon_0=0w"

wheat_points_sf <- end_coords |>
    dplyr::group_by(partner_iso) |>
    dplyr::arrange(
        dplyr::desc(net_wgt)
    ) |>
    sf::st_as_sf(
        coords = c("long", "lat"),
        crs = 4326
    )

p <- ggplot(data = wheat_points_sf[1:10, ],) +
    geom_sf(
        data = world_shp,
        fill = "#063140",
        color = "#18BFF2",
        size = 0.2,
        alpha = .35
    ) +
    geom_sf(
        data = wheat_lines_sf,
        aes(
            size = net_wgt / 1000000,
            alpha = net_wgt / 1000000
        ),
        fill = "#ff6103",
        # alpha=.35
        color = "#ff6103"
    ) +
    geom_sf(
        data = wheat_points_sf,
        aes(
            size = net_wgt / 1000000,
            alpha = net_wgt / 1000000
        ),
        fill = "#ff6103",
        color = "#ff6103",
        alpha = .85,
        stroke = .25
    ) +
    geom_sf_text(
        aes(
            label = round(
                net_wgt / 1000000, 3
            )
        ),
        size = 2,
        vjust = 1,
        hjust = 1,
        color = "#E65602",
        alpha = 1,
        nudge_x = c(.5, .5, 1),
        nudge_y = c(-.5, -.5, 0)
    ) +
    coord_sf(crs = crsROBINSON) +
    labs(
        y = "",
        subtitle = "",
        x = "",
        title = "Wheat imports from Ukraine in 2022",
        caption = "©2023 Milos Popovic https://milospopovic.net\nSource: United Nations. 2023. UN comtrade
      http://comtrade.un.org"
    ) +
    scale_size(
        name = "thousands of tonnes",
        range = c(.5, 2),
        breaks = scales::pretty_breaks(6)
    ) +
    scale_alpha(range = c(.25, .75)) +
    guides(
        alpha = "none",
        color = "none",
        fill = "none",
        size = guide_legend(
            override.aes = list(fill = NULL, alpha = .85, color = "#ff6103"),
            direction = "horizontal",
            keyheight = unit(1.15, units = "mm"),
            keywidth = unit(15, units = "mm"),
            title.position = "top",
            title.hjust = 0.5,
            label.hjust = .5,
            nrow = 1,
            byrow = T,
            reverse = F,
            label.position = "top"
        )
    ) +
    theme_void() +
    theme(
        plot.background = element_rect(
            fill = "#052833", color = NA
        ),
        panel.background = element_rect(
            fill = "#052833", color = NA
        ),
        legend.background = element_rect(
            fill = "#052833", color = NA
        ),
        legend.position = c(.55, -.05),
        panel.grid.major = element_line(
            color = "#052833", size = 0.1
        ),
        plot.title = element_text(
            size = 22, color = "#ff6103",
            hjust = 0.5, vjust = 1
        ),
        plot.subtitle = element_text(
            size = 11, color = "#7a2b41", hjust = 0.5, vjust = 0, face = "bold"
        ),
        plot.caption = element_text(
            size = 8, color = "grey80",
            hjust = 0.15, vjust = 0
        ),
        axis.title.x = element_text(
            size = 10, color = "grey20",
            hjust = 0.5, vjust = -6
        ),
        legend.text = element_text(
            size = 9, color = "#ff6103"
        ),
        legend.title = element_text(size = 10, color = "#ff6103"),
        strip.text = element_text(size = 20, color = "#ff6103", face = "bold"),
        plot.margin = unit(c(t = 1, r = -2, b = .5, l = -2), "lines")
    )

ggsave(
    "ukraine_export_wheat_2022_top10.png",
    width = 9.7, height = 6, dpi = 600,
    device = "png", bg = "white", p
)
