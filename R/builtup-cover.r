setwd("D:/ghsl-builtup")
libs <- c(
    "tidyverse",
    "terra",
    "sf",
    "tidyterra"
)

installed_libraries <- libs %in% rownames(
    installed.packages()
)

if (any(installed_libraries == F)) {
    install.packages(
        libs[!installed_libraries],
        dependencies = T
    )
}

invisible(
    lapply(
        libs, library,
        character.only = T
    )
)

# 2. CITY PERIMETER

city_centroids <- read_csv(
    "https://gist.githubusercontent.com/Fil/17fc857c3ce36bf8e21ddefab8bc9af4/raw/2731d08537f3cb2e4defa78884044a057bba64a0/cities.csv"
)

head(city_centroids)

mumbai_centroid <- city_centroids |>
    dplyr::select(
        2:4
    ) |>
    dplyr::rename(
        city = 1
    ) |>
    dplyr::filter(
        grepl(
            "Mumbai",
            city
        )
    ) |>
    as.data.frame()

mumbai_centroid

mumbai_centroid_sf <- sf::st_as_sf(
    mumbai_centroid,
    coords = c("Longitude", "Latitude"),
    crs = 4326
) |>
    sf::st_transform(crs = 4087) # Added this line to avoid sf error

# 3. CITY BUFFER

# sf::sf_use_s2(F)

mumbai_buffer <- sf::st_buffer(
    mumbai_centroid_sf,
    dist = units::set_units(
        5, km
    )
)

# 4. GET BUILT-UP FEATURES RASTER

url <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_BUILT_C_GLOBE_R2023A/GHS_BUILT_C_MSZ_E2018_GLOBE_R2023A_54009_10/V1-0/tiles/GHS_BUILT_C_MSZ_E2018_GLOBE_R2023A_54009_10_V1_0_R7_C26.zip"

file_name <- basename(url)

download.file(
    url = url,
    path = getwd(),
    destfile = file_name
)

unzip(file_name)

builtup_raster <- terra::rast(
    gsub(
        ".zip", ".tif",
        file_name
    )
)

# 5. MUMBAI BUILT-UP FEATURES RASTER

mumbai_buffer <- sf::st_transform(
    mumbai_buffer,
    crs = terra::crs(
        builtup_raster
    )
)


plot(sf::st_geometry(mumbai_buffer))

mumbai_builtup <- terra::crop(
    builtup_raster,
    terra::vect(mumbai_buffer),
    snap = "in",
    mask = T
)

terra::plot(mumbai_builtup)

vals <- terra::values(
    mumbai_builtup,
    dataframe = T
)

head(vals)
names(vals)[1] <- "value"
sort(unique(vals$value))

# 6. THEME, LABELS, COLORS

theme_for_the_win <- function() {
    theme_void() +
        theme(
            legend.position = "left",
            legend.title = element_text(
                size = 9, color = "grey20"
            ),
            legend.text = element_text(
                size = 8, color = "grey20"
            ),
            plot.margin = unit(
                c(
                    t = -1, r = -1,
                    b = -1, l = 1
                ), "lines"
            )
        )
}

labs <- c(
    "non-urban",
    "low vegetation",
    "medium vegetation",
    "high vegetation",
    "water",
    "road",
    "residential, building height <= 3m",
    "residential, building height 3-6m",
    "residential, building height 6-15m",
    "residential, building height 15-30m",
    "residential, building height > 30m",
    # "non-residential, building height <= 3m",
    "non-residential, building height 3-6m",
    "non-residential, building height 6-15m",
    "non-residential, building height 15-30m",
    "non-residential, building height > 30m"
)

cols <- c(
    "grey80", "#718C6C", "#8AD86B",
    "#C1FFA1", "#01B7FF", "#FFD501",
    "#D28200", "#FE5900", "#FF0101",
    "#CE001B", "#7A000A", "#FF9FF4",
    "#FF67E4", "#F701FF", "#A601FF",
    "#6E00FE"
)

# 7. THEME, LABELS, COLORS

map <- ggplot() +
    tidyterra::geom_spatraster(
        data = as.factor(
            mumbai_builtup
        )
    ) +
    scale_fill_manual(
        name = "Morphological Settlement Zone (MSZ)",
        values = cols,
        labels = labs,
        na.value = "white",
        na.translate = FALSE
    ) +
    coord_sf(crs = 4326) + # Added this line to have a nice circle shape
    guides(
        fill = guide_legend(
            direction = "vertical",
            width = unit(5, "mm"),
            height = unit(5, "mm")
        )
    ) +
    theme_for_the_win() +
    labs(
        title = "",
        caption = ""
    )

ggsave(
    "mumbai-builtup-new.png",
    map,
    width = 8,
    height = 6,
    units = "in",
    bg = "white"
)

# ©2023 Milos Popovic (https://milospopovic.net) | Data:  Sentinel-2 10m Land Use/Land Cover – Esri, Impact Observatory, and Microsoft
