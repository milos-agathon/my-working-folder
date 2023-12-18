windowsFonts(georg = windowsFont("Georgia"))

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

# install.packages("devtools")


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
    dplyr::rename(
        end_lat = lat,
        end_long = long
    ) |>
    na.omit()

start_coords <- capitals |>
    dplyr::filter(iso3 == "UKR") |>
    dplyr::rename(start_lat = lat, start_long = long) |>
    group_by(iso3) |>
    slice(
        rep(1:max(nrow(end_coords)),
            each = max(nrow(end_coords))
        )
    ) |>
    ungroup() |>
    dplyr::select(start_long, start_lat)

wheat_coords <- as.data.frame(cbind(start_coords, end_coords)) |>
    dplyr::select(
        partner_iso, net_wgt,
        start_long, start_lat,
        end_long, end_lat
    )

fix(wheat_coords)


# 4. GENERATE GREAT CIRCLES
#--------------------------

wheat_export_lines <- wheat_coords |>
    sf::st_as_sf(
        coords = c(
            "start_long",
            "start_lat",
            "end_long",
            "end_lat"
        ),
        crs = 4326
    )

sf::st_as_sf(
    coords = c(
        "start_long",
        "start_lat",
        "end_long",
        "end_lat"
    ),
    crs = sf::st_crs(4326)
) |>
    sf::st_cast("LINESTRING")

wheat_coords |>
    dplyr::select(
        start_long,
        start_lat,
        end_long,
        end_lat
    ) |>
    t() |>
    purrr::map(~ matrix(purrr::flatten_dbl(.), nrow = 2, byrow = TRUE)) |>
    purrr::map(sf::st_linestring) |>
    sf::st_sfc(crs = 4326) |>
    sf::st_sf(geometry = .) |>
    dplyr::bind_cols(wheat_coords) |>
    dplyr::select(iso3, capital, geometry) |>
    dplyr::left_join(wheat_df, by = "iso3") |>
    dplyr::filter(!is.na(Qty))


# 5. RETURN WORLD SPATIAL OBJECT
#---------

get_world <- function(world_shp) {
    world_shp <- giscoR::gisco_get_countries(
        year = "2016",
        epsg = "4326",
        resolution = "10"
    ) |>
        subset(NAME_ENGL != "Antarctica") # get rid of Antarctica

    return(world_shp)
}

# 6. MAP
#---------
map_url <- "https://raw.githubusercontent.com/milos-agathon/great-circles-with-r/main/R/make_map.r"
source(map_url) # load script
map <- map_great_circles()
