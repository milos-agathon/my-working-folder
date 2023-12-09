#############################################
# MAPPING WIND DIRECTION & SPEED WITH R
# Milos Popovic 2023/08/04
#############################################

# 1. INSTALL & LOAD LIBRARIES
#----------------------------

libs <- c(
    "ecmwfr", "tidyverse", "metR",
    "terra", "sf", "giscoR", "classInt",
    "lubridate", "hms"
)

installed_libs <- libs %in% rownames(
    installed.packages()
)

if (any(installed_libs == F)) {
    install.packages(
        libs[!installed_libs]
    )
}

invisible(lapply(libs, library, character.only = T))

# 1. QUERY WIND DATA
#--------------------


time <- seq(
    from = lubridate::ymd_hms(
        paste(2023, 12, 3, 00, 00, 00,
            sep = "-"
        )
    ),
    to = lubridate::ymd_hms(
        paste(2023, 12, 3, 23, 00, 00,
            sep = "-"
        )
    ),
    by = "1 hours"
)

hours <- hms::as_hms(time)

ymin <- 49.4
xmin <- -0.46
ymax <- 53.95
xmax <- 7.58

my_api <- "81378" # PLEASE INSERT YOUR UID
my_key <- "f2ac6e44-6f80-4a47-a985-1187328607a1" # PLEASE INSERT YOUR API KEY

request <- list(
    product_type = "reanalysis",
    format = "netcdf",
    variable = c(
        "10m_u_component_of_wind",
        "10m_v_component_of_wind"
    ),
    year = "2023",
    month = "12",
    day = "3",
    time = hours,
    area = c(
        ymax, xmin, ymin, xmax
    ),
    dataset_short_name = "reanalysis-era5-single-levels",
    target = "netherlands-wind.nc"
)

# ecmwfr::wf_set_key(
#     user = my_api,
#     key = my_key,
#     service = "cds"
# )

ecmwfr::wf_request(
    request = request,
    user = my_api,
    path = getwd()
)

# 2. LOAD WIND DATA
#--------------------

europe_wind <- terra::rast(
    "netherlands-wind.nc"
)

europe_wind_df <- europe_wind |>
    as.data.frame(
        xy = T, na.rm = T
    )

head(europe_wind_df)

# 3. U & V COMPONENT BY TIME
#-------------------------------

u <- europe_wind_df |>
    dplyr::select(
        x, y,
        dplyr::starts_with(
            "u"
        )
    ) |>
    tidyr::pivot_longer(
        !c("x", "y"),
        names_to = "time",
        values_to = "u10"
    ) |>
    dplyr::mutate(
        time = as.numeric(gsub("u10_", "", time))
    )

head(u)

v <- europe_wind_df |>
    dplyr::select(
        x, y,
        dplyr::starts_with(
            "v"
        )
    ) |>
    tidyr::pivot_longer(
        !c("x", "y"),
        names_to = "time",
        values_to = "v10"
    ) |>
    dplyr::mutate(
        time = as.numeric(gsub("v10_", "", time))
    )

head(v)

# 4. MERGE U & V COMPONENT
#-------------------------

europe_wind_stream <- dplyr::inner_join(
    u, v,
    by = c("x", "y", "time"),
    relationship = "many-to-many"
) |>
    dplyr::as_tibble()

head(europe_wind_stream)
nrow(europe_wind_stream)

# 5. AVERAGE
#-----------

europe_wind_stream_avg <- europe_wind_stream |>
    dplyr::mutate(
        id = match(
            stringr::str_c(x, y),
            unique(stringr::str_c(x, y))
        )
    ) |>
    arrange(id) |>
    dplyr::group_by(
        id, time
    ) |>
    dplyr::summarise(
        x = mean(x, na.rm = T),
        y = mean(y, na.rm = T),
        speed = mean(sqrt(u10^2 + v10^2),
            na.rm = T
        )
    )

head(europe_wind_stream_avg)
nrow(europe_wind_stream_avg)

# create 'ideal' data with all combinations of data
europe_wind_stream_full <- tidyr::expand_grid(
    id = unique(europe_wind_stream_avg$id),
    date = seq(
        from = min(europe_wind_stream_avg$time),
        to = max(europe_wind_stream_avg$time), by = 1
    )
)

# create complete dataset
europe_wind_stream_df <- dplyr::left_join(
    europe_wind_stream_full,
    europe_wind_stream_avg
)

nrow(europe_wind_stream_df)

