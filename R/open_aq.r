install.packages("openair")
install.packages("saqgetr")
# library(openair)
library(saqgetr)
library(tidyverse)

# Import site information
data_sites <- saqgetr::get_saq_sites()
head(data_sites)
names(data_sites)

nld_sites <- data_sites |>
    dplyr::filter(
        country_iso_code == "NL"
    ) |>
    dplyr::select(
        "site", "longitude", "latitude"
    )

# Get nitrogen data for the Netherlands
nld_no2_data <- saqgetr::get_saq_observations(
    site = nld_sites$site,
    variable = "no2", 
    start = 2014,
    end = 2023,
    valid_only = T,
    verbose = T
)

class(nld_no2_data)
nrow(nld_no2_data)
head(nld_no2_data)

nld_no2_coords <- nld_no2_data |>
    dplyr::left_join(
        nld_sites, by = "site"
    )

head(nld_no2_coords)

# Get monthly means
nld_no2_monthly <- saqgetr::get_saq_simple_summaries(
    file = nld_no2_coords,
    summary = "monthly_mean"
)
names(nld_no2_monthly)
nrow(nld_no2_monthly)


data_annual %>%
    filter(
        site == "gb0682a",
        lubridate::year(date) == 2017L,
        variable == "pm10",
        summary_source == 1L
    ) %>%
    select(
        date,
        site,
        variable,
        count,
        value
    )