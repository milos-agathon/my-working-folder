#############################################
# MAP no2 EMISSIONS WITH R
# Milos Popovic 2023/05/13
#############################################
setwd("/Users/mpopovic3/Downloads/population-emission")
setwd(no2_emissions_dir_path)

# 0. INSTALL & LOAD LIBRARIES
#----------------------------
# libraries we need

libs <- c(
    "tidyverse", "readr", "janitor",
    "classInt", "rayshader", "terra",
    "giscoR", "sf"
)

# install missing libraries
installed_libs <- libs %in% rownames(
    installed.packages()
)

if (any(installed_libs == F)) {
    install.packages(
        libs[!installed_libs]
    )
}

# load libraries

invisible(lapply(libs, library, character.only = T))

# 1. DOWNLOAD no2 DATA
#---------------------

url <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/N2O/TOTALS/emi_txt/v8.0_FT2022_GHG_N2O_2022_TOTALS.zip"
download.file(
    url,
    destfile = basename(url),
    mode = "wb"
)

# 2. UNZIP FILE
#---------------

no2_zip_file <- list.files(
    path = getwd(),
    pattern = "*.zip",
    full.names = T
)

unzip(no2_zip_file)

# 3. LOAD AND CLEAN THE DATA
#---------------------------

file <- list.files(
    path = getwd(),
    pattern = "*.txt",
    full.names = T
)

read_df <- function() {
    main_df <- readr::read_delim(
        file,
        delim = ";",
        col_names = T
    ) |>
        janitor::row_to_names(
            row_number = 2
        )

    names(main_df) <- "lat;long;emission"

    df <- main_df |>
        tidyr::separate(
            "lat;long;emission",
            into = c(
                "lat", "long", "emission"
            ),
            sep = ";"
        )

    final_df <- df |>
        dplyr::mutate_if(
            is.character, as.numeric
        )
    return(final_df)
}

final_df <- read_df()
head(final_df)

# 4. POPULATION DATA
#-------------------

url <-
    "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_BUILT_S_GLOBE_R2023A/GHS_BUILT_S_E2020_GLOBE_R2023A_4326_30ss/V1-0/GHS_BUILT_S_E2020_GLOBE_R2023A_4326_30ss_V1_0.zip"

file_name <- basename(url)

download.file(
    url = url,
    path = getwd(),
    destfile = file_name
)

# 2. LOAD GHSL DATA
#----------------------

unzip(file_name)
raster_name <- gsub(
    ".zip", ".tif",
    file_name
)

pop <- terra::rast(raster_name)

# 3. AUSTRALIA SHAPEFILE
#-----------------------

get_country_borders <- function() {
    country <- giscoR::gisco_get_countries(
        country = "NL",
        resolution = "1"
    )

    return(country)
}

country <- get_country_borders()

# 4. CROP AUSTRALIA GHSL
#-----------------------

country_pop <- terra::crop(
    pop,
    terra::vect(country),
    snap = "in",
    mask = T
)

no2_vect <- terra::vect(
    as.data.frame(final_df),
    geom = c("long", "lat"),
    crs(country_pop)
)

no2_points <- terra::extract(
    country_pop,
    no2_vect,
    xy = TRUE,
    bind = TRUE
) |> na.omit()

head(no2_points)
nrow(no2_points)

no2_population <- terra::merge(
    no2_vect, no2_points), 
    by.x="Ind", by.y="ID")




rows <- terra::cells(
    country_pop,
    no2_vect
) |>
    na.omit() |>
    as.data.frame()

no2_points <- dplyr::left_join(
    no2_points,
    rows,
    by = "ID"
)


tail(rows)
class(rows)
nrow(rows)

no2_points_df <- final_df |>
    dplyr::mutate(
        ID = 1:max(
            nrow(final_df)
        )
    ) |>
    dplyr::select("ID", "emission") |>
    dplyr::right_join(
        no2_points,
        by = "ID"
    )

nrow(no2_points_df)
head(no2_points_df)

country_pop_df <- as.data.frame(
    country_pop,
    xy = T
)

country_pop_df$cell <- 1:max(
    nrow(country_pop_df)
)

names(country_pop_df) <- c(
    "long", "lat",
    "population", "cell"
)

head(country_pop_df)

no2_population_df <- dplyr::inner_join(
    no2_points_df,
    country_pop_df,
    by = "cell"
)

nrow(no2_population_df)
head(no2_population_df)










