# 1. INSTALL & LOAD LIBRARIES
#----------------------------

libs <- c(
    "tidyverse",
    "geodata",
    "terra",
    "exactextractr",
    "sf", "classInt"
)

installed_libs <- libs %in% rownames(
    installed.packages()
)

if (any(installed_libs == F)) {
    install.packages(
        libs[!installed_libs],
        dependencies = T
    )
}

invisible(
    lapply(
        libs, library,
        character.only = T
    )
)

# 2. GHSL POPULATION DATA
#------------------------

url <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_BUILT_S_GLOBE_R2023A/GHS_BUILT_S_E2020_GLOBE_R2023A_4326_30ss/V1-0/GHS_BUILT_S_E2020_GLOBE_R2023A_4326_30ss_V1_0.zip"

file_name <- basename(url)

download.file(
    url = url,
    path = getwd(),
    destfile = file_name
)

# 3. LOAD GHSL DATA
#------------------

unzip(file_name)

raster_name <- gsub(
    ".zip", ".tif",
    file_name
)

pop <- terra::rast(raster_name)

# 4. POPULATION PER PROVINCE
#---------------------------

country <- geodata::gadm(
    country = "DE",
    level = 2,
    path = getwd()
) |>
sf::st_as_sf()

country$population <- exactextractr::exact_extract(
    pop,
    country,
    "sum"
)

# 5. CO2 EMISSIONS
#-----------------

u <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/CO2/TOTALS/emi_nc/v8.0_FT2022_GHG_CO2_2022_TOTALS_emi_nc.zip"

download.file(
    url = u,
    path = getwd(),
    destfile = basename(u)
)

unzip(basename(u))

list.files()

co2 <- terra::rast("v8.0_FT2022_GHG_CO2_2022_TOTALS_emi.nc")

# 6. CO2 EMISSIONS PER CAPITA
#----------------------------

country$sum_co2 <- exactextractr::exact_extract(
    co2,
    country,
    "sum"
)

country$co2_pc <- country$sum_co2 / country$population
summary(country$co2_pc)

# 7. THEME, COLORS & BREAKS
#--------------------------

theme_for_the_win <- function(){
    theme_void() +
    theme(
        legend.position = "top",
        legend.title = element_text(
            size = 9, color = "grey20"
        ),
         legend.text = element_text(
            size = 9, color = "grey20"
        ),
        plot.margin = unit(
            c(
                t = 1, r = 0, # Add 1
                b = 0, l = 0 
            ), "lines"
        )
    )
}

cols <- hcl.colors(
    5, "Inferno",
    rev = T
)

pal <- colorRampPalette(
    cols
)(64)

breaks <- classInt::classIntervals(
    country$co2_pc,
    n = 6,
    style = "equal"
)$brks

# 8. CO2 PER CAPITA MAP
#----------------------

crs_lambert <-
    "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_frfs"

map <- ggplot() +
    geom_sf(
        data = country,
        aes(
            fill = co2_pc
        ),
        color = "white",
        size = .15
    ) +
    scale_fill_gradientn(
        name = "tonnes per capita",
        colors = pal,
        breaks = round(breaks, 3), # ADD 3 DECIMAL PLACES
        labels = round(breaks, 3), # ADD 3 DECIMAL PLACES
        na.value = "white"
    ) +
    guides(
        fill = guide_colorbar(
            direction = "horizontal",
            barwidth = 12,
            barheight = .5
        )
    ) +
    coord_sf(crs = crs_lambert) +
    theme_for_the_win()

ggsave(
    "de_lvl2_co2.png",
    map,
    width = 6,
    height = 8,
    units = "in",
    bg = "white"
)