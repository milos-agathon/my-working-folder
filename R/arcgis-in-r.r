# 1. PACKAGES
#------------

remotes::install_github(
    "r-arcgis/arcgis", 
    dependencies = TRUE
)

libs <- c(
    "arcgis",
    "tidyverse",
    "giscoR",
    "sf",
    "classInt"
)

installed_libs <- libs %in% rownames(
    installed.packages()
)

if(
    any(installed_libs == F){
        install.packages(
            libs[!installed_libs],
            dependencies = T
        )
    }
)

invisible(
    lapply(
        libs, library,
        character.only = T
    )
)

# 2. COUNTRY BOUNDARIES
#---------------------------------

furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/World_Administrative_Divisions/FeatureServer/0"

region_data <- arcgislayers::arc_open(furl)
region_data

sicily_sf <- arcgislayers::arc_select(
  region_data, 
  fields = "NAME", 
  where = "NAME = 'Sicilia'",
  crs = 4326
)

plot(sf::st_geometry(sicily_sf))

# 3. LANDSAT IMAGE

url <- "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"

landsat_data <- arcgislayers::arc_open(url)

region_raster <- arcgislayers::arc_raster(
    
)
