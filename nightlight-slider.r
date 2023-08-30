######################################
# Let's visualize night lights with R
# Milos Popovic 2023/08/20
######################################

setwd("/Users/mpopovic3/Downloads/nightlight/nl")

# define libraries we need
libs <- c(
    "tidyverse", "terra", "sf",
    "geodata", "shiny", "shiny.slider"
)

# install missing libraries
installed_libraries <- libs %in% rownames(installed.packages())
if (any(installed_libraries == F)) {
    installed.packages(libs[!installed_libraries])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# 1. GET COUNTRY MAP
#-------------------

# 2. GET DATA
#------------

# https://zenodo.org/record/7750175/files/nightlights.average_viirs.v21_m_500m_s_20000101_20001231_go_epsg4326_v20230318.tif
years <- c(2001, 2021)
url <- paste0(
    "https://zenodo.org/record/7750175/files/nightlights.average_viirs.v21_m_500m_s_",
    years, "0101_",
    years, "1231_go_epsg4326_v20230318.tif"
)

for (u in url) {
    download.file(
        url = u,
        destfile = basename(u),
        mode = "wb"
    )
}

raster_files <- list.files(
    path = getwd(),
    pattern = "VNL",
    full.names = T
)

globe_lights <- lapply(
    raster_files,
    terra::rast
)

# get_bounding_box_europe <- function() {
#     xmin <- -23.6600
#     xmax <- 65.5500
#     ymin <- 26.5000
#     ymax <- 72.0500

#     bb <- sf::st_sfc(
#         sf::st_polygon(list(cbind(
#             c(xmin, xmax, xmax, xmin, xmin),
#             c(ymin, ymin, ymax, ymax, ymin)
#         ))),
#         crs = 4326
#     )

#     return(bb)
# }
# bb <- get_bounding_box_europe()

country_sf <- giscoR::gisco_get_countries(
    country = "UA",
    resolution = "1"
)

plot(sf::st_geometry(country_sf))

country_lights_list <-
    lapply(
        globe_lights,
        function(x) {
            terra::crop(
                x,
                terra::vect(
                    country_sf
                ),
                snap = "in",
                mask = T
            )
        }
    )

crs_lambert <-
    "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_frfs"

country_lights_reproj <- lapply(
    country_lights_list, function(x) {
        project(
            x,
            y = crs_lambert
        )
    }
)

country_lights_reproj2 <- lapply(
    country_lights_reproj, function(x) {
        terra::ifel(
            x <= 0,
            NA,
            x
        )
    }
)

# country_lights_reproj[[1]] <- terra::resample(
#     country_lights_reproj[[1]],
#     country_lights_reproj[[2]]
# )

# country_lights_stacked <- c(
#     country_lights_reproj[[1]],
#     country_lights_reproj[[2]]
# )

# names(country_lights_stacked) <- years



# 2. NC TO DATAFRAME
#-------------------
country_lights_df <- lapply(
    country_lights_reproj2, function(x) {
        as.data.frame(
            x,
            xy = T,
            na.rm = T
        )
    }
)

str(country_lights_df)
col_names <- c("x", "y", "value")
country_lights_df <- lapply(
    country_lights_df, setNames,
    col_names
)

head(country_lights_df)

# vmin <- min(country_lights_long$value)
# vmax <- max(country_lights_long$value)

# breaks <- classInt::classIntervals(
#     country_lights_long$value,
#     n = 5,
#     style = "equal"
# )$brks

cols <- c("#1f4762", "#FFD966", "white")
pal <- colorRampPalette(cols, bias = 8)(512)
w <- ncol(country_lights_reproj2[[1]])
h <- nrow(country_lights_reproj2[[1]])

map <- lapply(
    country_lights_df,
    function(df) {
        ggplot(
            data = df
        ) +
            geom_tile(aes(
                x = x, y = y, fill = value
            )) +
            scale_fill_gradientn(
                name = "",
                colours = pal
            ) +
            coord_sf(
                crs = crs_lambert
            ) +
            guides(
                fill = guide_colorbar(
                    direction = "vertical",
                    keyheight = unit(1.5, units = "mm"),
                    keywidth = unit(10, units = "mm"),
                    title.position = "top",
                    label.position = "right",
                    title.hjust = .5,
                    label.hjust = .1,
                    ncol = 1,
                    byrow = F
                )
            ) +
            theme_minimal() +
            theme(
                axis.line = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                legend.position = "none",
                legend.title = element_text(
                    size = 11, color = "grey10"
                ),
                legend.text = element_text(
                    size = 10, color = "grey10"
                ),
                plot.title = element_text(
                    size = 20, color = "grey10",
                    hjust = 0, vjust = -3
                ),
                plot.subtitle = element_text(
                    size = 40, color = "#c43c4e",
                    hjust = 0, vjust = -1
                ),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.margin = unit(
                    c(t = 1, r = 0, l = 0, b = 0), "lines"
                ),
                plot.background = element_rect(
                    fill = "#182833",
                    color = NA
                ),
                legend.background = element_rect(
                    fill = "#182833", color = NA
                ),
            ) +
            labs(
                x = "",
                y = "",
                title = "",
                subtitle = ""
            )
    }
)

for (i in 1:2) {
    file_name <- paste0("ua_map_", i, ".png")
    png(
        file_name,
        width = w,
        height = h,
        units = "px",
        res = NA,
        bg = "#182833"
    )
    print(map[[i]])
    dev.off()
}


devtools::install_github("Timag/shiny.slider")
.libPaths()
# setwd("/Users/mpopovic3/Library/R/arm64/4.2/library/shiny/app_template/")
# shiny::runApp()

ui <- fluidPage(
  
  tags$script(src = "jquery-1.6.1.min.js"),
  tags$script(src = "jquery-ui-1.8.13.custom.min.js"),
  tags$script(src = "jquery.beforeafter-1.4.min.js"),
  
  tags$script("
            $(function() {
                $('#slider').beforeAfter({
                    introDelay: 2000,
                    imagePath: 'img/',
                    introDuration: 500,
                    showFullLinks: false
                })
              });
  "),
  
  mainPanel(
    
    tags$div(
      id = "slider",
      img(
        src = "ua_map_1.png", 
        width = 1920, 
        height = 1080
      ),
      
      img(
        src = "ua_map_2.png", 
        width = 1920, 
        height = 1080
      )
    )
  )
  
)

server <- function(input, output, session){}

shinyApp(ui, server)