# 1. LIBRARIES

pacman::p_load(
    rstac,
    sf,
    terra
)

ms_data_source <- rstac::stac(
    "https://planetarycomputer.microsoft.com/api/stac/v1"
)

ms_collections_query <- ms_data_source |>
    rstac::collections()

class(ms_data_source)
class(ms_collections_query)

ms_collections <- rstac::get_request(ms_collections_query)
print(ms_collections, n = 123)

lux_bbox <- giscoR::gisco_get_countries(
    country = "LUX",
    resolution = "1"
) |> sf::st_bbox()

rstac::stac_search(
    q = ms_data_source,
    collections = "hrea",
    # datetime = "2021-01-01/2021-12-31",
    bbox = lux_bbox,
    limit = 999
)

ms_buildings_query <- rstac::get_request(ms_data_source)

ms_query_signin <- rstac::items_sign(
  ms_buildings_query,
  rstac::sign_planetary_computer()
)

rstac::assets_download(
    ms_query_signin, 
    "stac-items", 
    output_dir = getwd
)