# 1. LIBRARIES

install.packages("httr")

pacman::p_load(
    rstac,
    sf,
    terra
)

ms_data_source <- rstac::stac(
    "https://planetarycomputer.microsoft.com/api/stac/v1"
)

ms_data_source

ms_collections_query <- ms_data_source |>
    rstac::collections()

class(ms_data_source)
class(ms_collections_query)

ms_collections <- rstac::get_request(ms_collections_query)
print(ms_collections, n = 123)

bbox <- giscoR::gisco_get_countries(
    country = "SG",
    resolution = "1"
) |> sf::st_bbox()

ms_buildings_query <- rstac::stac_search(
    q = ms_data_source,
    collections = "hrea",
    datetime = "2019-12-31",
    bbox = bbox,
    limit = 100
) |>
    rstac::get_request()

ms_query_signin <- rstac::items_sign(
    ms_buildings_query,
    rstac::sign_planetary_computer()
)

rstac::assets_download(
    items = ms_query_signin,
    asset_names = "estimated-brightness",
    output_dir = getwd(),
    overwrite = TRUE
)
