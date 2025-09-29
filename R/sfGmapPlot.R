#' sfGmapPlot
#'
#' Plotting function to overlay sf objects on a ggmap
#'
#'
#'
#' @param sf a simple features object
#' @param maptype type of map for ggmap to fetch
#' @param color Plotting color of SF object
#' @param zoom The zoom level of the plot
#'
#' @export
#' @examples
#' \dontrun{
#'
#' connectSeatrack("testreader", "testreader")
#' hornoya <- getPosdata(selectColony = "Hornøya",
#'                       selectYear = "2015_16",
#'                       loadGeometries = T
#'                  )
#'
#' sub <- hornoya[1:500, ]
#'
#' sfGmapPlot(sub,
#'            zoom = 3)
#'
#'
#' }

sfGmapPlot <- function(sf,
                       maptype = c("terrain", "terrain-background", "satellite",
                                                 "roadmap", "hybrid", "toner", "watercolor", "terrain-labels", "terrain-lines",
                                                 "toner-2010", "toner-2011", "toner-background", "toner-hybrid",
                                                 "toner-labels", "toner-lines", "toner-lite"),
                       color = "salmon",
                       zoom = 4){

  maptype <- match.arg(maptype)


  ##Function below courtesy of @andyteucher at stackoverflow
  # Define a function to fix the bbox to be in EPSG:3857
  ggmap_bbox <- function(map) {
    if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
    # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
    # and set the names to what sf::st_bbox expects:
    map_bbox <- setNames(unlist(attr(map, "bb")),
                         c("ymin", "xmin", "ymax", "xmax"))

    # Coonvert the bbox to an sf polygon, transform it to 3857,
    # and convert back to a bbox (convoluted, but it works)
    bbox_3857 <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(map_bbox, crs = 4326)), 3857))

    # Overwrite the bbox of the ggmap object with the transformed coordinates
    attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
    attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
    attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
    attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
    map
  }

 ##bounding box not working so well, using center point and manual zoom instead
  #bbox_4326 <- st_bbox(sf)
  #names(bbox_4326) <- c("left", "bottom", "right", "top")

  if(sf::st_geometry_type(sf)[1] == "POINT"){
  center <- apply(sf::st_coordinates(sf), 2, mean)
  }
  if(sf::st_geometry_type(sf)[1] == "MULTIPOLYGON"){
    suppressWarnings(center <- sf::st_coordinates(sf::st_centroid(sf::st_union(nc))))
  }

  names(center) <- c("lon", "lat")

  suppressMessages(bg <- ggmap::get_map(location = center, zoom = zoom, maptype = maptype))
  bg <- ggmap_bbox(bg)

  overlay <- sf::st_transform(sf, 3857)

  suppressMessages(
    ggmap(bg) +
    coord_sf(crs = sf::st_crs(3857)) + # force the ggplot2 map to be in 3857
    geom_sf(data = overlay, col = color, inherit.aes = F)
  )


}


