library(PBSmapping)
library(sf)
library(dplyr)

importShapefile <- function(fn, readDBF=TRUE, projection=NULL, zone=NULL,
                            minverts=3, placeholes=FALSE, show.progress=FALSE) {

    ## Append .shp extension if necessary and check for file existence
    if (!grepl("\\.shp$", fn)) {
        fn <- paste0(fn, ".shp")
    }

    ## Check if the shapefile and associated files exist
    required_files <- c(fn, sub("\\.shp$", ".shx", fn), sub("\\.shp$", ".dbf", fn))
    if (!all(file.exists(required_files))) {
        stop("One or more required files (shp, shx, dbf) are missing.")
    }

    ## Read the shapefile
    shape <- st_read(fn, quiet = TRUE)

    if (nrow(shape) == 0) stop("No features found in the shapefile.")

    ## Get unique geometry type
    geom_types <- unique(st_geometry_type(shape))
    message("Geometry types: ", paste(geom_types, collapse = ", "))

    ## Initialize a list to store results
    result_list <- list()

    if ("POINT" %in% geom_types) {
        ## Handle POINT geometry
        data <- st_coordinates(shape) %>%
            as.data.frame() %>%
            rename(X = X, Y = Y)

        eventdata <- cbind(EID = seq_len(nrow(data)), data)
        if (readDBF) {
            eventdata <- cbind(eventdata, st_drop_geometry(shape))
        }
        result_list$data <- eventdata

    }

    if ("LINESTRING" %in% geom_types || "POLYGON" %in% geom_types || "MULTIPOLYGON" %in% geom_types) {
        ## Handle LINESTRING, POLYGON, and MULTIPOLYGON geometries
        ## Cast to MULTIPOLYGON to ensure we capture all geometries as needed
        shape <- st_cast(shape, "MULTIPOLYGON")

        ## Extract coordinates and identifiers
        coords <- st_coordinates(shape)

        ## Add POS (position), PID (part ID), and SID (sub part ID for multipolygons)
        polylines_data <- as.data.frame(coords) %>%
            group_by(L3) %>%                  # Group by PID for POS counting
            mutate(PID = L3,                   # Use L3 directly for unique PIDs
                   SID = as.numeric(factor(paste(L1, L2)))) %>%              # Sub-ID for polygons within the same feature
            mutate(POS = row_number()) %>%     # Position within each multipolygon group
            ungroup() %>%                      # Ungroup to return to a regular data frame
            dplyr::select(PID, SID, POS, X = X, Y = Y)

      result_list$data <- polylines_data

      if (readDBF) {
          attr_data <- st_drop_geometry(shape)
          result_list$PolyData <- attr_data
      }
  }

  if (length(unique(geom_types)) > 1) {
    warning("Mixed geometry types detected. Only points, linestrings, and polygons are processed.")
  }

  # Store the primary data and associated attributes
  main_data <- as.data.frame(result_list$data)

  if (exists("PolyData", result_list)) {
    attr(main_data, "PolyData") <- cbind(PID=1:nrow(result_list$PolyData), result_list$PolyData)
  }

  if (!is.null(projection)) {
    st_crs(shape) <- projection
    attr(main_data, "projection") <- projection
  }

  if (!is.null(zone)) {
    attr(main_data, "zone") <- zone
  }

  if (placeholes) {
    message("Note: In this version, automatic hole placement is not implemented.")
  }

  return(main_data)
}

# Example usage
# Adjust the filename path below to point to your actual shapefile
# shapefile_data <- importShapefile("path/to/your/shapefile.shp")

calcCentroid <- function(shp, rollup=3) {
    PBSmapping::calcCentroid(as.data.frame(shp), rollup=rollup)
}

calcArea <- function(shp, rollup=3) {
    PBSmapping::calcArea(as.data.frame(shp), rollup=rollup)
}
