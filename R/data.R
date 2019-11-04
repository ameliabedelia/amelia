#' Geospatial data of Landkreise in Germany
#'
#' A tidy dataframe containing the latitude and longitudes of Landkreise
#' in Germany for use with ggplot2::geom_polygon.
#'
#' \describe{
#'   \item{long}{longitude}
#'   \item{lat}{latitutde}
#'   \item{rs}{Landkreise code}
#'   \item{gen}{Landkreis name}
#'   \item{hole}{Logical. Whether polygon is a hole or not}
#'   ...
#' }
"landkreis"

#' Geospatial data of Bunderländer in Germany
#'
#' A tidy dataframe containing the latitude and longitudes of Bunderländer
#' in Germany for use with ggplot2::geom_polygon.
#'
#' \describe{
#'   \item{long}{longitude}
#'   \item{lat}{latitutde}
#'   \item{id}{Numeric code used as governmental ID and prefix for regional codes}
#'   \item{gen}{Bundesland name}
#'   \item{hole}{Logical. Whether polygon is a hole or not}
#'   ...
#' }
"bundesland"
