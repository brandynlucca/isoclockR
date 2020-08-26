#' Base class for isotope objects (ISO).
#'
#' @description
#' An S4-class that provides slots to contain relevant animal metadata for parameterizing isotopic fractionation and residents models
#' that can depend on any number of tissue-types and isotopes, as well as any other physiological (e.g., catabolism), morphological
#' (e.g., ontogenetic allometry), and/or environmental (e.g., temperature) variables. This object can be used for either single animals, or
#' incorporated into a series of animals (e.g., \link[isoclockR]{MISO}).
#'
#' @slot metadata text
#' @slot data text
#' @slot timestamp text
#'
#' @details
#' words
#'
#' @author Brandyn M. Lucca
#' @export

ISO <- setClass("ISO", slots=c(metadata="ANY", data="ANY", timestamp="ANY"),
                prototype=list(data=list()))

#' Wrapping class for multiple isotope objects (MISO).
#'
#' @description
#' An S4-class object that stores both data and metadata for all isotope objects of interest (i.e., \link[isoclockR]{ISO}).
#'
#' @slot metadata text
#' @slot data text
#' @slot timestamp text
#'
#' @details
#' words
#'
#' @author Brandyn M. Lucca
#' @export

MISO <- setClass("MISO",
                 contains = "ISO",
                 slots=c(metadata="numeric", data="numeric", timestamp="numeric"))
