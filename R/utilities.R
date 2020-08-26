#' Method for what is printed for ISO/MISO objects.
#' @export
setMethod("show", "ISO", function(object){
  cat(paste(is(object)[[1]]," object", sep=""), "\n",
    "Samples: ",max(unlist(lapply(object@data, dim))), "\n",
    "Isotopes: ", unique(names(object@data)), "\n",
    "Origin steady state isotopic composition (d0): ", object@metadata$doi, "\n",
    "New steady state isotopic composition (df):", object@metadata$dfi, "\n",
    "Tissue-specific isotopic turnover rate (lambda):", object@metadata$lambda, "\n",
    "Generated: ", paste(object@timestamp))
})

#' Edit ISO objects.
#' @export

ISOedit <- function(object, update){
  object@data <- update
  return(object)
}

#' Generates a named list.
#'
#' @param .tbl text
#' @usage
#' text
#' @examples
#' test
#' @return
#' text
#' @export

group_splitn <- function(.tbl, data.names){
    select(.tbl, data.names) %>%
      gather(key="Isotope", value="value") %>%
      group_split(Isotope, .keep=F) %>%
      rlang::set_names(unique(data.names))
}

#' Class filter list
#'
#' @param ls() text. Checks global environment for current ISO/MISO object names.
#' @usage
#' text
#' @examples
#' test
#' @return
#' text
#' @export

ISOfind <- function(x){inherits(get(x), "ISO")}



