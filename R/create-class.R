#' Generate ISO-class object.
#'
#' @param x text
#'
#' @usage
#' ISOgen(x)
#'
#' @examples
#' text
#'
#' @return
#' Generates ISO-class object.
#'
#' @export
#' @import tidyverse
ISOgen <- function(data, data.names = NULL, seperator = NA, doi, dfi, lambda) {
    requireNamespace("tidyverse", quietly = T)
    # Parse different isotope data; seperate into lists for appending
    if (is_tibble(data)) {
        isodata <- data %>% group_splitn(data.names)
    } else if (is_list(data)) {
        var <- names(data)
        data <- as.data.frame(data)
        colnames(data) <- c("value", "residence")
        isodata <- data %>% lst() %>% rlang::set_names(var)
    } else if (class(data) == "numeric") {
        isodata <- data %>% enframe(., name = NULL) %>% lst() %>% rlang::set_names(data.names)
    }

    # Generate metadata
    metadata <- data.frame(doi = doi, dfi = dfi, lambda = lambda)

    return(new("ISO", metadata = metadata, data = isodata, timestamp = Sys.time()))
}

#' Generate MISO-class object.
#'
#' @param x text
#'
#' @usage
#' MISOgen(x)
#'
#' @examples
#' text
#'
#' @return
#' Generates MISO-class object.
#'
#' @export
#' @import tidyverse

MISOgen <- function(ISOobjects) {
    requireNamespace("tidyverse", quietly = T)

    # Wrap ISO objects for MISO
    wrapped <- c(ISOobjects) %>% tibble()

    ma <- list()
    da <- list()

    for (i in seq_len(nrow(wrapped))) {
        md <- wrapped %>% slice(i) %>% unlist()
        m <- md[[1]]@metadata
        d <- md[[1]]@data
        ma[[i]] <- m
        da[[i]] <- d
    }

    return(new("MISO", metadata = ma, data = da, timestamp = Sys.time()))
}

#' Parameter pull function.
#'
#' @description
#' Function for pulling parameters
#' @export

get_param <- function(object){
  params <- new("param",
                parameter=names(object@data),
                dt=object@data[[sym(names(object@data))]]$value,
                doi=object@metadata$doi, dfi=object@metadata$dfi,
                lambda=object@metadata$lambda
  )
  params_df <- as.data.frame(
    setNames(
      lapply(slotNames(params),
             function(x){
               slot(params, x)
             }),
      slotNames(params))
  )

  return(params_df)
}

#' Value pull
#'
#' @description
#' Function for pulling data values
#' @export

get_param_sub <- function(object, index){
  newobj <- ISOgen(data=object@data[[index]][[sym(names(object@data[[index]]))]]$value,
                   data.names=names(object@data[[index]]),
                   doi=object@metadata[[index]]$doi,
                   dfi=object@metadata[[index]]$dfi,
                   lambda=object@metadata[[index]]$lambda)
  param_sub <- get_param(newobj)

  return(param_sub)
}

#' Pull data length
#'
#' @description
#' Function for getting MISO length
#' @export
setMethod("length", "MISO",
          function(x){
            length(x@data)
          })

