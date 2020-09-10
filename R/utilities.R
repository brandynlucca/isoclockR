#' Method for what is printed for ISO objects.
#' @export
setMethod("show", "ISO", function(object) {
    cat(paste(is(object)[[1]], " object", sep = ""), "\n", "Samples: ", max(unlist(lapply(object@data, dim))), "\n", "Isotopes: ", unique(names(object@data)),
        "\n", "Origin steady state isotopic composition (d0): ", median(object@metadata$doi), "\n", "New steady state isotopic composition (df):",
        median(object@metadata$dfi), "\n", "Tissue-specific isotopic turnover rate (lambda):", median(object@metadata$lambda), "\n", "Generated: ",
        paste(object@timestamp))
})

#' Method for what is printed for MISO objects.
#' @export
setMethod("show", "MISO", function(object) {
    cat(paste(is(object)[[1]], " object", sep = ""), "\n", "Number of animals: ", length(object@data), "\n", "Reference sample count: ", max(unlist(lapply(object@data[[1]],
        dim))), "\n", "Isotopes: ", unique(names(object@data[[1]])), "\n", "Ref origin steady state isotopic composition (d0): ", median(object@metadata[[1]]$doi),
        "\n", "Ref new steady state isotopic composition (df):", median(object@metadata[[1]]$dfi), "\n", "Ref tissue-specific isotopic turnover rate (lambda):",
        median(object@metadata[[1]]$lambda), "\n", "Generated: ", paste(object@timestamp))
})

#' Method for what is printed for MISO objects.
#' @export
setMethod("summary", "ISO", function(object) {
    cat(paste(class(object)[[1]], " object", sep = ""), "\n", "Number of samples: ", max(unlist(lapply(object@data, dim))), "\n", "Isotopes measured: ",
        unique(names(object@data)), "\n", "Median origin steady state isotopic composition (d0): ", median(object@metadata$doi), "\n", "Median new location steady state isotopic composition (df): ",
        median(object@metadata$dfi), "\n", "Median tissue-specific isotopic turnover rate (lambda): ", median(object@metadata$lambda), "\n", "Median residence time (days): ",
        pull(object@data[[sym(unique(names(object@data)))]], residence) %>% tibble %>% summarise(val = quantile(., probs = 0.5, names = F, na.rm = T)) %>%
            .$val, "[", pull(object@data[[sym(unique(names(object@data)))]], residence) %>% tibble %>% summarise(val = quantile(., probs = 0.025,
            names = F, na.rm = T)) %>% .$val, ",", pull(object@data[[sym(unique(names(object@data)))]], residence) %>% tibble %>% summarise(val = quantile(.,
            probs = 0.975, names = F, na.rm = T)) %>% .$val, ", 95% CI]", "\n", "Median residence time (days) -- trimmed [0, Inf]: ", pull(object@data[[sym(unique(names(object@data)))]],
            residence) %>% tibble %>% filter(. > 0) %>% na.omit() %>% summarise(val = quantile(., probs = 0.5, names = F)) %>% .$val, "[", pull(object@data[[sym(unique(names(object@data)))]],
            residence) %>% tibble %>% filter(. > 0) %>% na.omit() %>% summarise(val = quantile(., probs = 0.025, names = F)) %>% .$val, ",", pull(object@data[[sym(unique(names(object@data)))]],
            residence) %>% tibble %>% filter(. > 0) %>% na.omit() %>% summarise(val = quantile(., probs = 0.975, names = F)) %>% .$val, ", 95% CI]",
        sep = "")
})

#' Edit ISO objects.
#' @export

ISOedit <- function(object, ...){
  params <- get_param(object)
  res <- REScalc(object)
  object@data[[sym(params$parameter[1])]]$residence <- res
  return(object)
}

#' @rdname ISOedit
MISOedit <- function(object, rejection = F){
  nn <- seq_len(length(object@data))

  for(i in nn){
    if(rejection == F){
      params <- get_param_sub(object, i)
      res <- REScalc(object, i)
      object@data[[i]][[sym(params$parameter[1])]]$residence <- res
    }else{
      object <- rejection_sample(object, i)
    }
  }
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

group_splitn <- function(.tbl, data.names) {
    select(.tbl, data.names) %>% gather(key = "Isotope", value = "value") %>% group_split(Isotope, .keep = F) %>% rlang::set_names(unique(data.names))
}

#' Class filter list
#'
#' @param object text. Checks global environment for current ISO/MISO object names.
#' @usage
#' text
#' @examples
#' test
#' @return
#' text
#' @export

ISOfind <- function(object) {
    inherits(get(object), "ISO")
}

#' Add metadata
#'
#' @param ISOobject text. Edits this object.
#' @usage
#' text
#' @examples
#' test
#' @return
#' text
#' @export
ISOadd <- function(object, update, data.names=NULL){
  to_add <- data.frame(update)

  if(!is.null(data.names)){
    colnames(to_add) <- data.names
    to_add_sub <- to_add %>%
      select(all_of(data.names))
  }else{
    to_add_sub <- to_add
  }
  vars <- colnames(to_add_sub)
  tmpobj <- object

  for(i in seq_len(length(vars))){
    tmpobj@metadata[[sym(vars[i])]] <- to_add_sub[[sym(vars[i])]]
  }

  return(tmpobj)
}

#' Pull values
#'
#' @param ISOobject text. Pull values
#' @usage
#' text
#' @examples
#' test
#' @return
#' text
#' @export
ISOpull <- function(object, data.names=NULL){
  if(!is.null(data.names)){
    return(object@data[[sym(data.names)]])
  }
  else{
    return(object@data)
  }
}

#' @rdname ISOedit
MISOpull <- function(object, index, data.names=NULL){
  if(!is.null(data.names)){
    return(object@data[[index]][[sym(data.names)]])
  }
  else{
    return(object@data[[index]][[1]])
  }
}

