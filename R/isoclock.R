#' Calculates the residence time (days) for isotope values using the isoclock
#' equation.
#'
#' @param object (Optional). Isotope object (\link[isoclockR]{ISO}) or
#' multiple isotope object (\link[isoclockR]{MISO}) that
#' contains both relevant metadata and one or more isotope measurements. This
#' object is not required if running arbitrary distributions of values for
#' each parameter (i.e., \code{doi}, \code{dfi}, \code{dt}, and \code{lambda}).
#' @param doi Reference origin steady state isotopic composition (\eqn{\delta0}).
#' @param dfi Reference new steady state isotopic composition (\eqn{\deltaf}).
#' @param dt Isotope measurements (\eqn{\deltat}).
#' @param lambda Tissue-specific isotopic turnover rate (\eqn{\lambda}).
#' @param isotopes (Optional). Specify which isotopes to use in the isoclock
#' equation.
#' @param metasep (Optional). Separate out isotope measurements by specific
#' metadata variables such as tissue-type, year, geographic region, etc.
#' @param verbose Boolean (TRUE/FALSE) flag to print out diagnostics after each
#' model run.
#' @param create Boolean (TRUE/FALSE) flag to create an \link[isoclockR]{ISO}
#' object if arbitrary values are used.
#' @return Residence time (days) of an isotope.
#' @usage
#' isoclock(ISO_object)
#' @details
#' Calculates the residence time (days) of an individual (\link[isoclockR]{ISO}).
#'
#' @references
#' Klaasen, M., Piersma, T., Korthals, H., Dekinga, A., and Dietz, M.W. 2010. Single-point isotope measurements in blood cells and plasma to estimate
#' the time since diet switches. Functional Ecology, 24: 794-804.
#'
#' Shipley, O., Newton, A., Frisk, M., Henkes, G., Walters, H., LaBella, J., Hyatt, M., and Olin, J. In Review. Telemetry validated nitrogen stable isotope clocks
#' identify ocean-to-estuarine habitat shifts in mobile organisms. Methods in Ecology and Evolution.
#'
#' @export
isoclock <- function(object=NULL, doi=NA, dfi=NA, lambda=NA, dt=NA,
                     data.names=NULL, seperator=NULL,
                     verbose=T, create=T, rejection_sample = F){
  #solve equation for specific values
  if(is.null(object)){
    object <- ISOgen(data=dt, doi=doi, dfi=dfi, lambda=lambda,
                     data.names=ifelse(!is.null(data.names), data.names, "isotope"))
  }

  if(class(object) == "ISO"){
    object <- suppressWarnings(ISOedit(object))
  }else if(class(object) == "MISO"){
    object <- suppressWarnings(MISOedit(object, rejection_sample))
  }else{
    stop("isoclock(...) requires either specific inputs for each parameter, or an ISO and/or MISO object.")
  }

  # if(verbose == T){
  #   if(class(object) == "ISO"){
  #     cat(paste("ISO object edited: ", deparse(substitute(object)), sep = ""), "\n", "Residence times calculated for ", nrow(object@data[[1]]),
  #         " samples!", "\n", sep = "")
  #   }else if(class(object) == "MISO"){
  #     cat(paste("ISO object edited: ", deparse(substitute(object)), sep = ""), "\n", "Residence times calculated for ", length(object@data),
  #         " animals!", "\n", sep = "")
  #   }
  # }

  if(create == T){
    return(object)
  }else{
    tmp <- ISOpull(object)
    return(tmp %>% as.data.frame())
  }
}

#' Wrapper function to calculate isotope residence for ISO and MISO objects
#' @export
REScalc <- function(object, index){
  if(class(object) == "ISO"){
    params <- get_param(object)
  }else if(class(object) == "MISO"){
    params <- get_param_sub(object, index)
  }
  resval <- isosub(params)
  return(resval)
}

#' Wrapper function to calculate isotope residence
#' @export
isosub <- function(params){
  return(log((params$doi - params$dfi)/(params$dt - params$dfi))/params$lambda)
}
#' @rdname isosub
#'
isoreal <- function(doi, dfi, dt, lambda){
  return(log((doi - dfi)/(dt - dfi))/lambda)
}

#' Rejection sampling
#' @export
rejection_sample <- function(object, index=NULL){
  if(class(object) == "ISO"){
    par <- get_param(object)
  }else{
    par <- get_param_sub(object, index)
  }
  dt <- par$dt; doi <- par$doi
  dfi <- par$dfi; lambda <- par$lambda
  idx <- seq_len(length(dt))
  res_original <- suppressWarnings(isoreal(doi, dfi, dt, lambda))
  res <- res_original
  idx <- seq_len(length(dt))

  while(length(idx) > 0){
    idx <- which(is.na(res) | res < 0 | res > 400)
    dt[idx] <- sample(dt, length(idx), replace=T)
    doi[idx] <- sample(doi, length(idx), replace=T)
    dfi[idx] <- sample(dfi, length(idx), replace=T)
    lambda[idx] <- sample(lambda, length(idx), replace=T)
    res[idx] <- suppressWarnings(isoreal(doi[idx], dfi[idx], dt[idx], lambda[idx]))
  }

  if(class(object) == "MISO"){
    object@data[[index]][[sym(par$parameter[1])]]$residence_rejection <- res
    object@data[[index]][[sym(par$parameter[1])]]$residence <- res_original
  }else{
    object@data[[sym(par$parameter[1])]]$residence_rejection <- res
    object@data[[sym(par$parameter[1])]]$residence <- res_original
  }
  return(object)
}


