#' Calculate residence time for an individual.
#'
#' @param animal text
#'
#' @usage
#' isoclock(animal)
#'
#' @details
#' Calculates the residence time (days) of an individual (\link[isoclockR]{ISO}).
#'
#' @return
#' Residence time (days)
#'
#' @references
#' Klaasen, M., Piersma, T., Korthals, H., Dekinga, A., and Dietz, M.W. 2010. Single-point isotope measurements in blood cells and plasma to estimate
#' the time since diet switches. Functional Ecology, 24: 794-804.
#'
#' Shipley, O., Newton, A., Frisk, M., Henkes, G., Walters, H., LaBella, J., Hyatt, M., and Olin, J. In Review. Telemetry validated nitrogen stable isotope clocks
#' identify ocean-to-estuarine habitat shifts in mobile organisms. Methods in Ecology and Evolution.
#'
#' @export
isoclock <- function(animal=NULL, doi=animal@metadata$doi, dfi=animal@metadata$dfi, dt=animal@data,
                     lambda=animal@metadata$lambda, data.names=NULL, create=F){
  requireNamespace("dplyr", quietly=T)
  #Secondary check if animal is accidentally used as another variable
  if("animal" %in% Filter(ISOfind, ls())){
    warning("Variable 'animal' is currently in use and conflicts with isoclock function. Please consider deleting or renaming.")
  }

  #Secondary function
  isosub <- function(x){
    return(log((doi - dfi)/(x - dfi))/lambda)
  }

  #Test if animal object or arbitrary values
  if(!is.null(animal)){
    #Calculate residency
    resval <- map(dt, .f = ~ isosub(.$value))
    #Append to paired values within each isotope field
    appISO <- Map(cbind, dt, residence=resval)
  }else{
    resval <- data.frame(residence=isosub(dt))
    appISO <- Map(cbind, as.data.frame(dt), residence=resval)
    names(appISO) <- data.names
  }

  #Check for ISO object -- if not, create one
  if(is.null(animal)){
    flag <- 0
    while(flag == 0){
      varlist <- Filter(ISOfind, ls())
      hypname <- paste("animal",sample(1:999,1), sep="")
      if(!(hypname %in% varlist)){
        flag <- 1
        environ <- if(create == T) .GlobalEnv else baseenv()
        cat(paste("ISO object created: ", hypname, sep=""), "\n",
            "Residence times calculated for ", nrow(appISO[[1]])," samples!", sep="")
        return(assign(hypname, ISOgen(data=appISO, data.names=names(appISO), doi=doi, dfi=dfi, lambda=lambda),
               envir = environ))
      }
    }
  }else{
    cat(paste("ISO object edited: ", deparse(substitute(animal)), sep=""), "\n",
        "Residence times calculated for ", nrow(appISO[[1]])," samples!", "\n", sep="")
    ISOedit(animal, appISO)
  }
}

