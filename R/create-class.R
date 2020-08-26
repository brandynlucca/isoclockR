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
ISOgen <- function(data, data.names=NULL, seperator=NA, doi, dfi, lambda){
  requireNamespace("tidyverse", quietly=T)
  #Parse different isotope data; seperate into lists for appending
  if(is_tibble(data)){
    isodata <- data %>%
      group_splitn(data.names)
  }else if(is_list(data)){
    var <- names(data)
    data <- as.data.frame(data); colnames(data) <- c("value","residence")
    isodata <- data %>%
      lst() %>%
      rlang::set_names(var)
  }else if(class(data) == "numeric"){
    isodata <- data %>%
      enframe(., name=NULL) %>%
      lst() %>%
      rlang::set_names(data.names)
  }

  #Generate metadata
  metadata <- data.frame(doi=doi, dfi=dfi, lambda=lambda)

  return(new("ISO",
         metadata=metadata,
         data=isodata,
         timestamp=Sys.time()))
}
