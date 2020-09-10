#' Method for plotting residence times from ISO objects.
#' @export
#' @import viridis
#' @import tidyverse

resplot <- function(object, y = y, tail.prob = TRUE,
                    tail.summary = FALSE,
                    data.names=NULL, x=NULL, seperator=NULL,
                    xlim=NULL, ylim=NULL){
  if(!is.null(data.names)){
    residx <- which(names(object@data) == data.names)
  }else{
    residx <- seq_len(length(names(object@data)))
  }

  resext <- object@data %>%
    tibble() %>%
    mutate(Along = 1:n()) %>%
    slice(residx) %>%
    unnest(cols = c(.))
  pcts <- tailprob(resext[[sym(y)]],
                   force = ifelse(sym(y) == "residence_rejection", T, F))

  if(is.null(xlim)){
    xlim <- c(min(pcts$x) - 5, max(pcts$x) + 5)
  }else{
    if(sym(y) == "residence_rejection"){
      pcts <- pcts
    }else{
      pcts <- tailprob(resext[[sym(y)]],
                       force = ifelse(sym(y) == "residence_rejection", T, F),
                       xlim = xlim)
    }
  }

  if(is.null(ylim)){
    ylim <- c(0, max(pcts$y)*1.01)
  }

  pobj <- ggplot(data=pcts, aes(x=x, y=y)) +
    geom_line(size=2) +
    theme_bw() +
    theme(text = element_text(size = 18),
          axis.text = element_text(size = 16, color = "black"),
          panel.grid = element_blank()) +
    labs(y = "Kernel density", x = "Residence time (days)") +
    coord_cartesian(xlim=xlim, ylim=ylim, expand=0)

  if(tail.prob == T){
    pobj <- pobj +
      geom_segment(aes(xend = x, yend = 0, colour = 0.5 - abs(0.5 - run))) +
      labs(color = "Tail probability") +
      scale_color_viridis(direction = -1)
  }

  return(pobj)
}

#' Calculating along-axis ECDF for gradient fill.
#' @export
tailprob <- function(x, force=F, xlim=NULL) {
    x <- x[!is.na(x)]
    n <- length(x)
    if(force == T){
      d <- density(x, from = min(x), to = max(x, na.rm = TRUE), n = n)
      ecdf <- c(0, cumsum(d$y[1:(n - 1)] * (d$x[2:n] - d$x[1:(n - 1)])))
      return(data.frame(x = c(0, d$x, max(d$x)+0.01), y = c(0,d$y,0), run = c(0,ecdf,0)))
    }else{
      if(!is.null(xlim)){
        d <- density(x, from = xlim[1], to = xlim[2], n = n)
        ecdf <- c(0, cumsum(d$y[1:(n - 1)] * (d$x[2:n] - d$x[1:(n - 1)])))
      }else{
        d <- density(x, from = min(x, na.rm = TRUE), to = max(x, na.rm = TRUE), n = n)
        ecdf <- c(0, cumsum(d$y[1:(n - 1)] * (d$x[2:n] - d$x[1:(n - 1)])))
      }
      return(data.frame(x = d$x, y = d$y, run = ecdf))
    }
}

#' MISO plotting.
#' @export
miso_resplot <- function(object, y, tail.prob = TRUE,
                         data.names=NULL, x=NULL, seperator=NULL,
                         xlim=NULL, ylim=NULL, facet=NULL, quantile=F,
                         tail.summary=F){
  if(is.null(facet)){
    df <- data.frame(Along=c(), x=c(), y=c(), run=c())

    for(i in seq_len(length(object))){
      tmpdf <- MISOpull(object, i) %>%
        select(!!sym(y)) %>%
        mutate(Along=i, x=!!sym(y))

      if(is.null(xlim)){
        xlim <- c(min(pcts$x) - 5, max(pcts$x) + 5)
      }else{
        if(sym(y) == "residence_rejection"){
          pcts <- tailprob(tmpdf$x,
                           force = ifelse(sym(y) == "residence_rejection", T, F),
                           xlim = xlim)
        }else{
          pcts <- tailprob(tmpdf$x,
                           force = ifelse(sym(y) == "residence_rejection", T, F),
                           xlim = xlim)
        }
      }
      df <- bind_rows(df, cbind(data.frame(Along=i), pcts))
    }
  }else{
    df <- data.frame(Along=c(), x=c(), y=c(), run=c(), sep=c())
    dfall <- data.frame(Along=c(), x=c(), sep=c())

    for(i in seq_len(length(object))){
      var <- which(names(object@metadata[[i]]) == sym(facet))
      tmpdf <- MISOpull(object, i) %>%
        select(!!sym(y)) %>%
        mutate(Along=i, x=!!sym(y), sep=object@metadata[[i]][var][[1]])
      tmpn <- data.frame(x=tmpdf[,1], x=tmpdf$x, sep=tmpdf$sep)
      dfall <- bind_rows(dfall, tmpn)

      if(is.null(xlim)){
        xlim <- c(min(pcts$x) - 5, max(pcts$x) + 5)
      }else{
        if(sym(y) == "residence_rejection"){
          pcts <- tailprob(tmpdf$x,
                           force = ifelse(sym(y) == "residence_rejection", T, F),
                           xlim = xlim)
        }else{
          pcts <- tailprob(tmpdf$x,
                           force = ifelse(sym(y) == "residence_rejection", T, F),
                           xlim = xlim)
        }
      }

      df <- bind_rows(df, cbind(data.frame(Along=i), pcts, data.frame(sep=tmpdf$sep[1])))
    }
  }

  if(is.null(ylim)){
    ylim <- c(0, max(df$y)*1.01)
  }

  pobj <- ggplot(data=NULL, aes(x=x, y=y)) +
    geom_line(data=df, aes(group=Along), size=1) +
    theme_bw() +
    theme(text = element_text(size = 18),
          axis.text = element_text(size = 16, color = "black"),
          panel.grid = element_blank()) +
    labs(y = "Kernel density", x = "Residence time (days)") +
    coord_cartesian(xlim=xlim, ylim=ylim, expand=0)

  if(!is.null(facet)){
    pobj <- pobj +
      facet_wrap(~sep)
  }

  if(tail.prob == T){
    if(tail.summary == F){
      pobj <- pobj +
        geom_segment(data=df, aes(xend = x, yend = 0, colour = 0.5 - abs(0.5 - run))) +
        labs(color = "Tail probability") +
        scale_color_viridis(direction = -1)
    }else{
      sep_facet <- data.frame(x=c(), y=c(), run=c(), sep=c())
      all_facet <- data.frame(x=c(), y=c(), run=c(), sep=c())

      for(i in unique(dfall$sep)){
        index <- which(dfall$sep == i)
        tmp <- tailprob(dfall$x[index],
                        force = ifelse(sym(y) == "residence_rejection", T, F),
                        xlim = xlim)
        tmp$sep <- i
        sep_facet <- bind_rows(sep_facet, tmp)


        quants <- quantile(tmp$x, probs=c(0.025, 0.055, 0.3, 0.7, 0.945, 0.975))
        all89 <- tmp %>% filter(run > 0.055 & run < 0.945)
        all_facet <- bind_rows(all_facet, all89)
      }
      pobj <- pobj +
        geom_segment(data=sep_facet,
                     aes(xend = x, yend = 0, colour = 0.5 - abs(0.5 - run))) +
        geom_line(data=df, aes(group=Along), size=1) +
        labs(color = "Tail probability") +
        scale_color_viridis(direction = -1)
    }
  }

  if(quantile == T){
    pobj <- pobj +
      geom_boxplot(data=all_facet, aes(y=-0.00175, x=x, group=sep),
                   width=0.001, lwd=1, notch=T, outlier.shape=NA) +
      coord_cartesian(xlim=xlim, ylim=c(-0.0025, max(ylim)), expand=0) +
      facet_wrap(~sep)
  }

  return(pobj)
}
