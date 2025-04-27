

#' @title autolayer.aggRate
#' 
#' @description ..
#' 
#' @param object ..
#' 
#' @param type ..
#' 
#' @param ... ..
#' 
#' @importFrom geomtextpath geom_textline
#' @importFrom ggplot2 autolayer aes facet_grid label_both geom_line scale_y_continuous labs
#' @importFrom reshape2 melt
#' @importFrom rlang .data
#' @importFrom scales label_percent
#' @export autolayer.aggRate
#' @export
autolayer.aggRate <- function(object, type = c('plain', 'cumulative', 'acat', 'cratio'), ...) {
  
  rnm <- deparse1(attr(object, which = 'formula', exact = TRUE)[[2L]])  
  rc <- object[[rnm]] # 'cbind-ed ratings'
  id.vars <- setdiff(names(object), rnm)
  
  dats <- list(
    obs = rc/rowSums(rc),
    yhat = attr(object, which = 'yhat', exact = TRUE),
    #lower = object@.lower,
    #upper = object@.upper,
    cumulative = link_flatRate(object, type = 'cumulative'),
    acat = link_flatRate(object, type = 'acat'),
    cratio = link_flatRate(object, type = 'cratio')
  ) |>
    lapply(FUN = \(i) {
      if (!length(i)) return(invisible())
      data.frame(object[id.vars], i, check.names = FALSE) |>
        melt(id.vars = id.vars, measure.vars = colnames(i), variable.name = rnm)
    })
  
  type <- match.arg(type)
  
  mp <- aes(x = .data[[rnm]], y = .data$value, 
            group = .data[[id.vars[1L]]], 
            colour = .data[[id.vars[1L]]],
            label = .data[[id.vars[1L]]])
  
  mp_ <- mp
  mp_$label <- NULL
  
  list(
    
    switch(type, plain = {
      list(
        geom_textline(data = dats$obs, mapping = mp, ..., show.legend = FALSE),
        if (length(dats$yhat)) {
          geom_line(data = dats$yhat, mapping = mp_, linetype = 2L, show.legend = FALSE)
        }
      )
    }, {
      geom_textline(data = dats[[type]], mapping = mp, ..., show.legend = FALSE)
    }),
    
    # not plotting the confidence interval yet
    if (length(id.vars) > 1L) {
      facet_grid(rows = call(name = '~', str2lang(paste(id.vars[-1L], collapse = '+')), quote(.)), 
                 labeller = label_both)
    },
    
    switch(type, plain = {
      scale_y_continuous(labels = label_percent())
    }, scale_y_continuous(labels = label_percent(), trans = 'logit')),
    
    labs(y = switch(type, 
                    plain = 'Conditional Probs.', 
                    cumulative = 'Logit Conditional Cumulative Probs.', 
                    acat = 'Log Adjacent-Categories-Ratio', 
                    cratio = 'Logit Continuation-Ratio'))
    
  )
  
}



#' @importFrom ggplot2 autoplot ggplot
#' @export
autoplot.aggRate <- function(object, hjust = .1, ...) {
  ggplot() + autolayer.aggRate(object, hjust = hjust, ...)
}



