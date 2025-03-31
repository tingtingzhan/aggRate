

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
#' @importFrom ggplot2 autolayer facet_grid label_both geom_line
#' @importFrom reshape2 melt
#' @export autolayer.aggRate
#' @export
autolayer.aggRate <- function(object, type = c('plain', 'cumulative', 'acat', 'cratio'), ...) {
  
  rnm <- deparse1(attr(object, which = 'formula', exact = TRUE)[[2L]])  
  rc <- object[[rnm]] # 'cbind-ed ratings'
  id.vars <- setdiff(names(object), rnm)
  
  dats <- lapply(list(
    obs = rc/rowSums(rc),
    yhat = attr(object, which = 'yhat', exact = TRUE),
    #lower = object@.lower,
    #upper = object@.upper,
    cumulative = link_flatRate(object, type = 'cumulative'),
    acat = link_flatRate(object, type = 'acat'),
    cratio = link_flatRate(object, type = 'cratio')
  ), FUN = \(i) {
    if (!length(i)) return(invisible())
    melt(data = data.frame(object[id.vars], i, check.names = FALSE), 
         id.vars = id.vars, measure.vars = colnames(i), variable.name = rnm)
  })
  
  type <- match.arg(type)
  
  mp <- eval(call(name = 'aes', x = as.symbol(rnm), y = quote(value), group = as.symbol(id.vars[1L]), colour = as.symbol(id.vars[1L])))
  list(
    switch(type, plain = list(
      geom_line(data = dats$obs, mapping = mp),
      if (length(dats$yhat)) geom_line(data = dats$yhat, mapping = mp, linetype = 2L)
    ), geom_line(data = dats[[type]], mapping = mp)),
    # not plotting the confidence interval yet
    if (length(id.vars) > 1L) {
      facet_grid(rows = call(name = '~', str2lang(paste(id.vars[-1L], collapse = '+')), quote(.)), 
                 labeller = label_both)
    }
  )
  
}



#' @importFrom ggplot2 autoplot ggplot scale_y_continuous labs
#' @export
autoplot.aggRate <- function(object, type = c('plain', 'cumulative', 'acat', 'cratio'), ...) {
  type <- match.arg(type)
  ggplot() + 
    autolayer.aggRate(object, type = type, ...) +
    switch(type, plain = {
      scale_y_continuous(labels = function(x) sprintf(fmt = '%.0f%%', 1e2*x))
    }, scale_y_continuous(trans = 'logit')) +
    labs(y = switch(type, 
                    plain = 'Conditional Probs.', 
                    cumulative = 'Logit Conditional Cumulative Probs.', 
                    acat = 'Log Adjacent-Categories-Ratio', 
                    cratio = 'Logit Continuation-Ratio'))
}



