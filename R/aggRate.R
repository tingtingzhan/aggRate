
#' @title Create Aggregated 'Flat' Format of Rating Data
#' 
#' @description
#' 
#' Function [aggRate()] creates aggregate counts rating data *flat* format,
#' for \link[VGAM]{vglm} as well as human eyes.
#' Each row represents the aggregated rating records for a single *product*, 
#' and each column denotes the levels of rating (e.g. \link[VGAM]{wine}).
#' 
#' @param object ..
#' 
#' @param formula ..
#' 
#' @param data ..
#' 
#' @param pattern,rating ..
#' 
#' @param ... ..
#' 
#' @importFrom matrixStats rowCumsums
#' 
#' @examples 
#' VGAM::wine |>
#'  aggRate.data.frame(pattern = c(bitter = '^bitter'))
#' 
#' ordinal::wine |> 
#'  aggRate.formula(formula = rating ~ temp + contact)
#'  
#' m = MASS::housing |>
#'  reshape2::dcast(formula = Infl + Type + Cont ~ Sat, value.var = 'Freq') |>
#'  aggRate.data.frame(rating = list(Sat = c('Low', 'Medium', 'High')))
#' # printing method has a bug.
#' 
#' library(ordinal)
#' clm(rating ~ temp * contact, data = wine) |>
#'  aggRate.clm()
#' # aggRate(housing_clm) # I plan to deprecate
#' 
#' library(VGAM)
#' pneumo2 = VGAM::pneumo |>
#'  aggRate.data.frame(rating = list(y = c('normal', 'mild', 'severe')))
#' (m = vglm(y ~ log(exposure.time), family = multinomial, data = pneumo2, model = TRUE))
#' m |> aggRate.vlm()
#' 
#' @name aggRate
#' @export
aggRate <- function(object, ...) UseMethod(generic = 'aggRate')

#' @rdname aggRate
#' @export aggRate.data.frame
#' @export
aggRate.data.frame <- function(object, pattern, rating, ...) {
  
  # \pkg{VGAM}-type of data, see ?VGAM::wine
  data <- object; object <- NULL
  if (anyNA(data)) stop('do not allow missing in input')
  
  nm <- names(data)
  if (!identical(nm, make.names(nm))) stop('input column names must be syntactically valid')
  
  if (!missing(pattern) || !missing(rating)) {
    if (!missing(pattern)) {
      new_nm <- names(pattern)
      id <- grepl(pattern = pattern, x = nm)
    } else if (!missing(rating)) {
      new_nm <- names(rating)
      id <- match(rating[[1L]], table = nm)
    }
    
    if (!length(new_nm)) stop('`pattern` must be named')
    if (!identical(make.names(new_nm), new_nm)) stop()
    if (new_nm %in% nm) stop()
    data[[new_nm]] <- as.matrix.data.frame(data[id])
    data[nm[id]] <- NULL
    nm <- names(data)
  }
  
  id <- vapply(data, FUN = is.matrix, FUN.VALUE = NA)
  if (sum(id) != 1L) stop('input must contain one and only one matrix-column')
  
  storage.mode(data[[nm[id]]]) <- 'integer' # need to check if they are really 'integer'
  if (!length(colnames(data[[nm[id]]]))) {
    colnames(data[[nm[id]]]) <- seq_len(dim(data[[nm[id]]])[2L])
  }

  class(data) <- c('aggRate', class(data))
  attr(data, which = 'formula') <- eval(call(name = '~', as.symbol(nm[id]), str2lang(paste(nm[!id], collapse = '+'))), envir = .GlobalEnv)
  return(data)

}



#' @rdname aggRate
#' @importFrom stats aggregate
#' @export aggRate.formula
#' @export
aggRate.formula <- function(formula, data, ...) {
  
  # \pkg{ordinal}-type of data, see ?ordinal::wine
  
  # check `formula` is two-sided; check `formula` is not recursive
  #if (inherits(formula, what = 'terms')) class(formula) <- 'formula' # do I still need this??
  y <- formula[[2L]]
  if (!is.symbol(y)) stop('Response term must be \'symbol\'')
  yval <- eval(y, envir = data)
  if (is.matrix(yval)) .Defunct(new = 'aggRate.data.frame')
  
  if (!is.ordered(data[[y]])) data[[y]] <- ordered(data[[y]])

  ret <- aggregate(formula, data = data, FUN = table) # wow!
  # ?base::table return is typeof-integer
  class(ret) <- c('aggRate', class(ret))
  attr(ret, which = 'formula') <- formula
  return(ret)
  
}




#' @rdname aggRate
#' @importFrom stats formula predict
#' @export aggRate.clm
#' @export
aggRate.clm <- function(object, ...) {
  call_ <- object$call
  if (!is.data.frame(data <- eval(call_$data))) stop('`data` must be evaluable')
  if (length(call_$weights)) stop('using of `weights` has been discontinued')
  ret <- aggRate.formula(formula = formula(object), data = data)
  attr(ret, which = 'yhat') <- cast_predict.clm(object)
  return(ret)
}


cast <- reshape2:::cast
array_names <- reshape2:::array_names
cast_predict.clm <- function(object, ...) {
  
  fom <- formula(object)
  cast_fom <- eval(call(name = '~', fom[[3L]], fom[[2L]]))
  m <- unique.data.frame(object$model)
  m$yhat <- predict(object, newdata = m, interval = FALSE)$fit
    
  pred_cast <- cast(data = m, formula = cast_fom, value.var = 'yhat', drop = FALSE)
  
  ret <- pred_cast$data
  ret[is.na(ret)] <- 0
  colnames(ret) <- array_names(pred_cast$labels[[2]]) # see ?reshape2::dcast
  return(ret)
  
} 



#' @rdname aggRate
#' @importFrom VGAM formulavlm
#' @export aggRate.vlm
#' @export
aggRate.vlm <- function(object, ...) {
  #if (!is.data.frame(data <- eval(object@call$data))) stop('`data` must be evaluable')
  if (!length(data <- object@model)) stop('re-run VGAM::vglm with `model = TRUE`')
  fom <- formulavlm(object) # ?VGAM::formula.vlm; ?VGAM::formulavlm
  if (!is.symbol(fom[[2L]])) stop('Response term must be \'symbol\'')
  # fit <- predict(object, se.fit = TRUE) # No!!
  class(data) <- unique.default(c('aggRate', class(data)))
  attr(data, which = 'formula') <- fom
  attr(data, which = 'yhat') <- object@fitted.values # ?VGAM::fittedvlm
  # `yhat`: old name `@.est`
  return(data)
}




#' @importFrom stats nobs
#' @export
nobs.aggRate <- function(object, ...) sum(object[[deparse1(attr(object, which = 'formula', exact = TRUE)[[2L]])]])







#getMethod(f = 'familyname', signature = 'vlm')
#' @importFrom VGAM familyname.vlm
#' @export
autoplot.vlm <- function(object, type = familyname.vlm(object), ...) {
  cl <- match.call()
  if (!length(cl$type) || cl$type != 'plain') {
    cl$type <- 'plain'
    message('Use ', deparse1(cl), ' to see the conditional probabilities')
  }
  autoplot.aggRate(aggRate.vlm(object), type = type, ...)
} 





#' @export
print.aggRate <- function(x, ...) {
  print(autoplot.aggRate(x, ...))
}


# 
# \itemize{
# 
# \item{cumulative} {(no confidence ribbon) 
# cumulative probabilities with x-variable design matrix}
# 
# \item{acat} {(no confidence ribbon)
# probability ratios of adjacent categories,
# `ln(P[Y=j+1]/P[Y=j])`} (without `ln`, 
# which is accomplished by axis transformation in \link[ggplot2]{ggplot}) 
# as in `@@family@@blurb` of 'vglm' object.
# 
# \item{cratio} {(no confidence ribbon)
# conditional probability of 
# scoring higher than a specific category, 
# `logit(P[Y>j|Y>=j])` (without `logit`, 
# which is accomplished by axis transformation in \link[ggplot2]{ggplot}) 
# as in `@@family@@blurb` of 'vglm' object.}
link_flatRate <- function(x, type = c('cumulative', 'acat', 'cratio'), ...) {
  
  # look at the use of `@family@linkinv` in ?VGAM::fittedvlm
  fom <- attr(x, which = 'formula', exact = TRUE)
  rnm <- deparse1(fom[[2L]])
  rc <- x[[rnm]]
  rlevels <- colnames(rc)
  n <- length(rlevels)
  prob <- attr(x, which = 'yhat', exact = TRUE) %||% rc / rowSums(rc)
  
  
  ret <- switch(match.arg(type), cumulative = {
    cumulative <- rowCumsums(prob)[, -n, drop = FALSE] # matrix
    # cumulative is indeed plogis(VGAM::cumulative()@linkfun(prob))
    colnames(cumulative) <- paste0(rlevels[-n], '-')
    cumulative
    
  }, acat = {
    acat <- prob[, -1L, drop = FALSE] / prob[, -n, drop = FALSE] # matrix
    # acat is indeed exp(VGAM::acat()@linkfun(prob))
    colnames(acat) <- paste0(rlevels[-1L], '/', rlevels[-n])
    acat
    
  }, cratio = {
    revcum <- rCumsums_rev(prob) # matrix
    cratio <- revcum[, -1L, drop = FALSE] / revcum[, -n, drop = FALSE]
    # VGAM::cratio()@linkfun(prob) # gives error?
    colnames(cratio) <- paste0(rlevels[-1L], '+|', rlevels[-n], '+')
    cratio
    
  })
  
  if (!length(ret) || !is.matrix(ret)) stop('rewrite code so that ret is always matrix')
  return(ret)

}



rCumsums_rev <- function(x) { # slow anyway..
  nc <- dim(x)[2L]
  if (nc < 2L) return(x)
  id <- nc:1
  rowCumsums(x, cols = id)[, id, drop = FALSE]
}

