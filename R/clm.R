
#' @title S3 methods for \link[ordinal]{clm} and \link[ordinal]{clmm} Objects
#' 
#' @param x,object \link[ordinal]{clm} or \link[ordinal]{clmm} object
#' 
#' @param type,... additional parameters for S3 method dispatches
#' 
#' @examples
#' library(ordinal)
#' ?ordinal::clm
#' ?ordinal::clmm
#' m1 = clm(rating ~ temp + contact, data = wine)
#' 
#' m2 = clmm(rating ~ temp + contact + (1|judge), data = wine)
#' m2 |> coef_.clmm()
#' m2 |> summary() |> .pval.summary.clm()
#' m2 |> confint()
#' m2 |> nobsText.clmm()
#' 
#' m3 = clm(Sat ~ Infl + Type + Cont, weights = Freq, data = MASS::housing)
#' 
#' @name S3_clm_clmm
#' @export
coef_.clm <- function(x) x$beta
# I do not like ?ordinal:::coef.clm 

#' @rdname S3_clm_clmm
#' @export
coef_.clmm <- coef_.clm

# ?ordinal:::confint.clm includes only *beta* coefficients

# we do not have ordinal:::confint.clmm, as of packageDate('ordinal') 2023-12-04
# it dispatches to stats:::confint.default
# we *cannot* force ?ordinal:::confint.clm onto \link[ordinal]{clmm} object, error!!
#' @rdname S3_clm_clmm
#' @importFrom stats confint confint.default
#' @export confint.clmm
#' @export
confint.clmm <- function(object, ...) {
  tmp <- confint.default(object, ...)
  tmp[names(object$beta), , drop = FALSE]
}




#' @rdname S3_clm_clmm
#' @export
.pval.summary.clm <- function(x) {
  cf <- x$coefficients[names(x$beta), , drop = FALSE]
  ret <- cf[, 'Pr(>|z|)']
  names(ret) <- rownames(cf) # nrow-1 drops rownames
  return(ret)
}

#' @rdname S3_clm_clmm
#' @export
.pval.summary.clmm <- .pval.summary.clm


#' @rdname S3_clm_clmm
#' @importFrom ordinal clm
#' @export
getCanonicalLink.clm <- function(x) eval(formals(fun = clm)$link)[1L]

#' @rdname S3_clm_clmm
#' @importFrom ordinal clmm
#' @export
getCanonicalLink.clmm <- function(x) eval(formals(fun = clmm)$link)[1L]



model_matrix_clm <- ordinal:::model.matrix.clm
#' @rdname S3_clm_clmm
#' @importFrom stats model.matrix
#' @export
model.matrix.clm <- function(object, ...) {
  # overwrite ?ordinal:::model.matrix.clm
  model_matrix_clm(object, type = 'design', ...)$X
}
# needed as of packageDate('ordinal') 2023-12-04
# ?ordinal:::model.matrix.clmm is fine!!


#' @rdname S3_clm_clmm
#' @export
desc_.clm <- function(x) {
  switch(lk <- x$link, logit = {
    return('propotional odds') # canonical link
  }, paste0('cumulative probability (', lk, '-link)'))
}

#' @rdname S3_clm_clmm
#' @export
desc_.clmm <- function(x) paste('mixed-effect', desc_.clm(x))


#' @rdname S3_clm_clmm
# @importFrom nlme ranef
#' @importFrom ordinal ranef
# ?ordinal::ranef is indeed ?nlme::ranef
# I want to see if I can avoid Imports: nlme by this way!
#' @export
nobsText.clmm <- function(x) {
  ng <- vapply(ranef(x), FUN = .row_names_info, type = 2L, FUN.VALUE = NA_integer_, USE.NAMES = TRUE) # ?ordinal:::ranef.clmm
  # ?ordinal:::nobs.clmm
  sprintf(fmt = '%d records from %s', 
          x$dims$nobs,
          paste(sprintf(fmt = '%d `%s`', ng, names(ng)), collapse = ' nexted in '))
}




#' @rdname S3_clm_clmm
#' @importFrom stats terms
#' @export terms.clmm
#' @export
terms.clmm <- ordinal:::terms.clm 
# ?ordinal:::terms.clm function interface is function(x, type, ...)
# otherwise ?ordinal:::get_clmDesign inside ?ordinal:::model.matrix.clm will have error








