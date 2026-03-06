
#' @title \link[ordinal]{clm} and \link[ordinal]{clmm} Objects
#' 
#' @examples
#' library(ordinal)
#' ?ordinal::clm
#' ?ordinal::clmm
#' m1 = clm(rating ~ temp + contact, data = wine)
#' m2 = clmm(rating ~ temp + contact + (1|judge), data = wine)
#' m3 = clm(Sat ~ Infl + Type + Cont, weights = Freq, data = MASS::housing)
#' library(ecip); list(
#'  '`clm`' = list(m1, m3),
#'  '`clmm`' = m2
#' ) |> fastmd::render2html()
#' @name clm_clmm
NULL


#' @importFrom ecip coef_
#' @export
coef_.clm <- function(x) x$beta
# I do not like ?ordinal:::coef.clm 

#' @importFrom ecip coef_
#' @export
coef_.clmm <- coef_.clm

# ?ordinal:::confint.clm includes only *beta* coefficients

# we do not have ordinal:::confint.clmm, as of packageDate('ordinal') 2023-12-04
# it dispatches to stats:::confint.default
# we *cannot* force ?ordinal:::confint.clm onto \link[ordinal]{clmm} object, error!!
#' @export
confint.clmm <- function(object, ...) {
  tmp <- confint.default(object, ...)
  tmp[names(object$beta), , drop = FALSE]
}




#' @importFrom ecip .pval
#' @method .pval summary.clm
#' @export
.pval.summary.clm <- function(x) {
  cf <- x$coefficients[names(x$beta), , drop = FALSE]
  ret <- cf[, 'Pr(>|z|)']
  names(ret) <- rownames(cf) # nrow-1 drops rownames
  return(ret)
}

#' @importFrom ecip .pval
#' @method .pval summary.clmm
#' @export
.pval.summary.clmm <- .pval.summary.clm


#' @importFrom ordinal clm
#' @importFrom ecip getCanonicalLink
#' @export
getCanonicalLink.clm <- function(x) eval(formals(fun = clm)$link)[1L]

#' @importFrom ordinal clmm
#' @importFrom ecip getCanonicalLink
#' @export
getCanonicalLink.clmm <- function(x) eval(formals(fun = clmm)$link)[1L]



model_matrix_clm <- ordinal:::model.matrix.clm
#' @export
model.matrix.clm <- function(object, ...) {
  # overwrite ?ordinal:::model.matrix.clm
  model_matrix_clm(object, type = 'design', ...)$X
}
# needed as of packageDate('ordinal') 2023-12-04
# ?ordinal:::model.matrix.clmm is fine!!


#' @importFrom ecip desc_
#' @export
desc_.clm <- function(x) {
  switch(lk <- x$link, logit = {
    return('propotional odds') # canonical link
  }, paste0('cumulative probability (', lk, '-link)'))
}

#' @importFrom ecip desc_
#' @export
desc_.clmm <- function(x) paste('mixed-effect', desc_.clm(x))


# @importFrom nlme ranef
#' @importFrom ordinal ranef
#' @importFrom ecip nobsText
# ?ordinal::ranef is indeed ?nlme::ranef
# I want to see if I can avoid Imports: nlme by this way!
#' @export
nobsText.clmm <- function(x) {
  ng <- vapply(ranef(x), FUN = nrow, FUN.VALUE = NA_integer_, USE.NAMES = TRUE) # ?ordinal:::ranef.clmm
  # ?ordinal:::nobs.clmm
  sprintf(fmt = '%d records from %s', 
          x$dims$nobs,
          paste(sprintf(fmt = '%d `%s`', ng, names(ng)), collapse = ' nexted in '))
}




#' @export
terms.clmm <- ordinal:::terms.clm 
# ?ordinal:::terms.clm function interface is function(x, type, ...)
# otherwise ?ordinal:::get_clmDesign inside ?ordinal:::model.matrix.clm will have error


#' @importFrom fastmd md_
#' @importFrom ecip md_ecip
#' @export
md_.clm <- md_ecip

#' @importFrom fastmd md_
#' @importFrom ecip md_ecip
#' @export
md_.clmm <- md_ecip







