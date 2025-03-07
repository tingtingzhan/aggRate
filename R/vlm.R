
#' @title Get Family from \link[VGAM]{vlm} Object
#' 
#' @param object \link[VGAM]{vlm} object
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [family.vlm] returns a `'vglmff'` object.
#' 
#' @importFrom stats family
#' @export family.vlm
#' @export
family.vlm <- function(object, ...) object@family # 'vglmff'
# so that I can use my [getLink.default]




#' @title S3 methods of \link[VGAM]{vlm} and \link[VGAM]{vglm} Objects
#' 
#' @param x \link[VGAM]{vlm} or \link[VGAM]{vglm} object
#' 
#' @name S3_vlm
#' @importFrom VGAM summaryvglm
#' @export
.pval.vglm <- function(x) {
  x |> 
    summaryvglm() |> 
    .pval.summary.vglm()
}
# getMethod('summary', signature(object = 'vglm')) # ?VGAM::summaryvglm
# getMethod('summary', signature(object = 'vlm')) # ?VGAM::summaryvlm


#' @rdname S3_vlm
#' @export
.pval.summary.vglm <- function(x) {
  cf <- x@coef3
  ret <- cf[, 'Pr(>|z|)']
  names(ret) <- rownames(cf) # nrow-1 drops rownames
  return(ret)
}

# I don't know a 'vlm' example yet



# @rdname S3_vlm
# @export
#desc_.vlm <- function(x) {
#  .Defunct(msg = 'make this [desc_.default] in \pkg{tzh}')
#  ff <- x@family # 'vglmff'
#  lnk <- getLink.vglmff(ff)
#  clnk <- getCanonicalLink.vglmff(ff)
#  if (lnk == clnk) return(familyname.vglmff(ff))
#  return(paste0(familyname.vglmff(ff), ' (with ', xlnk, '-link)'))
#}


# getMethod(f = 'model.frame', signature = 'vlm')
#' @rdname S3_vlm
#' @importFrom VGAM model.framevlm
#' @importFrom stats .getXlevels
#' @export
xlevels.vlm <- function(x) {
  m <- model.framevlm(x)
  trm <- m |> attr(which = 'terms', exact = TRUE)
  .getXlevels(Terms = trm, m = m)
}



#' @rdname S3_vlm
#' @importFrom VGAM formulavlm familyname.vlm
#' @export
nobsText.vlm <- function(x) {
  
  # I do not understand ?VGAM::nobs.vlm
  
  #if (!is.data.frame(data <- eval(x@call$data))) stop('data must be evaluable')
  if (!is.data.frame(data <- x@model)) stop('rerun with `model = TRUE`')
  fom <- formulavlm(x) # ?VGAM::formula.vlm
  
  if (familyname.vlm(x) %in% c('multinomial', 'cumulative', 'acat', 'cratio')) {
    # multinomial logistic regression; `x@y`m is the observed(?) relative frequency (not observed freq)
    yval <- eval(fom[[2L]], envir = data)
    if (!is.matrix(yval)) stop('endpoint should be `cbind(...)`')
    return(sprintf(fmt = '%d subjects', sum(yval)))
  }
  
  stop('should not come here')
}



















#' @title S3 Methods for `vglmff` Object
#' 
#' @param x `vglmff` family object
#' 
#' @name S3_vglmff
#' @importFrom VGAM familyname.vglmff
#' @export
getCanonicalLink.vglmff <- function(x) {
  x |>
    familyname.vglmff() |>
    do.call(args = list()) |> # all defaults; 'canonical'
    getLink.vglmff()
}

#getMethod(f = 'familyname', signature = 'vglmff')

#' @rdname S3_vglmff
#' @export
getLink.vglmff <- function(x) x@infos()$link |> vlm_link()

vlm_link <- function(lk) {
  ls <- unique.default(lk)
  if (length(lk) == 1L) {
    if (endsWith(lk, suffix = 'link')) return(gsub('link$', replacement = '', x = lk))
    stop('write specific handles')
  }
  #tobit model https://stats.idre.ucla.edu/r/dae/tobit-models/; need to think more
  paste(lk, collapse = '; ')
}

