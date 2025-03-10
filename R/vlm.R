
#' @title Missing S3 Methods in \pkg{VGAM} Package
#' 
#' @param object,formula a \link[VGAM]{vlm} or \link[VGAM]{vglm} object
#' 
#' @param ... S3 method dispatch holder
#' 
#' @details
#' tzh has written to Dr. Yee about some of these
#' 
#' @name S3_VGAM
#' @importFrom VGAM summaryvglm
#' @export summary.vglm
#' @export
summary.vglm <- function(object, ...) summaryvglm(object = object, ...)
# getMethod('summary', signature(object = 'vglm')) 
# ?VGAM::summaryvglm

#' @rdname S3_VGAM
#' @importFrom VGAM summaryvlm
#' @export summary.vlm
#' @export
summary.vlm <- function(object, ...) summaryvlm(object, ...)
# getMethod('summary', signature(object = 'vlm'))
# ?VGAM::summaryvlm # does not have `...` parameter!!
# eh, tzh forgot to write to Dr. Yee about [summary.vlm()]

#' @rdname S3_VGAM
#' @importFrom stats model.frame
#' @importFrom VGAM model.framevlm
#' @export model.frame.vlm
#' @export
model.frame.vlm <- function(formula, ...) model.framevlm(object = formula, ...)
# getMethod(f = 'model.frame', signature = 'vlm')









#' @title Get Family from \link[VGAM]{vlm} Object
#' 
#' @param object \link[VGAM]{vlm} object
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [family.vlm] returns a `'vglmff'` object.
#' 
#' @examples
#' library(VGAM)
#' pneumo = transform(pneumo, let = log(exposure.time))
#' (m1 = vglm(cbind(normal, mild, severe) ~ let, family = multinomial, data = pneumo))
#' (m2 = vglm(cbind(normal, mild, severe) ~ let, family = propodds, data = pneumo))
#' (m3 = vglm(cbind(normal, mild, severe) ~ let, family = acat, data = pneumo))
#' m1 |> desc_.vlm()
#' m2 |> desc_.vlm()
#' m3 |> desc_.vlm()
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
#' @export
.pval.summary.vglm <- function(x) {
  cf <- x@coef3
  ret <- cf[, 'Pr(>|z|)']
  names(ret) <- rownames(cf) # nrow-1 drops rownames
  return(ret)
}

# .pval.summary.vlm <- function(x) {}
# I don't know a 'vlm' example yet



#' @rdname S3_vlm
#' @importFrom VGAM familyname.vglmff
#' @export
desc_.vlm <- function(x) {
  ff <- x@family # 'vglmff'
  fnm <- familyname.vglmff(ff)
  fnm <- switch(fnm, 
                cumulative = 'cumulative probability', 
                acat = 'adjacent-categories-ratio', # ?VGAM::acat
                cratio = 'continuation-ratio', # ?VGAM::cratio
                fnm)
  lnk <- getLink.vglmff(ff)
  clnk <- getCanonicalLink.vglmff(ff)
  if (lnk == clnk) return(fnm)
  return(paste0(fnm, ' (with ', lnk, '-link)'))
}




#' @rdname S3_vlm
#' @importFrom VGAM model.framevlm
#' @export
nobsText.vlm <- function(x) {
  
  # I do not understand VGAM::nobs.vlm(, type = 'vlm')
  
  if (familyname.vlm(x) %in% c('multinomial', 'cumulative', 'acat', 'cratio')) {
    # multinomial logistic regression
    (model.framevlm(x)[[1L]]) |> 
      sum() |>
      sprintf(fmt = '%d subjects')

  } else stop('should not come here')
  
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

