






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
#' @keywords internal
#' @name S3_vlm
#' @importFrom ecip .pval
#' @method .pval summary.vglm
#' @export .pval.summary.vglm
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
#' @importFrom ecip desc_
#' @export desc_.vlm
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
#' @importFrom ecip nobsText
#' @export nobsText.vlm
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





#' @title R Markdown Lines of \link[VGAM]{vlm} object
#' 
#' @param x,xnm,... ..
#' 
#' @examples
#' library(VGAM); library(ecip)
#' pneumo = transform(pneumo, let = log(exposure.time))
#' list(
#'  '`vglm`' = vglm(cbind(normal, mild, severe) ~ let, propodds, data = pneumo)
#' ) |> fastmd::render2html()
#' 
#' @keywords internal
#' @importFrom fastmd md_
#' @importFrom ecip md_ecip
#' @export md_.vlm
#' @export
md_.vlm <- md_ecip















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

