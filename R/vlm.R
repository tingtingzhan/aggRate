


#' @title \link[VGAM]{vlm} Object
#' 
#' @examples
#' library(VGAM)
#' pneumo = transform(pneumo, let = log(exposure.time))
#' (m1 = vglm(cbind(normal, mild, severe) ~ let, family = multinomial, data = pneumo))
#' (m2 = vglm(cbind(normal, mild, severe) ~ let, family = propodds, data = pneumo))
#' (m3 = vglm(cbind(normal, mild, severe) ~ let, family = acat, data = pneumo))
#' library(ecip); list(
#'  '`multinomial`' = m1,
#'  '`propodds`' = m2,
#'  '`acat`' = m3
#' ) |> fastmd::render2html()
#' @name vlm
NULL



#' @export
family.vlm <- function(object, ...) object@family # 'vglmff'
# so that I can use ecip:::getLink.default()




# tzh forgot to write to Dr. Yee about [summary.vlm()]..
#' @importFrom VGAM summaryvlm
#' @export
summary.vlm <- function(object, ...) summaryvlm(object, ...)
# methods::getMethod('summary', signature(object = 'vlm'))
# ?VGAM::summaryvlm # does not have `...` parameter!!






#' @importFrom ecip .pval
#' @method .pval summary.vglm
#' @export
.pval.summary.vglm <- function(x) {
  cf <- x@coef3
  ret <- cf[, 'Pr(>|z|)']
  names(ret) <- rownames(cf) # nrow-1 drops rownames
  return(ret)
}

# .pval.summary.vlm <- function(x) {}
# I don't know a 'vlm' example yet



#' @importFrom VGAM familyname.vglmff
#' @importFrom ecip desc_
#' @importClassesFrom fastmd md_lines
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
  if (lnk != clnk) fnm <- paste0(fnm, ' (with ', lnk, '-link)')
  fnm |>
    new(Class = 'md_lines')
}




#' @importFrom VGAM model.framevlm
#' @importFrom ecip nobsText
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





#' @importFrom fastmd md_
#' @importFrom ecip md_ecip
#' @export
md_.vlm <- md_ecip








#' @importFrom VGAM familyname.vglmff
#' @importFrom ecip getCanonicalLink
#' @export
getCanonicalLink.vglmff <- function(x) {
  x |>
    familyname.vglmff() |>
    do.call(args = list()) |> # all defaults; 'canonical'
    getLink.vglmff()
}

#getMethod(f = 'familyname', signature = 'vglmff')

#' @importFrom ecip getLink
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

