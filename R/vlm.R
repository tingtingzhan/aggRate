
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
#' @export
.pval.vglm <- function(x) {
  x |> 
    summary() |> 
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




#' @title S3 Methods for `vglmff` Object
#' 
#' @param x `vglmff` family object
#' 
#' @name S3_vglmff
#' @export
getCanonicalLink.vglmff <- function(x) {
  x@vfamily[1L] |>
    do.call(args = list()) |> # all defaults; 'canonical'
    getLink.vglmff()
}

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

