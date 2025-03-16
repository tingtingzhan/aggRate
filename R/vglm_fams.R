

#' @title Handy extensions of \link[VGAM]{vglm}
#' 
#' @description 
#' 
#' To try multiple family choices and select the one with the least `ResSS`.
#' 
#' @param object see **Usage**
#' 
#' @param ... see \link[VGAM]{vglm}
#' 
#' 
#' @examples 
#' m = VGAM::wine |>
#'  aggRate.data.frame(pattern = c(bitter = '^bitter')) |>
#'  vglm_fams()
#' autoplot(m, type = 'plain')
#' 
#' library(ordinal)
#' clm(rating ~ temp + contact, data = ordinal::wine) |>
#'  vglm_fams()
#' @name vglm_fams
#' @export
vglm_fams <- function(object, ...) UseMethod('vglm_fams')


#' @rdname vglm_fams
#' @export vglm_fams.clm
#' @export
vglm_fams.clm <- function(object, ...) {
  fr <- aggRate.clm(object)
  vlm0 <- vglm_fams.aggRate(fr, ...)
  if (!length(vlm0)) return(object) # exception
  return(vlm0)
}


#' @rdname vglm_fams
#' @importFrom VGAM vglm cumulative cratio acat familyname.vlm
#' @export vglm_fams.aggRate
#' @export
vglm_fams.aggRate <- function(object, ...) {
  
  fom <- attr(object, which = 'formula', exact = TRUE)
  
  vglm_mod <- list(
    cumulative(parallel = TRUE), # first one must be equivalent to ?ordinal::clm
    cratio(parallel = TRUE),
    acat(parallel = TRUE)
    # Future!!! try different `link` and `parallel`
  ) |>
    lapply(FUN = function(fam) {
      m <- tryCatch(expr = vglm(formula = fom, family = fam, data = as.data.frame(object), model = TRUE, ...), warning = identity)
      if (inherits(m, what = 'warning')) {
        # print(m$message)
        return(invisible())
      }
      return(m)
    })
  
  vglm_mod <- vglm_mod[lengths(vglm_mod) > 0L]
  
  if (!length(vglm_mod)) {
    #cat('vglm does not work (probably due to 0-freq\n')
    return(invisible())
  }
  
  #ResSS <- lapply(vglm_mod, FUN = slot, name = 'ResSS') # needs to @importFrom methods slot
  ResSS <- lapply(vglm_mod, FUN = function(i) i@ResSS) # stopifnot(is.primitive(`@`))
  id <- which.min(ResSS)
  z <- vglm_mod[[id]]
  if (id != 1L) message(sprintf(fmt = 'VGAM::%s for smaller residual sum of squares (%.3f vs %.3f)', 
                                familyname.vlm(z),
                                ResSS[id], ResSS[1L]))
  return(z)
}








