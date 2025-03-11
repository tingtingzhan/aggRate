#' @title Missing S3 Methods in \pkg{VGAM} Package
#' 
#' @param object,formula a \link[VGAM]{vlm} or \link[VGAM]{vglm} object
#' 
#' @param ... S3 method dispatch holder
#' 
#' @details
#' tzh has written to Dr. Yee about some of these ..
#' 
#' @name S3_VGAM
#' @importFrom VGAM summaryvglm
#' @export summary.vglm
#' @export
summary.vglm <- function(object, ...) summaryvglm(object = object, ...)
# methods::getMethod('summary', signature(object = 'vglm')) 
# ?VGAM::summaryvglm

#' @rdname S3_VGAM
#' @importFrom VGAM summaryvlm
#' @export summary.vlm
#' @export
summary.vlm <- function(object, ...) summaryvlm(object, ...)
# methods::getMethod('summary', signature(object = 'vlm'))
# ?VGAM::summaryvlm # does not have `...` parameter!!
# eh, tzh forgot to write to Dr. Yee about [summary.vlm()]

#' @rdname S3_VGAM
#' @importFrom stats model.frame
#' @importFrom VGAM model.framevlm
#' @export model.frame.vlm
#' @export
model.frame.vlm <- function(formula, ...) model.framevlm(object = formula, ...)
# methods::getMethod(f = 'model.frame', signature = 'vlm')



