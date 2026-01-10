

#' @title Missing S3 Methods in \pkg{VGAM} Package
#' 
#' @param object a \link[VGAM]{vlm} object
#' 
#' @param ... S3 method dispatch holder
#' 
#' @details
#' tzh forgot to write to Dr. Yee about [summary.vlm()]..
#' 
#' @name S3_VGAM
#' @importFrom VGAM summaryvlm
#' @export summary.vlm
#' @export
summary.vlm <- function(object, ...) summaryvlm(object, ...)
# methods::getMethod('summary', signature(object = 'vlm'))
# ?VGAM::summaryvlm # does not have `...` parameter!!

