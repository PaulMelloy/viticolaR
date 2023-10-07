#' Zoospore release
#'
#' @param WD_h integer wetness duration in hours
#' @param TWD_h numeric, mean temperature during wet duration.
#'
#' @return logical, TRUE or FALSE
zsp_release <- function(WD_h,TWD_h){
  x1 <- exp((-1.022 + 19.634)/TWD_h)
  return(WD_h >= x1)
}
