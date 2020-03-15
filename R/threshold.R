#' Check proportion of reliability estimates from the multiverse analyses above or below a set threshold
#'
#' The (unofficial) function version name is "This function will get you up to here with it"
#'
#' @param multiverse multiverse object
#' @param threshold threshold to look for
#' @param use set to check the reliability estimates, or the upper or lower CIs
#' @param dir look above or below the 'use' at the set threshold
#' @useDynLib splithalf, .registration = TRUE
#' @rdname multiverse.plot
#' @export

threshold <- function(multiverse,
                      threshold,
                      use = "estimate",
                      dir = "above") {

  tmp <- 1:multiverse$nS

  if(use == "estimate") {
    tmp <- multiverse$estimates$estimate
  }

  if(use == "lower") {
    tmp <- multiverse$estimates$low
  }

  if(use == "upper") {
    tmp <- multiverse$estimates$high
  }


  if(dir == "above") {
    threshold <- sum(tmp > threshold) / length(tmp)
  }
  if(dir == "below") {
    threshold <- sum(tmp < threshold) / length(tmp)
  }

  return(threshold)
}
