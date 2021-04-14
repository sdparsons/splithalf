#' Check proportion of reliability estimates from the multiverse analyses above or below a set threshold
#'
#' This function examines the output from splithalf_multiverse or testretest_multiverse to extract the proportions of estimates above or below a set threshold (can be the estimate or the upper or lower CI estimates).
#' The (unofficial) function version name is "This function will get you up to here with it"
#'
#' @param multiverse multiverse object
#' @param threshold threshold to look for, e.g. 0.7
#' @param use set to check the reliability "estimates", or the "upper" or "lower" CIs
#' @param dir look "above" or "below" the 'use' at the set threshold
#' @examples
#' \dontrun{
#' ## see online documentation for examples
#' https://github.com/sdparsons/splithalf
#' ## also see https://psyarxiv.com/y6tcz
#'
#' ## example simulated data
#' n_participants = 60 ## sample size
#' n_trials = 80
#' n_blocks = 2
#' sim_data <- data.frame(participant_number = rep(1:n_participants,
#'                        each = n_blocks * n_trials),
#'                        trial_number = rep(1:n_trials,
#'                        times = n_blocks * n_participants),
#'                        block_name = rep(c("A","B"),
#'                        each = n_trials,
#'                        length.out = n_participants * n_trials * n_blocks),
#'                        trial_type = rep(c("congruent","incongruent"),
#'                        length.out = n_participants * n_trials * n_blocks),
#'                        RT = rnorm(n_participants * n_trials * n_blocks,
#'                        500,
#'                        200),
#'                        ACC = 1)
#'
#' ## specify several data processing decisions
#' specifications <- list(RT_min = c(0, 100, 200),
#'                        RT_max = c(1000, 2000),
#'                        averaging_method = c("mean", "median"))
#' ## run splithalf, and save the output
#' difference <- splithalf(data = sim_data,
#'                         outcome = "RT",
#'                         score = "difference",
#'                         conditionlist = c("A"),
#'                         halftype = "random",
#'                         permutations = 5000,
#'                         var.RT = "RT",
#'                         var.condition = "block_name",
#'                         var.participant = "participant_number",
#'                         var.compare = "trial_type",
#'                         var.ACC = "ACC",
#'                         compare1 = "congruent",
#'                         compare2 = "incongruent",
#'                         average = "mean")
#'
#' ## run splithalf.multiverse to perform the multiverse of data processing
#' ## and reliability estimation
#' multiverse <- splithalf.multiverse(input = difference,
#'                                    specifications = specifications)
#'
#'
#' ## the threshold function can be used to return the number of estimates
#' ## above or below a certain threshold
#'
#' threshold(multiverse = multiverse,
#'           threshold = 0.7,
#'           use = "estimate",
#'           dir = "above")
#' }
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
