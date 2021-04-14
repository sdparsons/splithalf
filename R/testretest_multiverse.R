#' Multiverse of data processing decisions on test retest reliability estimates.
#'
#' This function enables the user to run a multiverse of data processing options and extract the resulting test-retest reliability estimates. The user specifies a set of data processing decisions and passes this to the function, along with specifying key variables within several "var." inputs (so that the function knows where to find your participant ids and RTs for example)
#'
#' The (unofficial) function version name is "This function will help you pay the troll toll"
#' @param data dataset
#' @param specifications list of data processing specifications
#' @param outcome from splithalf() specifies the RT outcome - only "RT" available currently
#' @param score currently only "difference" scores are supported
#' @param test test retest statistic, "ICC2", "cor", "ICC3"
#' @param var.participant = "subject",
#' @param var.ACC = "correct",
#' @param var.RT = "RT"
#' @param var.time codes the time variable (currently only works for 2 timepoints)
#' @param var.compare = "congruency" trial type used to create difference scores
#' @param compare1 specifies the first trial type to be compared (e.g. "Congruent" trials)
#' @param compare2 specifies the second trial type to be compared (e.g. "Incongruent" trials)
#' @return Returns a multiverse object containing the reliability estimates and dataframes from all data processing specifications provided
#' @examples
#' \dontrun{
#' ## see online documentation for examples
#' https://github.com/sdparsons/splithalf
#' ## also see https://psyarxiv.com/y6tcz
#'
#' n_participants <- 80 ## sample size
#' n_trials <- 120
#' n_blocks <- 2
#'
#' sim_data_mv <- data.frame(participant_number = rep(1:n_participants,
#'                                                    each = n_blocks * n_trials),
#'                           trial_number = rep(1:n_trials,
#'                                              times = n_blocks * n_participants),
#'                           block_name = rep(c(1,2),
#'                                            each = n_trials,
#'                                            length.out = n_participants * n_trials * n_blocks),
#'                           trial_type = rep(c("congruent","congruent",
#'                                              "incongruent","incongruent"),
#'                                            length.out = n_participants * n_trials * n_blocks / 2),
#'                           RT = rnorm(n_participants * n_trials * n_blocks,
#'                                      500,
#'                                      200),
#'                           ACC = c(rbinom(n_participants *
#'                                            n_trials *
#'                                            n_blocks / 6,
#'                                          1, .5),
#'                                   rbinom(n_participants *
#'                                            n_trials *
#'                                            n_blocks / 6,
#'                                          1, .7),
#'                                   rbinom(n_participants *
#'                                            n_trials *
#'                                            n_blocks / 6,
#'                                          1, .9),
#'                                   rbinom(n_participants *
#'                                            n_trials *
#'                                            n_blocks / 6,
#'                                          1, .5),
#'                                   rbinom(n_participants *
#'                                            n_trials *
#'                                            n_blocks / 6,
#'                                          1, .7),
#'                                   rbinom(n_participants *
#'                                            n_trials *
#'                                            n_blocks / 6,
#'                                          1, .9)))
#'
#' specifications <- list(
#' ACC_cutoff = c(0, 0.5),
#' RT_min           = c(0, 200),
#' RT_max            = c(2000, 3000),
#' RT_sd_cutoff      = c(0, 2),
#' split_by          = c("subject", "trial"),
#' averaging_method  = c("mean")
#' )
#'
#' icc2 <- testretest.multiverse(data = sim_data_acc,
#' specifications,
#' test = "ICC2",
#' score = "difference",
#' var.participant = "participant_number",
#' var.ACC = "ACC",
#' var.RT = "RT",
#' var.time = "block_name",
#' var.compare = "trial_type",
#' compare1 = "congruent",
#' compare2 = "incongruent")
#'
#' multiverse.plot(icc2)
#'
#' }
#' @import tidyr
#' @import Rcpp
#' @import ggplot2
#' @import grid
#' @import patchwork
#' @import lme4
#' @importFrom stats complete.cases cor median na.omit quantile sd cor.test
#' @importFrom robustbase colMedians
#' @importFrom dplyr select summarise group_by mutate n_distinct filter ungroup n
#' @importFrom tidyr gather
#' @importFrom plyr arrange
#' @importFrom psych ICC
#' @useDynLib splithalf, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom utils setTxtProgressBar txtProgressBar capture.output
#' @export

testretest.multiverse <- function(data,
                                   specifications,
                                   test = "ICC2",

                                   outcome = "RT",
                                   score = "difference",

                                   var.participant = "subject",
                                   var.ACC = "correct",
                                   var.RT = "RT",
                                   var.time = "time",
                                   var.compare = "congruency",
                                   compare1 = "Congruent",
                                   compare2 = "Incongruent") {


  # check that the dataframe is a data frame
  if (is.data.frame(data) == FALSE) {
    stop("a data frame has not been specified in data = ")
  }
  # check for missing variables
  if (outcome != "RT") {
    stop("only response time outcomes are supported. accuracy rates will be added in a future version")
  }
  if (score != "difference") {
    stop(
      "currently the only score option supported is the difference score. average scores will be added in a future version"
    )
  }

  # check that all of the variables exist in the data frame,
  # including the trial level components
  if (var.RT %in% colnames(data) == FALSE & outcome != "accuracy") {
    stop("the RT varible has not been specified")
  }
  if (var.participant %in% colnames(data) == FALSE) {
    stop("the participant varible has not been specified")
  }
  if (score == "difference" | score == "difference_of_difference") {
    if (var.compare %in% colnames(data) == FALSE) {
      stop("the compare varible has not been specified")
    }
    if (compare1 %in% unique(data[[var.compare]]) == FALSE) {
      stop("compare1 does not exist in the compare variable")
    }
    if (compare2 %in% unique(data[[var.compare]]) == FALSE) {
      stop("compare2 does not exist in the compare variable")
    }

  }





  ###

  data$subject <- data[, var.participant]
  data$time <- data[, var.time]
  data$correct <- data[, var.ACC]
  data$latency <- data[, var.RT]
  data$congruency <- data[, var.compare]

  data$congruency <- ifelse(data$congruency == compare1, "Congruent",
                         ifelse(data$congruency == compare2, "Incongruent", NA))

  data$trialnum <- 1:nrow(data)


  # set up the output list ####################################################

  outlist <- list("data" = data,
                  "specifications" = specifications,
                  "test" = test,
                  "reliability" = "test_retest")


  # create empty objects for the purposes of binding global variables
  #(and to pass CRAN checks)
  n <- 0
  ACC  <- 0
  latency <- 0
  blockcode <- 0
  congruency <- 0
  low <- 0
  high <- 0
  latency <- 0
  Incongruent <- 0
  Congruent <- 0
  time <- 0
  difference <- 0
  ICC <- 0
  subject <- 0
  correct <- 0
  RT <- 0


  # create the full specificaiton list ########################################

  # if anything is missing, add those variables

  outlist$cols <- names(specifications)

  if(!("ACC_cutoff" %in% names(specifications))){
    specifications[["ACC_cutoff"]] <- 0
  }
  if(!("RT_min" %in% names(specifications))){
    specifications[["RT_min"]] <- 0
  }
  if(!("RT_max" %in% names(specifications))){
    specifications[["RT_max"]] <- 1000000
  }
  if(!("RT_sd_cutoff" %in% names(specifications))){
    specifications[["RT_sd_cutoff"]] <- 0
  }
  if(!("split_by" %in% names(specifications))){
    specifications[["split_by"]] <- "subject"
  }
  if(!("averaging_method" %in% names(specifications))){
    specifications[["averaging_method"]] <- "mean"
  }

  specs <- expand.grid(specifications)

  nS <- nrow(specs)

  ## add specs and nS to outlist

  outlist$specs <- specs
  outlist$nS <- nS

  # process specifications ####################################################

  print(paste("running", nS, "pre-processing specifications"))

  # calculate accuracy rates
  temp_data <- data %>%
    group_by(time, subject) %>%
    mutate(ACC = sum(correct) / n())

  # make all the datasets

  perm_out <- list()

  pb <- txtProgressBar(min = 0, max = nS, style = 3)
  setTxtProgressBar(pb, 0)

  for(perm in 1:nS) {

    temp <- temp_data %>%
      filter(ACC >= specs[perm, "ACC_cutoff"]) %>%
      group_by(time, subject) %>%
      filter(correct == 1) %>%
      filter(latency >= specs[perm, "RT_min"],
             latency <= specs[perm, "RT_max"]) %>%
      ungroup()

    if(specs[perm, "split_by"] == "subject")
      temp <- temp %>%
        group_by(time, subject)
#    if(specs[perm, "split_by"] == "condition")
#      temp <- temp %>%
#        group_by(time, subject, blockcode)
    if(specs[perm, "split_by"] == "trial")
      temp <- temp %>%
        group_by(time, subject, congruency)

    if(specs[perm, "RT_sd_cutoff"] != 0)
      temp <- temp %>%
        mutate(high  = mean(latency) + (specs[perm, "RT_sd_cutoff"]*sd(latency)),
               low   = mean(latency) - (specs[perm, "RT_sd_cutoff"]*sd(latency))) %>%
        filter(latency >= low, latency <= high) %>%
        ungroup() %>%
        as.data.frame()

    perm_out[[perm]] <- temp
    setTxtProgressBar(pb, perm)
  }

  outlist$sca  <- perm_out



  # check removals ############################################################
  # note, needs tweaking for test-retest.

  nPar   <- 1:nS
  nTrial <- 1:nS

  for(i in 1:nS) {
    nPar[i]   <- length(unique(perm_out[[i]]$subject))
    nTrial[i] <- length(perm_out[[i]]$trialnum)
  }

  removals <- specs
  removals$nPar <- nPar
  removals$nTrial <- nTrial
  removals$nTrialperPar <- removals$nTrial / removals$nPar

  removals$pPar <- removals$nPar / length(unique(data$subject))
  removals$pTrial <- removals$nTrial / length(data$trialnum)

  outlist$removals <- removals

  ##### Run reliability estimates ############################################

  print("running reliability estimates")

  estimates <- list()

  # test retest

  if(test == "ICC2")
    icc <- 2
  if(test == "ICC3")
    icc <- 3

  pb2 <- txtProgressBar(min = 0, max = nS, style = 3)
  setTxtProgressBar(pb2, 0)

  if(test == "ICC2" | test == "ICC3") {

    for(perm2 in 1:nS) {
      capture.output({

        tmp <- perm_out[[perm2]] %>%
          group_by(time, subject, congruency) %>%
          summarise(RT = mean(latency)) %>%
          spread(congruency, RT) %>%
          mutate(difference = Incongruent - Congruent) %>%
          select(-Congruent, -Incongruent) %>%
          group_by(time) %>%
          spread(time, difference)

        estimates[[perm2]] <- psych::ICC(tmp[,2:3])$results[icc,]

      })
      setTxtProgressBar(pb2, perm2)
    }
  }

  if(test == "cor")
  {
    for(perm2 in 1:nS) {
      capture.output({

        tmp <- perm_out[[perm2]] %>%
          group_by(time, subject, congruency) %>%
          summarise(RT = mean(latency)) %>%
          spread(congruency, RT) %>%
          mutate(difference = Incongruent - Congruent) %>%
          select(-Congruent, -Incongruent) %>%
          group_by(time) %>%
          spread(time, difference) %>%
          as.data.frame()

        estimates[[perm2]] <- cor.test(tmp[,2],tmp[,3])

      })
      setTxtProgressBar(pb2, perm2)
    }
  }


  outlist$MULTIVERSEestimates <- estimates

  outlist$estimates <- specs

  if(test == "ICC2" | test == "ICC3") {
    for(i in 1:nS) {
      outlist$estimates$estimate[i] <- estimates[[i]]$ICC
      outlist$estimates$low[i]      <- estimates[[i]]$`lower bound`
      outlist$estimates$high[i]     <- estimates[[i]]$`upper bound`
    }
  }

  if(test == "cor") {
    for(i in 1:nS) {
      outlist$estimates$estimate[i] <- estimates[[i]]$estimate
      outlist$estimates$low[i]      <- estimates[[i]]$conf.int[1]
      outlist$estimates$high[i]     <- estimates[[i]]$conf.int[2]
    }
  }

  outlist$CI <- quantile(outlist$estimates$estimate, c(.025,.5, .975), na.rm = TRUE)

  class(outlist) <- "multiverse"

  return(outlist)
}
