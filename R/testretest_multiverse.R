#' Multiverse of data processing decisions on test retest reliability estimates.
#'
#' The (unofficial) function version name is "This function will help you pay the troll toll"
#' @param data dataset
#' @param specifications list of data processing specifications
#' @param test correlation, ICC2, r ICC3
#' @param var.participant = "subject",
#' @param var.ACC = "correct",
#' @param var.RT set to internal consistency or test-retest
#' @return Returns a multiverse object containing the reliability estimates and dataframes from all data processing specifications provided
#' @examples
#' \dontrun{
#' ## see online documentation for examples
#' https://github.com/sdparsons/splithalf
#' ## also see https://psyarxiv.com/y6tcz
#'
#' }
#' @import tidyr
#' @import Rcpp
#' @import ggplot2
#' @import grid
#' @import patchwork
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
  if (outcome != "RT" & outcome != "accuracy") {
    stop("the outcome has not been specified: select from RT or accuracy")
  }
  if (score != "average" &
      score != "difference" &
      score != "difference_of_difference") {
    stop(
      "the score has not been specified: select from average, difference, or difference_of_difference"
    )
  }
  if(score == "DPrime") {
    warning("the DPrime score is under development. There are many versions of d prime, please check this is the correct version for your analyses")
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

        estimates[[perm2]] <- ICC(tmp[,2:3])$results[icc,]

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
