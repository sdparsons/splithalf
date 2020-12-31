#' Multiverse of data processing decisions on internal consistency reliability estimates.
#'
#' The (unofficial) function version name is "This function will let you get honey from a hornets nest"
#' @param input splithalf object or list of splithalf objects
#' @param specifications list of data processing specifications
#' @return Returns a multiverse object containing the reliability estimates and dataframes from all data processing specifications provided
#' @examples
#' ## see online documentation for examples
#' @import tidyr
#' @import Rcpp
#' @import ggplot2
#' @import grid
#' @import patchwork
#' @importFrom stats complete.cases cor median na.omit quantile sd cor.test
#' @importFrom robustbase colMedians
#' @importFrom dplyr select summarise group_by mutate n_distinct filter ungroup
#' @importFrom tidyr gather
#' @importFrom plyr arrange
#' @useDynLib splithalf, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom utils setTxtProgressBar txtProgressBar capture.output
#' @export


splithalf.multiverse <- function(input,
                                 specifications) {


  if(class(input) != "splithalf") {
    stop("please use a splithalf object as the input")
  }

  ###

  if(length(input$call$conditionlist) > 1) {
    warning("splithalf.multiverse only extracts the first condition for the analyses. If you want to run a multiverse on more than one condition, specify these separately")
  }



  # set up the output list ####################################################

  outlist <- list("input" = input,
                  "specifications" = specifications,
                  "type" = "internal_consistency",
                  "reliability" = "internal_consistency")

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

  # pass forward variables


  outcome = input$call$outcome
  score = input$call$score
  conditionlist = input$call$conditionlist
  halftype = input$call$halftype
  permutations = input$call$permutations
  var.RT = input$call$var.RT
  var.ACC = input$call$var.ACC
  var.condition = input$call$var.condition
  var.participant = input$call$var.participant
  var.compare = input$call$var.compare
  compare1 = input$call$compare1
  compare2 = input$call$compare2
  average = input$call$average


  ### ensure that the input$data names match
  input$data$var.RT = input$data[,var.RT]
  input$data$var.ACC = input$data[,var.ACC]
  input$data$var.condition = input$data[,var.condition]
  input$data$var.participant = input$data[,var.participant]
  input$data$var.compare = input$data[,var.compare]

  if(length(input$call$conditionlist) > 1) {

    input$data <- input$data %>%
      filter(var.condition == input$call$conditionlist[1])

    }



  # create empty objects for the purposes of binding global variables
  #(and to pass CRAN checks)
  n <- 0
  ACC  <- 0
  latency <- 0
  blockcode <- 0
  congruency <- 0
  low <- 0
  high <- 0
  meanRT <- 0
  Incongruent <- 0
  Congruent <- 0
  RTdiff <- 0
  . <- 0



  specs <- expand.grid(specifications)

  nS <- nrow(specs)

  ## add specs and nS to outlist

  outlist$specs <- specs
  outlist$nS <- nS

  # process specifications ####################################################

  print(paste("running", nS, "pre-processing specifications"))

  # internal consistency


  # calculate accuracy rates
  temp_data <- input$data %>%
    group_by(var.participant) %>%
    mutate(ACC = sum(var.ACC) / n())

  # make all the datasets

  perm_out <- list()

  SE_out <- NULL

  pb <- txtProgressBar(min = 0, max = nS, style = 3)
  setTxtProgressBar(pb, 0)

  for(perm in 1:nS) {

    temp <- temp_data %>%
      dplyr::filter(var.ACC >= specs[perm, "ACC_cutoff"]) %>%
      dplyr::group_by(var.participant) %>%
      dplyr::filter(var.ACC == 1) %>%
      dplyr::filter(var.RT >= specs[perm, "RT_min"],
                    var.RT <= specs[perm, "RT_max"]) %>%
      dplyr::ungroup()

    if(specs[perm, "split_by"] == "subject")
      temp <- temp %>%
        group_by(var.participant)
    if(specs[perm, "split_by"] == "trial")
      temp <- temp %>%
        group_by(var.participant, var.compare)

    if(specs[perm, "RT_sd_cutoff"] != 0)
      temp <- temp %>%
        mutate(high  = mean(var.RT) + (specs[perm, "RT_sd_cutoff"]*sd(var.RT)),
               low   = mean(var.RT) - (specs[perm, "RT_sd_cutoff"]*sd(var.RT))) %>%
        dplyr::filter(var.RT >= low, var.RT <= high) %>%
        ungroup() %>%
        as.data.frame()

    perm_out[[perm]] <- temp

    SE_out[perm] <- temp %>%
      group_by(var.participant, var.compare) %>%
      summarise(meanRT = mean(var.RT)) %>%
      spread(var.compare, meanRT) %>%
      ungroup() %>%
      mutate(RTdiff = .[[compare1]] - .[[compare2]]) %>%
      summarise(SE = sd(RTdiff)/sqrt(n())) %>%
      as.double()

    setTxtProgressBar(pb, perm)
  }

  outlist$sca  <- perm_out
  outlist$SE   <- SE_out



  nPar   <- 1:nS
  nTrial <- 1:nS

  for(i in 1:nS) {
    nPar[i]   <- length(unique(perm_out[[i]]$var.participant))
    nTrial[i] <- length(perm_out[[i]]$var.participant)
  }

  removals <- specs
  removals$nPar <- nPar
  removals$nTrial <- nTrial
  removals$nTrialperPar <- removals$nTrial / removals$nPar

  removals$pPar <- removals$nPar / length(unique(input$data$var.participant))
  removals$pTrial <- removals$nTrial / length(input$data$var.participant)

  outlist$removals <- removals

  ##### Run reliability estimates ############################################

  print("running reliability estimates")

  estimates <- list()

  # internal consistency

  pb2 <- txtProgressBar(min = 0, max = nS, style = 3)
  setTxtProgressBar(pb2, 0)

  for(perm2 in 1:nS) {
    capture.output({
      suppressWarnings({
        estimates[[perm2]] <- splithalf(data = perm_out[[perm2]],
                                        outcome = input$call$outcome,
                                        # conditionlist = c("angry"),
                                        permutations = input$call$permutations,
                                        average = specs[perm2, "averaging_method"],
                                        # var.condition = "blockcode",
                                        var.ACC = input$call$var.ACC,
                                        var.RT = input$call$var.RT,
                                        var.participant = input$call$var.participant,
                                        var.compare = input$call$var.compare,
                                        compare1 = input$call$compare1,
                                        compare2 = input$call$compare2,
                                        round.to = 5)$final_estimates
      })
    })

    setTxtProgressBar(pb2, perm2)
  }


  # test retest

  outlist$MULTIVERSEestimates <- estimates


  # get quantiles

  # q <- 1:nS
  #
  # if(type == "internal_consistency")  {
  # for(k in 1:length(q)){
  #   q[k] <- estimates[[k]]$spearmanbrown
  # }
  # }
  # if(type == "test_retest")  {
  # for(k in 1:length(q)){
  #   q[k] <- estimates[[k]]$ICC
  # }
  # }

  outlist$estimates <- specs
  for(i in 1:nS) {
    outlist$estimates$estimate[i] <- estimates[[i]]$spearmanbrown
    outlist$estimates$low[i]      <- estimates[[i]]$SB_low
    outlist$estimates$high[i]     <- estimates[[i]]$SB_high
  }



  outlist$CI <- quantile(outlist$estimates$estimate, c(.025,.5, .975))

  class(outlist) <- "multiverse"

  if(length(input$call$conditionlist) > 1) {
    print("REMINDER: splithalf.multiverse only extracts the first condition for the analyses. If you want to run a multiverse on more than one condition, specify these separately")
  }

  return(outlist)
}
