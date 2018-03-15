#' Split Half for difference scores
#'
#' This function calculates split half reliability estimates for Dot Probe data
#' @param data specifies the raw dataset to be processed
#' @param RTmintrim specifies the lower cut-off point for RTs
#' @param RTmaxtrim specifies the maximum cut-off point for RTs
#' @param incErrors include incorrect trials?, defaults to FALSE
#' @param conditionlist sets conditions/blocks to be processed
#' @param halftype specifies the split method; "oddeven", "halfs", or "random"
#' @param no.iterations specifies the number of random splits to run
#' @param var.RT specifies the RT variable name in data
#' @param var.condition specifies the condition variable name in data - if not specified then splithalf will treat all trials as one condition
#' @param var.participant specifies the subject variable name in data
#' @param var.correct specifies the accuracy variable name in data
#' @param var.trialnum specifies the trial number variable
#' @param var.compare specified the variable that is used to calculate difference scores (e.g. including congruent and incongruent trials)
#' @param compare1 specifies the first trial type to be compared (e.g. congruent trials)
#' @param compare2 specifies the first trial type to be compared (e.g. incongruent trials)
#' @param removelist specifies a list of participants to be removed
#' @param average allows the user to specify whether mean or median will be used to create the bias index
#' @param sdtrim allows the user to trim the data by selected sd (after removal of errors and min/max RTs)
#' @return Returns a data frame containing split-half reliability estimates for the bias index in each condition specified.
#' @return splithalf returns the raw estimate of the bias index
#' @return spearmanbrown returns the spearman-brown corrected estimate of the bias index
#' @return Warning: If there are missing data (e.g one condition data missing for one participant) output will include details of the missing data and return a dataframe containing the NA data. Warnings will be displayed in the console.
#' @examples
#' ## split half estimates for the bias index in two blocks
#' ## using 50 iterations of the random split method (note: 5000 would be standard)
#' # not run:
#' # splithalf_diff(DPdata, conditionlist = c("block1","block2"),
#' # halftype = "random", no.iterations = 50)
#' ## In datasets with missing data an additional output is generated
#' ## the console will return a list of participants/blocks
#' ## the output will also include a full dataframe of missing values
#' # not run:
#' # splithalf_diff(DPdata_missing, conditionlist = c("block1","block2"),
#' # halftype = "random", no.iterations = 50)
#' @import tidyr
#' @import dplyr
#' @import utils
#' @importFrom stats complete.cases cor median na.omit quantile sd
#' @export

splithalf_diff <- function(data,
                        RTmintrim = 'none',
                        RTmaxtrim = 'none',
                        incErrors = FALSE,
                        conditionlist = FALSE,
                        halftype = "random",
                        no.iterations = 5000,
                        var.RT = "latency",
                        var.condition = FALSE,
                        var.participant = "subject",
                        var.correct = "correct",
                        var.trialnum = "trialnum",
                        var.compare = "congruency",
                        compare1 = "Congruent",
                        compare2 = "Incongruent",
                        removelist = "",
                        average = "mean",
                        sdtrim = FALSE
                        )
{
  # check for missing variables
  if(halftype != "oddeven" & halftype != "halfs" & halftype != "random") {
    stop("the halftype has not been specified")
  }

  # check that all of the variables exist in the data frame,
  # including the trial level components
  if(var.RT %in% colnames(data) == FALSE) {
    stop("the RT varible has not been specified")
  }
  if(var.participant %in% colnames(data) == FALSE) {
    stop("the participant varible has not been specified")
  }
  if(var.correct %in% colnames(data) == FALSE) {
    stop("the accuracy varible has not been specified")
  }
  if(var.trialnum %in% colnames(data) == FALSE) {
    stop("the trial number varible has not been specified")
  }
  if(var.compare %in% colnames(data) == FALSE) {
    stop("the compare varible has not been specified")
  }
  if(compare1 %in% unique(data[[var.compare]]) == FALSE) {
    stop("compare1 does not exist in the compare variable")
  }
  if(compare2 %in% unique(data[[var.compare]]) == FALSE) {
    stop("compare2 does not exist in the compare variable")
  }
  if(average != "mean" & average != "median") {
    stop("averaging method not selected")
  }

  # specifically checking the condition variables, alterning as necessary to run for all trials as one condition
  if(var.condition == FALSE) {
    warning("no condition variable specified, splithalf will treat all trials as one condition")
    data$all <- "all"
    var.condition <- "all"
    conditionlist <- "all"
  }  else if(var.condition %in% colnames(data) == FALSE)  {
    stop("condition variable cannot be found in dataframe")
  } else if(!exists("conditionlist")) {
    warning("condition list not specified, treating task as single condition")
    data$all <- "all"
    var.condition <- "all"
    conditionlist <- "all"
  } else if(exists("conditionlist")) {
    if(all(conditionlist %in% unique(data[[var.condition]])) == FALSE) {
      stop("one or more of the conditions do not exist in the condition variable")
    }
  }


  # create empty objects for the purposes of binding global variables
  RT <- 0
  correct <- 0
  participant <- 0
  condition <- 0
  half1bias <- 0
  half2bias <- 0
  bias1 <- 0
  bias2 <- 0
  iteration <- 0
  N <- 0
  spearmanbrown <- 0
  low <- 0
  high <- 0
  compare <- 0

  # set the data as a data.frame to avoid tibble issues
  data <- as.data.frame(data)

  # renames the dataset variables to fit with the code
  data$RT <- data[, var.RT]
  data$condition <- data[, var.condition]
  data$participant <- data[, var.participant]
  data$correct <- data[, var.correct]
  data$trialnum <- data[, var.trialnum]
  data$compare <- data[, var.compare]


  # for randdom samples, the number of samples drawn
  iterations <- 1:no.iterations

  # loads data into dataset
  dataset <- data

  # removes participants specified to be removed in removelist
  dataset <- dataset[!dataset$participant %in% removelist, ]

  # how many participants?
  n_par <- n_distinct(dataset$participant)

  # removes errors if FALSE, includes error trials if TRUE
  if (incErrors == FALSE) {
    dataset <- subset(dataset, correct == 1)
  }

  # removes trials below the minimum cutoff and above the maximum cutoff
  if (is.numeric(RTmintrim) == TRUE)
  {
    dataset <- subset(dataset, RT > RTmintrim)
  }
  if (is.numeric(RTmaxtrim) == TRUE)
  {
    dataset <- subset(dataset, RT < RTmaxtrim)
  }

  # creates a list of participants
  plist <- sort(unique(dataset$participant))

  # if there is a sd trim
  if (is.numeric(sdtrim)) {
  dataset <- dataset %>%
    dplyr::group_by(participant, condition, compare) %>%
    dplyr::mutate(low =  mean(RT) - (sdtrim * sd(RT)),
           high = mean(RT) + (sdtrim * sd(RT))) %>%
    dplyr::filter(RT >= low & RT <= high)
  }


    # checks whether user difference score is based on means or medians
  if (average == "mean") {
    ave_fun <- function(val) {mean(val, na.rm = TRUE)}
  } else if (average == "median") {
    ave_fun <- function(val) {median(val, na.rm = TRUE)}
  }


## Main splithalf processing

  if (halftype == "oddeven" | halftype == "halfs")
  {
    finaldata <- data.frame(i = rep(plist, times = length(conditionlist)),
                            j = rep(conditionlist, each = length(plist)),
                            half1.congruent = NA, half1.incongruent = NA,
                            half2.congruent = NA, half2.incongruent = NA)
    l <- 1

    if (halftype == "oddeven")
    {
      # this loop creates a dataframe split by odd and even trial numbers
      # giving mean RTs in congruent and incongruent conditions for each
      # split

      for (j in conditionlist)
      {
        for (i in plist)
        {
          temp <- subset(dataset, participant == i & condition == j)

          half1.congruent   <- ave_fun(subset(temp$RT, temp$compare ==
                                             compare1 & temp$trialnum %% 2 == 0))
          half1.incongruent <- ave_fun(subset(temp$RT, temp$compare ==
                                             compare2 & temp$trialnum %% 2 == 0))
          half2.congruent   <- ave_fun(subset(temp$RT, temp$compare ==
                                             compare1 & temp$trialnum %% 2 == 1))
          half2.incongruent <- ave_fun(subset(temp$RT, temp$compare ==
                                             compare2 & temp$trialnum %% 2 == 1))

          finaldata[l, 3:6] <- c(half1.congruent, half1.incongruent,
                                 half2.congruent, half2.incongruent)

          l <- l + 1
        }
        print(paste("condition", j, "complete"))
      }
    } else {

      if (halftype == "halfs")
      {
        # this loop takes the first and last half of trials within each
        # condition~participant
        for (j in conditionlist)
        {
          for (i in plist)
          {
            temp <- subset(dataset, participant == i & condition == j)

            triallist <- as.list(temp$trialnum)

            midtrial <- sum(!is.na(triallist))/2
            totaltrial <- sum(!is.na(triallist))

            half1 <- temp[1:midtrial, ]
            half2 <- temp[(midtrial + 1):totaltrial, ]

            half1.congruent  <- ave_fun(subset(half1$RT,
                                            half1$participant == i &
                                              half1$condition == j &
                                              half1$compare == compare1))
            half1.incongruent <- ave_fun(subset(half1$RT,
                                             half1$participant == i &
                                               half1$condition == j &
                                               half1$compare == compare2))
            half2.congruent  <- ave_fun(subset(half2$RT,
                                            half2$participant == i &
                                              half2$condition == j &
                                              half2$compare == compare1))
            half2.incongruent <- ave_fun(subset(half2$RT,
                                             half2$participant == i &
                                               half2$condition == j
                                             & half2$compare == compare2))

            finaldata[l, 3:6] <- c(half1.congruent, half1.incongruent,
                                   half2.congruent, half2.incongruent)

            l <- l + 1
          }
          print(paste("condition", j, "complete"))
        }
      }
    }
    names(finaldata)[1] <- "participant"
    names(finaldata)[2] <- "condition"

    if (sum(is.na(finaldata$half1.congruent)) +
        sum(is.na(finaldata$half1.incongruent)) +
        sum(is.na(finaldata$half2.congruent)) +
        sum(is.na(finaldata$half2.incongruent)) > 0)
    {
      print("the following are participants/conditions with missing data")
      omitted <- finaldata[!complete.cases(finaldata), ]
      print(unique(omitted[c("condition", "participant")]))
      print("note: these particpants will be removed from the split half
            reliability calculations, in that condition")
      warning("Bias indices missing:
              at least one participant has missing data from at one condition
              These cases are removed from calculating reliability estimates
              $omitted contains the missing cases")
    }

    # remove NA rows
    finaldata2 <- na.omit(finaldata)

    # calculate bias indices
    finaldata2$half1bias <- finaldata2$half1.incongruent -
      finaldata2$half1.congruent
    finaldata2$half2bias <- finaldata2$half2.incongruent -
      finaldata2$half2.congruent

    # create calculate estimates

    splithalf <- finaldata2 %>%
      dplyr::group_by(condition) %>%
      dplyr::summarise(n = round(sum(!is.na(half1bias)),0),
                splithalf = cor(half1bias, half2bias,
                                use = "pairwise.complete"),
                spearmanbrown = (2 * cor(half1bias, half2bias,
                                         use = "pairwise.complete")) /
                  (1 + (2 - 1) * cor(half1bias, half2bias,
                                     use = "pairwise.complete"))) %>%
      as.data.frame()


    if (halftype == "oddeven")
      print("Splithalf estimates, splitting by odd and even trials")
    if (halftype == "halfs")
      print("Splithalf estimates, splitting by the first and second half of trials")


    if (sum(is.na(finaldata$half1.congruent)) +
        sum(is.na(finaldata$half1.incongruent)) +
        sum(is.na(finaldata$half2.congruent)) +
        sum(is.na(finaldata$half2.incongruent)) > 0)
    {
      return(list(Estimates = splithalf, omitted = omitted))
    } else {
      return(splithalf)
    }

    }

  if (halftype == "random")
  {



    # create the data.frame to populate
    findata <-  data.frame(j = rep(conditionlist, each = (length(plist) *
                                                           length(iterations))),
                          i = rep(plist, each = length(iterations)),
                          h = rep(iterations, times = (length(conditionlist) *
                                                         length(plist))),
                          bias1 = NA, bias2 = NA)
    # loop counter
    l <- 1

    # participant loop counter for progress bar
    ppt <- 1

    # create vectors to contain both halfs to be compared
    bias1v <- vector(length = (length(conditionlist) * length(plist) *
                                 length(iterations)))
    bias2v <- vector(length = (length(conditionlist) * length(plist) *
                                 length(iterations)))

    for (j in conditionlist)
    {
      # set up progress bar
      pb <- txtProgressBar(min = 0, max = n_par, style = 3)
      setTxtProgressBar(pb, 0)

      for (i in plist)
      {
        # subset the dataframe into RT vectors by participant, condition, and
        # congruency
        temp.con   <- subset(dataset$RT, dataset$participant == i &
                               dataset$condition == j &
                               dataset$compare == compare1)
        temp.incon <- subset(dataset$RT, dataset$participant ==
                               i & dataset$condition == j &
                               dataset$compare == compare2)

        # calculates what will be the middle numbered trial in each congruent
        # and incongruent list
        midtrial.con <- sum(!is.na(temp.con))/2
        midtrial.incon <- sum(!is.na(temp.incon))/2

        ii.con <- seq(temp.con)
        ii.incon <- seq(temp.incon)

        # in the following loop random halves of each congruent and incongruent
        # list are taken the ind objects contain the trial list, and the h1/h2
        # contain the actual RTs the bias indices are then calculated and added
        # to the vector
        for (h in iterations)
        {
          ind1.con <- sample(ii.con, midtrial.con)
          ind2.con <- ii.con[!ii.con %in% ind1.con]
          ind1.incon <- sample(ii.incon, midtrial.incon)
          ind2.incon <- ii.incon[!ii.incon %in% ind1.incon]

          h1.congruent <- temp.con[ind1.con]
          h1.incongruent <- temp.incon[ind1.incon]
          h2.congruent <- temp.con[ind2.con]
          h2.incongruent <- temp.incon[ind2.incon]

          bias1v[l] <- ave_fun(h1.incongruent) - ave_fun(h1.congruent)
          bias2v[l] <- ave_fun(h2.incongruent) - ave_fun(h2.congruent)

          l <- l + 1

        }

        ppt <- ppt + 1
        setTxtProgressBar(pb, ppt)

      }
      ppt <- 1 # reset the progress bar

      print(paste("condition", j, "complete"))
    }

    print("Calculating split half estimates")

    findata$bias1 <- bias1v
    findata$bias2 <- bias2v

    names(findata)[1] <- "condition"
    names(findata)[2] <- "participant"
    names(findata)[3] <- "iteration"

    if (sum(is.na(findata$bias1)) +
        sum(is.na(findata$bias2)) > 0)
    {
      print("the following are participants/conditions with missing data")
      omitted <- findata[!complete.cases(findata), ]
      print(unique(omitted[c("condition", "participant")]))
      print("note: these iterations will be removed from the split half
            reliability calculations, in that condition")
      warning("Bias indices missing:
              at least one participant has missing data from at one condition
              These cases are removed from calculating reliability estimates
              $omitted contains the missing cases")
    }

    # remove NA rows
    findata2 <-  na.omit(findata)
    findata2$iteration <- as.factor(findata2$iteration)

    # calculate correlations per condition and iteration

    splithalf <- findata2 %>%
      dplyr::group_by(iteration, condition) %>%
      dplyr::summarise(n = round(sum(!is.na(bias1)),2),
                       splithalf = cor(bias1, bias2, use = "pairwise.complete"),
                       spearmanbrown = (2 * cor(bias1, bias2,
                                                use = "pairwise.complete")) /
                         (1 + (2 - 1) * cor(bias1, bias2,
                                           use = "pairwise.complete")))




    # take the mean estimates per condition

    splithalf2 <- splithalf %>%
      dplyr::group_by(condition) %>%
      dplyr::summarise(n = mean(n),
                       splithalf_estimate = round(mean(splithalf),2),
                       splithalf95CI_lower = round(quantile(splithalf, c(.025), names = F),2),
                       splithalf95CI_upper = round(quantile(splithalf, c(.975), names = F),2),
                       spearmanbrown_estimate = round(mean(spearmanbrown),2),
                       spearmanbrown95CI_lower = round(quantile(spearmanbrown, c(.025), names = F),2),
                       spearmanbrown95CI_upper = round(quantile(spearmanbrown, c(.975), names = F),2)  ) %>%
      as.data.frame()


    colnames(splithalf2) <- c("condition", "n",
                              paste("mc",no.iterations,"splithalf", sep = ""),
                              "95_low",
                              "95_high",
                              paste("mc",no.iterations,"spearmanbrown", sep = ""),
                              "SB_low",
                              "SB_high")


    print(paste("split half estimates for", no.iterations, "random splits",
                sep = " "))

    if (sum(is.na(findata$bias1)) +
        sum(is.na(findata$bias2)) > 0)
    {
      return(list(Estimates = splithalf2, omitted = omitted))
    } else {
      return(splithalf2)
    }

    }
  }
