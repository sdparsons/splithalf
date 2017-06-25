#' Task switching paradigm Split Half
#'
#' This function calculates split half reliability estimates for a task switching task
#' @param data specifies the raw dataset to be processed
#' @param RTmintrim specifies the lower cut-off point for RTs
#' @param RTmaxtrim specifies the maximum cut-off point for RTs
#' @param no.iterations specifies the number of random splits to run
#' @param incErrors include incorrect trials?, defaults to FALSE
#' @param conditionlist sets conditions/blocks to be processed
#' @param halftype specifies the split method; "oddeven", "halfs", or "random"
#' @param var.RT specifies the RT variable name in data
#' @param var.condition specifies the condition variable name in data
#' @param var.participant specifies the subject variable name in data
#' @param var.correct specifies the accuracy variable name in data
#' @param var.trialnum specifies the trial number variable
#' @param removelist specifies a list of participants to be removed
#' @return Returns a data frame containing split-half reliability estimates for the switch cost index in each condition specified. Also returns split half reliability estimates for repeat and switch trials separately.
#' @return splithalf returns the raw estimate of the swith cost index
#' @return spearmanbrown returns the spearman-brown corrected estimate of the switch cost index
#' @return REPEATsplithalf returns the raw estimate of repeat trials
#' @return REPEATspearmanbrown returns the spearman-brown corrected estimate of repeat trials
#' @return SWITCHsplithalf returns the raw estimate of switch trials
#' @return SWITCHspearmanbrown returns the spearman-brown corrected estimate of switch trials
#' @return Warning: If there are missing data (e.g one condition data missing for one participant) output will include details of the missing data and return a dataframe containing the NA data. Warnings will be displayed in the console.
#' @examples
#' ## split half estimates for the bias index and repeat/switch trials in two blocks
#' ## using 50 iterations of the random split method (Note: 5000 would be standard)
#' TSTsplithalf(TSTdata, conditionlist = c("block1","block2"),
#' halftype = "random", no.iterations = 50)
#' #' ## In datasets with missing data an additional output is generated
#' ## the console will return a list of participants/blocks
#' ## the output will also include a full dataframe of missing values
#' TSTsplithalf(TSTdata_missing, conditionlist = c("block1","block2"),
#' halftype = "random", no.iterations = 50)
#' @import plyr
#' @import stats
#' @export

TSTsplithalf <- function(data, RTmintrim = 'none', RTmaxtrim = 'none',
                         incErrors = FALSE, conditionlist, halftype,
                         no.iterations = 1, var.RT = "latency",
                         var.condition = "blockcode",
                         var.participant = "subject",
                         var.correct = "correct", var.trialnum = "trialnum",
                         removelist = "")
{
  # check for missing variables
  if(halftype != "oddeven" & halftype != "halfs" & halftype != "random") {
    stop("the halftype has not been specified")
  }

  # check if the trialtype variable exists
  if("trialtype" %in% colnames(data) == FALSE) {
    stop("the trialype (Repeat/Switch) variable does not exist")
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
  h1rep <- 0
  h1swit <- 0
  h2rep <- 0
  h2swit <- 0
  iteration <- 0
  N <- 0
  REPEATsplithalf <- 0
  REPEATspearmanbrown <- 0
  REPEATtwoalpha <- 0
  SWITCHsplithalf <- 0
  SWITCHspearmanbrown <- 0
  SWITCHtwoalpha <- 0
  spearmanbrown <- 0
  twoalpha <- 0

  # renames the dataset variables to fit with the code
  data$RT <- data[, var.RT]
  data$condition <- data[, var.condition]
  data$participant <- data[, var.participant]
  data$correct <- data[, var.correct]
  data$trialnum <- data[, var.trialnum]


  # for randdom samples, the number of samples drawn
  iterations <- 1:no.iterations

  # loads data into dataset
  dataset <- data

  # removes trials below the minimum cutoff and above the maximum cutoff
  if (is.numeric(RTmintrim) == TRUE)
  {
    dataset <- subset(dataset, RT > RTmintrim)
  }
  if (is.numeric(RTmaxtrim) == TRUE)
  {
    dataset <- subset(dataset, RT < RTmaxtrim)
  }

  # removes participants specified to be removed in removelist
  dataset <- dataset[!dataset$participant %in% removelist, ]

  # removes errors if FALSE, includes error trials if TRUE
  if (incErrors == FALSE)
    dataset <- subset(dataset, correct == 1)

  # creates a list of participants
  plist <- sort(unique(dataset$participant))


  if (halftype == "oddeven" | halftype == "halfs")
  {
    finalData <- data.frame(i = rep(plist, times = length(conditionlist)),
                            j = rep(conditionlist, each = length(plist)),
                            half1.repeat = NA, half1.switch = NA,
                            half2.repeat = NA, half2.switch = NA)
    l <- 1

    if (halftype == "oddeven")
    {
      # this loop creates a dataframe split by odd and even trial numbers
      # giving mean RTs in repeat and switch conditions for each
      # split

      for (j in conditionlist)
      {
        for (i in plist)
        {
          temp <- subset(dataset, participant == i & condition == j)

          half1.repeat   <- mean(subset(temp$RT,
                                           temp$trialtype == "Repeat" &
                                             temp$trialnum%%2 == 0),
                                    na.rm = T)
          half1.switch <- mean(subset(temp$RT,
                                           temp$trialtype == "Switch" &
                                             temp$trialnum%%2 == 0),
                                    na.rm = T)
          half2.repeat   <- mean(subset(temp$RT,
                                           temp$trialtype == "Repeat" &
                                             temp$trialnum%%2 == 1),
                                    na.rm = T)
          half2.switch <- mean(subset(temp$RT,
                                           temp$trialtype == "Switch" &
                                             temp$trialnum%%2 == 1),
                                    na.rm = T)

          finalData[l, 3:6] <- c(half1.repeat, half1.switch,
                                 half2.repeat, half2.switch)

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

            half1.repeat  <- mean(subset(half1$RT,
                                            half1$participant == i &
                                              half1$condition == j &
                                              half1$trialtype == "Repeat"),
                                     na.rm = T)
            half1.switch <- mean(subset(half1$RT,
                                             half1$participant == i &
                                               half1$condition == j &
                                               half1$trialtype == "Switch"),
                                      na.rm = T)
            half2.repeat  <- mean(subset(half2$RT,
                                            half2$participant == i &
                                              half2$condition == j &
                                              half2$trialtype == "Repeat"),
                                     na.rm = T)
            half2.switch <- mean(subset(half2$RT,
                                             half2$participant == i &
                                               half2$condition == j &
                                               half2$trialtype == "Switch"),
                                      na.rm = T)

            finalData[l, 3:6] <- c(half1.repeat, half1.switch,
                                   half2.repeat, half2.switch)

            l <- l + 1
          }
          print(paste("condition", j, "complete"))
        }
      }
    }
    names(finalData)[1] <- "participant"
    names(finalData)[2] <- "condition"

    if (sum(is.na(finalData$half1.repeat)) +
        sum(is.na(finalData$half1.switch)) +
        sum(is.na(finalData$half2.repeat)) +
        sum(is.na(finalData$half2.switch)) > 0)
    {
      print("the following are participants/conditions with missing data")
      omitted <- finalData[!complete.cases(finalData), ]
      print(unique(omitted[c("condition", "participant")]))
      print("note: these particpants will be removed from the split half
            reliability calculations, in that condition")
      warning("Bias indices missing:
              at least one participant has missing data from at one condition
              These cases are removed from calculating reliability estimates
              $omitted contains the missing cases")
    }

    # remove NA rows
    finalData2 <- na.omit(finalData)

    # calculate bias indices
    finalData2$half1bias <- finalData2$half1.switch -
      finalData2$half1.repeat
    finalData2$half2bias <- finalData2$half2.switch -
      finalData2$half2.repeat

    # create calculate estimates
    SplitHalf <- plyr::ddply(finalData2, ~condition, summarise,
                             N = sum(!is.na(half1bias)),
                             REPEATsplithalf = cor(half1.repeat,
                                                half2.repeat,
                                                use = "pairwise.complete"),
                             REPEATspearmanbrown = (2 * cor(half1.repeat,
                                                         half2.repeat,
                                                         use = "pairwise.complete"))/
                               (1 + (2 - 1) * cor(half1.repeat,
                                                  half2.repeat,
                                                  use = "pairwise.complete")),
                             REPEATtwoalpha = (4*cor(half1.repeat, half2.repeat,
                                                  use = "pairwise.complete")*
                                              sd(half1.repeat, na.rm = TRUE)*
                                              sd(half2.repeat, na.rm = TRUE)) /
                               ((sd(half1.repeat, na.rm = TRUE)^2) +
                                  (sd(half2.repeat, na.rm = TRUE)^2) +
                                  (2*cor(half1.repeat, half2.repeat,
                                         use = "pairwise.complete")*
                                     sd(half1.repeat, na.rm = TRUE)*
                                     sd(half2.repeat, na.rm = TRUE))),
                             SWITCHsplithalf = cor(half1.switch,
                                                  half2.switch,
                                                  use = "pairwise.complete"),
                             SWITCHspearmanbrown = (2 * cor(half1.switch,
                                                           half2.switch,
                                                           use = "pairwise.complete"))/
                               (1 + (2 - 1) * cor(half1.switch,
                                                  half2.switch,
                                                  use = "pairwise.complete")),
                             SWITCHtwoalpha = (4*cor(half1.switch, half2.switch,
                                                    use = "pairwise.complete")*
                                                sd(half1.switch, na.rm = TRUE)*
                                                sd(half2.switch, na.rm = TRUE)) /
                               ((sd(half1.switch, na.rm = TRUE)^2) +
                                  (sd(half2.switch, na.rm = TRUE)^2) +
                                  (2*cor(half1.switch, half2.switch,
                                         use = "pairwise.complete")*
                                     sd(half1.switch, na.rm = TRUE)*
                                     sd(half2.switch, na.rm = TRUE))),
                             splithalf = cor(half1bias, half2bias),
                             spearmanbrown = (2 * cor(half1bias, half2bias,
                                                      use = "pairwise.complete"))/
                               (1 + (2 - 1) * cor(half1bias, half2bias,
                                                  use = "pairwise.complete")),
                             twoalpha = (4*cor(half1bias, half2bias,
                                               use = "pairwise.complete")*
                                           sd(half1bias, na.rm = TRUE)*
                                           sd(half2bias, na.rm = TRUE)) /
                               ((sd(half1bias, na.rm = TRUE)^2) +
                                  (sd(half2bias, na.rm = TRUE)^2) +
                                  (2*cor(half1bias, half2bias,
                                         use = "pairwise.complete")*
                                     sd(half1bias, na.rm = TRUE)*
                                     sd(half2bias, na.rm = TRUE))))

    if (sum(is.na(finalData$half1.repeat)) +
        sum(is.na(finalData$half1.switch)) +
        sum(is.na(finalData$half2.repeat)) +
        sum(is.na(finalData$half2.switch)) > 0)
    {
      return(list(Estimates = SplitHalf, omitted = omitted))
    } else {
      return(SplitHalf)
    }

    }

  if (halftype == "random")
  {
    # create the data.frame to populate
    finData <- data.frame(j = rep(conditionlist, each = (length(plist) *
                                                           length(iterations))),
                          i = rep(plist, each = length(iterations)),
                          h = rep(iterations, times = (length(conditionlist) *
                                                         length(plist))),
                          h1rep = NA, h1swit = NA,
                          h2rep = NA, h2swit = NA,
                          bias1 = NA, bias2 = NA)
    # loop counter
    l <- 1

    # create vectors to contain both halfs to be compared

    h1repv   <- vector(length = (length(conditionlist) * length(plist) *
                                   length(iterations)))
    h1switv <- vector(length = (length(conditionlist) * length(plist) *
                                   length(iterations)))
    h2repv   <- vector(length = (length(conditionlist) * length(plist) *
                                   length(iterations)))
    h2switv <- vector(length = (length(conditionlist) * length(plist) *
                                   length(iterations)))
    bias1v <- vector(length = (length(conditionlist) * length(plist) *
                                 length(iterations)))
    bias2v <- vector(length = (length(conditionlist) * length(plist) *
                                 length(iterations)))

    for (j in conditionlist)
    {
      for (i in plist)
      {
        # subset the dataframe into RT vectors by participant, condition, and
        # congruency
        temp.rep   <- subset(dataset$RT, dataset$participant == i &
                               dataset$condition == j &
                               dataset$trialtype == "Repeat")
        temp.swit <- subset(dataset$RT, dataset$participant == i &
                               dataset$condition == j &
                               dataset$trialtype == "Switch")

        # calculates what will be the middle numbered trial in each congruent
        # and incongruent list
        midtrial.rep <- sum(!is.na(temp.rep))/2
        midtrial.swit <- sum(!is.na(temp.swit))/2

        ii.rep <- seq(temp.rep)
        ii.swit <- seq(temp.swit)

        # in the following loop random halves of each repeat and switch
        # list are taken the ind objects contain the trial list, and the h1/h2
        # contain the actual RTs the bias indices are then calculated and added
        # to the vector
        for (h in iterations)
        {
          ind1.rep <- sample(ii.rep, midtrial.rep)
          ind2.rep <- ii.rep[!ii.rep %in% ind1.rep]
          ind1.swit <- sample(ii.swit, midtrial.swit)
          ind2.swit <- ii.swit[!ii.swit %in% ind1.swit]

          h1.repeat <- temp.rep[ind1.rep]
          h1.switch <- temp.swit[ind1.swit]
          h2.repeat <- temp.rep[ind2.rep]
          h2.switch <- temp.swit[ind2.swit]

          h1repv[l]   <- mean(h1.repeat)
          h1switv[l] <- mean(h1.switch)
          h2repv[l]   <- mean(h2.repeat)
          h2switv[l] <- mean(h2.switch)

          bias1v[l] <- mean(h1.switch) - mean(h1.repeat)
          bias2v[l] <- mean(h2.switch) - mean(h2.repeat)

          l <- l + 1
        }
      }
      print(paste("condition", j, "complete"))
    }

    print("Calculating split half estimates")

    finData$h1rep   <- h1repv
    finData$h1swit  <- h1switv
    finData$h2rep   <- h2repv
    finData$h2swit  <- h2switv
    finData$bias1   <- bias1v
    finData$bias2   <- bias2v

    names(finData)[1] <- "condition"
    names(finData)[2] <- "participant"
    names(finData)[3] <- "iteration"

    if (sum(is.na(finData$bias1)) +
        sum(is.na(finData$bias2)) > 0)
    {
      print("the following are participants/conditions with missing data")
      omitted <- finData[!complete.cases(finData), ]
      print(unique(omitted[c("condition", "participant")]))
      print("note: these iterations will be removed from the split half
            reliability calculations, in that condition")
      warning("Bias indices missing:
              at least one participant has missing data from at one condition
              These cases are removed from calculating reliability estimates
              $omitted contains the missing cases")
    }

    # remove NA rows
    finData2 <- na.omit(finData)

    # calculate correlations per condition and iteration
    SplitHalf <- plyr::ddply(finData2, .(iteration, condition), summarise,
                             N = sum(!is.na(bias1)),
                             REPEATsplithalf = cor(h1rep, h2rep,
                                                use = "pairwise.complete"),
                             REPEATspearmanbrown = (2 * cor(h1rep, h2rep,
                                                         use = "pairwise.complete"))/
                               (1 + (2 - 1) * cor(h1rep, h2rep,
                                                  use = "pairwise.complete")),
                             REPEATtwoalpha = (4*cor(h1rep, h2rep, use = "pairwise.complete")*
                                              sd(h1rep)*sd(h2rep))/
                               ((sd(h1rep)^2) + (sd(h2rep)^2) +
                                  (2*cor(h1rep, h2rep, use = "pairwise.complete")
                                   *sd(h1rep)*sd(h2rep))),
                             SWITCHsplithalf = cor(h1swit, h2swit,
                                                  use = "pairwise.complete"),
                             SWITCHspearmanbrown = (2 * cor(h1swit, h2swit,
                                                           use = "pairwise.complete"))/
                               (1 + (2 - 1) * cor(h1swit, h2swit,
                                                  use = "pairwise.complete")),
                             SWITCHtwoalpha = (4*cor(h1rep, h2rep, use = "pairwise.complete")*
                                                sd(h1rep)*sd(h2rep))/
                               ((sd(h1rep)^2) + (sd(h2rep)^2) +
                                  (2*cor(h1rep, h2rep, use = "pairwise.complete")
                                   *sd(h1rep)*sd(h2rep))),
                             splithalf = cor(bias1, bias2, use = "pairwise.complete"),
                             spearmanbrown = (2 * cor(bias1, bias2,
                                                      use = "pairwise.complete"))/
                               (1 +(2 - 1) * cor(bias1, bias2,
                                                 use = "pairwise.complete")),
                             twoalpha = (4*cor(bias1, bias2, use = "pairwise.complete")*
                                           sd(bias1)*sd(bias2))/
                               ((sd(bias1)^2) + (sd(bias2)^2) +
                                  (2*cor(bias1, bias2, use = "pairwise.complete")
                                   *sd(bias1)*sd(bias2))))

    # take the mean estimates per condition
    SplitHalf2 <- plyr::ddply(SplitHalf, .(condition), summarise, N = mean(N),
                              REPEATsplithalf = mean(REPEATsplithalf),
                              REPEATspearmanbrown = mean(REPEATspearmanbrown),
                              REPEATtwoalpha = mean(REPEATtwoalpha),
                              SWITCHsplithalf = mean(SWITCHsplithalf),
                              SWITCHspearmanbrown = mean(SWITCHspearmanbrown),
                              SWITCHtwoalpha = mean(SWITCHtwoalpha),
                              splithalf = mean(splithalf),
                              spearmanbrown = mean(spearmanbrown),
                              twoalpha = mean(twoalpha))

    colnames(SplitHalf2) <- c("condition", "N",
                              paste("mc",no.iterations,"splithalf_REP", sep = ""),
                              paste("mc",no.iterations,"spearmanbrown_REP", sep = ""),
                              paste("mc",no.iterations,"twoalpha_REP", sep = ""),
                              paste("mc",no.iterations,"splithalf_SW", sep = ""),
                              paste("mc",no.iterations,"spearmanbrown_SW", sep = ""),
                              paste("mc",no.iterations,"twoalpha_SW", sep = ""),
                              paste("mc",no.iterations,"splithalf", sep = ""),
                              paste("mc",no.iterations,"spearmanbrown", sep = ""),
                              paste("mc",no.iterations,"twoalpha", sep = ""))

    print(paste("Split half estimates for", no.iterations, "random splits",
                sep = " "))

    if (sum(is.na(finData$bias1)) +
        sum(is.na(finData$bias2)) > 0)
    {
      return(list(Estimates = SplitHalf2, omitted = omitted))
    } else {
      return(SplitHalf2)
    }

    }
}
