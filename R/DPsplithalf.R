#' Dot-Probe Split Half
#'
#' This function calculates split half reliability estimates for Dot Probe data
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
#' @param removelist specifies a list of participants to be removed
#' @return Returns a data frame containing split-half reliability estimates for each condition specified.
#' @return splithalf returns the raw estimate
#' @return spearmanbrown returns the spearman-brown corrected estimate
#' @import plyr
#' @import stats
#' @export

DPsplithalf <- function(data, RTmintrim, RTmaxtrim, incErrors = FALSE,
                     conditionlist, halftype, no.iterations = 1,
                     var.RT = "latency", var.condition = "blockcode",
                     var.participant = "subject", var.correct = "correct",
                     removelist = "")
{
  # create empty objects for the purposes of binding global variables
  RT <- 0
  correct <- 0
  participant <- 0
  condition <- 0
  half1bias <- 0
  half2bias <- 0
  iteration <- 0
  N <- 0
  spearmanbrown <- 0

# renames the dataset variables to fit with the code
data$RT <- data[, var.RT]
data$condition <- data[, var.condition]
data$participant <- data[, var.participant]
data$correct <- data[, var.correct]

# for randdom samples, the number of samples drawn
iterations <- 1:no.iterations

# removes trials below the minimum cutoff and above the maximum cutoff
dataset <- subset(data, RT > RTmintrim & RT < RTmaxtrim)

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

      half1.congruent   <- mean(subset(temp$RT, temp$congruency ==
                                "Congruent" & temp$trialnum%%2 == 0),
                                na.rm = T)
      half1.incongruent <- mean(subset(temp$RT, temp$congruency ==
                                "Incongruent" & temp$trialnum%%2 == 0),
                                na.rm = T)
      half2.congruent   <- mean(subset(temp$RT, temp$congruency ==
                                "Congruent" & temp$trialnum%%2 == 1),
                                na.rm = T)
      half2.incongruent <- mean(subset(temp$RT, temp$congruency ==
                                "Incongruent" & temp$trialnum%%2 == 1),
                                na.rm = T)

      finalData[l, 3:6] <- c(half1.congruent, half1.incongruent,
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

       half1.congruent  <- mean(subset(half1$RT,
                                       half1$participant == i &
                                       half1$condition == j &
                                       half1$congruency == "Congruent"),
                                       na.rm = T)
       half1.incongruent <- mean(subset(half1$RT,
                                        half1$participant == i &
                                        half1$condition == j &
                                        half1$congruency == "Incongruent"),
                                        na.rm = T)
       half2.congruent  <- mean(subset(half2$RT,
                                       half2$participant == i &
                                       half2$condition == j &
                                       half2$congruency == "Congruent"),
                                       na.rm = T)
       half2.incongruent <- mean(subset(half2$RT,
                                        half2$participant == i &
                                        half2$condition == j
                                        & half2$congruency == "Incongruent"),
                                        na.rm = T)

       finalData[l, 3:6] <- c(half1.congruent, half1.incongruent,
                              half2.congruent, half2.incongruent)

       l <- l + 1
      }
      print(paste("condition", j, "complete"))
    }
  }
  }
    names(finalData)[1] <- "participant"
    names(finalData)[2] <- "condition"

  if (sum(is.na(finalData$half1.congruent)) +
      sum(is.na(finalData$half1.incongruent)) +
      sum(is.na(finalData$half2.congruent)) +
      sum(is.na(finalData$half2.incongruent)) > 0)
  {
  print("the following is a dataset containing participants with missing data")
  omitted <- finalData[!complete.cases(finalData), ]
  print(omitted)
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
  finalData2$half1bias <- finalData2$half1.incongruent -
                          finalData2$half1.congruent
  finalData2$half2bias <- finalData2$half2.incongruent -
                          finalData2$half2.congruent

  # create calculate estimates
  SplitHalf <- plyr::ddply(finalData2, ~condition, summarise,
                     N = sum(!is.na(half1bias)),
                     splithalf = cor(half1bias, half2bias,
                                     use = "pairwise.complete"),
                     spearmanbrown = (2 * cor(half1bias, half2bias,
                                              use = "pairwise.complete"))/
                                     (1 + (2 - 1) * cor(half1bias, half2bias,
                                              use = "pairwise.complete")))

  print(SplitHalf)

  if (sum(is.na(finalData$half1.congruent)) +
      sum(is.na(finalData$half1.incongruent)) +
      sum(is.na(finalData$half2.congruent)) +
      sum(is.na(finalData$half2.incongruent)) > 0)
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
                        bias1 = NA, bias2 = NA)
  # loop counter
  l <- 1

  # create vectors to contain both halfs to be compared
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
    temp.con   <- subset(dataset$RT, dataset$participant == i &
                         dataset$condition == j &
                         dataset$congruency == "Congruent")
    temp.incon <- subset(dataset$RT, dataset$participant ==
                         i & dataset$condition == j &
                         dataset$congruency == "Incongruent")

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

      bias1 <- mean(h1.incongruent) - mean(h1.congruent)
      bias2 <- mean(h2.incongruent) - mean(h2.congruent)

      bias1v[l] <- bias1
      bias2v[l] <- bias2

      l <- l + 1
      }
    }
    print(paste("condition", j, "complete"))
  }

  print("Calculating split half estimates")

  finData$bias1 <- bias1v
  finData$bias2 <- bias2v

  names(finData)[1] <- "condition"
  names(finData)[2] <- "participant"
  names(finData)[3] <- "iteration"

  if (sum(is.na(finData$half1.congruent)) +
      sum(is.na(finData$half1.incongruent)) +
      sum(is.na(finData$half2.congruent)) +
      sum(is.na(finData$half2.incongruent)) > 0)
  {
  print("the following is a dataset containing iterations with missing data")
  omitted <- finData[!complete.cases(finData), ]
  print(omitted)
  print("note: these iterations will be removed from the split half reliability
        calculations, in that condition")
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
                     splithalf = cor(bias1, bias2, use = "pairwise.complete"),
                     spearmanbrown = (2 * cor(bias1, bias2,
                                              use = "pairwise.complete"))/
                                     (1 +(2 - 1) * cor(bias1, bias2,
                                              use = "pairwise.complete")))

  # take the mean estimates per condition
  SplitHalf2 <- plyr::ddply(SplitHalf, .(condition), summarise, N = mean(N),
                      splithalf = mean(splithalf),
                      spearmanbrown = mean(spearmanbrown))

  print(paste("Split half estimates for", no.iterations, "random splits",
              sep = " "))
  print(SplitHalf2)

  if (sum(is.na(finData$half1.congruent)) +
      sum(is.na(finData$half1.incongruent)) +
      sum(is.na(finData$half2.congruent)) +
      sum(is.na(finData$half2.incongruent)) > 0)
  {
  return(list(Estimates = SplitHalf2, omitted = omitted))
  } else {
  return(SplitHalf2)
  }

  }
}
