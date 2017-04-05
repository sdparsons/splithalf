#' Dot-Probe Split Half
#'
#' This function calculates split half reliability estimates
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
#' @return Returns a data frame containing split-half reliability estimates for each condition specified.
#' @return splithalf returns the raw estimate
#' @return spearmanbrown returns the spearman-brown corrected estimate
#' @return Warning: If there are missing data (e.g one condition data missing for one participant) output will include details of the missing data and return a dataframe containing the NA data. Warnings will be displayed in the console.
#' @examples
#' ## split half estimates for two blocks of the task
#' ## using 5000 iterations of the random split method
#' splithalf(DPdata, conditionlist = c("block1","block2"), halftype = "random",
#' no.iterations = 5000)
#' ## In datasets with missing data an additional output is generated
#' ## the console will return a list of participants/blocks
#' ## the output will also include a full dataframe of missing values
#' splithalf(DPdata_missing, conditionlist = c("block1","block2"),
#' halftype = "random", no.iterations = 5000)
#' @import plyr
#' @import stats
#' @export
#'
splithalf <- function(data, RTmintrim = 'none', RTmaxtrim = 'none',
                      incErrors = FALSE,
                      conditionlist, halftype, no.iterations = 1,
                      var.RT = "latency", var.condition = "blockcode",
                      var.participant = "subject", var.correct = "correct",
                      var.trialnum = "trialnum", removelist = "")
{
  # check for missing variables
  if(halftype != "oddeven" & halftype != "halfs" & halftype != "random") {
    stop("the halftype has not been specified")
  }

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
                            half1 = NA, half2 = NA)
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

          half1 <- mean(subset(temp$RT, temp$trialnum%%2 == 0),
                               na.rm = T)
          half2 <- mean(subset(temp$RT, temp$trialnum%%2 == 0),
                               na.rm = T)

          finalData[l, 3:4] <- c(half1, half2)

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

            half1  <- mean(subset(half1$RT, half1$participant == i &
                                  half1$condition == j), na.rm = T)
            half2  <- mean(subset(half2$RT, half2$participant == i &
                                  half2$condition == j), na.rm = T)


            finalData[l, 3:4] <- c(half1, half2)

            l <- l + 1
          }
          print(paste("condition", j, "complete"))
        }
      }
    }
    names(finalData)[1] <- "participant"
    names(finalData)[2] <- "condition"

    if (sum(is.na(finalData$half1) + is.na(finalData$half2)) > 0)
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

    # create calculate estimates
    SplitHalf <- ddply(finalData2, ~condition, summarise,
                       N = sum(!is.na(half1)),
                       splithalf = cor(half1, half2,
                                       use = "pairwise.complete"),
                       spearmanbrown = (2 * cor(half1, half2,
                                                use = "pairwise.complete"))/
                         (1 + (2 - 1) * cor(half1, half2,
                                            use = "pairwise.complete")))

    if (sum(is.na(finalData$half1) + is.na(finalData$half2)) > 0)
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
                          half1 = NA, half2 = NA)
    # loop counter
    l <- 1

    # create vectors to contain both halfs to be compared
    half1v <- vector(length = (length(conditionlist) * length(plist) *
                                 length(iterations)))
    half2v <- vector(length = (length(conditionlist) * length(plist) *
                                 length(iterations)))

    for (j in conditionlist)
    {
      for (i in plist)
      {
        # subset the dataframe into RT vectors by participant, condition, and
        # congruency
        temp <- subset(dataset$RT, dataset$participant == i &
                       dataset$condition == j)


        # calculates what will be the middle numbered trial in each congruent
        # and incongruent list
        midtrial <- sum(!is.na(temp))/2

        ii <- seq(temp)

        # in the following loop random halves of each congruent and incongruent
        # list are taken the ind objects contain the trial list, and the h1/h2
        # contain the actual RTs the bias indices are then calculated and added
        # to the vector
        for (h in iterations)
        {
          ind1 <- sample(ii, midtrial)
          ind2 <- ii[!ii %in% ind1]

          h1 <- temp[ind1]
          h2 <- temp[ind2]

          half1v[l] <- mean(h1)
          half2v[l] <- mean(h2)

          l <- l + 1
        }
      }
      print(paste("condition", j, "complete"))
    }

    print("Calculating split half estimates")

    finData$half1 <- half1v
    finData$half2 <- half2v

    names(finData)[1] <- "condition"
    names(finData)[2] <- "participant"
    names(finData)[3] <- "iteration"

    if (sum(is.na(finData$half1) + sum(is.na(finData$half2)) > 0))
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
    SplitHalf <- ddply(finData2, .(iteration, condition), summarise,
                       N = sum(!is.na(half1)),
                       splithalf = cor(half1, half2,
                                       use = "pairwise.complete"),
                       spearmanbrown = (2 * cor(half1, half2,
                                                use = "pairwise.complete"))/
                         (1 +(2 - 1) * cor(half1, half2,
                                           use = "pairwise.complete")))

    # take the mean estimates per condition
    SplitHalf2 <- ddply(SplitHalf, .(condition), summarise, N = mean(N),
                        splithalf = mean(splithalf),
                        spearmanbrown = mean(spearmanbrown))

    print(paste("Split half estimates for", no.iterations, "random splits",
                sep = " "))

    if (sum(is.na(finData$half1)) + sum(is.na(finData$half2) > 0))
    {
      return(list(Estimates = SplitHalf2, omitted = omitted))
    } else {
      return(SplitHalf2)
    }

    }
  }
