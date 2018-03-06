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
#' ## using 50 iterations of the random split method (note: 5000 would be standard)
#' splithalf(DPdata, conditionlist = c("block1","block2"), halftype = "random",
#' no.iterations = 50)
#' ## In datasets with missing data an additional output is generated
#' ## the console will return a list of participants/blocks
#' ## the output will also include a full dataframe of missing values
#' splithalf(DPdata_missing, conditionlist = c("block1","block2"),
#' halftype = "random", no.iterations = 50)
#' @import tidyverse
#' @import dplyr
#' @import stats
#' @export
#'
splithalf_ACC <- function(data,
                      RTmintrim = 'none',
                      RTmaxtrim = 'none',
                      conditionlist = FALSE,
                      halftype,
                      no.iterations = 1,
                      var.RT = "latency",
                      var.condition = FALSE,
                      var.participant = "subject",
                      var.correct = "correct",
                      var.trialnum = "trialnum",
                      removelist = "",
                      sdtrim = FALSE
                      )
{

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


  # for running without a condition list
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
  iteration <- 0
  N <- 0
  spearmanbrown <- 0

  # set the data as a data.frame to avoid tibble issues
  data <- as.data.frame(data)

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

  # removes participants specified to be removed in removelist
  dataset <- dataset[!dataset$participant %in% removelist, ]

  # how many participants?
  n_par <- n_distinct(dataset$participant)


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
  if(is.numeric(sdtrim)) {
    dataset <- dataset %>%
      group_by(participant, condition) %>%
      mutate(low =  mean(RT) - (sdtrim * sd(RT)),
             high = mean(RT) + (sdtrim * sd(RT))) %>%
      filter(RT >= low & RT <= high)
  }

  # checks whether user difference score is based on means or medians


## Main splithalf processing


  if (halftype == "oddeven" | halftype == "halfs")
  {
    finaldata <- data.frame(i = rep(plist, times = length(conditionlist)),
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

          half1 <- sum(subset(temp$correct, temp$trialnum%%2 == 0),
                               na.rm = T)
          half2 <- sum(subset(temp$correct, temp$trialnum%%2 == 0),
                               na.rm = T)

          finaldata[l, 3:4] <- c(half1, half2)

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

            half1  <- sum(subset(half1$correct, half1$participant == i &
                                  half1$condition == j), na.rm = T)
            half2  <- sum(subset(half2$correct, half2$participant == i &
                                  half2$condition == j), na.rm = T)


            finaldata[l, 3:4] <- c(half1, half2)

            l <- l + 1
          }
          print(paste("condition", j, "complete"))
        }
      }
    }
    names(finaldata)[1] <- "participant"
    names(finaldata)[2] <- "condition"

    if (sum(is.na(finaldata$half1) + is.na(finaldata$half2)) > 0)
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

    # create calculate estimates
    splithalf <- finaldata2 %>%
      dplyr::group_by(condition) %>%
      dplyr::summarise(n = sum(!is.na(half1)),
                       splithalf = cor(half1, half2,
                                       use = "pairwise.complete"),
                       spearmanbrown = (2 * cor(half1, half2,
                                                use = "pairwise.complete"))/
                         (1 + (2 - 1) * cor(half1, half2,
                                            use = "pairwise.complete")))



    if (sum(is.na(finaldata$half1) + is.na(finaldata$half2)) > 0)
    {
      return(list(Estimates = splithalf, omitted = omitted))
    } else {
      return(splithalf)
    }

    }

  if (halftype == "random")
  {
    # create the data.frame to populate
    findata <- data.frame(j = rep(conditionlist, each = (length(plist) *
                                                         length(iterations))),
                          i = rep(plist, each = length(iterations)),
                          h = rep(iterations, times = (length(conditionlist) *
                                                         length(plist))),
                          half1 = NA, half2 = NA)
    # loop counter
    l <- 1

    # participant loop counter for progress bar
    ppt <- 1

    # create vectors to contain both halfs to be compared
    half1v <- vector(length = (length(conditionlist) * length(plist) *
                                 length(iterations)))
    half2v <- vector(length = (length(conditionlist) * length(plist) *
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
        temp <- subset(dataset$correct, dataset$participant == i &
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

          half1v[l] <- sum(h1)
          half2v[l] <- sum(h2)

          l <- l + 1
        }

        ppt <- ppt + 1
        setTxtProgressBar(pb, ppt)
      }
      ppt <- 1 # reset the progress bar

      print(paste("condition", j, "complete"))
    }

    print("Calculating split half estimates")

    findata$half1 <- half1v
    findata$half2 <- half2v

    names(findata)[1] <- "condition"
    names(findata)[2] <- "participant"
    names(findata)[3] <- "iteration"

    if (sum(is.na(findata$half1) + sum(is.na(findata$half2)) > 0))
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
    findata2 <- na.omit(findata)

    # calculate correlations per condition and iteration
    splithalf <- findata2 %>%
      group_by(iteration, condition) %>%
      summarise(n = sum(!is.na(half1)),
                splithalf = cor(half1, half2,
                                use = "pairwise.complete"),
                spearmanbrown = (2 * cor(half1, half2,
                                         use = "pairwise.complete"))/
                  (1 +(2 - 1) * cor(half1, half2,
                                    use = "pairwise.complete")))


    # take the mean estimates per condition
    splithalf2 <- splithalf %>%
                    group_by(condition) %>%
                    summarise(
                        n = mean(n),
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

    print(paste("Split half estimates for", no.iterations, "random splits",
                sep = " "))

    if (sum(is.na(findata$half1)) + sum(is.na(findata$half2) > 0))
    {
      return(list(Estimates = splithalf2, omitted = omitted))
    } else {
      return(splithalf2)
    }

    }
  }
