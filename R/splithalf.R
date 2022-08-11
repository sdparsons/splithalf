#' Internal consistency of task measures via a permutation split-half reliability approach
#'
#' This function calculates split half reliability estimates via a permutation approach for a wide range of tasks. Most of the user inputs relate to the variables in the dataset splithalf needs to read in order to estimate reliability. Currently supports response time and accuracy outcomes, for several scoring methods: average, difference, difference of difference scores, and a DPrime development.
#' The (unofficial) version name is "This function gives me the power to fight like a crow"
#' @param data specifies the raw dataset to be processed
#' @param outcome indicates the type of data to be processed, e.g. "RT" or "accuracy"
#' @param score indicates how the outcome score is calculated, e.g. most commonly the difference score between two trial types. Can be "average", "difference", "difference_of_difference", and "DPrime"
#' @param conditionlist sets conditions/blocks to be processed
#' @param halftype specifies the split method; "oddeven", "halfs", or "random"
#' @param permutations specifies the number of random splits to run - 5000 is good
#' @param var.RT specifies the RT variable name in data
#' @param var.ACC specific the accuracy variable name in data
#' @param var.condition specifies the condition variable name in data - if not specified then splithalf will treat all trials as one condition
#' @param var.participant specifies the subject variable name in data
#' @param var.compare specifies the variable that is used to calculate difference scores (e.g. including congruent and incongruent trials)
#' @param compare1 specifies the first trial type to be compared (e.g. congruent trials)
#' @param compare2 specifies the second trial type to be compared (e.g. incongruent trials)
#' @param average use "mean" or "median" to calculate average scores?
#' @param plot logical value giving the option to visualise the estimates in a raincloud plot. defaults to FALSE
#' @param round.to sets the number of decimals to round the estimates to defaults to 2
#' @param check runs several checks of the data to detect participants/conditions/trialtypes with too few trials to run splithalf
#' @return Returns a data frame containing permutation based split-half reliability estimates
#' @return splithalf is the raw estimate of the bias index
#' @return spearmanbrown is the spearman-brown corrected estimate of the bias index
#' @return Warning: If there are missing data (e.g one condition data missing for one participant) output will include details of the missing data and return a dataframe containing the NA data. Warnings will be displayed in the console.
#' @examples
#' \dontrun{
#' ## see online documentation for full examples
#' https://github.com/sdparsons/splithalf
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
#' ## example run of splithalf on a difference score
#' splithalf(data = sim_data,
#'           outcome = "RT",
#'           score = "difference",
#'           conditionlist = c("A", "B"),
#'           halftype = "random",
#'           permutations = 5000,
#'           var.RT = "RT",
#'           var.condition = "block_name",
#'           var.participant = "participant_number",
#'           var.compare = "trial_type",
#'           compare1 = "congruent",
#'           compare2 = "incongruent",
#'           average = "mean",
#'           plot = TRUE)
#'
#' ## example run of splithalf on an average score
#' splithalf(data = sim_data,
#'           outcome = "RT",
#'           score = "average",
#'           conditionlist = c("A", "B"),
#'           halftype = "random",
#'           permutations = 5000,
#'           var.RT = "RT",
#'           var.condition = "block_name",
#'           var.participant = "participant_number",
#'           average = "mean")
#'
#' ## example run of splithalf on a difference of differences score
#' splithalf(data = sim_data,
#'           outcome = "RT",
#'           score = "difference_of_difference",
#'           conditionlist = c("A", "B"),
#'           halftype = "random",
#'           permutations = 5000,
#'           var.RT = "RT",
#'           var.condition = "block_name",
#'           var.participant = "participant_number",
#'           var.compare = "trial_type",
#'           compare1 = "congruent",
#'           compare2 = "incongruent",
#'           average = "mean")
#'
#' }
#' @import tidyr
#' @import Rcpp
#' @import ggplot2
#' @import grid
#' @importFrom stats complete.cases cor median na.omit quantile sd
#' @importFrom robustbase colMedians
#' @importFrom dplyr select summarise group_by mutate n_distinct count
#' @importFrom tidyr complete
#' @importFrom plyr arrange
#' @useDynLib splithalf, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export

splithalf <- function(data,
                      outcome = "RT",
                      score = "difference",
                      conditionlist = FALSE,
                      halftype = "random",
                      permutations = 5000,
                      var.RT = "latency",
                      var.ACC = "accuracy",
                      var.condition = FALSE,
                      var.participant = "subject",
                      var.compare = "congruency",
                      compare1 = "Congruent",
                      compare2 = "Incongruent",
                      average = "mean",
                      plot = FALSE,
                      round.to = 2,
                      check = TRUE)

{
  # check that the dataframe is a data frame
  if (is.data.frame(data) == FALSE) {
    stop("a data frame has not been specified in data = ")
  }

  # check for missing variables
  if (halftype != "oddeven" &
      halftype != "halfs" & halftype != "random") {
    stop("the halftype has not been specified")
  }
  if (outcome != "RT" & outcome != "accuracy") {
    stop("the outcome has not been specified: select from RT or accuracy")
  }
  if (score != "average" &
      score != "difference" &
      score != "difference_of_difference" &
      score != "DPrime") {
    stop(
      "the score has not been specified: select from average, difference, difference_of_difference, or DPrime"
    )
  }
  if(score == "DPrime") {
    if(halftype != "random") {
      stop("The d prime score is only implemented with random halftypes, please use halftype = 'random'")
    }
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
    if (average != "mean" & average != "median" & average != "sum") {
      stop("averaging method not selected")
    }
  }

  # specifically checking the condition variables, alterning as necessary to run for all trials as one condition
  if (var.condition == FALSE) {
    warning("no condition variable specified, splithalf will treat all trials as one condition")
    data$all <- "all"
    var.condition <- "all"
    conditionlist <- "all"
  }  else if (var.condition %in% colnames(data) == FALSE)  {
    stop("condition variable cannot be found in dataframe")
  } else if (!exists("conditionlist")) {
    warning("condition list not specified, treating task as single condition")
    data$all <- "all"
    var.condition <- "all"
    conditionlist <- "all"
  } else if (exists("conditionlist")) {
    if (all(conditionlist %in% unique(data[[var.condition]])) == FALSE) {
      stop("one or more of the conditions do not exist in the condition variable")
    }
  }

  # save information about the call to splithalf
  call <- list(
    outcome = outcome,
    score = score,
    conditionlist = conditionlist,
    halftype = halftype,
    permutations = permutations,
    var.RT = var.RT,
    var.ACC = var.ACC,
    var.condition = var.condition,
    var.participant = var.participant,
    var.compare = var.compare,
    compare1 = compare1,
    compare2 = compare2,
    average = average
  )

  # create list to return at end of function
  output <- list(call = call,
                 data = data)

  # create empty objects for the purposes of binding global variables
  #(and to pass CRAN checks)
  RT <- 0
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
  bias <- 0
  value <- 0
  . <- 0
  difference1_1 <- 0
  difference1_2 <- 0
  difference2_1 <- 0
  difference2_2 <- 0
  cor1 <- 0
  cor2 <- 0
  group <- 0
  violinwidth  <- 0
  width <- 0
  x <- 0
  xmax <- 0
  xmaxv <- 0
  xminv <- 0
  y <- 0
  n <- 0



  # set the data as a data.frame to avoid tibble issues
  data <- as.data.frame(data)

  # renames the dataset variables to fit with the code
  if (outcome == "RT") {
    data$RT <- data[, var.RT]
  }
  data$condition <- data[, var.condition]
  data$participant <- data[, var.participant]
  data$trialnum <- 1:nrow(data)
  if (score == "difference" | score == "difference_of_difference") {
    data$compare <- data[, var.compare]
  }
  if (score == "DPrime") {
    data$compare <- data[, var.compare]
  }
  if (outcome == "accuracy") {
    data$accuracy <- data[, var.ACC]
  }


  # check data for cases that will likely result in errors

if(check == TRUE) {

  if(score == "average") {
    check1 <- data %>%
      dplyr::group_by(participant, condition) %>%
      dplyr::count() %>%
      dplyr::ungroup() %>%
      tidyr::complete(participant, condition)
  }

  if(score != "average") {
    check1 <- data %>%
      dplyr::group_by(participant, condition, compare) %>%
      dplyr::count() %>%
      dplyr::ungroup() %>%
      tidyr::complete(participant, condition, compare)
  }



  # check for missing conditions and comparisons

  check_missing <- check1 %>%
    dplyr::filter(is.na(n))


  # check for cases where there are not enough trials

  check_lown <- check1 %>%
    dplyr::filter(n < 4)

  ## to be included: check for whether there is sufficient variance
  # check_variance <- data %>%
  #   dplyr::group_by(condition, compare) %>%
  #   dplyr::summarise(var = var(RT))


  if(nrow(check_missing) > 0) {
    print(check_missing)
    stop("splithalf unlikely to run: at least one participant is missing data from at least one condition or trial type - see above tibble for missing data")
  }


  if(nrow(check_lown) > 0) {
    print(check_lown)
    stop("splithalf unlikely to run: at least one participant has too few trials in at least one condition or trial type - see above tibble for participant in question")
  }


}


  # for randdom samples, the number of samples drawn
  iterations <- 1:permutations

  # loads data into dataset
  dataset <- data

  # how many participants?
  n_par <- dplyr::n_distinct(dataset$participant)

  # creates a list of participants
  plist <- sort(unique(dataset$participant))



  # checks (first if outcome is RT) whether user difference score is based on means or medians
  if (outcome == "RT") {
    if (average == "mean") {
      ave_fun <- function(val) {
        colMeans(val)
      }
      ave_fun_basic <- function(val) {
        mean(val)
      }
    } else if (average == "median") {
      ave_fun <- function(val) {
        robustbase::colMedians(val)
      }
      ave_fun_basic <- function(val) {
        median(val)
      }
    }
  }
  if (outcome == "accuracy") {
    if (average == "mean") {
      ave_fun <- function(val) {
        colMeans(val)
      }
      ave_fun_basic <- function(val) {
        mean(val)
      }
    } else if (average == "median") {
      ave_fun <- function(val) {
        robustbase::colMedians(val)
      }
      ave_fun_basic <- function(val) {
        median(val)
      }
    } else if (average == "sum") {
      ave_fun <- function(val) {
        colSums(val)
      }
      ave_fun_basic <- function(val) {
        sum(val)
      }
    }
  }
  if (score == "DPrime") {
    colSdApply <- function(val, ...)
      apply(X = val,
            MARGIN = 2,
            FUN = sd,
            ...)
    colnApply <-
      function(val, ...)
        apply(X = val,
              MARGIN = 2,
              FUN = length,
              ...)
  }



  ## Main splithalf processing

  if (halftype == "oddeven" | halftype == "halfs")
  {
    finaldata <-
      data.frame(
        i = rep(plist, times = length(conditionlist)),
        j = rep(conditionlist, each = length(plist)),
        half1 = NA,
        half2 = NA
      )
    l <- 1

    if (halftype == "oddeven")
    {
      # this loop creates a dataframe split by odd and even trial numbers
      # giving mean RTs in congruent and incongruent conditions for each
      # split

      if (score == "average") {
        for (j in conditionlist)
        {
          for (i in plist)
          {
            temp <- subset(dataset, participant == i & condition == j)

            half1 <-
              ave_fun_basic(subset(temp[, outcome], temp$trialnum %% 2 == 0))
            half2 <-
              ave_fun_basic(subset(temp[, outcome], temp$trialnum %% 2 == 1))

            finaldata[l, 3:4] <- c(half1, half2)

            l <- l + 1
          }
          print(paste("condition", j, "complete"))
        }


      }

      if (score == "difference" |
          score == "difference_of_difference") {
        for (j in conditionlist)
        {
          for (i in plist)
          {
            temp <- subset(dataset, participant == i & condition == j)

            half1.congruent   <-
              ave_fun_basic(subset(
                temp[, outcome],
                temp$compare ==
                  compare1 &
                  temp$trialnum %% 2 == 0
              ))
            half1.incongruent <-
              ave_fun_basic(subset(
                temp[, outcome],
                temp$compare ==
                  compare2 &
                  temp$trialnum %% 2 == 0
              ))
            half2.congruent   <-
              ave_fun_basic(subset(
                temp[, outcome],
                temp$compare ==
                  compare1 &
                  temp$trialnum %% 2 == 1
              ))
            half2.incongruent <-
              ave_fun_basic(subset(
                temp[, outcome],
                temp$compare ==
                  compare2 &
                  temp$trialnum %% 2 == 1
              ))

            finaldata[l, 3] <- half1.congruent - half1.incongruent
            finaldata[l, 4] <- half2.congruent - half2.incongruent

            l <- l + 1
          }
          print(paste("condition", j, "complete"))
        }
      }

    } else {
      if (halftype == "halfs") {
        if (score == "average") {
          for (j in conditionlist)
          {
            for (i in plist)
            {
              temp <- subset(dataset, participant == i & condition == j)

              triallist <- as.list(temp$trialnum)

              midtrial <- sum(!is.na(triallist)) / 2
              totaltrial <- sum(!is.na(triallist))

              half1 <- temp[1:midtrial,]
              half2 <- temp[(midtrial + 1):totaltrial,]

              half1  <-
                ave_fun_basic(subset(
                  half1[, outcome],
                  half1$participant == i &
                    half1$condition == j
                ))
              half2  <-
                ave_fun_basic(subset(
                  half2[, outcome],
                  half2$participant == i &
                    half2$condition == j
                ))


              finaldata[l, 3:4] <- c(half1, half2)

              l <- l + 1
            }
            print(paste("condition", j, "complete"))
          }
        }

        if (score == "difference" |
            score == "difference_of_difference")   {
          # this loop takes the first and last half of trials within each
          # condition~participant
          for (j in conditionlist)
          {
            for (i in plist)
            {
              temp <- subset(dataset, participant == i & condition == j)

              triallist <- as.list(temp$trialnum)

              midtrial <- sum(!is.na(triallist)) / 2
              totaltrial <- sum(!is.na(triallist))

              half1 <- temp[1:midtrial,]
              half2 <- temp[(midtrial + 1):totaltrial,]

              half1.congruent  <-
                ave_fun_basic(
                  subset(
                    half1[, outcome],
                    half1$participant == i &
                      half1$condition == j &
                      half1$compare == compare1
                  )
                )
              half1.incongruent <-
                ave_fun_basic(
                  subset(
                    half1[, outcome],
                    half1$participant == i &
                      half1$condition == j &
                      half1$compare == compare2
                  )
                )
              half2.congruent  <-
                ave_fun_basic(
                  subset(
                    half2[, outcome],
                    half2$participant == i &
                      half2$condition == j &
                      half2$compare == compare1
                  )
                )
              half2.incongruent <-
                ave_fun_basic(
                  subset(
                    half2[, outcome],
                    half2$participant == i &
                      half2$condition == j
                    &
                      half2$compare == compare2
                  )
                )

              finaldata[l, 3] <- half1.congruent - half1.incongruent
              finaldata[l, 4] <- half2.congruent - half2.incongruent

              l <- l + 1
            }
            print(paste("condition", j, "complete"))
          }
        }

      }
    }

    names(finaldata)[1] <- "participant"
    names(finaldata)[2] <- "condition"

    if (sum(is.na(finaldata$half1)) +
        sum(is.na(finaldata$half2)) > 0)
    {
      print("the following are participants/conditions with missing data")
      omitted <- finaldata[!complete.cases(finaldata),]
      print(unique(omitted[c("condition", "participant")]))
      print(
        "note: these particpants will be removed from the split half
            reliability calculations, in that condition"
      )
      warning(
        "Bias indices missing:
              at least one participant has missing data from at one condition
              These cases are removed from calculating reliability estimates
              $omitted contains the missing cases"
      )
    }

    # remove NA rows
    finaldata2 <- na.omit(finaldata)


    # create calculate estimates

    if (score == "average" | score == "difference") {
      out <- finaldata2 %>%
        dplyr::group_by(condition) %>%
        dplyr::summarise(
          n = round(sum(!is.na(half1)), 0),
          splithalf = round(cor(half1, half2,
                          use = "pairwise.complete"),round.to),
          spearmanbrown = round((2 * cor(half1, half2,
                                   use = "pairwise.complete")) /
            (1 + (2 - 1) * abs(
              cor(half1, half2,
                  use = "pairwise.complete")
            )), round.to)
        ) %>%
        as.data.frame()
    }

    if (score == "difference_of_difference") {
      out <- finaldata2 %>%
        tidyr::gather(key = "bias", value = "value", 3:4) %>%
        tidyr::unite(compare, condition, bias, sep = "_") %>%
        tidyr::spread(compare, value) %>%
        dplyr::mutate(
          difference1_1 = .[[2]] - .[[4]],
          difference1_2 = .[[3]] - .[[5]],
          difference2_1 = .[[2]] - .[[5]],
          difference2_2 = .[[3]] - .[[4]]
        ) %>%
        dplyr::summarise(
          cor1 = cor(difference1_1, difference1_2, use = "pairwise.complete"),
          cor2 = cor(difference2_1, difference2_2, use = "pairwise.complete"),
          n = dplyr::n()
        ) %>%
        dplyr::mutate(splithalf = round((cor1 + cor2) / 2, round.to)) %>%
        dplyr::mutate(spearmanbrown = round((2 * splithalf) / ((1 + (2 - 1) * abs(splithalf))), round.to)) %>%
        dplyr::select(-1, -2)

    }



    if (halftype == "oddeven")
      print("Splithalf estimates, splitting by odd and even trials")
    if (halftype == "halfs")
      print("Splithalf estimates, splitting by the first and second half of trials")


    if (sum(is.na(finaldata$half1)) +
        sum(is.na(finaldata$half2)) > 0)
    {
      return(list(Estimates = out, omitted = omitted))
    } else {
      return(out)
    }

  }

  if (halftype == "random")
  {
    # create the data.frame to populate
    findata <-
      data.frame(
        j = rep(conditionlist, each = (length(plist) *
                                         length(iterations))),
        i = rep(plist, each = length(iterations)),
        h = rep(iterations, times = (
          length(conditionlist) *
            length(plist)
        )),
        bias1 = NA,
        bias2 = NA
      )
    # loop counter
    l <- 1

    # participant loop counter for progress bar
    ppt <- 1

    # create vectors to contain both halfs to be compared
    bias1v <-
      vector(length = (length(conditionlist) * length(plist) *
                         length(iterations)))
    bias2v <-
      vector(length = (length(conditionlist) * length(plist) *
                         length(iterations)))


    if (score == "average") {
      for (j in conditionlist)
      {
        # set up progress bar
        pb <- txtProgressBar(min = 0,
                             max = n_par,
                             style = 3)
        setTxtProgressBar(pb, 0)

        for (i in plist)
        {
          # subset the dataframe into RT vectors by participant, condition
          temp   <-
            subset(dataset[, outcome],
                   dataset$participant == i &
                     dataset$condition == j)

          # calculates what will be the middle numbered trial
          midtrial <- sum(!is.na(temp)) / 2

          # n.b. this calls a Rcpp function that is similar to replicate(permutations, sample(temp)), but is 20x quicker
          # n.g. con used for consistency with the difference score code
          con2 <-
            Speedloop(
              A = matrix(nrow = length(temp), ncol = permutations, 0),
              x = permutations,
              y = temp
            )
          con2.1 <- con2[1:floor(midtrial), ]
          con2.2 <- con2[(floor(midtrial) + 1):length(temp), ]


          # note - named bias for consistency with difference score code
          bias1v[l:(l + permutations - 1)] <- ave_fun(con2.1)
          bias2v[l:(l + permutations - 1)] <- ave_fun(con2.2)


          l <- l + permutations
          ppt <- ppt + 1
          setTxtProgressBar(pb, ppt)

        }
        ppt <- 1 # reset the progress bar

        print(paste("condition", j, "complete"))
      }
    }

    if (score == "difference") {
      for (j in conditionlist)
      {
        # set up progress bar
        pb <- txtProgressBar(min = 0,
                             max = n_par,
                             style = 3)
        setTxtProgressBar(pb, 0)

        for (i in plist)
        {
          # subset the dataframe into RT vectors by participant, condition, and
          # congruency
          temp.con   <-
            subset(
              dataset[, outcome],
              dataset$participant == i &
                dataset$condition == j &
                dataset$compare == compare1
            )
          temp.incon <-
            subset(
              dataset[, outcome],
              dataset$participant ==
                i & dataset$condition == j &
                dataset$compare == compare2
            )

          # calculates what will be the middle numbered trial in each congruent
          # and incongruent list
          midtrial.con <- sum(!is.na(temp.con)) / 2
          midtrial.incon <- sum(!is.na(temp.incon)) / 2

          # n.b. this calls a Rcpp function that is similar to replicate(permutations, sample(temp.con)), but is 20x quicker
          con2 <-
            Speedloop(
              A = matrix(nrow = length(temp.con), ncol = permutations, 0),
              x = permutations,
              y = temp.con
            )
          con2.1 <- con2[1:floor(midtrial.con), ]
          con2.2 <- con2[(floor(midtrial.con) + 1):length(temp.con), ]

          incon2 <-
            Speedloop(
              A = matrix(nrow = length(temp.incon), ncol = permutations, 0),
              x = permutations,
              y = temp.incon
            )
          incon2.1 <- incon2[1:floor(midtrial.incon), ]
          incon2.2 <-
            incon2[(floor(midtrial.incon) + 1):length(temp.incon), ]

          bias1v[l:(l + permutations - 1)] <-
            ave_fun(con2.1)  - ave_fun(incon2.1)
          bias2v[l:(l + permutations - 1)] <-
            ave_fun(con2.2)  - ave_fun(incon2.2)


          l <- l + permutations
          ppt <- ppt + 1
          setTxtProgressBar(pb, ppt)

        }
        ppt <- 1 # reset the progress bar

        print(paste("condition", j, "complete"))
      }
    }

    if (score == "difference_of_difference") {
      for (j in conditionlist)
      {
        # set up progress bar
        pb <- txtProgressBar(min = 0,
                             max = n_par,
                             style = 3)
        setTxtProgressBar(pb, 0)

        for (i in plist)
        {
          # subset the dataframe into RT vectors by participant, condition, and
          # congruency
          temp.con   <-
            subset(
              dataset[, outcome],
              dataset$participant == i &
                dataset$condition == j &
                dataset$compare == compare1
            )
          temp.incon <-
            subset(
              dataset[, outcome],
              dataset$participant ==
                i & dataset$condition == j &
                dataset$compare == compare2
            )

          # calculates what will be the middle numbered trial in each congruent
          # and incongruent list
          midtrial.con <- sum(!is.na(temp.con)) / 2
          midtrial.incon <- sum(!is.na(temp.incon)) / 2

          # n.b. this calls a Rcpp function that is similar to replicate(permutations, sample(temp.con)), but is 20x quicker
          con2 <-
            Speedloop(
              A = matrix(nrow = length(temp.con), ncol = permutations, 0),
              x = permutations,
              y = temp.con
            )
          con2.1 <- con2[1:floor(midtrial.con), ]
          con2.2 <- con2[(floor(midtrial.con) + 1):length(temp.con), ]

          incon2 <-
            Speedloop(
              A = matrix(nrow = length(temp.incon), ncol = permutations, 0),
              x = permutations,
              y = temp.incon
            )
          incon2.1 <- incon2[1:floor(midtrial.incon), ]
          incon2.2 <-
            incon2[(floor(midtrial.incon) + 1):length(temp.incon), ]

          bias1v[l:(l + permutations - 1)] <-
            ave_fun(con2.1)  - ave_fun(incon2.1)
          bias2v[l:(l + permutations - 1)] <-
            ave_fun(con2.2)  - ave_fun(incon2.2)


          l <- l + permutations
          ppt <- ppt + 1
          setTxtProgressBar(pb, ppt)

        }
        ppt <- 1 # reset the progress bar

        print(paste("condition", j, "complete"))
      }
    }

    if (score == "DPrime") {
      for (j in conditionlist)
      {
        # set up progress bar
        pb <- txtProgressBar(min = 0,
                             max = n_par,
                             style = 3)
        setTxtProgressBar(pb, 0)

        for (i in plist)
        {
          # subset the dataframe into RT vectors by participant, condition, and
          # congruency
          temp.con   <-
            subset(
              dataset[, outcome],
              dataset$participant == i &
                dataset$condition == j &
                dataset$compare == compare1
            )
          temp.incon <-
            subset(
              dataset[, outcome],
              dataset$participant ==
                i & dataset$condition == j &
                dataset$compare == compare2
            )

          # calculates what will be the middle numbered trial in each congruent
          # and incongruent list
          midtrial.con <- sum(!is.na(temp.con)) / 2
          midtrial.incon <- sum(!is.na(temp.incon)) / 2

          # n.b. this calls a Rcpp function that is similar to replicate(permutations, sample(temp.con)), but is 20x quicker
          con2 <-
            Speedloop(
              A = matrix(nrow = length(temp.con), ncol = permutations, 0),
              x = permutations,
              y = temp.con
            )
          con2.1 <- con2[1:floor(midtrial.con), ]
          con2.2 <- con2[(floor(midtrial.con) + 1):length(temp.con), ]

          incon2 <-
            Speedloop(
              A = matrix(nrow = length(temp.incon), ncol = permutations, 0),
              x = permutations,
              y = temp.incon
            )
          incon2.1 <- incon2[1:floor(midtrial.incon), ]
          incon2.2 <-
            incon2[(floor(midtrial.incon) + 1):length(temp.incon), ]

          M1.1 = colMeans(con2.1)
          M2.1 = colMeans(incon2.1)
          SD1.1 = colSdApply(con2.1)
          SD2.1 = colSdApply(incon2.1)
          N1.1 = colnApply(con2.1)
          N2.1 = colnApply(incon2.1)

          M1.2 = colMeans(con2.2)
          M2.2 = colMeans(incon2.2)
          SD1.2 = colSdApply(con2.2)
          SD2.2 = colSdApply(incon2.2)
          N1.2 = colnApply(con2.2)
          N2.2 = colnApply(incon2.2)

          bias1v[l:(l + permutations - 1)] <- (M2.1 - M1.1) /
            sqrt((((N1.1 - 1) * SD1.1 ^ 2) + ((N2.1 - 1) * SD2.1 ^ 2) + ((
              N1.1 + N2.1
            ) * ((M1.1 - M2.1) ^ 2
            ) / 4)) / (N1.1 + N2.1 - 1))

          bias2v[l:(l + permutations - 1)] <- (M2.2 - M1.2) /
            sqrt((((N1.2 - 1) * SD1.2 ^ 2) + ((N2.2 - 1) * SD2.2 ^ 2) + ((
              N1.2 + N2.2
            ) * ((M1.2 - M2.2) ^ 2
            ) / 4)) / (N1.2 + N2.2 - 1))


          l <- l + permutations
          ppt <- ppt + 1
          setTxtProgressBar(pb, ppt)

        }
        ppt <- 1 # reset the progress bar

        print(paste("condition", j, "complete"))
      }
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
      omitted <- findata[!complete.cases(findata),]
      output$omitted <- omitted
      print(unique(omitted[c("condition", "participant")]))
      print(
        "note: these iterations will be removed from the split half
            reliability calculations, in that condition"
      )
      warning(
        "Bias indices missing:
              at least one participant has missing data from at one condition
              These cases are removed from calculating reliability estimates
              $omitted contains the missing cases"
      )
    }

    # remove NA rows
    findata2 <-  na.omit(findata)
    findata2$iteration <- as.factor(findata2$iteration)

    # calculate correlations per condition and iteration

    # splithalf <- findata2 %>%
    #   dplyr::group_by(iteration, condition) %>%
    #   dplyr::summarise(n = round(sum(!is.na(bias1)),2),
    #                    splithalf = cor(bias1, bias2, use = "pairwise.complete"),
    #                    spearmanbrown = (2 * cor(bias1, bias2,
    #                                             use = "pairwise.complete")) /
    #                      (1 + (2 - 1) * cor(bias1, bias2,
    #                                         use = "pairwise.complete")))

    if (score == "average" |
        score == "difference" | score == "DPrime")   {
      out <- findata2 %>%
        dplyr::group_by(iteration, condition) %>%
        dplyr::summarise(
          n = round(sum(!is.na(bias1)), 2),
          splithalf = cor(bias1, bias2, use = "pairwise.complete"),
          spearmanbrown = (2 * cor(bias1, bias2,
                                   use = "pairwise.complete")) /
            (1 + (2 - 1) * abs(
              cor(bias1, bias2,
                  use = "pairwise.complete")
            ))
        )


      # possibility for some visualisations here
      # plot(ggplot(findata2, aes(x = bias1, y = bias2)) +
      #  geom_point())

      # take the mean estimates per condition

      out2 <- out %>%
        dplyr::group_by(condition) %>%
        dplyr::summarise(
          n = mean(n),
          splithalf_estimate = round(mean(splithalf), round.to),
          splithalf95CI_lower = round(quantile(splithalf, c(.025), names = F), round.to),
          splithalf95CI_upper = round(quantile(splithalf, c(.975), names = F), round.to),
          spearmanbrown_estimate = round(mean(spearmanbrown), round.to),
          spearmanbrown95CI_lower = round(quantile(spearmanbrown, c(.025), names = F), round.to),
          spearmanbrown95CI_upper = round(quantile(spearmanbrown, c(.975), names = F), round.to)
        ) %>%
        as.data.frame()
    }


    if (score == "difference_of_difference") {
      out <- findata2 %>%
        tidyr::gather(key = "bias", value = "value", 4:5) %>%
        tidyr::unite(compare, condition, bias, sep = "_") %>%
        tidyr::spread(compare, value) %>%
        dplyr::mutate(
          difference1_1 = .[[5]] - .[[3]],
          difference1_2 = .[[6]] - .[[4]],
          difference2_1 = .[[5]] - .[[4]],
          difference2_2 = .[[6]] - .[[3]]
        ) %>%
        dplyr::group_by(iteration) %>%
        dplyr::summarise(
          cor1 = cor(difference1_1, difference1_2, use = "pairwise.complete"),
          cor2 = cor(difference2_1, difference2_2, use = "pairwise.complete"),
          n = dplyr::n()
        ) %>%
        tidyr::gather("var", "splithalf", 2:3) %>%
        dplyr::mutate(spearmanbrown = (2 * splithalf) / ((1 + (2 - 1) * abs(splithalf)))) %>%
        dplyr::mutate(condition = "difference_of_difference score")


      # take the mean estimates per condition

      out2 <- out %>%
        dplyr::summarise(
          n = mean(n),
          splithalf_estimate = round(mean(splithalf), round.to),
          splithalf95CI_lower = round(quantile(splithalf, c(.025), names = F), round.to),
          splithalf95CI_upper = round(quantile(splithalf, c(.975), names = F), round.to),
          spearmanbrown_estimate = round(mean(spearmanbrown), round.to),
          spearmanbrown95CI_lower = round(quantile(spearmanbrown, c(.025), names = F), round.to),
          spearmanbrown95CI_upper = round(quantile(spearmanbrown, c(.975), names = F), round.to)
        ) %>%
        as.data.frame()

      out2 <- cbind(condition = "difference_of_difference score", out2)

    }


    colnames(out2) <- c(
      "condition",
      "n",
      "splithalf",
      "95_low",
      "95_high",
      "spearmanbrown",
      "SB_low",
      "SB_high"
    )


    print(paste(
      "split half estimates for",
      permutations,
      "random splits",
      sep = " "
    ))

    output$estimates <- out
    output$final_estimates <- out2

    #
    # if (sum(is.na(findata$bias1)) +
    #     sum(is.na(findata$bias2)) > 0)
    # {
    #   print(list(estimates = out2, omitted = omitted))
    # } else {
    #   print(out2)
    # }

    print(out2[,c( "condition",
                   "n",
                   "spearmanbrown",
                   "SB_low",
                   "SB_high")])

    print(paste("this could be reported as: ",
                "using ", output$call$permutations, " random splits, the spearman-brown corrected reliability  estimate for the ",
                out2[1,"condition"],
                " condition was ", out2[1,"spearmanbrown"],
                ", 95% CI [", out2[1,"SB_low"], ", ",
                out2[1,"SB_high"], "]", sep = ""))


    if(plot == TRUE) {

      "%||%" <- function(a, b) {
        if (!is.null(a)) a else b
      }

      ggname <- function(prefix, grob) {
        grob$name <- grobName(grob, prefix)
        grob
      }

      geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                                   position = "dodge", trim = TRUE, scale = "area",
                                   show.legend = NA, inherit.aes = TRUE, ...) {
        layer(
          data = data,
          mapping = mapping,
          stat = stat,
          geom = GeomFlatViolin,
          position = position,
          show.legend = show.legend,
          inherit.aes = inherit.aes,
          params = list(
            trim = trim,
            scale = scale,
            ...
          )
        )
      }

      GeomFlatViolin <-
        ggproto("GeomFlatViolin", Geom,
                setup_data = function(data, params) {
                  data$width <- data$width %||%
                    params$width %||% (resolution(data$x, FALSE) * 0.9)

                  # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
                  data %>%
                    dplyr::group_by(group) %>%
                    dplyr::mutate(ymin = min(y),
                           ymax = max(y),
                           xmin = x,
                           xmax = x + width / 2)

                },

                draw_group = function(data, panel_scales, coord) {
                  # Find the points for the line to go all the way around
                  data <- transform(data, xminv = x,
                                    xmaxv = x + violinwidth * (xmax - x))

                  # Make sure it's sorted properly to draw the outline
                  newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                                   plyr::arrange(transform(data, x = xmaxv), -y))

                  # Close the polygon: set first and last point the same
                  # Needed for coord_polar and such
                  newdata <- rbind(newdata, newdata[1,])

                  ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
                },

                draw_key = draw_key_polygon,

                default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                                  alpha = NA, linetype = "solid"),

                required_aes = c("x", "y")
        )

      custombox1 <- function(y){
        d <- data.frame(ymin=min(y),
                        lower=quantile(y,0.025),
                        middle=mean(y),
                        upper=quantile(y,0.975),
                        ymax=max(y),
                        y=y,
                        width=0.07,
                        row.names = NULL
        )
        d[1, ]
      }



      estimates_plot <- ggplot(data = output$estimates,
                               aes(x = condition, y = spearmanbrown, fill = condition)) +
        geom_flat_violin(position = position_nudge(x = .2)) +
        geom_jitter(width = .1, alpha = .3, size = .5) +
        stat_summary(fun.data = custombox1, geom = "boxplot",
                     position = position_nudge(x = .2),
                     alpha = .2, colour = "BLACK", size = .75) +
        theme(legend.position = "none") +
        labs(y = "estimate") +
        geom_text(data = output$final_estimates,
                  aes(label = spearmanbrown, group = condition),
                  stat = "identity", position = position_nudge(x = .35))

      print(estimates_plot)

      output$plot <- estimates_plot
    }

    class(output) <- c('splithalf')

    return(invisible(output))

  }
}
