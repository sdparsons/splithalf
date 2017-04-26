#' Generated dataset of Dot-probe data
#'
#' A dataset containing data necessary to run examples of each function
#'
#' The following code was used to generate the data
#'
#' DPdata <- data.frame(subject = rep(1:20, each = (96*2)),
#' blockcode = rep(c("block1","block2"), each = 96, length.out = 20*2*96),
#' trialnum = rep(1:96, length.out = 20*2*96),
#' congruency = rep(c("Congruent","Incongruent"), length.out = 20*2*96),
#' latency = rep(rnorm(100,25), length.out = 20*2*96),
#' correct = rep(1, length.out = 20*2*96))
#'
#' @format A dataframe with 3840 rows and 6 variables
#' \itemize{
#' \item subject: contains participant numbers for 20 subjects
#' \item blockcode: two block conditions "block1" and "block2"
#' \item trialnum: 96 trials per block
#' \item congruency: sets to congruent or incongruent trials
#' \item latency: RT measure (simulated data)
#' \item correct: accuracy (set to all accurate for the example)
#' }
"DPdata"

#' Generated dataset of Dot-probe data with missing data
#'
#' The data is adapted from the DPdata set using the following code
#'
#' DPdata_missing <- DPdata
#' DPdata_missing$correct <- ifelse(DPdata_missing$subject == 15 &
#'                                  DPdata_missing$blockcode == "block2",
#'                                  0,1)
#'
#' A dataset containing data necessary to run examples of each function
#' including missing data
#'
#' @format A dataframe with 3840 rows and 6 variables
#' \itemize{
#' \item subject: contains participant numbers for 20 subjects
#' \item blockcode: two block conditions "block1" and "block2"
#' \item trialnum: 96 trials per block
#' \item congruency: sets to congruent or incongruent trials
#' \item latency: RT measure (simulated data)
#' \item correct: accuracy (set to all accurate for the example)
#' }
"DPdata_missing"

#' Generated dataset of Task switching data
#'
#' The data is adapted from the DPdata set using the following code
#'
#' TSTdata <- DPdata
#' names(TSTdata)[names(TSTdata) == "congruency"] <- "trialtype"
#' TSTdata$trialtype <- ifelse(TSTdata$trialtype == "Congruent", "Repeat", "Switch")
#'
#' A dataset containing data necessary to run examples of each function
#'
#' @format A dataframe with 3840 rows and 6 variables
#' \itemize{
#' \item subject: contains participant numbers for 20 subjects
#' \item blockcode: two block conditions "block1" and "block2"
#' \item trialnum: 96 trials per block
#' \item trialtype: sets to repeat or switch trials
#' \item latency: RT measure (simulated data)
#' \item correct: accuracy (set to all accurate for the example)
#' }
"TSTdata"

#' Generated dataset of Task switching data with missing data
#'
#' The data is adapted from the DPdata_missing set using the following code
#'
#' TSTdata_missing <- DPdata_missing
#' names(TSTdata_missing)[names(TSTdata_missing) == "congruency"] <- "trialtype"
#' TSTdata_missing$trialtype <- ifelse(TSTdata_missing$trialtype == "Congruent", "Repeat", "Switch")
#'
#' A dataset containing data necessary to run examples of each function
#'
#' @format A dataframe with 3840 rows and 6 variables
#' \itemize{
#' \item subject: contains participant numbers for 20 subjects
#' \item blockcode: two block conditions "block1" and "block2"
#' \item trialnum: 96 trials per block
#' \item trialtype: sets to repeat or switch trials
#' \item latency: RT measure (simulated data)
#' \item correct: accuracy (set to all accurate for the example)
#' }
"TSTdata_missing"
