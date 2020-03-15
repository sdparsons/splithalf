#' Visualising reliability multiverses
#'
#' The (unofficial) function version name is "This function will make you a master in bird law"
#' @param multiverse multiverse object or list of multiverse objects
#' @param title add a title to the plot?
#' @param vline add a vertical line to the plot, e.g. use .5 for the median reliability estimate
#' @param heights relative heights of plot panels, defaults to c(4,5)
#' @param SE set to true to also plot the standard errors of the scores
#' @return Returns a visualistation of a multiverse object
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
#' @rdname multiverse.plot
#' @export


multiverse.plot <- function(multiverse,
                            title = "",
                            vline = "none",
                            heights = c(4,5),
                            SE = FALSE) {

  if(SE == TRUE & length(heights) != 3) {
    stop("heights must be length 3 is SE = TRUE")
  }

  if(class(multiverse) == "list") {
    x <- length(multiverse)

    for(i in 1:x){
      if(class(multiverse[[i]]) != "multiverse")
        stop("not all list objects are of class multiverse")
    }

    # create empty objects for the purposes of binding global variables
    #(and to pass CRAN checks)
    estimate <- 0
    low  <- 0
    high <- 0
    ns <- 0
    time <- 0
    Bigdecision <- 0
    Decision <- 0


    final <- NULL
    final2 <- NULL
    ord <- order(multiverse[[1]]$estimates[,"estimate"])


    for(x in 1:length(multiverse)) {
      final <- multiverse[[x]]$estimates
      final$time <- toString(x)
      final <- final[ord,]
      final$ns <- 1:nrow(final)

      final2 <- rbind(final2, final)

    }


    #final2 <- final
    # %>%
    #   group_by(time) %>%
    #   mutate(order = order(estimate))



  }


  if(class(multiverse) == "multiverse"){
    final2 <- multiverse$estimates
    final2 <- final2[order(final2[,"estimate"]),]
    final2$SE <- multiverse$SE
  }

  suppressWarnings({

    if(class(multiverse) == "multiverse"){
      final2$ns <- 1:multiverse$nS

      reliability_plot <- ggplot(data = final2,
                                 aes(x = 1:multiverse$nS, y = estimate)) +
        geom_ribbon(aes(ymin = low, ymax = high), fill = "grey80", alpha = .8) +
        {if(vline!="none")geom_vline(aes(), xintercept = round(multiverse$nS * vline), linetype = "dashed", colour = "black")} +
        geom_point() +
        scale_color_manual(values = c("#000000", "#FF0000")) +
        labs(x = " ") +
        theme(legend.position = "none",
              strip.text.x = element_blank(),
              strip.text.y = element_blank(),
              strip.background = element_blank(),
              text = element_text(size=10)) +
        geom_hline(yintercept = 0)

      if(SE == FALSE)
        reliability_plot <- reliability_plot +
        ggtitle(title) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    }

    if(class(multiverse) == "list"){
      reliability_plot <- ggplot(data = final2,
                                 aes(x = ns, y = estimate, fill = time)) +
        geom_ribbon(aes(ymin = low, ymax = high, fill = time), alpha = .1)  +
        {if(vline!="none")geom_vline(aes(), xintercept = round(multiverse[[1]]$nS * vline), linetype = "dashed", colour = "black")} +
        geom_point(aes(colour = time)) +
        # geom_line(aes(colour = time)) +
        labs(x = " ") +
        theme(legend.position = "top",
              legend.title = element_blank(),
              strip.text.x = element_blank(),
              strip.text.y = element_blank(),
              strip.background = element_blank(),
              text = element_text(size=10)) +
        geom_hline(yintercept = 0) +
        ggtitle(title) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))

    }

  })

  suppressWarnings({

    if(class(multiverse) == "multiverse"){

      final2$ns <- 1:multiverse$nS


      final3 <- final2 %>%
        gather(key = "Bigdecision",
               value = "Decision",
               -ns, -estimate, -low, -high)

    }

    if(class(multiverse) == "list"){
      final3 <- final2 %>%
        dplyr::filter(time == 1) %>%
        tidyr::gather(key = "Bigdecision",
               value = "Decision",
               -ns, -estimate, -low, -high)
    }

  })

  final3$Bigdecision <- factor(final3$Bigdecision, levels = c("ACC_cutoff",
                                                              "RT_min",
                                                              "RT_max",
                                                              "RT_sd_cutoff",
                                                              "split_by",
                                                              "averaging_method"))

  suppressWarnings({
    if(class(multiverse) == "multiverse"){
      dashboard <- ggplot(data = subset(final3, Bigdecision %in% multiverse$cols),
                          aes(x = ns, y = Decision, colour = Bigdecision)) +
        facet_grid(Bigdecision ~ ., scales = "free", space = "free", drop = ) +
        {if(vline!="none")geom_vline(aes(), xintercept = round(multiverse$nS*vline), linetype = "dashed", colour = "black")} +
        geom_point(aes(colour = Bigdecision), shape = 108, size = 6) +
        labs(x = "specification number") +
        theme_minimal() +
        theme(legend.position = "none",
              strip.text.x = element_blank(),
              strip.text.y = element_blank(),
              strip.background = element_blank(),
              text = element_text(size=10))
    }

    if(class(multiverse) == "list"){
      dashboard <- ggplot(data = subset(final3, Bigdecision %in% multiverse[[1]]$cols),
                          aes(x = ns, y = Decision, colour = Bigdecision)) +
        facet_grid(Bigdecision ~ ., scales = "free", space = "free", drop = ) +
        {if(vline!="none")geom_vline(aes(), xintercept = round(multiverse[[1]]$nS*vline), linetype = "dashed", colour = "black")} +
        geom_point(aes(colour = Bigdecision), shape = 108, size = 6) +
        labs(x = "specification number") +
        theme_minimal() +
        theme(legend.position = "none",
              strip.text.x = element_blank(),
              strip.text.y = element_blank(),
              strip.background = element_blank(),
              text = element_text(size=10))
    }
  })

  if(SE == TRUE) {
    SE_plot <- ggplot(data = final2,
                      aes(x = 1:multiverse$nS, y = SE)) +
      geom_point() +
      scale_color_manual(values = c("#000000", "#FF0000")) +
      labs(x = " ") +
      theme(legend.position = "none",
            strip.text.x = element_blank(),
            strip.text.y = element_blank(),
            strip.background = element_blank(),
            text = element_text(size=10)) +
      geom_hline(yintercept = 0) +
      ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  }


  if(SE == FALSE) {
    final_plot <- reliability_plot / dashboard + plot_layout(heights = heights)
  }
  if(SE == TRUE) {
    final_plot <- SE_plot / reliability_plot / dashboard + plot_layout(heights = heights)
  }

  return(final_plot)

}
