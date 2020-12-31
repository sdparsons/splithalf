---
# Example from https://joss.readthedocs.io/en/latest/submitting.html
title: 'splithalf: robust estimates of split half reliability'
tags:
- reliability
- cognitive tasks
- multiverse
- split half methods
- R
authors:
  - name: Sam Parsons
    affiliation: 1 # (Multiple affiliations must be quoted)
affiliations:
 - name: Department of Experimental Psychology, University of Oxford
   index: 1

citation_author: Parsons
date: 30 December 2020
year: 2020
bibliography: inst/book.bib
---

# splithalf: robust estimates of split half reliability

The `R` package **splithalf** provides tools to estimate the internal consistency reliability of cognitive measures. In particular, the tools were developed for application to tasks that use difference scores as the main outcome measure, for instance the Stroop score or dot-probe attention bias index (average RT in incongruent trials minus average RT in congruent trials). 

The methods in **splithalf** are built around split half reliability estimation. To increase the robustness of these estimates, the package implements a permutation approach that takes a large number of random (without replacement) split halves of the data. For each permutation the correlation between halves is calculated, with the Spearman-Brown correction applied [@spearman_proof_1904]. This process generates a distribution of reliability estimates from which we can extract summary statistics (e.g. average and 95% HDI).

### Statement of need

While many cognitive tasks yield highly robust effects, the same task may not yield reliable individual differences [@hedge_reliability_2018]. As these measures are used in questions of individual differences researchers need to have some psychometric information for the outcome measures. Recently, it was proposed that psychological science should set a standard expectation for the reporting of reliability information for cognitive and behavioural measures [-@parsons_kruijt_fox_2019]. **splithalf** was developed to support this proposition by providing a tool to easily extract internal consistency reliability estimates from behavioural measures. 

## Usage

The _splithalf_ function estimates reliability for a variety of scoring types, including average scores, difference scores, and difference-of-difference scores. The function is also extendable to other scoring calculations, e.g. signal detection. Reliability can be estimated for response time and accuracy rate outcomes. A plotting option is also provided for users to examine the distribution of reliability estimates. 

Users can perform a reliability multiverse analysis to examine the stability of reliability estimates, given a rage of data processing decisions [@parsons_exploring_2020]. By passing a list of data processing decisions (e.g. maximum and minimum RT cutoffs) and a _splithalf_ object into the _splithalf.multiverse_ function. The output can then be plotted with _plot_multiverse_. 


**splithalf** requires the **tidyr** [@R-tidyr] and **dplyr** [@R-dplyr] packages for data handling within the functions. The **robustbase** package is used to extract median scores when applicable. The computationally heavy tasks (extracting many random half samples of the data) are written in `c++` via the `R` package **Rcpp** [@R-Rcpp]. Figures use the **ggplot** package [@R-ggplot2], raincloud plots use code adapted from Allen et al. [@allen_raincloud_2019], and the **patchwork** package [@R-patchwork] is used for plotting the multiverse analyses. 


## Comparison to other software

**splithalf** is the only package to implement all of these tools, in particular reliability multiverse analyses. Some other `R` packages offer a bootstrapped approach to split-half reliability **multicon** [@R-multicon], **psych** [@R-psych], and **splithalfr** [@R-splithalfr].


## Acknowledgements

This work was supported by a grant from the Economic and Social Research Council (ESRC; Grant reference ES/R004285/1). I would also like to thank Anne-Wil Kruijt, James Bartlett, Andrew Jones, and Jean-Paul Snijder for their input on the package. I also thank all that have asked questions, posed suggestions, and shared bug reports - the **splithalf** package is more stable and powerful because of them. 

## References
