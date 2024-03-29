
<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->

# UPCOMING: splithalf v0.8.2 \[Unofficial version name: I eat stickers all the time, dude!\]

changes: \*added error checks for common issues - too few trials and missing conditions


# UPCOMING: splithalf v0.8.1 \[unofficial version name: Rum Ham\]

changes: \* added examples in the function help and improved function
documentation \* added validation vignette (tests many combinations of
options within splithalf functions, incluing the multiverse functions)
\* fixed minor bugs in some combinations of options within splithalf
(typically within the ) \* fixed some documentation \* fixed plotting
issue with score = “difference\_of\_difference” \* minor improvements to
documentation

# splithalf v0.7.2 \[unofficial version name: Kitten Mittens\]

changes: \* minor updates: reworked documentation and README \* added
hexlogo \* bug fixes to splithalf.multiverse

# splithalf v0.7.1 \[unofficial version name: Kitten Mittens\]

changes: \* added multiverse analyses to package. Functions
*splithalf.multiverse*, *testretest.multiverse*, *plot.multiverse*, and
*threshold* now available!

# splithalf v0.6.2 \[unofficial version name: the Golden God\]

changes: \* added “sum” option to the average input. average = “sum” can
now be used to calculate the sum score for accuracy (binary 0/1 values)

# splithalf v0.6.1 \[unofficial version name: the Golden God\]

changes: \* added DPrime option, this is still under development \* the
structure of splithalf has changed to include additional elements in the
output list - intended to make the future reliability multiverse add-on
easier to use \* added a basic plotting functionality, setting plot =
TRUE will now give a raincloud plot of all estimates (based on code from
Allen et al. <https://wellcomeopenresearch.org/articles/4-63/v1>) \*
added a ‘round.to’ option to specify the number of decimal points to
round the estimates to

# splithalf v0.5.3 \[unofficial version name: Fight Milk - rebrand\]

changes: \* updated code to allow for estimation of splithalf
reliability of accuracy rates. (beta testing needed) \* minor bug
checks, fixed using spearman over pearson correlation.

# splithalf v0.5.2 \[unofficial version name: Fight Milk - rebrand\]

changes: \* a guide to the package can be found here
<https://sdparsons.github.io/splithalf_documentation/> \* bug fix to the
spearman-brown correction, it now deals with negative splithalf
estimates properly. \* fixed error in splitting the sample in the halfs
type of split (Thanks to Anne-Wil Kruijt for pointing out) \* updated
documentation

# splithalf v0.5.1 \[unofficial version name: Fight Milk\]

Changes: \* all functions compressed into a single function “splithalf”
\* added utilities such as RT trimming have been removed, the function
requires you use data that has been processed \* added C++ code with
Rcpp to massively speed up the function.

# splithalf v0.4.1 \[unofficial version name: The nightman commeth\]

I forgot to record changes because I am awful - lots of improvements
mainly to speed things up

# splithalf v0.3.1

Changes: \* added dplyr to dependencies \* added splithalf functions for
accuracy (currently development version)

# splithalf v0.3.0

Changes:

-   reworked the package into three main functions
-   more options added for flexibility to many RT tasks
-   progress bars added

# splithalf v0.2.0 (current development version)

Changes:

-   Added the TSTsplithalf function
-   Some minor bug fixes
-   Output for all functions now includes two-part alpha

# splithalf v0.2.0 (Release data: 07/04/2017)

Changes:

-   Submitted to CRAN
