
<!-- README.md is generated from README.Rmd. Please edit that file -->

\[\[CURRENT README DOCUMENTATION UNDER DEVELOPMENT\]\]

<img src="inst/hexlogo/splithalf_logo.png" height="300"/>

# splithalf R package

this is version 0.7.1 \[unofficial version name: Kitten Mittens\]

Please see documentation at
<https://sdparsons.github.io/splithalf_documentation/>

\[UNDER DEVELOPMENT, BEYOND HERE BE MONSTERS\]

## Start-up notes

### Instalation

The **splithalf** package can be installed from CRAN or Github:

``` r
install.packages("splithalf")
## or the development version
## devtools::install_github("sdparsons/splithalf")
```

### Version note

**Now on github and submitted to CRAN: VERSION 0.7.1** Featuring
reliability multiverse analyses\!\!\!

The current version of **splithalf** is 0.7.1 \[unofficial version name:
“Kitten Mittens”\]

The most noticable difference to this version is the addition of
reliability multiverse functions. Now you can take your *splithalf*
object and run it via *splithalf.multiverse* to estimate reliability
across a user-defined list of data-processing specifications. Then,
because sometimes science is more art than science, you can plug the
multiverse output into *multiverse.plot* to generate some sweet
visualisations. *A tutorial will appear in an upcoming preprint and I
will also add a page to this site*

Additionally, the output of splithalf has been reworked. Now a list is
returned including the specific function calls, the processed data.
Since version 0.6.2 you can also set `plot = TRUE` in *splithalf* to
generate a raincloud plot of the distribution of reliability estimates.

### Citing the package

Citing packages is one way for developers to gain some recognition for
the time spent maintaining the work. I would like to keep track of how
the package is used so that I can solicit feedback and improve the
package more generally. This would also help me track the uptake of
reporting measurement reliability over time.

Please use the following reference for the code: Parsons, Sam (2020):
splithalf: robust estimates of split half reliability. figshare.
Software. <https://doi.org/10.6084/m9.figshare.11956746.v4>

If (eventually) this documentation turns into a publication, this
reference will change.

### User feedback

Developing the splithalf package is a labour of love (and occasionally
burning hatred). If you have any suggestions for improvement, additional
functionality, or anything else, please contact me
(sam.parsons@psy.ox.ac.uk) or raise an issue on github
(<https://github.com/sdparsons/splithalf>). Likewise, if you are having
trouble using the package (e.g. scream fits at your computer screen –
we’ve all been there) do contact me and I will do my best to help as
quickly as possible. These kind of help requests are super welcome. In
fact, the package has seen several increases in performance and
usability due to people asking for help.

### A note on terminology used in this document

It is important that we have a similar understanding of the terminology
I use in the package and documentation. Each is also discussed in
reference to the functions later in the documentation.

  - Trial – whatever happens in this task, e.g. a stimuli is presented.
    Importantly, participants give one response per trial
  - Trial type – often trials can be split into different trial types
    (e.g. to compare congruent and incongruent trials)
  - Condition - this might be different blocks of trials, or something
    to be assessed separately within the functions. e.g. a task might
    have a block of ‘positive’ trials and a block of ‘negative’ trials.
  - Datatype - I use this to refer to the outcome of interest.
    specifically whether one is interested in average response times or
    accuracy rates
  - Score - I use score to indicate how the final outcome is measured;
    e.g. the average RT, or the difference between two average RTs, or
    even the difference between two differences between two RTs (yes,
    the final one is confusing)

## Background

### Why should I estimate the reliabilty of my task measurement?

For my full argument, see our preprint (Parsons, Kruijt, and Fox 2019).
In short, the psychometric properties of our measurements are important
and should be taken into consideration when conducting our analyses and
interpreting our results. I think it should be standard practice to
estimate and report reliability of our tasks. To not do so renders us
incapable of knowing how much confidence we can place in results. If the
replication crisis was a big deal, the measurement crisis has the
potential to be as if not more catastrophic.

### Why did I develop this package in the first place?

Long story short-ish. I had some dot-probe attention bias data pre- and
post- an attention bias modification procedure. In an exploratory
analysis the post-training bias, but not the pre-training bias, was
associated with self-report measures at follow-up. I think on a whim, we
looked at how reliable the measures were pre/post and the post-training
measure was much more reliable (\~ .7) than the pre-training bias
measure (\~ .4). This led me down the road of wanting to understand
reliability; what it ‘means’, how it impacts our results, the
interesting reliability-power relationship, and so on. It also led me to
the realisation (shared with many others), that we should estimate and
report the reliability of our measures as standard practice. That has
become my quest (albeit, it’s not what I am actualy paid to do), taking
two main forms.

1.  I wrote a paper with my DPhil supervisors (Parsons, Kruijt, and Fox
    2019), that I hope will be out in AMPPS soon (after revisions); the
    message being that we should adopt reporting reliabilty as a
    standard practice.

2.  I developed the *splithalf* package with the aim to provide an easy
    to use tool to estimate internal consistency of bias measures
    (difference scores) drawn from cognitive tasks.

### How does *splithalf* work?

The permutation approach in *splithalf* is actually rather simple. Over
many repetitions (or permutations), the data is split in half and
outcome scores are calculated for each half. For each repetion, the
correlation coefficient between each half is calculated. Finally, the
average of these correlations is taken as the final estimate of
reliability. 95% percentiles are also taken from the distribution of
estimates to give a picture of the spread of reliability estimates.\[1\]

## Preprocessing

Splithalf requires that the input dataset has already undergone
preprocessing (e.g. removal of error trials, RT trimming, and
participants with high error rates). Splithalf should therefore be used
with the same data that will be used to calculate summary scores and
outcome indices.

In my earlier attempts to make splithalf as useful as possible I added a
number of user-inputted variables that helped remove participants and
trim RTs. This also resulted in far to many input variables and
potential confusion.

You might need to do a little pre-processing if you have not saved your
processing steps. Here is a code snippet that will work (each line
includes a note about what the code is doing after the hash).

You will need to change the numbers to match your data. This code could
be briefer, however has been structured like this for ease of use.

Note == indicates ‘is equal to’, :: indicates that the function uses the
package indicated, in the first case the **dplyr** package (Wickham et
al. 2018).

``` r
dataset %>%    
filter(accuracy == 1) %>%       ## keeps only trials in which participants made an accurate response
filter(RT >= 100, RT <= 2000)  %>%   ## removes RTs less than 100ms and greater than 2000ms
filter(participant != c(“p23”, “p45”)    ## removes participants “p23” and “p45”
```

If following rt trims you also trimmed by SD, use the following as well.
Note that this is for two standard deviations from the mean, within each
participant, and within each condition and trialtype.

``` r
dataset %>%    
    dplyr::group_by(participant, condition, compare) %>%
    dplyr::mutate(low =  mean(RT) - (2 * sd(RT)),
                  high = mean(RT) + (2 * sd(RT))) %>%
    dplyr::filter(RT >= low & RT <= high)
```

If you want to save yourself effort in running splithalf, you could also
rename your variable (column) names to the function defaults using the
following

``` r
dplyr::rename(dataset,
              RT = "latency",
              condition = FALSE,
              participant = "subject",
              correct = "correct",
              trialnum = "trialnum",
              compare = "congruency")
```

## Examples

This is what you are really here for, isn’t it?

I am assuming that you have preprocessed your data to remove any
outliers, use any cutoffs etc. These functions should be used on the
data that you will then create your scores from, i.e. there should not
be further processing to do before you run any of these. If there is,
then these internal consistency reliability estimates will not reflect
the reliability of the outcome measurements you actually analyse.

### Questions to ask before running splithalf

These questions should feed into what settings are appropriate for your
need, and are aimed to make the *splithalf* function easy to use.

1.  **What is the type of data you have? **

Are you interested in response times, or accuracy rates?

Knowing this, you can set *outcome* = “RT”, or *outcome* = “accuracy”

2.  **How is your outcome score calculated?**

Say that your response time based task has two trial types;
“incongruent” and “congruent”. When you analyse your data will you
use the average RT in each trial type, or will you create a difference
score (or bias) by e.g. subtracting the average RT in congruent trials
from the average RT in incongruent trials. The first I call “average”
and the second I call “difference”.

3.  **Which method would you like to use to estimate (split-half)
    reliability?**

A super common way is to split the data into odd and even trials.
Another is to split by the first half and second half of the trials.
Both approaches are implemented in the *splithalf* funciton. However, I
believe that the permutation splithalf approach is the way forward (and
it was the reason why this package was developed, so please use it).

### Our example dataset

For this quick example, we will simulate some data. Lets say we have 60
participants, who each complete a task with two blocks (A and B) of 80
trials. Trials are also evenly distributed between “congruent” and
“incongruent” trials. For each trial we have RT data, and are assuming
that participants were accurate in all trials.

``` r
n_participants = 60 ## sample size
n_trials = 80
n_blocks = 2

sim_data <- data.frame(participant_number = rep(1:n_participants, each = n_blocks * n_trials),
                       trial_number = rep(1:n_trials, times = n_blocks * n_participants),
                       block_name = rep(c("A","B"), each = n_trials, length.out = n_participants * n_trials * n_blocks),
                       trial_type = rep(c("congruent","incongruent"), length.out = n_participants * n_trials * n_blocks),
                       RT = rnorm(n_participants * n_trials * n_blocks, 500, 200))
```

### Difference scores

This is by far the most common outcome measure I have come across, so
lets start with that.

Our data will be analysed so that we have two ‘bias’ or ‘difference
score’ outcomes. So, within each block, we will take the average RT in
congruent trials and subtract the average RT in incongruent trials.
Calculating the final scores for each participant and for each block
separately might look a bit like this

    # A tibble: 120 x 5
    # Groups:   participant_number, block_name [120]
       participant_number block_name congruent incongruent     bias
                    <int> <fct>          <dbl>       <dbl>    <dbl>
     1                  1 A               483.        500. -17.4   
     2                  1 B               519.        481.  38.0   
     3                  2 A               505.        525. -19.4   
     4                  2 B               530.        504.  26.3   
     5                  3 A               502.        422.  80.1   
     6                  3 B               475.        520. -44.3   
     7                  4 A               515.        477.  37.9   
     8                  4 B               482.        487.  -4.64  
     9                  5 A               496.        497.  -0.0303
    10                  5 B               532.        554. -21.6   
    # ... with 110 more rows

ok, lets see how reliable our A and B outcome scores (“bias”) are.

``` r
library("splithalf")

difference <- splithalf(data = sim_data,
                        outcome = "RT",
                        score = "difference",
                        conditionlist = c("A", "B"),
                        halftype = "random",
                        permutations = 5000,
                        var.RT = "RT",
                        var.condition = "block_name",
                        var.participant = "participant_number",
                        var.compare = "trial_type",
                        compare1 = "congruent",
                        compare2 = "incongruent",
                        average = "mean")
```

``` 
  condition  n splithalf 95_low 95_high spearmanbrown SB_low SB_high
1         A 60      0.01  -0.16     0.2          0.01  -0.28    0.33
2         B 60      0.00  -0.16     0.2          0.00  -0.28    0.33
```

#### Reading and reporting the output

The *splithalf* output gives estimates separately for each condition
defined (if no condition is defined, the function assumes that you have
only a single condition, which it will call “all” to represent that all
trials were included).

The second column (n) gives the number of participants analysed. If, for
some reason one participant has too few trials to analyse, or did not
complete one condition, this will be reflected here. I suggest you
compare this n to your expected n to check that everything is running
correctly. If the ns dont match, we have a problem. More likely, it will
throw an error, but useful to know.

Next are the estimates; the splithalf column and the associated 95%
percentile intervals, and the Spearman-Brown corrected estimate with its
own percentile intervals. Unsurprisingly, our simlated random data does
not yield internally consistant measurements.

*What should I report?* Ideally, report everything. I have included 95%
percentiles of the estimates to give a picture of the spread of internal
consistency estimates. Also included is the spearman-brown corrected
estimates, which take into account that the estimates are drawn from
half the trials that they could have been. Negative reliabilities are
near uninterpretable and the spearman-brown formula is not useful in
this case. For comparibility between studies I recommend reporting both
the raw and the corrected estimates. Something like the following should
be sufficient;

> We estimated the internal consitency of bias A and B using a
> permutation-based splithalf approach (Parsons 2020) with 5000 random
> splits. The (Spearman-Brown corrected) splithalf internal consistency
> of bias A was were *r*<sub>SB</sub> = 0.01, 95%CI \[-0.28,0.33\].
> 
> — Me, reporting reliability, just now

Simples. I hope :)

### Average scores

OK, lets change things up and look at average scores only. In this case,
imagine that we have only a single trial type and we can then ignore the
trial type option. We will want separate outcome scores for each block
of trials, but this time it is simply the average RT in each block. Lets
see how that looks within *splithalf*. Note that the main difference is
that we have omitted the inputs about what trial types to ‘compare’, as
this is irrelevant for the current task.

``` r
average <- splithalf(data = sim_data,
                     outcome = "RT",
                     score = "average",
                     conditionlist = c("A", "B"),
                     halftype = "random",
                     permutations = 5000,
                     var.RT = "RT",
                     var.condition = "block_name",
                     var.participant = "participant_number",
                     average = "mean")
Warning in splithalf(data = sim_data, outcome = "RT", score = "average", :
var.trialnum will soon be depreciated
```

``` 
  condition  n splithalf 95_low 95_high spearmanbrown SB_low SB_high
1         A 60     -0.13   -0.3    0.05         -0.22  -0.46     0.1
2         B 60      0.08   -0.1    0.25          0.13  -0.18     0.4
```

### Difference-of-difference scores

The difference of differences score is a bit more complex, and perhaps
also less common. I programmed this aspect of the package initially
because I had seen a few papers that used a change in bias score in
their analysis, and I wondered “I wonder how reliable that is as an
individual difference measure”. Be warned, difference scores are nearly
always less reliable than raw averages, and it’s very likely that
differences-of-differences will be the least reliable amongst the bunch.

So, lets say our dependant/outcome variable in our task is the
difference between bias observed in block A and B. So our outcome is
calculated something like this.

BiasA = incongruent\_A - congruent\_A

BiasB = incongruent\_B - congruent\_B

Outcome = BiasB - BiasA

In our function, we specify this very similarly as in the difference
score example. The only change will be changing the score to
“difference\_of\_difference” (largely because I could not think of a
better name to use). Note that we will keep the condition list
consisting of A and B. But, specifying that we are interested in the
difference of differences will lead the function to calculate the
outcome scores apropriately.

``` r

diff_of_diff <- splithalf(data = sim_data,
                        outcome = "RT",
                        score = "difference_of_difference",
                        conditionlist = c("A", "B"),
                        halftype = "random",
                        permutations = 5000,
                        var.RT = "RT",
                        var.condition = "block_name",
                        var.participant = "participant_number",
                        var.compare = "trial_type",
                        compare1 = "congruent",
                        compare2 = "incongruent",
                        average = "mean")
Warning in splithalf(data = sim_data, outcome = "RT", score =
"difference_of_difference", : var.trialnum will soon be depreciated
```

``` 
     condition  n splithalf 95_low 95_high spearmanbrown SB_low SB_high
1 change score 60      0.05  -0.13    0.23          0.08  -0.22    0.38
```

I do not forsee the difference\_of\_difference option being used often,
but I will continue to maintain it.

## Multiverse analysis extension

(to be added, see [psyarxiv.com/y6tcz](https://psyarxiv.com/y6tcz))

## Important considerations

### how many permutations?

To examine how many random splits are required to provide a precise
estimate, a short simulation was performed including 20 estimates of the
spearman-brown reliability estimate, for each of 1, 10, 50, 100, 1000,
2000, 5000, 10000, and 20000 random splits. This simulation was
performed on data from one block of \~ 80 trials. Based on this
simulation, I recommend 5000 (or more) random splits be used to
calculate split-half reliability. 5000 permutations yielded a standard
deviation of .002 and a total range of .008, indicating that the
reliability estimates are stable to two decimal places with this number
of random splits. Increasing the number of splits improved precision,
however 20000 splits were required to reduce the standard deviation to
.001.

So, 5000 is good. More is better, but will not yield greatly improved
estimates.

### how fast is *splithalf*?

The quick answer is, it depends. The biggest factor will be your machine
speed. For relative times, I ran a simulation with a range of sample
sizes, numbers of conditions, numbers of trials, and permutations. I’ll
analyse this properly at a later date, but hopefully this visualisation
is a useful demonstration of the relative processing times.

``` r
speed <- read.csv("data/speedtestdata.csv")
speed <- speed %>%
  rename(Sim = X,
         sample_size = V1,
         Number_conditions = V2,
         trials = V3,
         permutations = V4,
         runtime = V5)

speed2 <- speed

speed2$sample_size <- as.factor(speed2$sample_size)
speed2$Number_conditions <- as.factor(speed2$Number_conditions)
speed2$trials <- as.factor(speed2$trials)
speed2$permutations <- as.factor(speed2$permutations)


ggplot(data = speed2, 
       aes(x = permutations, y = runtime, colour = sample_size, shape = trials, group = interaction(trials, sample_size))) +
  geom_point(size = 3) +
  geom_line() +
  facet_grid(. ~ Number_conditions)
```

![](README-unnamed-chunk-15-1.png)<!-- -->

## Wish list

The biggest task on my wishlist was to combine the six or so functions
that formed earlier versions of the splithalf package into a single,
unified, function. This I have achieved, so in no particular order I
have some things I still want to achieve with this package in future
versions. If you have any more, please contact me and I will add them to
my list

  - prettier output
  - error tracking
      - in testing the function, having little data for one participant,
        e.g. only two trials in one condition, leads to errors gone
        wild. So, I’d like to add some checking functionality, perhaps
        as a separate *splithalf-checker* function in the package.

## References

<div id="refs" class="references">

<div id="ref-R-splithalf">

Parsons, Sam. 2020. *Splithalf: Robust Estimates of Split Half
Reliability.* <https://doi.org/10.6084/m9.figshare.11956746.v4>.

</div>

<div id="ref-parsons_kruijt_fox_2019">

Parsons, Sam, Anne-Wil Kruijt, and Elaine Fox. 2019. “Psychological
Science Needs a Standard Practice of Reporting the Reliability of
Cognitive Behavioural Measurements.” Advances in Methods; Practices in
Psychological Science.
<https://doi.org/https://doi.org/10.1177/2515245919879695>.

</div>

<div id="ref-R-dplyr">

Wickham, Hadley, Romain François, Lionel Henry, and Kirill Müller. 2018.
*Dplyr: A Grammar of Data Manipulation*.
<https://CRAN.R-project.org/package=dplyr>.

</div>

</div>

1.  You can also use the package to estimate reliability by splitting
    the data into odd and even trials, or into the first and last half
    of trials. In all honesty I’d rather just remove these as it is not
    the actual purpose of the package. But, it works, and maybe it would
    be useful to somebody
