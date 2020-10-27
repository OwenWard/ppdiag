
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ppdiag

<!-- badges: start -->

[![R build
status](https://github.com/OwenWard/ppdiag/workflows/R-CMD-check/badge.svg)](https://github.com/OwenWard/ppdiag/actions)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Codecov test
coverage](https://codecov.io/gh/OwenWard/ppdiag/branch/master/graph/badge.svg)](https://codecov.io/gh/OwenWard/ppdiag?branch=master)
<!-- badges: end -->

`ppdiag` is an `R` package which provides a collection of tools which
can be used to assess the fit of temporal point processes to data.

These currently include:

  - Simulating data from a specified point process
  - Fitting a specified point process model to data
  - Evaluating the fit of a point process model to data using several
    diagnostic tools.

## Installation

<!-- You can install the released version of ppdiag from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("ppdiag") -->

<!-- ``` -->

The in development version of this package is available from
[GitHub](https://github.com/OwenWard/ppdiag) with:

``` r
# install.packages("devtools")
devtools::install_github("OwenWard/ppdiag")
```

## Example

To illustrate some of the basic functionality of this package, we can
simulate data from a specified Hawkes process and examine our diagnostic
results when we fit a homogeneous Poisson process to this data.

``` r
library(ppdiag)

hp_obj <- hp(lambda0 = 0.2, alpha = 0.35, beta = 0.8)
sim_hp <- simulatehp(hp_obj, end = 200)
#> Simulating up to endtime. To simulate n events specify n.
sim_hp
#> $events
#>  [1]   0.3908878   0.8577317   1.5681520   1.5713084   1.6367221   2.6602539
#>  [7]   2.7309693   3.0608964   3.5156298   3.6394814   5.7240249   7.0091418
#> [13]   8.2850326   8.7675389  14.8864769  15.5521787  16.5579773  19.9904679
#> [19]  23.7886048  24.5960262  36.9886643  37.2820406  38.4453425  39.5921402
#> [25]  39.8733005  41.3623368  43.1065573  52.4661212  54.2357306  56.0631450
#> [31]  56.6868955  60.3440922  61.3165750  61.6185522  62.1488882  63.2418154
#> [37]  77.2250328  77.8445198  78.0606972  80.6408385  87.0104041  88.6360059
#> [43]  91.1203814  91.8314681  92.1253177  92.5620552  93.7389691 105.3310593
#> [49] 109.8988345 111.4955364 115.6111632 116.7521965 118.7275897 119.2078358
#> [55] 120.9055618 123.5820700 126.7817414 146.7645031 152.9050843 189.4712542
#> [61] 190.9839907 195.9149238
#> 
#> $lambda.max
#> [1] 1.353443
```

We can readily evaluate the fit of a homogeneous Poisson process to this
data.

``` r
est_hpp <- fithpp(sim_hp$events)
est_hpp
#> $lambda
#> [1] 0.3170966
#> 
#> $events
#>  [1]   0.3908878   0.8577317   1.5681520   1.5713084   1.6367221   2.6602539
#>  [7]   2.7309693   3.0608964   3.5156298   3.6394814   5.7240249   7.0091418
#> [13]   8.2850326   8.7675389  14.8864769  15.5521787  16.5579773  19.9904679
#> [19]  23.7886048  24.5960262  36.9886643  37.2820406  38.4453425  39.5921402
#> [25]  39.8733005  41.3623368  43.1065573  52.4661212  54.2357306  56.0631450
#> [31]  56.6868955  60.3440922  61.3165750  61.6185522  62.1488882  63.2418154
#> [37]  77.2250328  77.8445198  78.0606972  80.6408385  87.0104041  88.6360059
#> [43]  91.1203814  91.8314681  92.1253177  92.5620552  93.7389691 105.3310593
#> [49] 109.8988345 111.4955364 115.6111632 116.7521965 118.7275897 119.2078358
#> [55] 120.9055618 123.5820700 126.7817414 146.7645031 152.9050843 189.4712542
#> [61] 190.9839907 195.9149238
#> 
#> attr(,"class")
#> [1] "hpp"

diagpp(est_hpp, events = sim_hp$events)
```

<img src="man/figures/README-fit hpp-1.png" width="100%" /><img src="man/figures/README-fit hpp-2.png" width="100%" />

    #> 
    #> Raw residual: 62
    #> Pearson residual: -0.2201141
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.22149, p-value = 0.003753
    #> alternative hypothesis: two-sided

``` r
hp_est <- fithp(events = sim_hp$events)
diagpp(hp_est, events = sim_hp$events)
```

<img src="man/figures/README-fit hp-1.png" width="100%" /><img src="man/figures/README-fit hp-2.png" width="100%" />

    #> Raw residual: -0.0002714196
    #> Pearson residual: 0.5984266
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.077394, p-value = 0.8237
    #> alternative hypothesis: two-sided

## MMHP Example

This is particularly useful for more complex point processes, such as
the Markov Modulated Hawkes Process. We can simulate events from this
model and examine the fit of simpler point processes to this data.

``` r
Q <- matrix(c(-0.2, 0.2, 0.1, -0.1), ncol = 2, byrow = TRUE)

mmhp_obj <- mmhp(Q, delta = c(1 / 3, 2 / 3), 
          lambda0 = 0.2,
          lambda1 = .75,
          alpha = 0.4,
          beta = 0.8)

mmhp_obj
#> $Q
#>      [,1] [,2]
#> [1,] -0.2  0.2
#> [2,]  0.1 -0.1
#> 
#> $delta
#> [1] 0.3333333 0.6666667
#> 
#> $events
#> NULL
#> 
#> $lambda0
#> [1] 0.2
#> 
#> $lambda1
#> [1] 0.75
#> 
#> $alpha
#> [1] 0.4
#> 
#> $beta
#> [1] 0.8
#> 
#> attr(,"class")
#> [1] "mmhp"
mmhp_events <- simulatemmhp(mmhp_obj, n = 50)
#> 50 events simulated. To simulate up to endtime set given_states=TRUE and provide states.
```

We can easily fit a homogeneous Poisson process and visualise the
goodness of fit.

``` r
est_hpp <- fithpp(events = mmhp_events$events)
diagpp(est_hpp,mmhp_events$events)
```

<img src="man/figures/README-fit hpp to mmhp-1.png" width="100%" /><img src="man/figures/README-fit hpp to mmhp-2.png" width="100%" />

    #> 
    #> Raw residual: 51
    #> Pearson residual: 7.105427e-15
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.26469, p-value = 0.001396
    #> alternative hypothesis: two-sided

Similarly for a Hawkes process.

``` r
est_hp <- fithp(events = mmhp_events$events)
diagpp(est_hp,mmhp_events$events)
```

<img src="man/figures/README-fit hp to mmhp-1.png" width="100%" /><img src="man/figures/README-fit hp to mmhp-2.png" width="100%" />

    #> Raw residual: -4.710199e-05
    #> Pearson residual: 0.427208
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.086338, p-value = 0.8191
    #> alternative hypothesis: two-sided

We can then compare to the true point process model.

``` r
diagpp(mmhp_obj,mmhp_events$events)
```

<img src="man/figures/README-fit mmhp-1.png" width="100%" /><img src="man/figures/README-fit mmhp-2.png" width="100%" />

    #> Raw residual: 6.621433
    #> Pearson residual: 5.466606
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.1353, p-value = 0.2923
    #> alternative hypothesis: two-sided
