
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
    diagnostic tools

# Installation

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

# Example

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
#>  [1]   2.916023   5.783456  22.404413  23.044874  23.153227  23.276124
#>  [7]  33.313528  35.253957  44.478615  52.440900  53.338280  53.514269
#> [13]  53.733039  54.941978  54.970953  55.039775  55.549487  55.586670
#> [19]  55.959531  55.960063  57.428581  57.461721  63.725128  64.136814
#> [25]  65.960233  68.397918  74.402604  74.458960  74.904348  75.539964
#> [31]  75.712993  76.922587  77.199168  77.414282  78.893970  91.316587
#> [37]  91.519747 102.263574 103.099690 112.604433 113.906237 122.746143
#> [43] 125.166665 138.749346 138.978248 139.866113 149.596461 151.833134
#> [49] 154.388409 156.386153 157.622489 158.519023 159.228525 159.938288
#> [55] 159.940480 160.181665 160.457904 160.717399 162.483716 166.986370
#> [61] 175.504747 179.593941 180.070925 182.624006 185.819246 189.839864
#> [67] 196.234216 198.116098
#> 
#> $lambda.max
#> [1] 1.715111
```

We can readily evaluate the fit of a homogeneous Poisson process to this
data.

``` r
est_hpp <- fithpp(sim_hp$events)
est_hpp
#> $lambda
#> [1] 0.3483605
#> 
#> $events
#>  [1]   2.916023   5.783456  22.404413  23.044874  23.153227  23.276124
#>  [7]  33.313528  35.253957  44.478615  52.440900  53.338280  53.514269
#> [13]  53.733039  54.941978  54.970953  55.039775  55.549487  55.586670
#> [19]  55.959531  55.960063  57.428581  57.461721  63.725128  64.136814
#> [25]  65.960233  68.397918  74.402604  74.458960  74.904348  75.539964
#> [31]  75.712993  76.922587  77.199168  77.414282  78.893970  91.316587
#> [37]  91.519747 102.263574 103.099690 112.604433 113.906237 122.746143
#> [43] 125.166665 138.749346 138.978248 139.866113 149.596461 151.833134
#> [49] 154.388409 156.386153 157.622489 158.519023 159.228525 159.938288
#> [55] 159.940480 160.181665 160.457904 160.717399 162.483716 166.986370
#> [61] 175.504747 179.593941 180.070925 182.624006 185.819246 189.839864
#> [67] 196.234216 198.116098
#> 
#> attr(,"class")
#> [1] "hpp"

diagpp(est_hpp, events = sim_hp$events)
```

<img src="man/figures/README-fit_hpp-1.png" width="50%" />

    #> 
    #> Raw residual: 68
    #> Pearson residual: -1.721097
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.21683, p-value = 0.002755
    #> alternative hypothesis: two-sided

``` r
hp_est <- fithp(events = sim_hp$events)
diagpp(hp_est, events = sim_hp$events)
```

<img src="man/figures/README-fit_hp-1.png" width="50%" />

    #> Raw residual: -0.0003561931
    #> Pearson residual: 0.8289376
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.046149, p-value = 0.9974
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

<img src="man/figures/README-fit_hpp_to_mmhp-1.png" width="50%" />

    #> 
    #> Raw residual: 51
    #> Pearson residual: -7.105427e-15
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.29346, p-value = 0.0002608
    #> alternative hypothesis: two-sided

Similarly for a Hawkes process.

``` r
est_hp <- fithp(events = mmhp_events$events)
diagpp(est_hp,mmhp_events$events)
```

<img src="man/figures/README-fit_hp_to_mmhp-1.png" width="50%" />

    #> Raw residual: -6.772639e-05
    #> Pearson residual: -0.1020939
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.081728, p-value = 0.8651
    #> alternative hypothesis: two-sided

We can then compare to the true point process model.

``` r
diagpp(mmhp_obj,mmhp_events$events)
```

<img src="man/figures/README-fit_mmhp-1.png" width="50%" />

    #> Raw residual: 9.341232
    #> Pearson residual: 9.748232
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.12392, p-value = 0.3941
    #> alternative hypothesis: two-sided

# Getting help and contributing

Please file any issues
[here](https://github.com/OwenWard/ppdiag/issues). Similarly, we would
be delighted if anyone would like to contribute to this package (such as
adding other point processes, kernel functions). Feel free to reach out.
