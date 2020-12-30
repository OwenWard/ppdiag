
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
sim_hp <- pp_simulate(hp_obj, end = 200)
#> Simulating up to endtime. To simulate n events specify n.
sim_hp
#> $events
#>  [1]   6.630539   8.767464   9.682285  13.399487  20.483223  21.773672
#>  [7]  33.694649  35.424763  40.187920  41.101909  44.857211  52.815650
#> [13]  55.139571  55.530779  57.110114  57.314076  58.718036  61.563907
#> [19]  70.462333  77.469141  77.951951  78.120784  78.285221  79.124248
#> [25]  79.226442  79.946662  80.224214  89.524457  93.052394  98.650243
#> [31]  99.058956 101.283241 102.879069 102.889929 104.402121 107.363445
#> [37] 108.751198 126.281714 128.926162 135.220356 138.066079 138.949757
#> [43] 142.411336 144.423330 149.258149 154.831158 155.358255 155.544310
#> [49] 156.100420 156.822724 164.427710 167.232118 167.420812 168.888179
#> [55] 173.991947 175.668796 175.840282 176.160054 176.251018 184.087337
#> [61] 187.897165 188.030346 188.629058 188.807999 189.008968 191.916507
#> [67] 192.898258 195.578002
#> 
#> $lambda.max
#> [1] 1.066947
```

We can readily evaluate the fit of a homogeneous Poisson process to this
data.

``` r
est_hpp <- fithpp(sim_hp$events)
est_hpp
#> $lambda
#> [1] 0.3598884
#> 
#> $events
#>  [1]   6.630539   8.767464   9.682285  13.399487  20.483223  21.773672
#>  [7]  33.694649  35.424763  40.187920  41.101909  44.857211  52.815650
#> [13]  55.139571  55.530779  57.110114  57.314076  58.718036  61.563907
#> [19]  70.462333  77.469141  77.951951  78.120784  78.285221  79.124248
#> [25]  79.226442  79.946662  80.224214  89.524457  93.052394  98.650243
#> [31]  99.058956 101.283241 102.879069 102.889929 104.402121 107.363445
#> [37] 108.751198 126.281714 128.926162 135.220356 138.066079 138.949757
#> [43] 142.411336 144.423330 149.258149 154.831158 155.358255 155.544310
#> [49] 156.100420 156.822724 164.427710 167.232118 167.420812 168.888179
#> [55] 173.991947 175.668796 175.840282 176.160054 176.251018 184.087337
#> [61] 187.897165 188.030346 188.629058 188.807999 189.008968 191.916507
#> [67] 192.898258 195.578002
#> 
#> attr(,"class")
#> [1] "hpp"

diagpp(est_hpp, events = sim_hp$events)
```

<img src="man/figures/README-fit_hpp-1.png" width="50%" />

    #> 
    #> Raw residual: 68
    #> Pearson residual: -3.977707
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.1204, p-value = 0.2565
    #> alternative hypothesis: two-sided

``` r
hp_est <- fithp(events = sim_hp$events)
diagpp(hp_est, events = sim_hp$events)
```

<img src="man/figures/README-fit_hp-1.png" width="50%" />

    #> Raw residual: 0.0006916934
    #> Pearson residual: -0.1296604
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.066655, p-value = 0.9033
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
mmhp_events <- pp_simulate(mmhp_obj, n = 50)
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
    #> Pearson residual: 0
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.17552, p-value = 0.08102
    #> alternative hypothesis: two-sided

Similarly for a Hawkes process.

``` r
est_hp <- fithp(events = mmhp_events$events)
diagpp(est_hp,mmhp_events$events)
```

<img src="man/figures/README-fit_hp_to_mmhp-1.png" width="50%" />

    #> Raw residual: 4.127367e-05
    #> Pearson residual: -0.1025674
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.09232, p-value = 0.7529
    #> alternative hypothesis: two-sided

We can then compare to the true point process model.

``` r
diagpp(mmhp_obj,mmhp_events$events)
```

<img src="man/figures/README-fit_mmhp-1.png" width="50%" />

    #> Raw residual: 12.09093
    #> Pearson residual: 14.77226
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.18486, p-value = 0.05729
    #> alternative hypothesis: two-sided

# Getting help and contributing

Please file any issues
[here](https://github.com/OwenWard/ppdiag/issues). Similarly, we would
be delighted if anyone would like to contribute to this package (such as
adding other point processes, kernel functions). Feel free to reach out.
