
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ppdiag

<!-- badges: start -->

[![R build
status](https://github.com/OwenWard/ppdiag/workflows/R-CMD-check/badge.svg)](https://github.com/OwenWard/ppdiag/actions)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Codecov test
coverage](https://codecov.io/gh/OwenWard/ppdiag/branch/main/graph/badge.svg)](https://codecov.io/gh/OwenWard/ppdiag?branch=main)
<!-- badges: end -->

`ppdiag` is an `R` package which provides a collection of tools which
can be used to assess the fit of temporal point processes to data.

These currently include:

-   Simulating data from a specified point process
-   Fitting a specified point process model to data
-   Evaluating the fit of a point process model to data using several
    diagnostic tools

# Installation

<!-- You can install the released version of ppdiag from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("ppdiag") -->
<!-- ``` -->

The current version of this package is available from
[GitHub](https://github.com/OwenWard/ppdiag) with:

``` r
# install.packages("remotes")
remotes::install_github("OwenWard/ppdiag")
```

# Example

To illustrate some of the basic functionality of this package, we can
simulate data from a specified Hawkes process and examine our diagnostic
results when we fit a homogeneous Poisson process to this data.

``` r
library(ppdiag)

hp_obj <- pp_hp(lambda0 = 0.2, alpha = 0.35, beta = 0.8)
sim_hp <- pp_simulate(hp_obj, end = 200)
sim_hp
#>  [1]   1.000335   7.362964  13.222860  22.691274  22.931674  23.812001
#>  [7]  30.111743  30.911255  31.785699  34.043541  35.810878  36.946860
#> [13]  37.859046  38.632845  39.744749  47.255919  50.786608  51.030776
#> [19]  51.620849  57.298307  79.279840  86.717237 127.258388 127.288075
#> [25] 141.386804 142.747274 143.066975 143.302322 143.742617 143.819971
#> [31] 145.807490 147.940873 155.743269 158.730903 168.932510 169.400273
#> [37] 176.868282 178.666560 182.035931 185.496845 187.281237 187.841325
#> [43] 189.068486 189.794997 190.047020 190.205587 190.578139 191.979409
#> [49] 196.398136 199.215925
```

We can readily evaluate the fit of a homogeneous Poisson process to this
data.

``` r
est_hpp <- fithpp(sim_hp)
est_hpp
#> $lambda
#> [1] 0.250984
#> 
#> $events
#>  [1]   1.000335   7.362964  13.222860  22.691274  22.931674  23.812001
#>  [7]  30.111743  30.911255  31.785699  34.043541  35.810878  36.946860
#> [13]  37.859046  38.632845  39.744749  47.255919  50.786608  51.030776
#> [19]  51.620849  57.298307  79.279840  86.717237 127.258388 127.288075
#> [25] 141.386804 142.747274 143.066975 143.302322 143.742617 143.819971
#> [31] 145.807490 147.940873 155.743269 158.730903 168.932510 169.400273
#> [37] 176.868282 178.666560 182.035931 185.496845 187.281237 187.841325
#> [43] 189.068486 189.794997 190.047020 190.205587 190.578139 191.979409
#> [49] 196.398136 199.215925
#> 
#> attr(,"class")
#> [1] "hpp"

pp_diag(est_hpp, events = sim_hp)
```

<img src="man/figures/README-fit_hpp-1.png" width="75%" />

    #> 
    #> Raw residual: 0
    #> Pearson residual: 2.842171e-14
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.20349, p-value = 0.02719
    #> alternative hypothesis: two-sided

``` r
hp_est <- fithp(events = sim_hp)
pp_diag(hp_est, events = sim_hp)
```

<img src="man/figures/README-fit_hp-1.png" width="75%" />

    #> Raw residual: 0.002281391
    #> Pearson residual: 0.02870257
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.058087, p-value = 0.9923
    #> alternative hypothesis: two-sided

## MMHP Example

This is particularly useful for more complex point processes, such as
the Markov Modulated Hawkes Process. We can simulate events from this
model and examine the fit of simpler point processes to this data.

``` r
Q <- matrix(c(-0.2, 0.2, 0.1, -0.1), ncol = 2, byrow = TRUE)

mmhp_obj <- pp_mmhp(Q, delta = c(1 / 3, 2 / 3), 
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
```

We can easily fit a homogeneous Poisson process and visualise the
goodness of fit.

``` r
est_hpp <- fithpp(events = mmhp_events$events)
pp_diag(est_hpp,mmhp_events$events)
```

<img src="man/figures/README-fit_hpp_to_mmhp-1.png" width="75%" />

    #> 
    #> Raw residual: -1
    #> Pearson residual: -1.319881
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.25761, p-value = 0.002052
    #> alternative hypothesis: two-sided

Similarly for a Hawkes process.

``` r
est_hp <- fithp(events = mmhp_events$events)
pp_diag(est_hp,mmhp_events$events)
```

<img src="man/figures/README-fit_hp_to_mmhp-1.png" width="75%" />

    #> Raw residual: -0.3648369
    #> Pearson residual: -1.327958
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.098845, p-value = 0.6761
    #> alternative hypothesis: two-sided

We can then compare to the true point process model.

``` r
pp_diag(mmhp_obj, mmhp_events$events)
```

<img src="man/figures/README-fit_mmhp-1.png" width="75%" />

    #> Raw residual: 12.79318
    #> Pearson residual: 14.1414
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.13084, p-value = 0.3297
    #> alternative hypothesis: two-sided

# Getting help and contributing

Please file any issues
[here](https://github.com/OwenWard/ppdiag/issues). Similarly, we would
be delighted if anyone would like to contribute to this package (such as
adding other point processes, kernel functions). Feel free to reach out.

# References

-   Wu et al., Diagnostics and Visualization of Point Process Models for
    Event Times on a Social Network, <https://arxiv.org/abs/2001.09359>
