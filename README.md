
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
#> Simulating up to endtime. To simulate n events specify n.
sim_hp
#>  [1]   0.5951986   0.7653878   0.8479978   4.5875672   5.3186084  15.1530210
#>  [7]  15.2011918  15.7163021  16.7334255  17.8686460  18.2447211  22.1398131
#> [13]  22.8523537  23.6965500  23.9506462  24.0203058  24.9664049  25.0474782
#> [19]  25.0786556  27.9278040  28.1164846  37.3315057  40.9141929  51.6368068
#> [25]  52.1606010  52.2770599  55.9608869  69.5354111  71.0461673  82.9885951
#> [31]  85.4633689  86.9622505  88.9228416  89.2506349  89.2630108 101.0753493
#> [37] 106.8634961 116.2641725 128.0664185 128.4523822 129.4587701 132.3100564
#> [43] 134.5765618 134.9458399 135.0961929 136.0137305 136.0352334 136.0939733
#> [49] 136.4842978 137.7900191 137.8519545 138.7862053 141.6156679 141.6649943
#> [55] 141.7122471 144.1042632 148.3011617 152.1033892 155.5969837 156.6854051
#> [61] 157.6770171 157.7113150 158.3595015 158.6691715 159.1357102 160.1443958
#> [67] 163.3646471 163.6378850 166.5655617 169.8380047 170.1328657 179.4675362
#> [73] 185.6367542 186.0615532 192.3051756 192.4605035 197.2653617 199.8978989
```

We can readily evaluate the fit of a homogeneous Poisson process to this
data.

``` r
est_hpp <- fithpp(sim_hp)
est_hpp
#> $lambda
#> [1] 0.3901992
#> 
#> $events
#>  [1]   0.5951986   0.7653878   0.8479978   4.5875672   5.3186084  15.1530210
#>  [7]  15.2011918  15.7163021  16.7334255  17.8686460  18.2447211  22.1398131
#> [13]  22.8523537  23.6965500  23.9506462  24.0203058  24.9664049  25.0474782
#> [19]  25.0786556  27.9278040  28.1164846  37.3315057  40.9141929  51.6368068
#> [25]  52.1606010  52.2770599  55.9608869  69.5354111  71.0461673  82.9885951
#> [31]  85.4633689  86.9622505  88.9228416  89.2506349  89.2630108 101.0753493
#> [37] 106.8634961 116.2641725 128.0664185 128.4523822 129.4587701 132.3100564
#> [43] 134.5765618 134.9458399 135.0961929 136.0137305 136.0352334 136.0939733
#> [49] 136.4842978 137.7900191 137.8519545 138.7862053 141.6156679 141.6649943
#> [55] 141.7122471 144.1042632 148.3011617 152.1033892 155.5969837 156.6854051
#> [61] 157.6770171 157.7113150 158.3595015 158.6691715 159.1357102 160.1443958
#> [67] 163.3646471 163.6378850 166.5655617 169.8380047 170.1328657 179.4675362
#> [73] 185.6367542 186.0615532 192.3051756 192.4605035 197.2653617 199.8978989
#> 
#> attr(,"class")
#> [1] "hpp"

pp_diag(est_hpp, events = sim_hp)
```

<img src="man/figures/README-fit_hpp-1.png" width="75%" />

    #> 
    #> Raw residual: 0
    #> Pearson residual: -1.421085e-14
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.21088, p-value = 0.001602
    #> alternative hypothesis: two-sided

``` r
hp_est <- fithp(events = sim_hp)
pp_diag(hp_est, events = sim_hp)
```

<img src="man/figures/README-fit_hp-1.png" width="75%" />

    #> Raw residual: -0.0008292571
    #> Pearson residual: 0.2835241
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.066161, p-value = 0.8618
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
    #> Pearson residual: -1.502973
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.21718, p-value = 0.01501
    #> alternative hypothesis: two-sided

Similarly for a Hawkes process.

``` r
est_hp <- fithp(events = mmhp_events$events)
pp_diag(est_hp,mmhp_events$events)
```

<img src="man/figures/README-fit_hp_to_mmhp-1.png" width="75%" />

    #> Raw residual: -0.5382629
    #> Pearson residual: -1.218022
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.068162, p-value = 0.962
    #> alternative hypothesis: two-sided

We can then compare to the true point process model.

``` r
pp_diag(mmhp_obj, mmhp_events$events)
```

<img src="man/figures/README-fit_mmhp-1.png" width="75%" />

    #> Raw residual: 14.33129
    #> Pearson residual: 18.97503
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.11249, p-value = 0.5154
    #> alternative hypothesis: two-sided

# Getting help and contributing

Please file any issues
[here](https://github.com/OwenWard/ppdiag/issues). Similarly, we would
be delighted if anyone would like to contribute to this package (such as
adding other point processes, kernel functions). Feel free to reach out.

# References

-   Wu et al., Diagnostics and Visualization of Point Process Models for
    Event Times on a Social Network, <https://arxiv.org/abs/2001.09359>
