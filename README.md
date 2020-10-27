
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
#>  [1]   2.779609   3.510927   8.948560   9.772952   9.864748  10.587827
#>  [7]  15.251465  15.252676  15.482503  15.540306  21.925012  22.121409
#> [13]  26.896706  26.908244  27.312798  28.462785  36.123851  38.042047
#> [19]  41.764488  43.741577  50.478795  57.531634  59.032628  63.689044
#> [25]  64.582702  65.278550  65.714621  67.185950  67.505089  70.785846
#> [31]  71.222587  71.909049  72.368473  74.687487  74.870645  81.273875
#> [37]  87.711854  92.769609 100.516512 102.809242 103.641661 105.990892
#> [43] 106.219851 110.871966 111.962612 115.433655 117.373376 118.765268
#> [49] 118.831500 119.002221 119.071469 123.548214 123.836244 127.572872
#> [55] 128.402148 129.078238 134.535625 139.608967 140.772007 144.536145
#> [61] 144.545341 144.606848 144.736958 145.299266 145.829514 149.061761
#> [67] 154.013332 158.516525 158.814930 166.543772 167.098650 168.318820
#> [73] 171.490055 173.044802 173.069064 178.545958 179.618169 180.517618
#> [79] 184.435011 185.502133 186.889170 187.736321 190.976533 194.044961
#> [85] 196.726108 197.049282 198.250082
#> 
#> $lambda.max
#> [1] 1.204697
```

We can readily evaluate the fit of a homogeneous Poisson process to this
data.

``` r
est_hpp <- fithpp(sim_hp$events)
est_hpp
#> $lambda
#> [1] 0.44508
#> 
#> $events
#>  [1]   2.779609   3.510927   8.948560   9.772952   9.864748  10.587827
#>  [7]  15.251465  15.252676  15.482503  15.540306  21.925012  22.121409
#> [13]  26.896706  26.908244  27.312798  28.462785  36.123851  38.042047
#> [19]  41.764488  43.741577  50.478795  57.531634  59.032628  63.689044
#> [25]  64.582702  65.278550  65.714621  67.185950  67.505089  70.785846
#> [31]  71.222587  71.909049  72.368473  74.687487  74.870645  81.273875
#> [37]  87.711854  92.769609 100.516512 102.809242 103.641661 105.990892
#> [43] 106.219851 110.871966 111.962612 115.433655 117.373376 118.765268
#> [49] 118.831500 119.002221 119.071469 123.548214 123.836244 127.572872
#> [55] 128.402148 129.078238 134.535625 139.608967 140.772007 144.536145
#> [61] 144.545341 144.606848 144.736958 145.299266 145.829514 149.061761
#> [67] 154.013332 158.516525 158.814930 166.543772 167.098650 168.318820
#> [73] 171.490055 173.044802 173.069064 178.545958 179.618169 180.517618
#> [79] 184.435011 185.502133 186.889170 187.736321 190.976533 194.044961
#> [85] 196.726108 197.049282 198.250082
#> 
#> attr(,"class")
#> [1] "hpp"

diagpp(est_hpp, events = sim_hp$events)
```

<img src="man/figures/README-fit hpp-1.png" width="100%" /><img src="man/figures/README-fit hpp-2.png" width="100%" />

    #> 
    #> Raw residual: 87
    #> Pearson residual: -1.854397
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.10688, p-value = 0.2546
    #> alternative hypothesis: two-sided

``` r
hp_est <- fithp(events = sim_hp$events)
diagpp(hp_est, events = sim_hp$events)
```

<img src="man/figures/README-fit hp-1.png" width="100%" /><img src="man/figures/README-fit hp-2.png" width="100%" />

    #> Raw residual: -0.002985878
    #> Pearson residual: 1.838571
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.099039, p-value = 0.3383
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
    #> Pearson residual: 0
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.17332, p-value = 0.08766
    #> alternative hypothesis: two-sided

Similarly for a Hawkes process.

``` r
est_hp <- fithp(events = mmhp_events$events)
diagpp(est_hp,mmhp_events$events)
```

<img src="man/figures/README-fit hp to mmhp-1.png" width="100%" /><img src="man/figures/README-fit hp to mmhp-2.png" width="100%" />

    #> Raw residual: -7.926736e-05
    #> Pearson residual: -0.1553224
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.077011, p-value = 0.906
    #> alternative hypothesis: two-sided

We can then compare to the true point process model.

``` r
diagpp(mmhp_obj,mmhp_events$events)
```

<img src="man/figures/README-fit mmhp-1.png" width="100%" /><img src="man/figures/README-fit mmhp-2.png" width="100%" />

    #> Raw residual: 12.28883
    #> Pearson residual: 19.04034
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  r
    #> D = 0.19954, p-value = 0.03205
    #> alternative hypothesis: two-sided
