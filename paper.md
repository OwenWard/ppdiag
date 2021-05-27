---
title: 'ppdiag: Diagnostic Tools for Temporal Point Processes'
tags:
  - R
  - Diagnostics
  - Goodness of fit
  - Temporal Point Processes
authors:
  - name: Sally Sun
    affiliation: 1 # (Multiple affiliations must be quoted)
  - name: Owen G. Ward
    orcid: 0000-0002-9678-3542
    affiliation: 1
  - name: Jing Wu
    affiliation: 1
  - name: Lihao Xiao
    affiliation: 1
  - name: Xiaoxi Zhao
    affiliation: 1
  - name: Tian Zheng
    affiliation: "1,2"
affiliations:
 - name: Department of Statistics, Columbia University
   index: 1
 - name: Data Science Institute, Columbia University
   index: 2
date: "2021-05-21"
bibliography: paper.bib
---



# Summary

Temporal Point Processes are widely used to model
phenomena in many fields, such as finance, seismology, and neuroscience.
However, tools to evaluate the fit of these point
processes to data, and to identify reasons for lack of
fit, are not readily implemented in common software.
Here we provide `ppdiag`, an `R`
package containing a 
selection of statistically motivated
tools to analyse the goodness of fit of point processes to
data, as have been described in @wu2021diagnostics.

# Statement of Need
Given the broad application of Temporal Point Processes, tools for analysing the fit of 
point processes to data can be used by many practitioners.
This package provides functions to evaluate the fit of
univariate temporal point processes. These functions allow: 

- Simulation 
of data from a range of common univariate point processes,
including Homogeneous 
Poisson Process, Hawkes Process, and Markov-modulated Hawkes and Poisson
Processes. 

- Fitting common univariate point processes to data,
and plotting the intensity function over data. 

- After fitting a point process model to data, evaluating the 
ability of that model to capture the temporal
structure present in data.
Methods for diagnostics include computing raw and Pearson residuals,
a Kolmogrov-Smirnov 
test (based on the time rescaling theorem)
and corresponding diagnostic plots. These diagnostics can be used to 
compare the fit of multiple point processes to data and to
identify reasons for lack of fit of a process to data.


Though there are some existing 
packages that contain functions for simulating point 
processes, [@hawkes_14;@Markov_17],
and for computing some 
simple residuals [@PtProcess2010],
to the best of our knowledge, 
there are no existing packages that provide methods for
diagnostics of different 
temporal point processes and for comparing the fit of these point processes. 
These diagnostic tools are useful to identify problems in model 
fitting and understand the causes of this lack of fit.

# Example

We illustrate the use of this functionality
with a simple example, using earthquake after-shock data
[@PtProcess2010]. We consider all after shocks within a
year of the original Phuket earthquake and wish to see 
if they can be described by a temporal point process.


```r
set.seed(2021) 
library(ppdiag)
library(PtProcess)
data("Phuket")
end <- 365
event_times <- Phuket$time[Phuket$time < end]
```

We can fit a homogeneous Poisson process and look at the
goodness of fit of this model to the data. From the resulting 
diagnostics it is clear that a homogeneous Poisson process
is not suitable for modelling the temporal patterns in this 
data.


```r
shock_hpp <- fithpp(event_times, end = end)
pp_diag(shock_hpp, events = event_times)
```

![](paper_files/figure-latex/fit_hpp-1.pdf)<!-- --> 

```
#> 
#> Raw residual: 0.07176529
#> Pearson residual: 0.08470523
#> 
#> 	One-sample Kolmogorov-Smirnov test
#> 
#> data:  r
#> D = 0.78626, p-value < 2.2e-16
#> alternative hypothesis: two-sided
```

We can fit a self exciting Hawkes process to this
data and examine the results of that fit. We see that from our
diagnostic tools a Hawkes process is much more capable of
describing the temporal structure of this data.
<!-- Examining the results of the Kolmogrov-Smirnov test, based -->
<!-- on the time rescaling theorem [@brown2002time], indicates that -->
<!-- a Hawkes process better describes these events. -->


```r
shock_hp <- fithp(events = event_times, end = end)
pp_diag(shock_hp, events = event_times)
```

![](paper_files/figure-latex/fit_hawkes-1.pdf)<!-- --> 

```
#> Raw residual: 1.026715
#> Pearson residual: -3.715193
#> 
#> 	One-sample Kolmogorov-Smirnov test
#> 
#> data:  r
#> D = 0.0357, p-value = 0.8922
#> alternative hypothesis: two-sided
```

# References
