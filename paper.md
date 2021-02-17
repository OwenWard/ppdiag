---
title: 'ppdiag: Diagnostic Tools for Temporal Point Processes'
tags:
  - R
  - Diagnostics
  - Temporal Point Processes
authors:
  - name: Sally Sun
    #affiliation: 1 # (Multiple affiliations must be quoted)
  - name: Owen G. Ward
    #orcid: 0000-0002-9678-3542
    #affiliation: 1
  - name: Xiaoxi Zhao
  - name: Jing Wu
  - name: Tian Zheng
affiliations:
 - name: Columbia University
citation_author: Sun et. al.
date: "2021-02-12"
year: 2020
bibliography: paper.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS
---



# Summary

Temporal Point Processes are widely used to model
phenomena in many fields, such as finance and neuroscience.
However, tools to evaluate the fit of these point
processes to data, and to identify reasons for lack of
fit, are not readily implemented in common software.
Here we provide `ppdiag`, an `R`
package containing a 
selection of statistically motivated
tools to analyse the goodness of fit of point processes to
data, as have been utilised in @wu2020diagnostics.

# Statement of Need
This package provides functions to evaluate the fit of
univariate temporal point processes. These functions allow: 

- Simulation 
of data from a range of common univariate point processes including Homogeneous 
Poisson Process, Hawkes Process, and Markov-modulated Hawkes Process. 

- Fitting common univariate point processes to data,
and plotting the intensity function over data. 

- After fitting a point process model to data, evaluating the 
ability of that model to capture the temporal structure present in data.
Methods for diagnostics include raw and Pearson residuals, a Kolmogrov-Smirnov 
test and corresponding diagnostic plots. 

# Demonstration

We illustrate the use of this functionality
with a simple example of readily available 
email data [@openintro_20].


```r
library(ppdiag)
library(openintro)
library(lubridate)
first <- ymd_hms(email$time[1],tz="EST")
second <- ymd_hms(email$time[2],tz="EST")
time <- (as.duration(interval(first,
                    ymd_hms(email$time[-1],tz="EST")))/3600)[1:300]
time_data <- sort(unique(time))
```

We can fit a homogeneous Poisson process and look at the
goodness of fit of this model to the data.


```r
email_hpp <- fithpp(time_data)
pp_diag(email_hpp, events = time_data)
#> Please input the right model. Select from hp, hpp, mmpp and mmhp.
```

Similarly, we can fit a self exciting Hawkes process to this
data and examine the results of that fit.


```r
email_hp <- fithp(events = time_data)
pp_diag(email_hp, time_data)
#> Please input the right model. Select from hp, hpp, mmpp and mmhp.
```
Examining the results of the Kolmogrov-Smirnov test, based
on the time rescaling theorem [@brown2002time], indicates that
a Hawkes process better describes these events.

We can then examine the estimated intensity of this Hawkes
process to this data.


```r
drawHPIntensity(email_hp, events = time_data, 
                plot_events = TRUE)
```

![](paper_files/figure-latex/hawkes intensity-1.pdf)<!-- --> 

# Comparison with Other Packages
Though there are some packages that contain functions for simulating point 
processes, [@hawkes_14],[@Markov_17], to the best of our knowledge, 
there are no existing packages that provide methods for
diagnostics of different 
temporal point processes and for comparing the fit of these point processes. 

# References
