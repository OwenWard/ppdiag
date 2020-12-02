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
    orcid: 0000-0002-9678-3542
    #affiliation: 1
  - name: Xiaoxi Zhao
  - name: Jing Wu
  - name: Tian Zheng
affiliations:
 - name: Columbia University
citation_author: Sun et. al.
date: 3 December 2020
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

# Example

We illustrate the use of this functionality
with a simple example of readily available 
email data [@openintro_20].


```r
library(ppdiag)
library(openintro)
#> Loading required package: airports
#> Loading required package: cherryblossom
#> Loading required package: usdata
library(lubridate)
#> 
#> Attaching package: 'lubridate'
#> The following objects are masked from 'package:base':
#> 
#>     date, intersect, setdiff, union
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
diagpp(email_hpp, events = time_data)
#> Warning in ks.test(r, "pexp"): ties should not be present for the Kolmogorov-
#> Smirnov test
```

![](paper_files/figure-latex/fit hpp-1.pdf)<!-- --> 

```
#> 
#> Raw residual: -1.012849
#> Pearson residual: -0.8935674
#> 
#> 	One-sample Kolmogorov-Smirnov test
#> 
#> data:  r
#> D = 0.12391, p-value = 0.0002123
#> alternative hypothesis: two-sided
```

Similarly, we can fit a self exciting Hawkes process to this
data and examine the results of that fit.


```r
email_hp <- fithp(events = time_data)
diagpp(email_hp, time_data)
```

![](paper_files/figure-latex/fit hawkes-1.pdf)<!-- --> 

```
#> Raw residual: 0.0001270847
#> Pearson residual: 1.560215
#> 
#> 	One-sample Kolmogorov-Smirnov test
#> 
#> data:  r
#> D = 0.041649, p-value = 0.6795
#> alternative hypothesis: two-sided
```
Examining the results of the Kolmogrov-Smirnov test, based
on the time rescaling theorem [@brown2002time], indicates that
a Hawkes process better describes these events.

We can then examine the estimated intensity of this Hawkes
process to this data.


```r
drawHPIntensity(email_hp, events = time_data, 
                plot_events = TRUE)
#> Using the hp object. Set fit=TRUE to fit events provided.
```

![](paper_files/figure-latex/hawkes intensity-1.pdf)<!-- --> 


<!-- # Citations -->

<!-- Citations to entries in paper.bib should be in -->
<!-- [rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html) -->
<!-- format. -->

<!-- For a quick reference, the following citation commands can be used: -->
<!-- - `@author:2001`  ->  "Author et al. (2001)" -->
<!-- - `[@author:2001]` -> "(Author et al., 2001)" -->
<!-- - `[@author1:2001; @author2:2001]` -> "(Author1 et al., 2001; Author2 et al., 2002)" -->



# References
