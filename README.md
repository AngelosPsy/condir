# condir

R package for the analysis of conditioning data.

## Warning: This is work in progress so many analyses are not complete.

### Installation

```r
install.packages ("devtools") # If not already installed
library(devtools)
install_github(repo = "AngelosPsy/condir")
library(condir)
```

### Load package

```r
library(condir)
```

### One group example

```r
cs1 <- rnorm(50, 5, 5)
cs2 <- rnorm(50, 1, 5)
tmp <- csCompare(cs1, cs2)
tmp
```

```
## $freq.results
##          method alternative       WG1       WpG1 WG2 WpG2 null.value
## 1 Paired t-test   two.sided 0.9587071 0.07846915   0    0          0
##        LCI     HCI t.statistic df      p.value
## 1 1.743274 5.78707    3.742225 49 0.0004791011
## 
## $bayes.results
##    LNI HNI rscale    bf10       bf01    propError
## 1 -Inf Inf  0.707 55.1932 0.01811817 7.021348e-10
## 
## $descriptives
##     vars  n mean   sd   se
## cs1    1 50 5.03 4.68 0.66
## cs2    2 50 1.26 4.88 0.69
```

```r
csPlot(cs1, cs2, ylab = "CRs")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
csReport(tmp)
```

```
## We perfromed a two.sided Paired t-test. The results of the t-test are t (49) = 3.742, p = 0.
## 
## Those results suggest that there are statistically significant differences between cs1 and cs2. 
## 
## We perfromed a two.sided Bayesian t-test, with a Catchy prior, with its width set to 0.707. The BF10 was equal to BF10 = 55.19. The BF01 was equal to BF01 = 0.02.
## 
## The results suggest that there is very strong evidence for H1, relative to H0.
## 
## The results suggest that there is no evidence for H0, relative to H1.
```

```r
tmp <- csSensitivity(cs1, cs2)
csRobustnessPlot(cs1, cs2, BF01 = FALSE)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-2.png)

```r
csReport(csSensitivityObj = tmp)
```

```
## We perfromed a Sensitivity Analysis using the scaling factors: medium, wide, ultrawide. The results for BF01 were: 0.02, 0.02, 0.03 respectively. The results for BF10 were: 55.19, 47.03, 37.29 respectively.
```

### Two groups example

```r
group <- rep(1:2, 25)
tmp <- csCompare(cs1, cs2, group)
tmp
```

```
## $freq.results
##                    method alternative       WG1     WpG1       WG2
## 1 Welch Two Sample t-test   two.sided 0.9531244 0.294494 0.9555284
##        WpG2 null.value       LCI      HCI t.statistic       df   p.value
## 1 0.3326235          0 -4.035712 4.142682  0.02630703 47.38485 0.9791229
## 
## $bayes.results
##    LNI HNI rscale      bf10     bf01    propError
## 1 -Inf Inf  0.707 0.2827777 3.536347 0.0001939419
## 
## $descriptives
## group: 1
##     vars  n mean   sd   se
## cs1    1 25 5.84 4.11 0.82
## cs2    2 25 2.05 4.76 0.95
## cs3    3 25 3.79 6.77 1.35
## -------------------------------------------------------- 
## group: 2
##     vars  n mean   sd   se
## cs1    1 25 4.21 5.14 1.03
## cs2    2 25 0.47 4.98 1.00
## cs3    3 25 3.74 7.59 1.52
```

```r
csPlot(cs1, cs2, group = group)
```

```
## $mean
## [1] 5.844099 2.052185
## 
## $mean
## [1] 4.2081441 0.4697145
## 
## $se
## [1] 0.8216307 0.9512185
## 
## $se
## [1] 1.0276958 0.9959088
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
csReport(csCompareObj = tmp)
```

```
## We perfromed a two.sided Welch Two Sample t-test. The results of the t-test are t (47.3848508399421) = 0.026, p = 0.979.
## 
## Those results suggest that there are no statistically significant between group differences. 
## 
## We perfromed a two.sided Bayesian t-test, with a Catchy prior, with its width set to 0.707. The BF10 was equal to BF10 = 0.28. The BF01 was equal to BF01 = 3.54.
## 
## The results suggest that there is no evidence for H1, relative to H0.
## 
## The results suggest that there is anecdotal evidence for H0, relative to H1.
```

```r
tmp <- csSensitivity(cs1, cs2)
csRobustnessPlot(cs1, cs2, group, BF01 = FALSE)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-2.png)

```r
csReport(csSensitivityObj = tmp)
```

```
## We perfromed a Sensitivity Analysis using the scaling factors: medium, wide, ultrawide. The results for BF01 were: 0.02, 0.02, 0.03 respectively. The results for BF10 were: 55.19, 47.03, 37.29 respectively.
```
