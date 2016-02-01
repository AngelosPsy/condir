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
##          method alternative       WG1      WpG1 WG2 WpG2 null.value
## 1 Paired t-test   two.sided 0.9896696 0.9381634   0    0          0
##        LCI      HCI t.statistic df      p.value
## 1 2.132716 5.710508    4.405385 49 5.735482e-05
## 
## $bayes.results
##    LNI HNI rscale     bf10        bf01    propError
## 1 -Inf Inf  0.707 381.8028 0.002619153 4.578698e-11
## 
## $descriptives
##     vars  n mean   sd   se
## cs1    1 50 4.57 4.62 0.65
## cs2    2 50 0.65 5.51 0.78
```

```r
csPlot(cs1, cs2, ylab = "CRs")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
csReport(tmp)
```

```
## We perfromed a two.sided Paired t-test. The results of the t-test are t (49) = 4.405, p = 0.
## 
## Those results suggest that there are statistically significant differences between cs1 and cs2. 
## 
## We perfromed a two.sided Bayesian t-test, with a Catchy prior, with its width set to 0.707. The BF10 was equal to BF10 = 381.8. The BF01 was equal to BF01 = 0.
## 
## The results suggest that there is decisive evidence for H1, relative to H0.
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
## We perfromed a Sensitivity Analysis using the scaling factors: medium, wide, ultrawide. The results for BF01 were: 0, 0, 0 respectively. The results for BF10 were: 381.79, 340.41, 278.81 respectively.
```

### Two groups example

```r
group <- rep(1:2, 25)
tmp <- csCompare(cs1, cs2, group)
tmp
```

```
## $freq.results
##                    method alternative      WG1      WpG1       WG2
## 1 Welch Two Sample t-test   two.sided 0.982407 0.9282729 0.9799842
##        WpG2 null.value       LCI       HCI t.statistic       df
## 1 0.8847949          0 -8.222973 -1.568675   -2.959618 47.37099
##       p.value
## 1 0.004797914
## 
## $bayes.results
##    LNI HNI rscale    bf10      bf01    propError
## 1 -Inf Inf  0.707 8.68352 0.1151607 2.438781e-06
## 
## $descriptives
## group: 1
##     vars  n mean   sd   se
## cs1    1 25 3.22 5.30 1.06
## cs2    2 25 1.75 5.45 1.09
## cs3    3 25 1.47 5.50 1.10
## -------------------------------------------------------- 
## group: 2
##     vars  n  mean   sd   se
## cs1    1 25  5.92 3.43 0.69
## cs2    2 25 -0.45 5.45 1.09
## cs3    3 25  6.37 6.18 1.24
```

```r
csPlot(cs1, cs2, group = group)
```

```
## $mean
## [1] 3.219989 1.746289
## 
## $mean
## [1]  5.915800 -0.453724
## 
## $se
## [1] 1.059670 1.089827
## 
## $se
## [1] 0.6858462 1.0902018
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
csReport(csCompareObj = tmp)
```

```
## We perfromed a two.sided Welch Two Sample t-test. The results of the t-test are t (47.3709854349633) = -2.96, p = 0.005.
## 
## Those results suggest that there are statistically significant between group differences. 
## 
## We perfromed a two.sided Bayesian t-test, with a Catchy prior, with its width set to 0.707. The BF10 was equal to BF10 = 8.68. The BF01 was equal to BF01 = 0.12.
## 
## The results suggest that there is substantial evidence for H1, relative to H0.
## 
## The results suggest that there is no evidence for H0, relative to H1.
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
## We perfromed a Sensitivity Analysis using the scaling factors: medium, wide, ultrawide. The results for BF01 were: 0, 0, 0 respectively. The results for BF10 were: 381.79, 340.41, 278.81 respectively.
```
