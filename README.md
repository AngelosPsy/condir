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
## 1 Paired t-test   two.sided 0.9742282 0.3408645   0    0          0
##        LCI      HCI t.statistic df      p.value
## 1 2.550717 6.238683     4.78935 49 1.586805e-05
## 
## $bayes.results
##    LNI HNI rscale     bf10         bf01   propError
## 1 -Inf Inf  0.707 1248.386 0.0008010345 4.86197e-12
## 
## $descriptives
##     vars  n mean   sd   se
## cs1    1 50 6.25 5.16 0.73
## cs2    2 50 1.85 5.59 0.79
```

```r
csPlot(cs1, cs2, ylab = "CRs")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
csReport(tmp)
```

```
## We perfromed a two.sided Paired t-test. The results of the t-test are t (49) = 4.789, p = 0.
## 
## Those results suggest that there are statistically significant differences between cs1 and cs2,
##         for an alpha level of 0.05. 
## 
## We perfromed a two.sided Bayesian t-test, with a Catchy prior, with its width set to 0.707. The BF10 was equal to BF10 = 1248.39. The BF01 was equal to BF01 = 0.
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
## We perfromed a Sensitivity Analysis using the scaling factors: medium, wide, ultrawide. The results for BF01 were: 0, 0, 0 respectively. The results for BF10 were: 1248.37, 1141.95, 953.47 respectively.
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
## 1 Welch Two Sample t-test   two.sided 0.952325 0.2826903 0.9193036
##         WpG2 null.value       LCI      HCI t.statistic       df   p.value
## 1 0.04939716          0 -3.056895 4.391331   0.3603249 47.50854 0.7202013
## 
## $bayes.results
##    LNI HNI rscale      bf10     bf01    propError
## 1 -Inf Inf  0.707 0.2982118 3.353321 0.0001933792
## 
## $descriptives
## group: 1
##     vars  n mean   sd   se
## cs1    1 25 7.04 5.41 1.08
## cs2    2 25 2.31 5.92 1.18
## cs3    3 25 4.73 6.20 1.24
## -------------------------------------------------------- 
## group: 2
##     vars  n mean   sd   se
## cs1    1 25 5.46 4.87 0.97
## cs2    2 25 1.39 5.31 1.06
## cs3    3 25 4.06 6.87 1.37
```

```r
csPlot(cs1, cs2, group = group)
```

```
## $mean
## [1] 7.041945 2.313635
## 
## $mean
## [1] 5.455845 1.394754
## 
## $se
## [1] 1.082811 1.184935
## 
## $se
## [1] 0.9746044 1.0619938
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
csReport(csCompareObj = tmp)
```

```
## We perfromed a two.sided Welch Two Sample t-test. The results of the t-test are t (47.509) = 0.36, p = 0.72.
## 
## Those results suggest that there are no statistically significant between group differences,
##         for an alpha level of 0.05. 
## 
## We perfromed a two.sided Bayesian t-test, with a Catchy prior, with its width set to 0.707. The BF10 was equal to BF10 = 0.3. The BF01 was equal to BF01 = 3.35.
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
## We perfromed a Sensitivity Analysis using the scaling factors: medium, wide, ultrawide. The results for BF01 were: 0, 0, 0 respectively. The results for BF10 were: 1248.37, 1141.95, 953.47 respectively.
```
