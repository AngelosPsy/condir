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
## 1 Paired t-test   two.sided 0.9759461 0.3962825   0    0          0
##        LCI      HCI t.statistic df      p.value
## 1 3.321165 7.206703    5.444946 49 1.655344e-06
## 
## $bayes.results
##    LNI HNI rscale     bf10         bf01    propError
## 1 -Inf Inf  0.707 10220.02 9.784721e-05 3.155232e-13
## 
## $descriptives
##     vars  n mean   sd   se
## cs1    1 50 6.11 4.63 0.65
## cs2    2 50 0.84 4.99 0.71
```

```r
csPlot(cs1, cs2, ylab = "CRs")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
csReport(tmp)
```

```
## We perfromed a two.sided Paired t-test. The results of the t-test are t (49) = 5.445, p = 0. We perfromed a two.sidedBayesian t-test, with a Catchy prior, with its width set to 0.707. The BF10 was equal to BF10 = 10220.02. The BF01 was equal to BF01 = 0.
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
## We perfromed a Sensitivity Analysis using the scaling factors: medium, wide, ultrawide. The results for BF01 were: 0, 0, 0 respectively. The results for BF10 were: 10220.05, 9747.77, 8412.46 respectively.
```

### Two groups example

```r
group <- rep(1:2, 25)
tmp <- csCompare(cs1, cs2, group)
tmp
```

```
## $freq.results
##                    method alternative       WG1      WpG1       WG2
## 1 Welch Two Sample t-test   two.sided 0.9758862 0.7934724 0.9230818
##         WpG2 null.value       LCI      HCI t.statistic       df   p.value
## 1 0.06024697          0 -3.887943 3.970757   0.0211958 47.30325 0.9831786
## 
## $bayes.results
##    LNI HNI rscale      bf10     bf01    propError
## 1 -Inf Inf  0.707 0.2827494 3.536701 0.0001939461
## 
## $descriptives
## group: 1
##     vars  n mean   sd   se
## cs1    1 25 5.74 4.00 0.80
## cs2    2 25 0.46 5.77 1.15
## cs3    3 25 5.28 7.31 1.46
## -------------------------------------------------------- 
## group: 2
##     vars  n mean   sd   se
## cs1    1 25 6.48 5.24 1.05
## cs2    2 25 1.23 4.15 0.83
## cs3    3 25 5.24 6.47 1.29
```

```r
csPlot(cs1, cs2, group = group)
```

```
## $mean
## [1] 5.7396482 0.4550111
## 
## $mean
## [1] 6.477328 1.234098
## 
## $se
## [1] 0.8006104 1.1548124
## 
## $se
## [1] 1.0472457 0.8294176
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
csReport(csCompareObj = tmp)
```

```
## We perfromed a two.sided Welch Two Sample t-test. The results of the t-test are t (47.3032504092779) = 0.021, p = 0.983. We perfromed a two.sidedBayesian t-test, with a Catchy prior, with its width set to 0.707. The BF10 was equal to BF10 = 0.28. The BF01 was equal to BF01 = 3.54.
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
## We perfromed a Sensitivity Analysis using the scaling factors: medium, wide, ultrawide. The results for BF01 were: 0, 0, 0 respectively. The results for BF10 were: 10220.05, 9747.77, 8412.46 respectively.
```
