<!-- README.md is generated from README.Rmd. Please edit that file -->
GCalcium
========

Calcium imaging methods produce massive datasets that require immense data manipulation, exploration, and summarizing. GCalcium provides highly-accessible functions to address these issues for both inexperienced and seasoned R users to save researchers time. This package is catered to calcium imaging data, but works with any type of waveform data. A few functions include:

-   `format_data` converts a data frame or matrix to a GCalcium-friendly format
-   `avg_curve_slope` gets the average slope of curves for a trial
-   `between_trial_change` finds the difference in mean activity between trials
-   `centered_AUC` finds the area under each curve
-   `moving_window` summarizes data within windows of time

The "Examples" vignette explains all functions in greater detail.

Installation
------------

``` r
### Install from CRAN repository
install.packages("GCalcium")
```

Getting started
---------------

Since there is currently no ubiquitous way to analyze or format calcium imaging data, most of GCalcium's commands require the data frame to be in "GCalcium format." This is essentially a time series data frame; where the times of recorded signals are in the first row or column, and the observed values of each trial are in the following rows or columns.

``` r
### Format data
df.new <- format_data(GCaMP)

### What is the average slope for each curve in trial 1?
cat( avg_curve_slope(Dataframe = df.new, Trial = 1) )
#> NA 2.686828 -5.232394 3.079274 -4.75037 5.301835 -3.920928 -0.1513382 -2.021098 0.1617518 -0.3044619 6.910018 -2.222297 3.505145 -4.423766 6.068415 -3.884568 5.948769 -0.6739143 3.88837 -4.38267 NA 1.690167 -3.259148 1.04693 -1.352179 3.079839 -1.26231 2.280547 -2.764681 2.086012 -2.176606 5.576817 0.9183673 7.01988 -2.683797 2.563535 -4.106713 -1.010101 -5.764288 4.887615 -0.8646999 7.591648 -9.51594 1.778811 -1.007121 4.799507 0.1424286 1.088477 -5.46543 5.243424 -4.167272 3.803906 -0.6307223 0.8850458 -5.676258 -0.2339776 -2.8535 7.08697 -1.74929 -0.508647 -7.278496 3.408955 -6.524187 3.929158 -4.112074 1.066656 -4.287113 4.406864 NA -3.244568 2.916868 -0.5786535 4.053837 -11.21297 5.769475 0.8163265 4.744986 -3.187327 4.494263 -5.132678 10.89168 -10.5337 11.71813 -6.204929 6.66565 -4.533736 4.002959 -3.571172 4.214516 -6.47418 8.999957 -8.60198 9.384561 -5.144969

### How does activity of each curve differ from the average in trial 1?
head( centered_AUC(Dataframe = df.new, Trial = 1, FUN = mean) )
#>   Curve.Num          AUC
#> 1         1           NA
#> 2         2 0.0102761563
#> 3         3 0.0158426888
#> 4         4 0.0142982157
#> 5         5 0.0168281125
#> 6         6 0.0001885507

### What is the average activity for trial 1 in 0.5 second intervals?
head ( moving_window(Dataframe = df.new, Trial = 1, Window.length = 0.5, FUN = mean) )
#>   Summary.vals Start.time Stop.time Window.num
#> 1     82.58849    -3.9902   -3.4902          1
#> 2     82.57888    -3.4902   -2.9902          2
#> 3     82.38037    -2.9902   -2.4902          3
#> 4     82.66410    -2.4902   -1.9902          4
#> 5     82.59971    -1.9902   -1.4902          5
#> 6     82.77696    -1.4902   -0.9902          6

### How much does activity differ in the first second of trials 1 & 2, and 3 & 4?
head( between_trial_change(Dataframe = df.new, TrialRange1 = 1:2, TrialRange2 = 3:4, 
                           Time.period = c(0, 1)) )
#> [1] 3.778916
```
