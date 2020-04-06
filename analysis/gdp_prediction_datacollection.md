GDP Prediction
================
Nikhil Gupta
2020-04-05 20:57:43

-   [Setup](#setup)
-   [GDP Growth (Seasonally Adjusted)](#gdp-growth-seasonally-adjusted)
    -   [Observations](#observations)
    -   [Fits](#fits)
        -   [Pure ARMA fit](#pure-arma-fit)
            -   [Observations](#observations-1)
        -   [ARIMA (no seasonality)](#arima-no-seasonality)
            -   [Observations](#observations-2)
        -   [ARIMA with d = 0 and Seasonality != 0](#arima-with-d-0-and-seasonality-0)
            -   [Observations](#observations-3)
    -   [Predictions](#predictions)
        -   [Pure ARMA](#pure-arma)
        -   [ARIMA()](#arima)
-   [Conclusion](#conclusion)
-   [Collecting more correlated data](#collecting-more-correlated-data)
    -   [GDP](#gdp)
    -   [LABOR MARKET](#labor-market)
        -   [Unemployment Rate](#unemployment-rate)
        -   [Non Farm Jobs (Seasonally Adjusted)](#non-farm-jobs-seasonally-adjusted)
    -   [MONITORY POLICY](#monitory-policy)
        -   [10 yr Treasury Rate](#yr-treasury-rate)
        -   [3 mo Treasury Rate](#mo-treasury-rate)
        -   [10yr - 3mo Treasury](#yr---3mo-treasury)
        -   [Federal Funds Interest Rate](#federal-funds-interest-rate)
        -   [3 month LIBOR](#month-libor)
    -   [CONSUMER RELATED](#consumer-related)
        -   [Real Disposable Personal Income Change](#real-disposable-personal-income-change)
        -   [Consumer Price Index (inflation)](#consumer-price-index-inflation)
        -   [Population](#population)
    -   [HOUSING RELATED](#housing-related)
        -   [Permits](#permits)
        -   [Housing Price Index](#housing-price-index)
        -   [Home Ownership](#home-ownership)
    -   [BUSINESS ENVIRONMENT](#business-environment)
        -   [Corporate Profits](#corporate-profits)
        -   [Inventories](#inventories)
        -   [Domestic Investments](#domestic-investments)
        -   [Crude Oil](#crude-oil)
        -   [Producer Price Index](#producer-price-index)
        -   [Industrial Production Index](#industrial-production-index)
    -   [STOCK MARKER](#stock-marker)
        -   [Willshire 5000](#willshire-5000)
    -   [MACRO ECONOMIC FACTORS](#macro-economic-factors)
        -   [Gold](#gold)
        -   [Exchange Rate](#exchange-rate)
            -   [China](#china)
            -   [Japan](#japan)
            -   [UK](#uk)
-   [Merge Data](#merge-data)
-   [TODO](#todo)
    -   [Questions for Dr. Sadler](#questions-for-dr.-sadler)
    -   [Things to Consider](#things-to-consider)
    -   [Data Collection](#data-collection)
    -   [EDA](#eda)
    -   [Model Analysis](#model-analysis)

Setup
=====

``` r
library(tswge)
```

    ## Warning: package 'tswge' was built under R version 3.5.3

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.5.3

    ## -- Attaching packages --------------------------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.3.0     v purrr   0.3.3
    ## v tibble  2.1.3     v dplyr   0.8.5
    ## v tidyr   0.8.3     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## Warning: package 'ggplot2' was built under R version 3.5.3

    ## Warning: package 'tibble' was built under R version 3.5.3

    ## Warning: package 'tidyr' was built under R version 3.5.3

    ## Warning: package 'readr' was built under R version 3.5.2

    ## Warning: package 'purrr' was built under R version 3.5.3

    ## Warning: package 'dplyr' was built under R version 3.5.3

    ## Warning: package 'stringr' was built under R version 3.5.3

    ## Warning: package 'forcats' was built under R version 3.5.3

    ## -- Conflicts ------------------------------------------------------------------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(tidyquant)
```

    ## Warning: package 'tidyquant' was built under R version 3.5.3

    ## Loading required package: lubridate

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

    ## Loading required package: PerformanceAnalytics

    ## Warning: package 'PerformanceAnalytics' was built under R version 3.5.3

    ## Loading required package: xts

    ## Warning: package 'xts' was built under R version 3.5.2

    ## Loading required package: zoo

    ## Warning: package 'zoo' was built under R version 3.5.3

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## 
    ## Attaching package: 'xts'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     first, last

    ## 
    ## Attaching package: 'PerformanceAnalytics'

    ## The following object is masked from 'package:graphics':
    ## 
    ##     legend

    ## Loading required package: quantmod

    ## Warning: package 'quantmod' was built under R version 3.5.3

    ## Loading required package: TTR

    ## Warning: package 'TTR' was built under R version 3.5.2

    ## Version 0.4-0 included new data defaults. See ?getSymbols.

``` r
library(skimr)
```

    ## Warning: package 'skimr' was built under R version 3.5.3

    ## 
    ## Attaching package: 'skimr'

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

``` r
library(plyr)
```

    ## Warning: package 'plyr' was built under R version 3.5.3

    ## -------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## -------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following object is masked from 'package:lubridate':
    ## 
    ##     here

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following object is masked from 'package:purrr':
    ## 
    ##     compact

``` r
library(ggplot2)
source("common_functions_tq.R")
```

``` r
from = "1970-01-01"
to = "2019-09-30"
```

GDP Growth (Seasonally Adjusted)
================================

``` r
ID = "A191RP1Q027SBEA"
data = tqw_get_fred(ID = ID, from = from, to = to)
data %>% glimpse()
```

    ## Observations: 199
    ## Variables: 2
    ## $ date  <date> 1970-01-01, 1970-04-01, 1970-07-01, 1970-10-01, 1971-01...
    ## $ price <dbl> 5.1, 6.3, 7.2, 0.9, 18.2, 7.7, 7.6, 4.4, 14.3, 12.1, 7.9...

``` r
data = data %>%    
    tq_transmute(mutate_fun = to.period,
                 period = "quarters", 
                 col_rename = "gdp_change")
data %>% glimpse()
```

    ## Observations: 199
    ## Variables: 2
    ## $ date       <date> 1970-01-01, 1970-04-01, 1970-07-01, 1970-10-01, 19...
    ## $ gdp_change <dbl> 5.1, 6.3, 7.2, 0.9, 18.2, 7.7, 7.6, 4.4, 14.3, 12.1...

``` r
data %>%
    ggplot(aes(x = date, y = gdp_change)) +
    geom_line() +
    #labs(title = title, y = y_label, x = x_label) + 
    theme_tq()
```

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
px = plotts.sample.wge(data$gdp_change, lag.max = 40)
```

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-6-1.png)

Observations
------------

-   Looks like there are several peaks, but if we note carefully, the magnitude is less than 0.
-   Peak at f = 0 is the most appreciable --&gt; so this could come from an ARMA, ARIMA (no seasonality) or an ARIMA (with seasonality model)
-   ACF's have not died down even after lag of at least 32 (8 years!!) --&gt; some indication of extended autocorrelations pointing to a ARIMA model (--&gt; this is using older data)

Let's see various fits to see.

Fits
----

### Pure ARMA fit

``` r
grid = aic5.wge(data$gdp_change)
```

    ## ---------WORKING... PLEASE WAIT... 
    ## 
    ## 
    ## Five Smallest Values of  aic

``` r
grid 
```

    ##       p    q        aic
    ## 8     2    1   2.452372
    ## 11    3    1   2.452735
    ## 6     1    2   2.456483
    ## 5     1    1   2.459303
    ## 14    4    1   2.462603

``` r
arma_fit = aic.wge(data$gdp_change)

factor.wge(phi = arma_fit$phi)
```

    ## 
    ## Coefficients of Original polynomial:  
    ## 1.1563 -0.1704 
    ## 
    ## Factor                 Roots                Abs Recip    System Freq 
    ## 1-0.9829B              1.0174               0.9829       0.0000
    ## 1-0.1733B              5.7690               0.1733       0.0000
    ##   
    ## 

``` r
factor.wge(phi = arma_fit$theta)
```

    ## 
    ## Coefficients of Original polynomial:  
    ## 0.8891 
    ## 
    ## Factor                 Roots                Abs Recip    System Freq 
    ## 1-0.8891B              1.1247               0.8891       0.0000
    ##   
    ## 

#### Observations

-   This solves a lot of the open questions
-   The factor table for the fitted phi's shows that the value of the dominant root is very close to 1 hence the realization is displaying ARIMA type of properties.

### ARIMA (no seasonality)

``` r
results = tribble(~d, ~best_p, ~best_q, ~best_aic)


data_diff = artrans.wge(data$gdp_change, phi.tr = 1, plottr = TRUE)
```

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
# px = plotts.sample.wge(data_no_season)
grid = aic5.wge(data_diff)
```

    ## ---------WORKING... PLEASE WAIT... 
    ## 
    ## 
    ## Five Smallest Values of  aic

``` r
best_aic = grid$`       aic`[1]
best_p = grid$`   p`[1]
best_q = grid$`   q`[1]
  
results = results %>% 
  add_row(d = 1, best_p = best_p, best_q = best_q, best_aic = best_aic)

results %>% 
  arrange(best_aic)
```

    ## # A tibble: 1 x 4
    ##       d best_p best_q best_aic
    ##   <dbl>  <dbl>  <dbl>    <dbl>
    ## 1     1      0      2     2.45

``` r
px = plotts.sample.wge(data_diff)
```

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
arima_fit = aic.wge(data_diff)

if (arima_fit$p != 0){ factor.wge(phi = arima_fit$phi)   }
if (arima_fit$q != 0){ factor.wge(phi = arima_fit$theta) }
```

    ## 
    ## Coefficients of Original polynomial:  
    ## 0.7688 0.1479 
    ## 
    ## Factor                 Roots                Abs Recip    System Freq 
    ## 1-0.9281B              1.0774               0.9281       0.0000
    ## 1+0.1593B             -6.2759               0.1593       0.5000
    ##   
    ## 

#### Observations

-   Best AIC is for ARIMA(0,1,2) with s = 0.
-   Best AIC = 2.444 which is very close to the pure ARMA model since that has a dominant root very close to 1 anyway.

### ARIMA with d = 0 and Seasonality != 0

``` r
results = tribble(~seasonality, ~best_p, ~best_q, ~best_aic)

for (s in 2:12){
  # Note when s = 1, this returns a ARIMA model with d = 1 and no seasonality
  # For s > 1, this returns a ARIMA model with d = 0 and seasonality = s
  data_no_season = artrans.wge(data$gdp_change, phi.tr = c(rep(0, s-1),1), plottr = TRUE)
  # px = plotts.sample.wge(data_no_season)
  grid = aic5.wge(data_no_season)
  best_aic = grid$`       aic`[1]
  best_p = grid$`   p`[1]
  best_q = grid$`   q`[1]
  
  results = results %>% 
    add_row(seasonality = s, best_p = best_p, best_q = best_q, best_aic = best_aic)
}
```

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-11-1.png)

    ## ---------WORKING... PLEASE WAIT... 
    ## 
    ## 
    ## Five Smallest Values of  aic

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-11-2.png)

    ## ---------WORKING... PLEASE WAIT... 
    ## 
    ## 
    ## Five Smallest Values of  aic

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-11-3.png)

    ## ---------WORKING... PLEASE WAIT... 
    ## 
    ## 
    ## Five Smallest Values of  aic

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-11-4.png)

    ## ---------WORKING... PLEASE WAIT... 
    ## 
    ## 
    ## Five Smallest Values of  aic

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-11-5.png)

    ## ---------WORKING... PLEASE WAIT... 
    ## 
    ## 
    ## Five Smallest Values of  aic

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-11-6.png)

    ## ---------WORKING... PLEASE WAIT... 
    ## 
    ## 
    ## Five Smallest Values of  aic

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-11-7.png)

    ## ---------WORKING... PLEASE WAIT... 
    ## 
    ## 
    ## Five Smallest Values of  aic

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-11-8.png)

    ## ---------WORKING... PLEASE WAIT... 
    ## 
    ## 
    ## Five Smallest Values of  aic

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-11-9.png)

    ## ---------WORKING... PLEASE WAIT... 
    ## 
    ## 
    ## Five Smallest Values of  aic

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-11-10.png)

    ## ---------WORKING... PLEASE WAIT... 
    ## 
    ## 
    ## Five Smallest Values of  aic

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-11-11.png)

    ## ---------WORKING... PLEASE WAIT... 
    ## 
    ## 
    ## Five Smallest Values of  aic

``` r
results %>% 
  arrange(best_aic)
```

    ## # A tibble: 11 x 4
    ##    seasonality best_p best_q best_aic
    ##          <int>  <dbl>  <dbl>    <dbl>
    ##  1           2      4      2     2.55
    ##  2           5      5      2     2.67
    ##  3           3      5      1     2.71
    ##  4          11      4      2     2.77
    ##  5           9      3      2     2.86
    ##  6          10      3      1     2.87
    ##  7           4      4      0     2.87
    ##  8           6      4      1     2.91
    ##  9           7      2      2     2.92
    ## 10           8      5      2     3.15
    ## 11          12      2      1     3.24

#### Observations

-   Best Model including Seasonality is ARIMA(5,0,2) with s = 2
-   However, this is not as good as the pure ARMA model or the ARIMA with no seasonality model.
-   We will exclude this from the final consideration.

Predictions
-----------

``` r
n.ahead = 4

compute_ase = function(data, forecast, n.ahead){
  n = length(data)
  ase = mean((data[(n-n.ahead+1):n] - forecast$f)^2)
  return(ase)
}

results = tribble(~model, ~ase)
```

### Pure ARMA

``` r
model = "ARMA"
fore_arma = fore.arma.wge(data$gdp_change, phi = arma_fit$phi, theta = arma_fit$theta, n.ahead = n.ahead, lastn = TRUE)
```

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
ase_arma = compute_ase(data = data$gdp_change, forecast = fore_arma, n.ahead = n.ahead)

results = results %>% 
    add_row(model = model, ase = ase_arma)
```

### ARIMA()

``` r
model = "ARIMA"
fore_arima = fore.aruma.wge(data$gdp_change, phi = arima_fit$phi, theta = arima_fit$theta, n.ahead = n.ahead, lastn = TRUE) 
```

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
ase_arima = compute_ase(data = data$gdp_change, forecast = fore_arima, n.ahead = n.ahead)

results = results %>% 
    add_row(model = model, ase = ase_arima)
```

``` r
results
```

    ## # A tibble: 2 x 2
    ##   model    ase
    ##   <chr>  <dbl>
    ## 1 ARMA    1.47
    ## 2 ARIMA 184.

Conclusion
==========

-   ARMA(2,1) and ARIMA(0,1,2) seem to give similar results based on AIC
-   However, ARIMA fails terribly at predicton and the ASE is huge especially for immediate predictons.
-   Hence we would recommend using the ARMA model in this case.
-   This model only takes 'time' into consideration while building the model. There may be other exogenous variables that may impact GDP which we have not considered at all. We will do that next with VAR models and Neural Networks.

Collecting more correlated data
===============================

``` r
aggregate.period = "quarters"
index.at = "yearqtr"
from = "1900-01-01"  ## Going to increase the horizon since we need more data for NN models
to = "2019-09-30"
```

GDP
---

``` r
gdp_change = get_format_tq(ID = "A191RP1Q027SBEA", from = from, to = to, col_rename = 'gdp_change', return = FALSE)
```

    ## 
    ## 
    ## Initial data pull
    ## 
    ## Observations: 290
    ## Variables: 2
    ## $ date  <date> 1947-04-01, 1947-07-01, 1947-10-01, 1948-01-01, 1948-04...
    ## $ price <dbl> 4.7, 6.0, 17.3, 9.6, 10.7, 10.1, 1.7, -7.4, -5.2, 2.3, -...
    ## 
    ## 
    ## Final Data
    ## 
    ## Observations: 290
    ## Variables: 2
    ## $ date       <yearqtr> 1947 Q2, 1947 Q3, 1947 Q4, 1948 Q1, 1948 Q2, 19...
    ## $ gdp_change <dbl> 4.7, 6.0, 17.3, 9.6, 10.7, 10.1, 1.7, -7.4, -5.2, 2...

    ## Don't know how to automatically pick scale for object of type yearqtr. Defaulting to continuous.

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-17-1.png)

LABOR MARKET
------------

### Unemployment Rate

``` r
unrate = get_format_tq(ID = "UNRATE", from = from, to = to, col_rename = 'unrate', return = FALSE)
```

    ## 
    ## 
    ## Initial data pull
    ## 
    ## Observations: 861
    ## Variables: 2
    ## $ date  <date> 1948-01-01, 1948-02-01, 1948-03-01, 1948-04-01, 1948-05...
    ## $ price <dbl> 3.4, 3.8, 4.0, 3.9, 3.5, 3.6, 3.6, 3.9, 3.8, 3.7, 3.8, 4...
    ## 
    ## 
    ## Final Data
    ## 
    ## Observations: 287
    ## Variables: 2
    ## $ date   <yearqtr> 1948 Q1, 1948 Q2, 1948 Q3, 1948 Q4, 1949 Q1, 1949 Q...
    ## $ unrate <dbl> 4.0, 3.6, 3.8, 4.0, 5.0, 6.2, 6.6, 6.6, 6.3, 5.4, 4.4, ...

    ## Don't know how to automatically pick scale for object of type yearqtr. Defaulting to continuous.

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-18-1.png)

### Non Farm Jobs (Seasonally Adjusted)

``` r
nfjobschg = get_format_tq(ID = "PAYEMS", from = from, to = to, col_rename = 'nfjobschg', return = TRUE)
```

    ## 
    ## 
    ## Initial data pull
    ## 
    ## Observations: 969
    ## Variables: 2
    ## $ date  <date> 1939-01-01, 1939-02-01, 1939-03-01, 1939-04-01, 1939-05...
    ## $ price <int> 29923, 30100, 30280, 30094, 30299, 30502, 30419, 30663, ...
    ## 
    ## 
    ## Data after calculating period return
    ## 
    ## Observations: 323
    ## Variables: 2
    ## $ date  <date> 1939-03-01, 1939-06-01, 1939-09-01, 1939-12-01, 1940-03...
    ## $ price <dbl> 1.1930622, 0.7331572, 1.7343125, 1.6467404, 0.8972164, 0...
    ## 
    ## 
    ## Final Data
    ## 
    ## Observations: 323
    ## Variables: 2
    ## $ date      <yearqtr> 1939 Q1, 1939 Q2, 1939 Q3, 1939 Q4, 1940 Q1, 194...
    ## $ nfjobschg <dbl> 1.1930622, 0.7331572, 1.7343125, 1.6467404, 0.897216...

    ## Don't know how to automatically pick scale for object of type yearqtr. Defaulting to continuous.

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-19-1.png)

MONITORY POLICY
---------------

### 10 yr Treasury Rate

``` r
treas10yr = get_format_tq(ID = "DGS10", from = from, to = to, col_rename = 'treas10yr', return = FALSE)
```

    ## 
    ## 
    ## Initial data pull
    ## 
    ## Observations: 15,065
    ## Variables: 2
    ## $ date  <date> 1962-01-02, 1962-01-03, 1962-01-04, 1962-01-05, 1962-01...
    ## $ price <dbl> 4.06, 4.03, 3.99, 4.02, 4.03, 4.05, 4.07, 4.08, 4.08, 4....

    ## Warning in fun_transmute(., OHLC = FALSE, ...): missing values removed from
    ## data

    ## 
    ## 
    ## Final Data
    ## 
    ## Observations: 231
    ## Variables: 2
    ## $ date      <yearqtr> 1962 Q1, 1962 Q2, 1962 Q3, 1962 Q4, 1963 Q1, 196...
    ## $ treas10yr <dbl> 3.86, 4.00, 3.94, 3.85, 3.95, 4.00, 4.07, 4.14, 4.23...

    ## Don't know how to automatically pick scale for object of type yearqtr. Defaulting to continuous.

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-20-1.png)

### 3 mo Treasury Rate

``` r
treas3mo = get_format_tq(ID = "DGS3MO", from = from, to = to, col_rename = 'treas3mo', return = FALSE)
```

    ## 
    ## 
    ## Initial data pull
    ## 
    ## Observations: 9,846
    ## Variables: 2
    ## $ date  <date> 1982-01-04, 1982-01-05, 1982-01-06, 1982-01-07, 1982-01...
    ## $ price <dbl> 11.87, 12.20, 12.16, 12.17, 11.98, 12.49, 12.52, 12.70, ...

    ## Warning in fun_transmute(., OHLC = FALSE, ...): missing values removed from
    ## data

    ## 
    ## 
    ## Final Data
    ## 
    ## Observations: 151
    ## Variables: 2
    ## $ date     <yearqtr> 1982 Q1, 1982 Q2, 1982 Q3, 1982 Q4, 1983 Q1, 1983...
    ## $ treas3mo <dbl> 13.99, 13.36, 7.88, 8.20, 8.96, 9.15, 9.04, 9.33, 10....

    ## Don't know how to automatically pick scale for object of type yearqtr. Defaulting to continuous.

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-21-1.png)

### 10yr - 3mo Treasury

``` r
treas10yr3mo = get_format_tq(ID = "T10Y3M", from = from, to = to, col_rename = 'treas10yr3mo', return = FALSE)
```

    ## 
    ## 
    ## Initial data pull
    ## 
    ## Observations: 9,846
    ## Variables: 2
    ## $ date  <date> 1982-01-04, 1982-01-05, 1982-01-06, 1982-01-07, 1982-01...
    ## $ price <dbl> 2.32, 2.24, 2.43, 2.46, 2.50, 2.32, 2.10, 2.13, 1.53, 2....

    ## Warning in fun_transmute(., OHLC = FALSE, ...): missing values removed from
    ## data

    ## 
    ## 
    ## Final Data
    ## 
    ## Observations: 151
    ## Variables: 2
    ## $ date         <yearqtr> 1982 Q1, 1982 Q2, 1982 Q3, 1982 Q4, 1983 Q1, ...
    ## $ treas10yr3mo <dbl> 0.19, 1.08, 3.85, 2.16, 1.66, 1.81, 2.40, 2.49, 2...

    ## Don't know how to automatically pick scale for object of type yearqtr. Defaulting to continuous.

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-22-1.png)

### Federal Funds Interest Rate

``` r
fedintrate = get_format_tq(ID = "FEDFUNDS", from = from, to = to, col_rename = 'fedintrate', return = FALSE)
```

    ## 
    ## 
    ## Initial data pull
    ## 
    ## Observations: 783
    ## Variables: 2
    ## $ date  <date> 1954-07-01, 1954-08-01, 1954-09-01, 1954-10-01, 1954-11...
    ## $ price <dbl> 0.80, 1.22, 1.06, 0.85, 0.83, 1.28, 1.39, 1.29, 1.35, 1....
    ## 
    ## 
    ## Final Data
    ## 
    ## Observations: 261
    ## Variables: 2
    ## $ date       <yearqtr> 1954 Q3, 1954 Q4, 1955 Q1, 1955 Q2, 1955 Q3, 19...
    ## $ fedintrate <dbl> 1.06, 1.28, 1.35, 1.64, 2.18, 2.48, 2.50, 2.71, 2.9...

    ## Don't know how to automatically pick scale for object of type yearqtr. Defaulting to continuous.

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-23-1.png)

### 3 month LIBOR

``` r
libor3mo = get_format_tq(ID = "USD3MTD156N", from = from, to = to, col_rename = 'libor3mo', return = FALSE)
```

    ## 
    ## 
    ## Initial data pull
    ## 
    ## Observations: 8,803
    ## Variables: 2
    ## $ date  <date> 1986-01-02, 1986-01-03, 1986-01-06, 1986-01-07, 1986-01...
    ## $ price <dbl> 8.0000, 8.0625, 8.1250, 8.1250, 8.0000, 8.1875, 8.2500, ...

    ## Warning in fun_transmute(., OHLC = FALSE, ...): missing values removed from
    ## data

    ## 
    ## 
    ## Final Data
    ## 
    ## Observations: 135
    ## Variables: 2
    ## $ date     <yearqtr> 1986 Q1, 1986 Q2, 1986 Q3, 1986 Q4, 1987 Q1, 1987...
    ## $ libor3mo <dbl> 7.43750, 6.87500, 6.12500, 6.43750, 6.68750, 7.18750,...

    ## Don't know how to automatically pick scale for object of type yearqtr. Defaulting to continuous.

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-24-1.png)

CONSUMER RELATED
----------------

### Real Disposable Personal Income Change

``` r
personincomechg = get_format_tq(ID = "A067RO1Q156NBEA", from = from, to = to, col_rename = 'personincomechg', return = FALSE)
```

    ## 
    ## 
    ## Initial data pull
    ## 
    ## Observations: 287
    ## Variables: 2
    ## $ date  <date> 1948-01-01, 1948-04-01, 1948-07-01, 1948-10-01, 1949-01...
    ## $ price <dbl> 2.1, 6.3, 5.2, 7.3, 3.3, 0.7, -0.3, -0.3, 9.6, 8.7, 9.4,...
    ## 
    ## 
    ## Final Data
    ## 
    ## Observations: 287
    ## Variables: 2
    ## $ date            <yearqtr> 1948 Q1, 1948 Q2, 1948 Q3, 1948 Q4, 1949 Q...
    ## $ personincomechg <dbl> 2.1, 6.3, 5.2, 7.3, 3.3, 0.7, -0.3, -0.3, 9.6,...

    ## Don't know how to automatically pick scale for object of type yearqtr. Defaulting to continuous.

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-25-1.png)

### Consumer Price Index (inflation)

``` r
cpichg = get_format_tq(ID = "CPIAUCNS", from = from, to = to, col_rename = 'cpichg', return = TRUE)
```

    ## 
    ## 
    ## Initial data pull
    ## 
    ## Observations: 1,281
    ## Variables: 2
    ## $ date  <date> 1913-01-01, 1913-02-01, 1913-03-01, 1913-04-01, 1913-05...
    ## $ price <dbl> 9.8, 9.8, 9.8, 9.8, 9.7, 9.8, 9.9, 9.9, 10.0, 10.0, 10.1...
    ## 
    ## 
    ## Data after calculating period return
    ## 
    ## Observations: 427
    ## Variables: 2
    ## $ date  <date> 1913-03-01, 1913-06-01, 1913-09-01, 1913-12-01, 1914-03...
    ## $ price <dbl> 0.0000000, 0.0000000, 2.0408163, 0.0000000, -1.0000000, ...
    ## 
    ## 
    ## Final Data
    ## 
    ## Observations: 427
    ## Variables: 2
    ## $ date   <yearqtr> 1913 Q1, 1913 Q2, 1913 Q3, 1913 Q4, 1914 Q1, 1914 Q...
    ## $ cpichg <dbl> 0.0000000, 0.0000000, 2.0408163, 0.0000000, -1.0000000,...

    ## Don't know how to automatically pick scale for object of type yearqtr. Defaulting to continuous.

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-26-1.png)

### Population

``` r
popchg = get_format_tq(ID = "POPTHM", from = from, to = to, col_rename = 'popchg', return = TRUE)
```

    ## 
    ## 
    ## Initial data pull
    ## 
    ## Observations: 729
    ## Variables: 2
    ## $ date  <date> 1959-01-01, 1959-02-01, 1959-03-01, 1959-04-01, 1959-05...
    ## $ price <int> 175818, 176044, 176274, 176503, 176723, 176954, 177208, ...
    ## 
    ## 
    ## Data after calculating period return
    ## 
    ## Observations: 243
    ## Variables: 2
    ## $ date  <date> 1959-03-01, 1959-06-01, 1959-09-01, 1959-12-01, 1960-03...
    ## $ price <dbl> 0.2593591, 0.3857631, 0.4526600, 0.4213665, 0.6739345, 0...
    ## 
    ## 
    ## Final Data
    ## 
    ## Observations: 243
    ## Variables: 2
    ## $ date   <yearqtr> 1959 Q1, 1959 Q2, 1959 Q3, 1959 Q4, 1960 Q1, 1960 Q...
    ## $ popchg <dbl> 0.2593591, 0.3857631, 0.4526600, 0.4213665, 0.6739345, ...

    ## Don't know how to automatically pick scale for object of type yearqtr. Defaulting to continuous.

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-27-1.png)

HOUSING RELATED
---------------

### Permits

``` r
housingpermitschg = get_format_tq(ID = "PERMIT", from = from, to = to, col_rename = 'housingpermitschg', return = TRUE)
```

    ## 
    ## 
    ## Initial data pull
    ## 
    ## Observations: 717
    ## Variables: 2
    ## $ date  <date> 1960-01-01, 1960-02-01, 1960-03-01, 1960-04-01, 1960-05...
    ## $ price <int> 1092, 1088, 955, 1016, 1052, 958, 999, 994, 984, 972, 97...
    ## 
    ## 
    ## Data after calculating period return
    ## 
    ## Observations: 239
    ## Variables: 2
    ## $ date  <date> 1960-03-01, 1960-06-01, 1960-09-01, 1960-12-01, 1961-03...
    ## $ price <dbl> -12.5457875, 0.3141361, 2.7139875, -3.3536585, 5.1524711...
    ## 
    ## 
    ## Final Data
    ## 
    ## Observations: 239
    ## Variables: 2
    ## $ date              <yearqtr> 1960 Q1, 1960 Q2, 1960 Q3, 1960 Q4, 1961...
    ## $ housingpermitschg <dbl> -12.5457875, 0.3141361, 2.7139875, -3.353658...

    ## Don't know how to automatically pick scale for object of type yearqtr. Defaulting to continuous.

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-28-1.png)

### Housing Price Index

``` r
hpichg = get_format_tq(ID = "USSTHPI", from = from, to = to, col_rename = 'hpichg', return = TRUE)
```

    ## 
    ## 
    ## Initial data pull
    ## 
    ## Observations: 179
    ## Variables: 2
    ## $ date  <date> 1975-01-01, 1975-04-01, 1975-07-01, 1975-10-01, 1976-01...
    ## $ price <dbl> 59.77, 60.97, 61.18, 62.22, 62.90, 65.40, 66.60, 67.36, ...
    ## 
    ## 
    ## Data after calculating period return
    ## 
    ## Observations: 179
    ## Variables: 2
    ## $ date  <date> 1975-01-01, 1975-04-01, 1975-07-01, 1975-10-01, 1976-01...
    ## $ price <dbl> 0.0000000, 2.0076962, 0.3444317, 1.6999019, 1.0928962, 3...
    ## 
    ## 
    ## Final Data
    ## 
    ## Observations: 179
    ## Variables: 2
    ## $ date   <yearqtr> 1975 Q1, 1975 Q2, 1975 Q3, 1975 Q4, 1976 Q1, 1976 Q...
    ## $ hpichg <dbl> 0.0000000, 2.0076962, 0.3444317, 1.6999019, 1.0928962, ...

    ## Don't know how to automatically pick scale for object of type yearqtr. Defaulting to continuous.

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-29-1.png)

### Home Ownership

``` r
homeownership = get_format_tq(ID = "RHORUSQ156N", from = from, to = to, col_rename = 'homeownership', return = FALSE)
```

    ## 
    ## 
    ## Initial data pull
    ## 
    ## Observations: 219
    ## Variables: 2
    ## $ date  <date> 1965-01-01, 1965-04-01, 1965-07-01, 1965-10-01, 1966-01...
    ## $ price <dbl> 62.9, 62.9, 62.9, 63.4, 63.5, 63.2, 63.3, 63.8, 63.3, 63...
    ## 
    ## 
    ## Final Data
    ## 
    ## Observations: 219
    ## Variables: 2
    ## $ date          <yearqtr> 1965 Q1, 1965 Q2, 1965 Q3, 1965 Q4, 1966 Q1,...
    ## $ homeownership <dbl> 62.9, 62.9, 62.9, 63.4, 63.5, 63.2, 63.3, 63.8, ...

    ## Don't know how to automatically pick scale for object of type yearqtr. Defaulting to continuous.

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-30-1.png)

BUSINESS ENVIRONMENT
--------------------

### Corporate Profits

``` r
corpprofitchg = get_format_tq(ID = "CP", from = from, to = to, col_rename = 'corpprofitchg', return = TRUE)
```

    ## 
    ## 
    ## Initial data pull
    ## 
    ## Observations: 291
    ## Variables: 2
    ## $ date  <date> 1947-01-01, 1947-04-01, 1947-07-01, 1947-10-01, 1948-01...
    ## $ price <dbl> 21.970, 20.788, 20.564, 22.451, 23.739, 24.941, 24.551, ...
    ## 
    ## 
    ## Data after calculating period return
    ## 
    ## Observations: 291
    ## Variables: 2
    ## $ date  <date> 1947-01-01, 1947-04-01, 1947-07-01, 1947-10-01, 1948-01...
    ## $ price <dbl> 0.000000, -5.380064, -1.077545, 9.176230, 5.736938, 5.06...
    ## 
    ## 
    ## Final Data
    ## 
    ## Observations: 291
    ## Variables: 2
    ## $ date          <yearqtr> 1947 Q1, 1947 Q2, 1947 Q3, 1947 Q4, 1948 Q1,...
    ## $ corpprofitchg <dbl> 0.000000, -5.380064, -1.077545, 9.176230, 5.7369...

    ## Don't know how to automatically pick scale for object of type yearqtr. Defaulting to continuous.

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-31-1.png)

### Inventories

``` r
inventorieschg = get_format_tq(ID = "INVCMRMTSPL", from = from, to = to, col_rename = 'inventorieschg', return = TRUE)
```

    ## 
    ## 
    ## Initial data pull
    ## 
    ## Observations: 633
    ## Variables: 2
    ## $ date  <date> 1967-01-01, 1967-02-01, 1967-03-01, 1967-04-01, 1967-05...
    ## $ price <dbl> 518404.4, 521161.2, 523994.7, 526049.5, 527402.3, 526752...
    ## 
    ## 
    ## Data after calculating period return
    ## 
    ## Observations: 211
    ## Variables: 2
    ## $ date  <date> 1967-03-01, 1967-06-01, 1967-09-01, 1967-12-01, 1968-03...
    ## $ price <dbl> 1.07838042, 0.52631274, 1.30594787, 1.50481281, 0.942207...
    ## 
    ## 
    ## Final Data
    ## 
    ## Observations: 211
    ## Variables: 2
    ## $ date           <yearqtr> 1967 Q1, 1967 Q2, 1967 Q3, 1967 Q4, 1968 Q1...
    ## $ inventorieschg <dbl> 1.07838042, 0.52631274, 1.30594787, 1.50481281,...

    ## Don't know how to automatically pick scale for object of type yearqtr. Defaulting to continuous.

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-32-1.png)

### Domestic Investments

``` r
## Gives issues with conversion to to % change
# investment = get_format_tq(ID = "W790RC1Q027SBEA", from = from, to = to, col_rename = 'investment', return = FALSE)
# investmentchg = get_format_tq(ID = "W790RC1Q027SBEA", from = from, to = to, col_rename = 'investmentchg', return = TRUE)
```

### Crude Oil

``` r
crude_wtichg = get_format_tq(ID = "WTISPLC", from = from, to = to, col_rename = 'crude_wtichg', return = TRUE)
```

    ## 
    ## 
    ## Initial data pull
    ## 
    ## Observations: 885
    ## Variables: 2
    ## $ date  <date> 1946-01-01, 1946-02-01, 1946-03-01, 1946-04-01, 1946-05...
    ## $ price <dbl> 1.17, 1.17, 1.17, 1.27, 1.27, 1.27, 1.27, 1.52, 1.52, 1....
    ## 
    ## 
    ## Data after calculating period return
    ## 
    ## Observations: 295
    ## Variables: 2
    ## $ date  <date> 1946-03-01, 1946-06-01, 1946-09-01, 1946-12-01, 1947-03...
    ## $ price <dbl> 0.000000, 8.547009, 19.685039, 6.578947, 0.000000, 15.43...
    ## 
    ## 
    ## Final Data
    ## 
    ## Observations: 295
    ## Variables: 2
    ## $ date         <yearqtr> 1946 Q1, 1946 Q2, 1946 Q3, 1946 Q4, 1947 Q1, ...
    ## $ crude_wtichg <dbl> 0.000000, 8.547009, 19.685039, 6.578947, 0.000000...

    ## Don't know how to automatically pick scale for object of type yearqtr. Defaulting to continuous.

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-34-1.png)

### Producer Price Index

``` r
ppichg = get_format_tq(ID = "PPIACO", from = from, to = to, col_rename = 'ppichg', return = TRUE)
```

    ## 
    ## 
    ## Initial data pull
    ## 
    ## Observations: 1,281
    ## Variables: 2
    ## $ date  <date> 1913-01-01, 1913-02-01, 1913-03-01, 1913-04-01, 1913-05...
    ## $ price <dbl> 12.1, 12.0, 12.0, 12.0, 11.9, 11.9, 12.0, 12.0, 12.2, 12...
    ## 
    ## 
    ## Data after calculating period return
    ## 
    ## Observations: 427
    ## Variables: 2
    ## $ date  <date> 1913-03-01, 1913-06-01, 1913-09-01, 1913-12-01, 1914-03...
    ## $ price <dbl> -0.8264463, -0.8333333, 2.5210084, -2.4590164, -1.680672...
    ## 
    ## 
    ## Final Data
    ## 
    ## Observations: 427
    ## Variables: 2
    ## $ date   <yearqtr> 1913 Q1, 1913 Q2, 1913 Q3, 1913 Q4, 1914 Q1, 1914 Q...
    ## $ ppichg <dbl> -0.8264463, -0.8333333, 2.5210084, -2.4590164, -1.68067...

    ## Don't know how to automatically pick scale for object of type yearqtr. Defaulting to continuous.

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-35-1.png)

### Industrial Production Index

``` r
ipichg = get_format_tq(ID = "INDPRO", from = from, to = to, col_rename = 'ipichg', return = TRUE)
```

    ## 
    ## 
    ## Initial data pull
    ## 
    ## Observations: 1,209
    ## Variables: 2
    ## $ date  <date> 1919-01-01, 1919-02-01, 1919-03-01, 1919-04-01, 1919-05...
    ## $ price <dbl> 5.0124, 4.7908, 4.6524, 4.7355, 4.7632, 5.0678, 5.3724, ...
    ## 
    ## 
    ## Data after calculating period return
    ## 
    ## Observations: 403
    ## Variables: 2
    ## $ date  <date> 1919-03-01, 1919-06-01, 1919-09-01, 1919-12-01, 1920-03...
    ## $ price <dbl> -7.182188, 8.928725, 5.463909, -1.036541, 7.329892, -1.9...
    ## 
    ## 
    ## Final Data
    ## 
    ## Observations: 403
    ## Variables: 2
    ## $ date   <yearqtr> 1919 Q1, 1919 Q2, 1919 Q3, 1919 Q4, 1920 Q1, 1920 Q...
    ## $ ipichg <dbl> -7.182188, 8.928725, 5.463909, -1.036541, 7.329892, -1....

    ## Don't know how to automatically pick scale for object of type yearqtr. Defaulting to continuous.

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-36-1.png)

STOCK MARKER
------------

### Willshire 5000

``` r
wilshirechg = get_format_tq(ID = "WILL5000INDFC", from = from, to = to, col_rename = 'wilshirechg', return = TRUE)
```

    ## 
    ## 
    ## Initial data pull
    ## 
    ## Observations: 12,718
    ## Variables: 2
    ## $ date  <date> 1970-12-31, 1971-01-01, 1971-01-04, 1971-01-05, 1971-01...
    ## $ price <dbl> 1.00, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...

    ## Warning in to_period(xx, period = on.opts[[period]], ...): missing values
    ## removed from data

    ## 
    ## 
    ## Data after calculating period return
    ## 
    ## Observations: 196
    ## Variables: 2
    ## $ date  <date> 1970-12-31, 1971-03-31, 1971-06-30, 1971-09-30, 1971-12...
    ## $ price <dbl> 0.0000000, 12.0000000, 0.8928571, -0.8849558, 5.3571429,...
    ## 
    ## 
    ## Final Data
    ## 
    ## Observations: 196
    ## Variables: 2
    ## $ date        <yearqtr> 1970 Q4, 1971 Q1, 1971 Q2, 1971 Q3, 1971 Q4, 1...
    ## $ wilshirechg <dbl> 0.0000000, 12.0000000, 0.8928571, -0.8849558, 5.35...

    ## Don't know how to automatically pick scale for object of type yearqtr. Defaulting to continuous.

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-37-1.png)

MACRO ECONOMIC FACTORS
----------------------

### Gold

``` r
goldchg = get_format_tq(ID = "GOLDAMGBD228NLBM", from = from, to = to, col_rename = 'goldchg', return = TRUE)
```

    ## 
    ## 
    ## Initial data pull
    ## 
    ## Observations: 13,436
    ## Variables: 2
    ## $ date  <date> 1968-04-01, 1968-04-02, 1968-04-03, 1968-04-04, 1968-04...
    ## $ price <dbl> 38.00, 37.60, 37.70, 36.70, 37.20, 37.00, 37.25, 37.60, ...

    ## Warning in to_period(xx, period = on.opts[[period]], ...): missing values
    ## removed from data

    ## 
    ## 
    ## Data after calculating period return
    ## 
    ## Observations: 206
    ## Variables: 2
    ## $ date  <date> 1968-06-28, 1968-09-30, 1968-12-31, 1969-03-31, 1969-06...
    ## $ price <dbl> 7.7631579, -3.0525031, 5.6675063, 2.7413588, -4.3503480,...
    ## 
    ## 
    ## Final Data
    ## 
    ## Observations: 206
    ## Variables: 2
    ## $ date    <yearqtr> 1968 Q2, 1968 Q3, 1968 Q4, 1969 Q1, 1969 Q2, 1969 ...
    ## $ goldchg <dbl> 7.7631579, -3.0525031, 5.6675063, 2.7413588, -4.350348...

    ## Don't know how to automatically pick scale for object of type yearqtr. Defaulting to continuous.

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-38-1.png)

### Exchange Rate

#### China

``` r
chinachg = get_format_tq(ID = "EXCHUS", from = from, to = to, col_rename = 'chinachg', return = TRUE)
```

    ## 
    ## 
    ## Initial data pull
    ## 
    ## Observations: 465
    ## Variables: 2
    ## $ date  <date> 1981-01-01, 1981-02-01, 1981-03-01, 1981-04-01, 1981-05...
    ## $ price <dbl> 1.5518, 1.6131, 1.6314, 1.6660, 1.7270, 1.7605, 1.7647, ...
    ## 
    ## 
    ## Data after calculating period return
    ## 
    ## Observations: 155
    ## Variables: 2
    ## $ date  <date> 1981-03-01, 1981-06-01, 1981-09-01, 1981-12-01, 1982-03...
    ## $ price <dbl> 5.1295270, 7.9134486, -0.3578529, -0.7809828, 5.8833668,...
    ## 
    ## 
    ## Final Data
    ## 
    ## Observations: 155
    ## Variables: 2
    ## $ date     <yearqtr> 1981 Q1, 1981 Q2, 1981 Q3, 1981 Q4, 1982 Q1, 1982...
    ## $ chinachg <dbl> 5.1295270, 7.9134486, -0.3578529, -0.7809828, 5.88336...

    ## Don't know how to automatically pick scale for object of type yearqtr. Defaulting to continuous.

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-39-1.png)

#### Japan

``` r
japanchg = get_format_tq(ID = "EXJPUS", from = from, to = to, col_rename = 'japanchg', return = TRUE)
```

    ## 
    ## 
    ## Initial data pull
    ## 
    ## Observations: 585
    ## Variables: 2
    ## $ date  <date> 1971-01-01, 1971-02-01, 1971-03-01, 1971-04-01, 1971-05...
    ## $ price <dbl> 358.0200, 357.5450, 357.5187, 357.5032, 357.4130, 357.41...
    ## 
    ## 
    ## Data after calculating period return
    ## 
    ## Observations: 195
    ## Variables: 2
    ## $ date  <date> 1971-03-01, 1971-06-01, 1971-09-01, 1971-12-01, 1972-03...
    ## $ price <dbl> -0.14002011, -0.02990053, -5.42533850, -5.30981803, -5.4...
    ## 
    ## 
    ## Final Data
    ## 
    ## Observations: 195
    ## Variables: 2
    ## $ date     <yearqtr> 1971 Q1, 1971 Q2, 1971 Q3, 1971 Q4, 1972 Q1, 1972...
    ## $ japanchg <dbl> -0.14002011, -0.02990053, -5.42533850, -5.30981803, -...

    ## Don't know how to automatically pick scale for object of type yearqtr. Defaulting to continuous.

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-40-1.png)

#### UK

``` r
ukchg = get_format_tq(ID = "EXUSUK", from = from, to = to, col_rename = 'ukchg', return = TRUE)
```

    ## 
    ## 
    ## Initial data pull
    ## 
    ## Observations: 585
    ## Variables: 2
    ## $ date  <date> 1971-01-01, 1971-02-01, 1971-03-01, 1971-04-01, 1971-05...
    ## $ price <dbl> 2.4058, 2.4178, 2.4187, 2.4179, 2.4187, 2.4188, 2.4185, ...
    ## 
    ## 
    ## Data after calculating period return
    ## 
    ## Observations: 195
    ## Variables: 2
    ## $ date  <date> 1971-03-01, 1971-06-01, 1971-09-01, 1971-12-01, 1972-03...
    ## $ price <dbl> 0.536204173, 0.004134452, 2.091946420, 2.316352150, 3.62...
    ## 
    ## 
    ## Final Data
    ## 
    ## Observations: 195
    ## Variables: 2
    ## $ date  <yearqtr> 1971 Q1, 1971 Q2, 1971 Q3, 1971 Q4, 1972 Q1, 1972 Q2...
    ## $ ukchg <dbl> 0.536204173, 0.004134452, 2.091946420, 2.316352150, 3.62...

    ## Don't know how to automatically pick scale for object of type yearqtr. Defaulting to continuous.

![](gdp_prediction_datacollection_files/figure-markdown_github/unnamed-chunk-41-1.png)

Merge Data
==========

``` r
dfs = list(gdp_change = gdp_change,
           labor_unrate = unrate,
           labor_nfjobschg = nfjobschg,
           monitory_treas10yr = treas10yr,
           monitory_treas3mo = treas3mo,
           monitory_treas10yr3mo = treas10yr3mo,
           monitory_fedintrate = fedintrate,
           # monitory_libor3mo = libor3mo,
           consumer_personincomechg = personincomechg,
           consumer_cpichg = cpichg,
           consumer_popchg = popchg,
           housing_housingpermitschg = housingpermitschg,
           # housing_hpichg = hpichg,
           housing_homeownership = homeownership,
           business_corpprofitchg = corpprofitchg,
           business_inventorieschg = inventorieschg,
           business_crude_wtichg = crude_wtichg,
           business_ppichg = ppichg,
           business_ipichg = ipichg,
           stock_wilshirechg = wilshirechg,
           macro_goldchg = goldchg,
           macro_exc_japanchg = japanchg,
           # macro_exc_chinachg = chinachg,
           macro_exc_ukchg = ukchg
           )

for (i in 1:length(dfs)){
  print(paste0("Number of observations in ", names(dfs[i]), " = ", nrow(dfs[[i]])))
}
```

    ## [1] "Number of observations in gdp_change = 290"
    ## [1] "Number of observations in labor_unrate = 287"
    ## [1] "Number of observations in labor_nfjobschg = 323"
    ## [1] "Number of observations in monitory_treas10yr = 231"
    ## [1] "Number of observations in monitory_treas3mo = 151"
    ## [1] "Number of observations in monitory_treas10yr3mo = 151"
    ## [1] "Number of observations in monitory_fedintrate = 261"
    ## [1] "Number of observations in consumer_personincomechg = 287"
    ## [1] "Number of observations in consumer_cpichg = 427"
    ## [1] "Number of observations in consumer_popchg = 243"
    ## [1] "Number of observations in housing_housingpermitschg = 239"
    ## [1] "Number of observations in housing_homeownership = 219"
    ## [1] "Number of observations in business_corpprofitchg = 291"
    ## [1] "Number of observations in business_inventorieschg = 211"
    ## [1] "Number of observations in business_crude_wtichg = 295"
    ## [1] "Number of observations in business_ppichg = 427"
    ## [1] "Number of observations in business_ipichg = 403"
    ## [1] "Number of observations in stock_wilshirechg = 196"
    ## [1] "Number of observations in macro_goldchg = 206"
    ## [1] "Number of observations in macro_exc_japanchg = 195"
    ## [1] "Number of observations in macro_exc_ukchg = 195"

``` r
# We may want to remove some of these variables if that is reducing the number of observations 
# and is already highly correlated to other features. Example: libor3mo is related to fedintrate
```

``` r
data = join_all(dfs, by='date', type='full') %>% 
  arrange(date) %>% 
  drop_na()
  
data %>% glimpse()
```

    ## Observations: 151
    ## Variables: 22
    ## $ date              <yearqtr> 1982 Q1, 1982 Q2, 1982 Q3, 1982 Q4, 1983...
    ## $ gdp_change        <dbl> -0.8, 7.2, 4.2, 4.4, 8.6, 12.7, 12.9, 11.9, ...
    ## $ unrate            <dbl> 9.0, 9.6, 10.1, 10.8, 10.3, 10.1, 9.2, 8.3, ...
    ## $ nfjobschg         <dbl> -0.5071786, -0.6291881, -0.7589162, -0.46197...
    ## $ treas10yr         <dbl> 14.18, 14.44, 11.73, 10.36, 10.62, 10.96, 11...
    ## $ treas3mo          <dbl> 13.99, 13.36, 7.88, 8.20, 8.96, 9.15, 9.04, ...
    ## $ treas10yr3mo      <dbl> 0.19, 1.08, 3.85, 2.16, 1.66, 1.81, 2.40, 2....
    ## $ fedintrate        <dbl> 14.68, 14.15, 10.31, 8.95, 8.77, 8.98, 9.45,...
    ## $ personincomechg   <dbl> 2.6, 3.1, 1.4, 1.6, 2.4, 2.5, 3.5, 5.5, 6.6,...
    ## $ cpichg            <dbl> 0.5319149, 2.6455026, 0.9278351, -0.3064351,...
    ## $ popchg            <dbl> 0.2098904, 0.2297481, 0.2662760, 0.2290424, ...
    ## $ housingpermitschg <dbl> 11.9346734, 2.4691358, 14.1292442, 29.654510...
    ## $ homeownership     <dbl> 64.8, 64.9, 64.9, 64.5, 64.7, 64.7, 64.8, 64...
    ## $ corpprofitchg     <dbl> -9.8822616, 1.0334227, -2.5010250, -3.405117...
    ## $ inventorieschg    <dbl> -1.3318626, -0.2449114, -0.1071666, -1.63672...
    ## $ crude_wtichg      <dbl> -18.6285714, 23.1390449, 1.5968064, -10.9738...
    ## $ ppichg            <dbl> 0.80971660, 0.40160643, 0.00000000, 0.500000...
    ## $ ipichg            <dbl> -0.81243157, -1.84873818, -1.48731619, -1.99...
    ## $ wilshirechg       <dbl> -9.0163934, -1.3513514, 11.4155251, 18.85245...
    ## $ goldchg           <dbl> -19.8750000, -2.0280811, 25.7165605, 13.4895...
    ## $ japanchg          <dbl> 10.1727985, 4.1327655, 4.8121460, -8.1069348...
    ## $ ukchg             <dbl> -5.1489518, -2.7142303, -2.5223481, -5.60747...

``` r
data %>% skim()
```

    ## Warning: No summary functions for vectors of class: yearqtr.
    ## Coercing to character

    ## Skim summary statistics
    ##  n obs: 151 
    ##  n variables: 22 
    ## 
    ## -- Variable type:character -----------------------------------------------------------------------------------------------------------------------
    ##  variable missing complete   n min max empty n_unique
    ##      date       0      151 151   7   7     0      151
    ## 
    ## -- Variable type:numeric -------------------------------------------------------------------------------------------------------------------------
    ##           variable missing complete   n  mean     sd      p0   p25   p50
    ##      corpprofitchg       0      151 151  1.75  7.45  -41.16  -0.98  1.77
    ##             cpichg       0      151 151  0.67  0.78   -3.91   0.23  0.7 
    ##       crude_wtichg       0      151 151  1.84 17.33  -60.52  -7.67  1.93
    ##         fedintrate       0      151 151  4.14  3.31    0.07   1.04  4.24
    ##         gdp_change       0      151 151  5.15  2.9    -7.2    3.75  5   
    ##            goldchg       0      151 151  1.14  7.33  -24.91  -3.86  0.6 
    ##      homeownership       0      151 151 65.58  1.78   62.9   64.1  64.9 
    ##  housingpermitschg       0      151 151  0.81  9.31  -32.46  -4.09  1.04
    ##     inventorieschg       0      151 151  0.6   0.91   -2.86   0.22  0.67
    ##             ipichg       0      151 151  0.51  1.37   -6.3    0.11  0.68
    ##           japanchg       0      151 151 -0.32  5.51  -14.36  -3.74 -0.12
    ##          nfjobschg       0      151 151  0.34  0.47   -1.73   0.22  0.41
    ##    personincomechg       0      151 151  2.93  1.67   -2.5    1.95  3   
    ##             popchg       0      151 151  0.23  0.059   0.092  0.19  0.23
    ##             ppichg       0      151 151  0.48  2.05  -13.2   -0.28  0.41
    ##          treas10yr       0      151 151  5.65  2.99    1.49   3.24  5.15
    ##       treas10yr3mo       0      151 151  1.77  1.13   -0.77   0.86  1.84
    ##           treas3mo       0      151 151  3.88  3.12    0      0.95  3.96
    ##              ukchg       0      151 151 -0.17  4.82  -17.35  -2.95 -0.36
    ##             unrate       0      151 151  6.14  1.71    3.5    5     5.7 
    ##        wilshirechg       0      151 151  3.06  8.21  -23.08  -0.72  3.85
    ##    p75   p100     hist
    ##   4.41  50.08 <U+2581><U+2581><U+2581><U+2587><U+2582><U+2581><U+2581><U+2581>
    ##   1.18   2.65 <U+2581><U+2581><U+2581><U+2581><U+2583><U+2587><U+2585><U+2581>
    ##  11.24  99.67 <U+2581><U+2581><U+2586><U+2587><U+2581><U+2581><U+2581><U+2581>
    ##   6.12  14.68 <U+2587><U+2583><U+2585><U+2583><U+2582><U+2581><U+2581><U+2581>
    ##   6.85  12.9  <U+2581><U+2581><U+2581><U+2582><U+2587><U+2586><U+2582><U+2581>
    ##   5.82  25.72 <U+2581><U+2581><U+2582><U+2587><U+2587><U+2583><U+2581><U+2581>
    ##  67.2   69.2  <U+2581><U+2587><U+2583><U+2582><U+2581><U+2582><U+2582><U+2582>
    ##   6.5   29.65 <U+2581><U+2581><U+2582><U+2585><U+2587><U+2585><U+2581><U+2581>
    ##   1.11   2.79 <U+2581><U+2581><U+2581><U+2583><U+2587><U+2587><U+2582><U+2581>
    ##   1.14   4.2  <U+2581><U+2581><U+2581><U+2582><U+2583><U+2587><U+2582><U+2581>
    ##   2.81  18.8  <U+2581><U+2582><U+2585><U+2587><U+2585><U+2582><U+2581><U+2581>
    ##   0.63   1.36 <U+2581><U+2581><U+2581><U+2582><U+2582><U+2587><U+2585><U+2581>
    ##   3.9    7.6  <U+2581><U+2581><U+2582><U+2586><U+2587><U+2583><U+2582><U+2581>
    ##   0.27   0.37 <U+2581><U+2582><U+2585><U+2587><U+2587><U+2583><U+2582><U+2582>
    ##   1.49   6.71 <U+2581><U+2581><U+2581><U+2581><U+2582><U+2587><U+2582><U+2581>
    ##   7.46  14.44 <U+2587><U+2587><U+2586><U+2585><U+2583><U+2581><U+2582><U+2581>
    ##   2.68   3.85 <U+2582><U+2585><U+2585><U+2586><U+2587><U+2587><U+2586><U+2585>
    ##   5.87  13.99 <U+2587><U+2583><U+2585><U+2585><U+2582><U+2581><U+2581><U+2581>
    ##   2.57  15.52 <U+2581><U+2581><U+2582><U+2587><U+2587><U+2583><U+2581><U+2581>
    ##   7.2   10.8  <U+2585><U+2587><U+2587><U+2583><U+2585><U+2581><U+2582><U+2582>
    ##   7.63  21.51 <U+2581><U+2581><U+2582><U+2583><U+2587><U+2587><U+2582><U+2582>

``` r
head(data)
```

    ##        date gdp_change unrate  nfjobschg treas10yr treas3mo treas10yr3mo
    ## 277 1982 Q1       -0.8    9.0 -0.5071786     14.18    13.99         0.19
    ## 278 1982 Q2        7.2    9.6 -0.6291881     14.44    13.36         1.08
    ## 279 1982 Q3        4.2   10.1 -0.7589162     11.73     7.88         3.85
    ## 280 1982 Q4        4.4   10.8 -0.4619715     10.36     8.20         2.16
    ## 281 1983 Q1        8.6   10.3  0.3593516     10.62     8.96         1.66
    ## 282 1983 Q2       12.7   10.1  1.0450107     10.96     9.15         1.81
    ##     fedintrate personincomechg     cpichg    popchg housingpermitschg
    ## 277      14.68             2.6  0.5319149 0.2098904         11.934673
    ## 278      14.15             3.1  2.6455026 0.2297481          2.469136
    ## 279      10.31             1.4  0.9278351 0.2662760         14.129244
    ## 280       8.95             1.6 -0.3064351 0.2290424         29.654511
    ## 281       8.77             2.4  0.3073770 0.1955059          9.178386
    ## 282       8.98             2.5  1.6343207 0.2207987         19.932203
    ##     homeownership corpprofitchg inventorieschg crude_wtichg      ppichg
    ## 277          64.8    -9.8822616     -1.3318626   -18.628571  0.80971660
    ## 278          64.9     1.0334227     -0.2449114    23.139045  0.40160643
    ## 279          64.9    -2.5010250     -0.1071666     1.596806  0.00000000
    ## 280          64.5    -3.4051171     -1.6367250   -10.973898  0.50000000
    ## 281          64.7     0.9142255     -1.4504862    -9.142497 -0.09950249
    ## 282          64.7    10.8940228      0.3513805     7.564192  0.59760956
    ##         ipichg wilshirechg     goldchg   japanchg     ukchg
    ## 277 -0.8124316   -9.016393 -19.8750000 10.1727985 -5.148952
    ## 278 -1.8487382   -1.351351  -2.0280811  4.1327655 -2.714230
    ## 279 -1.4873162   11.415525  25.7165605  4.8121460 -2.522348
    ## 280 -1.9981311   18.852459  13.4895503 -8.1069348 -5.607477
    ## 281  2.1205413   11.379310  -7.5334821 -1.5239234 -7.797030
    ## 282  2.5240838   13.003096   0.6035003  0.7458837  3.892617

``` r
tail(data)
```

    ##        date gdp_change unrate nfjobschg treas10yr treas3mo treas10yr3mo
    ## 422 2018 Q2        7.1    4.0 0.4276444      2.85     1.93         0.92
    ## 423 2018 Q3        4.8    3.7 0.3089571      3.05     2.19         0.86
    ## 424 2018 Q4        2.9    3.9 0.3461714      2.69     2.45         0.24
    ## 425 2019 Q1        3.9    3.8 0.2782504      2.41     2.40         0.01
    ## 426 2019 Q2        4.7    3.7 0.3174033      2.00     2.12        -0.12
    ## 427 2019 Q3        3.8    3.5 0.4039560      1.68     1.88        -0.20
    ##     fedintrate personincomechg     cpichg     popchg housingpermitschg
    ## 422       1.82             3.9  0.9757407 0.12559851         -7.112376
    ## 423       1.95             4.1  0.1785792 0.14716366         -1.378254
    ## 424       2.27             3.9 -0.4777392 0.12036856          3.959627
    ## 425       2.41             3.3  1.1817715 0.09245641         -3.808813
    ## 426       2.38             3.0  0.7635660 0.11553997         -4.347826
    ## 427       2.04             2.7  0.2404907 0.15011997         12.905844
    ##     homeownership corpprofitchg inventorieschg crude_wtichg      ppichg
    ## 422          64.3     0.9507799     -0.1003632     8.193847  2.45860512
    ## 423          64.4     0.1634533      1.0358615     3.477236 -0.29382958
    ## 424          64.8    -1.5463943      1.0624532   -29.488822 -1.27701375
    ## 425          64.2    -1.5322271      1.2453783    17.427302 -0.09950249
    ## 426          64.1     3.3161893      0.6596441    -6.001720 -0.24900398
    ## 427          64.8    -1.2422698      0.5905517     4.189535 -0.94857713
    ##         ipichg wilshirechg   goldchg   japanchg     ukchg
    ## 422  0.8566748   4.1121495 -5.540449  3.7879502 -4.879794
    ## 423  1.3905799   7.0564359 -5.361641  1.8476556 -1.715059
    ## 424  0.7993625 -14.4221655  8.293198  0.0909923 -3.076688
    ## 425 -0.7874151  14.2285081  0.741232 -0.9403794  3.971889
    ## 426 -0.3680671   3.9307824  9.452813 -2.7673934 -3.736614
    ## 427  0.1785365   0.7535525  5.264648 -0.4890417 -2.414201

-   But wait. We need to predict the GDP in the future quarters using data available in this quarter.
-   So we may need to change the gdp column to a lagged gdp column (for next quarter). We will evaluate how to do this.

``` r
write.csv(data, "../data/economic_indicators_all_ex_3mo_china_inc_treas3mo.csv", row.names = FALSE)
```

TODO
====

Questions for Dr. Sadler
------------------------

1.  Some data is quarterly and some if monthly. How do we not lose the monthly granularity and still use the quarterly data? Is this still possible, since our output (GDP) is measured quarterly, i.e. our can we predict quarterly data using monthly frequency of certain variables?
2.  If we expect certain predictors to themselves be non-stationary (such as $ corporate profits), should we convert this to a stationary signal such as % change from las period?
3.  When collecting the data (predictors), some are seasonally adjusted and some are not. If we have a choice, which one should we pick? Does it matter?

Things to Consider
------------------

1.  Consider removing the data related to the 3mo Treasury Rate (since it only has 137 quarterly observations) and China Exchange Rate (it only has &lt; 160 quarterly observations and also this data was controlled artificially for a while). If we remove these we can increase our dataset to close to 200 observations (almost 50% increase).

Data Collection
---------------

1.  See if the value used during the resampling can be converted to the median value during that period.

-   <https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ02-quant-integrations-in-tidyquant.html#example-2-use-xts-to.period-to-change-the-periodicity-from-daily-to-monthly>

<!-- -->

    unrate2 = unrate %>%    
        tq_transmute(mutate_fun = apply.quarterly,
                     indexAt = index.at,
                     FUN = median,
                     col_rename = "unrate") %>% 
        mutate(date = paste0(year(date), " Q", quarter(date))) 
    # Need to convert date to type 'datetime'

    unrate2 %>% glimpse()

EDA
---

1.  Plot correlation plots with lagged variables to gain insight into what features might be useful predictors.

Model Analysis
--------------

1.  If I take GDP data from way back (1946), the resultsfor ARMA and ARIMA analysis are very different (spectral densities, etc). So, while fitting, should we not do something like 5 fold cross validation?
2.  Use Rolling Window to calculate the ASE inistead of just the last n values. This will give a more robust estimate.
