    ## Warning: package 'mosaic' was built under R version 3.6.3

    ## Loading required package: dplyr

    ## Warning: package 'dplyr' was built under R version 3.6.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## Loading required package: lattice

    ## Warning: package 'lattice' was built under R version 3.6.3

    ## Loading required package: ggformula

    ## Warning: package 'ggformula' was built under R version 3.6.3

    ## Loading required package: ggplot2

    ## Warning: package 'ggplot2' was built under R version 3.6.3

    ## Loading required package: ggstance

    ## Warning: package 'ggstance' was built under R version 3.6.3

    ## 
    ## Attaching package: 'ggstance'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     geom_errorbarh, GeomErrorbarh

    ## 
    ## New to ggformula?  Try the tutorials: 
    ##  learnr::run_tutorial("introduction", package = "ggformula")
    ##  learnr::run_tutorial("refining", package = "ggformula")

    ## Loading required package: mosaicData

    ## Warning: package 'mosaicData' was built under R version 3.6.3

    ## Loading required package: Matrix

    ## Warning: package 'Matrix' was built under R version 3.6.3

    ## Registered S3 method overwritten by 'mosaic':
    ##   method                           from   
    ##   fortify.SpatialPolygonsDataFrame ggplot2

    ## 
    ## The 'mosaic' package masks several functions from core packages in order to add 
    ## additional features.  The original behavior of these functions should not be affected by this.
    ## 
    ## Note: If you use the Matrix package, be sure to load it BEFORE loading mosaic.
    ## 
    ## Have you tried the ggformula package for your plots?

    ## 
    ## Attaching package: 'mosaic'

    ## The following object is masked from 'package:Matrix':
    ## 
    ##     mean

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     stat

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     count, do, tally

    ## The following objects are masked from 'package:stats':
    ## 
    ##     binom.test, cor, cor.test, cov, fivenum, IQR, median, prop.test,
    ##     quantile, sd, t.test, var

    ## The following objects are masked from 'package:base':
    ## 
    ##     max, mean, min, prod, range, sample, sum

    ## Warning: package 'tidyverse' was built under R version 3.6.3

    ## -- Attaching packages ------------------------------------------------------ tidyverse 1.3.0 --

    ## v tibble  3.0.2     v purrr   0.3.4
    ## v tidyr   1.1.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## Warning: package 'tibble' was built under R version 3.6.3

    ## Warning: package 'tidyr' was built under R version 3.6.3

    ## Warning: package 'readr' was built under R version 3.6.3

    ## Warning: package 'purrr' was built under R version 3.6.3

    ## Warning: package 'stringr' was built under R version 3.6.3

    ## Warning: package 'forcats' was built under R version 3.6.3

    ## -- Conflicts --------------------------------------------------------- tidyverse_conflicts() --
    ## x mosaic::count()            masks dplyr::count()
    ## x purrr::cross()             masks mosaic::cross()
    ## x mosaic::do()               masks dplyr::do()
    ## x tidyr::expand()            masks Matrix::expand()
    ## x dplyr::filter()            masks stats::filter()
    ## x ggstance::geom_errorbarh() masks ggplot2::geom_errorbarh()
    ## x dplyr::lag()               masks stats::lag()
    ## x tidyr::pack()              masks Matrix::pack()
    ## x mosaic::stat()             masks ggplot2::stat()
    ## x mosaic::tally()            masks dplyr::tally()
    ## x tidyr::unpack()            masks Matrix::unpack()

    ## Warning: package 'scales' was built under R version 3.6.3

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

    ## The following object is masked from 'package:mosaic':
    ## 
    ##     rescale

\#Problem 1

Basic information about our building: Total GFA: 250,000 sqft Storie: 15
Age: 0 Baseline Construction Cost per sqft: $400

The question we are trying to answer is whether buildings with green
certificates can commnad higher rent, with *everything else being
equal*. So the key is to use a comparable set of buildings data to
estimate rents for our building, both with and without green
certificates.

There are lots of factors that influence average rents. The stats guru
only accounted for leasing rate and outliers by getting rid of the
buildings with leasing\_rate &lt; 10% and use median rent instead of
mean rent. However, he did not account for other factors that may skew
the comparison between green buildings and non-green buildings.

For example, the following histogram shows that nearlly all of the green
buildings are less than 50 years but a significant amount of nongreen
buildings are more than 50 years old. Since newer buildings generally
charge higher rents, the rent differential between green and nongreen
buildings may be the result of the age of the building, and not
necessarily due to the green certificate.

``` r
ggplot(greenbuilding,aes(x=age)) + 
  geom_histogram(bins = 30,color="black", fill="white") +
  facet_wrap(~ green,nrow = 1)
```

![](HW2_files/figure-markdown_github/unnamed-chunk-3-1.png)

In addition, only one third of the nongreen buildings are class a
buildings whereas more than three quarters of green buildings are class
a building. Thus the higher rent charged by green buildings could also
be explained by building quality.

``` r
ggplot(greenbuilding) +
  geom_col(aes(x = 1, y = n, fill = class), position = "fill") +
  coord_polar(theta = "y") +
  facet_wrap(~ green) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank())
```

![](HW2_files/figure-markdown_github/unnamed-chunk-4-1.png)

Green buildings also tend to have better amenities.

``` r
ggplot(greenbuilding) +
  geom_col(aes(x = 1, y = n, fill = amenities), position = "fill") +
  coord_polar(theta = "y") +
  facet_wrap(~ green) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()) +
  labs(
    caption = "Amenities = 1 means with amenities and 0 means without"
  )
```

![](HW2_files/figure-markdown_github/unnamed-chunk-5-1.png)

Lastly, we have to account for location. If green buildings are
generally built in areas with higher rent, then the rent differential
between green and nongreen buildings are not due to the green
certificate, but more likely due to location premium.

Thus, we have to compare rent differentials between buildings in the
same location. We first create a compare\_set that only include
buildings with leasing rate more than 10%, less than 50 years old, with
class a ratings and have amenities.

``` r
compare_set <- subset(greenbuilding, leasing_rate > 10 &
                      age < 50 & class_a == 1 & amenities == 1)
```

We then create two subsets for green and nongreen buildings. For each
subset, we calculate the median rent for each cluster. We then create a
separate data table with each cluster as one row and with three columns:
median rent for green buildings, median rent for nongreen buildings and
rent differentials between green and nongreen buildings.

``` r
green <- compare_set[compare_set$green_rating == 1, ]
nongreen <- compare_set[compare_set$green_rating == 0, ]

green_median = green %>%
  group_by(cluster) %>%
  summarize(median_rent = median(Rent))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
nongreen_median = nongreen %>%
  group_by(cluster) %>%
  summarize(median_rent = median(Rent))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
merged = merge(nongreen_median,green_median,by.x = 'cluster', by.y = 'cluster',suffixes  = c(".nongreen", ".green"))

merged$rent_diff = merged$median_rent.green-merged$median_rent.nongreen
```

The scatterplot shows that the rent differentials are equally spread
along the 0 line. The average rent differential is -0.123, suggesting
that green buildings do not in fact command a higher rent compared to
non green buildings.

``` r
ggplot(data = merged) + 
  geom_point(mapping = aes(x = cluster, y = rent_diff)) +
  geom_hline(aes(yintercept=0),
            color="blue", linetype="dashed", size=1) +
  annotate("text", x=20, y=20, color="red", label= paste('avg rent differential: ', mean(merged$rent_diff)),hjust=0) 
```

![](HW2_files/figure-markdown_github/unnamed-chunk-8-1.png)

In conclusion, our analysis shows that green certificates do not
generate rent premium. The higher rent charged by green buildings are
likely the result of other factors such as newer buildings and better
amenities. Thus, from a purely economic perspective, it is not advisable
to get green certified.

\#Problem 2 The following is an analysis of Austin Bergstrom
International Airport. The data has been grouped in a way that shows
optimal times for flying in regards to month, day of week, time of day,
and carrier in order to avoid arrival and/or departure delays.

``` r
library(mosaic)
library(tidyverse)
library(ggplot2)

data1 <-"data/ABIA.csv"
ABIA = read.csv(data1)
#ABIA = read.csv('ABIA.csv')
head(ABIA)
```

    ##   Year Month DayofMonth DayOfWeek DepTime CRSDepTime ArrTime CRSArrTime
    ## 1 2008     1          1         2     120       1935     309       2130
    ## 2 2008     1          1         2     555        600     826        835
    ## 3 2008     1          1         2     600        600     728        729
    ## 4 2008     1          1         2     601        605     727        750
    ## 5 2008     1          1         2     601        600     654        700
    ## 6 2008     1          1         2     636        645     934        932
    ##   UniqueCarrier FlightNum TailNum ActualElapsedTime CRSElapsedTime AirTime
    ## 1            9E      5746  84129E               109            115      88
    ## 2            AA      1614  N438AA               151            155     133
    ## 3            YV      2883  N922FJ               148            149     125
    ## 4            9E      5743  89189E                86            105      70
    ## 5            AA      1157  N4XAAA                53             60      38
    ## 6            NW      1674   N967N               178            167     145
    ##   ArrDelay DepDelay Origin Dest Distance TaxiIn TaxiOut Cancelled
    ## 1      339      345    MEM  AUS      559      3      18         0
    ## 2       -9       -5    AUS  ORD      978      7      11         0
    ## 3       -1        0    AUS  PHX      872      7      16         0
    ## 4      -23       -4    AUS  MEM      559      4      12         0
    ## 5       -6        1    AUS  DFW      190      5      10         0
    ## 6        2       -9    AUS  MSP     1042     11      22         0
    ##   CancellationCode Diverted CarrierDelay WeatherDelay NASDelay SecurityDelay
    ## 1                         0          339            0        0             0
    ## 2                         0           NA           NA       NA            NA
    ## 3                         0           NA           NA       NA            NA
    ## 4                         0           NA           NA       NA            NA
    ## 5                         0           NA           NA       NA            NA
    ## 6                         0           NA           NA       NA            NA
    ##   LateAircraftDelay
    ## 1                 0
    ## 2                NA
    ## 3                NA
    ## 4                NA
    ## 5                NA
    ## 6                NA

``` r
# Delays vs. Month
plot(ABIA$Month, ABIA$ArrDelay, main = "Arrival Delays Occur Most in Dec. and March", xlab= "Month", ylab= "Arrival Delay")
```

![](HW2_files/figure-markdown_github/unnamed-chunk-9-1.png) This is
showing the arrival delays per month. From this, it appears that most
arrival delays occur in December and March.

``` r
plot(ABIA$Month, ABIA$DepDelay, main = "Departure Delay Occur Most in March and August", xlab= "Month", ylab= "Departure Delay")
```

![](HW2_files/figure-markdown_github/unnamed-chunk-10-1.png) To compare
arrival delays with depature delays, departure delays were aslo ran
against months. Departure delays occur most in March and August. As
March was present in both results, March seems to be full of delays of
both kinds.

``` r
# Delays vs. Day of Week
plot(ABIA$DayOfWeek, ABIA$ArrDelay, main = "Tuesday Has Lowest Arrival Delay", xlab= "Day of Week", ylab= "Arrival Delay")
```

![](HW2_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
plot(ABIA$DayOfWeek, ABIA$DepDelay, main = "Tuesday Has Lowest Departure Delay", xlab= "Day of Week", ylab= "Departure Delay")
```

![](HW2_files/figure-markdown_github/unnamed-chunk-11-2.png) These are a
couple plots that represent what day is best to fly on in order to avoid
delays. Tuesday was found as the best day to fly in order to avoid
delays.

``` r
# Departure Time vs. Delay
ggplot(data = ABIA) + 
  geom_point(mapping = aes(x = DepTime, y = DepDelay)) +
  ggtitle("Later Departure Flights Have Higher Delays")
```

    ## Warning: Removed 1413 rows containing missing values (geom_point).

![](HW2_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
# Arrival Time vs. Delay
ggplot(data = ABIA) + 
  geom_point(mapping = aes(x = ArrTime, y = ArrDelay)) +
  ggtitle("      Arrival Delays Greatest At Night")
```

    ## Warning: Removed 1601 rows containing missing values (geom_point).

![](HW2_files/figure-markdown_github/unnamed-chunk-12-2.png) Now that
month and day have been narrowed down, the next step was to narrow down
a time to fly. From these plots, it appears that mornings are best to
fly. However this does not mean morning as in midnight, but rather
around 5AM. The increase around midnight is rollover from the high level
of delays as it gets later in the night.

``` r
################################################

# Departure Delay vs. Distance
ggplot(data = ABIA) + 
  geom_point(mapping = aes(x = DepDelay, y = Distance, color = Distance)) + 
  ggtitle("Departure Delay Greatest For Small and Large Distances")
```

    ## Warning: Removed 1413 rows containing missing values (geom_point).

![](HW2_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
# Arrival Delay vs. Distance
ggplot(data = ABIA) + 
  geom_point(mapping = aes(x = ArrDelay, y = Distance, color = Distance)) + 
  ggtitle("Arrival Delay Greatest For Small and Large Distances")
```

    ## Warning: Removed 1601 rows containing missing values (geom_point).

![](HW2_files/figure-markdown_github/unnamed-chunk-13-2.png) Next, I
wanted to see whether distance was a factor in delays. From these plots,
we can see that delays are greatest both on the extremes. This makes
sense as shorter flights don’t have enough distance to make up for lost
time and long distances tend to require more preparations.

``` r
# Arrival Delay vs. Carrier
ggplot(data = ABIA) + 
  geom_point(mapping = aes(x = ArrDelay, y = UniqueCarrier, color = UniqueCarrier)) + 
  ggtitle("Arrival Delay Greatest Among JetBlue and SouthWest")
```

    ## Warning: Removed 1601 rows containing missing values (geom_point).

![](HW2_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
# Departure Delay vs. Carrier
ggplot(data = ABIA) + 
  geom_point(mapping = aes(x = DepDelay, y = UniqueCarrier, color = UniqueCarrier)) + 
  ggtitle("Departure Delay Greatest Among JetBlue")
```

    ## Warning: Removed 1413 rows containing missing values (geom_point).

![](HW2_files/figure-markdown_github/unnamed-chunk-14-2.png) These plots
are testing whether a specific carrier tends to have more delays than
others. Both Jetblue and SouthWest tend to have the most arrival delays,
while JetBlue also seems to have the greatest departure delays. To avoid
delays, one might want to fly with someone other than JetBlue.

``` r
###
# facets
###

#####################################################################
# Departure Delay vs. Day of Week vs. Month
ggplot(data = ABIA) + 
  geom_point(mapping = aes(x = DepDelay, y = DayOfWeek)) + 
  facet_wrap(~ Month, nrow = 2) +
  ggtitle("What Day to Fly, Per Month to Avoid Departure Delays")
```

    ## Warning: Removed 1413 rows containing missing values (geom_point).

![](HW2_files/figure-markdown_github/unnamed-chunk-15-1.png)

``` r
# Arrival Delay vs. Day of Week vs. Month
ggplot(data = ABIA) + 
  geom_point(mapping = aes(x = ArrDelay, y = DayOfWeek)) + 
  facet_wrap(~ Month, nrow = 2) +
  ggtitle("What Day to Fly, Per Month to Avoid Arrival Delays")
```

    ## Warning: Removed 1601 rows containing missing values (geom_point).

![](HW2_files/figure-markdown_github/unnamed-chunk-15-2.png)

``` r
###################################################################33
```

In preparation for your next flight, you might want to know what a
certain day looks like on the particular month you need to travel. These
two plots will point you in the right direction, where you will first be
able to look at the month that you are flying and then find the smallest
line, indicating that is the day with the fewest delays during that
month.

\#Problem 3

``` r
library(mosaic)
library(quantmod)
```

    ## Warning: package 'quantmod' was built under R version 3.6.3

    ## Loading required package: xts

    ## Warning: package 'xts' was built under R version 3.6.3

    ## Loading required package: zoo

    ## Warning: package 'zoo' was built under R version 3.6.3

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

    ## Loading required package: TTR

    ## Warning: package 'TTR' was built under R version 3.6.3

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

    ## Version 0.4-0 included new data defaults. See ?getSymbols.

``` r
library(foreach)
```

    ## Warning: package 'foreach' was built under R version 3.6.3

    ## 
    ## Attaching package: 'foreach'

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     accumulate, when

``` r
#### Possibility 1
#### XLE is an energy ETF, XLI is an industrial ETF, XLP is a consumer staples ETF
#### This is a homogeneous portfolio

mystocks = c("XLE", "XLI", "XLP")
myprices = getSymbols(mystocks, from = "2014-01-01")
```

    ## 'getSymbols' currently uses auto.assign=TRUE by default, but will
    ## use auto.assign=FALSE in 0.5-0. You will still be able to use
    ## 'loadSymbols' to automatically load data. getOption("getSymbols.env")
    ## and getOption("getSymbols.auto.assign") will still be checked for
    ## alternate defaults.
    ## 
    ## This message is shown once per session and may be disabled by setting 
    ## options("getSymbols.warning4.0"=FALSE). See ?getSymbols for details.

``` r
#### Adjusting Stocks
for(ticker in mystocks) {
  expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
  eval(parse(text=expr))
}
```

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query1.finance.yahoo.com/v7/finance/download/XLE?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query2.finance.yahoo.com/v7/finance/download/XLE?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query1.finance.yahoo.com/v7/finance/download/XLI?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query2.finance.yahoo.com/v7/finance/download/XLI?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query1.finance.yahoo.com/v7/finance/download/XLP?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query2.finance.yahoo.com/v7/finance/download/XLP?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

``` r
# Combine all the returns in a matrix
all_returns = cbind(    ClCl(XLEa),
                     ClCl(XLIa),
                     ClCl(XLPa))
all_returns = as.matrix(na.omit(all_returns))

# Compute the returns from the closing prices
pairs(all_returns)
```

![](HW2_files/figure-markdown_github/unnamed-chunk-16-1.png)

``` r
# Sample a random return from the empirical joint distribution
return.today = resample(all_returns, 1, orig.ids=FALSE)

# Update the value of your holdings, assuming equal allocation
total_wealth = 100000
my_weights = c(0.34,0.33,0.33)
holdings = total_wealth*my_weights
holdings = holdings*(1 + return.today)

# Compute new total wealth
holdings
```

    ##            ClCl.XLEa ClCl.XLIa ClCl.XLPa
    ## 2015-09-03  34094.62   33019.4  33224.82

``` r
total_wealth = sum(holdings)
total_wealth
```

    ## [1] 100338.8

``` r
# Now simulate many different possible futures
initial_wealth = 100000
sim1 = foreach(i=1:5000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  weights = c(0.34,0.33,0.33)
  holdings = weights * total_wealth
  n_days = 20
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}

# each row is a simulated trajectory
# each column is a data
head(sim1)
```

    ##               [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]
    ## result.1  99760.15  99730.55 100963.35 100340.71  97960.81  98408.89  98802.94
    ## result.2  98868.41  97414.58  99480.78  98596.91  99073.74  99702.51 100399.36
    ## result.3  98020.63  98896.88 100201.04  98699.01  98908.45  97770.47  98379.94
    ## result.4  99208.00  98905.71  99610.60 100428.13  99753.52 100364.23 101079.81
    ## result.5  99804.06  97128.31  97899.60  98708.27  98642.22  99116.55  99194.23
    ## result.6 101171.13 101160.18 100743.01 100647.68 100591.67  98611.78  99659.81
    ##               [,8]      [,9]     [,10]     [,11]     [,12]     [,13]     [,14]
    ## result.1  98802.44  98874.42  98470.78  98632.94  99142.97  98916.22  98109.16
    ## result.2 101327.16 100616.22 101091.50 101631.27 103788.44 104549.55 105026.87
    ## result.3  98795.58  98186.96  99548.97  99981.24 100271.65  98754.28 100676.54
    ## result.4 101376.33 100571.84 100505.67 100516.44 101325.91 101741.37 101647.92
    ## result.5  98889.17  98548.72  98873.51  98968.34  98066.88  99939.96  97653.77
    ## result.6  99610.65  99632.88 101558.13 101714.69 101596.74 101266.59 101134.24
    ##              [,15]     [,16]     [,17]     [,18]     [,19]     [,20]
    ## result.1  97684.81  94345.62  95147.47  95543.15  96305.43  95551.29
    ## result.2 105590.07 104733.23 105813.05 106168.42 105365.86 106337.57
    ## result.3 100596.34  99625.60  99575.82  99383.33  98984.24  98101.34
    ## result.4 102389.56 102636.98 100506.71 100556.58 100737.52 100384.78
    ## result.5  98612.62  98418.13  98198.25  96584.20  96643.22  96105.41
    ## result.6 101485.08 100909.28  99637.79  99678.38 100449.79 100984.72

``` r
hist(sim1[,n_days], 25)
```

![](HW2_files/figure-markdown_github/unnamed-chunk-16-2.png)

``` r
# Profit/loss
mean(sim1[,n_days])
```

    ## [1] 100315

``` r
mean(sim1[,n_days] - initial_wealth)
```

    ## [1] 315.0134

``` r
hist(sim1[,n_days]- initial_wealth, breaks=30)
```

![](HW2_files/figure-markdown_github/unnamed-chunk-16-3.png)

``` r
# 5% value at risk:
quantile(sim1[,n_days]- initial_wealth, prob=0.05)
```

    ##        5% 
    ## -8381.932

Portfolio 1 is a small and homogeneous portfolio containng : XLE - an
energy ETF, XLI - an industrial ETF, XXP - a consumer staples ETF. We
see a nearly normal distribution, with the returns skewed slightly
towards the negative side.

``` r
#### Possibility 2
#### VWO is an Asia Pacific ETF, GCX is a China ETF, EZU is an Europe ETF, EWJ is a Japan ETF, EWZ is a latin America ETF
#### This is a globally hedged portfolio

mystocks = c("VWO", "GCX", "EZU", "EWJ", "EWZ")
myprices = getSymbols(mystocks, from = "2014-01-01")
```

    ## Warning: GCX contains missing values. Some functions will not work if objects
    ## contain missing values in the middle of the series. Consider using na.omit(),
    ## na.approx(), na.fill(), etc to remove or replace them.

``` r
#### Adjusting Stocks
for(ticker in mystocks) {
  expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
  eval(parse(text=expr))
}
```

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query2.finance.yahoo.com/v7/finance/download/VWO?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query2.finance.yahoo.com/v7/finance/download/VWO?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query1.finance.yahoo.com/v7/finance/download/GCX?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=div&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query2.finance.yahoo.com/v7/finance/download/GCX?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query2.finance.yahoo.com/v7/finance/download/GCX?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query2.finance.yahoo.com/v7/finance/download/EZU?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query2.finance.yahoo.com/v7/finance/download/EZU?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query2.finance.yahoo.com/v7/finance/download/EWJ?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query2.finance.yahoo.com/v7/finance/download/EWJ?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query2.finance.yahoo.com/v7/finance/download/EWZ?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query1.finance.yahoo.com/v7/finance/download/EWZ?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

``` r
# Combine all the returns in a matrix
all_returns = cbind(    ClCl(VWOa),
                     ClCl(GCXa),
                     ClCl(EZUa),
                     ClCl(EWJa),
                     ClCl(EWZa))
all_returns = as.matrix(na.omit(all_returns))

# Compute the returns from the closing prices
pairs(all_returns)
```

![](HW2_files/figure-markdown_github/unnamed-chunk-17-1.png)

``` r
# Sample a random return from the empirical joint distribution
return.today = resample(all_returns, 1, orig.ids=FALSE)

# Update the value of your holdings, assuming equal allocation
total_wealth = 100000
my_weights = c(0.2,0.2,0.2,0.2,0.2)
holdings = total_wealth*my_weights
holdings = holdings*(1 + return.today)

# Compute new total wealth
holdings
```

    ##            ClCl.VWOa ClCl.GCXa ClCl.EZUa ClCl.EWJa ClCl.EWZa
    ## 2017-04-05   19965.1     20000  19856.27  19860.41   19565.9

``` r
total_wealth = sum(holdings)
total_wealth
```

    ## [1] 99247.68

``` r
# Now simulate many different possible futures
initial_wealth = 100000
sim1 = foreach(i=1:5000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  weights = c(0.2,0.2,0.2,0.2,0.2)
  holdings = weights * total_wealth
  n_days = 20
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}

# each row is a simulated trajectory
# each column is a data
head(sim1)
```

    ##               [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]
    ## result.1  98926.98  98600.87  99086.55  99799.03  99709.22  98978.53  98226.14
    ## result.2 100199.85  97470.60  94914.96  95248.89  95206.57  94886.58  85140.90
    ## result.3  99865.90  98723.74  98844.01  98918.12  97145.03  96182.29  96386.12
    ## result.4 110752.29 111504.14 111765.50 109238.08 109224.08 108893.48 108586.43
    ## result.5  99267.89  98900.24  98384.22 103710.74 103188.13 103561.55 104166.52
    ## result.6  99277.10  99775.76 100121.57 100095.41 100833.06 101104.99 101014.87
    ##               [,8]      [,9]     [,10]     [,11]     [,12]     [,13]     [,14]
    ## result.1  98177.20  97957.71  95778.40  96933.55  96925.67  97002.86  92181.38
    ## result.2  84075.82  84356.42  83692.75  85693.20  85330.97  85871.98  89818.88
    ## result.3  94904.27  95351.69  95048.00  94856.24  94186.36  94889.06  93282.33
    ## result.4 123885.33 124761.09 126048.76 126486.41 126822.12 127077.60 127707.46
    ## result.5 105230.73 105614.28 103578.25 104642.44  93983.68  92072.61  92329.96
    ## result.6 100523.01 102297.80 102835.32 102536.68 100427.62 100075.18  99019.92
    ##              [,15]     [,16]     [,17]     [,18]     [,19]     [,20]
    ## result.1  91848.34  91176.84  84524.93  84353.69  84112.46  81846.89
    ## result.2  89715.74  89763.04  87653.60  74724.30  74014.92  76710.23
    ## result.3  94410.33  95153.36  94380.35  94238.48  92971.74  93471.10
    ## result.4 126835.58 124183.20 125077.05 124742.27 124756.53 124634.49
    ## result.5  93931.83  94219.54  94447.30  95698.40  95545.80  95534.68
    ## result.6 102451.39 103251.22 103773.53 104253.03 100983.34 100466.84

``` r
hist(sim1[,n_days], 25)
```

![](HW2_files/figure-markdown_github/unnamed-chunk-17-2.png)

``` r
# Profit/loss
mean(sim1[,n_days])
```

    ## [1] 102588.8

``` r
mean(sim1[,n_days] - initial_wealth)
```

    ## [1] 2588.771

``` r
hist(sim1[,n_days]- initial_wealth, breaks=30)
```

![](HW2_files/figure-markdown_github/unnamed-chunk-17-3.png)

``` r
# 5% value at risk:
quantile(sim1[,n_days]- initial_wealth, prob=0.05)
```

    ##        5% 
    ## -13295.49

Portfolio 2 is a globally hedged portfolio, with holdings across
geographies. VWO is an Asia Pacific ETF, GCX is a China ETF, EZU is an
Europe ETF, EWJ is a Japan ETF, EWZ is a latin America ETF. We see the
simulation giving us a positively skewed return over time. This is
logical, given the geographic hedging. However, it is worthwhile to note
that the frequency is highest for the 0 to slightly negative bin (almost
-10,000).

``` r
#### Possibility 3
#### SPY is a large cap growth ETF, SHY is a short treasury bond ETF, MBB is a mortgage backed security ETF,
#### USO is an oil and gas commodity ETF, DBB is a metals commodity ETF, DBA is an agriculture commodity ETF,
#### VNQ is a real estate ETF, SH is an inverse equities ETF, EUO is a leveraged currencies ETF, FGD is a global equities ETF
#### This is an aggresive portfolio with an inverse ETF as a hedge

mystocks = c("SPY", "SHY", "MBB", "USO", "DBB", "DBA", "VNQ", "SH", "EUO", "FGD")
myprices = getSymbols(mystocks, from = "2014-01-01")
```

    ## pausing 1 second between requests for more than 5 symbols
    ## pausing 1 second between requests for more than 5 symbols
    ## pausing 1 second between requests for more than 5 symbols
    ## pausing 1 second between requests for more than 5 symbols
    ## pausing 1 second between requests for more than 5 symbols
    ## pausing 1 second between requests for more than 5 symbols

``` r
#### Adjusting Stocks
for(ticker in mystocks) {
  expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
  eval(parse(text=expr))
}
```

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query1.finance.yahoo.com/v7/finance/download/SPY?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query2.finance.yahoo.com/v7/finance/download/SPY?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query2.finance.yahoo.com/v7/finance/download/SHY?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query2.finance.yahoo.com/v7/finance/download/SHY?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query2.finance.yahoo.com/v7/finance/download/MBB?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query1.finance.yahoo.com/v7/finance/download/MBB?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query1.finance.yahoo.com/v7/finance/download/USO?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=div&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query2.finance.yahoo.com/v7/finance/download/USO?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query1.finance.yahoo.com/v7/finance/download/USO?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query1.finance.yahoo.com/v7/finance/download/DBB?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=div&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query2.finance.yahoo.com/v7/finance/download/DBB?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query1.finance.yahoo.com/v7/finance/download/DBB?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query2.finance.yahoo.com/v7/finance/download/DBA?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=div&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query2.finance.yahoo.com/v7/finance/download/DBA?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query1.finance.yahoo.com/v7/finance/download/DBA?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query1.finance.yahoo.com/v7/finance/download/VNQ?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query1.finance.yahoo.com/v7/finance/download/VNQ?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query1.finance.yahoo.com/v7/finance/download/SH?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query1.finance.yahoo.com/v7/finance/download/SH?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query2.finance.yahoo.com/v7/finance/download/EUO?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=div&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query1.finance.yahoo.com/v7/finance/download/EUO?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query1.finance.yahoo.com/v7/finance/download/EUO?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query1.finance.yahoo.com/v7/finance/download/FGD?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

    ## Warning in read.table(file = file, header = header, sep = sep,
    ## quote = quote, : incomplete final line found by readTableHeader
    ## on 'https://query1.finance.yahoo.com/v7/finance/download/FGD?
    ## period1=-2208988800&period2=1597708800&interval=1d&events=split&crumb=9GUzf4i0K9V'

``` r
# Combine all the returns in a matrix
all_returns = cbind(    ClCl(SPYa),
                     ClCl(SHYa),
                     ClCl(MBBa),
                     ClCl(USOa),
                     ClCl(DBBa),
                     ClCl(DBAa),
                     ClCl(VNQa),
                     ClCl(SHa),
                     ClCl(EUOa),
                     ClCl(FGDa))
all_returns = as.matrix(na.omit(all_returns))

# Compute the returns from the closing prices
pairs(all_returns)
```

![](HW2_files/figure-markdown_github/unnamed-chunk-18-1.png)

``` r
# Sample a random return from the empirical joint distribution
return.today = resample(all_returns, 1, orig.ids=FALSE)

# Update the value of your holdings, assuming equal allocation
total_wealth = 100000
my_weights = c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)
holdings = total_wealth*my_weights
holdings = holdings*(1 + return.today)

# Compute new total wealth
holdings
```

    ##            ClCl.SPYa ClCl.SHYa ClCl.MBBa ClCl.USOa ClCl.DBBa ClCl.DBAa
    ## 2018-12-19  9850.243   10002.4  10019.24  10234.93  9974.874   9982.58
    ##            ClCl.VNQa ClCl.SHa ClCl.EUOa ClCl.FGDa
    ## 2018-12-19  9890.237 10154.54  9983.733  9877.773

``` r
total_wealth = sum(holdings)
total_wealth
```

    ## [1] 99970.55

``` r
# Now simulate many different possible futures
initial_wealth = 100000
sim1 = foreach(i=1:5000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  weights = c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)
  holdings = weights * total_wealth
  n_days = 20
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}

# each row is a simulated trajectory
# each column is a data
head(sim1)
```

    ##               [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]
    ## result.1 100152.50 100297.64 100225.64 100104.28 100246.29 100705.14 101137.78
    ## result.2  99931.79  99226.06  99503.96  99248.47  98922.02  99114.29  99060.29
    ## result.3 100019.88 100740.25 100725.51  99576.95  99558.82  99841.39 100747.21
    ## result.4 100179.18  99774.96  99902.41 100014.75  99728.22  98918.67  99131.64
    ## result.5 100409.67  99830.42 100042.58 100116.95  99697.48  99897.44  99925.21
    ## result.6 100030.42  99702.58  99036.70  98810.82  98537.61  98260.49  98194.59
    ##               [,8]      [,9]     [,10]     [,11]     [,12]     [,13]     [,14]
    ## result.1 101314.11 101317.62 101523.56 101376.29 101228.48 101265.59 100929.13
    ## result.2  98849.17  98827.14  98988.77  99136.27  98673.94  98424.02  98581.48
    ## result.3 101001.89 101153.23 101223.64 101413.53 101291.22 102054.38 102439.80
    ## result.4  98751.15  98682.39  98743.33  97180.30  97107.59  96766.92  96157.68
    ## result.5 100155.01  99901.97  99856.90  99768.05 100551.34 100789.12 100388.75
    ## result.6  97925.01  97943.82  97488.97  97528.42  97405.58  97586.81  97846.23
    ##              [,15]     [,16]     [,17]     [,18]     [,19]     [,20]
    ## result.1 100835.74 100565.15 100402.75 100796.75 100713.46 100620.31
    ## result.2  98506.92  98923.67  99045.36  99036.21  99275.16  98940.29
    ## result.3 102745.29 103081.54 103039.01 103221.49 102492.32 102617.25
    ## result.4  95777.29  95257.36  95500.20  95860.64  95662.64  95625.42
    ## result.5 100310.55  99935.93 100014.19  99073.07  99099.16  99263.61
    ## result.6  97670.28  97601.04  97096.77  97235.42  97448.47  97695.50

``` r
hist(sim1[,n_days], 25)
```

![](HW2_files/figure-markdown_github/unnamed-chunk-18-2.png)

``` r
# Profit/loss
mean(sim1[,n_days])
```

    ## [1] 99733.13

``` r
mean(sim1[,n_days] - initial_wealth)
```

    ## [1] -266.8681

``` r
hist(sim1[,n_days]- initial_wealth, breaks=30)
```

![](HW2_files/figure-markdown_github/unnamed-chunk-18-3.png)

``` r
# 5% value at risk:
quantile(sim1[,n_days]- initial_wealth, prob=0.05)
```

    ##        5% 
    ## -4403.323

This is an aggressive portolio with multiple ETF’s across bonds, large
caps, mid caps, commodities, inverse equities, leveraged currencies and
global equities. SPY is a large cap growth ETF, SHY is a short treasury
bond ETF, MBB is a mortgage backed security ETF, USO is an oil and gas
commodity ETF, DBB is a metals commodity ETF, DBA is an agriculture
commodity ETF, VNQ is a real estate ETF, SH is an inverse equities ETF,
EUO is a leveraged currencies ETF, FGD is a global equities ETF. Here we
see the high risk, high reward strategy paying dividends. We find the
highest frequency of returns to be positive, though by a slight margin.
Also, it is worthwhile to note here that there is a long tail of very
negative returns (high risk).

\#Problem 4

The goal of this report is to help NutrientH2O find market segments that
may be of interest. For the purposes of this goal, k-means clustering
has been used and several different predictors have been plotted to try
and understand the market segments present with NutrientH2O’s online
audience.

    ## Warning: package 'LICORS' was built under R version 3.6.3

Cleaning data
-------------

It is important to note that the dataset received from Amazon’s
Mechanical Turk Service has to be cleaned in order to better understand
the market. In order to do this any row which had adult or spam content
was deleted from the dataset. The next step taken was to filter out rows
with high density of uncategorized and chatter content. Upon closer
inspection, it was recognized that several rows had high counts in the
chatter column. So, it was decided that any row that had a chatter count
of more than 2 would be deleted from the dataset along with rows that
had content in the uncategorized column.

Scaling data
------------

After cleaning the dataset, steps were taken to scale the dataset.In the
process of scaling, columns for adult, spam, uncategorized and chatter
were deleted. The means and the standard deviations of the dataset were
saved so that they could be reverted back to understandable data points
once clusters were made.

Choosing a value for k
----------------------

In order to create the model, a number had to be chosen for the k. The
Gap statistic method was first used for this purpose. The plot from the
gap statistic method is shown:

\#{r gap, echo = FALSE,fig.align=‘center’, fig.height = 5, fig.width =
8,echo = FALSE, message = FALSE, warning = \#FALSE} \#library(cluster)
\#mark\_gap = clusGap(social, FUN = kmeans, nstart = 50, K.max = 20, B =
100) \#plot(mark\_gap)

As can be seen, no clear dip could be observed. So, it was decided after
several trials that a k value of 7 would be chosen. This was based on
formation of discernible clusters for this specific value of k.

Identifying clusters
--------------------

At first, a combination of different variables were plotted. One of the
first pairs of covariates that showed an interesting cluster was beauty
and fashion.

<img src="HW2_files/figure-markdown_github/beatyfasion-1.png" style="display: block; margin: auto;" />

The concentration of members of the same cluster for higher number of
tweets in fashion and beauty suggests that it is a pretty clear cluster
which can considered as an exploitable market.This cluster can be
thought as consisting of young individuals who are interested in
appearance and fashion trends.

Driven by this finding of clusters formed by individuals who were
interested in fashion and beauty, another plot was created with the
covariates of travel and personal\_fitness.

<img src="HW2_files/figure-markdown_github/travelfit-1.png" style="display: block; margin: auto;" />

In the plot above, two other clusters emerge: one of people who are very
interested in personal fitness and another group for individuals who are
very interested in travel. In order to see if the cluster of people
interested in personal fitness were in someway also interested in
playing sports, another plot was created.

<img src="HW2_files/figure-markdown_github/fitsport-1.png" style="display: block; margin: auto;" />
The above plot suggests that the hypothesis of the existence of a market
segment that is interested in both playing sports and fitness is
invalid. However, in order to further investigate if the brand’s
audience included active college students, a plot was created with the
variables college\_uni and sports\_playing.

<img src="HW2_files/figure-markdown_github/sportuni-1.png" style="display: block; margin: auto;" />
The existence of two distinct clusters is clear in the plot above. One
with individuals who attend college and have an active lifestyle and
another with neither one of those qualities.Since the demographic that
goes to college and plays sports consists of young adults, further
investigation was conducted into the online gaming community since its
membership is dominated to a certain extent by young adults.

<img src="HW2_files/figure-markdown_github/gametrave-1.png" style="display: block; margin: auto;" />
From the above plot, it is clear that there is cluster of individuals
who are heavily involved in online gaming and are interested in
NutrientH2O.In an effort to see if there exists clusters among tech
savvy individuals, the covariates online\_gaming and computers were
plotted.

<img src="HW2_files/figure-markdown_github/componli-1.png" style="display: block; margin: auto;" />

This shows that there are actually two distinct clusters: a group for
online gamers and others who are solely interested in computers. So this
could be interpreted as clusters for younger individuals and another for
possibly older individuals just interested in computers.

In an effort to understand if individuals who were interested in healthy
eating were a target audience, a plot was made using personal\_fitness
and health\_nutrition. As can be seen below, two distinct clusetrs form:
one whose membership consists of individuals interested in healthy
eating and personal fitness and another who are not interested in
either.

<img src="HW2_files/figure-markdown_github/healthfit-1.png" style="display: block; margin: auto;" />

It is worth noting that other covariates such as politics, news, family
etc. were plotted but informative clusters were not found.

Conclusion
----------

From the above analysis it is clear that the brand NutrientH2O appeals
to certain groups of the general populace, namely:

-individuals interested in beauty and fashion: possibly the younger side
of the population who follow influencers. -active individuals attending
college: a demographic primarily between the ages of 18 and 22. -fitness
obsessed individuals. -individuals interested in travel. -online gamers:
once again, most likely a younger portion of the population.
-individuals interested in technology.

\#Problem 5

\#Problem 6

``` r
library(tidyverse)
library(arules)  
```

    ## Warning: package 'arules' was built under R version 3.6.3

    ## 
    ## Attaching package: 'arules'

    ## The following objects are masked from 'package:mosaic':
    ## 
    ##     inspect, lhs, rhs

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode

    ## The following objects are masked from 'package:base':
    ## 
    ##     abbreviate, write

``` r
library(arulesViz)
```

    ## Warning: package 'arulesViz' was built under R version 3.6.3

    ## Loading required package: grid

    ## Registered S3 method overwritten by 'seriation':
    ##   method         from 
    ##   reorder.hclust gclus

``` r
# Reading in as a sparse matrix
grocery_list <- read.transactions("data/groceries.txt", sep=',')
summary(grocery_list)
```

    ## transactions as itemMatrix in sparse format with
    ##  9835 rows (elements/itemsets/transactions) and
    ##  169 columns (items) and a density of 0.02609146 
    ## 
    ## most frequent items:
    ##       whole milk other vegetables       rolls/buns             soda 
    ##             2513             1903             1809             1715 
    ##           yogurt          (Other) 
    ##             1372            34055 
    ## 
    ## element (itemset/transaction) length distribution:
    ## sizes
    ##    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16 
    ## 2159 1643 1299 1005  855  645  545  438  350  246  182  117   78   77   55   46 
    ##   17   18   19   20   21   22   23   24   26   27   28   29   32 
    ##   29   14   14    9   11    4    6    1    1    1    1    3    1 
    ## 
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   2.000   3.000   4.409   6.000  32.000 
    ## 
    ## includes extended item information - examples:
    ##             labels
    ## 1 abrasive cleaner
    ## 2 artif. sweetener
    ## 3   baby cosmetics

``` r
# Looking at items in first ten transactions
arules::inspect(grocery_list[1:10])
```

    ##      items                     
    ## [1]  {citrus fruit,            
    ##       margarine,               
    ##       ready soups,             
    ##       semi-finished bread}     
    ## [2]  {coffee,                  
    ##       tropical fruit,          
    ##       yogurt}                  
    ## [3]  {whole milk}              
    ## [4]  {cream cheese,            
    ##       meat spreads,            
    ##       pip fruit,               
    ##       yogurt}                  
    ## [5]  {condensed milk,          
    ##       long life bakery product,
    ##       other vegetables,        
    ##       whole milk}              
    ## [6]  {abrasive cleaner,        
    ##       butter,                  
    ##       rice,                    
    ##       whole milk,              
    ##       yogurt}                  
    ## [7]  {rolls/buns}              
    ## [8]  {bottled beer,            
    ##       liquor (appetizer),      
    ##       other vegetables,        
    ##       rolls/buns,              
    ##       UHT-milk}                
    ## [9]  {pot plants}              
    ## [10] {cereals,                 
    ##       whole milk}

``` r
# Checking frequencies of first ten items. This will give us an idea on the 'support'
itemFrequency(grocery_list[, 1:10])
```

    ## abrasive cleaner artif. sweetener   baby cosmetics        baby food 
    ##     0.0035587189     0.0032536858     0.0006100661     0.0001016777 
    ##             bags    baking powder bathroom cleaner             beef 
    ##     0.0004067107     0.0176919166     0.0027452974     0.0524656838 
    ##          berries        beverages 
    ##     0.0332486019     0.0260294865

``` r
# Running the apriori rule
grocery_rules <- apriori(grocery_list, parameter = list(support = 0.003, confidence = 0.25, minlen = 2))
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##        0.25    0.1    1 none FALSE            TRUE       5   0.003      2
    ##  maxlen target  ext
    ##      10  rules TRUE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 29 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[169 item(s), 9835 transaction(s)] done [0.01s].
    ## sorting and recoding items ... [136 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 4 5 done [0.00s].
    ## writing ... [1771 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

``` r
summary(grocery_rules)
```

    ## set of 1771 rules
    ## 
    ## rule length distribution (lhs + rhs):sizes
    ##    2    3    4    5 
    ##  228 1207  326   10 
    ## 
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   2.000   3.000   3.000   3.067   3.000   5.000 
    ## 
    ## summary of quality measures:
    ##     support           confidence        coverage             lift        
    ##  Min.   :0.003050   Min.   :0.2500   Min.   :0.003559   Min.   : 0.9932  
    ##  1st Qu.:0.003457   1st Qu.:0.3056   1st Qu.:0.008541   1st Qu.: 1.8089  
    ##  Median :0.004270   Median :0.3846   Median :0.011591   Median : 2.1879  
    ##  Mean   :0.005984   Mean   :0.4055   Mean   :0.016290   Mean   : 2.3102  
    ##  3rd Qu.:0.006202   3rd Qu.:0.4924   3rd Qu.:0.017692   3rd Qu.: 2.6962  
    ##  Max.   :0.074835   Max.   :0.8857   Max.   :0.255516   Max.   :11.4214  
    ##      count       
    ##  Min.   : 30.00  
    ##  1st Qu.: 34.00  
    ##  Median : 42.00  
    ##  Mean   : 58.85  
    ##  3rd Qu.: 61.00  
    ##  Max.   :736.00  
    ## 
    ## mining info:
    ##          data ntransactions support confidence
    ##  grocery_list          9835   0.003       0.25

We see that our data contains 9835 transactions and 169 different items.
Also there were 2159 transactions with only one item purchased, 1643
transactions with two items purchased and so on. Also, item frequencies
tell us the support for the first ten products, like the abrasive
cleaner has 0.36% support, artificial sweetener has 0.32% support, baby
cosmetics have 0.061% support and so on.

``` r
# Looking at a list of first twenty grocery rules, ordered by lift (to find the most interesting ones)
arules::inspect(sort(grocery_rules, by = 'lift')[1:20])
```

    ##      lhs                        rhs                      support confidence    coverage      lift count
    ## [1]  {Instant food products} => {hamburger meat}     0.003050330  0.3797468 0.008032537 11.421438    30
    ## [2]  {flour}                 => {sugar}              0.004982206  0.2865497 0.017386884  8.463112    49
    ## [3]  {processed cheese}      => {white bread}        0.004168785  0.2515337 0.016573462  5.975445    41
    ## [4]  {citrus fruit,                                                                                    
    ##       other vegetables,                                                                                
    ##       tropical fruit,                                                                                  
    ##       whole milk}            => {root vegetables}    0.003152008  0.6326531 0.004982206  5.804238    31
    ## [5]  {other vegetables,                                                                                
    ##       root vegetables,                                                                                 
    ##       tropical fruit,                                                                                  
    ##       whole milk}            => {citrus fruit}       0.003152008  0.4492754 0.007015760  5.428284    31
    ## [6]  {liquor}                => {bottled beer}       0.004677173  0.4220183 0.011082867  5.240594    46
    ## [7]  {citrus fruit,                                                                                    
    ##       other vegetables,                                                                                
    ##       root vegetables,                                                                                 
    ##       whole milk}            => {tropical fruit}     0.003152008  0.5438596 0.005795628  5.183004    31
    ## [8]  {berries,                                                                                         
    ##       whole milk}            => {whipped/sour cream} 0.004270463  0.3620690 0.011794611  5.050990    42
    ## [9]  {herbs,                                                                                           
    ##       whole milk}            => {root vegetables}    0.004168785  0.5394737 0.007727504  4.949369    41
    ## [10] {tropical fruit,                                                                                  
    ##       whole milk,                                                                                      
    ##       yogurt}                => {curd}               0.003965430  0.2617450 0.015149975  4.912713    39
    ## [11] {other vegetables,                                                                                
    ##       whipped/sour cream,                                                                              
    ##       whole milk}            => {butter}             0.003965430  0.2708333 0.014641586  4.887424    39
    ## [12] {butter,                                                                                          
    ##       other vegetables,                                                                                
    ##       whole milk}            => {whipped/sour cream} 0.003965430  0.3451327 0.011489578  4.814724    39
    ## [13] {herbs,                                                                                           
    ##       other vegetables}      => {root vegetables}    0.003863752  0.5000000 0.007727504  4.587220    38
    ## [14] {citrus fruit,                                                                                    
    ##       root vegetables,                                                                                 
    ##       tropical fruit,                                                                                  
    ##       whole milk}            => {other vegetables}   0.003152008  0.8857143 0.003558719  4.577509    31
    ## [15] {onions,                                                                                          
    ##       whole milk}            => {butter}             0.003050330  0.2521008 0.012099644  4.549379    30
    ## [16] {butter,                                                                                          
    ##       other vegetables,                                                                                
    ##       yogurt}                => {tropical fruit}     0.003050330  0.4761905 0.006405694  4.538114    30
    ## [17] {citrus fruit,                                                                                    
    ##       other vegetables,                                                                                
    ##       tropical fruit}        => {root vegetables}    0.004473818  0.4943820 0.009049314  4.535678    44
    ## [18] {beef,                                                                                            
    ##       tropical fruit}        => {root vegetables}    0.003762074  0.4933333 0.007625826  4.526057    37
    ## [19] {onions,                                                                                          
    ##       other vegetables,                                                                                
    ##       whole milk}            => {root vegetables}    0.003253686  0.4923077 0.006609049  4.516648    32
    ## [20] {beef,                                                                                            
    ##       soda}                  => {root vegetables}    0.003965430  0.4875000 0.008134215  4.472540    39

``` r
# Depending on the value of lifts in the previous output, generating a subset of rules to plot (cutoff by lift threshold)
int_rules <- subset(grocery_rules, subset= lift > 4.5)

# Visualizing the subset
plot(int_rules, method="graph", control=list(type="items"))
```

    ## Warning: Unknown control parameters: type

    ## Available control parameters (with default values):
    ## main  =  Graph for 19 rules
    ## nodeColors    =  c("#66CC6680", "#9999CC80")
    ## nodeCol   =  c("#EE0000FF", "#EE0303FF", "#EE0606FF", "#EE0909FF", "#EE0C0CFF", "#EE0F0FFF", "#EE1212FF", "#EE1515FF", "#EE1818FF", "#EE1B1BFF", "#EE1E1EFF", "#EE2222FF", "#EE2525FF", "#EE2828FF", "#EE2B2BFF", "#EE2E2EFF", "#EE3131FF", "#EE3434FF", "#EE3737FF", "#EE3A3AFF", "#EE3D3DFF", "#EE4040FF", "#EE4444FF", "#EE4747FF", "#EE4A4AFF", "#EE4D4DFF", "#EE5050FF", "#EE5353FF", "#EE5656FF", "#EE5959FF", "#EE5C5CFF", "#EE5F5FFF", "#EE6262FF", "#EE6666FF", "#EE6969FF", "#EE6C6CFF", "#EE6F6FFF", "#EE7272FF", "#EE7575FF",  "#EE7878FF", "#EE7B7BFF", "#EE7E7EFF", "#EE8181FF", "#EE8484FF", "#EE8888FF", "#EE8B8BFF", "#EE8E8EFF", "#EE9191FF", "#EE9494FF", "#EE9797FF", "#EE9999FF", "#EE9B9BFF", "#EE9D9DFF", "#EE9F9FFF", "#EEA0A0FF", "#EEA2A2FF", "#EEA4A4FF", "#EEA5A5FF", "#EEA7A7FF", "#EEA9A9FF", "#EEABABFF", "#EEACACFF", "#EEAEAEFF", "#EEB0B0FF", "#EEB1B1FF", "#EEB3B3FF", "#EEB5B5FF", "#EEB7B7FF", "#EEB8B8FF", "#EEBABAFF", "#EEBCBCFF", "#EEBDBDFF", "#EEBFBFFF", "#EEC1C1FF", "#EEC3C3FF", "#EEC4C4FF", "#EEC6C6FF", "#EEC8C8FF",  "#EEC9C9FF", "#EECBCBFF", "#EECDCDFF", "#EECFCFFF", "#EED0D0FF", "#EED2D2FF", "#EED4D4FF", "#EED5D5FF", "#EED7D7FF", "#EED9D9FF", "#EEDBDBFF", "#EEDCDCFF", "#EEDEDEFF", "#EEE0E0FF", "#EEE1E1FF", "#EEE3E3FF", "#EEE5E5FF", "#EEE7E7FF", "#EEE8E8FF", "#EEEAEAFF", "#EEECECFF", "#EEEEEEFF")
    ## edgeCol   =  c("#474747FF", "#494949FF", "#4B4B4BFF", "#4D4D4DFF", "#4F4F4FFF", "#515151FF", "#535353FF", "#555555FF", "#575757FF", "#595959FF", "#5B5B5BFF", "#5E5E5EFF", "#606060FF", "#626262FF", "#646464FF", "#666666FF", "#686868FF", "#6A6A6AFF", "#6C6C6CFF", "#6E6E6EFF", "#707070FF", "#727272FF", "#747474FF", "#767676FF", "#787878FF", "#7A7A7AFF", "#7C7C7CFF", "#7E7E7EFF", "#808080FF", "#828282FF", "#848484FF", "#868686FF", "#888888FF", "#8A8A8AFF", "#8C8C8CFF", "#8D8D8DFF", "#8F8F8FFF", "#919191FF", "#939393FF",  "#959595FF", "#979797FF", "#999999FF", "#9A9A9AFF", "#9C9C9CFF", "#9E9E9EFF", "#A0A0A0FF", "#A2A2A2FF", "#A3A3A3FF", "#A5A5A5FF", "#A7A7A7FF", "#A9A9A9FF", "#AAAAAAFF", "#ACACACFF", "#AEAEAEFF", "#AFAFAFFF", "#B1B1B1FF", "#B3B3B3FF", "#B4B4B4FF", "#B6B6B6FF", "#B7B7B7FF", "#B9B9B9FF", "#BBBBBBFF", "#BCBCBCFF", "#BEBEBEFF", "#BFBFBFFF", "#C1C1C1FF", "#C2C2C2FF", "#C3C3C4FF", "#C5C5C5FF", "#C6C6C6FF", "#C8C8C8FF", "#C9C9C9FF", "#CACACAFF", "#CCCCCCFF", "#CDCDCDFF", "#CECECEFF", "#CFCFCFFF", "#D1D1D1FF",  "#D2D2D2FF", "#D3D3D3FF", "#D4D4D4FF", "#D5D5D5FF", "#D6D6D6FF", "#D7D7D7FF", "#D8D8D8FF", "#D9D9D9FF", "#DADADAFF", "#DBDBDBFF", "#DCDCDCFF", "#DDDDDDFF", "#DEDEDEFF", "#DEDEDEFF", "#DFDFDFFF", "#E0E0E0FF", "#E0E0E0FF", "#E1E1E1FF", "#E1E1E1FF", "#E2E2E2FF", "#E2E2E2FF", "#E2E2E2FF")
    ## alpha     =  0.5
    ## cex   =  1
    ## itemLabels    =  TRUE
    ## labelCol  =  #000000B3
    ## measureLabels     =  FALSE
    ## precision     =  3
    ## layout    =  NULL
    ## layoutParams  =  list()
    ## arrowSize     =  0.5
    ## engine    =  igraph
    ## plot  =  TRUE
    ## plot_options  =  list()
    ## max   =  100
    ## verbose   =  FALSE

![](HW2_files/figure-markdown_github/unnamed-chunk-20-1.png) Here we
have created a graph to see the top rules with the highest ‘lifts’,
which signifies that these rules can probably be insightful as they
happen frequently and not at random. Here are some interesting trends:
1. Customers who bought instant food products are 12 times more likely
to buy hamburger meat! 2. Customers who bought flour are 9 times more
likely to buy sugar. Seems like flour relates to baking more often than
not! 3. Customers who bought processed cheese are 6 times more likely to
buy white bread. Looks like these go well together! 4. An interesting
one is someone buying onions and whole milk is 5 times more likely to
buy butter. Speaking from personal experience, this could relate to the
similar frequency of usage of these items in cooking! 5. Another
interesting one is someone who picks beef and soda is nearly 5 times
more likely to pick root vegetables. Looks like an European cuisine of
meat and veggies with soda accompaniment is popular in the US!

``` r
# Visualizing on the value of support and lift, based on confidence
plotly_arules(grocery_rules, measure = c("support", "lift"), shading = "confidence")
```

    ## Warning: 'plotly_arules' is deprecated.
    ## Use 'plot' instead.
    ## See help("Deprecated")

    ## Warning: plot: Too many rules supplied. Only plotting the best 1000 rules using
    ## measure confidence (change parameter max if needed)

    ## To reduce overplotting, jitter is added! Use jitter = 0 to prevent jitter.

    ## Warning: `arrange_()` is deprecated as of dplyr 0.7.0.
    ## Please use `arrange()` instead.
    ## See vignette('programming') for more help
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

![](HW2_files/figure-markdown_github/unnamed-chunk-21-1.png) The plot
gives an overview of the distribution of support and lift in the grocery
rule set. There are a few high-lift rules, located close to the minimum
support threshold. This means that there are some items which might not
occur frequently but form strong rules with some other products,
according to the apriori principle.

``` r
plotly_arules(grocery_rules, method = "two-key plot")
```

    ## Warning: 'plotly_arules' is deprecated.
    ## Use 'plot' instead.
    ## See help("Deprecated")

    ## Warning: plot: Too many rules supplied. Only plotting the best 1000 rules using
    ## measure NA (change parameter max if needed)

    ## To reduce overplotting, jitter is added! Use jitter = 0 to prevent jitter.

![](HW2_files/figure-markdown_github/unnamed-chunk-22-1.png) Here we see
a plot between support and confidence. We find a lot of points with high
confidence and support in the bottom left corner, extending upwards on
both the x and the y axes. This means that there are items which occur
frequently in the grocery list and they also form strong rules with
other group of items as per the apriori principle.

``` r
# Visualizing frequency of top 10 support items
itemFrequencyPlot(grocery_list, topN = 10)
```

![](HW2_files/figure-markdown_github/unnamed-chunk-23-1.png)

``` r
# For each item in the previous list, building a subset of rules

# inspecting rules for 'whole milk'
ham_rules <- subset(grocery_rules, items %in% 'ham')
arules::inspect(ham_rules[1:10])
```

    ##      lhs                       rhs                support     confidence
    ## [1]  {ham}                  => {yogurt}           0.006710727 0.2578125 
    ## [2]  {ham}                  => {rolls/buns}       0.006914082 0.2656250 
    ## [3]  {ham}                  => {other vegetables} 0.009150991 0.3515625 
    ## [4]  {ham}                  => {whole milk}       0.011489578 0.4414062 
    ## [5]  {ham,yogurt}           => {other vegetables} 0.003050330 0.4545455 
    ## [6]  {ham,other vegetables} => {yogurt}           0.003050330 0.3333333 
    ## [7]  {ham,yogurt}           => {whole milk}       0.003965430 0.5909091 
    ## [8]  {ham,whole milk}       => {yogurt}           0.003965430 0.3451327 
    ## [9]  {ham,rolls/buns}       => {whole milk}       0.003457041 0.5000000 
    ## [10] {ham,whole milk}       => {rolls/buns}       0.003457041 0.3008850 
    ##      coverage    lift     count
    ## [1]  0.026029487 1.848095  66  
    ## [2]  0.026029487 1.444125  68  
    ## [3]  0.026029487 1.816930  90  
    ## [4]  0.026029487 1.727509 113  
    ## [5]  0.006710727 2.349162  30  
    ## [6]  0.009150991 2.389456  30  
    ## [7]  0.006710727 2.312611  39  
    ## [8]  0.011489578 2.474038  39  
    ## [9]  0.006914082 1.956825  34  
    ## [10] 0.011489578 1.635823  34

``` r
# inspecting rules for 'other vegetables'
veg_rules <- subset(grocery_rules, items %in% 'other vegetables')
arules::inspect(veg_rules[1:10])
```

    ##      lhs                   rhs                support     confidence
    ## [1]  {soups}            => {other vegetables} 0.003152008 0.4626866 
    ## [2]  {specialty cheese} => {other vegetables} 0.004270463 0.5000000 
    ## [3]  {flower (seeds)}   => {other vegetables} 0.003762074 0.3627451 
    ## [4]  {turkey}           => {other vegetables} 0.003965430 0.4875000 
    ## [5]  {rice}             => {other vegetables} 0.003965430 0.5200000 
    ## [6]  {spread cheese}    => {other vegetables} 0.003050330 0.2727273 
    ## [7]  {cling film/bags}  => {other vegetables} 0.003253686 0.2857143 
    ## [8]  {salt}             => {other vegetables} 0.003660397 0.3396226 
    ## [9]  {mayonnaise}       => {other vegetables} 0.003558719 0.3888889 
    ## [10] {frozen dessert}   => {other vegetables} 0.003660397 0.3396226 
    ##      coverage    lift     count
    ## [1]  0.006812405 2.391236 31   
    ## [2]  0.008540925 2.584078 42   
    ## [3]  0.010371124 1.874723 37   
    ## [4]  0.008134215 2.519476 39   
    ## [5]  0.007625826 2.687441 39   
    ## [6]  0.011184545 1.409497 30   
    ## [7]  0.011387900 1.476616 32   
    ## [8]  0.010777834 1.755223 36   
    ## [9]  0.009150991 2.009838 35   
    ## [10] 0.010777834 1.755223 36

``` r
# inspecting rules for 'rolls/buns'
bun_rules <- subset(grocery_rules, items %in% 'rolls/buns')
arules::inspect(bun_rules[1:10])
```

    ##      lhs                   rhs          support     confidence coverage  
    ## [1]  {spread cheese}    => {rolls/buns} 0.004168785 0.3727273  0.01118454
    ## [2]  {mustard}          => {rolls/buns} 0.004270463 0.3559322  0.01199797
    ## [3]  {canned fish}      => {rolls/buns} 0.004067107 0.2702703  0.01504830
    ## [4]  {processed cheese} => {rolls/buns} 0.004677173 0.2822086  0.01657346
    ## [5]  {soft cheese}      => {rolls/buns} 0.005388917 0.3154762  0.01708185
    ## [6]  {meat}             => {rolls/buns} 0.006914082 0.2677165  0.02582613
    ## [7]  {butter milk}      => {rolls/buns} 0.007625826 0.2727273  0.02796136
    ## [8]  {ham}              => {rolls/buns} 0.006914082 0.2656250  0.02602949
    ## [9]  {sliced cheese}    => {rolls/buns} 0.007625826 0.3112033  0.02450432
    ## [10] {hamburger meat}   => {rolls/buns} 0.008642603 0.2599388  0.03324860
    ##      lift     count
    ## [1]  2.026408 41   
    ## [2]  1.935099 42   
    ## [3]  1.469380 40   
    ## [4]  1.534285 46   
    ## [5]  1.715151 53   
    ## [6]  1.455496 68   
    ## [7]  1.482738 75   
    ## [8]  1.444125 68   
    ## [9]  1.691921 75   
    ## [10] 1.413211 85

``` r
# inspecting rules for 'soda'
soda_rules <- subset(grocery_rules, items %in% 'soda')
arules::inspect(soda_rules[1:10])
```

    ##      lhs                    rhs    support     confidence coverage   lift    
    ## [1]  {spread cheese}     => {soda} 0.003152008 0.2818182  0.01118454 1.616141
    ## [2]  {canned vegetables} => {soda} 0.003050330 0.2830189  0.01077783 1.623027
    ## [3]  {cake bar}          => {soda} 0.004473818 0.3384615  0.01321810 1.940973
    ## [4]  {chewing gum}       => {soda} 0.005388917 0.2560386  0.02104728 1.468303
    ## [5]  {pasta}             => {soda} 0.004067107 0.2702703  0.01504830 1.549917
    ## [6]  {processed cheese}  => {soda} 0.005287239 0.3190184  0.01657346 1.829473
    ## [7]  {specialty bar}     => {soda} 0.007219115 0.2639405  0.02735130 1.513618
    ## [8]  {misc. beverages}   => {soda} 0.007320793 0.2580645  0.02836807 1.479921
    ## [9]  {candy}             => {soda} 0.008642603 0.2891156  0.02989324 1.657990
    ## [10] {dessert}           => {soda} 0.009862735 0.2657534  0.03711235 1.524015
    ##      count
    ## [1]  31   
    ## [2]  30   
    ## [3]  44   
    ## [4]  53   
    ## [5]  40   
    ## [6]  52   
    ## [7]  71   
    ## [8]  72   
    ## [9]  85   
    ## [10] 97

``` r
# inspecting rules for 'yogurt'
yogurt_rules <- subset(grocery_rules, items %in% 'yogurt')
arules::inspect(yogurt_rules[1:10])
```

    ##      lhs                    rhs      support     confidence coverage   lift    
    ## [1]  {spread cheese}     => {yogurt} 0.003558719 0.3181818  0.01118454 2.280844
    ## [2]  {canned vegetables} => {yogurt} 0.003050330 0.2830189  0.01077783 2.028783
    ## [3]  {frozen fish}       => {yogurt} 0.003253686 0.2782609  0.01169293 1.994676
    ## [4]  {baking powder}     => {yogurt} 0.004575496 0.2586207  0.01769192 1.853888
    ## [5]  {flour}             => {yogurt} 0.004880529 0.2807018  0.01738688 2.012173
    ## [6]  {soft cheese}       => {yogurt} 0.005998983 0.3511905  0.01708185 2.517462
    ## [7]  {cat food}          => {yogurt} 0.006202339 0.2663755  0.02328419 1.909478
    ## [8]  {hard cheese}       => {yogurt} 0.006405694 0.2614108  0.02450432 1.873889
    ## [9]  {butter milk}       => {yogurt} 0.008540925 0.3054545  0.02796136 2.189610
    ## [10] {ham}               => {yogurt} 0.006710727 0.2578125  0.02602949 1.848095
    ##      count
    ## [1]  35   
    ## [2]  30   
    ## [3]  32   
    ## [4]  45   
    ## [5]  48   
    ## [6]  59   
    ## [7]  61   
    ## [8]  63   
    ## [9]  84   
    ## [10] 66

``` r
# inspecting rules for 'bottled water'
water_rules <- subset(grocery_rules, items %in% 'bottled water')
arules::inspect(water_rules[1:10])
```

    ##      lhs                                  rhs                support    
    ## [1]  {bottled water}                   => {soda}             0.028978139
    ## [2]  {bottled water}                   => {whole milk}       0.034367056
    ## [3]  {bottled water,hygiene articles}  => {whole milk}       0.003050330
    ## [4]  {bottled water,frozen vegetables} => {whole milk}       0.003253686
    ## [5]  {bottled water,curd}              => {whole milk}       0.003253686
    ## [6]  {bottled water,napkins}           => {whole milk}       0.003965430
    ## [7]  {bottled water,frankfurter}       => {rolls/buns}       0.003050330
    ## [8]  {bottled beer,bottled water}      => {soda}             0.005083884
    ## [9]  {bottled beer,soda}               => {bottled water}    0.005083884
    ## [10] {bottled beer,bottled water}      => {other vegetables} 0.004168785
    ##      confidence coverage    lift     count
    ## [1]  0.2621895  0.110523640 1.503577 285  
    ## [2]  0.3109476  0.110523640 1.216940 338  
    ## [3]  0.5357143  0.005693950 2.096598  30  
    ## [4]  0.5245902  0.006202339 2.053062  32  
    ## [5]  0.5333333  0.006100661 2.087279  32  
    ## [6]  0.4588235  0.008642603 1.795674  39  
    ## [7]  0.4166667  0.007320793 2.265294  30  
    ## [8]  0.3225806  0.015760041 1.849901  50  
    ## [9]  0.2994012  0.016980173 2.708934  50  
    ## [10] 0.2645161  0.015760041 1.367060  41

``` r
# inspecting rules for 'root vegetables'
root_rules <- subset(grocery_rules, items %in% 'root vegetables')
arules::inspect(root_rules[1:10])
```

    ##      lhs                            rhs               support     confidence
    ## [1]  {rice}                      => {root vegetables} 0.003152008 0.4133333 
    ## [2]  {packaged fruit/vegetables} => {root vegetables} 0.003457041 0.2656250 
    ## [3]  {roll products}             => {root vegetables} 0.003152008 0.3069307 
    ## [4]  {mustard}                   => {root vegetables} 0.003355363 0.2796610 
    ## [5]  {pasta}                     => {root vegetables} 0.003863752 0.2567568 
    ## [6]  {herbs}                     => {root vegetables} 0.007015760 0.4312500 
    ## [7]  {flour}                     => {root vegetables} 0.004677173 0.2690058 
    ## [8]  {oil}                       => {root vegetables} 0.007015760 0.2500000 
    ## [9]  {onions}                    => {root vegetables} 0.009456024 0.3049180 
    ## [10] {chicken}                   => {root vegetables} 0.010879512 0.2535545 
    ##      coverage    lift     count
    ## [1]  0.007625826 3.792102  31  
    ## [2]  0.013014743 2.436961  34  
    ## [3]  0.010269446 2.815917  31  
    ## [4]  0.011997966 2.565733  33  
    ## [5]  0.015048297 2.355600  38  
    ## [6]  0.016268429 3.956477  69  
    ## [7]  0.017386884 2.467978  46  
    ## [8]  0.028063040 2.293610  69  
    ## [9]  0.031011693 2.797452  93  
    ## [10] 0.042907982 2.326221 107

``` r
# inspecting rules for 'tropical fruit'
fruit_rules <- subset(grocery_rules, items %in% 'tropical fruit')
arules::inspect(fruit_rules[1:10])
```

    ##      lhs                              rhs                support     confidence
    ## [1]  {processed cheese}            => {tropical fruit}   0.004270463 0.2576687 
    ## [2]  {grapes}                      => {tropical fruit}   0.006100661 0.2727273 
    ## [3]  {pip fruit}                   => {tropical fruit}   0.020437214 0.2701613 
    ## [4]  {tropical fruit}              => {yogurt}           0.029283172 0.2790698 
    ## [5]  {tropical fruit}              => {other vegetables} 0.035892222 0.3420543 
    ## [6]  {tropical fruit}              => {whole milk}       0.042297916 0.4031008 
    ## [7]  {grapes,tropical fruit}       => {other vegetables} 0.003660397 0.6000000 
    ## [8]  {grapes,other vegetables}     => {tropical fruit}   0.003660397 0.4044944 
    ## [9]  {frozen meals,tropical fruit} => {whole milk}       0.003558719 0.6481481 
    ## [10] {frozen meals,whole milk}     => {tropical fruit}   0.003558719 0.3608247 
    ##      coverage    lift     count
    ## [1]  0.016573462 2.455593  42  
    ## [2]  0.022369090 2.599101  60  
    ## [3]  0.075648195 2.574648 201  
    ## [4]  0.104931368 2.000475 288  
    ## [5]  0.104931368 1.767790 353  
    ## [6]  0.104931368 1.577595 416  
    ## [7]  0.006100661 3.100893  36  
    ## [8]  0.009049314 3.854847  36  
    ## [9]  0.005490595 2.536624  35  
    ## [10] 0.009862735 3.438674  35

``` r
# inspecting rules for 'shopping bags'
bag_rules <- subset(grocery_rules, items %in% 'shopping bags')
arules::inspect(bag_rules[1:10])
```

    ##      lhs                            rhs                support     confidence
    ## [1]  {canned beer,shopping bags} => {soda}             0.003558719 0.3125000 
    ## [2]  {canned beer,soda}          => {shopping bags}    0.003558719 0.2573529 
    ## [3]  {canned beer,shopping bags} => {rolls/buns}       0.003253686 0.2857143 
    ## [4]  {canned beer,rolls/buns}    => {shopping bags}    0.003253686 0.2882883 
    ## [5]  {coffee,shopping bags}      => {whole milk}       0.003152008 0.3369565 
    ## [6]  {napkins,shopping bags}     => {whole milk}       0.003558719 0.4929577 
    ## [7]  {frankfurter,shopping bags} => {rolls/buns}       0.003152008 0.3827160 
    ## [8]  {brown bread,shopping bags} => {soda}             0.003050330 0.3296703 
    ## [9]  {brown bread,shopping bags} => {other vegetables} 0.003050330 0.3296703 
    ## [10] {brown bread,shopping bags} => {whole milk}       0.003558719 0.3846154 
    ##      coverage    lift     count
    ## [1]  0.011387900 1.792092 35   
    ## [2]  0.013828165 2.612039 35   
    ## [3]  0.011387900 1.553344 32   
    ## [4]  0.011286223 2.926022 32   
    ## [5]  0.009354347 1.318730 31   
    ## [6]  0.007219115 1.929264 35   
    ## [7]  0.008235892 2.080714 31   
    ## [8]  0.009252669 1.890558 30   
    ## [9]  0.009252669 1.703788 30   
    ## [10] 0.009252669 1.505250 35

``` r
# inspecting rules for 'sausage'
sausage_rules <- subset(grocery_rules, items %in% 'sausage')
arules::inspect(sausage_rules[1:10])
```

    ##      lhs                           rhs                support     confidence
    ## [1]  {sliced cheese}            => {sausage}          0.007015760 0.2863071 
    ## [2]  {sausage}                  => {soda}             0.024300966 0.2586580 
    ## [3]  {sausage}                  => {rolls/buns}       0.030604982 0.3257576 
    ## [4]  {sausage}                  => {other vegetables} 0.026944586 0.2867965 
    ## [5]  {sausage}                  => {whole milk}       0.029893238 0.3181818 
    ## [6]  {sausage,sliced cheese}    => {whole milk}       0.003152008 0.4492754 
    ## [7]  {sliced cheese,whole milk} => {sausage}          0.003152008 0.2924528 
    ## [8]  {dessert,sausage}          => {other vegetables} 0.003253686 0.5517241 
    ## [9]  {dessert,other vegetables} => {sausage}          0.003253686 0.2807018 
    ## [10] {chicken,sausage}          => {other vegetables} 0.003152008 0.5961538 
    ##      coverage    lift     count
    ## [1]  0.024504321 3.047435  69  
    ## [2]  0.093950178 1.483324 239  
    ## [3]  0.093950178 1.771048 301  
    ## [4]  0.093950178 1.482209 265  
    ## [5]  0.093950178 1.245252 294  
    ## [6]  0.007015760 1.758306  31  
    ## [7]  0.010777834 3.112850  31  
    ## [8]  0.005897306 2.851396  32  
    ## [9]  0.011591256 2.987772  32  
    ## [10] 0.005287239 3.081016  31

Going through this list, we find some interesting rules such as:
1.Customers who buy speciality cheese are 3 times more likely to buy
other vegetables. 2.Customers who buy herbs and other vegetables are 5
times more likely to buy root vegetables. This seems counter-intuitive
since these customrs have already bought vegetables! Interesting
insight. 3.Customers who bought baking power and whole milk are 3 times
more likely to buy other vegetables. This insight can dictate where to
place the vegetables relative to the baking and the dairy section in a
supermart! 4.Customers who buy hard cheese and other veg are 4 times
more likely to pick root vegetables, building on our hunch of placing
the dairy section right next to the vegetable section. 5.Customers who
pick butter, other vegetables and yoghurt are 5 times more likely to
pick tropical fruits (not citrus fruits). This insight could be useful
in placing fruits in a supermart and increasing sales on a perishable
item!
