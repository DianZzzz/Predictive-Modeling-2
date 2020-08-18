knitr::knit(“pm-q2.Rmd”)

The following is an analysis of Austin Bergstrom International Airport.
The data has been grouped in a way that shows optimal times for flying
in regards to month, day of week, time of day, and carrier in order to
avoid arrival and/or departure delays.

    library(mosaic)

    ## Loading required package: dplyr

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## Loading required package: lattice

    ## Loading required package: ggformula

    ## Loading required package: ggplot2

    ## Loading required package: ggstance

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

    ## Loading required package: Matrix

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

    library(tidyverse)

    ## ── Attaching packages ──────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ tibble  3.0.2     ✓ purrr   0.3.4
    ## ✓ tidyr   1.1.0     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ─────────────────────────────────────────────── tidyverse_conflicts() ──
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

    library(ggplot2)

    data1 <-"/Users/trishaschutter/Desktop/MSBA/SUMMER/Predictive Modeling/Second Half/STA380-master/data/ABIA.csv"
    ABIA = read.csv(data1)
    head(ABIA)

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

    # Delays vs. Month
    plot(ABIA$Month, ABIA$ArrDelay, main = "Arrival Delays Occur Most in Dec. and March", xlab= "Month", ylab= "Arrival Delay")

![](PM-Q2_files/figure-markdown_strict/unnamed-chunk-1-1.png) 
This is showing the arrival delays per month. From this, it appears that most
arrival delays occur in December and March.

    plot(ABIA$Month, ABIA$DepDelay, main = "Departure Delay Occur Most in March and August", xlab= "Month", ylab= "Departure Delay")

![](PM-Q2_files/figure-markdown_strict/unnamed-chunk-2-1.png) 
To compare arrival delays with depature delays, departure delays were aslo ran
against months. Departure delays occur most in March and August. As
March was present in both results, March seems to be full of delays of
both kinds.

    # Delays vs. Day of Week
    plot(ABIA$DayOfWeek, ABIA$ArrDelay, main = "Tuesday Has Lowest Arrival Delay", xlab= "Day of Week", ylab= "Arrival Delay")

![](PM-Q2_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    plot(ABIA$DayOfWeek, ABIA$DepDelay, main = "Tuesday Has Lowest Departure Delay", xlab= "Day of Week", ylab= "Departure Delay")

![](PM-Q2_files/figure-markdown_strict/unnamed-chunk-3-2.png) 
These are a couple plots that represent what day is best to fly on in order to
avoid delays. Tuesday was found as the best day to fly in order to avoid
delays.

    # Departure Time vs. Delay
    ggplot(data = ABIA) + 
      geom_point(mapping = aes(x = DepTime, y = DepDelay)) +
      ggtitle("Later Departure Flights Have Higher Delays")

    ## Warning: Removed 1413 rows containing missing values (geom_point).

![](PM-Q2_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    # Arrival Time vs. Delay
    ggplot(data = ABIA) + 
      geom_point(mapping = aes(x = ArrTime, y = ArrDelay)) +
      ggtitle("      Arrival Delays Greatest At Night")

    ## Warning: Removed 1601 rows containing missing values (geom_point).

![](PM-Q2_files/figure-markdown_strict/unnamed-chunk-4-2.png) 
Now that month and day have been narrowed down, the next step was to narrow down
a time to fly. From these plots, it appears that mornings are best to
fly. However this does not mean morning as in midnight, but rather
around 5AM. The increase around midnight is rollover from the high level
of delays as it gets later in the night.

    ################################################

    # Departure Delay vs. Distance
    ggplot(data = ABIA) + 
      geom_point(mapping = aes(x = DepDelay, y = Distance, color = Distance)) + 
      ggtitle("Departure Delay Greatest For Small and Large Distances")

    ## Warning: Removed 1413 rows containing missing values (geom_point).

![](PM-Q2_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    # Arrival Delay vs. Distance
    ggplot(data = ABIA) + 
      geom_point(mapping = aes(x = ArrDelay, y = Distance, color = Distance)) + 
      ggtitle("Arrival Delay Greatest For Small and Large Distances")

    ## Warning: Removed 1601 rows containing missing values (geom_point).

![](PM-Q2_files/figure-markdown_strict/unnamed-chunk-5-2.png) 
Next, I wanted to see whether distance was a factor in delays. From these plots,
we can see that delays are greatest both on the extremes. This makes
sense as shorter flights don’t have enough distance to make up for lost
time and long distances tend to require more preparations.

    # Arrival Delay vs. Carrier
    ggplot(data = ABIA) + 
      geom_point(mapping = aes(x = ArrDelay, y = UniqueCarrier, color = UniqueCarrier)) + 
      ggtitle("Arrival Delay Greatest Among JetBlue and SouthWest")

    ## Warning: Removed 1601 rows containing missing values (geom_point).

![](PM-Q2_files/figure-markdown_strict/unnamed-chunk-6-1.png)

    # Departure Delay vs. Carrier
    ggplot(data = ABIA) + 
      geom_point(mapping = aes(x = DepDelay, y = UniqueCarrier, color = UniqueCarrier)) + 
      ggtitle("Departure Delay Greatest Among JetBlue")

    ## Warning: Removed 1413 rows containing missing values (geom_point).

![](PM-Q2_files/figure-markdown_strict/unnamed-chunk-6-2.png) 
These plots are testing whether a specific carrier tends to have more delays
than others. Both Jetblue and SouthWest tend to have the most arrival
delays, while JetBlue also seems to have the greatest departure delays.
To avoid delays, one might want to fly with someone other than JetBlue.

    ###
    # facets
    ###

    #####################################################################
    # Departure Delay vs. Day of Week vs. Month
    ggplot(data = ABIA) + 
      geom_point(mapping = aes(x = DepDelay, y = DayOfWeek)) + 
      facet_wrap(~ Month, nrow = 2) +
      ggtitle("What Day to Fly, Per Month to Avoid Departure Delays")

    ## Warning: Removed 1413 rows containing missing values (geom_point).

![](PM-Q2_files/figure-markdown_strict/unnamed-chunk-7-1.png)

    # Arrival Delay vs. Day of Week vs. Month
    ggplot(data = ABIA) + 
      geom_point(mapping = aes(x = ArrDelay, y = DayOfWeek)) + 
      facet_wrap(~ Month, nrow = 2) +
      ggtitle("What Day to Fly, Per Month to Avoid Arrival Delays")

    ## Warning: Removed 1601 rows containing missing values (geom_point).

![](PM-Q2_files/figure-markdown_strict/unnamed-chunk-7-2.png)

    ###################################################################33

In preparation for your next flight, you might want to know what a
certain day looks like on the particular month you need to travel. These
two plots will point you in the right direction, where you will first be
able to look at the month that you are flying and then find the smallest
line, indicating that is the day with the fewest delays during that
month.
