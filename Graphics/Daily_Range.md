Graphics Showing Diurnal Range of pCO<sub>2</sub> by Season, in Casco Bay Maine
================
Curtis C. Bohlen

-   [Introduction](#introduction)
-   [Load Libraries](#load-libraries)
-   [Load Data](#load-data)
    -   [Establish Folder References](#establish-folder-references)
    -   [Load Data](#load-data-1)
-   [Graphic Ideas](#graphic-ideas)
    -   [Preliminary Boxplot](#preliminary-boxplot)
    -   [DotPlot](#dotplot)
    -   [Plot by Day of Year, not Month](#plot-by-day-of-year-not-month)
        -   [Alternate X Axis for DOY Graphic](#alternate-x-axis-for-doy-graphic)
-   [Diurnal Range by Temperature and Month](#diurnal-range-by-temperature-and-month)
-   [Diurnal Range by Daily Median](#diurnal-range-by-daily-median)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

Introduction
============

This R Notebook looks at patterns of diurnal range of pCO<sub>2</sub> over the course of the year. The first step is to extract data summaries by the day. In addition to the daily range, which is sensitive to outliers, we also calculate more resistant measures of variability, including IQRs and 80% ranges. For graphic presentation, selection of the statistic makes little difference with such large sample sizes.

Once data is extracted by day, we can depict (or model -- see related notebook in the Anlysis folder) daily ranges as a function of explanatory variables, such as time if year, median pCO\[2\], temperature, temperature range, etc.

We assume the daily values are autocorrelated. While detrending variables by daily medians We can examine relationships with day of the year, month, or season, and see how much detail we need.

Load Libraries
==============

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.1     v dplyr   1.0.0
    ## v tidyr   1.1.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ---------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readr)

library(CBEPgraphics)

load_cbep_fonts()
```

Load Data
=========

Establish Folder References
---------------------------

``` r
sibfldnm <- 'Derived_Data'
parent   <- dirname(getwd())
sibling  <- file.path(parent,sibfldnm)

fn    <- 'CascoBayOAData.csv'
fpath <- file.path(sibling,fn)
```

Load Data
---------

The following loads existing data, including a "temperature corrected" pCO2 value based on Takehashi et al. 2002. It then collapses that data to daily summaries.

> Takahashi, Taro & Sutherland, Stewart & Sweeney, Colm & Poisson, Alain & Metzl, Nicolas & Tilbrook, Bronte & Bates, Nicholas & Wanninkhof, Rik & Feely, Richard & Chris, Sabine & Olafsson, Jon & Nojiri, Yukihiro. (2002). Global sea-air CO2 flux based on climatological surface ocean pCO2, and seasonal biological and temperature effects. Deep Sea Research Part II: Topical Studies in Oceanography. 49. 1601-1622. 10.1016/S0967-0645(02)00003-6.

(See the "Data\_Review\_And\_Filtering" R Notebook for details on why and how we calculated temperature-corrected pCO2 values.)

``` r
all_data <- read_csv(fpath,
                     col_types = cols(dd = col_integer(), 
                                      doy = col_integer(),
                                      hh = col_integer(),
                                      mm = col_integer(),
                                      yyyy = col_integer())) %>%
  select(c(13, 1:4, 14, 5:6, 8, 7 ,16, 9:12))    # Reorder columns


daily_data <- all_data %>%
  select(-hh, -yyyy, -mm, -dd, -doy) %>%         # Will recalculate these 
  mutate(the_date = as.Date(datetime)) %>%
  select(-datetime) %>%
  group_by(the_date) %>%
  summarise_at(c("temp", "sal", "co2", "co2_corr", "do", "ph"),
               c(m    = function(x) median(x, na.rm=TRUE),
                 r    = function(x) {suppressWarnings(max(x, na.rm=TRUE) -
                                                        min(x, na.rm=TRUE))},
                iqr   = function(x) IQR(x, na.rm=TRUE),
                p80r  = function(x) {as.numeric(quantile(x, 0.90, na.rm=TRUE) -
                       quantile(x, 0.10, na.rm=TRUE))},
                n     = function(x) sum((! is.na(x) )))) %>%
  mutate(yyyy = as.numeric(format(the_date, format = '%Y')),
         mm   = as.numeric(format(the_date, format = '%m')),
         dd   = as.numeric(format(the_date, format = '%d')),
         doy  = as.numeric(format(the_date, format = '%j')),
         Month = factor(mm, levels=1:12, labels = month.abb)
         )
```

Graphic Ideas
=============

Preliminary Boxplot
-------------------

``` r
plt <- daily_data %>%
  select_at(vars(contains('co2_corr'), dd, doy, mm, Month, yyyy)) %>%
  filter(! is.na(co2_corr_r) & ! is.na(Month)) %>%

  ggplot(aes(x=Month)) +
  geom_boxplot(aes(y= co2_corr_r), fill = 'lightblue') +
  #xlab('Month') + 
  ylab(expression(paste('Daily Range of ', pCO[2*(cor)], ' (', mu, 'Atm)'))) +
  theme_cbep() +
   theme(axis.text.x=element_text(angle=90, vjust = .25))
plt
```

    ## Warning: Removed 241 rows containing non-finite values (stat_boxplot).

![](Daily_Range_files/figure-markdown_github/preliminary_graphic-1.png) \#\# Better Boxplot

``` r
# compute lower and upper whiskers
(ylim1 = boxplot.stats(daily_data$co2_corr_r)$stats[c(1, 5)])
```

    ## [1]   0.00 296.31

``` r
ylim2 <- c(0,400)

# scale y limits based on ylim1
plt + coord_cartesian(ylim = ylim2)
```

    ## Warning: Removed 241 rows containing non-finite values (stat_boxplot).

![](Daily_Range_files/figure-markdown_github/narrower_RANGE-1.png)

``` r
ggsave('diurnalrangeboxplot.pdf', device=cairo_pdf)
```

    ## Saving 7 x 5 in image

    ## Warning: Removed 241 rows containing non-finite values (stat_boxplot).

DotPlot
-------

``` r
plt <- daily_data %>%
  select_at(vars(contains('co2_corr'), dd, doy, mm, Month, yyyy)) %>%
  filter(! is.na(co2_corr_r) & ! is.na(Month)) %>%

  ggplot(aes(x=Month, y=co2_corr_r)) +
  geom_dotplot(binwidth = 20, binaxis= 'y', stackdir = 'center',dotsize = .25,
               pch=1, color = cbep_colors()[1]) +
  #xlab('Month') + 
  ylab(expression(paste('Daily Range of ', pCO[2*(cor)], ' (', mu, 'Atm)'))) +
  theme_minimal() +
  theme_cbep() +
  theme(axis.text.x=element_text(angle=90, vjust = .3))
```

    ## Warning: Ignoring unknown parameters: shape

``` r
plt
```

    ## Warning: Removed 241 rows containing non-finite values (stat_bindot).

![](Daily_Range_files/figure-markdown_github/unnamed-chunk-1-1.png)

Plot by Day of Year, not Month
------------------------------

``` r
plt <- daily_data %>%
  select_at(vars(contains('CO2_corr'), dd, doy, mm, Month, yyyy)) %>%
  filter(! is.na(co2_corr_m)) %>%
  filter(co2_corr_n >= 20) %>%                  # Drop any "short" days
  ggplot(aes(x=doy, y = co2_corr_r)) +
  geom_point(color = cbep_colors()[1]) + 
  geom_smooth(method = 'gam', formula = y~s(x,bs='cc'), # k can be specified
              se = FALSE, color = cbep_colors()[3]) +
  xlab('Day of the Year') + 
  ylab(expression(paste('Daily Range of ', pCO[2*(cor)], ' (', mu, 'Atm)'))) +
  theme_cbep()
  
plt +scale_x_continuous(breaks = c(0,90, 180, 270, 360))
```

![](Daily_Range_files/figure-markdown_github/doy_plot-1.png)

### Alternate X Axis for DOY Graphic

``` r
mlengths <- c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
mbreaks <-  cumsum(mlengths)
mbreaks  <- mbreaks
mlabs    <- c(month.abb, '')

plt + coord_cartesian(ylim = ylim2) +
  xlab(NULL) +
  scale_x_continuous(breaks = mbreaks, labels = mlabs) +
  theme(axis.text.x=element_text(angle=90, vjust = 2))
```

![](Daily_Range_files/figure-markdown_github/better_x_axis-1.png)

``` r
ggsave('diurnalrangepoints.pdf', device=cairo_pdf, width = 7, height = 5)
```

Diurnal Range by Temperature and Month
======================================

``` r
tmp <- daily_data %>%
  select(temp_m, do_m, co2_corr_n, co2_corr_m, co2_corr_r, dd, doy, mm, yyyy) %>%
  mutate(Month = factor(mm, levels = 1:12, labels = month.abb)) %>%
  filter(! is.na(co2_corr_r), co2_corr_n >= 20)

plt <- ggplot(tmp, aes(temp_m, co2_corr_r)) +
  geom_point(aes(color = Month)) +
  geom_smooth(method = 'gam', formula = y~s(x, bs='tp'), se = FALSE) +
  ylab(expression(paste('Daily Range of ', pCO[2*(cor)], ' (', mu, 'Atm)'))) +
  xlab('Daily Median Temperature (C)') +
  theme_cbep()

plt + coord_cartesian(ylim = ylim2) 
```

![](Daily_Range_files/figure-markdown_github/range_by_temperature-1.png)

Diurnal Range by Daily Median
=============================

``` r
tmp <- daily_data %>%
  select(temp_m, do_m, co2_corr_n, co2_corr_m, co2_corr_r, dd, doy, mm, yyyy) %>%
  mutate(Month = factor(mm, levels = 1:12, labels = month.abb)) %>%
  filter(! is.na(co2_corr_r), co2_corr_n >= 20)

plt <- ggplot(tmp, aes(co2_corr_m, co2_corr_r)) +
  geom_point(aes(color = Month)) +
  geom_smooth(method = 'lm') +
  ylab("Daily Range of pCO[2*(cor)], (uAtm)") + 
  xlab(expression(paste('Median ', pCO[2], ' (', mu, 'Atm)'))) +
  theme_cbep()

plt + coord_cartesian(ylim = ylim2) 
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Daily_Range_files/figure-markdown_github/range_by_median-1.png) What is most interesting here is seeing how temperature range and temperature levels are seasonally decoupled. High pCO<sub>2</sub> in late fall and winter is not associated with high ranges. The high ranges are summer and early fall

``` r
plt <- ggplot(tmp, aes(co2_corr_m, co2_corr_r)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ylab("Diurnal Range of pCO2, (uAtm)") + 
  xlab(expression(paste('Median', pCO[2*(cor)], ' (', mu, 'Atm)'))) +
  theme_cbep() +
  facet_wrap(~Month)
plt
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Daily_Range_files/figure-markdown_github/facet_plot-1.png) That suggests a robust ANCOVA analysis would show both different slopes and different offsets.
