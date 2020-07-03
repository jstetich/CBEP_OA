Graphics Showing Diurnal Fluctuations in pH and Corrected CO<sub>2</sub>
================
Curtis C. Bohlen

  - [Introduction](#introduction)
  - [Load Libraries](#load-libraries)
  - [Load Data](#load-data)
      - [Establish Folder References](#establish-folder-references)
      - [Load Data](#load-data-1)
  - [Diurnal Deviations](#diurnal-deviations)
  - [Plot Daily pCO2 Deviations By
    Month](#plot-daily-pco2-deviations-by-month)
      - [Plotting curves against scatter
        plots](#plotting-curves-against-scatter-plots)
  - [Combined Graphic](#combined-graphic)
  - [Corrected pCO2 Graph](#corrected-pco2-graph)
  - [pH Graph](#ph-graph)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

# Introduction

This notebook seeks to document diurnal patterns in pH and temperature
corrected pCO<sub>2</sub> based on equations in Takehashi et al. 2002.
The approach is to look at 24 hour patterns in the deviations from daily
averages. We expect these patterns do vary seasonally, so we analyze
patterns by month, as a convenient way to look at annual variation.

> Takahashi, Taro & Sutherland, Stewart & Sweeney, Colm & Poisson, Alain
> & Metzl, Nicolas & Tilbrook, Bronte & Bates, Nicholas & Wanninkhof,
> Rik & Feely, Richard & Chris, Sabine & Olafsson, Jon & Nojiri,
> Yukihiro. (2002). Global sea-air CO2 flux based on climatological
> surface ocean pCO2, and seasonal biological and temperature effects.
> Deep Sea Research Part II: Topical Studies in Oceanography. 49.
> 1601-1622. 10.1016/S0967-0645(02)00003-6.

(See the “Data\_Review\_And\_Filtering” R Notebook for details on why
and how we calculated temperature-corrected pCO2 values.)

# Load Libraries

``` r
library(tidyverse)
```

    ## -- Attaching packages ----------------------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.1     v dplyr   1.0.0
    ## v tidyr   1.1.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts -------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.8-31. For overview type 'help("mgcv-package")'.

``` r
library(CBEPgraphics)

load_cbep_fonts()
```

# Load Data

## Establish Folder References

``` r
sibfldnm <- 'Derived_Data'
parent   <- dirname(getwd())
sibling  <- file.path(parent,sibfldnm)

fn    <- 'CascoBayOAData.csv'
fpath <- file.path(sibling,fn)
```

## Load Data

``` r
all_data <- read_csv(fpath,
                     col_types = cols(dd = col_integer(), 
                                      doy = col_integer(),
                                      hh = col_integer(),
                                      mm = col_integer(),
                                      yyyy = col_integer())) %>%
  select(c(13, 1:4, 14, 5:6, 8, 7 ,16, 9:12))
```

# Diurnal Deviations

We calculate hourly deviation from daily averages. This can be done
using linear models and extracting residuals, but the code is cleaner
using the Tidyverse. You could make the code shorter using mutate\_at(),
but being explicit here is clearer for readers. An alternative would be
to calculate deviations from daily medians instead of daily averages. In
this setting, it probably makes little differnce, but we did not explore
that alternative.

``` r
new_data <- all_data %>%
  mutate(month = factor(mm, levels = 1:12, labels=month.abb)) %>%
  group_by(yyyy, mm, dd) %>%
  
  # Calculate sample sizes for each daay.
  mutate(co2_n      = sum(! is.na(co2)),
         co2_corr_n = sum(! is.na(co2_corr)),
         ph_n       = sum(! is.na(ph))) %>%
  
  # Calculate centered but not scaled values, day by day.
  mutate(co2_res      = scale(co2, scale = FALSE),
         co2_corr_res = scale(co2_corr, scale = FALSE),
         ph_res       = scale(ph, scale = FALSE)) %>%
  ungroup(yyyy, mm, dd) %>%
  
  # Replace data from any days with less than 20 hours of data with NA
  mutate(co2_res      = ifelse(co2_n>=20, co2_res, NA),
         co2_corr_res = ifelse(co2_corr_n>=20, co2_corr_res, NA),
         ph_res       = ifelse(ph_n>=20, ph_res, NA)) %>%
  
  # Delete the daily sample size variable
  select(-contains("_n")) %>%
  mutate(Month = factor(mm, labels=month.abb))
```

# Plot Daily pCO2 Deviations By Month

By using ggplot’s geom\_smooth(), we can look graphically at GAM models
by month. This is a convenient way to explore diurnal patterns by
season.

It is worth noting that these models are likely to over-fit the data,
because they do not take into account temporal autocorrelations. For
graphical presentation, this is adequate, as detrending the time-series
by day removed most of the autocorrelation structure that we do not wish
to model directly. Large sample sizes also reduce the probability that
over-fitting will matter.

Since GAM models use heuristics (based in part on the assumption of
independent errors) to select an optimal level of smoothing, they are
likely to try to fit some details that are an artifact of
autocorrelation. While it is possible to fit GAMM models with a formal
autocorrelation structure here, those models are slow, and raise other
complications.

## Plotting curves against scatter plots

``` r
plt <- ggplot(new_data, aes(hh, co2_corr_res)) + geom_jitter(alpha=0.05) +
  geom_smooth(aes(color = Month), method = gam,
              formula = y~s(x, bs='cc'), se=FALSE) +
  theme_cbep() +
  xlab('Hour of Day') +
  ylab(expression (atop(pCO[2*(cor)]~(mu*Atm), Difference~From~Daily~Average)))
plt
```

    ## Warning: Removed 6420 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 6420 rows containing missing values (geom_point).

![](Diurnal_Patterns_files/figure-gfm/scatter_plot_with_lines-1.png)<!-- -->

``` r
plt + facet_wrap(~ mm)
```

    ## Warning: Removed 6420 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 6420 rows containing missing values (geom_point).

![](Diurnal_Patterns_files/figure-gfm/facet_plot-1.png)<!-- -->

# Combined Graphic

Lets combine those into faceted graphs. To do so we first must
reorganize the data into “long” format.

``` r
long_data <- new_data %>% select(-c(7:15)) %>%  # Drop all raw observations; retain diurnal residuals
                                                # retain only pC02_corr f pH residuals
  pivot_longer(contains("_res"), names_to='metric') %>%
  mutate(metric= sub("_res", "", metric))       #simplify names
```

Notice that the selection of the dimension of the periodic basis of the
smooths in the GAM (signaled in k=6) is essentially selected here by eye
to create a visual balance between simplicity and complexity. The
default here probably over-fits, as described above. We explore the
implied GAM (and related GAMM) further in the another R Notebook in the
“Analysis” folder.

Unfortunately, ggplot does not make it easy to change the labels of
facets. The key here is to alter the labels of the factor used to
specify facets to include all the plotmath to generate the desired label

``` r
labs <- c(expression(paste(pCO[2*(cor)], ' (', mu, 'Atm)')), 'pH')
names(labs) <- unique(long_data$metric)[2:3]

tmp <- long_data %>%
  filter( ! metric == 'co2') %>%
  mutate(metric = factor(metric, levels = names(labs), labels = labs))

plt <-
  ggplot(tmp, aes(hh,value, color = month)) +
  geom_smooth(aes(color = month), method = "gam",
              formula = y~s(x, bs='cc', k=6), se=FALSE) +

  theme_cbep() +
  theme(panel.spacing = unit(2, "lines")) +
  
  xlab('Hour of the Day') +
  ylab('Difference from Daily Average') +
  facet_wrap(~metric, nrow=1, scales='free_y',
                   labeller = label_parsed ) +
  scale_color_discrete(name = "Month") + 
  scale_x_continuous(breaks = c(0,6,12,18,24)) #+
  #ggtitle('Daily Fluctuations')
plt
```

    ## Warning: Removed 18451 rows containing non-finite values (stat_smooth).

![](Diurnal_Patterns_files/figure-gfm/combined_plot-1.png)<!-- -->

``` r
ggsave('dailyco2andphbymonth.pdf', device = cairo_pdf, width = 8, height = 5)
```

    ## Warning: Removed 18451 rows containing non-finite values (stat_smooth).

``` r
ggsave('dailyco2andphbymonth.png', type = 'cairo', width = 8, height = 5)
```

    ## Warning: Removed 18451 rows containing non-finite values (stat_smooth).

Daily fluctuations are at their peak in the late summer and early fall,
with highest pCO<sub>2</sub> observed in the morning, soon after
sunrise. Lowest values are a few hours after sunset. By mid-winter, the
fluctuations are smaller, with peaks later in the day, around sunset,
which suggests CO2 is being released all day long, and reabsorbed at
night, which I don’t quite understand. Also note that timing or
pCO<sub>2</sub> and pH fluctuations don’t match consistently across
months.

# Corrected pCO2 Graph

``` r
plt <- new_data %>%
  ggplot(aes(hh,co2_corr_res, color = month)) +
  geom_smooth(aes(color=month), method = "gam",
              formula = y~s(x, bs='cc', k=6), se=FALSE, lwd=0.75) +
  # annotate(geom = "text", label = expression(atop("Temperature", "Corrected"~pCO[2])),
  #          x = 24, y = 30,
  #          color = "black", hjust=1, size = 4) +
  theme_cbep(base_size = 12) +
  xlab('Hour of Day') +
  ylab(expression (atop(pCO[2*(cor)]~(mu*Atm), Difference~From~Daily~Average))) +
  scale_color_discrete(name = "Month") + 
  theme(legend.key.width = unit(0.25,"in"),
        legend.text      = element_text(size = 10)
        ) +
  scale_x_continuous(breaks = c(0,6,12,18,24)) #+
  #ggtitle(expression(Daily~pCO[2]))
plt
```

![](Diurnal_Patterns_files/figure-gfm/pco2_figure-1.png)<!-- -->

``` r
ggsave('dailyCO2bymonth.pdf', device=cairo_pdf, width = 4, height = 4)
ggsave('dailyCO2bymonth.png', type='cairo', width = 4, height = 4)
```

# pH Graph

``` r
plt <- new_data %>%
  ggplot(aes(hh,ph_res, color = month)) +
  geom_smooth(aes(color=month), method = "gam",
              formula = y~s(x, bs='cc', k=6), se=FALSE) +
  theme_cbep(base_size = 12) +
  theme(legend.key.width = unit(0.25,"in"),
        legend.text      = element_text(size = 10)
        ) +
  xlab('Hour of Day') +
  ylab(expression (atop(pH, Difference~From~Daily~Average))) +
  scale_color_discrete(name = "Month") + 
  scale_linetype_discrete(name = "Month") + 
  theme(legend.key.width=unit(0.25,"in")) +
  scale_x_continuous(breaks = c(0,6,12,18,24)) #+
  #ggtitle(expression(Daily~pH))
plt
```

![](Diurnal_Patterns_files/figure-gfm/phfigure-1.png)<!-- -->

``` r
ggsave('dailyphbymonth.pdf', device=cairo_pdf, width = 4, height = 4)
ggsave('dailyphbymonth.png', type='cairo', width = 4, height = 4)
```