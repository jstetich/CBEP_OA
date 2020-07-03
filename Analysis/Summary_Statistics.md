Summary Statistics for Casco Bay OA Data 2015-2018
================
Curtis C. Bohlen

  - [Load Libraries](#load-libraries)
  - [Load Data](#load-data)
      - [Establish Folder References](#establish-folder-references)
      - [Load The Data](#load-the-data)
  - [Overall Summary Statistics](#overall-summary-statistics)
  - [Omega Aragonite Observations and Percentage Below Levels of
    Concern](#omega-aragonite-observations-and-percentage-below-levels-of-concern)
  - [Daily Omega Aragonite (medians) Observations and and Percentage
    Below Levels of
    Concern](#daily-omega-aragonite-medians-observations-and-and-percentage-below-levels-of-concern)

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
library(readr)

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

## Load The Data

The following loads existing data, including a “Temperature Adjusted”
pCO<sub>2</sub> value based on Takehashi et al. 2002. It then collapses
that data to daily summaries.

``` r
all_data <- read_csv(fpath,
                     col_types = cols(dd = col_integer(), 
                                      doy = col_integer(),
                                      hh = col_integer(),
                                      mm = col_integer(),
                                      yyyy = col_integer())) %>%
  select(c(13, 1:4, 14, 5:6, 8, 7 ,16, 9:12))

daily_data <- all_data %>%
  select(-hh, -yyyy, -mm, -dd, -doy) %>%         # Will recalculate these 
  mutate(the_date = as.Date(datetime)) %>%
  select(-datetime) %>%
  group_by(the_date) %>%
  summarise_at(c("temp", "sal", "co2", "co2_corr", "do", "ph", 'omega_a'),
               c(m    = function(x) median(x, na.rm=TRUE),
                 r    = function(x) {suppressWarnings(max(x, na.rm=TRUE) -
                                                        min(x, na.rm=TRUE))},
                iqr  = function(x) IQR(x, na.rm=TRUE),
                p80r = function(x) {as.numeric(quantile(x, 0.90, na.rm=TRUE) -
                       quantile(x, 0.10, na.rm=TRUE))})) %>%
  mutate(yyyy = as.numeric(format(the_date, format = '%Y')),
         mm   = as.numeric(format(the_date, format = '%m')),
         dd   = as.numeric(format(the_date, format = '%d')),
         doy  = as.numeric(format(the_date, format = '%j')),
         Month = factor(mm, levels=1:12, labels = month.abb)
         )
```

# Overall Summary Statistics

This is legacy code. It would be easier today to develop this directly
in the tidyverse.

``` r
the.mins     <- sapply(all_data[7:15], min, na.rm=TRUE)
the.medians  <- sapply(all_data[7:15], median, na.rm=TRUE)
the.means    <- sapply(all_data[7:15], mean, na.rm=TRUE)
the.maxes    <- sapply(all_data[7:15], max, na.rm=TRUE)
the.SDs  <-   sapply(all_data[7:15], sd, na.rm=TRUE)
the.samplesizes <-  sapply(all_data[7:15], function(x) sum(! is.na(x)) )
result   <-  cbind(the.mins, the.medians, the.means, the.maxes, the.SDs, the.samplesizes)
colnames(result) <- c('Minimum', 'Median', 'Mean', 'Maximum', 'Std. Deviation', 'Observations')
rownames(result) <- c('Temperature',
                      'Salinity',
                      'DO',
                      'pCO2',
                      'pCO2_corr',
                      'pH',
                      'Omega Aragonite',
                      'Omega Calcite',
                      'TA'
                      )
knitr::kable(result, digits = c(1,1,2,1,3.0))
```

|                 | Minimum | Median |    Mean | Maximum | Std. Deviation | Observations |
| :-------------- | ------: | -----: | ------: | ------: | -------------: | -----------: |
| Temperature     |   \-1.4 |   12.7 |   11.49 |    25.3 |          4.795 |        24620 |
| Salinity        |     1.7 |   29.8 |   29.20 |    32.1 |          2.068 |        23708 |
| DO              |   174.1 |  324.3 |  328.55 |   417.8 |         46.003 |        18542 |
| pCO2            |   190.8 |  560.6 |  578.28 |  1409.5 |        171.722 |        18535 |
| pCO2\_corr      |   228.1 |  584.6 |  582.18 |  1298.4 |        127.360 |        18528 |
| pH              |     6.7 |    7.9 |    7.93 |     8.3 |          0.119 |        12830 |
| Omega Aragonite |     0.2 |    1.7 |    1.67 |     3.4 |          0.471 |         7110 |
| Omega Calcite   |     0.3 |    2.7 |    2.63 |     5.3 |          0.744 |         7110 |
| TA              |   703.8 | 2289.7 | 2328.74 |  4700.0 |        473.949 |         7110 |

``` r
write.table(result, 'summarystats.txt', sep='\t')
```

# Omega Aragonite Observations and Percentage Below Levels of Concern

``` r
below1.5 <- sum(all_data$omega_a<1.5, na.rm=TRUE)
below1.0 <- sum(all_data$omega_a<1.0, na.rm=TRUE)
TotObs   <- sum(! is.na(all_data$omega_a))
pctbelow1.5 <- below1.5/TotObs
pctbelow1.0 <- below1.0/TotObs

res <- unlist( list(`Count Below 1.0` = below1.0, `Count Below 1.5` = below1.5,
      `Observations` = TotObs,
      `Percent Below 1.0` = pctbelow1.0,
      `Percent Below 1.5` =pctbelow1.5))
rm(below1.0, below1.5, TotObs, pctbelow1.0, pctbelow1.5)
knitr::kable(t(res), digits = c(0,0,0,3,3))
```

| Count Below 1.0 | Count Below 1.5 | Observations | Percent Below 1.0 | Percent Below 1.5 |
| --------------: | --------------: | -----------: | ----------------: | ----------------: |
|             686 |            2317 |         7110 |             0.096 |             0.326 |

# Daily Omega Aragonite (medians) Observations and and Percentage Below Levels of Concern

``` r
below1.5 <- sum(daily_data$omega_a_m<1.5, na.rm=TRUE)
below1.0 <- sum(daily_data$omega_a_m<1.0, na.rm=TRUE)
TotObs   <- sum(! is.na(daily_data$omega_a_m))
pctbelow1.5 <- below1.5/TotObs
pctbelow1.0 <- below1.0/TotObs

res <- unlist(list(`Count Below 1.0` = below1.0, `Count Below 1.5` = below1.5,
      `Observations` = TotObs,
      `Percent Below 1.0` = pctbelow1.0,
      `Percent Below 1.5` =pctbelow1.5))
rm(below1.0, below1.5, TotObs, pctbelow1.0, pctbelow1.5)
knitr::kable(t(res), digits = c(0,0,0,3,3))
```

| Count Below 1.0 | Count Below 1.5 | Observations | Percent Below 1.0 | Percent Below 1.5 |
| --------------: | --------------: | -----------: | ----------------: | ----------------: |
|              29 |             102 |          312 |             0.093 |             0.327 |
