Review and Cleaning of Casco Bay OA Data
================
Curtis C. Bohlen

  - [Load Libraries](#load-libraries)
  - [Load Data](#load-data)
      - [Establish Folder References](#establish-folder-references)
  - [Load data from identical excel
    spreadsheets](#load-data-from-identical-excel-spreadsheets)
  - [Combine Data](#combine-data)
      - [Demonstrate the Problem](#demonstrate-the-problem)
      - [Drop any records that have no
        data](#drop-any-records-that-have-no-data)
  - [Filtering out Bad PH Data](#filtering-out-bad-ph-data)
      - [Graph of pH data over time](#graph-of-ph-data-over-time)
      - [Graph of pH versus Alkalinity](#graph-of-ph-versus-alkalinity)
      - [Graph of Alkalinity
        vs. Salinity](#graph-of-alkalinity-vs.-salinity)
          - [2017](#section)
          - [2015](#section-1)
      - [Remove Questionable Data](#remove-questionable-data)
          - [Show What pH Data Will be
            Removed](#show-what-ph-data-will-be-removed)
          - [Remove Unwanted Data](#remove-unwanted-data)
  - [Final Data Cleanup](#final-data-cleanup)
      - [Rename Everything, drop categories we won’t
        use](#rename-everything-drop-categories-we-wont-use)
  - [Calculate Temperature Corrected
    pCO2](#calculate-temperature-corrected-pco2)
  - [Output Cleaned Data](#output-cleaned-data)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

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
library(readxl)
library(CBEPgraphics)
```

``` r
load_cbep_fonts()
```

# Load Data

## Establish Folder References

``` r
sibfldnm <- 'Original_Data'
parent   <- dirname(getwd())
sibling  <- file.path(parent,sibfldnm)
```

# Load data from identical excel spreadsheets

This code retains only the median hourly values.

``` r
the_data <- list()
for (yr in c(2015, 2016, 2017, 2018)) {
  fn <- paste0('CascoBay_lvl3_',  yr ,'.xls')
  fpath <- file.path(sibling,fn)
  df <- read_excel(fpath, col_types = 'numeric',
                   sheet = 'Data')[-1,] %>%  # dropping garbage second line
  select(- contains('_min')) %>%
  select(- contains('_mean')) %>%
  select(- contains('_max')) %>%
  select(- contains('_std'))
  n <- names(df)
  n <- sub('_median', '', n)
  names(df) <- n
  the_data <- append(the_data, list(df))  #  add datframe to a list of dataframes. 
}
```

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in A2 / R2C1: got 'yyyy'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B2 / R2C2: got 'mm'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in C2 / R2C3: got 'dd'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D2 / R2C4: got 'hh'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E2 / R2C5: got 'yyyymmdd'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F2 / R2C6: got 'Matlab_datenum'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in G2 / R2C7: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H2 / R2C8: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in I2 / R2C9: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J2 / R2C10: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K2 / R2C11: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in L2 / R2C12: got 'S/m'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M2 / R2C13: got 'S/m'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in N2 / R2C14: got 'S/m'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in O2 / R2C15: got 'S/m'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in P2 / R2C16: got 'S/m'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in Q2 / R2C17: got 'psu'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in R2 / R2C18: got 'psu'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in S2 / R2C19: got 'psu'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in T2 / R2C20: got 'psu'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in U2 / R2C21: got 'psu'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in V2 / R2C22: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W2 / R2C23: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in X2 / R2C24: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in Y2 / R2C25: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in Z2 / R2C26: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AA2 / R2C27: got 'Âµatm'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AB2 / R2C28: got 'Âµatm'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AC2 / R2C29: got 'Âµatm'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AD2 / R2C30: got 'Âµatm'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AE2 / R2C31: got 'Âµatm'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AF2 / R2C32: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AG2 / R2C33: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AH2 / R2C34: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AI2 / R2C35: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AJ2 / R2C36: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AK2 / R2C37: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AL2 / R2C38: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AM2 / R2C39: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AN2 / R2C40: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AO2 / R2C41: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AP2 / R2C42: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AQ2 / R2C43: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AR2 / R2C44: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AS2 / R2C45: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AT2 / R2C46: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AU2 / R2C47: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AV2 / R2C48: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AW2 / R2C49: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AX2 / R2C50: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AY2 / R2C51: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AZ2 / R2C52: got 'omega-a'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in BA2 / R2C53: got 'omega-c'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in BB2 / R2C54: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in BC2 / R2C55: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in A2 / R2C1: got 'yyyy'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B2 / R2C2: got 'mm'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in C2 / R2C3: got 'dd'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D2 / R2C4: got 'hh'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E2 / R2C5: got 'yyyymmdd'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F2 / R2C6: got 'Matlab_datenum'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in G2 / R2C7: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H2 / R2C8: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in I2 / R2C9: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J2 / R2C10: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K2 / R2C11: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in L2 / R2C12: got 'S/m'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M2 / R2C13: got 'S/m'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in N2 / R2C14: got 'S/m'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in O2 / R2C15: got 'S/m'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in P2 / R2C16: got 'S/m'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in Q2 / R2C17: got 'psu'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in R2 / R2C18: got 'psu'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in S2 / R2C19: got 'psu'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in T2 / R2C20: got 'psu'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in U2 / R2C21: got 'psu'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in V2 / R2C22: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W2 / R2C23: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in X2 / R2C24: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in Y2 / R2C25: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in Z2 / R2C26: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AA2 / R2C27: got 'Âµatm'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AB2 / R2C28: got 'Âµatm'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AC2 / R2C29: got 'Âµatm'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AD2 / R2C30: got 'Âµatm'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AE2 / R2C31: got 'Âµatm'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AF2 / R2C32: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AG2 / R2C33: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AH2 / R2C34: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AI2 / R2C35: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AJ2 / R2C36: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AK2 / R2C37: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AL2 / R2C38: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AM2 / R2C39: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AN2 / R2C40: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AO2 / R2C41: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AP2 / R2C42: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AQ2 / R2C43: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AR2 / R2C44: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AS2 / R2C45: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AT2 / R2C46: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AU2 / R2C47: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AV2 / R2C48: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AW2 / R2C49: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AX2 / R2C50: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AY2 / R2C51: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AZ2 / R2C52: got 'omega-a'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in BA2 / R2C53: got 'omega-c'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in BB2 / R2C54: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in BC2 / R2C55: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in A2 / R2C1: got 'yyyy'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B2 / R2C2: got 'mm'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in C2 / R2C3: got 'dd'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D2 / R2C4: got 'hh'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E2 / R2C5: got 'yyyymmdd'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F2 / R2C6: got 'Matlab_datenum'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in G2 / R2C7: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H2 / R2C8: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in I2 / R2C9: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J2 / R2C10: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K2 / R2C11: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in L2 / R2C12: got 'S/m'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M2 / R2C13: got 'S/m'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in N2 / R2C14: got 'S/m'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in O2 / R2C15: got 'S/m'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in P2 / R2C16: got 'S/m'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in Q2 / R2C17: got 'psu'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in R2 / R2C18: got 'psu'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in S2 / R2C19: got 'psu'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in T2 / R2C20: got 'psu'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in U2 / R2C21: got 'psu'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in V2 / R2C22: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W2 / R2C23: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in X2 / R2C24: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in Y2 / R2C25: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in Z2 / R2C26: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AA2 / R2C27: got 'Âµatm'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AB2 / R2C28: got 'Âµatm'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AC2 / R2C29: got 'Âµatm'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AD2 / R2C30: got 'Âµatm'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AE2 / R2C31: got 'Âµatm'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AF2 / R2C32: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AG2 / R2C33: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AH2 / R2C34: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AI2 / R2C35: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AJ2 / R2C36: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AK2 / R2C37: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AL2 / R2C38: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AM2 / R2C39: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AN2 / R2C40: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AO2 / R2C41: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AP2 / R2C42: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AQ2 / R2C43: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AR2 / R2C44: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AS2 / R2C45: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AT2 / R2C46: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AU2 / R2C47: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AV2 / R2C48: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AW2 / R2C49: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AX2 / R2C50: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AY2 / R2C51: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AZ2 / R2C52: got 'omega-a'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in BA2 / R2C53: got 'omega-c'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in BB2 / R2C54: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in BC2 / R2C55: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in A2 / R2C1: got 'yyyy'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in B2 / R2C2: got 'mm'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in C2 / R2C3: got 'dd'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in D2 / R2C4: got 'hh'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in E2 / R2C5: got 'yyyymmdd'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in F2 / R2C6: got 'Matlab_datenum'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in G2 / R2C7: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in H2 / R2C8: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in I2 / R2C9: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in J2 / R2C10: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in K2 / R2C11: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in L2 / R2C12: got 'S/m'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in M2 / R2C13: got 'S/m'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in N2 / R2C14: got 'S/m'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in O2 / R2C15: got 'S/m'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in P2 / R2C16: got 'S/m'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in Q2 / R2C17: got 'psu'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in R2 / R2C18: got 'psu'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in S2 / R2C19: got 'psu'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in T2 / R2C20: got 'psu'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in U2 / R2C21: got 'psu'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in V2 / R2C22: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in W2 / R2C23: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in X2 / R2C24: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in Y2 / R2C25: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in Z2 / R2C26: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AA2 / R2C27: got 'Âµatm'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AB2 / R2C28: got 'Âµatm'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AC2 / R2C29: got 'Âµatm'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AD2 / R2C30: got 'Âµatm'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AE2 / R2C31: got 'Âµatm'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AF2 / R2C32: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AG2 / R2C33: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AH2 / R2C34: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AI2 / R2C35: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AJ2 / R2C36: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AK2 / R2C37: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AL2 / R2C38: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AM2 / R2C39: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AN2 / R2C40: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AO2 / R2C41: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AP2 / R2C42: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AQ2 / R2C43: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AR2 / R2C44: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AS2 / R2C45: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AT2 / R2C46: got 'pH (Total scale)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AU2 / R2C47: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AV2 / R2C48: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AW2 / R2C49: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AX2 / R2C50: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AY2 / R2C51: got 'degreesCelcius'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in AZ2 / R2C52: got 'omega-a'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in BA2 / R2C53: got 'omega-c'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in BB2 / R2C54: got 'Âµmol/kg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in BC2 / R2C55: got 'Âµmol/kg'

# Combine Data

``` r
the_data <- bind_rows(the_data) %>%
  select(-yyyymmdd, -Matlab_datenum, -FET_TEMP_CON) %>%
  mutate(datetime = ISOdatetime(yyyy, mm, dd, hh,0,0, 'America/New_York')) %>%
  mutate(doy = as.numeric(strftime(datetime, format = "%j")))
```

In downstream analysis steps, I uncovered duplicate dates and times that
lack data, specifically from January of 2016. I filter them out here.

## Demonstrate the Problem

``` r
the_data %>% select(datetime) %>% group_by(datetime) %>% summarize(n = n()) %>% filter(n>1) %>% arrange(datetime)
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 475 x 2
    ##    datetime                n
    ##    <dttm>              <int>
    ##  1 2016-01-01 00:00:00     2
    ##  2 2016-01-01 01:00:00     2
    ##  3 2016-01-01 02:00:00     2
    ##  4 2016-01-01 03:00:00     2
    ##  5 2016-01-01 04:00:00     2
    ##  6 2016-01-01 05:00:00     2
    ##  7 2016-01-01 06:00:00     2
    ##  8 2016-01-01 07:00:00     2
    ##  9 2016-01-01 08:00:00     2
    ## 10 2016-01-01 09:00:00     2
    ## # ... with 465 more rows

## Drop any records that have no data

``` r
the_data <-  the_data %>% filter(
   ! (
     is.na(SBE37_TEMP) &
     is.na(SBE37_CON) &
     is.na(SBE37_SALINITY) &
     is.na(SAMICO2_TEMP) &
     is.na(SAMI_CO2) &
     is.na(Optode_O2) &
     is.na(FET_PHINT) &
     is.na(FET_PHEXT) &  
     is.na(`omega-a`) &
     is.na(`omega-c`) &
     is.na(TA_calc) &    
     is.na(DIC_calc)
   )
)
```

# Filtering out Bad PH Data

Larry Harris pointed out that certain pH observations look impossible.
Many low pH values look suspect, especially in 2017. In addition, any pH
measurements associated with calculated alkalinity below about 1000
should be considered highly suspect, since low alkalinity is unlikely at
high salinity.

We expolore these problems graphically.

## Graph of pH data over time

``` r
plt <- ggplot(the_data, aes(datetime, FET_PHINT)) +
  geom_point(aes(color = TA_calc), alpha = 0.05) +
  xlab('Date') +
  ylab('pH') +
  theme_cbep()
plt
```

    ## Warning: Removed 8224 rows containing missing values (geom_point).

![](Data_Review_And_Filtering_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
I note low pH excursions in 2015, perhaps 2016, and most of 2017. But
note that the fist “spike” of low values in 2015 lack alkalinity
calculations, presumably because other required data was not available.

## Graph of pH versus Alkalinity

(Note that since calculated alkalinity is only possible when all other
data is available, this graph omits a lot of observations, including
some low pH values.)

``` r
plt <- ggplot(the_data, aes(FET_PHINT, TA_calc, color = factor(yyyy))) +
  geom_point(alpha = 0.1, size = 0.5) +
  xlab('pH') +
  ylab('Alkalinity') +
  theme_cbep() +
   guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1)))
plt
```

    ## Warning: Removed 14131 rows containing missing values (geom_point).

![](Data_Review_And_Filtering_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
The most obvious problematic data (with very low alkalinity AND very low
pH) are from 2015 and 2017.

## Graph of Alkalinity vs. Salinity

``` r
plt <- ggplot(the_data, aes(SBE37_SALINITY, TA_calc, color = factor(yyyy))) +
  geom_point(alpha = 0.2, size = 1)  +
  xlab('Salinity') +
  ylab('Alkalinity') +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 4))) +
  scale_x_continuous(limits = c(22,32)) +
  theme_cbep()
plt
```

    ## Warning: Removed 14139 rows containing missing values (geom_point).

![](Data_Review_And_Filtering_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
Again, what jumps out are the points with low pH in 2017 and 2015.
Although the HIGH alkalinity observations from early in 2017 also look
off-base.

If we look at a plot of pH versus temperature, the same points jump out,
with possibly some other low pH observations from 2016.

``` r
plt <- ggplot(the_data, aes(SBE37_TEMP, FET_PHINT, color = factor(yyyy))) +
  geom_point(alpha = 0.1) +
  theme_cbep() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab('Temperature (C)') +
    ylab('pH') +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))
plt
```

    ## Warning: Removed 8289 rows containing missing values (geom_point).

![](Data_Review_And_Filtering_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
I see no obvious way to eliminate the erroneous data. There are “bad”
data popping up several times. It’s mostly the 2017 data, but there may
also be problematic data in 2015 and 2016.

### 2017

Identify the date in 2017 when things went wonky.

``` r
plt <- the_data %>% filter(yyyy==2017, mm == 6, dd<18, dd>10) %>%
  ggplot(aes(x=datetime, y=FET_PHINT)) + geom_line() +
  theme_cbep() +
  xlab('Date')
plt
```

![](Data_Review_And_Filtering_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
The pH drop off kicks in around June 14th. or 15th.

So, things went odd around June 14th.

### 2015

``` r
plt <- the_data %>% filter(yyyy==2015, mm %in% c(8,9,10)) %>%
  ggplot(aes(x=datetime, y=FET_PHINT)) + 
  geom_line(aes(color=(TA_calc<1200)))+
  theme_cbep() +
  xlab('Date')
plt
```

![](Data_Review_And_Filtering_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

That shows a low pH period, in September around end of September 8
through Sept 14. That one may actually have started just befor the data
gap, around August 17th and another around September 30 through October
11. The exact period we should remove from the data here is somewhat
arbitrary. Since the primary indicator of problematic data is the low
observed pH value, we risk biasing results. Here we back that up by
tossing out data by whole days, and by focusing on days with low
calculated alkalinity.

## Remove Questionable Data

Remove questionable pH values and ALSO CO2Sys calculated values

``` r
d1 <- ISOdatetime(2015,8, 17,0,0, 0,'America/New_York')
d2 <- ISOdatetime(2015,9,14,0,0, 0,'America/New_York')
d3 <- ISOdatetime(2015,9,30,0,0, 0,'America/New_York')
d4 <- ISOdatetime(2015,10,11,0,0, 0,'America/New_York')
d5 <- ISOdatetime(2017,6,14,0,0, 0,'America/New_York')
d6 <- ISOdatetime(2017,12,31,0,0,0,'America/New_York')
phomitflag1  <- ! (the_data$datetime < d1 | the_data$datetime > d2)
phomitflag2  <- ! (the_data$datetime < d3 | the_data$datetime > d4)
phomitflag3  <- ! (the_data$datetime < d5 | the_data$datetime > d6)
flag <- ! (phomitflag1 | phomitflag2 | phomitflag3)
rm(phomitflag1, phomitflag2, phomitflag3)
```

### Show What pH Data Will be Removed

``` r
tt <- the_data %>%  mutate(t = flag) %>% filter(yyyy==2015)

plt <- ggplot(tt, aes(x=datetime, y=FET_PHINT)) +
  geom_point(aes(color = t),size = 0.5, alpha = 0.2) +
  theme_cbep() +
  xlab('Date') +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))
plt
```

    ## Warning: Removed 830 rows containing missing values (geom_point).

![](Data_Review_And_Filtering_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
rm(tt)
```

We are still left with some low values, but the really wonky sudden pH
changes are removed.

### Remove Unwanted Data

TO trim the data, we use the flags to set to NA any variables that
depend on accurate pH values, and then toss out any data that lacks
remaining valid observations.

``` r
trimmed_data <- the_data %>%
  mutate(FET_PHINT = ifelse(flag, FET_PHINT, NA)) %>%
  mutate(FET_PHEXT = ifelse(flag, FET_PHEXT, NA)) %>%
  mutate(`omega-a` = ifelse(flag, `omega-a`, NA)) %>%
  mutate(`omega-c` = ifelse(flag, `omega-c`, NA)) %>%
  mutate(TA_calc = ifelse(flag, TA_calc, NA)) %>%
  mutate(DIC_calc = ifelse(flag, DIC_calc, NA)) %>%
  filter_all(any_vars(!is.na(.)))
```

# Final Data Cleanup

## Rename Everything, drop categories we won’t use

names()

``` r
trimmed_data <- trimmed_data %>%
  select(-SBE37_CON,
         -SAMICO2_TEMP,
         -FET_PHEXT,
         -DIC_calc
         ) %>%
  rename(doy = doy,
         temp = SBE37_TEMP,
         sal = SBE37_SALINITY,
         co2 = SAMI_CO2,
         do = Optode_O2,
         ph = FET_PHINT,
         omega_a = `omega-a`,
         omega_c = `omega-c`
  )
```

# Calculate Temperature Corrected pCO2

Here we follow a formula for calculating a “Temperature Corrected”
version of pCO2, which isderived from Takehashi aet al. 2002.

``` r
(t_ref = 12)
```

    ## [1] 12

``` r
(t_mean = mean(trimmed_data$temp, na.rm=TRUE))
```

    ## [1] 11.49428

``` r
(co2_mean = mean(trimmed_data$co2, na.rm=TRUE))
```

    ## [1] 578.2843

``` r
trimmed_data <- trimmed_data %>%
  mutate(co2_thermal =  round(co2_mean*exp(0.0423*(temp-t_mean)),2)) %>%
  mutate(co2_corr =  round(co2*exp(0.0423*(t_ref-temp)),2))
```

# Output Cleaned Data

``` r
write_csv(trimmed_data,'CascoBayOAData.csv')
```
