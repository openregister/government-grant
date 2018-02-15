Exploratory analysis of government grant awards
================
Thu Feb 15 13:03:59 2018

``` r
# Render to HTML with
# rmarkdown::render("R/explore-awards.R")

library(tidyverse)
library(readxl)
library(scales)
library(here)

path <- here("lists", "government-grant-awards-2016-17.xlsx")

awards201617 <- read_excel(path, guess_max = 10000)
glimpse(awards201617)
```

    ## Observations: 3,351
    ## Variables: 26
    ## $ Identifier                                  <chr> "360G-cabinetoffic...
    ## $ Title                                       <chr> "Transport Technol...
    ## $ Description                                 <chr> "Analysis of the a...
    ## $ Currency                                    <chr> "GBP", "GBP", "GBP...
    ## $ `Amount Awarded`                            <dbl> 25000, 2000000, 50...
    ## $ `Award Date`                                <dttm> 2016-10-03, 2016-...
    ## $ `Planned Dates:Start Date`                  <dttm> 2016-10-03, 2016-...
    ## $ `Planned Dates:End Date`                    <dttm> 2017-02-15, 2017-...
    ## $ `Planned Dates:Duration (months)`           <dbl> 4, 12, 12, 5, 12, ...
    ## $ `Recipient Org:Identifier`                  <chr> "E5104X", "E5104X"...
    ## $ `Recipient Org:Name`                        <chr> "Transport for Lon...
    ## $ `Recipient Org:Charity Number`              <chr> NA, NA, NA, NA, NA...
    ## $ `Recipient Org:Company Number`              <chr> NA, NA, NA, NA, NA...
    ## $ `Recipient Org:Street Address`              <chr> "42-50 Victoria St...
    ## $ `Recipient Org:City`                        <chr> "LONDON", "LONDON"...
    ## $ `Recipient Org:Country`                     <chr> "United Kingdom", ...
    ## $ `Recipient Org:Postal Code`                 <chr> "SW1H 0TL", "SW1H ...
    ## $ `Beneficiary Location:Name`                 <chr> "England; Scotland...
    ## $ `Beneficiary Location:Geographic Code`      <chr> "K02000001", "K020...
    ## $ `Beneficiary Location:Geographic Code Type` <chr> "CTRY", "CTRY", "C...
    ## $ `Funding Org:Identifier`                    <chr> "D9", "D9", "D9", ...
    ## $ `Funding Org:Name`                          <chr> "Department for Tr...
    ## $ `Grant Programme:Code`                      <chr> "SCH-000003403", "...
    ## $ `Grant Programme:Title`                     <chr> "T-TRIG Grants Aug...
    ## $ `From an open call?`                        <chr> "Yes", "Yes", "Yes...
    ## $ `Last modified`                             <chr> "2017-08-03T00:00:...

Only two funding organisations are represented

``` r
count(awards201617, `Funding Org:Identifier`, `Funding Org:Name`)
```

    ## # A tibble: 2 x 3
    ##   `Funding Org:Identifier` `Funding Org:Name`           n
    ##   <chr>                    <chr>                    <int>
    ## 1 D18                      Ministry of Justice        774
    ## 2 D9                       Department for Transport  2577

There are a lot of different types of charity number.

  - NIC = Northern Ireland Charity (omit the prefix when searching)
  - SCO = Scottish Charity
  - SC = Scottish Charity, but they mistyped the O as a 0.
  - CIO = Charitable Incorporated Organisation
    <https://www.gov.uk/guidance/charity-types-how-to-choose-a-structure#charities-with-a-corporate-structure-which-type-to-choose>
    (in the Charity register, ommitting the prefix)
  - IP = Industrial and Provident Society
    <http://www.fsa.gov.uk/doing/small_firms/msr/societies>
  - JAA = Justice After Acquittal (not an identifier)
  - JAGSFO = JAGS Foundation (not an identifier)
  - MAGAAM = Mothers Against Murder And Aggression (UK) (not an
    identifier)
  - SP = “SP-G16-A08-07” (no idea what this is)
  - XT = XT21929 St Albans Child Contact Centre, might be the ID in the
    National Association of Child Contact
CentresContact

<!-- end list -->

``` r
table(str_extract(awards201617$`Recipient Org:Charity Number`, "^[A-Z]+"))
```

    ## 
    ##    CIO     IP    JAA JAGSFO MAGAAM    NIC     SC    SCO     SP     XT 
    ##      1      2      1      1      1      2      4      1      1      1

``` r
count(awards201617, `From an open call?`)
```

    ## # A tibble: 2 x 2
    ##   `From an open call?`     n
    ##   <chr>                <int>
    ## 1 No                    2544
    ## 2 Yes                    807

Beneficiary location is cardinality=‘n’ And the ONS geography codes are
spurious because they don’t understand how to refer to subsets of the UK

``` r
awards201617 %>%
  distinct(`Beneficiary Location:Name`,
           `Beneficiary Location:Geographic Code`,
           `Beneficiary Location:Geographic Code Type`)
```

    ## # A tibble: 4 x 3
    ##   `Beneficiary Location:Name` `Beneficiary Locatio… `Beneficiary Location…
    ##   <chr>                       <chr>                 <chr>                 
    ## 1 England; Scotland; Wales    K02000001             CTRY                  
    ## 2 UK Wide                     K02000001             CTRY                  
    ## 3 England                     E92000001             CTRY                  
    ## 4 England; Wales              K02000001             CTRY

``` r
count(awards201617, `Award Date`)
```

    ## # A tibble: 16 x 2
    ##    `Award Date`            n
    ##    <dttm>              <int>
    ##  1 2015-04-01 00:00:00     6
    ##  2 2015-07-15 00:00:00     5
    ##  3 2016-04-01 00:00:00  3202
    ##  4 2016-07-15 00:00:00    31
    ##  5 2016-10-03 00:00:00    33
    ##  6 2016-10-10 00:00:00     2
    ##  7 2016-10-24 00:00:00     1
    ##  8 2016-11-03 00:00:00     1
    ##  9 2016-11-07 00:00:00     1
    ## 10 2016-12-15 00:00:00    62
    ## 11 2016-12-20 00:00:00     1
    ## 12 2017-01-01 00:00:00     1
    ## 13 2017-01-12 00:00:00     1
    ## 14 2017-01-20 00:00:00     1
    ## 15 2017-01-29 00:00:00     2
    ## 16 2017-02-10 00:00:00     1

``` r
count(awards201617, `Planned Dates:Duration (months)`)
```

    ## # A tibble: 12 x 2
    ##    `Planned Dates:Duration (months)`     n
    ##                                <dbl> <int>
    ##  1                              0        1
    ##  2                              1.00     2
    ##  3                              2.00     6
    ##  4                              3.00    61
    ##  5                              4.00    17
    ##  6                              5.00    18
    ##  7                              6.00     2
    ##  8                              9.00    31
    ##  9                             12.0   3200
    ## 10                             21.0      1
    ## 11                             24.0     11
    ## 12                             48.0      1

Some awards are zero amounts\! At least none are negative.

``` r
count(awards201617, `Amount Awarded`, sort = TRUE)
```

    ## # A tibble: 2,480 x 2
    ##    `Amount Awarded`     n
    ##               <dbl> <int>
    ##  1                0   109
    ##  2             3000    97
    ##  3            25000    39
    ##  4             1000    38
    ##  5             2000    38
    ##  6            24000    36
    ##  7            60000    35
    ##  8            18000    23
    ##  9            45000    23
    ## 10            70000    15
    ## # ... with 2,470 more rows

``` r
count(awards201617, `Amount Awarded`, sort = TRUE) %>% tail()
```

    ## # A tibble: 6 x 2
    ##   `Amount Awarded`     n
    ##              <dbl> <int>
    ## 1         35400000     1
    ## 2         38785000     1
    ## 3         47828470     1
    ## 4         50311300     1
    ## 5         57988111     1
    ## 6       1418364000     1

All currency is GBP

``` r
count(awards201617, `Currency`)
```

    ## # A tibble: 1 x 2
    ##   Currency     n
    ##   <chr>    <int>
    ## 1 GBP       3351
