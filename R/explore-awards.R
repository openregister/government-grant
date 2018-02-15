#' ---
#' title: Exploratory analysis of government grant awards
#' author: ""
#' ---

# Render to HTML with
# rmarkdown::render("R/explore-awards.R")

library(tidyverse)
library(readxl)
library(scales)
library(here)

path <- here("lists", "government-grant-awards-2016-17.xlsx")

awards201617 <- read_excel(path, guess_max = 10000)
glimpse(awards201617)

#' Only two funding organisations are represented
count(awards201617, `Funding Org:Identifier`, `Funding Org:Name`)

#' There are a lot of different types of charity number.
#'
#' * NIC = Northern Ireland Charity (omit the prefix when searching)
#' * SCO = Scottish Charity
#' * SC = Scottish Charity, but they mistyped the O as a 0.
#' * CIO = Charitable Incorporated Organisation
#' https://www.gov.uk/guidance/charity-types-how-to-choose-a-structure#charities-with-a-corporate-structure-which-type-to-choose (in the Charity register, ommitting the prefix)
#' * IP = Industrial and Provident Society http://www.fsa.gov.uk/doing/small_firms/msr/societies
#' * JAA = Justice After Acquittal (not an identifier)
#' * JAGSFO = JAGS Foundation (not an identifier)
#' * MAGAAM = Mothers Against Murder And Aggression (UK) (not an identifier)
#' * SP = "SP-G16-A08-07" (no idea what this is)
#' * XT = XT21929 St Albans Child Contact Centre, might be the ID in the National Association of Child Contact CentresContact
table(str_extract(awards201617$`Recipient Org:Charity Number`, "^[A-Z]+"))

count(awards201617, `From an open call?`)

#' Beneficiary location is cardinality='n'
#' And the ONS geography codes are spurious because they don't understand how to
#' refer to subsets of the UK
awards201617 %>%
  distinct(`Beneficiary Location:Name`,
           `Beneficiary Location:Geographic Code`,
           `Beneficiary Location:Geographic Code Type`)

count(awards201617, `Award Date`)
count(awards201617, `Planned Dates:Duration (months)`)

#' Some awards are zero amounts!  At least none are negative.
count(awards201617, `Amount Awarded`, sort = TRUE)
count(awards201617, `Amount Awarded`, sort = TRUE) %>% tail()

#' All currency is GBP
count(awards201617, `Currency`)
