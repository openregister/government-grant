#' ---
#' title: Exploratory analysis of government grant schemes
#' author: ""
#' output: github_document
#' ---

# Render to HTML with
# rmarkdown::render("R/explore-schemes.R")

library(tidyverse)
library(scales)
library(here)

path201314 <- here("lists", "government-grant-schemes-2013-14.csv")
path201415 <- here("lists", "government-grant-schemes-2014-15.csv")
path201516 <- here("lists", "government-grant-schemes-2015-16.csv")
path201617 <- here("lists", "government-grant-schemes-2016-17.csv")

scheme201314 <- read_csv(path201314)
scheme201415 <- read_csv(path201415)
scheme201516 <- read_csv(path201516)
scheme201617 <- read_csv(path201617)

#'
#' The 2013/14 and 2014/15 years
#'
#' * give the number of recipients
#' * don't have grant IDs
#' * use different monetary units (£,000s and £,000,000s)
#'
#' ## 2016/17 schemes
#'
#' All seem to be in the government-organisation register,
#' Though some names have changed
count(scheme201617, `Funding Org:Name`)

#' `Allocation Method` is cardinality='n'
distinct(scheme201617, `Allocation Method`)

#' There are only four `Allocation Method`s available
distinct(scheme201617, `Allocation Method`) %>%
  transmute(method = map(`Allocation Method`, str_split, pattern = "; "),
            method = map(method, unlist)) %>%
  unnest() %>%
  distinct() %>%
  arrange(method) %>%
  print() %>%
  write_tsv(here("lists", "government-grant-allocation-method.tsv"))

#' `Recipient Sector` is cardinality='n'
distinct(scheme201617, `Recipient Sector`)

#' There are only eight `Recipient Sector`s available
distinct(scheme201617, `Recipient Sector`) %>%
  transmute(sector = map(`Recipient Sector`, str_split, pattern = "; "),
            sector = map(sector, unlist)) %>%
  unnest() %>%
  distinct() %>%
  arrange(sector) %>%
  print() %>%
  write_tsv(here("lists", "government-grant-recipient-sector.tsv"))

#' What is `COFOG L0 Long Name`? Another potential register?j
count(scheme201617, `COFOG L0 Long Name`)

#' Durations in years.  Is 20 a legislative maximum?
count(scheme201617, `Duration Numeric`)

#' The top six aims and objectives of government grants
slice(count(scheme201617, `Aims and objectives`, sort = TRUE), 1:6)

pull(slice(count(scheme201617, `Aims and objectives`, sort = TRUE), 1:6), 1)

#' The unique ID is the `Grant Programme:Code`
count(scheme201617, `Grant Programme:Code`, sort = TRUE)

#' Some values are zero
count(scheme201617, `Value per year`, sort = TRUE)

#' Some values are negative, some are not really grants (Crossrail?!)
count(scheme201617, `Value per year`)
arrange(scheme201617, `Value per year`) %>%
  select(1:3, 5)

count(scheme201617, `Value per year`) %>%
  arrange(-`Value per year`)

pound <- dollar_format(prefix = "£")

scheme201617 %>%
  count(`Value per year`) %>%
  ggplot(aes(x = `Value per year`, y = n)) +
  geom_point() +
  scale_x_log10(label = pound) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

scheme201617 %>%
  ggplot(aes(x = `Value per year`)) +
  geom_density() +
  scale_x_log10(label = pound) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
