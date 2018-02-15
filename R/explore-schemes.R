#' ---
#' title: Exploratory analysis of government grant schemes
#' author: ""
#' ---

# Render to HTML with
# rmarkdown::render("R/explore-schemes.R")

library(tidyverse)
library(scales)
library(here)

source_url <- "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/655239/GGIS_Grant_Schemes_2016_to_2017.csv"

source_data <- read_csv(source_url)

write_tsv(source_data, here("lists", "government-grant-scheme-source.tsv"))

# All seem to be in the government-organisation register,
# Though some names have changed
count(source_data, `Funding Org:Name`)

# `Allocation Method` is cardinality='n'
distinct(source_data, `Allocation Method`)

# There are only four `Allocation Method`s available
distinct(source_data, `Allocation Method`) %>%
  transmute(method = map(`Allocation Method`, str_split, pattern = "; "),
            method = map(method, unlist)) %>%
  unnest() %>%
  distinct() %>%
  arrange(method) %>%
  print() %>%
  write_tsv(here("lists", "government-grant-allocation-method.tsv"))

# `Recipient Sector` is cardinality='n'
distinct(source_data, `Recipient Sector`)

# There are only eight `Recipient Sector`s available
distinct(source_data, `Recipient Sector`) %>%
  transmute(sector = map(`Recipient Sector`, str_split, pattern = "; "),
            sector = map(sector, unlist)) %>%
  unnest() %>%
  distinct() %>%
  arrange(sector) %>%
  print() %>%
  write_tsv(here("lists", "government-grant-recipient-sector.tsv"))

# What is `COFOG L0 Long Name`? Another potential register?j
count(source_data, `COFOG L0 Long Name`)

# Durations in years.  Is 20 a legislative maximum?
count(source_data, `Duration Numeric`)

# The top six aims and objectives of government grants
slice(count(source_data, `Aims and objectives`, sort = TRUE), 1:6)
# # A tibble: 6 x 2
#   `Aims and objectives`                                                       n
# 1 To reduce global poverty in support of the UK_s national interest. Fur…   393
# 2 <NA>                                                                      200
# 3 .                                                                          81
# 4 FCO Policy Objectives                                                      24
# 5 TBC Q1 2017/18                                                             21
# 6 n/a                                                                        18

pull(slice(count(source_data, `Aims and objectives`, sort = TRUE), 1:6), 1)
# [1] "To reduce global poverty in support of the UK_s national interest. Further details on all UK overseas development assistance is available on Development Tracker at https://devtracker.dfid.gov.uk/"
# [2] NA
# [3] "."
# [4] "FCO Policy Objectives"
# [5] "TBC Q1 2017/18"
# [6] "n/a"

# The unique ID is the `Grant Programme:Code`
count(source_data, `Grant Programme:Code`, sort = TRUE)

# Some values are zero
count(source_data, `Value per year`, sort = TRUE)

# Some values are negative, some are not really grants (Crossrail?!)
count(source_data, `Value per year`)
arrange(source_data, `Value per year`) %>%
  select(1:3, 5)

count(source_data, `Value per year`) %>%
  arrange(-`Value per year`)

pound <- dollar_format(prefix = "£")

source_data %>%
  count(`Value per year`) %>%
  ggplot(aes(x = `Value per year`, y = n)) +
  geom_point() +
  scale_x_log10(label = pound) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

source_data %>%
  ggplot(aes(x = `Value per year`)) +
  geom_density() +
  scale_x_log10(label = pound) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
