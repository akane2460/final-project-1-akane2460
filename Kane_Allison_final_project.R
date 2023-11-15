# Final project ----
# Stat 301-1

## load packages ----

library(tidyverse)
library(skimr)
library(janitor)

shark_tank_us <- read_csv("data/shark_tank_us_data.csv")

# skim_without_charts(shark_tank_us)

head(shark_tank_us)

# cleaning names

shark_tank_us <- shark_tank_us |>
  clean_names()

# clean got_deal and multiple_entrepreneurs

shark_tank_us <- shark_tank_us |> 
  mutate(
    got_deal = ifelse(got_deal == 1, TRUE, FALSE),
    royalty_deal = ifelse(is.na(royalty_deal) == TRUE, FALSE, TRUE)
  )

# replace NAs in `Total Deal Amount`, `Deal Valuation`,
# `Number of sharks in deal`, `Investment Amount Per Shark`, and `Equity Per Shark`

shark_tank_us <- shark_tank_us |> 
  mutate(
    total_deal_amount = ifelse(is.na(total_deal_amount) == TRUE, 0, total_deal_amount),
    deal_valuation = ifelse(is.na(deal_valuation) == TRUE, 0, deal_valuation),
    number_of_sharks_in_deal = ifelse(is.na(number_of_sharks_in_deal) == TRUE, 0, number_of_sharks_in_deal),
    investment_amount_per_shark = ifelse(is.na(investment_amount_per_shark) == TRUE, 0, investment_amount_per_shark),
    equity_per_shark = ifelse(is.na(equity_per_shark) == TRUE, 0, equity_per_shark)
  )

