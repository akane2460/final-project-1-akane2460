# Final project Data Cleaning ----
# Stat 301-1

## Load Packages ----

library(tidyverse)
library(skimr)
library(janitor)

## Load Data ----
shark_tank_us <- read_csv("data/raw/shark_tank_us_data.csv")

# skim_without_charts(shark_tank_us)
# head(shark_tank_us)

### Cleaning Names----
shark_tank_us <- shark_tank_us |>
  clean_names()


### Pitched and Made Deals ----
# replace NAs in `Total Deal Amount`, `Deal Valuation`,
# `Number of sharks in deal`, `Deal Valuation `, `Investment Amount Per Shark`, `Loan` and `Equity Per Shark`
shark_tank_us <- shark_tank_us |> 
  mutate(
    total_deal_amount = ifelse(is.na(total_deal_amount) == TRUE, 0, total_deal_amount),
    deal_valuation = ifelse(is.na(deal_valuation) == TRUE, 0, deal_valuation),
    number_of_sharks_in_deal = ifelse(is.na(number_of_sharks_in_deal) == TRUE, 0, number_of_sharks_in_deal),
    investment_amount_per_shark = ifelse(is.na(investment_amount_per_shark) == TRUE, 0, investment_amount_per_shark),
    equity_per_shark = ifelse(is.na(equity_per_shark) == TRUE, 0, equity_per_shark),
    loan = ifelse(is.na(loan) == TRUE, 0, loan),
    deal_valuation = ifelse(is.na(deal_valuation) == TRUE, 0, deal_valuation),
    total_deal_equity = ifelse(is.na(total_deal_equity) == TRUE, 0, total_deal_equity)
  )

# clean got_deal and multiple_entrepreneurs
shark_tank_us <- shark_tank_us |> 
  mutate(
    got_deal = ifelse(got_deal == 1, TRUE, FALSE),
    royalty_deal = ifelse(is.na(royalty_deal) == TRUE, FALSE, TRUE)
  )


# Multiple Entrepreneurs ----
shark_tank_us <- shark_tank_us |> 
  # handle the NA values first
  mutate(
    multiple_entrepreneurs = ifelse(is.na(multiple_entrepreneurs) == TRUE & is.na(entrepreneur_names) == TRUE,
                                    pitchers_gender, multiple_entrepreneurs),
    
    # for cases where there could be multiple entrepreneurs, there are some instances where the names are unavailable
    # and, if the team is not mixed gender, there is no way of telling
    # these instances will be treated as if there is a single entrepreneur
    
    # assign mixed teams to be multiple
    multiple_entrepreneurs = ifelse(multiple_entrepreneurs == "Mixed Team", 1, multiple_entrepreneurs),
    
    # check for any missed NAs
    multiple_entrepreneurs = ifelse(is.na(multiple_entrepreneurs) == TRUE, entrepreneur_names, multiple_entrepreneurs)
  )

# check to see if these missed NAs are single entrepreneurs or teams
missed_NAs <- shark_tank_us |> 
  filter(multiple_entrepreneurs == entrepreneur_names) |> 
  select(entrepreneur_names, multiple_entrepreneurs)

missed_NAs$multiple_entrepreneurs <- toString(missed_NAs$multiple_entrepreneurs)

missed_NAs |> filter(str_detect(multiple_entrepreneurs, pattern = "^and$"))

# since none of these entrepreneurs contain "and" in their name (indicative of a team)
# all the "missed NAs" are single entrepreneurs

# convert to TRUE and FALSE for multiple entrepreneur teams
shark_tank_us <- shark_tank_us |> 
  mutate(
    # address the missing NAs first
    multiple_entrepreneurs = ifelse(is.na(multiple_entrepreneurs) == TRUE, FALSE, multiple_entrepreneurs),
    # then the remaining observations
    multiple_entrepreneurs = ifelse(multiple_entrepreneurs == 1, TRUE, FALSE)
  )

# remove NAs for pitcher gender

shark_tank_us <- shark_tank_us |> 
  filter(is.na(pitchers_gender) == FALSE)

### Series Regular Investors ----
# replace NAs for each investor
shark_tank_us <- shark_tank_us |> 
  mutate(
    # barbara corcoran
    barbara_corcoran_investment_amount = ifelse(is.na(barbara_corcoran_investment_amount) == TRUE, 0, barbara_corcoran_investment_amount),
    barbara_corcoran_investment_equity = ifelse(is.na(barbara_corcoran_investment_equity) == TRUE, 0, barbara_corcoran_investment_equity),
    barbara_corcoran_present = ifelse(barbara_corcoran_present == 1, TRUE, FALSE),
    # mark cuban
    mark_cuban_investment_amount = ifelse(is.na(mark_cuban_investment_amount) == TRUE, 0, mark_cuban_investment_amount),
    mark_cuban_investment_equity = ifelse(is.na(mark_cuban_investment_equity) == TRUE, 0, mark_cuban_investment_equity),
    mark_cuban_present = ifelse( mark_cuban_present == 1, TRUE, FALSE),
    # lori greiner
    lori_greiner_investment_amount = ifelse(is.na(lori_greiner_investment_amount) == TRUE, 0, lori_greiner_investment_amount),
    lori_greiner_investment_equity = ifelse(is.na(lori_greiner_investment_equity) == TRUE, 0, lori_greiner_investment_equity),
    lori_greiner_present = ifelse(lori_greiner_present == 1, TRUE, FALSE),
    # robert herjavec
    robert_herjavec_investment_amount = ifelse(is.na(robert_herjavec_investment_amount) == TRUE, 0, robert_herjavec_investment_amount),
    robert_herjavec_investment_equity = ifelse(is.na(robert_herjavec_investment_equity) == TRUE, 0, robert_herjavec_investment_equity),
    robert_herjavec_present = ifelse(robert_herjavec_present == 1, TRUE, FALSE),
    # daymond john 
    daymond_john_investment_amount = ifelse(is.na(daymond_john_investment_amount) == TRUE, 0, daymond_john_investment_amount),
    daymond_john_investment_equity = ifelse(is.na(daymond_john_investment_equity) == TRUE, 0, daymond_john_investment_equity),
    daymond_john_present = ifelse(daymond_john_present == 1, TRUE, FALSE),
    # kevin oleary
    kevin_o_leary_investment_amount = ifelse(is.na(kevin_o_leary_investment_amount) == TRUE, 0, kevin_o_leary_investment_amount),
    kevin_o_leary_investment_equity = ifelse(is.na(kevin_o_leary_investment_equity) == TRUE, 0, kevin_o_leary_investment_equity),
    kevin_o_leary_present = ifelse(kevin_o_leary_present == 1, TRUE, FALSE),
    # guest
    guest_investment_amount = ifelse(is.na(guest_investment_amount) == TRUE, 0, guest_investment_amount),
    guest_investment_equity = ifelse(is.na(guest_investment_equity) == TRUE, 0, guest_investment_equity)
  )

## Frequency of investing for individual investors
shark_tank_us <- shark_tank_us |> 
  mutate(
    # barbara corcoran
    barbara_corcoran_invested = ifelse(barbara_corcoran_investment_amount > 0, TRUE, FALSE),
    # mark cuban
    mark_cuban_invested = ifelse(mark_cuban_investment_amount > 0, TRUE, FALSE),
    # lori greiner
    lori_greiner_invested = ifelse(lori_greiner_investment_amount > 0, TRUE, FALSE),
    # robert herjavec
    robert_herjavec_invested = ifelse(robert_herjavec_investment_amount > 0, TRUE, FALSE),
    # daymond john 
    daymond_john_invested = ifelse(daymond_john_investment_amount > 0, TRUE, FALSE),
    # kevin oleary
    kevin_o_leary_invested = ifelse(kevin_o_leary_investment_amount > 0, TRUE, FALSE),
    # guest
    guest_invested = ifelse(guest_investment_amount > 0, TRUE, FALSE)
  )




### Guest Names and Genders----
# fix mistake in Daniel Lubetzsky (spelled incorrectly some places, correctly in others)
shark_tank_us <- shark_tank_us |> 
  mutate(
    guest_name = ifelse(guest_name == "Daniel Lubetzsky", "Daniel Lubetzky", guest_name)
  )

# fix mistake in Nirv Tolia (spelled incorrectly here)
shark_tank_us <- shark_tank_us |> 
  mutate(
    guest_name = ifelse(guest_name == "Nirv Tolia", "Nirav Tolia", guest_name)
  )

# handling the rotating guests-- what is their gender? did they invest? how much? etc.

guest_names <- shark_tank_us |> 
  filter(is.na(guest_name) == FALSE) |> 
  select(guest_name) |> 
  unique()


# guest investor genders
shark_tank_us <- shark_tank_us |> 
  mutate(
    guest_gender = case_when(
      guest_name == "Kevin Harrington" ~ "M",
      guest_name == "Jeff Foxworthy" ~ "M",
      guest_name == "John Paul DeJoria" ~ "M",
      guest_name == "Steve Tisch" ~ "M",
      guest_name == "Nick Woodman" ~ "M",
      guest_name == "Ashton Kutcher" ~ "M",
      guest_name == "Troy Carter" ~ "M",
      guest_name == "Chris Sacca" ~ "M",
      guest_name == "Richard Branson" ~ "M",
      guest_name == "Rohan Oza" ~ "M",
      guest_name == "Alex Rodriguez" ~ "M",
      guest_name == "Sara Blakely" ~ "F",
      guest_name == "Jamie Siminoff" ~ "M",
      guest_name == "Matt Higgins" ~ "M",
      guest_name == "Charles Barkley" ~ "M",
      guest_name == "Alli Webb" ~ "F",
      guest_name == "Daniel Lubetzky" ~ "M",
      guest_name == "Anne Wojcicki" ~ "F",
      guest_name == "Maria Sharapova" ~ "F",
      guest_name == "Katrina Lake" ~ "F",
      guest_name == "Blake Mycoskie" ~ "M",
      guest_name == "Kendra Scott" ~ "F",
      guest_name == "Emma Grede" ~ "F",
      guest_name == "Peter Jones" ~ "M",
      guest_name == "Nirav Tolia" ~ "M",
      guest_name == "Kevin Hart" ~ "M",
      guest_name == "Gwyneth Paltrow" ~ "F",
      guest_name == "Tony Xu" ~ "M",
      guest_name == NA ~ NA
    )
  )

  

### creating difference in asked for vs received investment (per shark), equity (per shark), valuation variables----
shark_tank_us <- shark_tank_us |> 
  mutate(
    investment_difference = ifelse(got_deal == TRUE, investment_amount_per_shark - original_ask_amount, NA),
    
    equity_difference = ifelse(got_deal == TRUE, equity_per_shark - original_offered_equity, NA),
    
    valuation_difference = ifelse(got_deal == TRUE, deal_valuation - valuation_requested, NA)) |> 
  relocate(
    investment_difference:valuation_difference,
    .after = equity_per_shark
  )

### removing irrelevant variables ----

shark_tank_us <- shark_tank_us |> 
  select(!pitchers_city)

shark_tank_us <- shark_tank_us |> 
  select(!pitchers_state)

shark_tank_us <- shark_tank_us |> 
  select(!company_website)


### Save the final cleaned dataset----

write_csv(shark_tank_us, "data/shark_tank_us.csv")
