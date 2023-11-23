# Final project ----
# Stat 301-1

## Load Packages ----

library(tidyverse)
library(skimr)
library(janitor)

## Load Data ----
shark_tank_us <- read_csv("data/shark_tank_us_data.csv")

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


### removing irrelevant variables ----

shark_tank_us <- shark_tank_us |> 
  select(!pitchers_city)

shark_tank_us <- shark_tank_us |> 
  select(!pitchers_state)

shark_tank_us <- shark_tank_us |> 
  select(!company_website)


### Univariate Analysis Plots----

# male/female/mixed team ratio
gender_colors <- c("Male" = "#5484FF", "Female" = "#EC837D", "Mixed Team" = "#11B642")

gender_ratios_plot <- shark_tank_us |> 
ggplot(aes(x = pitchers_gender, fill = pitchers_gender)) +
  geom_bar() +
  scale_fill_manual(values = gender_colors) +
  theme_light() +
  labs(
    x = "Pitcher's Gender",
    y = "Count",
    title = "Shark Tank Entrepreneur Teams by Gender",
    subtitle = "There are substantially more male entrepreneurs and all male teams than female entrepreneurs and mixed teams.",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/gender_ratios_plot.png",
       plot = gender_ratios_plot,
       width = 8,
       height = 6,
       units = "in"
       )

# types of businesses represented

businesses_represented_plot <-
shark_tank_us |> 
  ggplot(aes(x = industry, fill = industry)) +
    geom_bar() +
    theme_light() +
    theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 7)) +
    labs(
      x = "Type of Business",
      y = "Count",
      title = "Shark Tank Types of Businesses and Industries",
      subtitle = "The most represented industries are Food/Beverage, Fashion/Beauty, and Lifestyle/Home.",
      caption = "Source: Thirumani et al"
      )

ggsave(filename = "plots/businesses_represented_plot.png",
       plot = businesses_represented_plot,
       width = 8,
       height = 10,
       units = "in"
)

# typical valuation
shark_tank_us |> 
  skim_without_charts(valuation_requested)


typical_valuation_plot <-
  shark_tank_us |> 
  ggplot(aes(x = valuation_requested)) +
  geom_boxplot() +
  theme_light() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 7)) +
  coord_cartesian(xlim = c(0, 10000000)) +
  labs(
    x = "Valuation Requested (USD)",
    title = "Typical Business Valuation Requested on Shark Tank",
    subtitle = "The typical valuation requested is approximately 1,500,000 USD",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/typical_valuation_plot.png",
       plot = typical_valuation_plot,
       width = 8,
       height = 6,
       units = "in"
)

# typical investments
shark_tank_us |> 
  filter(total_deal_amount > 0) |> 
  skim_without_charts(total_deal_amount)

typical_investment_plot <-
shark_tank_us |> 
  filter(total_deal_amount > 0) |> 
  ggplot(aes(x = total_deal_amount)) +
  geom_boxplot() +
  theme_light() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 7)) +
  coord_cartesian(xlim = c(0, 1000000)) +
  labs(
    x = "Typical Investment Received",
    title = "Typical Investment Received on Shark Tank",
    subtitle = "The typical investment received is approximately 200,000 USD",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/typical_investment_plot.png",
       plot = typical_investment_plot,
       width = 8,
       height = 6,
       units = "in"
)

# typical equities
shark_tank_us |> 
  filter(total_deal_equity > 0) |> 
  skim_without_charts(total_deal_equity)

typical_equity_plot <-
  shark_tank_us |> 
  filter(total_deal_equity > 0) |> 
  ggplot(aes(x = total_deal_equity)) +
  geom_boxplot() +
  theme_light() +
  labs(
    x = "Typical Equity Received (in %)",
    title = "Typical Equity Received (in %)",
    subtitle = "The typical equity received is approximately 20 percent",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/typical_equity_plot.png",
       plot = typical_equity_plot,
       width = 8,
       height = 6,
       units = "in"
)

