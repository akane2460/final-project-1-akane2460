# Final project Univariate Analysis----
# Stat 301-1

## Load Packages ----

library(tidyverse)
library(skimr)
library(janitor)

## Load Data ----
shark_tank_us <- read_csv("data/shark_tank_us.csv")

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

# how often deals are made

deals_summary <- shark_tank_us |>
  summarize(
    deals_made = sum(got_deal == TRUE),
    deals_passed = sum(got_deal == FALSE),
    total_pitches = n(),
    pct_made = deals_made / total_pitches,
    pct_passed = deals_passed / total_pitches
  )
# 
# frequency_deal <- 
#   shark_tank_us |> 
#   ggplot(aes(x = ))

### Initial Asks----

# typical investments asked for
shark_tank_us |> 
  skim_without_charts(original_ask_amount)

investments_asked_plot <-
  shark_tank_us |> 
  ggplot(aes(x = original_ask_amount)) +
  geom_histogram(binwidth = 50000, color = "white") +
  geom_vline(xintercept = 200000, color = "#D52514") +
  annotate("text", x = 215000, y = 200, label = "Median Requested", angle = 90, size = 3) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 7)) +
  coord_cartesian(xlim = c(0, 1000000)) +
  labs(
    x = "Valuation Requested (USD)",
    y = "Count", 
    title = "Typical Requested Investment on Shark Tank",
    subtitle = "The typical investment requested originally is approximately 200,000 USD",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/investments_asked_plot.png",
       plot = investments_asked_plot,
       width = 8,
       height = 6,
       units = "in"
)

# typical equity asked for
shark_tank_us |> 
  skim_without_charts(original_offered_equity)

equity_asked_plot <-
  shark_tank_us |> 
  ggplot(aes(x = original_offered_equity)) +
  geom_histogram(binwidth = 5, color = "white") +
  geom_vline(xintercept = 10, color = "#D52514") +
  annotate("text", x = 14, y = 325, label = "Median Equity Offered", angle = 90, size = 3) +
  theme_light() +
  labs(
    x = "Equity Originally Offered (in %)",
    title = "Typical Originally Offered Equity (in %) on Shark Tank",
    subtitle = "The typical equity offered is approximately 10 percent",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/equity_asked_plot.png",
       plot = equity_asked_plot,
       width = 8,
       height = 6,
       units = "in"
)


# typical valuation requested
shark_tank_us |> 
  skim_without_charts(valuation_requested)

valuation_requested_plot <-
  shark_tank_us |> 
  ggplot(aes(x = valuation_requested)) +
  geom_histogram(binwidth = 500000, color = "white") +
  geom_vline(xintercept = 1500000, color = "#D52514") +
  annotate("text", x = 1650000, y = 200, label = "Median Valuation", angle = 90, size = 3) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 7)) +
  coord_cartesian(xlim = c(0, 10000000)) +
  labs(
    x = "Valuation Requested (USD)",
    title = "Typical Business Valuation Requested on Shark Tank",
    subtitle = "The typical valuation requested is approximately 1,500,000 USD",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/valuation_requested_plot.png",
       plot = valuation_requested_plot,
       width = 8,
       height = 6,
       units = "in"
)


### Received from Deal----

# typical investments received
shark_tank_us |> 
  filter(total_deal_amount > 0) |> 
  skim_without_charts(total_deal_amount)

typical_investment_plot <-
  shark_tank_us |> 
  filter(total_deal_amount > 0) |> 
  ggplot(aes(x = total_deal_amount)) +
  geom_histogram(binwidth = 65000, color = "white") +
  annotate("text", x = 225000, y = 150, label = "Median Investment", angle = 90, size = 3) +
  geom_vline(xintercept = 200000, color = "#D52514") +
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

# typical equities received
shark_tank_us |> 
  filter(total_deal_equity > 0) |> 
  skim_without_charts(total_deal_equity)

typical_equity_plot <-
  shark_tank_us |> 
  filter(total_deal_equity > 0) |> 
  ggplot(aes(x = total_deal_equity)) +
  geom_histogram(binwidth = 5, color = "white") +
  geom_vline(xintercept = 20, color = "#D52514") +
  annotate("text", x = 15, y = 125, label = "Median Equity", angle = 90, size = 3) +
  theme_light() +
  labs(
    x = "Typical Equity Received (in %)",
    title = "Typical Equity Received (in %) on Shark Tank",
    subtitle = "The typical equity received is approximately 20 percent",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/typical_equity_plot.png",
       plot = typical_equity_plot,
       width = 8,
       height = 6,
       units = "in"
)

# typical valuation received
shark_tank_us |> 
  filter(deal_valuation > 0) |> # filter to only include those who received deals
  skim_without_charts(deal_valuation)

typical_valuation_plot <-
  shark_tank_us |> 
  filter(deal_valuation > 0) |> 
  ggplot(aes(x = valuation_requested)) +
  geom_histogram(binwidth = 500000, color = "white") +
  geom_vline(xintercept = 1000000, color = "#D52514") +
  annotate("text", x = 1150000, y = 160, label = "Median Valuation", angle = 90, size = 3) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 7)) +
  coord_cartesian(xlim = c(0, 10000000), ylim = c(0, 225)) +
  labs(
    x = "Valuation Requested (USD)",
    title = "Typical Business Valuation Received on Shark Tank",
    subtitle = "The typical valuation received is approximately 1,000,000 USD",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/typical_valuation_plot.png",
       plot = typical_valuation_plot,
       width = 8,
       height = 6,
       units = "in"
)

