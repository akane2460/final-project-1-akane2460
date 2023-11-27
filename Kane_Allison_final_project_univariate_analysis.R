# Final project Univariate Analysis----
# Stat 301-1

## Load Packages ----

library(tidyverse)
library(skimr)
library(janitor)

## Load Data ----
shark_tank_us <- read_csv("data/shark_tank_us.csv")


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

# how often deals are made

### Initial Asks----

# typical investments asked for

# typical equity asked for

# typical valuation requested
shark_tank_us |> 
  skim_without_charts(valuation_requested)

typical_valuation_plot <-
  shark_tank_us |> 
  ggplot(aes(x = valuation_requested)) +
  geom_histogram(binwidth = 500000, color = "white") +
  geom_vline(xintercept = 1500000, color = "#D52514") +
  annotate("text", x = 1850000, y = 200, label = "Median Valuation", angle = 90, size = 3) +
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


# typical valuation received

