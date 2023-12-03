# Final project Multivariate Analysis----
# Stat 301-1

## Load Packages ----

library(tidyverse)
library(skimr)
library(janitor)

## Load Data ----
shark_tank_us <- read_csv("data/shark_tank_us.csv")


# (1) female entrepreneurs are represented as often as male entrepreneurs in the show, 
# (2) if female entrepreneurs ask for similar amounts of investment, valuation, and equity in their businesses as male entrepreneurs, 
# (3) female entrepreneurs receive investment as often as male entrepreneurs, 
# (4) if that investment aligns with their initial demands (i.e. quality of investment),
# (5) which sharks invest in female entrepreneurs more often, and 
# (6) if specific sharks undervalue businesses started by women compared to men. Additionally, I plan to investigate 
# (7) which types of businesses attract more female entreprenurs, 
# (8) if these businesses receive investment as often by investors, and 
# (9) which sharks invest most often in these industries.

### typical requested investment, equity and valuation by gender----

# typical investments asked for by gender
shark_tank_us |> 
  group_by(pitchers_gender) |> 
  skim_without_charts(original_ask_amount)

median_investment_asked_gender <- shark_tank_us |> 
  group_by(pitchers_gender) |> 
  summarise(median_value = median(original_ask_amount), 
            median_label = str_c("Median Investment Requested: $", median_value))


# mixed team median both 200000 but female only 150000
gender_colors <- c("Male" = "#5484FF", "Female" = "#EC837D", "Mixed Team" = "#11B642")

gender_investments_asked_plot <-
  shark_tank_us |> 
  ggplot(aes(x = original_ask_amount, fill = pitchers_gender)) +
  geom_histogram(binwidth = 50000, color = "white", show.legend = FALSE) +
  facet_wrap(~ pitchers_gender) +
  scale_fill_manual(values = gender_colors) + 
  theme_light() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 7)) +
  coord_cartesian(xlim = c(0, 1000000)) +
  geom_vline(data = median_investment_asked_gender, aes(xintercept = median_value), color = "red") +
  geom_text(data = median_investment_asked_gender,
            aes(x = median_value , y = 105, label = "Median Investment \nRequested"), vjust = 1.25, size = 2.5, angle = 90) +
  labs(
    x = "Investment Requested (USD)",
    y = "Count", 
    title = "Typical Requested Investment on Shark Tank",
    subtitle = "The typical investment requested by all male and mixed teams is approximately 50,000 greater than female teams",
    caption = "Source: Thirumani et al"
  ) 

ggsave(filename = "plots/gender_investments_asked_plot.png",
       plot = gender_investments_asked_plot,
       width = 8,
       height = 6,
       units = "in"
)

# typical equity asked for by gender
shark_tank_us |> 
  group_by(pitchers_gender) |> 
  skim_without_charts(original_offered_equity)

median_equity_offered_gender <- shark_tank_us |> 
  group_by(pitchers_gender) |> 
  summarise(median_value = median(original_offered_equity), 
            median_label = str_c("Median Equity Offered: ", median_value, "%"))

equity_asked_plot_gender <-
  shark_tank_us |> 
  ggplot(aes(x = original_offered_equity, fill = pitchers_gender)) +
  geom_histogram(binwidth = 5, color = "white", show.legend = FALSE) +
  facet_wrap(~ pitchers_gender) +
  geom_vline(data = median_investment_asked_gender, 
             aes(xintercept = median_value), 
             color = "red") +
  geom_text(data = median_investment_asked_gender,
            aes(x = median_value , y = 105, label = median_label),
            vjust = 1.25, 
            size = 2.5, 
            angle = 90) +
  theme_light() +
  labs(
    x = "Equity Originally Offered (in %)",
    title = "Typical Originally Offered Equity (in %) on Shark Tank",
    subtitle = "The typical equity offered is approximately 10 percent",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/equity_asked_plot_gender.png",
       plot = equity_asked_plot_gender,
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










