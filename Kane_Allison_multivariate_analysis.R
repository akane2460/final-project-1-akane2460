# Final project Multivariate Analysis----
# Stat 301-1

## Load Packages ----

library(tidyverse)
library(skimr)
library(janitor)

## Load Data ----
shark_tank_us <- read_csv("data/shark_tank_us.csv")

# (5) which sharks invest in female entrepreneurs more often, and 
# (6) if specific sharks undervalue businesses started by women compared to men. Additionally, I plan to investigate 


### frequency of investments in women run businesses----

frequency_colors <- c("FALSE" = "#EF664F", "TRUE" = "#5FBF53")

freq_of_investments_gender_summary <- shark_tank_us |> 
  group_by(pitchers_gender) |> 
  summarize(
    deals_made = sum(got_deal == TRUE),
    deals_passed = sum(got_deal == FALSE),
    total_pitches = n(),
    prop_made = deals_made / total_pitches,
    prop_passed = deals_passed / total_pitches,
    pct_made = deals_made / total_pitches * 100,
    pct_passed = deals_passed / total_pitches * 100
  )

gender_freq_of_investments_plot <- shark_tank_us |> 
  ggplot(aes(x = pitchers_gender, fill = got_deal)) +
  geom_bar(position = "fill") +
  annotate(geom = "text", x = 1, y = 0.606, label = "63.6%") +
  annotate(geom = "text", x = 2, y = 0.535, label = "56.5%") +
  annotate(geom = "text", x = 3, y = 0.624, label = "65.4%") +
  annotate(geom = "text", x = 1, y = .97, label = "36.4%") +
  annotate(geom = "text", x = 2, y = .97, label = "43.5%") +
  annotate(geom = "text", x = 3, y = .97, label = "34.6%") +
  scale_fill_manual(name = "Deal Made?", values = frequency_colors, labels = c("No Deal", "Deal")) +
  labs(
    x = "Pitcher's Gender",
    y = "",
    fill = "Got deal?",
    title = "Percent of Deals Made by Gender on Shark Tank (US)",
    subtitle = "Women and mixed gender teams make deals more frequently than all male teams",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/gender_freq_of_investments_plot.png",
       plot = gender_freq_of_investments_plot,
       width = 8,
       height = 6,
       units = "in"
)

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

gender_equity_asked_plot <-
  shark_tank_us |> 
  ggplot(aes(x = original_offered_equity, fill = pitchers_gender)) +
  geom_histogram(binwidth = 5, color = "white", show.legend = FALSE) +
  facet_wrap(~ pitchers_gender) +
  coord_cartesian(xlim = c(0, 50)) +
  geom_vline(data = median_equity_offered_gender, 
             aes(xintercept = median_value), 
             color = "red") +
  geom_text(data = median_equity_offered_gender,
            aes(x = median_value , y = 175, label = "Median Equity Offered"),
            vjust = 2.5, 
            size = 2.5, 
            angle = 90 
            ) +
  theme_light() +
  labs(
    x = "Equity Originally Offered (in %)",
    title = "Typical Originally Offered Equity (in %) on Shark Tank",
    subtitle = "Female teams typically offer 2% more equity than male and mixed teams.",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/gender_equity_asked_plot.png",
       plot = gender_equity_asked_plot,
       width = 8,
       height = 6,
       units = "in"
)

# typical valuation requested
shark_tank_us |> 
  group_by(pitchers_gender) |> 
  skim_without_charts(valuation_requested) 

median_valuation_requested_gender <- shark_tank_us |> 
  group_by(pitchers_gender) |> 
  summarise(median_value = median(valuation_requested), 
            median_label = str_c("Median Valuation: ", median_value))

gender_valuation_requested_plot <-
  shark_tank_us |> 
  ggplot(aes(x = valuation_requested, fill = pitchers_gender)) +
  geom_histogram(binwidth = 500000, color = "white", show.legend = FALSE) +
  facet_wrap(~ pitchers_gender) +
  geom_vline(data = median_valuation_requested_gender, 
             aes(xintercept = median_value), 
             color = "red") +
  geom_text(data = median_valuation_requested_gender,
            aes(x = median_value , y = 100, label = "Median Initial Valuation"),
            vjust = 1.5, 
            size = 3, 
            angle = 90) +
  scale_fill_manual(values = gender_colors) + 
  theme_light() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 7)) +
  coord_cartesian(xlim = c(0, 10000000)) +
  labs(
    x = "Valuation Requested (USD)",
    title = "Typical Business Valuation Requested on Shark Tank By Gender",
    subtitle = "Female run businesses are initially valued over 600,000 USD less than male-run businesses.",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/gender_valuation_requested_plot.png",
       plot = gender_valuation_requested_plot,
       width = 8,
       height = 6,
       units = "in"
)



### typical investments, equity, and valuation received by gender----

# typical investments received by gender
shark_tank_us |> 
  filter(total_deal_amount > 0) |> 
  group_by(pitchers_gender) |> 
  skim_without_charts(total_deal_amount)

median_investment_received_gender <- shark_tank_us |> 
  filter(total_deal_amount > 0) |> 
  group_by(pitchers_gender) |> 
  summarise(median_value = median(total_deal_amount), 
            median_label = str_c("Median Investment: $", format(median_value, scientific = FALSE)))


# mixed team median both 200000 but female only 150000
gender_colors <- c("Male" = "#5484FF", "Female" = "#EC837D", "Mixed Team" = "#11B642")

gender_investments_received_plot <-
  shark_tank_us |> 
  filter(total_deal_amount > 0) |> 
  ggplot(aes(x = total_deal_amount, fill = pitchers_gender)) +
  geom_histogram(binwidth = 100000, color = "white", show.legend = FALSE) +
  facet_wrap(~ pitchers_gender) +
  scale_fill_manual(values = gender_colors) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 7)) +
  coord_cartesian(xlim = c(0, 1250000)) +
  geom_vline(data = median_investment_received_gender, aes(xintercept = median_value), color = "red") +
  geom_text(data = median_investment_received_gender,
            aes(x = median_value , y = 90, label = median_label), vjust = 1.75, size = 3, angle = 90) +
  labs(
    x = "Investment Received (USD)",
    y = "Count", 
    title = "Typical Requested Investment on Shark Tank",
    subtitle = "The typical investment received by all male teams is approximately 50,000 greater than female teams",
    caption = "Source: Thirumani et al"
  ) 

# is this just a case where they are asking for more and therefore get more?

ggsave(filename = "plots/gender_investments_received_plot.png",
       plot = gender_investments_received_plot,
       width = 8,
       height = 6,
       units = "in"
)

# typical equity received by gender
shark_tank_us |> 
  filter(total_deal_equity > 0) |> 
  group_by(pitchers_gender) |> 
  skim_without_charts(total_deal_equity)

median_equity_received_gender <- shark_tank_us |> 
  filter(total_deal_equity > 0) |> 
  group_by(pitchers_gender) |> 
  summarise(median_value = median(total_deal_equity), 
            median_label = str_c("Median Equity: ", median_value, "%"))

gender_equity_received_plot <-
  shark_tank_us |> 
  filter(total_deal_equity > 0) |> 
  ggplot(aes(x = total_deal_equity, fill = pitchers_gender)) +
  geom_histogram(binwidth = 5, color = "white", show.legend = FALSE) +
  facet_wrap(~ pitchers_gender) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 95)) +
  geom_vline(data = median_equity_received_gender, 
             aes(xintercept = median_value), 
             color = "red") +
  geom_text(data = median_equity_received_gender,
            aes(x = median_value , y = 80, label = median_label),
            vjust = 1.75, 
            size = 3, 
            angle = 90 
  ) +
  theme_light() +
  labs(
    x = "Equity Received (in %)",
    title = "Typical Received Equity (in %) on Shark Tank",
    subtitle = "Female teams typically give 5% more equity than male teams.",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/gender_equity_received_plot.png",
       plot = gender_equity_received_plot,
       width = 8,
       height = 6,
       units = "in"
)

# typical valuation received by gender
shark_tank_us |> 
  filter(deal_valuation > 0) |> 
  group_by(pitchers_gender) |> 
  skim_without_charts(deal_valuation) 

median_valuation_received_gender <- shark_tank_us |> 
  filter(deal_valuation > 0) |> 
  group_by(pitchers_gender) |> 
  summarise(median_value = median(deal_valuation), 
            median_label = str_c("Median Valuation: ", format(round(median_value, -3), scientific = FALSE)))

gender_valuation_received_plot <-
  shark_tank_us |> 
  ggplot(aes(x = deal_valuation, fill = pitchers_gender)) +
  geom_histogram(binwidth = 500000, color = "white", show.legend = FALSE) +
  facet_wrap(~ pitchers_gender) +
  geom_vline(data = median_valuation_received_gender, 
             aes(xintercept = median_value), 
             color = "red") +
  geom_text(data = median_valuation_received_gender,
            aes(x = median_value , y = 200, label = median_label),
            vjust = 1.5, 
            size = 3, 
            angle = 90) +
  scale_fill_manual(values = gender_colors) + 
  theme_light() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 7)) +
  coord_cartesian(xlim = c(0, 5000000)) +
  labs(
    x = "Valuation Received (USD)",
    title = "Typical Business Valuation Received on Shark Tank By Gender",
    subtitle = "Female run businesses are valued over 40,000 USD less than male-run businesses.",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/gender_valuation_received_plot.png",
       plot = gender_valuation_received_plot,
       width = 8,
       height = 6,
       units = "in"
)



### quality of investment----

# difference in investments by gender
shark_tank_us |> 
  filter(total_deal_amount > 0) |> 
  group_by(pitchers_gender) |> 
  skim_without_charts(investment_difference)

median_investment_difference_gender <- shark_tank_us |> 
  filter(total_deal_amount > 0) |> 
  group_by(pitchers_gender) |> 
  summarise(median_value = median(investment_difference), 
            median_label = str_c("Median Difference: $", format(median_value, scientific = FALSE)))

gender_investment_difference_plot <-
  shark_tank_us |> 
  filter(total_deal_amount > 0) |> 
  ggplot(aes(x = investment_difference, fill = pitchers_gender)) +
  geom_histogram(binwidth = 25000, color = "white", show.legend = FALSE) +
  facet_wrap(~ pitchers_gender) +
  scale_fill_manual(values = gender_colors) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 7)) +
  coord_cartesian(xlim = c(-100000, 100000)) +
  geom_vline(data = median_investment_difference_gender, aes(xintercept = median_value), color = "red") +
  geom_text(data = median_investment_difference_gender,
            aes(x = median_value , y = 150, label = median_label), vjust = 2.25, size = 3, angle = 90) +
  labs(
    x = "Investment Difference (USD)",
    y = "Count", 
    title = "Difference in Investment Requested and Investment Received (USD) by Gender on Shark Tank (US)",
    subtitle = "Median investment difference is approximately the same across all genders.",
    caption = "Source: Thirumani et al"
  ) 

ggsave(filename = "plots/gender_investment_difference_plot.png",
       plot = gender_investment_difference_plot,
       width = 8,
       height = 6,
       units = "in"
)

# difference in equity by gender
shark_tank_us |> 
  filter(total_deal_equity > 0) |> 
  group_by(pitchers_gender) |> 
  skim_without_charts(equity_difference)

median_equity_difference_gender <- shark_tank_us |> 
  filter(total_deal_equity > 0) |> 
  group_by(pitchers_gender) |> 
  summarise(median_value = median(equity_difference), 
            median_label = str_c("Median Equity: \n", median_value, "%"))

gender_equity_difference_plot <-
  shark_tank_us |> 
  filter(total_deal_equity > 0) |> 
  ggplot(aes(x = equity_difference, fill = pitchers_gender)) +
  geom_histogram(binwidth = 5, color = "white", show.legend = FALSE) +
  facet_wrap(~ pitchers_gender) +
  coord_cartesian(xlim = c(-50, 50), ylim = c(0, 110)) +
  geom_vline(data = median_equity_difference_gender, 
             aes(xintercept = median_value), 
             color = "red") +
  geom_text(data = median_equity_difference_gender,
            aes(x = median_value , y = 90, label = median_label),
            vjust = 1.25, 
            size = 3, 
            angle = 90 
  ) +
  theme_light() +
  labs(
    x = "Equity Difference (in %)",
    title = "Difference in Equity Given and Offered (in %) by Gender on Shark Tank (US)",
    subtitle = "All genders typically give more equity than originally offered, mixed teams the most.",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/gender_equity_difference_plot.png",
       plot = gender_equity_difference_plot,
       width = 8,
       height = 6,
       units = "in"
)

# difference in valuation by gender
shark_tank_us |> 
  filter(deal_valuation > 0) |> 
  group_by(pitchers_gender) |> 
  skim_without_charts(valuation_difference) 

median_valuation_difference_gender <- shark_tank_us |> 
  filter(deal_valuation > 0) |> 
  group_by(pitchers_gender) |> 
  summarise(median_value = median(valuation_difference), 
            median_label = str_c("Median Valuation: ", format(round(median_value, -3), scientific = FALSE)))

gender_valuation_difference_plot <-
  shark_tank_us |> 
  ggplot(aes(x = valuation_difference, fill = pitchers_gender)) +
  geom_histogram(binwidth = 250000, color = "white", show.legend = FALSE) +
  facet_wrap(~ pitchers_gender) +
  geom_vline(data = median_valuation_difference_gender, 
             aes(xintercept = median_value), 
             color = "red") +
  geom_text(data = median_valuation_difference_gender,
            aes(x = median_value , y = 90, label = median_label),
            vjust = - .5, 
            size = 3, 
            angle = 90) +
  scale_fill_manual(values = gender_colors) + 
  theme_light() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 7)) +
  coord_cartesian(xlim = c(-5000000, 500000), ylim = c(0, 130)) +
  labs(
    x = "Valuation Received (USD)",
    title = "Typical Difference of Valuation Received and Valuation Requested by Gender on Shark Tank (US)",
    subtitle = "Female run businesses are valued over 40,000 USD less than male-run businesses.",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/gender_valuation_difference_plot.png",
       plot = gender_valuation_difference_plot,
       width = 10,
       height = 6,
       units = "in"
)


### what industries attract more women----
# what industries do female teams work in
industries_of_women_plot <- shark_tank_us |> 
  filter(pitchers_gender == "Female") |> 
  ggplot(aes(x = industry, fill = industry)) +
  geom_bar() +
  theme_light() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 7)) +
  labs(
    x = "Type of Business",
    y = "Count",
    title = "Shark Tank Types of Businesses and Industries Run by All-Female Teams",
    subtitle = "The most represented industries are Food/Beverage, Fashion/Beauty, Children/Education and Lifestyle/Home.",
    caption = "Source: Thirumani et al",
    fill = "Industry"
  )

ggsave(filename = "plots/industries_of_women_plot.png",
       plot = industries_of_women_plot,
       width = 10,
       height = 6,
       units = "in"
)

# what percentage of each industry is made up of female teams
women_dominated_industries_summary <- shark_tank_us |> 
  group_by(industry) |> 
  summarize(
    total = n(),
    count_women = sum(pitchers_gender == "Female"),
    count_men = sum(pitchers_gender == "Male"),
    count_mixed = sum(pitchers_gender == "Mixed Team"),
    pct_women = count_women/total * 100,
    pct_men = count_men/total * 100,
    pct_mixed = count_mixed/total * 100,
  ) |> 
  arrange(desc(pct_women))

women_dominated_industries_plot <- shark_tank_us |> 
ggplot(aes(x = industry, fill = factor(pitchers_gender, levels = c("Mixed Team", "Male", "Female")))) +
  geom_bar(position = "fill") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 7)) +
  scale_fill_manual(values = gender_colors) +
  labs(
    x = "Type of Business",
    y = "Count",
    title = "Gender Representation in All Industries Featured on Shark Tank (US)",
    subtitle = "Women are most represented in Children/Education, Fashion/Beauty and Health/Wellness.",
    caption = "Source: Thirumani et al",
    fill = "Industry"
  )

ggsave(filename = "plots/women_dominated_industries_plot.png",
       plot = women_dominated_industries_plot,
       width = 10,
       height = 6,
       units = "in"
)


### do female dominated industries if these businesses receive investment as often ----

industry_investment_frequencies_summary <- shark_tank_us |> 
  group_by(industry) |> 
  summarize(
    deals_made = sum(got_deal == TRUE),
    deals_passed = sum(got_deal == FALSE),
    total_pitches = n(),
    prop_made = deals_made / total_pitches,
    prop_passed = deals_passed / total_pitches,
    pct_made = deals_made / total_pitches * 100,
    pct_passed = deals_passed / total_pitches * 100
  ) |> 
  arrange(desc(pct_made))

industry_investment_frequency_plot <- shark_tank_us |> 
  ggplot(aes(x = industry, fill = got_deal)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 7)) +
  scale_fill_manual(name = "Deal Made?", values = frequency_colors, labels = c("No Deal", "Deal")) +
  labs(
    x = "Industry",
    y = "",
    fill = "Got deal?",
    title = "Percent of Deals Made by Industry on Shark Tank (US)",
    subtitle = "Industries with deals made most often include Automotive, Lifestyle/Home and Uncertain/Other.",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/industry_investment_frequency_plot.png",
       plot = industry_investment_frequency_plot,
       width = 8,
       height = 6,
       units = "in"
)



### which sharks invest more in female-dominated industries ----
