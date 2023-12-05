# Final project Multivariate Analysis Part 2----
# Stat 301-1

## Load Packages ----

library(tidyverse)
library(skimr)
library(janitor)

## Load Data ----
shark_tank_us <- read_csv("data/shark_tank_us.csv")

## Part: individual shark behavior broken down by gender 

### which sharks invest in female entrepreneurs more often----

bc_freq_invest_women_summary <- shark_tank_us |> 
  filter(barbara_corcoran_invested == TRUE) |> 
  summarize(
    shark_name = "Barbara Corcoran",
    total = n(),
    count_women = sum(pitchers_gender == "Female"),
    count_men = sum(pitchers_gender == "Male"),
    count_mixed = sum(pitchers_gender == "Mixed Team"),
    pct_women = count_women/total * 100,
    pct_men = count_men/total * 100,
    pct_mixed = count_mixed/total * 100,
  ) |> 
  arrange(desc(pct_women))

mc_freq_invest_women_summary <- shark_tank_us |> 
  filter(mark_cuban_invested == TRUE) |> 
  summarize(
    shark_name = "Mark Cuban",
    total = n(),
    count_women = sum(pitchers_gender == "Female"),
    count_men = sum(pitchers_gender == "Male"),
    count_mixed = sum(pitchers_gender == "Mixed Team"),
    pct_women = count_women/total * 100,
    pct_men = count_men/total * 100,
    pct_mixed = count_mixed/total * 100,
  ) |> 
  arrange(desc(pct_women))

lg_freq_invest_women_summary <- shark_tank_us |> 
  filter(lori_greiner_invested == TRUE) |> 
  summarize(
    shark_name = "Lori Greiner",
    total = n(),
    count_women = sum(pitchers_gender == "Female"),
    count_men = sum(pitchers_gender == "Male"),
    count_mixed = sum(pitchers_gender == "Mixed Team"),
    pct_women = count_women/total * 100,
    pct_men = count_men/total * 100,
    pct_mixed = count_mixed/total * 100,
  ) |> 
  arrange(desc(pct_women))

rh_freq_invest_women_summary <- shark_tank_us |> 
  filter(robert_herjavec_invested == TRUE) |> 
  summarize(
    shark_name = "Robert Herjavec",
    total = n(),
    count_women = sum(pitchers_gender == "Female"),
    count_men = sum(pitchers_gender == "Male"),
    count_mixed = sum(pitchers_gender == "Mixed Team"),
    pct_women = count_women/total * 100,
    pct_men = count_men/total * 100,
    pct_mixed = count_mixed/total * 100,
  ) |> 
  arrange(desc(pct_women))

dj_freq_invest_women_summary <- shark_tank_us |> 
  filter(daymond_john_invested == TRUE) |> 
  summarize(
    shark_name = "Daymond John",
    total = n(),
    count_women = sum(pitchers_gender == "Female"),
    count_men = sum(pitchers_gender == "Male"),
    count_mixed = sum(pitchers_gender == "Mixed Team"),
    pct_women = count_women/total * 100,
    pct_men = count_men/total * 100,
    pct_mixed = count_mixed/total * 100,
  ) |> 
  arrange(desc(pct_women))

kol_freq_invest_women_summary <- shark_tank_us |> 
  filter(kevin_o_leary_invested == TRUE) |> 
  summarize(
    shark_name = "Kevin O'Leary",
    total = n(),
    count_women = sum(pitchers_gender == "Female"),
    count_men = sum(pitchers_gender == "Male"),
    count_mixed = sum(pitchers_gender == "Mixed Team"),
    pct_women = count_women/total * 100,
    pct_men = count_men/total * 100,
    pct_mixed = count_mixed/total * 100,
  ) |> 
  arrange(desc(pct_women))

female_guest_freq_invest_women_summary <- shark_tank_us |> 
  filter(guest_invested == TRUE) |> 
  filter(guest_gender == "F") |> 
  summarize(
    shark_name = "Female Guest",
    total = n(),
    count_women = sum(pitchers_gender == "Female"),
    count_men = sum(pitchers_gender == "Male"),
    count_mixed = sum(pitchers_gender == "Mixed Team"),
    pct_women = count_women/total * 100,
    pct_men = count_men/total * 100,
    pct_mixed = count_mixed/total * 100,
  ) |> 
  arrange(desc(pct_women))

male_guest_freq_invest_women_summary <- shark_tank_us |> 
  filter(guest_invested == TRUE) |> 
  filter(guest_gender == "M") |> 
  summarize(
    shark_name = "Male Guest",
    total = n(),
    count_women = sum(pitchers_gender == "Female"),
    count_men = sum(pitchers_gender == "Male"),
    count_mixed = sum(pitchers_gender == "Mixed Team"),
    pct_women = count_women/total * 100,
    pct_men = count_men/total * 100,
    pct_mixed = count_mixed/total * 100,
  ) |> 
  arrange(desc(pct_women))

freq_invest_women_summary_by_shark <- 
  bind_rows(male_guest_freq_invest_women_summary, 
            female_guest_freq_invest_women_summary,
            kol_freq_invest_women_summary,
            dj_freq_invest_women_summary,
            rh_freq_invest_women_summary,
            lg_freq_invest_women_summary,
            mc_freq_invest_women_summary, 
            bc_freq_invest_women_summary) |> 
  arrange(desc(pct_women))

freq_invest_women_plot_by_shark <- freq_invest_women_summary_by_shark |> 
  ggplot(aes(reorder(x = shark_name, desc(pct_women)), y = pct_women, fill = reorder(x = shark_name, desc(pct_women)))) +
  geom_col() +
  theme(axis.text.x = element_blank()) +
  labs(
    x = "Shark",
    y = "Percent of Investments in Female-Led Businesses",
    title = "Percent of Investments in Female-Led Businesses Per Shark on Shark Tank (US)",
    subtitle = "Female sharks invest in female-led businesses more often than male sharks.",
    caption = "Source: Thirumani et al",
    fill = "Shark"
  ) 

ggsave(filename = "plots/freq_invest_women_plot_by_shark.png",
       plot = freq_invest_women_plot_by_shark,
       width = 8,
       height = 6,
       units = "in"
)


### how are these female entrepreneurs treated by female vs. male sharks----

female_entrepreneurs_female_sharks <- shark_tank_us |> 
  filter(total_deal_amount > 0) |> 
  filter(pitchers_gender == "Female") |> 
  filter(barbara_corcoran_invested == TRUE | lori_greiner_invested == TRUE | guest_gender == "F")

female_entrepreneurs_male_sharks <- shark_tank_us |> 
  filter(total_deal_amount > 0) |> 
  filter(pitchers_gender == "Female") |> 
  filter(mark_cuban_invested == TRUE | robert_herjavec_invested == TRUE | daymond_john_invested == TRUE | kevin_o_leary_invested == TRUE| guest_gender == "M")


# female entrepreneurs difference in investments female sharks
median_investment_difference_female_entrepreneurs_female_sharks <- female_entrepreneurs_female_sharks |> 
  filter(total_deal_amount > 0) |> 
  summarise(median_value = median(investment_difference), 
            median_label = str_c("Median Difference: $", format(median_value, scientific = FALSE)))

investment_diff_female_entrepeneurs_female_sharks_plot <-
  female_entrepreneurs_female_sharks |> 
  filter(total_deal_amount > 0) |> 
  ggplot(aes(x = investment_difference, fill = pitchers_gender)) +
  geom_histogram(binwidth = 25000, color = "white", show.legend = FALSE) +
  scale_fill_manual(values = gender_colors) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 7)) +
  coord_cartesian(xlim = c(-100000, 100000), ylim = c(0, 100)) +
  geom_vline(data = median_investment_difference_gender, aes(xintercept = median_value), color = "red") +
  geom_text(data = median_investment_difference_gender,
            aes(x = median_value , y = 85, label = median_label), vjust = 2.25, size = 3, angle = 90) +
  labs(
    x = "Investment Difference (USD)",
    y = "Count", 
    title = "Investment Difference (USD) for Female Entrepreneurs with Female Sharks on Shark Tank (US)",
    subtitle = "Median investment difference is approximately $0.",
    caption = "Source: Thirumani et al"
  ) 

ggsave(filename = "plots/investment_diff_female_entrepeneurs_female_sharks_plot.png",
       plot = investment_diff_female_entrepeneurs_female_sharks_plot,
       width = 8,
       height = 6,
       units = "in"
)

# female entrepreneurs difference in investments male sharks
median_investment_difference_female_entrepreneurs_male_sharks <- female_entrepreneurs_male_sharks |> 
  filter(total_deal_amount > 0) |> 
  summarise(median_value = median(investment_difference), 
            median_label = str_c("Median Difference: $", format(median_value, scientific = FALSE)))

investment_diff_female_entrepeneurs_male_sharks_plot <-
  female_entrepreneurs_male_sharks |> 
  filter(total_deal_amount > 0) |> 
  ggplot(aes(x = investment_difference, fill = pitchers_gender)) +
  geom_histogram(binwidth = 25000, color = "white", show.legend = FALSE) +
  scale_fill_manual(values = gender_colors) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 7)) +
  coord_cartesian(xlim = c(-100000, 100000), ylim = c(0, 105)) +
  geom_vline(data = median_investment_difference_gender, aes(xintercept = median_value), color = "red") +
  geom_text(data = median_investment_difference_gender,
            aes(x = median_value , y = 90, label = median_label), vjust = 2.25, size = 3, angle = 90) +
  labs(
    x = "Investment Difference (USD)",
    y = "Count", 
    title = "Investment Difference (USD) for Female Entrepreneurs with Male Sharks on Shark Tank (US)",
    subtitle = "Median investment difference is approximately $0.",
    caption = "Source: Thirumani et al"
  ) 

ggsave(filename = "plots/investment_diff_female_entrepeneurs_male_sharks_plot.png",
       plot = investment_diff_female_entrepeneurs_male_sharks_plot,
       width = 8,
       height = 6,
       units = "in"
)


# female entrepreneurs difference in equity female sharks 
median_equity_diff_female_entrepreneurs_female_sharks <- female_entrepreneurs_female_sharks |> 
  filter(total_deal_equity > 0) |> 
  summarise(median_value = median(equity_difference), 
            median_label = str_c("Median Equity Difference: ", median_value, "%"))

equity_diff_female_entrepreneurs_female_sharks <-
  female_entrepreneurs_female_sharks |> 
  filter(total_deal_equity > 0) |> 
  ggplot(aes(x = equity_difference, fill = pitchers_gender)) +
  geom_histogram(binwidth = 5, color = "white", show.legend = FALSE) +
  coord_cartesian(xlim = c(-50, 50), ylim = c(0, 95)) +
  geom_vline(data = median_equity_diff_female_entrepreneurs_female_sharks, 
             aes(xintercept = median_value), 
             color = "red") +
  geom_text(aes(x = 5 , y = 70, label = "Median Equity Difference: 5%"),
            vjust = 1.25,
            size = 3,
            angle = 90
  ) +
  theme_light() +
  labs(
    x = "Equity Difference (in %)",
    title = "Difference in Equity for Female Entrepreneurs with Female Sharks on Shark Tank (US)",
    subtitle = "Typically 5% more equity given than initially offered.",
    caption = "Source: Thirumani et al",
    y = "Count"
  )

ggsave(filename = "plots/equity_diff_female_entrepreneurs_female_sharks.png",
       plot = equity_diff_female_entrepreneurs_female_sharks,
       width = 8,
       height = 6,
       units = "in"
)


# female entrepreneurs difference in equity male sharks 
median_equity_diff_female_entrepreneurs_male_sharks <- female_entrepreneurs_male_sharks |> 
  filter(total_deal_equity > 0) |> 
  summarise(median_value = median(equity_difference), 
            median_label = str_c("Median Equity: ", median_value, "%"))

equity_diff_female_entrepreneurs_male_sharks <-
  female_entrepreneurs_male_sharks |> 
  filter(total_deal_equity > 0) |> 
  ggplot(aes(x = equity_difference, fill = pitchers_gender)) +
  geom_histogram(binwidth = 5, color = "white", show.legend = FALSE) +
  coord_cartesian(xlim = c(-50, 50), ylim = c(0, 110)) +
  geom_vline(data = median_equity_diff_female_entrepreneurs_male_sharks, 
             aes(xintercept = median_value), 
             color = "red") +
  geom_text(aes(x = 5 , y = 90, label = "Median Equity Difference: 5%"),
            vjust = 1.25,
            size = 3,
            angle = 90
  ) +
  theme_light() +
  labs(
    x = "Equity Difference (in %)",
    y = "Count",
    title = "Difference in Equity for Female Entrepreneurs with Male Sharks on Shark Tank (US)",
    subtitle = "Typically 5% more equity given than initially offered.",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/equity_diff_female_entrepreneurs_male_sharks.png",
       plot = equity_diff_female_entrepreneurs_male_sharks,
       width = 8,
       height = 6,
       units = "in"
)

# female entrepreneurs difference in valuation female sharks 
median_valuation_diff_female_entrepreneurs_female_sharks <- female_entrepreneurs_female_sharks |> 
  filter(deal_valuation > 0) |> 
  summarize(median_value = median(valuation_difference), 
            median_label = str_c("Median Valuation: ", format(round(median_value, -3), scientific = FALSE)))


valuation_diff_female_entrepreneurs_female_sharks_plot <-
  female_entrepreneurs_female_sharks |> 
  filter(total_deal_equity > 0) |> 
  ggplot(aes(x = equity_difference, fill = pitchers_gender)) +
  geom_histogram(binwidth = 5, color = "white", show.legend = FALSE) +
  coord_cartesian(xlim = c(-25, 50), ylim = c(0, 110)) +
  geom_vline(data = median_equity_diff_female_entrepreneurs_male_sharks, 
             aes(xintercept = median_value), 
             color = "red") +
  geom_text(aes(x = 5 , y = 75, label = "Median Valuation Difference: -333000"),
            vjust = 1.25,
            size = 3,
            angle = 90
  ) +
  theme_light() +
  labs(
    x = "Valuation Difference (in USD)",
    y = "Count",
    title = "Difference in Valuation for Female Entrepreneurs with Female Sharks on Shark Tank (US)",
    subtitle = "Typical difference in valuation is -333000 USD.",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/valuation_diff_female_entrepreneurs_female_sharks_plot.png",
       plot = valuation_diff_female_entrepreneurs_female_sharks_plot,
       width = 8,
       height = 6,
       units = "in"
)

# female entrepreneurs difference in valuation male sharks 
median_valuation_diff_female_entrepreneurs_male_sharks <- female_entrepreneurs_male_sharks |> 
  filter(total_deal_amount > 0) |> 
  summarize(median_value = median(valuation_difference), 
            median_label = str_c("Median Valuation: ", format(round(median_value, -3), scientific = FALSE)))

valuation_diff_female_entrepreneurs_male_sharks_plot <-
  female_entrepreneurs_male_sharks |> 
  filter(total_deal_amount > 0) |> 
  ggplot(aes(x = valuation_difference, fill = pitchers_gender)) +
  geom_histogram(binwidth = 1000000, color = "white", show.legend = FALSE) +
  coord_cartesian(xlim = c(-10000000, 5000000), ylim = c(0, 110)) +
  geom_vline(aes(xintercept = -500000), 
             color = "red") +
  geom_text(aes(x = 5 , y = 75, label = "Median Valuation Difference: -500000"),
            vjust = - 3,
            size = 3,
            angle = 90
  ) +
  theme_light() +
  labs(
    x = "Valuation Difference (in USD)",
    y = "Count",
    title = "Difference in Valuation for Female Entrepreneurs with Male Sharks on Shark Tank (US)",
    subtitle = "Typical difference in valuation is -500000 USD.",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/valuation_diff_female_entrepreneurs_male_sharks_plot.png",
       plot = valuation_diff_female_entrepreneurs_male_sharks_plot,
       width = 8,
       height = 6,
       units = "in"
)
### which industries do sharks invest in more often (female dominated?)----