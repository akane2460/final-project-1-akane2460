# Final project Univariate Analysis----
# Stat 301-1

## Load Packages ----

library(tidyverse)
library(skimr)
library(janitor)

## Load Data ----
shark_tank_us <- read_csv("data/shark_tank_us.csv")

# male/female/mixed team ratio ----
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
    caption = "Source: Thirumani et al",
    fill = "Pitcher's Gender"
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
    caption = "Source: Thirumani et al",
    fill = "Industry"
  )

ggsave(filename = "plots/businesses_represented_plot.png",
       plot = businesses_represented_plot,
       width = 8,
       height = 10,
       units = "in"
)

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
    x = "Investment Requested (USD)",
    y = "Count", 
    title = "Typical Requested Investment on Shark Tank (US)",
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
  filter(total_deal_equity > 0) |> # filter to only include those who received deals
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


### Deal Frequencies and Investors----

# how often deals are made
deals_summary <- shark_tank_us |>
  summarize(
    deals_made = sum(got_deal == TRUE),
    deals_passed = sum(got_deal == FALSE),
    total_pitches = n(),
    pct_made = deals_made / total_pitches,
    pct_passed = deals_passed / total_pitches
  )

frequency_colors <- c("FALSE" = "#EF664F", "TRUE" = "#5FBF53")

frequency_plot <-
  shark_tank_us |> 
  ggplot(aes(x = got_deal, fill = got_deal)) +
  geom_bar() +
  scale_fill_manual(name = "Deal Made?", values = frequency_colors, labels = c("No Deal", "Deal")) +
  annotate(geom = "text", x = 2, y = 785, label = "60%", fontface = 2) +
  annotate(geom = "text", x = 1, y = 535, label = "40%", fontface = 2) +
  scale_x_discrete(labels =  c("No Deal", "Deal")) +
  coord_cartesian(ylim = c(0, 800)) +
  theme_light() +
  labs(
    x = "",
    y = "Count",
    title = "Frequency of deals made on Shark Tank (US)",
    subtitle = "Deals are made approximately 60% and not made 40% of the time.",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/frequency_plot.png",
       plot = frequency_plot,
       width = 6,
       height = 4,
       units = "in"
)


## Individual investor frequencies of deals

# barbara corcoran
shark_tank_us |>
  summarize(
    bc_deals_made = sum(barbara_corcoran_invested == TRUE),
    bc_deals_passed = sum(barbara_corcoran_invested == FALSE),
    bc_total_pitches = n(),
    bc_pct_made = bc_deals_made / bc_total_pitches,
    bc_pct_passed = bc_deals_passed / bc_total_pitches,
  )

# how often barbara invests when someone pitches
bc_invest_frequency <- shark_tank_us |> 
  ggplot(aes(x = barbara_corcoran_invested, fill = barbara_corcoran_invested)) +
  geom_bar() +
  scale_fill_manual(name = "Deal Made?", values = frequency_colors, labels = c("No Deal", "Deal")) +
  annotate(geom = "text", x = 2, y = 150, label = "9.3%", fontface = 2) +
  annotate(geom = "text", x = 1, y = 1200, label = "90.7%", fontface = 2) +
  scale_x_discrete(labels =  c("No Deal", "Deal")) +
  coord_cartesian(ylim = c(0, 1250)) +
  theme_light() +
  labs(
    x = "",
    y = "Count",
    title = "Frequency of deals made by Barbara Corcoran on Shark Tank (US)",
    subtitle = "When pitched to, Corcoran makes deals less than 10% of the time.",
    caption = "Source: Thirumani et al"
  )
  
ggsave(filename = "plots/bc_invest_frequency.png",
       plot = bc_invest_frequency,
       width = 6,
       height = 4,
       units = "in"
)


# mark cuban
shark_tank_us |>
  summarize(
    mc_deals_made = sum(mark_cuban_invested == TRUE),
    mc_deals_passed = sum(mark_cuban_invested == FALSE),
    mc_total_pitches = n(),
    mc_pct_made = mc_deals_made / mc_total_pitches,
    mc_pct_passed = mc_deals_passed / mc_total_pitches,
  )

# how often mark cuban invests when someone pitches
mc_invest_frequency <- shark_tank_us |> 
  ggplot(aes(x = mark_cuban_invested, fill = mark_cuban_invested)) +
  geom_bar() +
  scale_fill_manual(name = "Deal Made?", values = frequency_colors, labels = c("No Deal", "Deal")) +
  annotate(geom = "text", x = 2, y = 265, label = "18.1%", fontface = 2) +
  annotate(geom = "text", x = 1, y = 1075, label = "81.9%", fontface = 2) +
  scale_x_discrete(labels =  c("No Deal", "Deal")) +
  coord_cartesian(ylim = c(0, 1250)) +
  theme_light() +
  labs(
    x = "",
    y = "Count",
    title = "Frequency of deals made by Mark Cuban on Shark Tank (US)",
    subtitle = "When pitched to, Cuban makes deals nearly 20% of the time.",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/mc_invest_frequency.png",
       plot = mc_invest_frequency,
       width = 6,
       height = 4,
       units = "in"
)

# lori greiner
shark_tank_us |>
  summarize(
    lg_deals_made = sum(lori_greiner_invested == TRUE),
    lg_deals_passed = sum(lori_greiner_invested == FALSE),
    lg_total_pitches = n(),
    lg_pct_made = lg_deals_made / lg_total_pitches,
    lg_pct_passed = lg_deals_passed / lg_total_pitches,
  )

# how often lori greiner invests when someone pitches
lg_invest_frequency <- shark_tank_us |> 
  ggplot(aes(x = lori_greiner_invested, fill = lori_greiner_invested)) +
  geom_bar() +
  scale_fill_manual(name = "Deal Made?", values = frequency_colors, labels = c("No Deal", "Deal")) +
  annotate(geom = "text", x = 2, y = 230, label = "15.6%", fontface = 2) +
  annotate(geom = "text", x = 1, y = 1100, label = "84.4%", fontface = 2) +
  scale_x_discrete(labels =  c("No Deal", "Deal")) +
  coord_cartesian(ylim = c(0, 1250)) +
  theme_light() +
  labs(
    x = "",
    y = "Count",
    title = "Frequency of deals made by Lori Greiner on Shark Tank (US)",
    subtitle = "When pitched to, Greiner makes deals approximately 15% of the time.",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/lg_invest_frequency.png",
       plot = lg_invest_frequency,
       width = 6,
       height = 4,
       units = "in"
)

# robert herjavec
shark_tank_us |>
  summarize(
    rh_deals_made = sum(robert_herjavec_invested == TRUE),
    rh_deals_passed = sum(robert_herjavec_invested == FALSE),
    rh_total_pitches = n(),
    rh_pct_made = rh_deals_made / rh_total_pitches,
    rh_pct_passed = rh_deals_passed / rh_total_pitches,
  )

# how often robert herjavec invests when someone pitches
rh_invest_frequency <- shark_tank_us |> 
  ggplot(aes(x = robert_herjavec_invested, fill = robert_herjavec_invested)) +
  geom_bar() +
  scale_fill_manual(name = "Deal Made?", values = frequency_colors, labels = c("No Deal", "Deal")) +
  annotate(geom = "text", x = 2, y = 150, label = "9.6%", fontface = 2) +
  annotate(geom = "text", x = 1, y = 1175, label = "90.4%", fontface = 2) +
  scale_x_discrete(labels =  c("No Deal", "Deal")) +
  coord_cartesian(ylim = c(0, 1250)) +
  theme_light() +
  labs(
    x = "",
    y = "Count",
    title = "Frequency of deals made by Robert Herjavec on Shark Tank (US)",
    subtitle = "When pitched to, Herjavec makes deals less than 10% of the time.",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/rh_invest_frequency.png",
       plot = rh_invest_frequency,
       width = 6,
       height = 4,
       units = "in"
)

# daymond john
shark_tank_us |>
  summarize(
    dj_deals_made = sum(daymond_john_invested == TRUE),
    dj_deals_passed = sum(daymond_john_invested == FALSE),
    dj_total_pitches = n(),
    dj_pct_made = dj_deals_made / dj_total_pitches,
    dj_pct_passed = dj_deals_passed / dj_total_pitches,
  )

# how often daymond john invests when someone pitches
dj_invest_frequency <- shark_tank_us |> 
  ggplot(aes(x = daymond_john_invested, fill = daymond_john_invested)) +
  geom_bar() +
  scale_fill_manual(name = "Deal Made?", values = frequency_colors, labels = c("No Deal", "Deal")) +
  annotate(geom = "text", x = 2, y = 145, label = "8.8%", fontface = 2) +
  annotate(geom = "text", x = 1, y = 1200, label = "91.2%", fontface = 2) +
  scale_x_discrete(labels =  c("No Deal", "Deal")) +
  coord_cartesian(ylim = c(0, 1250)) +
  theme_light() +
  labs(
    x = "",
    y = "Count",
    title = "Frequency of deals made by Daymond John on Shark Tank (US)",
    subtitle = "When pitched to, John makes deals approximately 9% of the time.",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/dj_invest_frequency.png",
       plot = dj_invest_frequency,
       width = 6,
       height = 4,
       units = "in"
)

# kevin o leary
shark_tank_us |>
  summarize(
    kol_deals_made = sum(kevin_o_leary_invested == TRUE),
    kol_deals_passed = sum(kevin_o_leary_invested == FALSE),
    kol_total_pitches = n(),
    kol_pct_made = kol_deals_made / kol_total_pitches,
    kol_pct_passed = kol_deals_passed / kol_total_pitches,
  )

# how often kevin o'leary invests when someone pitches
kol_invest_frequency <- shark_tank_us |> 
  ggplot(aes(x = kevin_o_leary_invested, fill = kevin_o_leary_invested)) +
  geom_bar() +
  scale_fill_manual(name = "Deal Made?", values = frequency_colors, labels = c("No Deal", "Deal")) +
  annotate(geom = "text", x = 2, y = 145, label = "9.2%", fontface = 2) +
  annotate(geom = "text", x = 1, y = 1190, label = "90.8%", fontface = 2) +
  scale_x_discrete(labels =  c("No Deal", "Deal")) +
  coord_cartesian(ylim = c(0, 1250)) +
  theme_light() +
  labs(
    x = "",
    y = "Count",
    title = "Frequency of deals made by Kevin O'Leary on Shark Tank (US)",
    subtitle = "When pitched to, O'Leary makes deals approximately 9% of the time.",
    caption = "Source: Thirumani et al"
  )

ggsave(filename = "plots/kol_invest_frequency.png",
       plot = kol_invest_frequency,
       width = 6,
       height = 4,
       units = "in"
)


### Investor Deal Quality----


