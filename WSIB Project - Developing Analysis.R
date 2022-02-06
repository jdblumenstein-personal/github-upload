library(readr)
wsib_fund_activity <- read_csv("Downloads/Combined_WSIB_cleaned_filtered_sheets.pdf - Sheet1.csv", 
                               col_types = cols(report_date = col_date(format = "%m/%d/%Y"), 
                                                initial_investment_date = col_date(format = "%m/%d/%Y"), 
                                                strategy = col_factor(levels = c("Corporate Finance/Buyout - Mid", 
                                                                                 "Growth Equity", "Corporate Finance/Buyout - Mega", 
                                                                                 "Distressed Debt", "Corporate Finance/Buyout - Small", 
                                                                                 "Corporate Finance/Buyout - Large", 
                                                                                 "Venture Capital", "Special Situations", 
                                                                                 "Special Situation", "Co-Investment", 
                                                                                 "Mezzanine"))))
str(wsib_fund_activity)
library(dplyr)
library(lubridate)
library(tidyr)
##I'll start by normalizing the reporting and seeking out any additional issues where data should be removed or fixed
##I want to introduce an artificial time 0 for all ids, but first I need to make sure I pull out all artifical time 0s in the set
uniform_starting_point <- wsib_fund_activity %>%
  filter(contributions != 0)

funds_to_examine_clipped_beginning <- uniform_starting_point %>%
  group_by(assigned_unid) %>%
  mutate(initial_investment_year = year(initial_investment_date), initial_investment_month = month(initial_investment_date),
         report_year = year(report_date), report_month = month(report_date)) %>%
  filter(report_date == min(as.integer(report_date))) %>%
  mutate(matching_year = initial_investment_year == report_year, mathcing_month = initial_investment_month == report_month) %>%
  filter(matching_year == FALSE) %>%
  select(assigned_unid)

ids_to_examine <- funds_to_examine_clipped_beginning$assigned_unid

funds_to_examine_clipped_ending <- uniform_starting_point %>%
  group_by(assigned_unid) %>%
  mutate(report_year = year(report_date), report_month = month(report_date)) %>%
  filter(report_date == max(as.integer(report_date))) %>%
  filter(report_year != 2021)

more_ids_to_examine <- funds_to_examine_clipped_ending$assigned_unid

ids_to_examine_all <- c(ids_to_examine, more_ids_to_examine)

funds_to_examine <- wsib_fund_activity %>%
  filter(assigned_unid %in% ids_to_examine_all)

##Need to annotate further but this analysis to understand that the dating is a bit off for "TPG Growth III / f0hx32ntdvxi8ezsuj1x"
##I took the additional step of visually confirming via the pdf reports. I've also discovered that there are two other funds that 
##WSIB adjusted the name and I need to collapse the assigned IDs. "113dq3betpg2l4hhdnzf" will collapse into "8uttvqc4bm5byh2zydeu"
## and I'll collapse "g536990rpxbvyyfqoucu" into "ue4atzld4kkmqb9zh9w9"

library(stringr)
uniform_starting_point$assigned_unid <- str_replace(uniform_starting_point$assigned_unid, "113dq3betpg2l4hhdnzf", "8uttvqc4bm5byh2zydeu")
uniform_starting_point$assigned_unid <- str_replace(uniform_starting_point$assigned_unid, "g536990rpxbvyyfqoucu", "ue4atzld4kkmqb9zh9w9")

##It appears that we have complete fund histories as the starting point matches the signalled investment date and the fund histories
##end at the last report date. Now is the time to add an artifical time 0 so that I can capture all quarterly interval sums correctly
##i'll do so by creating a series count that starts at 1 for each fund and counts that number of reporting quarters

uniform_starting_point_series <- uniform_starting_point %>%
  group_by(assigned_unid) %>%
  mutate(series_count = 1:n())

uniform_starting_point_series_time_zero <- uniform_starting_point_series %>%
  filter(series_count == 1) %>%
  mutate(series_count = 0, unfunded_commitment = 0, contributions = 0, distributions = 0, 
         current_market_value = 0, total_value = 0, net_benefit = 0)

uniform_starting_point_artificial_zero <- rbind(uniform_starting_point_series, uniform_starting_point_series_time_zero)

##Going to introduce new rows that we are going to leverage. I'm first checking and replacing any leftover nas in the set
##with the values we are going to be calculating. Then I'm going to calculate out the quaterly intervals between time periods for
##contributions and distributions and lastly I filter the artificial zero counts right back out of the set when completed

uniform_starting_point_artificial_zero %>%
  filter(is.na(distributions) | is.na(contributions)) %>%
  select(report_date, assigned_unid, contributions, distributions)

uniform_starting_point_artificial_zero$distributions <- replace_na(uniform_starting_point_artificial_zero$distributions, 0)

wsib_fund_activity_quarterly <- uniform_starting_point_artificial_zero %>%
  arrange(assigned_unid, series_count) %>%
  group_by(assigned_unid) %>%
  mutate(quarterly_contributions = contributions - lag(contributions), 
         quarterly_distributions = distributions - lag(distributions),
         quarterly_net_cashflow = quarterly_distributions - quarterly_contributions) %>%
  filter(series_count != 0)

##Now it is time to introduce our benchmark and the corresponding data that I'm pulling from FRED - I have added a column to the data
##the column simply aligns the index_date to a normalized end of quarter date that aligns with the report_dates in my WSIB data

wsib_fund_activity_quarterly_sp500 <- merge(wsib_fund_activity_quarterly, sp500, by.x = "report_date", 
                                            by.y = "normalized_eoq_date", all.x = TRUE)

##I need to get the latest sp500 value that matches my latest report_date for WSIB to create a growth ratio over time for the sp500

sp500_current <- sp500[[37,2]]

##Now I'm going to create that growth ratio and use it as a way to convert the quarterly contributions and distributions to model
##the concept of purchasing and selling sp500 shares at the same time these same actions are happening at the fund level. 

wsib_fund_activity_quarterly_sp500_value <- wsib_fund_activity_quarterly_sp500 %>%
  mutate(sp500_growth_ratio = sp500_current / sp500_value, 
         sp500_quarterly_contributions = sp500_growth_ratio * quarterly_contributions,
         sp500_quarterly_distributions = sp500_growth_ratio * quarterly_distributions,
         sp500_quarterly_net_cashflow = sp500_quarterly_distributions - sp500_quarterly_contributions)

##I can run a KS PME calculation now as I have all the right attributes to be able to run the calculation per fund and we can
##start to summarize and start to visualize the data and start to create some takeaways

ks_pme_results <- wsib_fund_activity_quarterly_sp500_value %>%
  group_by(assigned_unid) %>%
  mutate(sp500_cumulative_contributions = cumsum(sp500_quarterly_contributions),
         sp500_cumulative_distributions = cumsum(sp500_quarterly_distributions)) %>%
  filter(report_date == "2021-06-30") %>%
  summarize(sp500_ks_pme = (sp500_cumulative_distributions + current_market_value) / sp500_cumulative_contributions)

##KS PME works well to create natural groupings as the calculation revolves around the value of 1. Above 1 means outperformance
##of the underlying fund as compared to the sp500, conversely, below 1 implies that the fund underperformed compared to the 
##sp500 and you would have generated greater value committing those funds to the index over that same timeframe. Also, I'm goign
##to add back some of the static fund attributes to include as part of the analysis. I'm creating a new attribute which is called
##vintage. It's a common way to create peers as you group funds based on the intial year investments began. 
unique(wsib_fund_activity$strategy)
buyout <- c("Corporate Finance/Buyout - Mid", "Corporate Finance/Buyout - Small", "Corporate Finance/Buyout - Mega", "Corporate Finance/Buyout - Large")
venture <- c("Venture Capita", "Growth Equity")
other_diverse <- c("Distressed Debt", "Special Situations", "Special Situation", "Co-Investment", "Mezzanine")

wsib_fund_vintage <- wsib_fund_activity %>%
  group_by(assigned_unid) %>%
  filter(report_date == min(as.integer(report_date))) %>%
  transmute(assigned_unid, vintage = year(report_date))

wsib_fund_activity_added_attributes <- wsib_fund_activity %>%
  merge(wsib_fund_vintage, by = "assigned_unid") %>%
  select(report_date, assigned_unid, fund_name, strategy, vintage, tvpi, irr, commitment, contributions, distributions, current_market_value) %>%
  group_by(assigned_unid) %>%
  filter(report_date == "2021-06-30") %>%
  transmute(assigned_unid, fund_name, strategy, vintage, tvpi, irr, current_percent_called = contributions / commitment, 
            current_dpi = distributions / contributions, current_rvpi = current_market_value / contributions, 
            current_tvpi_calculated = (distributions + current_market_value) / contributions)

key_metrics_results_analysis <- merge(wsib_fund_activity_added_attributes, ks_pme_results, by = "assigned_unid") %>% 
  transmute(assigned_unid, fund_name, strategy, grouped_strategy = ifelse(strategy %in% buyout, "buyout", 
                                                                          ifelse(strategy %in% venture, "venture","other_diversification")),
            vintage, tvpi, irr, current_percent_called, current_dpi, current_rvpi, 
            current_tvpi_calculated, sp500_ks_pme, outperformance = sp500_ks_pme > 1)

##Alright things are pretty well set up to run some analysis and compare WSIB's private portfolio against a common public index
##at times I make use of funds in their first couple of years, but largely we want to exclude them from most of our analysis. 
##They should be treated as a curiosity as these large investments are designed to be long term vehicles (traditionally 10 to 12 years)
##and there are a lot of fees loaded upfront that acts as a drag on the investment until the pitched value add kicks in during later
##maturing years of the investment.There aren't any clear cut rules, but generally you wouldn't start to evaluate a fund's performance
##until it has had three years of investment history. I'll be a little more generous to WSIB and exclude 2018 and later vintages.

library(ggplot2)
key_metrics_results_analysis_mature <- key_metrics_results_analysis %>%
  filter(vintage < 2018)

ks_pme_results_analysis_mature %>%
  count(outperformance) %>%
  mutate(pct_of_total = n / sum(n))

key_metrics_results_analysis_mature %>%
  group_by(grouped_strategy) %>%
  summarize(mean_pme = mean(sp500_ks_pme), sd_pme = sd(sp500_ks_pme), median_pme = median(sp500_ks_pme), 
            mean_tvpi = mean(current_tvpi_calculated), sd_tvpi = sd(current_tvpi_calculated), median_tvpi = median(current_tvpi_calculated))


ggplot(key_metrics_results_analysis, aes(y = strategy, x = tvpi)) +
  geom_point() +
  facet_grid(rows = vars(as.factor(vintage)), cols = vars(grouped_strategy))


ggplot(ks_pme_results_analysis_mature, aes(y = strategy, x = sp500_ks_pme)) +
  geom_boxplot()

ks_pme_results_analysis_mature %>%
  count(vintage)

ggplot(key_metrics_results_analysis_mature, aes(vintage, current_tvpi_calculated, color = grouped_strategy)) +
    geom_point(position = "jitter")