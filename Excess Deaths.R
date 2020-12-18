library(tidyverse)
library(ggthemes)
library(data.table)
excess_deaths <- read_csv("excess_deaths.csv") %>%
  tibble(., .name_repair = "universal")
excess_deaths_avg <- 
  filter(excess_deaths, Cause.Group == "Alzheimer disease and dementia")
excess_all_wt <- 
  excess_deaths_avg %>% select(Jurisdiction, Year, Week, Number.of.Deaths, Type) %>% filter(Type == "Predicted (weighted)")
excess_all_wt <- 
  excess_all_wt %>% filter(Jurisdiction == "United States")
excess_deaths_avg <- excess_deaths_avg %>% select(Jurisdiction, Year, Week, Number.of.Deaths, Time.Period, Average.Number.of.Deaths.in.Time.Period, Type)

pct_complete <- 
  excess_deaths_avg %>% 
  mutate(percent_complete = (Number.of.Deaths/lag(Number.of.Deaths)) * 100) %>% 
  select(Jurisdiction, Year, Week, percent_complete, Type) %>% 
  filter(Year == 2020 & Type == "Unweighted")
  
excess_deaths_wt <- excess_deaths_avg %>% filter(Type == "Predicted (weighted)") %>% filter(Year >= 2019) %>% select(Jurisdiction, Week, Time.Period, Average.Number.of.Deaths.in.Time.Period)
excess_deaths_unwt <- excess_deaths_avg %>% filter(Type == "Predicted (weighted)") %>% filter(Year >= 2019) %>% select(Jurisdiction, Week, Time.Period, Average.Number.of.Deaths.in.Time.Period)
excess_deaths_wt <- excess_deaths_wt %>%
  mutate(Jurisdiction = str_replace(Jurisdiction, "New York City", "New York")) %>%
  group_by(Jurisdiction, Time.Period, Week) %>%
  summarize(Average.Number.of.Deaths.in.Time.Period = sum(Average.Number.of.Deaths.in.Time.Period)) %>%
  ungroup()

excess_deaths_analysis <- excess_deaths_wt %>%
  group_by(Jurisdiction, Week) %>%
  mutate(Diff = Average.Number.of.Deaths.in.Time.Period - lag(Average.Number.of.Deaths.in.Time.Period)) %>%
  ungroup()

total_state <- group_by(excess_deaths_analysis, Jurisdiction) %>% filter(Week <= 44 & Time.Period == "2020" & Diff > 0) %>% summarize(excess_death = sum(Diff))

nd_wt <- excess_deaths_wt %>% filter(Jurisdiction == "North Dakota")
new_york_wt <- excess_deaths_wt %>% filter(Jurisdiction == "New York")

national_wt <- excess_deaths_wt %>% filter(Jurisdiction == "United States")
national_unwt <- excess_deaths_unwt %>% filter(Jurisdiction == "United States")

colors <- c("#34D9C3","#4A0D66")
national_unwt %>% filter(Week < 47) %>% ggplot(aes(x = Week, y = Average.Number.of.Deaths.in.Time.Period, group = Time.Period, color = Time.Period)) + geom_path() +
  labs(title = "Alzheimer's and Dementia Deaths in the US", x = "Week", y ="Number", color = "") + 
  ylim(0,7000) + xlim(0,47) + scale_colour_manual(values = colors, labels = c("2015-2019 Average  ", "  2020")) + 
  theme(plot.background = element_rect(fill = "#DEDFE0"), panel.background = element_rect(fill = "#FFE8BF"), panel.grid.major = element_line(colour = "white", size = 0.2), panel.grid.minor = element_blank(), plot.title = element_text(hjust = .5, face = "bold"), legend.position="top", axis.title = element_text(face = "bold"), legend.background = element_rect(fill="#DEDFE0"),legend.key = element_rect(fill = "#DEDFE0"), legend.key.width = unit(.8,"cm"), legend.text = element_text(size = 10))
ggplot(new_york_wt, aes(x = Week, y = Average.Number.of.Deaths.in.Time.Period, group = Time.Period, color = Time.Period)) + geom_path() + labs(title = "Alzheimer's and Dementia Deaths in NY", x = "Week", y ="Number", color = "") + ylim(0,500) + xlim(0,47) + scale_colour_manual(values = colors, labels = c("2015-2019 Average  ", "  2020")) + theme(plot.background = element_rect(fill = "#DEDFE0"), panel.background = element_rect(fill = "#FFE8BF"), panel.grid.major = element_line(colour = "white", size = 0.2), panel.grid.minor = element_blank(), plot.title = element_text(hjust = .5, face = "bold"), legend.position="top", axis.title = element_text(face = "bold"), legend.background = element_rect(fill="#DEDFE0"),legend.key = element_rect(fill = "#DEDFE0"), legend.key.width = unit(.8,"cm"), legend.text = element_text(size = 10))

