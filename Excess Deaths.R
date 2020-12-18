library(tidyverse)
library(ggthemes)
library(data.table)
excess_deaths <- read_csv("excess_deaths.csv") %>%
  tibble(., .name_repair = "universal")

# Include just Alzheimer's and Dementia deaths
excess_deaths_avg <- filter(excess_deaths, Cause.Group == "Alzheimer disease and dementia")
# Remove unnecessary columns
excess_deaths_avg <- excess_deaths_avg %>% 
  select(Jurisdiction, Year, Week, Number.of.Deaths, Time.Period, Average.Number.of.Deaths.in.Time.Period, Type)
# Remove all excess years, so that there are observations for 2020 and 2015-2019 only.
# Also, create new column Complete which tells what percent complete the unweighted
# estimates are. Weighted estimates are listed as 100% complete.
excess_deaths_all <- excess_deaths_avg %>% 
  mutate(Complete = ifelse(Type == "Unweighted", 100*Number.of.Deaths/lag(Number.of.Deaths), 100)) %>%
  filter(Type == "Unweighted" & Complete > 90) %>%
  select(Jurisdiction, Year, Week, Number.of.Deaths, Complete) %>%
  mutate(Jurisdiction = str_replace(Jurisdiction, "New York City", "New York")) %>%
  group_by(Jurisdiction, Year, Week) %>%
  summarize(Number.of.Deaths = sum(Number.of.Deaths)) %>%
  ungroup() %>%
  filter(Jurisdiction == "United States")
  
  
excess_deaths_avg <- excess_deaths_avg %>%
  filter(Year >= 2019) %>%
  select(Jurisdiction, Week, Time.Period, Average.Number.of.Deaths.in.Time.Period, Type) %>%
  mutate(Complete = ifelse(Type == "Unweighted", 100*Average.Number.of.Deaths.in.Time.Period/lag(Average.Number.of.Deaths.in.Time.Period), 100))


# Create seperate data sets with weighted and unweighted data
excess_deaths_wt <- excess_deaths_avg %>% filter(Type == "Predicted (weighted)")
excess_deaths_unwt <- excess_deaths_avg %>% filter(Type == "Unweighted")

# Combine New York city and New York into one observation, "New York."
excess_deaths_wt <- excess_deaths_wt %>%
  mutate(Jurisdiction = str_replace(Jurisdiction, "New York City", "New York")) %>%
  group_by(Jurisdiction, Time.Period, Week) %>%
  summarize(Average.Number.of.Deaths.in.Time.Period = sum(Average.Number.of.Deaths.in.Time.Period)) %>%
  ungroup()

# Create variable that lists difference between 2015-2019 in the 2020 observation
# column for weighted deaths.
excess_deaths_analysis <- excess_deaths_wt %>%
  mutate(Diff = ifelse(Time.Period == "2020", Average.Number.of.Deaths.in.Time.Period - lag(Average.Number.of.Deaths.in.Time.Period), NA))

total_state <- group_by(excess_deaths_analysis, Jurisdiction) %>%
  filter(Week <= 44 & Time.Period == "2020" & Diff > 0) %>%
  summarize(excess_death = sum(Diff)) %>%
  ungroup()

nd_wt <- excess_deaths_wt %>% filter(Jurisdiction == "North Dakota")
new_york_wt <- excess_deaths_wt %>% filter(Jurisdiction == "New York")
national_wt <- excess_deaths_wt %>% filter(Jurisdiction == "United States" & Type == "Predicted (weighted)")
national_unwt <- excess_deaths_unwt %>% 
  filter(Jurisdiction == "United States" & Complete > 90 & Week <=47)


# Set up month as the X-axis (while still plotting weeks)
month <- seq(as.Date("2020-01-01"), 
             as.Date("2020-12-01"), 
             by = "1 month")

month_numeric <- lubridate::yday(month) / 365 * 52 + 1
month_label <- lubridate::month(month, label = TRUE)

colors <- c("#34D9C3","#4A0D66")

# Plot of unweighted 2020 vs. 2015-2019 Average
ggplot(national_unwt, aes(x = Week, 
                          y = Average.Number.of.Deaths.in.Time.Period, 
                          group = Time.Period, color = Time.Period)) + 
  scale_x_continuous(breaks = month_numeric, labels = month_label) + 
  geom_path() + 
  labs(title = "Alzheimer's and Dementia Deaths in the US", x = "Month", y ="Number", color = "") + 
  ylim(4000,7000) + 
  scale_colour_manual(values = colors, labels = c("2015-2019 Average  ", "  2020")) +
  theme(plot.background = element_rect(fill = "#DEDFE0"), 
        panel.background = element_rect(fill = "#FFE8BF"), 
        panel.grid.major = element_line(colour = "white", size = 0.2), 
        panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = .5, face = "bold"), 
        legend.position="top", axis.title = element_text(face = "bold"), 
        legend.background = element_rect(fill="#DEDFE0"), 
        legend.key = element_rect(fill = "#DEDFE0"), 
        legend.key.width = unit(.8,"cm"), 
        legend.text = element_text(size = 10))

# Plot of all years vs. each other (haven't figured out trendline)
ggplot(excess_deaths_all, aes(x = Week, 
                          y = Number.of.Deaths, 
                          group = Year, color = Year)) + 
  scale_x_continuous(breaks = month_numeric, labels = month_label) + 
  geom_path() + 
  labs(title = "Alzheimer's and Dementia Deaths in the US", x = "Month", y ="Number", color = "") + 
  ylim(4000,7000) +
  theme(plot.background = element_rect(fill = "#DEDFE0"), 
        panel.background = element_rect(fill = "#FFE8BF"), 
        panel.grid.major = element_line(colour = "white", size = 0.2), 
        panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = .5, face = "bold"), 
        legend.position="top", axis.title = element_text(face = "bold"), 
        legend.background = element_rect(fill="#DEDFE0"), 
        legend.key = element_rect(fill = "#DEDFE0"), 
        legend.key.width = unit(.8,"cm"), 
        legend.text = element_text(size = 10))

ggplot(new_york_wt, aes(x = Week, y = Average.Number.of.Deaths.in.Time.Period, group = Time.Period, color = Time.Period)) + geom_path() + labs(title = "Alzheimer's and Dementia Deaths in NY", x = "Week", y ="Number", color = "") + ylim(0,500) + xlim(0,47) + scale_colour_manual(values = colors, labels = c("2015-2019 Average  ", "  2020")) + theme(plot.background = element_rect(fill = "#DEDFE0"), panel.background = element_rect(fill = "#FFE8BF"), panel.grid.major = element_line(colour = "white", size = 0.2), panel.grid.minor = element_blank(), plot.title = element_text(hjust = .5, face = "bold"), legend.position="top", axis.title = element_text(face = "bold"), legend.background = element_rect(fill="#DEDFE0"),legend.key = element_rect(fill = "#DEDFE0"), legend.key.width = unit(.8,"cm"), legend.text = element_text(size = 10))



