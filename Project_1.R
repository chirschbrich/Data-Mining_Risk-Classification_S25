library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
library("cluster")
library("dbscan")

cases <- read_csv("COVID-19_cases_plus_census.csv")
mobility_TX <- read_csv("Global_Mobility_Report.csv")

epide_TX <- read_csv("epidemiology.csv") %>%
  filter(location_key == "US_TX")
hosp <- read_csv("hospitalizations.csv")

vacc_TX <- read_csv("COVID-19_Vaccinations_US.csv") %>%
  filter(Recip_State == "TX",
         FIPS != "UNK", #No county associated
         Completeness_pct > 0) |> # Remove invalid information
  select(Date, 
         FIPS, 
         county = Recip_County, 
         state = Recip_State, 
         First_Dose_Pct = Administered_Dose1_Pop_Pct, 
         Full_Dose_Pct = Series_Complete_Pop_Pct, 
         Full_Dose_Pct65plus = Series_Complete_65PlusPop_Pct) |>
  mutate(county = str_to_lower(county),
         county = str_remove(county, " county$"))  # Convert to lowercase 

case_tl <- read.csv("TX_cases_to-may23.csv")

str(cases)

cases <- cases |> mutate_if(is.character, factor)
dim(cases)

str(cases)

cases_TX <- cases %>% filter(state == "TX")
dim(cases_TX)
summary(cases_TX[,1:10])

cases_TX_select <- cases_TX %>% filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, 
         confirmed_cases, 
         deaths, 
         total_pop, 
         median_income,
         median_age)
cases_TX_select <- cases_TX_select %>% mutate(
   cases_per_1000 = confirmed_cases/total_pop*1000, 
  deaths_per_1000 = deaths/total_pop*1000, 
  death_per_case = deaths/confirmed_cases)

head(cases_TX_select)

ggplot(cases_TX_select, mapping = aes(x = cases_per_1000, y = deaths_per_1000, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_TX_select, deaths_per_1000 > quantile(deaths_per_1000, .95))) +
  labs(x = "confirmed cases", y = "deaths per 1000", size = "total population")

cor_TX <- cor(cases_TX_select[,-1])
ggcorrplot(cor_TX, p.mat = cor_pmat(cases_TX_select[,-1]), insig = "blank", hc.order = TRUE)       


counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "texas") %>% rename(c(county = subregion))

cases_TX <- cases_TX_select %>% mutate(county = county_name %>% str_to_lower() %>% 
                                         str_replace('\\s+county\\s*$', ''))
counties_TX <- counties_TX %>% left_join(cases_TX %>% 
                                           select(c(county, cases_per_1000, deaths_per_1000, death_per_case)))

ggplot(counties_TX, aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = deaths_per_1000)) +
  # geom_text_repel(data = counties_TX %>% filter(complete.cases(.)) %>% group_by(county) %>% 
  #    summarize(long = mean(long), lat = mean(lat)) %>% mutate(county = str_to_title(county))) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "COVID-19 Cases per 1000 People", 
       subtitle = "Only counties reporting 100+ cases",
       fill = "cases per 1000")

case_tl$date <- as.Date(case_tl$date, format = "%m/%d/%Y")
case_tl <- case_tl %>%
  mutate(county = str_to_lower(county),
         county = str_remove(county, " county$"))  %>%
  filter(county != "unallocated texas")


# time stamps

cases_TX_3 <- case_tl %>%
  filter(date == "2021-10-20")



# Mobility

mobility_TX <- mobility_TX %>%
  select(FIPS = census_fips_code,
         date,
         county = sub_region_2,
         retail_and_rec_pct = retail_and_recreation_percent_change_from_baseline,
         work_pct = workplaces_percent_change_from_baseline) |>
  mutate(county = str_to_lower(county),
         county = str_remove(county, " county$")) %>% # Convert to lowercase 
  filter(!is.na(FIPS)) 

dim(mobility_TX)
head(mobility_TX)
summary(mobility_TX)


# Vaccines

vacc_TX$Date <- as.Date(vacc_TX$Date, format = "%m/%d/%Y")

# Look at geographic data when vaccines first given out
vacc_TX_3 <- vacc_TX |>
  filter(Date == "2021-10-22")

summary(vacc_TX_3)

vacc_TX_4 <- vacc_TX |>
  filter(Date == "2023-05-10")

counties_TX <- counties_TX %>% 
  left_join(vacc_TX_3, by = c("county" = "county")) 

counties_TX <- counties_TX %>% 
  left_join(vacc_TX_4, by = c("county" = "county")) 

# Plot the counties based on vaccine % after they were first released
ggplot(counties_TX, aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = Full_Dose_Pct.x)) +
  # geom_text_repel(data = counties_TX %>% filter(complete.cases(.)) %>% group_by(county) %>% 
  #    summarize(long = mean(long), lat = mean(lat)) %>% mutate(county = str_to_title(county))) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Vaccination Percentage per County", 
       subtitle = "First day of booster vaccination",
       fill = "Vaccination %")

ggplot(counties_TX, aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = Full_Dose_Pct.y)) +
  # geom_text_repel(data = counties_TX %>% filter(complete.cases(.)) %>% group_by(county) %>% 
  #    summarize(long = mean(long), lat = mean(lat)) %>% mutate(county = str_to_title(county))) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Vaccination Percentage per County", 
       subtitle = "Last recorded day of vaccination",
       fill = "Vaccination %")






# Epidemiology data for TX, I use this because it shows the statewide 
# recovery that other tables do not, it also shows the effectiveness
# of combative measures against COVID-19 as a whole

# Clean variable names and select important data

epide_TX_clean <- epide_TX |>
  select(date, 
         location_key, 
         cumulative_confirmed, 
         cumulative_deceased, 
         new_confirmed_TX = new_confirmed,
         new_deceased_TX = new_deceased,
         new_recovered_TX = new_recovered)

epide_TX_week <- epide_TX_clean |> 
  mutate(week = floor_date(date, unit = "week")) |> 
  group_by(week) |> 
  summarise(
    weekly_new_confirmed = sum(new_confirmed_TX, na.rm = TRUE),
    weekly_new_deceased = sum(new_deceased_TX, na.rm = TRUE),
    weekly_new_recovered = sum(new_recovered_TX, na.rm = TRUE)
  )

#clean negative values out, most likely a reporting error
epide_TX_clean <- epide_TX_clean %>%
  mutate(new_recovered_TX = ifelse(new_recovered_TX < 0, 0, new_recovered_TX),
         new_confirmed_TX = ifelse(new_confirmed_TX < 0, 0, new_confirmed_TX))

# Plot the data
ggplot(epide_TX_week, aes(x = week, y = weekly_new_confirmed)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Covid-19 New Cases Over Time in TX (Weekly Aggregated)",
       x = "Date",
       y = "New Confirmed Cases",
       color = "Legend") +
       scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  # Add a vertical line at 2021-01-20
  geom_vline(xintercept = as.Date("2020-07-15"), linetype = "dashed", color = "orange", size = 1) +
  geom_vline(xintercept = as.Date("2021-01-20"), linetype = "dashed", color = "orange", size = 1) +
  geom_vline(xintercept = as.Date("2021-10-22"), linetype = "dashed", color = "orange", size = 1)

ggplot(epide_TX_week, aes(x = week, y = weekly_new_deceased)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Covid-19 New Deaths Over Time in TX (Weekly Aggregated)",
       x = "Date",
       y = "New Confirmed Deaths",
       color = "Legend") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  # Add a vertical line at 2021-01-20
  geom_vline(xintercept = as.Date("2020-07-15"), linetype = "dashed", color = "darkgreen", size = 1) +
  geom_vline(xintercept = as.Date("2021-01-20"), linetype = "dashed", color = "darkgreen", size = 1) +
  geom_vline(xintercept = as.Date("2021-10-22"), linetype = "dashed", color = "darkgreen", size = 1)



# Hospitalizations, 

# Clean data and select important variables to explore
hosp_TX <- hosp |>
  select(date, 
         location_key, 
         current_hospitalized_patients, 
         current_intensive_care_patients) |>
  filter(location_key == "US_TX")

# Take aggregate and turn into weekly data, this makes the visualizations more
# readable without losing data
hosp_TX_week <- hosp_TX |>
  mutate(week = floor_date(date, unit = "week")) |> 
  group_by(week) |> 
  summarise(
    weekly_hosp_patients = sum(current_hospitalized_patients, na.rm = TRUE),
    weekly_icu_patients = sum(current_intensive_care_patients, na.rm = TRUE)
  )

# Plot data 
ggplot(hosp_TX_week, aes(x = week)) +
  geom_line(aes(y = weekly_hosp_patients, color = "Hospitalized Patients"), size = 1) +
  geom_line(aes(y = weekly_icu_patients, color = "ICU Patients"), size = 1) +
  labs(title = "Covid-19 Hospitalizations and ICU Over Time in TX",
       x = "Date",
       y = "Count",
       color = "Legend") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("Hospitalized Patients" = "blue", "ICU Patients" = "red")) +
  theme_minimal()




mobility_TX_filtered <- mobility_TX %>% filter(date == as.Date("2021-01-22"))

cor_data <- mobility_TX_filtered %>%
  left_join(vacc_TX_3, by = "county") %>% # GOOD
  select(date,
         county,
         work_pct, 
         retail_and_rec_pct, 
         First_Dose_Pct,
         Full_Dose_Pct,
         Full_Dose_Pct65plus)

cor_data <- cor_data %>%
  left_join(cases_TX_3, by = "county") %>%
  select(
    work_pct, 
    retail_and_rec_pct, 
    First_Dose_Pct,
    New.cases,
    New.deaths,
    Full_Dose_Pct,
    Full_Dose_Pct65plus
  )

cor_data <- na.omit(cor_data)

# Compute the correlation matrix
cor_matrix <- cor(cor_data, use = "complete.obs")

ggcorrplot(cor_matrix, 
           lab = TRUE,          # Show correlation values
           title = "Correlation Heatmap of COVID-19 Metrics",
           colors = c("blue", "white", "red"))  # Color scale




mobility_weekly <- mobility_TX %>%
  mutate(week = floor_date(date, unit = "week")) %>%  # Aggregate by week
  group_by(week) %>%
  summarise(
    avg_retail_and_rec_pct = mean(retail_and_rec_pct, na.rm = TRUE),
    avg_work_pct = mean(work_pct, na.rm = TRUE)
  ) %>%
  ungroup()

# Convert to long format for ggplot2
mobility_weekly_long <- mobility_weekly %>%
  pivot_longer(cols = c(avg_retail_and_rec_pct, avg_work_pct),
               names_to = "Mobility_Type", values_to = "Average_Percentage_Change")

ggplot(mobility_weekly_long, aes(x = week, y = Average_Percentage_Change, fill = Mobility_Type)) +
  geom_bar(stat = "identity", position = "dodge") +  # Grouped bars
  labs(title = "Weekly Mobility Trends in Texas",
       x = "Week",
       y = "Average % Change from Baseline",
       fill = "Mobility Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# Create Statistics for analysis and summary
summary(cases_TX_select)
summary(hosp_TX_time)
summary(cases_TX)
summary(epide_TX_clean)
summary(vacc_TX)
summary(mobility_TX)
summary(case_tl)


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode(cases_TX_select$total_pop)
sd(cases_TX_select$total_pop)
diff(range(cases_TX_select$total_pop))

Mode(cases_TX_select$median_income)
sd(cases_TX_select$median_income)
diff(range(cases_TX_select$median_income))

Mode(cases_TX_select$median_age)
sd(cases_TX_select$median_age)
diff(range(cases_TX_select$median_age))

Mode(case_tl$New.cases)
sd(case_tl$New.cases)
diff(range(case_tl$New.cases))

Mode(case_tl$New.deaths)
sd(case_tl$New.deaths)
diff(range(case_tl$New.deaths))

Mode(epide_TX_clean$new_confirmed_TX)
sd(epide_TX_clean$new_confirmed_TX)
diff(range(epide_TX_clean$new_confirmed_TX))

Mode(epide_TX_clean$new_deceased_TX)
sd(epide_TX_clean$new_deceased_TX)
diff(range(epide_TX_clean$new_deceased_TX))

epide_TX_stat <- epide_TX_clean |> 
  filter(complete.cases(new_recovered_TX))

Mode(epide_TX_stat$new_recovered_TX)
sd(epide_TX_stat$new_recovered_TX)
diff(range(epide_TX_stat$new_recovered_TX))

Mode(vacc_TX$First_Dose_Pct)
sd(vacc_TX$First_Dose_Pct)
diff(range(vacc_TX$First_Dose_Pct))

Mode(vacc_TX$First_Dose_Pct)
sd(vacc_TX$First_Dose_Pct)
diff(range(vacc_TX$First_Dose_Pct))

Mode(vacc_TX$Full_Dose_Pct)
sd(vacc_TX$Full_Dose_Pct)
diff(range(vacc_TX$Full_Dose_Pct))

Mode(vacc_TX$Full_Dose_Pct65plus)
sd(vacc_TX$Full_Dose_Pct65plus)
diff(range(vacc_TX$Full_Dose_Pct65plus))

mobility_TX_stat <- mobility_TX |> 
  filter(complete.cases(work_pct)) |>
  filter(complete.cases(retail_and_rec_pct))

Mode(mobility_TX_stat$work_pct)
sd(mobility_TX_stat$work_pct)
diff(range(mobility_TX_stat$work_pct))

Mode(mobility_TX_stat$retail_and_rec_pct)
sd(mobility_TX_stat$retail_and_rec_pct)
diff(range(mobility_TX_stat$retail_and_rec_pct))

hosp_TX_stat <- hosp_TX |> 
  filter(complete.cases(current_hospitalized_patients)) |>
  filter(complete.cases(current_intensive_care_patients))

Mode(hosp_TX_stat$current_hospitalized_patients)
sd(hosp_TX_stat$current_hospitalized_patients)
diff(range(hosp_TX_stat$current_hospitalized_patients))

Mode(hosp_TX_stat$current_intensive_care_patients)
sd(hosp_TX_stat$current_intensive_care_patients)
diff(range(hosp_TX_stat$current_intensive_care_patients))

