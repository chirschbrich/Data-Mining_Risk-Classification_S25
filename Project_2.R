library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
library("cluster")
library("dbscan")

cases <- read_csv("COVID-19_cases_plus_census.csv")
mobility_TX <- read_csv("Global_Mobility_Report.csv") %>%
  filter(sub_region_1 == "Texas") %>%
  rename(county = sub_region_2, FIPS = census_fips_code) %>%
  mutate(county = str_to_lower(county),
         county = str_remove(county, " county$"))  # Convert to lowercase 


vacc_TX <- read_csv("COVID-19_Vaccinations_US.csv") %>%
  filter(Recip_State == "TX",
         FIPS != "UNK", #No county associated
         Completeness_pct > 0) # Remove invalid information
  
  
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

counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "texas") %>% rename(c(county = subregion))

cases_TX <- cases_TX_select %>% mutate(county = county_name %>% str_to_lower() %>% 
                                         str_replace('\\s+county\\s*$', ''))
counties_TX <- counties_TX %>% left_join(cases_TX %>% 
                                           select(c(county, cases_per_1000, deaths_per_1000, death_per_case)))
# Mobility

dim(mobility_TX)
head(mobility_TX)
summary(mobility_TX)

mobility_1 <- mobility_TX %>%
  filter(date == "2020-07-15")

mobility_2 <- mobility_TX %>%
  filter(date == "2021-01-20")


# Vaccines

vacc_TX$Date <- as.Date(vacc_TX$Date, format = "%m/%d/%Y")


# Create Statistics for analysis and summary
summary(cases_TX_select)
summary(cases_TX)
summary(vacc_TX)
summary(mobility_TX)
summary(case_tl)

mobility_TX_stat <- mobility_TX |> 
  filter(complete.cases(workplaces_percent_change_from_baseline))

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

Mode(mobility_TX_stat$work_pct)
sd(mobility_TX_stat$work_pct)
diff(range(mobility_TX_stat$work_pct))

Mode(cases_TX_select$cases_per_1000)
sd(cases_TX_select$cases_per_1000)
diff(range(cases_TX_select$cases_per_1000))

Mode(cases_TX_select$deaths_per_1000)
sd(cases_TX_select$deaths_per_1000)
diff(range(cases_TX_select$deaths_per_1000))

Mode(cases_TX_select$death_per_case)
sd(cases_TX_select$death_per_case)
diff(range(cases_TX_select$death_per_case))

###########################################################################
mobility_2020 <- mobility_TX %>%
  filter(date == as.Date("2020-07-15")) %>%
  select(county, FIPS, 
         work_pct = workplaces_percent_change_from_baseline)
  
  
features_2020 <- cases_TX %>%
  select(county, cases_per_1000, deaths_per_1000, death_per_case, total_pop,
         median_income, median_age) %>%
  left_join(mobility_2020, by = "county")

county_names2020 <- features_2020 %>% select(county, FIPS)

features_2020 <- features_2020 %>%
  select(-county)

features_2020 <- features_2020 %>%
  na.omit() %>%
  mutate(across(everything(), as.numeric))

features_scaled2020 <- scale(features_2020)


###########################################################################

# Filter vaccination data for 2021-10-22
vacc_2021 <- vacc_TX %>% 
  filter(Date == as.Date("2021-10-22")) %>%
  mutate(county = str_to_lower(county),
         county = str_remove(county, " county$"))

# Combine with case and demographic data
features_2021 <- cases_TX %>%
  select(county, cases_per_1000, deaths_per_1000, death_per_case,
         total_pop, median_income, median_age) %>%
  left_join(vacc_2021, by = "county")

# Store county names
county_names2021 <- features_2021 %>% select(county)

# Clean and scale the features
features_scaled2021 <- features_2021 %>%
  select(-county, -Date, -FIPS, -state) %>%
  na.omit() %>%
  mutate(across(everything(), as.numeric)) %>%
  scale()

#########################################################################
#########################################################################

features_for_clustering <- cases_TX %>%
  select(county, cases_per_1000, deaths_per_1000, death_per_case, total_pop,
         median_income, median_age) 
features_for_clustering2020 <- features_for_clustering %>%
  left_join(mobility_TX, by = c("county" = "county")) %>%
  filter(date == "2020-07-15")

county_names <- features_for_clustering2020 %>%
  select(county, FIPS)
  
features_for_clustering$county <- NULL
features_for_clustering$date <- NULL
features_for_clustering$FIPS <- NULL
# Drop rows with missing values
features_clean <- na.omit(features_for_clustering)
features_clean <- features_clean %>%
  mutate(across(everything(), as.numeric))

str(features_clean)

# Scale
features_scaled <- scale(features_clean)

############################################################## K MEANS
############################################################## 
############################################################## 
# Find best K for data
library(cluster)
wss <- map_dbl(1:10, function(k) {
  kmeans(features_scaled2021, centers = k, nstart = 25)$tot.withinss
})

plot(1:10, wss, type = "b",
     xlab = "Number of clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares",
     main = "Sum of Squared Errors")




#################################################################### 2020
# Perform clustering
kmeans_2020 <- kmeans(features_scaled2020, centers = 3, nstart = 25)

# Add cluster assignments
clustered_2020 <- county_names2020 %>%
  slice(1:nrow(features_scaled2020)) %>%  # Align with non-NA rows
  mutate(cluster = kmeans_2020$cluster)

# Load Texas county map
county_map <- map_data("county") %>%
  filter(region == "texas") %>%
  rename(county = subregion)

# Merge clusters with map data
map_data_clustered <- county_map %>%
  left_join(clustered_2020, by = "county")

# Plot clusters on map
ggplot(map_data_clustered, aes(long, lat, group = group, fill = factor(cluster))) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "Texas County Clusters - (K-means)",
       fill = "Cluster")

# Convert to tibble and add cluster as a column
cluster_centers <- as_tibble(kmeans_2020$centers) %>%
  mutate(cluster = factor(1:nrow(kmeans_2020$centers)))  # Add cluster ID as a factor

# Pivot longer just the feature columns
cluster_centers_long <- cluster_centers %>%
  pivot_longer(cols = -cluster, names_to = "feature", values_to = "zscore")

# Plot
ggplot(cluster_centers_long, aes(x = zscore, y = feature, fill = cluster)) +
  geom_col() +
  facet_wrap(~ cluster) +
  labs(title = "Cluster Profiles (Social Distancing)", x = "Z-score", y = "Feature") +
  guides(fill = "none")



#################################################################### 2021
# Perform clustering
kmeans_2021 <- kmeans(features_scaled2021, centers = 3, nstart = 25)

# Add cluster assignments
clustered_2021 <- county_names2021 %>%
  slice(1:nrow(features_scaled2021)) %>%  # Align with non-NA rows
  mutate(cluster = kmeans_2021$cluster)

# Load Texas county map
county_map <- map_data("county") %>%
  filter(region == "texas") %>%
  rename(county = subregion)

# Merge clusters with map data
map_data_clustered <- county_map %>%
  left_join(clustered_2021, by = "county")

# Plot clusters on map
ggplot(map_data_clustered, aes(long, lat, group = group, fill = factor(cluster))) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "Texas County Clusters - (K-means)",
       fill = "Cluster")

# Convert to tibble and add cluster as a column
cluster_centers <- as_tibble(kmeans_2021$centers) %>%
  mutate(cluster = factor(1:nrow(kmeans_2021$centers)))  # Add cluster ID as a factor

# Pivot longer just the feature columns
cluster_centers_long <- cluster_centers %>%
  pivot_longer(cols = -cluster, names_to = "feature", values_to = "zscore")

# Plot
ggplot(cluster_centers_long, aes(x = zscore, y = feature, fill = cluster)) +
  geom_col() +
  facet_wrap(~ cluster) +
  labs(title = "Cluster Profiles (Vaccines)", x = "Z-score", y = "Feature") +
  guides(fill = "none")

############################################################## HIERARCHICAL
############################################################## 
############################################################## 
#################################################################### 2020

dist_2020 <- dist(features_scaled2020, method = "euclidean")
hc_2020 <- hclust(dist_2020, method = "ward.D2")
plot(hc_2020, labels = FALSE, main = "Dendrogram - Texas Counties (2020)")
clusters_2020 <- cutree(hc_2020, k = 3)
clustered_2020 <- county_names2020 %>%
  slice(1:nrow(features_scaled2020)) %>%
  mutate(cluster = clusters_2020)
map_data_clustered <- county_map %>%
  left_join(clustered_2020, by = "county")

ggplot(map_data_clustered, aes(long, lat, group = group, fill = factor(cluster))) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "Texas County Clusters - 2020 (Hierarchical)", fill = "Cluster")


features_with_clusters_2020 <- features_scaled2020 %>%
  as_tibble() %>%
  mutate(cluster = factor(clusters_2020))

cluster_centers_2020 <- features_with_clusters_2020 %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

# Pivot for plotting
cluster_centers_long_2020 <- cluster_centers_2020 %>%
  pivot_longer(cols = -cluster, names_to = "feature", values_to = "zscore")

ggplot(cluster_centers_long_2020, aes(x = zscore, y = feature, fill = cluster)) +
  geom_col() +
  facet_wrap(~ cluster) +
  labs(title = "Hierarchical Cluster Profiles (Social Distancing)", x = "Z-score", y = "Feature") +
  guides(fill = "none")

#################################################################### 2021

dist_2021 <- dist(features_scaled2021, method = "euclidean")
hc_2021 <- hclust(dist_2021, method = "ward.D2")
plot(hc_2021, labels = FALSE, main = "Dendrogram - Texas Counties (2021)")
clusters_2021 <- cutree(hc_2021, k = 3)
clustered_2021 <- county_names2021 %>%
  slice(1:nrow(features_scaled2021)) %>%
  mutate(cluster = clusters_2021)
map_data_clustered <- county_map %>%
  left_join(clustered_2021, by = "county")

ggplot(map_data_clustered, aes(long, lat, group = group, fill = factor(cluster))) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "Texas County Clusters - 2021 (Hierarchical)", fill = "Cluster")


features_with_clusters_2021 <- features_scaled2021 %>%
  as_tibble() %>%
  mutate(cluster = factor(clusters_2021))

cluster_centers_2021 <- features_with_clusters_2021 %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

# Pivot for plotting
cluster_centers_long_2021 <- cluster_centers_2021 %>%
  pivot_longer(cols = -cluster, names_to = "feature", values_to = "zscore")

ggplot(cluster_centers_long_2021, aes(x = zscore, y = feature, fill = cluster)) +
  geom_col() +
  facet_wrap(~ cluster) +
  labs(title = "Hierarchical Cluster Profiles (Vaccines)", x = "Z-score", y = "Feature") +
  guides(fill = "none")

###########################################################################

library(ggplot2)

kmeans_2020 <- kmeans(features_scaled2020, centers = 3, nstart = 25)

# Convert feature values of clusters into a long format for 2020
cluster_features_2020 <- as.data.frame(kmeans_2020$centers)
cluster_features_2020$cluster <- factor(1:nrow(cluster_features_2020))
cluster_features_2020_long <- cluster_features_2020 %>%
  pivot_longer(cols = -cluster, names_to = "feature", values_to = "value")

# Plot heatmap for 2020
ggplot(cluster_features_2020_long, aes(x = feature, y = cluster, fill = value)) +
  geom_tile() +
  labs(title = "Heatmap of Feature Values by Cluster (Social Distancing)", x = "Feature", y = "Cluster") +
  scale_fill_gradient(low = "white", high = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

kmeans_2021 <- kmeans(features_scaled2021, centers = 3, nstart = 25)

# Convert feature values of clusters into a long format for 2021
cluster_features_2021 <- as.data.frame(kmeans_2021$centers)
cluster_features_2021$cluster <- factor(1:nrow(cluster_features_2021))
cluster_features_2021_long <- cluster_features_2021 %>%
  pivot_longer(cols = -cluster, names_to = "feature", values_to = "value")

# Plot heatmap for 2021
ggplot(cluster_features_2021_long, aes(x = feature, y = cluster, fill = value)) +
  geom_tile() +
  labs(title = "Heatmap of Feature Values by Cluster (Vaccines)", x = "Feature", y = "Cluster") +
  scale_fill_gradient(low = "white", high = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Unsupervised Evaluation

# silhouette score
silhouette_score <- silhouette(kmeans_2020$cluster, dist(features_scaled2020))
# Plot silhouette score
plot(silhouette_score, main = "Silhouette Plot for Clusters")

# silhouette score
silhouette_score <- silhouette(kmeans_2021$cluster, dist(features_scaled2021))
# Plot silhouette score
plot(silhouette_score, main = "Silhouette Plot for Clusters")

## Supervised Evaluation
cases_TX_select <- cases_TX_select %>%
  mutate(county = str_to_lower(county_name),
         county = str_remove(county, " county$"))

# Create ground truth for high vs low case rates
cases_threshold <- median(cases_TX_select$cases_per_1000)  # Define a threshold for "high" vs "low"
cases_TX_select <- cases_TX_select %>%
  mutate(case_rate_group = if_else(cases_per_1000 > cases_threshold, "High", "Low"))

# Compare clusters with ground truth
clustered_2020 <- clustered_2020 %>%
  left_join(cases_TX_select %>% select(county, case_rate_group), by = "county")

# Plot the comparison
ggplot(clustered_2020, aes(x = factor(cluster), fill = case_rate_group)) +
  geom_bar(position = "fill") +
  labs(title = "Cluster Distribution by COVID-19 Case Rates",
       x = "Cluster", y = "Proportion", fill = "Case Rate Group")


# Create ground truth for high vs low case rates
cases_threshold <- median(cases_TX_select$cases_per_1000)  # Define a threshold for "high" vs "low"
cases_TX_select <- cases_TX_select %>%
  mutate(case_rate_group = if_else(cases_per_1000 > cases_threshold, "High", "Low"))

# Compare clusters with ground truth
clustered_2021 <- clustered_2021 %>%
  left_join(cases_TX_select %>% select(county, case_rate_group), by = "county")

# Plot the comparison
ggplot(clustered_2021, aes(x = factor(cluster), fill = case_rate_group)) +
  geom_bar(position = "fill") +
  labs(title = "Cluster Distribution by COVID-19 Case Rates",
       x = "Cluster", y = "Proportion", fill = "Case Rate Group")

