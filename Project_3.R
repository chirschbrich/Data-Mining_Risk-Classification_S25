# Chris Hirschbrich, 48767607

# Load required libraries
library(tidyverse)
library(caret)
library(e1071)          # For Naive Bayes
library(class)          # For k-NN
library(neuralnet)      # For ANN
library(randomForest)   # For feature importance
library(pROC)           # For ROC curves
library(gridExtra)      # For arranging multiple plots
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# ----------------------------
# Prepare data
# ----------------------------
cases <- read_csv("COVID-19_cases_plus_census.csv") %>%
  mutate_if(is.character, factor) %>%
  filter(confirmed_cases > 0) %>%
  filter(state == "TX") %>%
  mutate(
    cases_per_10000 = confirmed_cases / total_pop * 10000,
    deaths_per_10000 = deaths / total_pop * 10000,
    death_per_case = deaths / confirmed_cases
  )

# Select and normalize relevant features
cases_sel <- cases %>%
  dplyr::select(
    county_name, state, total_pop,
    nonfamily_households, median_year_structure_built, median_age, 
    commuters_by_public_transportation, households, median_income, 
    housing_units, vacant_housing_units,
    percent_income_spent_on_rent, employed_pop, unemployed_pop,
    in_school, in_undergrad_college, death_per_case, deaths_per_10000, cases_per_10000
  ) %>%
  mutate(across(c(nonfamily_households, commuters_by_public_transportation,
                  households, housing_units, vacant_housing_units, employed_pop,
                  unemployed_pop, in_school, in_undergrad_college), ~ . / total_pop)) %>%
  filter(state == "TX", county_name != "NA")

# Prepare county name column for merging
cases_sel <- cases_sel %>%
  mutate(county = str_to_lower(county_name),
         county = str_remove(county, " county$"))

# ----------------------------
# Process vaccination data
# ----------------------------
vacc <- read_csv("COVID-19_Vaccinations_US.csv") %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  filter(Completeness_pct > 0,
         Recip_State == "TX") %>%
  mutate(county = str_to_lower(Recip_County),
         county = str_remove(county, " county$")) %>%
  dplyr::select(date = Date, county, state = Recip_State,
                First_Dose_Pct = Administered_Dose1_Pop_Pct,
                Full_Dose_Pct = Series_Complete_Pop_Pct,
                Full_Dose_Pct65plus = Series_Complete_65PlusPop_Pct) %>%
  filter(county != "NA", county != "unknown") %>%
  na.omit()

# Merge vaccination data across dates
vacc_summary <- vacc %>%
  group_by(county, state) %>%
  summarise(
    avg_First_Dose_Pct = mean(First_Dose_Pct, na.rm = TRUE),
    avg_Full_Dose_Pct = mean(Full_Dose_Pct, na.rm = TRUE),
    avg_Full_Dose_65plus = mean(Full_Dose_Pct65plus, na.rm = TRUE),
    .groups = "drop"
  )

# ----------------------------
# Process mobility data
# ----------------------------
mobility <- read_csv("Global_Mobility_Report.csv") %>%
  mutate(date = as.Date(date)) %>%
  filter(sub_region_1 == "Texas") %>%
  mutate(county = str_to_lower(sub_region_2),
         county = str_remove(county, " county$"),
         state = "TX") %>%
  dplyr::select(date, county, state,
                work_pct = workplaces_percent_change_from_baseline,
                retail_and_rec_pct = retail_and_recreation_percent_change_from_baseline) %>%
  filter(county != "NA", county != "unknown") %>%
  na.omit()

# Merge mobility data across dates
mobility_summary <- mobility %>%
  group_by(county, state) %>%
  summarise(
    mean_work_pct = mean(work_pct, na.rm = TRUE),
    sd_work_pct = sd(work_pct, na.rm = TRUE),
    min_work_pct = min(work_pct, na.rm = TRUE),
    max_work_pct = max(work_pct, na.rm = TRUE),
    
    mean_retail_pct = mean(retail_and_rec_pct, na.rm = TRUE),
    sd_retail_pct = sd(retail_and_rec_pct, na.rm = TRUE),
    min_retail_pct = min(retail_and_rec_pct, na.rm = TRUE),
    max_retail_pct = max(retail_and_rec_pct, na.rm = TRUE),
    
    .groups = "drop"
  )

# ----------------------------
# Create risk level classification
# ----------------------------
county_metrics <- cases_sel %>%
  group_by(county, state) %>%
  summarise(
    # Historical impact factors
    deaths_rate = mean(deaths_per_10000, na.rm = TRUE),
    case_rate = mean(cases_per_10000, na.rm = TRUE),
    fatality_rate = mean(death_per_case, na.rm = TRUE),
    
    # Demographic vulnerability factors
    pop_density = mean(total_pop / (housing_units * 0.001), na.rm = TRUE),
    median_age = mean(median_age, na.rm = TRUE),
    unemployment_rate = mean(unemployed_pop / (employed_pop + unemployed_pop), na.rm = TRUE),
    median_income = mean(median_income, na.rm = TRUE),
    
    # Susceptibility to spread
    public_transit_use = mean(commuters_by_public_transportation, na.rm = TRUE),
    housing_density = mean(households / housing_units, na.rm = TRUE),
    
    .groups = "drop"
  )

# Join with vaccination data
if(exists("vacc_summary")) {
  county_metrics <- county_metrics %>%
    left_join(vacc_summary, by = c("county", "state"))
}

# Join with mobility data
if(exists("mobility_summary")) {
  county_metrics <- county_metrics %>%
    left_join(mobility_summary, by = c("county", "state"))
}

# Calculate risk score
# Z-score standardization for each factor
risk_factors <- county_metrics %>%
  mutate(across(c(deaths_rate, case_rate, fatality_rate, pop_density, 
                  median_age, unemployment_rate, public_transit_use, housing_density),
                ~scale(.)[,1])) %>%
  mutate(across(c(median_income, avg_Full_Dose_Pct, avg_Full_Dose_65plus),
                ~-scale(.)[,1]))  # Inverse scale since higher values reduce risk

# Calculate composite risk score
risk_factors <- risk_factors %>%
  rowwise() %>%
  mutate(
    # Historical impact component (40% weight)
    historical_impact = mean(c(deaths_rate, case_rate, fatality_rate), na.rm = TRUE) * 0.4,
    
    # Vulnerability component (30% weight)
    demographic_vulnerability = mean(c(median_age, unemployment_rate, -median_income), na.rm = TRUE) * 0.3,
    
    # Transmission risk component (30% weight)
    transmission_risk = mean(c(pop_density, public_transit_use, housing_density, 
                               -avg_Full_Dose_Pct, -avg_Full_Dose_65plus), na.rm = TRUE) * 0.3,
    
    # Total risk score
    total_risk_score = sum(historical_impact, demographic_vulnerability, transmission_risk, na.rm = TRUE)
  ) %>%
  ungroup()

# Create risk level classifications based on composite score
# Using quantiles to categorize into three groups
risk_quantiles <- quantile(risk_factors$total_risk_score, probs = c(0.33, 0.67), na.rm = TRUE)

cases_agg <- risk_factors %>%
  mutate(risk_level = case_when(
    total_risk_score < risk_quantiles[1] ~ "low",
    total_risk_score >= risk_quantiles[1] & total_risk_score <= risk_quantiles[2] ~ "medium",
    total_risk_score > risk_quantiles[2] ~ "high"
  )) %>%
  mutate(risk_level = factor(risk_level, levels = c("low", "medium", "high"))) %>%
  dplyr::select(county, state, risk_level, total_risk_score)

# ----------------------------
# Merge all data for modeling
# ----------------------------
county_model_data <- cases_sel %>%
  group_by(county, state) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
  left_join(vacc_summary, by = c("county", "state")) %>%
  left_join(mobility_summary, by = c("county", "state")) %>%
  left_join(cases_agg %>% dplyr::select(county, state, risk_level), by = c("county", "state")) %>%
  drop_na()

# ----------------------------
# Exploratory Data Analysis
# ----------------------------
# Add risk score to county_model_data
county_model_data <- county_model_data %>%
  left_join(risk_factors %>% dplyr::select(county, state, 
                                    historical_impact, demographic_vulnerability, 
                                    transmission_risk, total_risk_score), 
            by = c("county", "state"))

# Class distribution
class_dist <- table(county_model_data$risk_level)
class_dist_df <- as.data.frame(class_dist)
colnames(class_dist_df) <- c("Risk_Level", "Count")

# Plot class distribution
p1 <- ggplot(class_dist_df, aes(x = Risk_Level, y = Count, fill = Risk_Level)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("low" = "green", "medium" = "orange", "high" = "red")) +
  labs(title = "Distribution of Risk Levels",
       x = "Risk Level", y = "Number of Counties") +
  theme_minimal()

# Plot relationship between risk components
p2 <- ggplot(county_model_data, aes(x = demographic_vulnerability, y = transmission_risk, 
                                    color = risk_level, size = historical_impact)) +
  geom_point(alpha = 0.7) + 
  scale_color_manual(values = c("low" = "green", "medium" = "orange", "high" = "red")) +
  scale_size_continuous(name = "Historical Impact") +
  labs(title = "Risk Level Components",
       x = "Demographic Vulnerability", y = "Transmission Risk") +
  theme_minimal()

# Plot relationship between cases and deaths (with new classification)
p3 <- ggplot(county_model_data, aes(x = deaths_per_10000, y = cases_per_10000, color = risk_level)) +
  geom_point(size = 3, alpha = 0.7) + 
  scale_color_manual(values = c("low" = "green", "medium" = "orange", "high" = "red")) +
  labs(title = "Risk Level by Deaths and Cases per 10,000",
       x = "Deaths per 10,000", y = "Cases per 10,000") +
  theme_minimal()

# Display plots
grid.arrange(p1, p2, p3, ncol = 2)

# Feature correlation analysis
model_features <- county_model_data %>%
  dplyr::select(-c(county, state, risk_level, 
                   min_work_pct, min_retail_pct,
                   max_work_pct, max_retail_pct,
                   sd_work_pct, sd_retail_pct,
                   in_undergrad_college, nonfamily_households,
                   vacant_housing_units, median_year_structure_built,
                   unemployed_pop, percent_income_spent_on_rent))

cor_matrix <- cor(model_features, use = "pairwise.complete.obs")

cor_long <- as.data.frame(as.table(cor_matrix)) %>%
  rename(Var1 = Var1, Var2 = Var2, Correlation = Freq)

ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 8, hjust = 1)) +
  coord_fixed()

# ----------------------------
# Data Preparation for Modeling
# ----------------------------
# Exclude outcome-dependent variables and non-predictive features
predictors <- county_model_data %>%
  dplyr::select(-c(county, state, county, risk_level, 
            deaths_per_10000, cases_per_10000, death_per_case))

# Feature scaling for distance-based algorithms (k-NN, ANN)
preproc <- preProcess(predictors, method = c("center", "scale"))
scaled_predictors <- predict(preproc, predictors)

# Combine with target variable
model_data <- cbind(scaled_predictors, risk_level = county_model_data$risk_level)

# Split data
train_index <- createDataPartition(model_data$risk_level, p = 0.7, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Prepare input/output for neural network
train_data_nn <- train_data %>% dplyr::select(-risk_level)
test_data_nn <- test_data %>% dplyr::select(-risk_level)

# Create dummy variables for the target variable (for neural network)
risk_dummies <- dummyVars(~ risk_level, data = train_data)
train_risk_dummies <- predict(risk_dummies, train_data)
test_risk_dummies <- predict(risk_dummies, test_data)

# ----------------------------
# Model 1: Naive Bayes
# ----------------------------
nb_model <- naiveBayes(risk_level ~ ., data = train_data)

# Predictions
nb_preds <- predict(nb_model, newdata = test_data)
nb_probs <- predict(nb_model, newdata = test_data, type = "raw")

# Evaluation
nb_cm <- confusionMatrix(nb_preds, test_data$risk_level)
print("Naive Bayes Performance:")
print(nb_cm)

# Create ROC curve for Naive Bayes
nb_roc <- multiclass.roc(test_data$risk_level, nb_probs)
nb_auc <- nb_roc$auc

# ----------------------------
# Model 2: k-Nearest Neighbors
# ----------------------------
# Determine optimal k using cross-validation
k_values <- 1:20
k_accuracies <- sapply(k_values, function(k) {
  knn_cv <- knn.cv(train_data %>% dplyr::select(-risk_level), 
                   train_data$risk_level, k = k)
  cm <- confusionMatrix(knn_cv, train_data$risk_level)
  return(cm$overall["Accuracy"])
})

optimal_k <- k_values[which.max(k_accuracies)]
cat("Optimal k value:", optimal_k, "\n")

# Train k-NN with optimal k
knn_preds <- knn(train = train_data %>% dplyr::select(-risk_level),
                 test = test_data %>% dplyr::select(-risk_level),
                 cl = train_data$risk_level,
                 k = optimal_k,
                 prob = TRUE)

# Evaluation
knn_cm <- confusionMatrix(knn_preds, test_data$risk_level)
print("k-NN Performance:")
print(knn_cm)

# ----------------------------
# Model 3: Artificial Neural Network
# ----------------------------
# Formula for the neural network
features <- names(train_data_nn)
f <- as.formula(paste("risk_level.low + risk_level.medium + risk_level.high ~", 
                      paste(features, collapse = " + ")))

# Train neural network
ann_model <- neuralnet(
  f,
  data = cbind(train_data_nn, risk_level.low = train_risk_dummies[,1],
               risk_level.medium = train_risk_dummies[,2],
               risk_level.high = train_risk_dummies[,3]),
  hidden = c(10, 5),  # Two hidden layers
  linear.output = FALSE,
  threshold = 0.01,
  stepmax = 1e6,
  rep = 1,
  lifesign = "minimal"
)

# Predict with neural network
ann_pred_probs <- compute(ann_model, test_data_nn)$net.result
ann_preds <- levels(test_data$risk_level)[apply(ann_pred_probs, 1, which.max)]
ann_preds <- factor(ann_preds, levels = levels(test_data$risk_level))

# Evaluation
ann_cm <- confusionMatrix(ann_preds, test_data$risk_level)
print("Neural Network Performance:")
print(ann_cm)

# ----------------------------
# Model Comparison
# ----------------------------
# Create comparison dataframe
model_names <- c("Naive Bayes", "k-NN", "Neural Network")
accuracies <- c(nb_cm$overall["Accuracy"], knn_cm$overall["Accuracy"], ann_cm$overall["Accuracy"])
kappas <- c(nb_cm$overall["Kappa"], knn_cm$overall["Kappa"], ann_cm$overall["Kappa"])

model_comparison <- data.frame(
  Model = model_names,
  Accuracy = accuracies,
  Kappa = kappas
)

print(model_comparison)

# Plot comparison
ggplot(model_comparison, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.3f", Accuracy)), vjust = -0.5) +
  ylim(0, 1) +
  labs(title = "Model Accuracy Comparison",
       x = "Model", y = "Accuracy") +
  theme_minimal() +
  theme(legend.position = "none")

# ----------------------------
# Feature Importance Analysis
# ----------------------------
# Using Random Forest for feature importance
rf_model <- randomForest(risk_level ~ ., data = train_data, importance = TRUE, ntree = 500)
importance_df <- as.data.frame(importance(rf_model))
importance_df$Feature <- rownames(importance_df)

# Sort by Mean Decrease Gini
importance_df <- importance_df[order(-importance_df$MeanDecreaseGini),]

# Plot top 15 features
top_features <- head(importance_df, 15)
ggplot(top_features, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Top 15 Features by Importance",
       x = "Feature", y = "Mean Decrease Gini") +
  theme_minimal()

# ----------------------------
# Visualization of Confusion Matrices
# ----------------------------
# Function to plot confusion matrix
plot_confusion_matrix <- function(cm, title) {
  cm_df <- as.data.frame(cm$table)
  colnames(cm_df) <- c("Actual", "Predicted", "Freq")
  
  ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq), color = "white", size = 5) +
    scale_fill_gradient(low = "steelblue", high = "darkred") +
    labs(title = title, x = "Predicted", y = "Actual") +
    theme_minimal()
}

# Plot confusion matrices
p1 <- plot_confusion_matrix(nb_cm, "Naive Bayes Confusion Matrix")
p2 <- plot_confusion_matrix(knn_cm, "k-NN Confusion Matrix")
p3 <- plot_confusion_matrix(ann_cm, "Neural Network Confusion Matrix")

grid.arrange(p1, p2, p3, ncol = 2)

# ----------------------------
# Risk Class Profiles - Understanding the Class Characteristics
# ----------------------------
# Calculate median values for key metrics by risk level
risk_profiles <- county_model_data %>%
  group_by(risk_level) %>%
  summarise(
    # Historical COVID impact
    median_deaths_per_10k = median(deaths_per_10000, na.rm = TRUE),
    median_cases_per_10k = median(cases_per_10000, na.rm = TRUE),
    median_death_rate = median(death_per_case, na.rm = TRUE),
    
    # Demographics
    median_population = median(total_pop, na.rm = TRUE),
    median_age = median(median_age, na.rm = TRUE),
    median_income = median(median_income, na.rm = TRUE),
    unemployment_pct = median(unemployed_pop * 100, na.rm = TRUE),
    
    # Vaccination rates
    median_full_vacc_pct = median(avg_Full_Dose_Pct, na.rm = TRUE),
    median_65plus_vacc_pct = median(avg_Full_Dose_65plus, na.rm = TRUE),
    
    # Mobility data
    median_workplace_mobility = median(mean_work_pct, na.rm = TRUE),
    median_retail_mobility = median(mean_retail_pct, na.rm = TRUE),
    
    # Risk scores
    median_historical_impact = median(historical_impact, na.rm = TRUE),
    median_demographic_vulnerability = median(demographic_vulnerability, na.rm = TRUE),
    median_transmission_risk = median(transmission_risk, na.rm = TRUE),
    median_total_risk = median(total_risk_score, na.rm = TRUE),
    
    count = n(),
    .groups = "drop"
  )

# Print the risk profiles
print("Risk Level Profiles - Median Values:")
print(risk_profiles)

# Create a radar chart to visualize risk profiles
risk_profile_radar <- risk_profiles %>%
  dplyr::select(risk_level, 
         "Deaths per 10k" = median_deaths_per_10k,
         "Cases per 10k" = median_cases_per_10k,
         "Age" = median_age,
         "Pop Density" = median_population,
         "Unemployment" = unemployment_pct,
         "Vaccine Coverage" = median_full_vacc_pct,
         "Mobility Change" = median_workplace_mobility) %>%
  gather(key = "Metric", value = "Value", -risk_level) %>%
  # Normalize values to 0-1 scale for radar chart
  group_by(Metric) %>%
  mutate(Value = (Value - min(Value)) / (max(Value) - min(Value))) %>%
  ungroup()

ggplot(risk_profile_radar, aes(x = Metric, y = Value, fill = risk_level)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("low" = "green", "medium" = "orange", "high" = "red")) +
  labs(title = "Factor Contribution to Risk Levels",
       x = "Metric", y = "Normalized Value", fill = "Risk Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ----------------------------
# Predictions for a new county
# ----------------------------
predict_risk <- function(new_county_data, nb_model, knn_model_data, ann_model, preproc) {
  # Preprocess the new data
  new_data_scaled <- predict(preproc, new_county_data)
  
  # Naive Bayes prediction
  nb_pred <- predict(nb_model, newdata = new_data_scaled)
  
  # k-NN prediction
  knn_pred <- knn(train = knn_model_data %>% dplyr::select(-risk_level),
                  test = new_data_scaled,
                  cl = knn_model_data$risk_level,
                  k = optimal_k,
                  prob = TRUE)
  
  # Neural Network prediction
  ann_pred_probs <- compute(ann_model, new_data_scaled)$net.result
  ann_pred <- levels(train_data$risk_level)[which.max(ann_pred_probs)]
  
  # Return predictions
  return(list(
    naive_bayes = nb_pred,
    knn = knn_pred,
    neural_network = ann_pred,
    ensemble = names(which.max(table(c(nb_pred, knn_pred, ann_pred))))
  ))
}


# ------------------
# Texas County Map
# ------------------
library(sf)
library(tigris)
library(ggplot2)
library(viridis)

tx_counties <- counties(state = "TX", cb = TRUE)
tx_counties$county <- tolower(tx_counties$NAME)
map_data <- left_join(tx_counties, cases_agg, by = c("county"))

# Create map
ggplot() +
  geom_sf(data = map_data, aes(fill = risk_level), color = "white", size = 0.2) +
  scale_fill_manual(values = c("low" = "green", "medium" = "orange", "high" = "red"),
                    name = "Risk Level",
                    na.value = "gray90") +
  theme_minimal() +
  labs(title = "Pandemic Risk Level by Texas County",
       subtitle = "Based on multi-factor classification model")


# ------------------
# Decision Tree 
# ------------------

library(rpart)
library(rpart.plot)

# Train a decision tree model
tree_model <- rpart(risk_level ~ ., data = train_data, method = "class",
                    control = rpart.control(maxdepth = 5, minsplit = 5))

# Plot the decision tree with improved visualization
rpart.plot(tree_model, 
           box.palette = list("green", "orange", "red"), # Colors for risk levels
           shadow.col = "gray",
           nn = TRUE,   # Show node numbers
           extra = 101, # Show percentages and counts
           main = "Decision Tree for County Risk Classification")

# Variable importance from the tree
tree_importance <- tree_model$variable.importance
tree_importance_df <- data.frame(
  Feature = names(tree_importance),
  Importance = tree_importance
)
tree_importance_df <- tree_importance_df[order(-tree_importance_df$Importance),]

# Feature importance from decision tree
ggplot(head(tree_importance_df, 10), 
       aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Top 7 Features in Decision Tree",
       x = "Feature", y = "Importance") +
  theme_minimal()

# --------------------------
# Risk Score Distribution
# --------------------------

# Density plot of risk scores with color by risk level
ggplot(risk_factors, aes(x = total_risk_score, fill = cases_agg$risk_level)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c("low" = "green", "medium" = "orange", "high" = "red"),
                    name = "Risk Level") +
  geom_vline(xintercept = risk_quantiles, linetype = "dashed", color = "black") +
  annotate("text", x = risk_quantiles[1] - 0.5, y = 0.1, 
           label = "Low/Medium\nThreshold", angle = 90) +
  annotate("text", x = risk_quantiles[2] + 0.5, y = 0.1, 
           label = "Medium/High\nThreshold", angle = 90) +
  labs(title = "Distribution of Risk Scores by Classification",
       x = "Total Risk Score", y = "Density") +
  theme_minimal()

# Boxplot of the risk components by risk level
risk_components <- risk_factors %>%
  dplyr::select(county, state, historical_impact, demographic_vulnerability, transmission_risk) %>%
  left_join(cases_agg %>% 
              dplyr::select(county, state, risk_level), by = c("county", "state")) %>%
  pivot_longer(cols = c(historical_impact, demographic_vulnerability, transmission_risk),
               names_to = "Risk_Component", values_to = "Score")

ggplot(risk_components, aes(x = Risk_Component, y = Score, fill = risk_level)) +
  geom_boxplot() +
  scale_fill_manual(values = c("low" = "green", "medium" = "orange", "high" = "red"),
                    name = "Risk Level") +
  labs(title = "Distribution of Risk Components by Classification",
       x = "Risk Component", y = "Component Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# --------------------------
# ROC Curves
# --------------------------

library(pROC)
library(reshape2)

# For each binary classification (one-vs-rest)
risk_levels <- levels(train_data$risk_level)
roc_colors <- c("green", "orange", "red")

# Function to plot ROC curves for each model and risk level
plot_roc_curves <- function() {
  par(mfrow = c(1, 3))  # 1 row, 3 columns
  
  for (i in 1:length(risk_levels)) {
    level <- risk_levels[i]
    
    # Create binary outcomes for current risk level
    actual_binary <- ifelse(test_data$risk_level == level, 1, 0)
    
    # Get NB probabilities for current level
    nb_probs_binary <- nb_probs[, i]
    
    # Plot ROC for NB
    roc_nb <- roc(actual_binary, nb_probs_binary)
    plot(roc_nb, col = roc_colors[i], main = paste("ROC for", level, "Risk"),
         identity = TRUE, print.auc = TRUE)
    
    # Add legend
    legend("bottomright", legend = paste("AUC =", round(auc(roc_nb), 3)),
           col = roc_colors[i], lwd = 2)
  }
  par(mfrow = c(1, 1))  # Reset layout
}

plot_roc_curves()

# Create a heatmap of prediction probabilities by actual class
nb_probs_df <- as.data.frame(nb_probs)
nb_probs_df$actual <- test_data$risk_level

# Calculate average probabilities by actual class
avg_probs <- aggregate(nb_probs_df[, 1:3], 
                       by = list(Actual = nb_probs_df$actual), 
                       FUN = mean)

# Convert to long format for heatmap
avg_probs_long <- melt(avg_probs, id.vars = "Actual", 
                       variable.name = "Predicted", value.name = "Probability")

# Create heatmap
ggplot(avg_probs_long, aes(x = Predicted, y = Actual, fill = Probability)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", Probability)), color = "white") +
  scale_fill_gradient(low = "steelblue", high = "darkred") +
  labs(title = "Average Prediction Probabilities by Actual Class",
       x = "Predicted Risk Level", y = "Actual Risk Level") +
  theme_minimal()


ggplot(model_comparison, aes(x = Model)) +
  geom_bar(aes(y = Accuracy, fill = "Accuracy"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Kappa, fill = "Kappa"), stat = "identity", position = "dodge") +
  # Add text labels with actual decimal values
  geom_text(aes(y = Accuracy, label = sprintf("%.3f", Accuracy)), 
            position = position_dodge(width = 0.9), vjust = -0.5, color = "black") +
  geom_text(aes(y = Kappa, label = sprintf("%.3f", Kappa)), 
            position = position_dodge(width = 0.9), vjust = -0.5, color = "black") +
  # Set y-axis to go up to 1
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Model Performance: Accuracy vs Kappa",
       y = "Score") +
  scale_fill_manual(values = c("Accuracy" = "skyblue", "Kappa" = "darkblue"),
                    name = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# -----------------
# Cross Validation
# -----------------
generate_learning_curve <- function(model_type, predictor_data, response, folds = 5) {
  set.seed(123)
  
  # Create size sequence
  train_sizes <- seq(0.1, 1, by = 0.1)
  results <- data.frame()
  
  # For each training size
  for (size in train_sizes) {
    train_accuracies <- numeric(folds)
    test_accuracies <- numeric(folds)
    
    # Create folds
    fold_indices <- createFolds(response, k = folds)
    
    # For each fold
    for (i in 1:folds) {
      # Get test indices for this fold
      test_idx <- fold_indices[[i]]
      train_idx <- setdiff(1:length(response), test_idx)
      
      # Subsample the training data based on size parameter
      if (size < 1) {
        subsample_size <- floor(length(train_idx) * size)
        train_idx <- sample(train_idx, subsample_size)
      }
      
      # Prepare training and test sets
      train_x <- predictor_data[train_idx, ]
      train_y <- response[train_idx]
      test_x <- predictor_data[test_idx, ]
      test_y <- response[test_idx]
      
      # Train the model based on type
      if (model_type == "nb") {
        model <- naiveBayes(risk_level ~ ., data = cbind(train_x, risk_level = train_y))
        train_pred <- predict(model, train_x)
        test_pred <- predict(model, test_x)
      } else if (model_type == "knn") {
        train_pred <- knn(train = train_x, test = train_x, cl = train_y, k = optimal_k)
        test_pred <- knn(train = train_x, test = test_x, cl = train_y, k = optimal_k)
      }
      
      # Calculate accuracies
      train_accuracies[i] <- sum(train_pred == train_y) / length(train_y)
      test_accuracies[i] <- sum(test_pred == test_y) / length(test_y)
    }
    
    # Store results
    results <- rbind(results, data.frame(
      Model = model_type,
      TrainingSize = size,
      TrainAccuracy = mean(train_accuracies),
      TestAccuracy = mean(test_accuracies),
      TrainSD = sd(train_accuracies),
      TestSD = sd(test_accuracies)
    ))
  }
  
  return(results)
}

# Generate learning curves
nb_lc <- generate_learning_curve("nb", 
                                 train_data %>% dplyr::select(-risk_level), 
                                 train_data$risk_level)
knn_lc <- generate_learning_curve("knn", 
                                  train_data %>% dplyr::select(-risk_level), 
                                  train_data$risk_level)

# Combine learning curves
lc_combined <- rbind(nb_lc, knn_lc)

# Plot learning curves
ggplot(lc_combined, aes(x = TrainingSize, color = Model)) +
  geom_line(aes(y = TrainAccuracy, linetype = "Training")) +
  geom_line(aes(y = TestAccuracy, linetype = "Testing")) +
  geom_ribbon(aes(ymin = TestAccuracy - TestSD, 
                  ymax = TestAccuracy + TestSD, 
                  fill = Model), alpha = 0.1, color = NA) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Learning Curves for Classification Models",
       x = "Training Set Size", y = "Accuracy", 
       linetype = "Dataset") +
  theme_minimal() +
  facet_wrap(~ Model, ncol = 2)