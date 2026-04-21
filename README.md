# COVID-19 Data Science Series - Texas County Analysis
**Author:** Chris Hirschbrich | **Course:** CS 5331

---

## Overview

This repository contains three sequential data science projects that together form a complete, end-to-end analytical pipeline examining COVID-19 across Texas counties. Each project builds on the last - moving from raw data exploration, to unsupervised pattern discovery, to supervised classification - culminating in a deployable public health decision-support tool.

The central research question across all three projects is:

> **How do social distancing and vaccination interventions compare in their effectiveness at reducing COVID-19 infection rates and mortality, and which Texas counties remain most vulnerable to future pandemic events?**

---

## Project Progression

### Project 1 - Data and Visualization
*Foundation: Understanding the data landscape*

The first project establishes the analytical foundation. Using county- and state-level data from Google Cloud, the Google COVID-19 Open Data Repository, and the CDC, it explores how COVID-19 unfolded in Texas from 2020 through May 2023.

Key work includes:
- Cleaning and standardizing data across multiple sources using FIPS codes
- Creating time-series visualizations of cases, deaths, hospitalizations, and ICU admissions across four key pandemic milestones
- Mapping vaccination rates and case/death rates at the county level
- Producing correlation heatmaps linking demographic factors (age, income) to pandemic outcomes

**Core finding:** Vaccination had a stronger and more sustained effect on reducing mortality than social distancing. Rural counties - with lower vaccination rates and less healthcare infrastructure - consistently showed worse outcomes per capita than urban counties.

---

### Project 2 - Cluster Analysis
*Pattern Discovery: Grouping counties by shared characteristics*

Building on the cleaned dataset from Project 1, this project applies unsupervised machine learning to identify natural groupings among Texas counties. Both K-means and hierarchical clustering are applied at two critical time points: pre-vaccine (July 2020) and post-vaccine/booster (October 2021).

Key work includes:
- Determining the optimal number of clusters (k=3) using the elbow method (SSE)
- Profiling clusters using z-score heatmaps and bar charts for interpretability
- Evaluating cluster quality with silhouette analysis
- Performing supervised evaluation by overlaying known case rate groups onto cluster assignments

**Core finding:** Before vaccines, clusters separated primarily by mobility behavior and socioeconomic factors. After vaccines, vaccination rate became the dominant clustering variable - counties with lower uptake formed a distinct high-mortality cluster. This shift confirms vaccination as the critical differentiating factor in pandemic outcomes across Texas regions.

---

### Project 3 - Classification
*Prediction: Labeling counties by future pandemic risk*

The final project shifts from descriptive to predictive analysis. A composite **risk score** is engineered for each county from three weighted components:

| Component | Weight | Factors |
|---|---|---|
| Historical Impact | 40% | Case rates, death rates, fatality ratios |
| Demographic Vulnerability | 30% | Median age, unemployment, income (inverse) |
| Transmission Risk | 30% | Population density, public transit, housing density, vaccination (inverse) |

Three classifiers - **k-NN**, **Naive Bayes**, and **Neural Network** - are trained to assign each county a risk level of Low, Medium, or High. Model performance is compared using accuracy, Kappa, confusion matrices, ROC/AUC curves, and learning curves.

**Core finding:** Naive Bayes outperformed other models (78.2% accuracy, Kappa 0.670). The composite risk map shows high-risk counties concentrated in the Panhandle, southern border, and parts of West Texas - regions that also appeared in vulnerable clusters in Project 2. AUC values of 0.979 (low risk) and 0.907 (high risk) confirm strong discriminative ability.

---

## Data Sources

All three projects draw from the same three sources:

- **[Google Cloud Public Marketplace](https://console.cloud.google.com/marketplace)** - Mobility data (workplace and retail percent change from baseline)
- **[Google COVID-19 Open Data](https://health.google.com/covid-19/opendata/raw-data)** - Case counts, deaths, recoveries, and vaccination rates (Texas data supplied by the Texas Department of State Health Services)
- **[CDC COVID-19 Dataset](https://www.cdc.gov/covid/about/index.html)** - Official U.S. hospitalization, ICU, and mortality data

---

## Key Variables

| Variable | Description |
|---|---|
| `cases_per_1000` / `deaths_per_1000` | Normalized COVID-19 impact by county population |
| `death_per_case` | Case fatality ratio per county |
| `First_Dose_Pct` / `Full_Dose_Pct` | Vaccination coverage metrics |
| `Full_Dose_Pct65Plus` | Senior vaccination coverage (highest-risk demographic) |
| `work_pct` / `retail_and_rec_pct` | Mobility change from pre-pandemic baseline |
| `total_risk_score` | Composite risk index (engineered in Project 3) |
| `risk_level` | Ordinal classification: Low / Medium / High |

---

## How the Projects Connect

```
Project 1: Raw Data → Cleaned Dataset + Visualizations
                              ↓
Project 2: Cleaned Dataset → Cluster Assignments (unsupervised patterns)
                              ↓
Project 3: Clusters + Features → Risk Score + Classification Model (deployable tool)
```

Each project answers a progressively more actionable question:
- *What happened?* (Project 1)
- *What types of counties share similar experiences?* (Project 2)
- *Which counties are at risk, and can we predict it?* (Project 3)

---

## Policy Implications

Across all three projects, the recommendations converge on a consistent set of priorities for Texas public health officials:

1. **Prioritize vaccination over blanket social distancing** - vaccination provided sustained protection; mobility restrictions alone did not.
2. **Target rural and underserved counties** - the Panhandle and West Texas consistently appear as high-risk, low-vaccination areas across all three analyses.
3. **Use cluster profiles for tailored interventions** - one-size-fits-all statewide policy is less effective than cluster- or risk-level-specific responses.
4. **Invest in variant surveillance** - the largest mortality spikes were driven by new variants outpacing intervention timelines.
5. **Deploy the classification model operationally** - with biweekly data refreshes and quarterly retraining, the Project 3 model could serve as a real-time risk dashboard for county health departments.
