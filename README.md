# Factors-Influencing-Happiness
Exploratory analysis of the World Happiness Report 2024, examining how key socio-economic factors (GDP, health, freedom, generosity, corruption perception) relate to reported happiness levels across countries.

🔑 Key Dimensions
Data preprocessing: handling missing values with median imputation
Descriptive statistics: summary measures of happiness indicators
Visualization: scatter plots, correlation heatmaps, density plots, and boxplots
Regression analysis:ladder score vs. GDP, life expectancy, freedom
Multivariate: combined model with stepwise selection (AIC)

🔍 Methods
Data cleaning and missing value treatment (dplyr, tidyverse)
Descriptive analysis (summary)
Visualization (ggplot2, pheatmap)
Regression modeling (lm, glm, MASS::stepAIC)

⚠️ Notes
Dataset: The World Happiness Report 2024 dataset should be placed in the data/ folder.
Confidentiality: Do not commit raw sensitive data; if necessary, provide only anonymized or sampled data and reference the official source in README.
For reproducibility, consider using renv to lock R package versions.
