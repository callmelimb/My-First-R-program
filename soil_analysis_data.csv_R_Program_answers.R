#Dealing with Data of soil analysis

Data1 <-read.csv("E:\\R Data Sets\\soil_analysis_data.csv")
Data1

#QN2.DATA INSPECTION
# Dimensions of the dataset
dim(Data1)

# First few rows
head(Data1)

# Last few rows
tail(Data1)

# Summary statistics
summary(Data1)

# Data types of each column
str(Data1)

# Missing values per column
colSums(is.na(Data1))

# Number of duplicate rows
sum(duplicated(Data1))


library(magrittr)
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(dplyr)

#QN3. DATA CLEANING

# Handling Missing Data (example: impute with median for Nitrogen, remove rows for others)
median_nitrogen <- median(data$Nitrogen, na.rm = TRUE)
data$Nitrogen[is.na(data$Nitrogen)] <- median_nitrogen
data1 <- na.omit(Data1) # Remove rows with remaining missing values
Data1
# Removing Duplicates
Data1 <- unique(Data1)
Data1


# Install and load necessary libraries
install.packages("readr")
install.packages("ggplot2")
library(readr)
library(ggplot2)

Data1 <- read.csv("E:\\R Data Sets\\soil_analysis_data.csv")
names(Data1) <- c("District", "SoilType", "pHLevel", "OrganicMatter", "Nitrogen", "Phosphorus", "Potassium")
Data1
# Now your column names are: District, SoilType, pHLevel, OrganicMatter, Nitrogen, Phosphorus, Potassium

#QN4. DATA VISUALIZATION
# Histograms for Numeric Data
ggplot(Data1, aes(x = `pHLevel`)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Soil pH Level", x = "pH Level", y = "Frequency") +
  theme_minimal()

ggplot(Data1, aes(x = OrganicMatter)) +  # Corrected data frame & column name
  geom_histogram(binwidth = 0.2, fill = "forestgreen", color = "black") +
  labs(title = "Distribution of Soil Organic Matter", x = "Organic Matter (%)", y = "Frequency") +
  theme_minimal()

# Box Plots
ggplot(Data1, aes(y = `Nitrogen`)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Nitrogen Content", y = "Nitrogen (Kg/ha)") +
  theme_minimal()

ggplot(Data1, aes(x = `SoilType`, y = `Phosphorus`)) +
  geom_boxplot(fill = "lightcoral", color = "black") +
  labs(title = "Phosphorus Content by Soil Type", x = "Soil Type", y = "Phosphorus (Kg/ha)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatter Plot
ggplot(Data1, aes(x = OrganicMatter , y = Potassium )) +
  geom_point(color = "orange", size = 3) +
  labs(title = "Relationship between Organic Matter and Potassium", x = "Organic Matter (%)", y = "Potassium (Kg/ha)") +
  theme_minimal()

# Bar Plot for Categorical Data
ggplot(Data1, aes(x = District)) +
  geom_bar(fill = "purple") +
  labs(title = "Frequency of Samples by District", x = "District", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#QN. 4d part
# Correlation Matrix
# Load the dplyr library
library(dplyr)

# Calculate the correlation matrix
numeric_data <- data %>%  # Changed data_cleaned to data (important!)
  select_if(is.numeric)  # Ensure only numeric data is used
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
cor_matrix




####ASSIGNMENT 2.

#1.  Step 1: Choose a Target Variable

#Identify and justify one numeric target variable.
#From the soil analysis data, I'll choose "Potassium Content (kg/ha)" as the target variable. 

#Justification: Potassium is an essential nutrient for plant growth, and its level in the soil is a critical indicator of soil health and fertility. Analyzing which factors influence Potassium Content can provide valuable insights for agricultural management. 


#2.  Step 2: Define Hypotheses for Each Feature

#Here are the null and alternative hypotheses for each feature:

#Numeric Features: 
#pH Level, Organic Matter (%), Nitrogen Content (kg/ha), Phosphorus Content (kg/ha)
#Null (Ho): There is no correlation between this feature and Potassium Content (kg/ha). 
#Alternative (H1): There is a significant correlation between this feature and Potassium Content (kg/ha). 

# Categorical Features: District, Soil Type
# Null (Ho): The mean Potassium Content (kg/ha) is the same across all districts/soil types. 
# Alternative (H1): At least one district/soil type has a significantly different mean Potassium Content (kg/ha).


#QN3.  Step 3: Perform Statistical Tests
# Load necessary libraries
library(readr)
library(dplyr)

# Load the data
data <- read_csv("E:\\R Data Sets\\soil_analysis_data.csv") 
data

# Numeric Features vs. Numeric Target (Potassium Content)
cor_test_results <- data.frame(Feature = character(0), Correlation = numeric(0), P_value = numeric(0), stringsAsFactors = FALSE)

numeric_features <- c("pH Level", "Organic Matter (%)", "Nitrogen Content (kg/ha)", "Phosphorus Content (kg/ha)")

for (feature in numeric_features) {
  test_result <- cor.test(data[[feature]], data$`Potassium Content (kg/ha)`)
  cor_test_results <- rbind(cor_test_results, data.frame(Feature = feature, Correlation = test_result$estimate, P_value = test_result$p.value))
}

print("Correlation Tests:")
print(cor_test_results)

# Categorical Features vs. Numeric Target (Potassium Content)
# ANOVA for Categorical Features (District and Soil Type)
# ULTIMATE, EXTREME, PARANOID Debugging - ANOVA

# 0.  Cleanest Possible Data Preparation

# Ensure the target variable is numeric and the features are character
data$CleanTarget <- as.numeric(data$`Potassium Content (kg/ha)`)
data$CleanDistrict <- as.character(data$District)
data$CleanSoilType <- as.character(data$`Soil Type`)

# Remove any rows with NA in the clean columns (just to be 100% safe)
data_clean <- na.omit(data[, c("CleanTarget", "CleanDistrict", "CleanSoilType")])


# 1.  REBUILD Everything from Scratch - No paste()

# Hardcoded test, extreme simplification, NO paste()
feature <- "CleanSoilType"  # Try BOTH "CleanDistrict" and "CleanSoilType" separately
target_variable <- "CleanTarget"
separator <- " ~ "
formula_string <- paste0(target_variable, separator, feature)  # SUPER simple paste0
print(paste0("Formula string (ULTIMATE): '", formula_string, "'"))

# Manually construct the formula object (bypass as.formula() if possible)
if (feature == "CleanSoilType") {
  formula <- CleanTarget ~ CleanSoilType  # HARDCODED formula
} else if (feature == "CleanDistrict") {
  formula <- CleanTarget ~ CleanDistrict  # HARDCODED formula
}
print(paste0("Formula object (ULTIMATE): ", deparse(formula)))  # deparse() for safe printing


anova_test <- aov(formula, data = data_clean)  # Use the CLEAN data
anova_summary <- summary(anova_test)

print(str(anova_summary))
print(anova_summary)

# 2.   ULTRA-Defensive Access to Summary (If Needed)

# If the above fails, access summary elements with EXTREME caution
# (But it SHOULDN'T fail now with the cleaning and hardcoding)

anova_results <- data.frame(Feature = character(0), F_value = numeric(0), P_value = numeric(0), stringsAsFactors = FALSE)

categorical_features_clean <- c("CleanDistrict", "CleanSoilType")

for (clean_feature in categorical_features_clean) {
  # (Re-use the formula creation from above, but for each clean_feature)
  target_variable <- "CleanTarget"
  separator <- " ~ "
  formula_string <- paste0(target_variable, separator, clean_feature)
  print(paste0("Formula string (loop): '", formula_string, "'"))
  
  if (clean_feature == "CleanSoilType") {
    formula_loop <- CleanTarget ~ CleanSoilType  # HARDCODED in loop
  } else if (clean_feature == "CleanDistrict") {
    formula_loop <- CleanTarget ~ CleanDistrict
  }
  print(paste0("Formula object (loop): ", deparse(formula_loop)))
  
  anova_test_loop <- aov(formula_loop, data = data_clean)
  anova_summary_loop <- summary(anova_test_loop)
  
  print(str(anova_summary_loop))
  
  # Access summary with ABSOLUTE caution
  anova_results <- rbind(anova_results,
                         data.frame(Feature = clean_feature,
                                    F_value = anova_summary_loop[[1]][1, "F value"],  # Explicit row/col
                                    P_value = anova_summary_loop[[1]][1, "Pr(>F)"],   # Explicit row/col
                                    stringsAsFactors = FALSE))
}

print(anova_results)



# #5.  Deliverables for Phase 3
# Here's how to structure your deliverables:
# 
# Short note on P-value, t-test, and ANOVA:
# Explain what a p-value represents (the probability of observing the data if the null hypothesis is true).
# Briefly describe the t-test (compares means of two groups) and ANOVA (compares means of more than two groups). 
# R code: Provide the R code I gave you, with clear comments. 
# Summary Table: Create a table like this (replace with your actual values):



# Brief Reflection:
#   Discuss which features seem to be the strongest predictors of Potassium Content.
# Mention any limitations of the analysis.
# Suggest potential future research directions.
# 
# Optional Visual Plots:
#   Scatter plots for numeric features with correlation lines.
# Box plots for categorical features showing the distribution of Potassium Content across groups. 
# Update GitHub Link: Make sure your GitHub repository is updated with all the code and report





