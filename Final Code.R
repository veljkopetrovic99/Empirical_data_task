# -------------------------------
# NUMBER OF PATIENTS DATA SET
# -------------------------------

# Install and load necessary packages
required_packages <- c("dplyr", "tidyverse", "plm", "corrplot", "car", "lmtest", "sandwich", "stargazer")
installed_packages <- rownames(installed.packages())

for(pkg in required_packages){
  if(!pkg %in% installed_packages){
    install.packages(pkg)
  }
}

library(dplyr)
library(tidyverse)
library(plm)
library(corrplot)
library(car)
library(lmtest)
library(sandwich)
library(stargazer)

# Create a copy of the dataset
Number_of_patients_v2 <- Number_of_patients 

# Transform the data
Number_of_patients_v2 <- Number_of_patients_v2 %>%
  mutate(
    # Step 1: Create new_age_bracket based on cla_age_5
    new_age_bracket = case_when(
      cla_age_5 %in% c("00-04", "05-09", "10-14", "15-19", "20-24") ~ "00-24",
      cla_age_5 %in% c("25-29", "30-34") ~ "25-34",
      cla_age_5 %in% c("35-39", "40-44") ~ "35-44",
      cla_age_5 %in% c("45-49", "50-54") ~ "45-54",
      cla_age_5 %in% c("55-59", "60-64") ~ "55-64",
      cla_age_5 %in% c("65-69", "70-74") ~ "65-74",
      cla_age_5 %in% c("75-79", "80-84") ~ "75-84",
      cla_age_5 %in% c("85-89", "90-94") ~ "85-94",
      cla_age_5 %in% "95et+" ~ "95p",
      TRUE ~ cla_age_5
    ),
    # Step 2: Capitalize 'femmes' and 'hommes' in 'libelle_sexe'
    libelle_sexe = case_when(
      libelle_sexe == "hommes" ~ "Hommes",
      libelle_sexe == "femmes" ~ "Femmes",
      TRUE ~ libelle_sexe
    ),
    # Step 3: Remove leading zeros from 'dept' where applicable
    dept = sub("^0([1-9])$", "\\1", dept)
  ) %>%
  # Step 4: Group the data by specified columns
  group_by(Year, dept, libelle_sexe, `Gender Dummy`, new_age_bracket) %>%
  # Step 5: Summarize the data by aggregating Ntop, Npop, and prev
  summarise(
    Ntop = sum(Ntop, na.rm = TRUE),
    Npop = sum(Npop, na.rm = TRUE),
    prev = sum(prev, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Step 6: Rename columns for clarity
  rename(
    Department = dept,
    Gender = libelle_sexe,
    Gender_Dummy = `Gender Dummy`,
    age = new_age_bracket
  ) %>%
  # Recode 'Gender_Dummy' column (1 remains 1, 2 becomes 0)
  mutate(
    Gender_Dummy = case_when(
      Gender_Dummy == 1 ~ 1,
      Gender_Dummy == 2 ~ 0,
      TRUE ~ as.numeric(Gender_Dummy)
    ),
    Year = as.numeric(Year)
  )

# -------------------------------
# NUMBER OF DEATHS DATA SET
# -------------------------------

# Create a copy of the dataset
Number_of_deaths_2 <- Number_of_deaths %>%
  # Step 1: Rename columns
  rename(
    Department = dep
  ) %>%
  # Step 2: Create Gender Dummy
  mutate(
    Gender_Dummy = case_when(
      Gender == "Hommes" ~ 1,
      Gender == "Femmes" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  # Step 3: Group the Age Variable
  mutate(
    new_age_bracket = case_when(
      classe_d_age_de_10_ans %in% c("<1", "< 1", "1-24") ~ "00-24",
      classe_d_age_de_10_ans == "25-34" ~ "25-34",
      classe_d_age_de_10_ans == "35-44" ~ "35-44",
      classe_d_age_de_10_ans == "45-54" ~ "45-54",
      classe_d_age_de_10_ans == "55-64" ~ "55-64",
      classe_d_age_de_10_ans == "65-74" ~ "65-74",
      classe_d_age_de_10_ans == "75-84" ~ "75-84",
      classe_d_age_de_10_ans == "85-94" ~ "85-94",
      classe_d_age_de_10_ans == "95p" ~ "95p",
      TRUE ~ classe_d_age_de_10_ans
    )
  ) %>%
  # Step 4: Summarize the Data
  group_by(Year, Department, Gender, Gender_Dummy, new_age_bracket) %>%
  summarise(
    Total_Deaths = sum(effectif_de_deces, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Step 5: Rename 'new_age_bracket' to 'age'
  rename(age = new_age_bracket)

# -------------------------------
# DATA MERGING
# -------------------------------

# Assign datasets
Cummulative_AIR_quality <- Cummulative_AIR_quality
Number_of_doctors <- Number_of_doctors_new

# Check if the columns to merge are of the same type
str(Number_of_patients_v2)
str(Number_of_deaths_2)
str(Cummulative_AIR_quality)
str(Number_of_doctors)

# Merge patients and deaths data
patients_deaths_merged <- Number_of_patients_v2 %>%
  inner_join(Number_of_deaths_2, 
             by = c("Year", "Department", "Gender", "Gender_Dummy", "age"))

# Merge with AIR quality data
final_merged_data <- patients_deaths_merged %>%
  left_join(Cummulative_AIR_quality, by = c("Year", "Department"))

# Merge with Number of doctors data
Data <- final_merged_data %>%
  left_join(Number_of_doctors, by = c("Year", "Department"))

# Extract and sort unique Department values
unique_departments <- Data %>%
  distinct(Department) %>%
  arrange(Department) %>%
  pull(Department)

# Print the unique departments and their count
print(unique_departments)
print(length(unique_departments))

# -------------------------------
# CLEAN THE NEW MERGED DATASET
# -------------------------------

# Filter years of interest (2015-2020)
Data_filtered <- Data %>%
  filter(Year >= 2015 & Year <= 2020)

# Verify excluded years
unique_years <- unique(Data_filtered$Year)
print(unique_years)

# Check for missing values
print(anyNA(Data_filtered))
print(sum(is.na(Data_filtered)))

# Identify and print rows with missing values
rows_with_NA <- Data_filtered %>%
  filter(if_any(everything(), is.na))
print(rows_with_NA)

# Exclude overseas departments
overseas_departments <- c("971", "972", "973", "974", "976")  # Ensure Department is character
Data_filtered <- Data_filtered %>%
  filter(!Department %in% overseas_departments)

# Verify removal
print(unique(Data_filtered$Department))

# Check again for missing values
print(anyNA(Data_filtered))

# Recalculate prevalence and mortality rate
Data_filtered <- Data_filtered %>%
  mutate(
    prev = Ntop / Npop,
    Total_Deaths = Total_Deaths / Npop
  )

# -------------------------------
# MAIN ANALYSIS
# -------------------------------

# Assign final dataset
df <- Data_filtered

# Convert to panel data
pdata <- pdata.frame(df, index = c("Department", "Year"))

# Check the panel structure
print(is.pbalanced(pdata))
print(pdim(pdata))
print(summary(pdata))

# Rename air pollutant columns for easier handling
pdata <- pdata %>%
  rename(
    NO2 = `Annual.average.concentration.of.NO2..µg.m..`,
    NO2_PopWeighted = `Population.weighted.annual.average.concentration.of.NO...µg.m..`,
    O3 = `Annual.average.concentration.of.O...µg.m..`,
    O3_PopWeighted = `Population.weighted.annual.average.concentration.of.O...µg.m..`,
    SOMO35 = `Annual.average.of.SOMO35..µg.m..day.`,
    SOMO35_PopWeighted = `Population.weighted.annual.average.of.SOMO35..µg.m..day.`,
    AOT40 = `Annual.average.of.AOT40..µg.m..hour.`,
    PM10 = `Annual.average.concentration.of.PM10..µg.m..`,
    PM10_PopWeighted = `Population.weighted.annual.average.concentration.of.PM10..µg.m..`,
    PM2_5 = `Annual.average.concentration.of.PM2.5..µg.m..`,
    PM2_5_PopWeighted = `Population.weighted.annual.average.concentration.of.PM2.5..µg.m..`
  )

# -------------------------------
# VARIABILITY ASSESSMENT
# -------------------------------

# Function to calculate within-group variance and CV
calculate_within_variability <- function(data, group_var, vars) {
  data %>%
    group_by(across(all_of(group_var))) %>%
    summarise(across(all_of(vars),
                     list(Variance = ~ var(.x, na.rm = TRUE),
                          Mean = ~ mean(.x, na.rm = TRUE)),
                     .names = "{col}_{fn}"),
              .groups = "drop") %>%
    mutate(across(ends_with("_Variance"), 
                  ~ sqrt(.x) / get(sub("_Variance$", "_Mean", cur_column())), 
                  .names = "{.col}_CV")) %>%
    select(all_of(group_var), ends_with("_Variance"), ends_with("_CV"))
}

# Function to calculate between-group variance and CV
calculate_between_variability <- function(data, time_var, vars) {
  data %>%
    group_by(across(all_of(time_var))) %>%
    summarise(across(all_of(vars),
                     list(Variance = ~ var(.x, na.rm = TRUE),
                          Mean = ~ mean(.x, na.rm = TRUE)),
                     .names = "{col}_{fn}"),
              .groups = "drop") %>%
    mutate(across(ends_with("_Variance"), 
                  ~ sqrt(.x) / get(sub("_Variance$", "_Mean", cur_column())), 
                  .names = "{.col}_CV")) %>%
    select(all_of(time_var), ends_with("_Variance"), ends_with("_CV"))
}

# Define variables of interest
variables_of_interest <- c("Ntop", "prev", "Total_Deaths",
                           "NO2", "NO2_PopWeighted",
                           "O3", "O3_PopWeighted",
                           "SOMO35", "SOMO35_PopWeighted",
                           "AOT40",
                           "PM10", "PM10_PopWeighted",
                           "PM2_5", "PM2_5_PopWeighted",
                           "Density", "Population")

# Calculate within-group variability
within_variability <- calculate_within_variability(pdata, "Department", variables_of_interest)

# Calculate between-group variability
between_variability <- calculate_between_variability(pdata, "Year", variables_of_interest)

# -------------------------------
# MULTICOLLINEARITY CHECK FOR AIR POLLUTANTS
# -------------------------------

# Select population-weighted air pollutants
pollutants_pop_weighted <- pdata %>%
  select(NO2_PopWeighted, O3_PopWeighted, SOMO35_PopWeighted, PM10_PopWeighted, PM2_5_PopWeighted)

# Correlation Matrix
corr_matrix <- cor(pollutants_pop_weighted, use = "complete.obs")
print("Correlation Matrix of Population-Weighted Air Pollutants:")
print(round(corr_matrix, 2))

# Plot Correlation Matrix
corrplot(corr_matrix, 
         method = "color", 
         type = "upper",
         tl.col = "black", 
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         title = "Correlation Matrix of Population-Weighted Air Pollutants",
         mar = c(0,0,1,0)) 

# Variance Inflation Factor (VIF)
lm_model <- lm(prev ~ NO2_PopWeighted + O3_PopWeighted + 
                 SOMO35_PopWeighted + PM10_PopWeighted + 
                 PM2_5_PopWeighted, data = pdata)

print(summary(lm_model))
vif_values <- vif(lm_model)
print(vif_values)

# After observing the VIF values, drop PM10 and O3 to reduce multicollinearity
lm_model_drop1 <- lm(prev ~ NO2_PopWeighted + 
                       SOMO35_PopWeighted + 
                       PM2_5_PopWeighted, data = pdata)
print(summary(lm_model_drop1))
vif_values_drop1 <- vif(lm_model_drop1)
print(vif_values_drop1)

# -------------------------------
# REGRESSION ANALYSIS
# -------------------------------

# Convert necessary variables to factors
pdata <- pdata %>%
  mutate(
    age = as.factor(age),
    Gender_Dummy = as.factor(Gender_Dummy)
  )

# Fixed Effects Model for Prevalence
fe_model_prev <- plm(prev ~ NO2_PopWeighted + SOMO35_PopWeighted + PM2_5_PopWeighted +
                       Gender_Dummy + age,
                     data = pdata, model = "within")

print(summary(fe_model_prev))

# Fixed Effects Model for Mortality
fe_model_deaths <- plm(Total_Deaths ~ NO2_PopWeighted + SOMO35_PopWeighted + PM2_5_PopWeighted +
                         Gender_Dummy + age,
                       data = pdata, model = "within")

print(summary(fe_model_deaths))

# Random Effects Model for Prevalence
re_model_prev <- plm(prev ~ NO2_PopWeighted + SOMO35_PopWeighted + PM2_5_PopWeighted +
                       Gender_Dummy + age + Density,
                     data = pdata, model = "random")

print(summary(re_model_prev))

# Random Effects Model for Mortality
re_model_deaths <- plm(Total_Deaths ~ NO2_PopWeighted + SOMO35_PopWeighted + PM2_5_PopWeighted +
                         Gender_Dummy + age + Density,
                       data = pdata, model = "random")

print(summary(re_model_deaths))

# Hausman Test to Compare FE and RE Models
hausman_prev <- phtest(fe_model_prev, re_model_prev)
print(hausman_prev)

hausman_deaths <- phtest(fe_model_deaths, re_model_deaths)
print(hausman_deaths)

# -------------------------------
# RESIDUAL DIAGNOSTICS
# -------------------------------

# Extract residuals
fe_residuals_prev <- residuals(fe_model_prev)
fe_residuals_deaths <- residuals(fe_model_deaths)

# Shapiro-Wilk Test for Normality
shapiro_prev <- shapiro.test(fe_residuals_prev)
print(shapiro_prev)

shapiro_deaths <- shapiro.test(fe_residuals_deaths)
print(shapiro_deaths)

# Kolmogorov-Smirnov Test for Normality (Large Sample Size)
ks_test_prev <- ks.test(fe_residuals_prev, "pnorm", mean = mean(fe_residuals_prev), sd = sd(fe_residuals_prev))
print(ks_test_prev)

ks_test_deaths <- ks.test(fe_residuals_deaths, "pnorm", mean = mean(fe_residuals_deaths), sd = sd(fe_residuals_deaths))
print(ks_test_deaths)

# -------------------------------
# ROBUST STANDARD ERRORS
# -------------------------------

# Robust SE for Fixed Effects Model (Prevalence)
fe_prev_robust <- coeftest(fe_model_prev, vcov = vcovHC(fe_model_prev, type = "HC1", cluster = "group"))
print(fe_prev_robust)

# Robust SE for Fixed Effects Model (Mortality)
fe_deaths_robust <- coeftest(fe_model_deaths, vcov = vcovHC(fe_model_deaths, type = "HC1", cluster = "group"))
print(fe_deaths_robust)

# Generate Stargazer Table for FE Models
stargazer(fe_model_prev, fe_model_deaths,
          type = "text",             
          title = "Fixed Effects Models with Robust Standard Errors",
          dep.var.labels = c("Prevalence", "Mortality"),
          covariate.labels = c("NO2 (Pop. Weighted)", 
                               "SOMO35 (Pop. Weighted)", 
                               "PM2.5 (Pop. Weighted)", 
                               "Gender (Dummy)", 
                               "Age: 25-34", 
                               "Age: 35-44", 
                               "Age: 45-54", 
                               "Age: 55-64", 
                               "Age: 65-74", 
                               "Age: 75-84", 
                               "Age: 85-94", 
                               "Age: 95+"),
          se = list(fe_prev_robust[, 2], fe_deaths_robust[, 2]),
          omit.stat = c("f", "ser"),
          notes.append = TRUE,
          notes = "Robust standard errors in parentheses.")

