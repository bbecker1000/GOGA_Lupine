source("Code_with_2015/2a_GLLVM_Setup.R")

library(ggpubr)
library(gridExtra)
library(dplyr)
library(gt)
library(writexl)
library(stringr)
library(dplyr)
library(tidyverse)

# CREATE A DATA FRAME WITH ALL DATA

sum_gllvm <- as.data.frame(glvvm.coef.plot_2015_yr) %>%
  select("Covariate", "Species", "Estimate", "Estimate.sd") %>%
  # Relabel the covariates to make them clearer
  mutate(Covariate = case_when(
    Covariate == "yearly_rain" ~ "Annual Rainfall",
    str_detect(Covariate, "Year\\d{4}\\.Treatment") ~ str_c(
      str_replace_all(str_extract(Covariate, "Treatment\\w+"), "Treatment", "Treatment "),
      str_extract(Covariate, "Year\\d{4}") %>% str_replace("Year", ""),
      sep = " "),TRUE ~ Covariate)) %>%
  mutate(confidence_interval = paste0(
    round(Estimate, 2), " ± ", round(2 * Estimate.sd, 2))) %>%
  select("Covariate", "Species", "confidence_interval", "Estimate", "Estimate.sd")
  


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ #


# CREATE A DATA FRAME WITH BURN AND MECHANICAL

# Filter to keep only the relevant columns and covariates
sum_gllvm_mechburn <- as.data.frame(glvvm.coef.plot_2015_yr) %>%
  select("Covariate", "Species", "Estimate", "Estimate.sd") %>%
  filter(Covariate %in% c("Year2009.TreatmentBurn",
                          "Year2011.TreatmentBurn",
                          "Year2012.TreatmentBurn",
                          "Year2013.TreatmentBurn",
                          "Year2015.TreatmentBurn",
                          "Year2009.TreatmentMechanical",
                          "Year2011.TreatmentMechanical",
                          "Year2012.TreatmentMechanical",
                          "Year2013.TreatmentMechanical",
                          "Year2015.TreatmentMechanical")) %>%
# Create new columns for Treatment and Burn
  mutate(
    Treatment = case_when(
      str_detect(Covariate, "Burn") ~ "Burn",
      str_detect(Covariate, "Mechanical") ~ "Mechanical"),
    Year = str_extract(Covariate, "\\d{4}")) %>%
# Relabel the covariates to make them clearer
  mutate(Covariate = case_when(
         Covariate == "yearly_rain" ~ "Annual Rainfall",
         str_detect(Covariate, "Year\\d{4}\\.Treatment") ~ str_c(
            str_replace_all(str_extract(Covariate, "Treatment\\w+"), "Treatment", "Treatment "),
            str_extract(Covariate, "Year\\d{4}") %>% str_replace("Year", ""),
            sep = " "),
         TRUE ~ Covariate)) %>%
    mutate(confidence_interval = paste0(
    round(Estimate, 2), " ± ", round(2 * Estimate.sd, 2))) %>%
  select("Treatment", "Species", "Year", "confidence_interval")


# Pivot the data wider
df_wide <- sum_gllvm_mechburn %>%
  select("Species", "Year", "Treatment", "confidence_interval") %>%
  pivot_wider(
    names_from = Treatment,
    values_from = confidence_interval,
    names_prefix = "Treatment")

# Define your custom order
species_order <- c(
  "Substrate", "Native UG", "Native PU", "Native PS", "Native PL", "Native PG", "Native PF",
  "Native NG", "Native NF", "Native BF", "Native AG", "Native AF", "Lupine",
  "Invasive PS", "Invasive PG", "Invasive PF", "Invasive AG", "Invasive AF",
  "Introduced PS", "Introduced AG", "Introduced AF"
)

# Make Species a factor with this order
df_wide <- df_wide %>%
  mutate(Species = factor(Species, levels = species_order)) %>%
  arrange(Species)



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ #



# CREATE A DATA FRAME WITH ANNUAL RAINFALL ONLY

# Filter to keep only the relevant columns and covariates
sum_gllvm_rain <- as.data.frame(glvvm.coef.plot_2015_yr) %>%
  select("Covariate", "Species", "Estimate", "Estimate.sd") %>%
  filter(Covariate %in% c("yearly_rain")) %>%
  mutate(confidence_interval = paste0(
    round(Estimate, 2), " ± ", round(2 * Estimate.sd, 2))) %>%
  select("Covariate", "Species", "confidence_interval") %>%
  mutate(Species = factor(Species, levels = species_order)) %>%
  arrange(Species)

view(sum_gllvm_rain)



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ #


# SAVE THE DATA

# Define the file path (e.g., saving to Downloads)
file_path <- file.path(Sys.getenv("HOME"), "Downloads", "gllvm_output_all.xlsx")

# Save the data
write_xlsx(sum_gllvm, path = file_path)
