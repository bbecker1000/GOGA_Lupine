source("Code_with_2015/3b_UpdatedCensusData_GLMER.R")

library(ggpubr)
library(gridExtra)
library(dplyr)
library(writexl)


# Turn the model output into a dataframe
#sum_immature_df <- as.data.frame(sum_immature$coefficients)
sum_immature_count_df <- as.data.frame(sum_immature_count$coefficients)
sum_census_count_df <- as.data.frame(sum_census_count$coefficients)


# Add a column with the response variable
#sum_immature_df$label <- "Percent Immature Lupine (Census Data)"
sum_immature_count_df$label <- "Immature Lupine Count (Census Data)"
sum_census_count_df$label <- "Lupine Count (Census Data)"


# Merge the datasets together
glmer_combined_census <- bind_rows(sum_immature_df,
                                   sum_immature_count_df,
                                   sum_census_count_df)


# Round to three decimal places
glmer_combined_census$Estimate <- round(glmer_combined_census$Estimate, 2)
glmer_combined_census$`Std. Error` <- round(glmer_combined_census$`Std. Error`, 2)
glmer_combined_census$`z value` <- round(glmer_combined_census$`z value`, 2)
glmer_combined_census$`Pr(>|z|)` <- ifelse(glmer_combined_census$`Pr(>|z|)` < 0.001, 
                                    "< 0.001", 
                                    round(glmer_combined_census$`Pr(>|z|)`, 3))


# Make the fixed effects which are currently row names into their own column
glmer_combined_census_df <- rownames_to_column(glmer_combined_census)

glmer_combined_censusdf_2 <- glmer_combined_census_df %>%
  mutate(Fixed_Effect = rowname %>%
           str_replace_all("Treatment", "Treatment ") %>%  
           str_replace_all(":Year", " "))

glmer_combined_censusdf_2$Fixed_Effect <- gsub("\\.\\.\\.\\d+$", "", 
                                               glmer_combined_censusdf_2$Fixed_Effect)

glmer_combined_censusdf_2 <- glmer_combined_censusdf_2 %>%
  filter(Fixed_Effect %in% c("Treatment Burn 2009",
                             "Treatment Burn 2011",
                             "Treatment Burn 2012",
                             "Treatment Burn 2013",
                             "Treatment Burn 2015",
                             "Treatment Mechanical 2009",
                             "Treatment Mechanical 2011",
                             "Treatment Mechanical 2012",
                             "Treatment Mechanical 2013",
                             "Treatment Mechanical 2015"))


# Reorder the columns
glmer_combined_censusdf_3 <- glmer_combined_censusdf_2 %>%
  select("label", "Fixed_Effect", "Estimate", "Std. Error", "z value", "Pr(>|z|)")


# Rename the columns for clarity
colnames(glmer_combined_censusdf_3) <- c("Response Variable",
                                   "Fixed Effect", 
                                   "Estimate", 
                                   "Standard Error", 
                                   "z-value", 
                                   "Pr(>|z|)")

# Save the dataframe
file_path <- file.path(Sys.getenv("HOME"), "Downloads", "census_glmer.xlsx")

write_xlsx(glmer_combined_censusdf_3, path = file_path)

