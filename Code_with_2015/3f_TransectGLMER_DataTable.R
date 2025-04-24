source("Code_with_2015/3b_TransectGLMER_ModelRun.R")

library(ggpubr)
library(gridExtra)
library(dplyr)

# Create the raw data
lupine_LPI_df <- data.frame(
  Term = c(
    "(Intercept)", "TreatmentBurn", "TreatmentMechanical", "Year2009", "Year2011", "Year2012", "Year2013", "Year2015",
    "TreatmentBurn:Year2009", "TreatmentMechanical:Year2009",
    "TreatmentBurn:Year2011", "TreatmentMechanical:Year2011",
    "TreatmentBurn:Year2012", "TreatmentMechanical:Year2012",
    "TreatmentBurn:Year2013", "TreatmentMechanical:Year2013",
    "TreatmentBurn:Year2015", "TreatmentMechanical:Year2015"
  ),
  Estimate = c(
    -4.34858, 0.70641, 0.54936, 0.01302, 0.38642, 0.74678, 0.86798, 1.27514,
    0.28443, 0.15763,
    -0.89311, -1.82216,
    -0.71213, -1.24949,
    -0.76931, -1.15178,
    -0.74617, -1.31437
  ),
  Std.Error = c(
    0.33761, 0.24560, 0.25339, 0.31780, 0.26027, 0.25484, 0.24329, 0.23680,
    0.38076, 0.39478,
    0.34850, 0.43779,
    0.32750, 0.36718,
    0.31124, 0.33867,
    0.30289, 0.33774
  ),
  zvalue = c(
    -12.880, 2.876, 2.168, 0.041, 1.485, 2.930, 3.568, 5.385,
    0.747, 0.399,
    -2.563, -4.162,
    -2.174, -3.403,
    -2.472, -3.401,
    -2.463, -3.892
  ),
  Pr_z = c(
    "< 2e-16", 0.004024, 0.030152, 0.967315, 0.137619, 0.003386, 0.000360, "7.25e-08",
    0.455065, 0.689683,
    0.010386, "3.15e-05",
    0.029672, 0.000667,
    0.013447, 0.000672,
    0.013759, "9.95e-05"
  ),
  stringsAsFactors = FALSE
)

# Format numeric columns
lupine_LPI_df <- lupine_LPI_df %>%
  mutate(
    Estimate = sprintf("%.2f", Estimate),
    Std.Error = sprintf("%.2f", Std.Error),
    zvalue = sprintf("%.2f", zvalue),
    Pr_z = ifelse(grepl("[<a-z]", Pr_z), 
                        as.character(Pr_z), 
                        sprintf("%.3f", as.numeric(Pr_z))))


colnames(lupine_LPI_df) <- c("Fixed Effect", 
                             "Estimate", 
                             "Standard Error", 
                             "z-value", 
                             "Pr(>|z|)")


# Create the formatted table
table_lupine_LPI <- ggtexttable(lupine_LPI_df, rows = NULL, theme = ttheme("light"))


# Bold the Pr(>|z|) column values where Pr(>|z|) < 0.05
# 'Pr(>|z|)' is in column 6 (1-based index)
library(ggpubr)

# Let's assume your data frame is called `lupine_LPI_df`
# and it has been correctly formatted already

# Step 1: Create the table
table_lupine_LPI <- ggtexttable(lupine_LPI_df, rows = NULL, theme = ttheme("light"))

# Step 2: Loop through and bold significant p-values
for (i in 1:nrow(lupine_LPI_df)) {
  pr_raw <- lupine_LPI_df$`Pr(>|z|)`[i]
  pr_value <- suppressWarnings(as.numeric(pr_raw))
  
  if (!is.na(pr_value) && pr_value < 0.05) {
    # +1 because the first row in the table is the header
    table_lupine_LPI <- table_cell_font(table_lupine_LPI, row = i + 1, column = which(colnames(lupine_LPI_df) == "Pr(>|z|)"), face = "bold")
  }
}




# Tell R where to save the table
file_path <- file.path(Sys.getenv("HOME"), "Downloads", "lupineGLMER_table.png")

# Save the table as PNG
ggsave(file_path, plot = table_lupine_LPI, width = 10, height = 6, dpi = 300)
