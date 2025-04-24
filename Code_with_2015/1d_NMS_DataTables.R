library(ggpubr)
library(gridExtra)

# Create the data frame
model_output_adonis <- data.frame(
  Term = c("Year", "Treatment", "Year:Treatment", "Residual", "Total"),
  Df = c(5, 2, 10, 123, 140),
  SumOfSqs = c(3.91, 0.84, 0.46, 8.17, 13.38),
  R2 = c(0.29, 0.06, 0.03, 0.61, 1.00),
  F = c(11.7838, 6.3379, 0.6929, NA, NA),
  Pr_F = c(0.001, 0.001, 0.91, NA, NA)
)

colnames(model_output_adonis) <- c("Fixed Effect", "Df", "Sum of Squares", "R2", "F", "Pr(>F)")

# Replace NA values with blank strings
model_output_adonis[is.na(model_output_adonis)] <- ""


# Create a table plot
table_adonis <- ggtexttable(model_output_adonis,
                            rows = NULL, 
                            theme = ttheme("light"))

# Bold the Pr(>F) column values where Pr(>F) < 0.05
# 'Pr(>F)' is in column 6 (1-based index)
for (i in 1:nrow(model_output_adonis)) {
  pr_value <- as.numeric(model_output_adonis$`Pr(>F)`[i])
  if (!is.na(pr_value) && pr_value < 0.05) {
    table_adonis <- table_cell_font(table_adonis, row = i + 1, column = 6, face = "bold")
  }
}


# Tell R where to save the table
file_path <- file.path(Sys.getenv("HOME"), "Downloads", "adonis_output_table.png")


# Save as PNG
ggsave(file_path, plot = table_adonis, width = 8, height = 4, dpi = 300)

