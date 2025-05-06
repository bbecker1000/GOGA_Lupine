source("Code_with_2015/3b_TransectGLMER_ModelRun.R")

library(ggpubr)
library(gridExtra)
library(dplyr)
library(gt)


# Turn the model output into a dataframe
#sum_lupine_df <- as.data.frame(sum_lupine$coefficients)
sum_native_df <- as.data.frame(sum_native$coefficients)
sum_invasive_df <- as.data.frame(sum_invasive$coefficients)
sum_shrub_df <- as.data.frame(sum_shrub$coefficients)


#Add a column with the response variable
#sum_lupine_df$label <- "Lupine Cover (LPI Transect Data)"
sum_native_df$label <- "Native Species Cover (LPI Transect Data)"
sum_invasive_df$label <- "Invasive Species Cover (LPI Transect Data)"
sum_shrub_df$label <- "Shrub Cover (LPI Transect Data)"


# Merge the datasets together
glmer_combined <- bind_rows(sum_shrub_df,
                            sum_native_df, 
                            sum_invasive_df,
                            #sum_lupine_df
                            )

# Round to three decimal places
glmer_combined$Estimate <- round(glmer_combined$Estimate, 2)
glmer_combined$`Std. Error` <- round(glmer_combined$`Std. Error`, 2)
glmer_combined$`z value` <- round(glmer_combined$`z value`, 2)
glmer_combined$`Pr(>|z|)` <- ifelse(glmer_combined$`Pr(>|z|)` < 0.001, 
                                    "< 0.001", 
                                    round(glmer_combined$`Pr(>|z|)`, 3))


# Make the fixed effects which are currently row names into their own column
glmer_combined_df <- rownames_to_column(glmer_combined)

glmer_combined_df_2 <- glmer_combined_df %>%
  mutate(Fixed_Effect = rowname %>%
           str_replace_all("Treatment", "Treatment ") %>%  
           str_replace_all(":Year", " "))

glmer_combined_df_2$Fixed_Effect <- gsub("\\.\\.\\.\\d+$", "", glmer_combined_df_2$Fixed_Effect)

glmer_combined_df_2 <- glmer_combined_df_2 %>%
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
glmer_combined_df_3 <- glmer_combined_df_2 %>%
  select("label", "Fixed_Effect", "Estimate", "Std. Error", "z value", "Pr(>|z|)")

# Rename the columns for clarity
colnames(glmer_combined_df_3) <- c("Response Variable",
                             "Fixed Effect", 
                             "Estimate", 
                             "Standard Error", 
                             "z-value", 
                             "Pr(>|z|)")


n_rows <- nrow(glmer_combined_df_3)


glmer_combined_table <- glmer_combined_df_3 %>%
  gt(groupname_col = "Response Variable") %>%
  #tab_stubhead(label = "Response Variable") %>%
  tab_options(heading.align = "center",
              table_body.hlines.style = "none",
              table_body.vlines.style = "none",
              stub.border.color = "black") %>%
  # Add horizontal line to separate by response variable
  tab_style(style = cell_borders(sides = c("bottom"),  weight = px(2)),
            locations = list(
              cells_body(rows = c(10, 20, 30, 40, 50)),
              cells_stub(rows = c(10, 20, 30, 40, 50)))) %>%
  # Left-align the "Fixed_Effect" column
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_body(columns = "Fixed Effect")) %>%
  # Center-align all other columns
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = -`Fixed Effect`)) %>%
  tab_options(
    table.border.top.color = "black",
    table.border.top.width = px(2),
    column_labels.border.top.color = "black",
    column_labels.border.top.width = px(2),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(2)
    ) %>%
  # Add black border below the last row
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(2)),
    locations = cells_body(rows = n_rows)) 
  
  
glmer_combined_table

# Bold the Pr_z values that are significant
pr_value <- suppressWarnings(as.numeric(glmer_combined_table$`Pr(>|z|)`[5]))

if (!is.na(pr_value) && pr_value < 0.05) {
  glmer_combined_table_2 <- glmer_combined_table %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = "Pr(>|z|)"
      )
    )
}



# Calculate total number of rows for bottom border
n_rows <- nrow(glmer_combined_df_3)

glmer_combined_table_3 <- glmer_combined_df_3 %>%
  gt(groupname_col = "Response Variable") %>%
  tab_options(
    heading.align = "center",
    table_body.hlines.style = "none",
    table_body.vlines.style = "none",
    table.border.top.color = "white",
    table.border.bottom.color = "white",
    heading.border.bottom.color = "black",
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "white",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black",
    table_body.hlines.color = "white",
    stub.border.color = "transparent"  # Keep this if you want no stub border
  ) %>%
  # Add horizontal separator lines at specific row breaks
  tab_style(
    style = cell_borders(sides = "bottom", weight = px(2)),
    locations = list(
      cells_body(rows = c(10, 20, 30, 40, 50)),
      cells_stub(rows = c(10, 20, 30, 40, 50))
    )
  ) %>%
  # Add bottom border to last row
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(2)),
    locations = cells_body(rows = n_rows)
  ) %>%
  # Left-align Fixed Effect
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_body(columns = "Fixed Effect")
  ) %>%
  # Center-align all other columns
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = -`Fixed Effect`)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = `Pr(>|z|)`,
      rows = as.numeric(gsub("[^0-9.]", "", `Pr(>|z|)`)) < 0.05 |
        grepl("< 0.001", `Pr(>|z|)`)))


glmer_combined_table_3


# Tell R where to save the table
file_path <- file.path(Sys.getenv("HOME"), "Downloads", "glmertable3.png")

# Save the table as PNG
gtsave(data = glmer_combined_table_3, filename = file_path)

