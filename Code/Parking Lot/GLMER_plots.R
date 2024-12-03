# FOR LUPIN

# # Plotting m2_Lupin_numeric
plot_model(m2_Lupin_numeric, type = "pred", 
           terms = c("Year_Time_since_trt", "Treatment")) +
  theme_classic() +
  labs(x = "Time Since Treatment in Years",
       y = "Percent Cover of Lupin",
       title = NULL)

# # Plotting m2_Lupin_cat
plot_model(m2_Lupin_cat, type = "pred", 
           terms = c("Year", "Treatment")) +
  theme_classic() +
  labs(x = "Year of Data Collection",
       y = "Percent Cover of Lupin",
       title = NULL)

# # Plotting m2_Lupin_numeric
plot_model(m2_Lupin_numeric, type = "pred", 
           terms = c("Year_Time_since_trt", "Treatment")) +
  theme_classic() +
  labs(x = "Time Since Treatment in Years",
       y = "Percent Cover of Lupin",
       title = NULL)

# # Plotting m2_Lupin_cat
plot_model(m2_Lupin_cat, type = "pred", 
           terms = c("Year", "Treatment")) +
  theme_classic() +
  labs(x = "Year of Data Collection",
       y = "Percent Cover of Lupin",
       title = NULL)

Lupin_data2 <- Lupin_data %>%
  mutate(Percent_Cover_Lupin = Total_Lupin/Total_Count)

actual_lupin_plot <- ggplot(Lupin_data2, 
                            aes(Trt_Status, Percent_Cover_Lupin, fill = Treatment)) +
  geom_boxplot() +
  geom_point(position = position_dodge(width = .75)) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 0.15)) +
  labs(
    title = "Actual Percent Cover of Lupin by Treatment Status",
    x = "Treatment Status", 
    y = "Percent Cover of Lupin",
    fill = "Treatment"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"), # Bold x-axis label
    axis.title.y = element_text(face = "bold"), # Bold y-axis label
    #legend.title = element_text(face = "bold"), # Bold legend title
    #legend.text = element_text(face = "plain") # Lowercase text
  )

actual_lupin_plot

lupine_plots <- actual_lupin_plot | predicted_lupin_plot

# Lupin_data2 <- Lupin_data %>% mutate(Treatment = tolower(Treatment))
# Lupin_data2$Treatment <- factor(Nativity$Treatment, 
#                                 levels = c("control", "burn", "mechanical"))



# FOR NATIVE SPECIES

# Plot Residuals for m2_Nativity_cat
plot_model(m2_Nativity_cat, type = "diag")

# Forest Plot for m2_Nativity_cat
plot_model(m2_Nativity_cat, type = "est") + 
  geom_hline(yintercept = 1, linetype = 2)

# Prediction Plot m2_Nativity_cat
plot_model(m2_Nativity_cat, type = "pred", 
           terms = c("Year", "Treatment")) +
  theme_classic() +
  labs(x = "Year of Data Collection",
       y = "Percent Cover of Native Species",
       title = NULL)

# Plot Residuals for m2_Nativity_numeric
plot_model(m2_Nativity_numeric, type = "diag")

# Forest Plot for m2_Nativity_numeric
plot_model(m2_Nativity_numeric, type = "est") + 
  geom_hline(yintercept = 1, linetype = 2)

# Prediction Plot m2_Nativity_numeric
plot_model(m2_Nativity_numeric, type = "pred", 
           terms = c("Year_Time_since_trt", "Treatment")) +
  theme_classic() +
  labs(x = "Time Since Treatment in Years",
       y = "Percent Cover of Native Species",
       title = NULL)

Nativity_data2 <- Nativity_data %>%
  mutate(Percent_Cover_Native = Total_Native/Total_Count)

actual_native_plot <- ggplot(Nativity_data2, 
                             aes(Trt_Status, Percent_Cover_Native, fill = Treatment)) +
  geom_boxplot() +
  geom_point(position = position_dodge(width = .75)) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)
  ) +
  labs(
    title = "Actual Percent Cover of Native Species by Treatment Status",
    x = "Treatment Status",
    y = "Percent Cover of Native Species",
    fill = "Treatment"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"), # Bold x-axis label
    axis.title.y = element_text(face = "bold"), # Bold y-axis label
    #legend.title = element_text(face = "bold"), # Bold legend title
    #legend.text = element_text(face = "plain") # Lowercase text
    legend.position = "none"
  )

actual_native_plot

native_plots <- actual_native_plot | predicted_native_plot

native_plots

# FOR INVASIVE

actual_invasive_plot <- ggplot(Invasive_data2, 
                               aes(Trt_Status, Percent_Cover_Invasive, fill = Treatment)) +
  geom_boxplot() +
  geom_point(position = position_dodge(width = .75)) +
  scale_y_continuous(labels = scales::percent
                     #limits = c(0, 0.9)
  ) +
  labs(
    title = "Actual Percent Cover of Invasive Species by Treatment Status",
    x = "Treatment Status",
    y = "Percent Cover of Invasive Species",
    fill = "Treatment"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"), # Bold x-axis label
    axis.title.y = element_text(face = "bold"), # Bold y-axis label
    legend.title = element_text(face = "bold"), # Bold legend title
    legend.text = element_text(face = "plain") # Lowercase text
  )

actual_invasive_plot

invasive_plots <- actual_invasive_plot | predicted_invasive_plot

invasive_plots



# FOR SHRUBS

actual_shrub_plot <- ggplot(Shrubs_data2, 
                            aes(Trt_Status, Percent_Cover_Shrubs, fill = Treatment)) +
  geom_boxplot() +
  geom_point(position = position_dodge(width = .75)) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 0.8)
  ) +
  labs(
    title = "Actual Percent Cover of Shrubs by Treatment Status",
    x = "Treatment Status",
    y = "Percent Cover of Shrubs",
    fill = "Treatment"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"), # Bold x-axis label
    axis.title.y = element_text(face = "bold"), # Bold y-axis label
    #legend.title = element_text(face = "bold"), # Bold legend title
    #legend.text = element_text(face = "plain") # Lowercase text
    legend.position = "none"
  )

actual_shrub_plot

shrub_plots <- actual_shrub_plot | predicted_shrub_plot

shrub_plots


