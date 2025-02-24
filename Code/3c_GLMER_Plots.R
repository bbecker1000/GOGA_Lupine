source("Code/2C_GLMER.R")
library(patchwork)

# LUPIN DATA #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #

# Plot Residuals Lupin for m2_Lupin_PrePost
plot_model(m2_Lupin_PrePost, type = "diag")

# Forest Plot Lupin for m2_Lupin_PrePost
plot_model(m2_Lupin_PrePost, type = "est") + 
  geom_hline(yintercept = 1, linetype = 2)

# Prediction Plot m2_Lupin_PrePost
plot_model(m2_Lupin_PrePost, type = "pred", 
           terms = c("Trt_Status", "Treatment")) +
  theme_classic() +
  labs(x = "Treatment Status",
       y = "Percent Cover of Lupin",
       title = NULL)
 

# Extract the model data
predicted_data <- Lupin_data %>%
  mutate(predicted = predict(m2_Lupin_PrePost, type = "response"))

predicted_data_year <- Lupin_data %>%
  mutate(predicted = predict(m_Lupin_Year, type = "response"))

# Create a new df that has the percent cover of lupine
Lupin_data2 <- Lupin_data %>%
  mutate(Percent_Cover_Lupin = Total_Lupin/Total_Count)


# Use ggplot to create a boxplot of the model data
predicted_lupin_plot <- ggplot(predicted_data, 
                               aes(x = Trt_Status, y = predicted, fill = Treatment)) +
  geom_boxplot() +
  geom_point(data = Lupin_data2,
             aes(Trt_Status, Percent_Cover_Lupin),
             position = position_dodge(width = .75),
             alpha = 0.7
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Predicted Percent Cover of Lupin by Treatment Status",
    x = "Treatment Status", 
    y = "Predicted Percent Cover of Lupin",
    fill = "Treatment"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20), # Bold x-axis label
    axis.title.y = element_text(face = "bold", size = 20), # Bold y-axis label
    axis.text = element_text(size = 17),
    legend.title = element_text(face = "bold", size = 19), # Bold legend title
    legend.text = element_text(face = "plain", size = 17) # Lowercase text
  )

# View graph
predicted_lupin_plot



# Use ggplot to create a boxplot of the model data
predicted_lupin_plot_year <- ggplot(predicted_data_year, 
                                    aes(x = as.factor(Year), y = predicted, fill = Treatment)) +
  geom_boxplot() +
  geom_point(data = Lupin_data2,
             aes(x = as.factor(Year), y = Percent_Cover_Lupin, fill = Treatment),
             shape = 21, color = "black", size = 2,
             position = position_dodge(width = .75),
             alpha = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    #title = "Lupine LPI Transect Data",
    x = "Treatment Status", 
    y = "Percent Cover",
    fill = "Treatment"
  ) +
  theme_classic() +
  scale_fill_hue(
    labels = c("CONTROL" = "Control", "BURN" = "Burn", "MECHANICAL" = "Mechanical")) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20,
                                margin = margin(t = 15, b = 15)), 
    axis.title.y = element_text(face = "bold", size = 20,
                                margin = margin(l = 15, r = 15)),
    axis.text = element_text(color = "black", size = 17),
    legend.title = element_text(face = "bold", size = 19), # Bold legend title
    legend.text = element_text(face = "plain", size = 17), # Lowercase text
    legend.position = c(0.076,0.894),
    panel.border = element_rect(color = "black", fill = NA, size = .7),
    panel.grid.major = element_line(color = "black", size = 0.2),
    panel.grid.minor = element_line(color = "black", size = 0.2)
  ) 

# View graph
predicted_lupin_plot_year


# Predicted change in percent cover lupin with annual precip
rainfall_lupin <- plot_model(m2_Lupin_PrePost,
  type = "pred", 
  terms = c("yearly_rain"),
  color = "blue"
  ) +
  theme_classic() +
  labs(
    x = "Annual Precipitation (cm)",
       y = "Percent Cover of Lupin",
       title = "Predicted Percent Cover of Lupin by Rainfall"
    ) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title.x = element_text(size = 20), # Bold x-axis label
    axis.title.y = element_text( size = 20), # Bold y-axis label
    axis.text = element_text(size = 17),
    legend.title = element_text(face = "bold", size = 19), # Bold legend title
    legend.text = element_text(face = "plain", size = 17) # Lowercase text
  )
# View graph
rainfall_lupin


# FOR NATIVE DATA #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #

# Plot Residuals for m2_Nativity_PrePost
plot_model(m2_Nativity_PrePost, type = "diag")

# Forest Plot for m2_Nativity_PrePost
plot_model(m2_Nativity_PrePost, type = "est") + 
  geom_hline(yintercept = 1, linetype = 2)

# Prediction Plot m2_Nativity_PrePost
plot_model(m2_Nativity_PrePost, type = "pred", 
           terms = c("Trt_Status", "Treatment")) +
  theme_classic() +
  labs(x = "Treatment Status",
       y = "Percent Cover of Native Species",
       title = NULL)

# Extract the model data
predicted_data_nativity <- Nativity_data %>%
  mutate(predicted = predict(m2_Nativity_PrePost, type = "response"))

predicted_data_nativity_year <- Nativity_data %>%
  mutate(predicted = predict(m_Nativity_Year, type = "response"))

# Create a new df that has the percent cover of native species
Nativity_data2 <- Nativity_data %>%
  mutate(Percent_Cover_Native = Total_Native/Total_Count)

# Use ggplot to create a boxplot of the model data
predicted_native_plot <- ggplot(predicted_data_nativity, 
  aes(x = Trt_Status, y = predicted, fill = Treatment)) +
  geom_boxplot() +
  geom_point(data = Nativity_data2, 
             aes(Trt_Status, Percent_Cover_Native, fill = Treatment),
             position = position_dodge(width = .75),
             alpha = 0.5
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Predicted Percent Cover of Native Species by Treatment Status",
    x = "Treatment Status",
    y = "Predicted Percent Cover of Native Species",
    fill = "Treatment"
    ) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20), # Bold x-axis label
    axis.title.y = element_text(face = "bold", size = 20), # Bold y-axis label
    axis.text = element_text(size = 17),
    legend.title = element_text(face = "bold", size = 19), # Bold legend title
    legend.text = element_text(face = "plain", size = 17) # Lowercase text
  )

# View graph
predicted_native_plot

# Use ggplot to create a boxplot of the model data
predicted_native_plot_year <- ggplot(predicted_data_nativity_year, 
                                aes(x = as.factor(Year), y = predicted, fill = Treatment)) +
  geom_boxplot() +
  geom_point(data = Nativity_data2, 
             aes(as.factor(Year), Percent_Cover_Native, fill = Treatment),
             shape = 21, color = "black",
             position = position_dodge(width = .75),
             alpha = 0.7
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(
   title = "Native Species",
    x = "Treatment Status",
    #y = "Percent Cover",
    fill = "Treatment"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_text(face = "bold", size = 20,
                                 margin = margin(t = 15, b = 15)), 
    # axis.title.y = element_text(face = "bold", size = 20,
    #                             margin = margin(l = 15, r = 15)),
    axis.text = element_text(color = "black", size = 17),
    #legend.title = element_text(face = "bold", size = 19), # Bold legend title
    #legend.text = element_text(face = "plain", size = 17), # Lowercase text
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = .7),
    panel.grid.major = element_line(color = "black", size = 0.2),
    panel.grid.minor = element_line(color = "black", size = 0.2)
  ) 

# View graph
predicted_native_plot_year

# Predicted change in percent cover lupin with annual precip
rainfall_native <- plot_model(m2_Nativity_PrePost, 
  type = "pred", 
  terms = c("yearly_rain_scaled"),
  color = "green4") +
  theme_classic() +
  labs(
    x = "Annual Precipitation (cm)",
       y = "Percent Cover of Native",
       title = "Predicted Percent Cover of Native Species by Rainfall"
    ) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title.x = element_text(size = 20), # Bold x-axis label
    axis.title.y = element_text( size = 20), # Bold y-axis label
    axis.text = element_text(size = 17),
    legend.title = element_text(face = "bold", size = 19), # Bold legend title
    legend.text = element_text(face = "plain", size = 17) # Lowercase text
  )

# View graph
rainfall_native


# FOR INVASIVE DATA #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #

# Plot Residuals Invasive Species
plot_model(m2_Invasive_PrePost, type = "diag")

# Forest Plot Invasive Species
plot_model(m2_Invasive_PrePost, type = "est") + 
  geom_hline(yintercept = 1, linetype = 2)

# Predicted change in percent cover invasive with time since treatment for each treatment
plot_model(m2_Invasive_PrePost, type = "pred", 
           terms = c("Treatment", "Trt_Status")) +
  theme_classic() +
  labs(x = "Treatment Status",
       y = "Percent Cover of Invasive Species",
       title = NULL)

# Predicted change in percent cover invasive with annual precip
rainfall_invasive <- plot_model(m2_Invasive_PrePost, type = "pred", terms = c("yearly_rain")) +
  theme_classic() +
  labs(x = "Annual Precipitation (cm)",
       y = "Percent Cover of Invasive Species",
       title = "Predicted Percent Cover of Invasive Species by Rainfall") +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title.x = element_text(size = 20), # Bold x-axis label
    axis.title.y = element_text( size = 20), # Bold y-axis label
    axis.text = element_text(size = 17),
    legend.title = element_text(face = "bold", size = 19), # Bold legend title
    legend.text = element_text(face = "plain", size = 17) # Lowercase text
  )

# View graph
rainfall_invasive

# Extract the model data
predicted_data_invasive <- Invasive_data %>%
  mutate(predicted = predict(m2_Invasive_PrePost, type = "response"))

predicted_data_invasive_year <- Invasive_data %>%
  mutate(predicted = predict(m_Invasive_Year, type = "response"))

# Create a new df that has the percent cover of invasive species
Invasive_data2 <- Invasive_data %>%
  mutate(Percent_Cover_Invasive = Total_Invasive/Total_Count)

# Use ggplot to create a boxplot of the model data
predicted_invasive_plot <- ggplot(predicted_data_invasive, 
  aes(x = Trt_Status, y = predicted, fill = Treatment)) +
  geom_boxplot() +
  geom_point(data = Invasive_data2, 
             aes(Trt_Status, Percent_Cover_Invasive, fill = Treatment),
             position = position_dodge(width = .75),
             alpha = 0.5
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Predicted Percent Cover of Invasive Species by Treatment Status",
    x = "Treatment Status",
    y = "Predicted Percent Cover of Invasive Species",
    fill = "Treatment"
    ) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20), # Bold x-axis label
    axis.title.y = element_text(face = "bold", size = 20), # Bold y-axis label
    axis.text = element_text(size = 17),
    legend.title = element_text(face = "bold", size = 19), # Bold legend title
    legend.text = element_text(face = "plain", size = 17) # Lowercase text
  )

# View graph
predicted_invasive_plot

# Use ggplot to create a boxplot of the model data
predicted_invasive_plot_year <- ggplot(predicted_data_invasive_year, 
                                  aes(x = as.factor(Year), y = predicted, fill = Treatment)) +
  geom_boxplot() +
  geom_point(data = Invasive_data2, 
             aes(as.factor(Year), Percent_Cover_Invasive, fill = Treatment),
             shape = 21, color = "black",
             position = position_dodge(width = .75),
             alpha = 0.7
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(
    title = "Invasive Species",
    #x = "Treatment Status",
    #y = "Percent Cover",
    fill = "Treatment"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    axis.title = element_blank(),
    # axis.title.x = element_text(face = "bold", size = 20,
    #                             margin = margin(t = 15, b = 15)), 
    # axis.title.y = element_text(face = "bold", size = 20,
    #                             margin = margin(l = 15, r = 15)),
    axis.text = element_text(color = "black", size = 17),
    #legend.title = element_text(face = "bold", size = 19), # Bold legend title
    #legend.text = element_text(face = "plain", size = 17), # Lowercase text
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = .7),
    panel.grid.major = element_line(color = "black", size = 0.2),
    panel.grid.minor = element_line(color = "black", size = 0.2)
  ) 

# View graph
predicted_invasive_plot_year


# FOR SHRUB DATA #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #

# Plot Residuals Invasive Species
plot_model(m2_Shrub_PrePost, type = "diag")

# Forest Plot Invasive Species
plot_model(m2_Shrub_PrePost, type = "est") + 
  geom_hline(yintercept = 1, linetype = 2)

# Predicted change in percent cover invasive with time since treatment for each treatment
plot_model(m2_Shrub_PrePost, type = "pred", 
           terms = c("Treatment", "Trt_Status")) +
  theme_classic() +
  labs(x = "Treatment Status",
       y = "Percent Cover of Shrubs",
       title = NULL)

# Extract the model data
predicted_data_shrub <- Shrubs_data %>%
  mutate(predicted = predict(m2_Shrub_PrePost, type = "response"))

predicted_data_shrub_year <- Shrubs_data %>%
  mutate(predicted = predict(m_Shrub_Year, type = "response"))


# Create a new df that has the percent cover of shrubs
Shrubs_data2 <- Shrubs_data %>%
  mutate(Percent_Cover_Shrubs = Total_Shrubs/Total_Count)

# Use ggplot to create a boxplot of the model data
predicted_shrub_plot <- ggplot(predicted_data_shrub, 
            aes(x = Trt_Status, y = predicted, fill = Treatment)) +
  geom_boxplot() +
  geom_point(data = Shrubs_data2, 
            aes(Trt_Status, Percent_Cover_Shrubs, fill = Treatment), 
  position = position_dodge(width = .75),
  alpha = 0.5
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Predicted Percent Cover of Shrubs by Treatment Status",
    x = "Treatment Status",
    y = "Predicted Percent Cover of Shrubs",
    fill = "Treatment"
    ) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20), # Bold x-axis label
    axis.title.y = element_text(face = "bold", size = 20), # Bold y-axis label
    axis.text = element_text(size = 17),
    legend.title = element_text(face = "bold", size = 19), # Bold legend title
    legend.text = element_text(face = "plain", size = 17) # Lowercase text
  )

# View graph
predicted_shrub_plot

# Use ggplot to create a boxplot of the model data
predicted_shrub_plot_year <- ggplot(predicted_data_shrub_year, 
                               aes(x = as.factor(Year), y = predicted, fill = Treatment)) +
  geom_boxplot() +
  geom_point(data = Shrubs_data2, 
             aes(as.factor(Year), Percent_Cover_Shrubs, fill = Treatment), 
             shape = 21, color = "black",
             position = position_dodge(width = .75),
             alpha = 0.7
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(
    title = "Shrubs",
    x = "Treatment Status",
    y = "Percent Cover",
    fill = "Treatment"
  ) +
  theme_classic() +
  scale_fill_hue(
    labels = c("CONTROL" = "Control", "BURN" = "Burn", "MECHANICAL" = "Mechanical")) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    axis.title.x = element_blank(),
    #element_text(face = "bold", size = 20, margin = margin(t = 15, b = 15)), 
    axis.title.y = element_text(face = "bold", size = 20,
                                margin = margin(l = 15, r = 15)),
    axis.text = element_text(color = "black", size = 17),
    legend.title = element_text(face = "bold", size = 19), # Bold legend title
    legend.text = element_text(face = "plain", size = 17), # Lowercase text
    legend.position = c(0.79,0.894),
    panel.border = element_rect(color = "black", fill = NA, size = .7),
    panel.grid.major = element_line(color = "black", size = 0.2),
    panel.grid.minor = element_line(color = "black", size = 0.2),
    plot.tag.position = c(0.15, 0.99)
  ) 

# View graph
predicted_shrub_plot_year



# Predicted change in percent cover invasive with annual precip
rainfall_shrubs <- plot_model(m2_Shrub_PrePost, 
  type = "pred", 
  terms = c("yearly_rain"),
  color = "orange2") +
  theme_classic() +
  labs(x = "Annual Precipitation (cm)",
       y = "Percent Cover of Shrubs",
       title = "Predicted Percent Cover of Shrubs by Rainfall") +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title.x = element_text(size = 20), # Bold x-axis label
    axis.title.y = element_text( size = 20), # Bold y-axis label
    axis.text = element_text(size = 17),
    legend.title = element_text(face = "bold", size = 19), # Bold legend title
    legend.text = element_text(face = "plain", size = 17) # Lowercase text
  )

# View graph
rainfall_shrubs

# Put a rainfall graphs together
rainfall_plots <- (rainfall_lupin | rainfall_native) / (rainfall_invasive | rainfall_shrubs)

# View all rainfall graphs
rainfall_plots


shrub_nativity_plots <- predicted_shrub_plot_year + predicted_native_plot_year + 
  predicted_invasive_plot_year + plot_annotation(tag_levels = "a")

shrub_nativity_plots

# FOR SAVING GRAPHS

file_path <- file.path(Sys.getenv("HOME"), "Downloads", "Lupine_LPI.png")
# 
# # Save the plot using ggsave
ggsave(file_path, plot = predicted_lupin_plot_year,
       width = 13, height = 7,   # Set desired width and height in inches
       dpi = 300,               # Set the resolution (300 DPI for high quality)
       units = "in",            # Set units to inches
       device = "png")

