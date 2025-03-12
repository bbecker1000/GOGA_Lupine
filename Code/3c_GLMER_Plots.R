source("Code/2C_GLMER.R")
library(patchwork)


# LUPINE MODEL

# Extract model data
predicted_data_year <- Lupin_data %>%
  mutate(predicted = predict(m_Lupin_Year, type = "response"))

# Plot extracted data
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
    y = "Percent Cover Lupine",
    fill = "Treatment"
  ) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "black", size = 1) +
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





# # SHRUB MODEL

# Extract data from model
predicted_data_shrub_year <- Shrubs_data %>%
  mutate(predicted = predict(m_Shrub_Year, type = "response"))

# Plot extracted data
predicted_shrub_plot_year <- ggplot(predicted_data_shrub_year, 
  aes(x = as.factor(Year), y = predicted, fill = Treatment)) +
  geom_boxplot() +
  geom_point(data = Shrubs_data, 
             aes(x = as.factor(Year), y = Total_Shrubs/Total_Count, fill = Treatment), 
             shape = 21, color = "black",
             position = position_dodge(width = .75),
             alpha = 0.7) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(
    title = "Shrubs",
    x = "Treatment Status",
    y = "Percent Cover",
    fill = "Treatment") +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "black", size = 1) +
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
    panel.grid.major = element_line(color = "black", linewidth = 0.2),
    panel.grid.minor = element_line(color = "black", linewidth = 0.2),
    plot.tag.position = c(0.15, 0.99)) 

# View graph
predicted_shrub_plot_year





# # NATIVE MODEL

# Extract model data
predicted_data_nativity_year <- Nativity_data %>%
  mutate(predicted = predict(m_Nativity_Year, type = "response"))

# Plot extracted data
predicted_native_plot_year <- ggplot(predicted_data_nativity_year, 
  aes(x = as.factor(Year), y = predicted, fill = Treatment)) +
  geom_boxplot() +
  geom_point(data = Nativity_data2, 
             aes(as.factor(Year), Percent_Cover_Native, fill = Treatment),
             shape = 21, color = "black",
             position = position_dodge(width = .75),
             alpha = 0.7) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(
    title = "Native Species",
    x = "Treatment Status",
    #y = "Percent Cover",
    fill = "Treatment"
  ) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "black", size = 1) +
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





# # INVASIVE MODEL

# Extract model data
predicted_data_invasive_year <- Invasive_data %>%
  mutate(predicted = predict(m_Invasive_Year, type = "response"))

# Plot extracted data
predicted_invasive_plot_year <- ggplot(predicted_data_invasive_year, 
  aes(x = as.factor(Year), y = predicted, fill = Treatment)) +
  geom_boxplot() +
  geom_point(data = Invasive_data, 
             aes(x = as.factor(Year), y = Total_Invasive/Total_Count, fill = Treatment),
             shape = 21, color = "black",
             position = position_dodge(width = .75),
             alpha = 0.7) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(
    title = "Invasive Species",
    #x = "Treatment Status",
    #y = "Percent Cover",
    fill = "Treatment"
  ) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "black", size = 1) +
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




# Create a figure panel with shrubs, native, and invasive
shrub_nativity_plots <- predicted_shrub_plot_year + predicted_native_plot_year + 
  predicted_invasive_plot_year + plot_annotation(tag_levels = "a")

# View Plot
shrub_nativity_plots



# FOR SAVING GRAPHS

# file_path <- file.path(Sys.getenv("HOME"), "Downloads", "LPI_Lupine.png")
#  
# # Save the plot using ggsave
# ggsave(file_path, plot = predicted_lupin_plot_year,
#        width = 13, height = 7,   # Set desired width and height in inches
#        dpi = 300,               # Set the resolution (300 DPI for high quality)
#        units = "in",            # Set units to inches
#        device = "png")

