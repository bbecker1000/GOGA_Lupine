source("Code/2d_Census_GLMER.R")

# FOR THE MODEL WITH INDIVIDUAL AS.FACTOR(YEAR)

# Plot residuals with negative binomial 
plot_model(m_Lupin_census_nb,type = "diag")
plot(m_Lupin_census_nb)

# Plot basic view of model
plot_model(m_Lupin_census_nb, type = "eff", 
           terms = c("Year", "Treatment", "Status")) +
  theme_classic() 

# Extract the model data
predicted_census <- Lupin_Census %>%
  mutate(predicted = predict(m_Lupin_census_nb, type = "response"))

# Use ggplot to create a boxplot of the model data
predicted_lupin_census_plot <- ggplot(predicted_census, 
                                      aes(x = Year, y = predicted, fill = Treatment)) +
  geom_boxplot() +
  geom_point(data = Lupin_Census, 
             aes(Year, RowCount),
             position = position_dodge(width = .75),
             alpha = 0.5) +
  labs(
    title = "Predicted Lupin Count by Year",
    x = "Treatment Status", 
    y = "Predicted Percent Cover of Lupine",
    fill = "Treatment"
  ) +
  facet_wrap(~Status) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20), # Bold x-axis label
    axis.title.y = element_text(face = "bold", size = 20), # Bold y-axis label
    axis.text = element_text(size = 17),
    legend.title = element_text(face = "bold", size = 19), # Bold legend title
    legend.text = element_text(face = "plain", size = 17) # Lowercase text
  )

# View graph
predicted_lupin_census_plot


# FOR THE MODEL WITH PRE/POST-TREATMENT

# Plot residuals with negative binomial
plot_model(m_Lupin_census_nb_PrePost,type = "diag")
plot(m_Lupin_census_nb_PrePost)

# Plot basic view of model
plot_model(m_Lupin_census_nb_PrePost, type = "eff", 
           terms = c("Trt_Status", "Treatment", "Status")) +
  theme_classic() 

# Extract the model data
predicted_census_PrePost <- Lupin_Census %>%
  mutate(predicted = predict(m_Lupin_census_nb_PrePost, type = "response"))

# Use ggplot to create a boxplot of the model data
predicted_lupin_census_plot_PrePost <- ggplot(predicted_census_PrePost, 
                                              aes(x = Trt_Status, y = predicted, fill = Treatment)) +
  geom_boxplot() +
  geom_point(data = Lupin_Census, 
             aes(Trt_Status, RowCount),
             position = position_dodge(width = .75),
             alpha = 0.5) +
  labs(
    title = "Predicted Lupin Count by Treatment Status",
    x = "Treatment Status", 
    y = "Predicted Percent Cover of Lupine",
    fill = "Treatment"
  ) +
  facet_wrap(~Status) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20), # Bold x-axis label
    axis.title.y = element_text(face = "bold", size = 20), # Bold y-axis label
    axis.text = element_text(size = 17),
    legend.title = element_text(face = "bold", size = 19), # Bold legend title
    legend.text = element_text(face = "plain", size = 17) # Lowercase text
  )

# View graph
predicted_lupin_census_plot_PrePost


# FOR SAVING GRAPHS

file_path <- file.path(Sys.getenv("HOME"), "Downloads", "predicted_lupin_census_plot.png")
# 
# # Save the plot using ggsave
ggsave(file_path, plot = predicted_lupin_census_plot,
       width = 15, height = 10,   # Set desired width and height in inches
       dpi = 300,               # Set the resolution (300 DPI for high quality)
       units = "in",            # Set units to inches
       device = "png")
