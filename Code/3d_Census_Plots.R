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

predicted_census2 <- filter(predicted_census, Status == "L")

# Use ggplot to create a boxplot of the model data
predicted_lupin_census_plot <- ggplot(predicted_census2, 
                                      aes(x = Year, y = predicted, fill = Treatment)) +
  geom_boxplot() +
  geom_point(data = Lupin_Census, 
             aes(Year, RowCount),
             shape = 21, color = "black", size = 2,
             position = position_dodge(width = .75),
             alpha = 0.7) +
  labs(
    #title = "Predicted Lupin Count by Year (Census)",
    x = "Treatment Status", 
    y = "Count of Lupine Individuals",
    fill = "Treatment"
  ) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "black", size = 1) +
  theme_classic() +
  scale_fill_hue(
    labels = c("C" = "Control", "B" = "Burn", "M" = "Mechanical")) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20,
                                margin = margin(t = 15, b = 15)), 
    axis.title.y = element_text(face = "bold", size = 20,
                                margin = margin(l = 15, r = 15)),
    axis.text = element_text(color = "black", size = 17),
    legend.title = element_text(face = "bold", size = 19), # Bold legend title
    legend.text = element_text(face = "plain", size = 17), # Lowercase text
    legend.position = c(0.079,0.894),
    panel.border = element_rect(color = "black", fill = NA, size = .7),
    panel.grid.major = element_line(color = "black", size = 0.2),
    panel.grid.minor = element_line(color = "black", size = 0.2)
  ) 


# View graph
predicted_lupin_census_plot


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
    title = "Predicted Lupine Count by Treatment Status (Census)",
    x = "Treatment Status", 
    y = "Predicted Percent Cover of Lupine",
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
predicted_lupin_census_plot_PrePost


# FOR THE MODEL OF THE RATIO OF IMMATURE INDIVIDUALS

# Plot residuals
plot_model(m_lupin_immature,type = "diag")
plot(m_lupin_immature)

# Plot basic view of model
plot_model(m_lupin_immature, type = "eff", terms = c("Year", "Treatment")) +
  theme_classic() 

# Extract the model data
predicted_census_immature <- Lupin_Ratio %>%
  mutate(predicted = predict(m_lupin_immature, type = "response"))


# Ensure the predicted data frame is used instead of the model object
predicted_lupin_census_plot_immature <- ggplot(predicted_census_immature,    
  aes(x = Year, y = predicted, fill = Treatment)) +   
  geom_boxplot() +   
  geom_point(data = Lupin_Ratio,               
             aes(x = Year, y = Count_I / Total_Count),  # Use a proper ratio for y
             shape = 21, color = "black", size = 2,
             position = position_dodge(width = .75),
             alpha = 0.7) + 
  scale_y_continuous(labels = scales::percent) +
  labs(     
    x = "Treatment Status",      
    y = "Percentage of Immature Lupine",     
    fill = "Treatment"   
  ) +   
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "black", size = 1) +
  theme_classic() +
  scale_fill_hue(
    labels = c("C" = "Control", "B" = "Burn", "M" = "Mechanical")) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20,
                                margin = margin(t = 15, b = 15)), 
    axis.title.y = element_text(face = "bold", size = 20,
                                margin = margin(l = 15, r = 15)),
    axis.text = element_text(color = "black", size = 17),
    legend.title = element_text(face = "bold", size = 19), # Bold legend title
    legend.text = element_text(face = "plain", size = 17), # Lowercase text
    legend.position = c(0.08,0.893),
    panel.border = element_rect(color = "black", fill = NA, size = .7),
    panel.grid.major = element_line(color = "black", size = 0.2),
    panel.grid.minor = element_line(color = "black", size = 0.2)
  ) 

print(predicted_lupin_census_plot_immature)



# FOR SAVING GRAPHS

file_path <- file.path(Sys.getenv("HOME"), "Downloads", "Lupine_Immature.png")
#
# # Save the plot using ggsave
ggsave(file_path, plot = predicted_lupin_census_plot_immature,
       width = 12, height = 7,   # Set desired width and height in inches
       dpi = 300,               # Set the resolution (300 DPI for high quality)
       units = "in",            # Set units to inches
       device = "png")
