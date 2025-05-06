source("Code_with_2015/3B_TransectGLMER_ModelRun.R") 

library(patchwork)

# LUPINE PLOT # # # # # # # # # # # # # # # # # # #

# Change levels of year for plotting
predicted_lupine_year_2015$Year <- factor(predicted_lupine_year_2015$Year, 
                                   levels = c("2009", "2010", "2011", "2012", "2013", "2015"))

Lupin_data_2015_2 <- Lupin_data_2015 %>%
  mutate(Lupin_Ratio = Total_Lupin / Total_Count)

Lupin_data_2015_2 <- as.data.frame(Lupin_data_2015_2)

Lupin_data_2015_2$Year <- factor(Lupin_data_2015_2$Year, 
                          levels = c("2009", "2010", "2011", "2012", "2013", "2015"))


# Plot extracted data
predicted_lupin_plot_2015 <- ggplot(predicted_lupine_year_2015, 
  aes(x = Year, y = predicted, fill = Treatment)) +
  geom_boxplot() +
  geom_point(data = Lupin_data_2015_2,
             aes(x = Year, y = Lupin_Ratio, fill = Treatment),
             shape = 21, 
             color = "black", 
             size = 2,
             position = position_dodge(width = .75),
             alpha = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "black", size = 1) +
  labs(x = "Treatment Status", 
       y = "Percent Cover Lupine",
       fill = "Treatment") +
  scale_fill_manual(
    values = c(
      "Control" = "#00BA38",
      "Burn" = "#F8766D",
      "Mechanical" = "#619CFF")) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20,
                                margin = margin(t = 15, b = 15)), 
    axis.title.y = element_text(face = "bold", size = 20,
                                margin = margin(l = 15, r = 15)),
    axis.text = element_text(color = "black", size = 17),
    legend.title = element_text(face = "bold", size = 19),
    legend.text = element_text(face = "plain", size = 17), 
    legend.position = c(0.01, 0.99), 
    legend.justification = c(0, 1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = .7),
    panel.grid.major = element_line(color = "black", linewidth = 0.2),
    panel.grid.minor = element_line(color = "black", linewidth = 0.2)
  ) 


# View graph
predicted_lupin_plot_2015


# NATIVITY PLOT # # # # # # # # # # # # # # # # # # #

# Change levels of year for plotting
predicted_nativity_year_2015$Year <- factor(predicted_nativity_year_2015$Year, 
                                     levels = c("2009", "2010", "2011", "2012", "2013", "2015"))

Nativity_data_2015_2 <- Nativity_data_2015 %>%
  mutate(Native_Ratio = Total_Native / Total_Count)

Nativity_data_2015_2 <- as.data.frame(Nativity_data_2015_2)

Nativity_data_2015_2$Year <- factor(Nativity_data_2015_2$Year, 
                                 levels = c("2009", "2010", "2011", "2012", "2013", "2015"))


# Plot extracted data
predicted_nativity_plot_2015 <- ggplot(predicted_nativity_year_2015, 
  aes(x = Year, y = predicted, fill = Treatment)) +
  geom_boxplot() +
  geom_point(data = Nativity_data_2015_2,
             aes(x = Year, y = (Total_Native/Total_Count), fill = Treatment),
             shape = 21, 
             color = "black", 
             size = 2,
             position = position_dodge(width = .75),
             alpha = 0.7) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "black", size = 1) +
  labs(title = "Native Species",
      x = "Treatment Status", 
       #y = "Percent Cover",
       fill = "Treatment") +
  scale_fill_manual(
    values = c(
      "Control" = "#00BA38",
      "Burn" = "#F8766D",
      "Mechanical" = "#619CFF")) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_text(face = "bold", size = 20,
                                margin = margin(t = 15, b = 15)), 
    axis.text = element_text(color = "black", size = 17),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = .7),
    panel.grid.major = element_line(color = "black", linewidth = 0.2),
    panel.grid.minor = element_line(color = "black", linewidth = 0.2)
  ) 

# View graph
predicted_nativity_plot_2015



# INVASIVE PLOT # # # # # # # # # # # # # # # # # # #

# Change levels of year for plotting
predicted_invasive_year_2015$Year <- factor(predicted_invasive_year_2015$Year, 
                                     levels = c("2009", "2010", "2011", "2012", "2013", "2015"))

Invasive_data_2015_2 <- Invasive_data_2015 %>%
  mutate(Invasive_Ratio = Total_Invasive / Total_Count)

Invasive_data_2015_2 <- as.data.frame(Invasive_data_2015_2)

Invasive_data_2015_2$Year <- factor(Invasive_data_2015_2$Year, 
                                    levels = c("2009", "2010", "2011", "2012", "2013", "2015"))


# Plot extracted data
predicted_invasive_plot_2015 <- ggplot(predicted_invasive_year_2015, 
  aes(x = Year, y = predicted, fill = Treatment)) +
  geom_boxplot() +
  geom_point(data = Invasive_data_2015_2,
             aes(x = Year, y = Invasive_Ratio, fill = Treatment),
             shape = 21, 
             color = "black", 
             size = 2,
             position = position_dodge(width = .75),
             alpha = 0.7) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "black", size = 1) +
  labs(title = "Invasive Species",
       #x = "Treatment Status", 
       #y = "Percent Cover",
       fill = "Treatment") +
  scale_fill_manual(
    values = c(
      "Control" = "#00BA38",
      "Burn" = "#F8766D",
      "Mechanical" = "#619CFF")) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(), 
    axis.text = element_text(color = "black", size = 17),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = .7),
    panel.grid.major = element_line(color = "black", size = 0.2),
    panel.grid.minor = element_line(color = "black", size = 0.2)
  ) 

# View graph
predicted_invasive_plot_2015







# SHRUB PLOT # # # # # # # # # # # # # # # # # # #


# Change levels of year for plotting
predicted_shrub_year_2015$Year <- factor(predicted_shrub_year_2015$Year, 
                                  levels = c("2009", "2010", "2011", "2012", "2013", "2015"))

Shrub_data_2015_2 <- Shrubs_data_2015 %>%
                     mutate(Shrub_Ratio = Total_Shrubs / Total_Count)

Shrub_data_2015_2 <- as.data.frame(Shrub_data_2015_2)

Shrub_data_2015_2$Year <- factor(Shrub_data_2015_2$Year, 
                          levels = c("2009", "2010", "2011", "2012", "2013", "2015"))

# Plot extracted data
predicted_shrub_plot_2015 <- ggplot(predicted_shrub_year_2015, 
  aes(x = Year, y = predicted, fill = Treatment)) +
  geom_boxplot() +
  geom_point(data = Shrub_data_2015_2,
             aes(x = Year, y = Shrub_Ratio, fill = Treatment),
             shape = 21, 
             color = "black", 
             size = 2,
             position = position_dodge(width = .75),
             alpha = 0.7) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "black", size = 1) +
  labs(title = "Shrubs",
       #x = "Treatment Status", 
       y = "Percent Cover",
       fill = "Treatment") +
  scale_fill_manual(
    values = c(
      "Control" = "#00BA38",
      "Burn" = "#F8766D",
      "Mechanical" = "#619CFF")) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold", size = 20,
                                 margin = margin(l = 15, r = 15)), 
    axis.text = element_text(color = "black", size = 17),
    legend.title = element_text(face = "bold", size = 19), 
    legend.text = element_text(face = "plain", size = 17), 
    legend.position      = c(0.01, 0.99), 
    legend.justification = c(0, 1),
    panel.border = element_rect(color = "black", fill = NA, size = .7),
    panel.grid.major = element_line(color = "black", linewidth = 0.2),
    panel.grid.minor = element_line(color = "black", linewidth = 0.2),
    plot.tag.position = c(0.15, 0.99)
  ) 

# View graph
predicted_shrub_plot_2015



# Create a figure panel with shrubs, native, and invasive
community_plots <- predicted_shrub_plot_2015 + 
                   predicted_nativity_plot_2015 + 
                   predicted_invasive_plot_2015 + 
                   plot_annotation(tag_levels = "a")

# View graph
community_plots 




# FOR SAVING GRAPHS

# Tell R where to save the graphs
file_path_lupine <- file.path(Sys.getenv("HOME"), "Downloads", "predicted_lupin_plot_2015.png")
file_path_community <- file.path(Sys.getenv("HOME"), "Downloads", "community_plots.png")


# Save the plot using ggsave
ggsave(file_path_community, plot = community_plots ,
       width = 20, height = 10, 
       dpi = 300,
       units = "in",          
       device = "png")
