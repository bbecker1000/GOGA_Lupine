source("Code_with_2015/4b_UpdatedCensusData_GLMER.R")


# # LUPINE CENSUS NEGATIVE BINOMIAL BY YEAR # #

# Change levels of year for plotting
predicted_Lupin_nb_yr$Year <- factor(predicted_Lupin_nb_yr$Year, 
                                      levels = c("2009", "2010", "2011", "2012", "2013", "2015"))

Lupine_Density_2009_2015_grouped_live2 <- as.data.frame(Lupine_Density_2009_2015_grouped_live)
 
Lupine_Density_2009_2015_grouped_live2$Year <- factor(Lupine_Density_2009_2015_grouped_live2$Year, 
                                           levels = c("2009", "2010", "2011", "2012", "2013", "2015"))


# Use ggplot to create a boxplot of the model data
boxplot_2015Census_nb <- ggplot(predicted_Lupin_nb_yr, 
                         aes(x = Year, y = predicted, fill = Treatment)) +
  geom_boxplot() +
  geom_point(data = Lupine_Density_2009_2015_grouped_live2, 
             aes(Year, Count),
             shape = 21, color = "black", size = 2,
             position = position_dodge(width = .75),
             alpha = 0.7) +
  geom_vline(xintercept = 2.5, 
             linetype = "dashed", 
             color = "black", 
             linewidth = 1) +
  labs(x = "Year", 
       y = "Count of Lupine Individuals",
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
    panel.border = element_rect(color = "black", fill = NA, size = .7),
    panel.grid.major = element_line(color = "black", linewidth = 0.2),
    panel.grid.minor = element_line(color = "black", linewidth = 0.2)
  ) 

# View graph
boxplot_2015Census_nb





# # PERCENT IMMATURE LUPINE BINOMIAL BY YEAR # #

# Change levels of year for plotting
predicted_ImmatureLupin_b$Year <- factor(predicted_ImmatureLupin_b$Year, 
                                  levels = c("2009", "2010", "2011", "2012", "2013", "2015"))

Lupin_Ratio_2009_2015_2 <- as.data.frame(Lupin_Ratio_2009_2015)

Lupin_Ratio_2009_2015_2$Year <- factor(Lupin_Ratio_2009_2015_2$Year, 
                                levels = c("2009", "2010", "2011", "2012", "2013", "2015"))


# Use ggplot to create a boxplot of the model data
boxplot_2015Census_Immature <- ggplot(predicted_ImmatureLupin_b, 
                                aes(x = Year, y = predicted, fill = Treatment)) +
  geom_boxplot() +
  geom_point(data = Lupin_Ratio_2009_2015_2, 
             aes(x = Year, y = (Count_I / Total_Count)),
             shape = 21, color = "black", size = 2,
             position = position_dodge(width = .75),
             alpha = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  geom_vline(xintercept = 2.5, 
             linetype = "dashed", 
             color = "black", 
             linewidth = 1) +
  labs(x = "Year", 
       y = "Percentage of Immature Lupine",
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
    legend.position = c(0.079,0.894),
    panel.border = element_rect(color = "black", fill = NA, size = .7),
    panel.grid.major = element_line(color = "black", linewidth = 0.2),
    panel.grid.minor = element_line(color = "black", linewidth = 0.2)
  ) 


# View graph
boxplot_2015Census_Immature



# # COUNT IMMATURE LUPINE BINOMIAL BY YEAR # #

# Change levels of year for plotting
predicted_ImmatureCount_nb$Year <- factor(predicted_ImmatureCount_nb$Year, 
                                         levels = c("2009", "2010", "2011", "2012", "2013", "2015"))

Lupin_Ratio_2009_2015_2 <- as.data.frame(Lupin_Ratio_2009_2015)

Lupin_Ratio_2009_2015_2$Year <- factor(Lupin_Ratio_2009_2015_2$Year, 
                                       levels = c("2009", "2010", "2011", "2012", "2013", "2015"))


# Use ggplot to create a boxplot of the model data
boxplot_Immature_Count <- ggplot(predicted_ImmatureCount_nb, 
                                      aes(x = Year, y = predicted, fill = Treatment)) +
  geom_boxplot() +
  geom_point(data = Lupin_Ratio_2009_2015_2, 
             aes(x = Year, y = Count_I),
             shape = 21, color = "black", size = 2,
             position = position_dodge(width = .75),
             alpha = 0.7) +
  geom_vline(xintercept = 2.5, 
             linetype = "dashed", 
             color = "black", 
             linewidth = 1) +
  labs(x = "Year", 
       y = "Count of Immature Lupine",
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
    legend.position = "none", #c(0.079,0.894),
    panel.border = element_rect(color = "black", fill = NA, size = .7),
    panel.grid.major = element_line(color = "black", linewidth = 0.2),
    panel.grid.minor = element_line(color = "black", linewidth = 0.2)
  ) 


# View graph
boxplot_Immature_Count



lupine_plots <- boxplot_2015Census_nb + boxplot_Immature_Count


# # FOR SAVING GRAPHS # #

file_path <- file.path(Sys.getenv("HOME"), "Downloads", "LupinePlots.png")
#
# # Save the plot using ggsave
ggsave(file_path, plot = lupine_plots,
       width = 15, height = 7,   # Set desired width and height in inches
       dpi = 300,               # Set the resolution (300 DPI for high quality)
       units = "in",            # Set units to inches
       device = "png")
