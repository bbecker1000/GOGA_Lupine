library(tidyverse)

power_with_census <- read_csv("Data/power_with_census.csv")

power_with_census$test2 <- paste(power_with_census$Type, 
                                 power_with_census$Fixed_Effect, 
                                 sep = "_")

# Retitle the facet wrap labels
new_labels <- c("Census" = "Lupine Count (Census)", 
                "Lupine" = "Lupine Cover (Transect)", 
                "Shrub" = "Shrub Cover (Transect)",
                "Native" = "Native Species Cover (Transect)",
                "Invasive" = "Invasive Species Cover (Transect)",
                "Immature" = "Immature Lupine Cover (Census)"
                )

# Create the forest plot
power_forestplot <- ggplot(power_with_census, 
                           aes(x = test2, y = Mean, ymin = Lower_CI, ymax = Upper_CI)) +
  geom_pointrange(color = "blue") +  # Add points and CI ranges
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +  # Reference line at 0
  #coord_flip() +  # Flip the coordinates for a vertical forest plot
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 15),
        axis.text.y = element_text(color = "black", size = 15),
        axis.title.x = element_text(color = "black", size = 20, face = "bold",
                                    margin = margin(t = 15, b = 15)),
        axis.title.y = element_text(color = "black", size = 20, face = "bold",
                                    margin = margin(l = 15, r = 15)), 
        strip.text = element_text(color = "black", size = 15, face = "bold"),
        #title = element_text(face = "bold", size = 15)
        ) +
  facet_wrap(~Group, ncol = 1, labeller = as_labeller(new_labels)) +
  scale_x_discrete(labels = c("2010_BurnAfter" = "2010 Burn",
                              "2010_MechAfter" = "2010 Mech",
                              "2011_BurnAfter" = "2011 Burn",
                              "2011_MechAfter" = "2011 Mech",
                              "2012_BurnAfter" = "2012 Burn",
                              "2012_MechAfter" = "2012 Mech",
                              "2013_BurnAfter" = "2013 Burn",
                              "2013_MechAfter" = "2013 Mech",
                              "PrePost_BurnAfter" = "Pre & Post Burn",
                              "PrePost_MechAfter" = "Pre & Post Mech",
                              "PrePost_Rain" = "Pre & Post Rain"
                              )) +
  labs(#title = "Forest Plot of Power Analysis Results",
       x = "Fixed Effect",
       y = "Power")

power_forestplot

# Make a graph that is not facetted

# Create the forest plot
power_forestplot_color <- ggplot(power_with_census, 
          aes(x = test2, y = Mean, ymin = Lower_CI, ymax = Upper_CI, color = Group)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +  # Add points and CI ranges
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +  # Reference line at 0
  #coord_flip() +  # Flip the coordinates for a vertical forest plot
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 15),
        axis.text.y = element_text(color = "black", size = 15),
        axis.title.x = element_text(color = "black", size = 18, face = "bold", 
                                    margin = margin(t = 15, b = 15)),
        axis.title.y = element_text(color = "black", size = 18, face = "bold", 
                                    margin = margin(l = 15, r = 15)), 
        strip.text = element_text(color = "black", size = 15, face = "bold"),
        #title = element_text(face = "bold", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", face = "bold", size = 18)
  ) +
  labs(#title = "Forest Plot of Power Analysis Results",
       x = "Fixed Effect",
       y = "Estimated Mean (with 95% CI)")

power_forestplot_color 

file_path <- file.path(Sys.getenv("HOME"), "Downloads", "power_forestplot_all.png")
#
# # Save the plot using ggsave
ggsave(file_path, plot = power_forestplot,
       width = 15, height = 10,   # Set desired width and height in inches
       dpi = 300,               # Set the resolution (300 DPI for high quality)
       units = "in",            # Set units to inches
       device = "png")


