library(tidyverse)
library(simr)


# Upload new data file
power_2015 <- read_csv("Data/power_2015.csv")

# New graph without lupine cover from LPI transect
power_2015_filtered <- power_2015 %>%
  filter(Group != "Lupine")

# Re-title the facet wrap labels for clarity
new_facet_labels <- c("Census" = "Lupine Count (Census)", 
                "Lupine" = "Lupine Cover (Transect)", 
                "Shrub" = "Shrub Cover (Transect)",
                "Native" = "Native Species Cover (Transect)",
                "Invasive" = "Invasive Species Cover (Transect)",
                "Immature" = "Immature Lupine Count (Census)",
                "Census" = "Lupine Count (Census)")



# Clean up the the x-axis labels
new_x_labels <- c("2009_BurnBefore" = "2009 Burn",
                  "2009_MechBefore" = "2009 Mech",
                  "2011_BurnAfter" = "2011 Burn",
                  "2011_MechAfter" = "2011 Mech",
                  "2012_BurnAfter" = "2012 Burn",
                  "2012_MechAfter" = "2012 Mech",
                  "2013_BurnAfter" = "2013 Burn",
                  "2013_MechAfter" = "2013 Mech",
                  "2015_BurnAfter" = "2015 Burn",
                  "2015_MechAfter" = "2015 Mech",
                  "PrePost_BurnAfter" = "Pre & Post Burn",
                  "PrePost_MechAfter" = "Pre & Post Mech",
                  "PrePost_Rainfall" = "Pre & Post Rain")


# Create the forest plot
power_forestplot_2015 <- ggplot(power_2015_filtered, 
  aes(x = test2, y = Mean, ymin = Lower_CI, ymax = Upper_CI)) +
  geom_pointrange(color = "blue") + 
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +  
  facet_wrap(~Group, ncol = 1, labeller = as_labeller(new_facet_labels)) +
  labs(x = "Fixed Effect",
       y = "Power") +
  scale_x_discrete(labels = new_x_labels) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 15),
        axis.text.y = element_text(color = "black", size = 15),
        axis.title.x = element_text(color = "black", size = 20, face = "bold",
                                    margin = margin(t = 15, b = 15)),
        axis.title.y = element_text(color = "black", size = 20, face = "bold",
                                    margin = margin(l = 15, r = 15)), 
        strip.text = element_text(color = "black", size = 15, face = "bold"))



# View the graph
power_forestplot_2015






# FOR SAVING GRAPHS

file_path_power <- file.path(Sys.getenv("HOME"), "Downloads", "power_forestplot_2015.png")

# Save the plot using ggsave
ggsave(file_path_power, plot = power_forestplot_2015,
       width = 14, height = 10, 
       dpi = 300,
       units = "in",          
       device = "png")
