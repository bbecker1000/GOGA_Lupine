library(tidyverse)
library(simr)


# Upload new data file
power_2015 <- read_csv("Data/power_2015.csv")

# Retitle the facet wrap labels
new_labels <- c("Census" = "Lupine Count (Census)", 
                "Lupine" = "Lupine Cover (Transect)", 
                "Shrub" = "Shrub Cover (Transect)",
                "Native" = "Native Species Cover (Transect)",
                "Invasive" = "Invasive Species Cover (Transect)",
                "Immature" = "Immature Lupine Cover (Census)"
                )


# Create the forest plot
power_forestplot_2015 <- ggplot(power_2015, 
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

power_forestplot_2015
