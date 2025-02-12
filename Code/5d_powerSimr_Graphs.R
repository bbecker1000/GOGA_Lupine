library(tidyverse)

power_with_census <- read_csv("Data/power_with_census.csv")

power_with_census$test2 <- paste(power_with_census$Type, 
                                 power_with_census$Fixed_Effect, 
                                 sep = "_")

# Create the forest plot
power_forestplot <- ggplot(power_with_census, 
                           aes(x = test2, y = Mean, ymin = Lower_CI, ymax = Upper_CI)) +
  geom_pointrange(color = "blue") +  # Add points and CI ranges
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +  # Reference line at 0
  #coord_flip() +  # Flip the coordinates for a vertical forest plot
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 15),
        axis.text.y = element_text(color = "black", size = 15),
        axis.title.x = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color = "black", size = 15, face = "bold"), 
        strip.text = element_text(color = "black", size = 15, face = "bold"),
        #title = element_text(face = "bold", size = 15)
        ) +
  facet_wrap(~Group, ncol = 1) +
  labs(#title = "Forest Plot of Power Analysis Results",
       x = "Fixed Effect",
       y = "Estimated Mean (with 95% CI)")

power_forestplot

# Make a graph that is not facetted

# Create the forest plot
power_forestplot_color <- ggplot(power_with_census, 
          aes(x = test2, y = Mean, ymin = Lower_CI, ymax = Upper_CI, color = Group)) +
  geom_pointrange() +  # Add points and CI ranges
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +  # Reference line at 0
  #coord_flip() +  # Flip the coordinates for a vertical forest plot
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 15),
        axis.text.y = element_text(color = "black", size = 15),
        axis.title.x = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color = "black", size = 15, face = "bold"), 
        strip.text = element_text(color = "black", size = 15, face = "bold"),
        #title = element_text(face = "bold", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", face = "bold", size = 15)
  ) +
  labs(#title = "Forest Plot of Power Analysis Results",
       x = "Fixed Effect",
       y = "Estimated Mean (with 95% CI)")

power_forestplot_color 

file_path <- file.path(Sys.getenv("HOME"), "Downloads", "power_forestplot2.png")
# 
# # Save the plot using ggsave
ggsave(file_path, plot = power_forestplot,
       width = 15, height = 10,   # Set desired width and height in inches
       dpi = 300,               # Set the resolution (300 DPI for high quality)
       units = "in",            # Set units to inches
       device = "png")


