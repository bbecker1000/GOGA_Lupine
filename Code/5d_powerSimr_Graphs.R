library(tidyverse)

power_df <- read_csv("Data/power_df.csv")
# View(power_analysis_results)

power_df$test2 <- paste(power_df$Type, power_df$Fixed_Effect, sep = "_")

# Create the forest plot
power_forestplot <- ggplot(power_df, aes(x = test2, y = Mean, ymin = Lower_CI, ymax = Upper_CI)) +
  geom_pointrange(color = "blue") +  # Add points and CI ranges
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Reference line at 0
  #coord_flip() +  # Flip the coordinates for a vertical forest plot
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, color = "black", size = 15),
        axis.text.y = element_text(color = "black", size = 15),
        axis.title.x = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color = "black", size = 15, face = "bold"), 
        strip.text = element_text(color = "black", size = 15, face = "bold"),
        title = element_text(face = "bold", size = 15)
        ) +
  facet_wrap(~Group, ncol = 1) +
  labs(title = "Forest Plot of Power Analysis Results",
       x = "Fixed Effect",
       y = "Estimated Mean (with 95% CI)")

power_forestplot

file_path <- file.path(Sys.getenv("HOME"), "Downloads", "power_forestplot.png")
# 
# # Save the plot using ggsave
ggsave(file_path, plot = power_forestplot,
       width = 20, height = 10,   # Set desired width and height in inches
       dpi = 300,               # Set the resolution (300 DPI for high quality)
       units = "in",            # Set units to inches
       device = "png")


