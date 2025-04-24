source("Code_with_2015/0a_UpdatedTransectData_Setup.R")

# Graph of Precipitation in cm by year
Precip_2015 <- ggplot(Precip_cm, aes(x = as.numeric(Year), y = yearly_rain)) +
  geom_line() +
  geom_point() +
  labs(y = "Average Annual Precipitation (cm)",
       x = "Year") +
  ylim(0,60) +
  scale_x_continuous(
    limits = c(2009, 2015),
    breaks  = 2009:2015, 
    labels  = 2009:2015) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20), # Bold x-axis label
    axis.title.y = element_text(face = "bold", size = 20), # Bold y-axis label
    axis.text = element_text(size = 17),
    legend.title = element_text(face = "bold", size = 19), # Bold legend title
    legend.text = element_text(face = "plain", size = 17) # Lowercase text
  )


# View the graph
Precip_2015


# # FOR SAVING GRAPHS # #

file_path <- file.path(Sys.getenv("HOME"), "Downloads", "Precip_2015.png")
#
# # Save the plot using ggsave
ggsave(file_path, plot = Precip_2015,
       width = 10, height = 7,   # Set desired width and height in inches
       dpi = 300,               # Set the resolution (300 DPI for high quality)
       units = "in",            # Set units to inches
       device = "png")
