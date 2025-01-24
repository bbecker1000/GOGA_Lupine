source("Code/1a_alldata.R")

# Graph of Precipitation in cm by year
Precip_by_Year <- ggplot(CL_Complete, aes(x = as.numeric(Year), y = yearly_rain)) +
  geom_line() +
  geom_point() +
  labs(y = "Precipitation (cm)",
       x = "Year") +
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
Precip_by_Year


# Recreating Figure 1 from the Poster

# Upload data

# Make year a factor
Lupin_Census$Year.F <- as.factor(Lupin_Census_spp$Year)

# The poster only shows the years 2010, 2011, and 2013
data_F1 <- filter(Lupin_Census_spp, Year.F %in% c("2010", "2011", "2013")) %>%
  filter(Lupin_Census_spp, Species == "LUAL")

# Make graph
Figure1 <- ggplot(data_F2, aes(x = Treatment, y = RowCount, fill = Year.F)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("white", "lightblue", "green4")) +
  labs(y = "Lupine Count",
       fill = "Year") +
  theme()

#View graph
Figure1




# Recreate Figure 3 from the Poster

source("Code/1c_Data_setup_GLMER.R")

# Make year a factor
Nativity_data$Year <- as.factor(Nativity_data$Year)

# The poster only shows the years 2010, 2011, and 2013
data_F3 <- filter(Nativity_data, Year %in% c("2010", "2011", "2013")) %>%
  mutate("Cover" = (Total_Native/Total_Count)*100)

# Make graph
Figure3 <- ggplot(data_F3, aes(x = Treatment, y = Cover, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("white", "lightblue", "green4")) +
  labs(y = "Percent Cover Native",
       fill = "Year") +
  theme()

#View graph
Figure3



# FOR SAVING GRAPHS

file_path <- file.path(Sys.getenv("HOME"), "Downloads", "Precip_by_Year.png")
# 
# # Save the plot using ggsave
ggsave(file_path, plot = Precip_by_Year,
       width = 10, height = 7,   # Set desired width and height in inches
       dpi = 300,               # Set the resolution (300 DPI for high quality)
       units = "in",            # Set units to inches
       device = "png")
