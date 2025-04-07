source("Code/1a_alldata.R")

# Graph of Precipitation in cm by year
Precip_by_Year <- ggplot(CL_Complete, aes(x = as.numeric(Year), y = yearly_rain)) +
  geom_line() +
  geom_point() +
  labs(y = "Precipitation (cm)",
       x = "Year") +
  ylim(0,60) +
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
Lupin_Census_spp <- read_csv("Data/Lupin_Individuals_AllYears.csv")

# Make year a factor
Lupin_Census_spp$Year.F <- as.factor(Lupin_Census_spp$Year)

# The poster only shows the years 2010, 2011, and 2013
data_F1 <- Lupin_Census_spp %>%
  filter(Year.F %in% c("2010", "2011", "2013")) %>%
  filter(Species == "LUAL") %>%
  filter(Status == "L") %>%
  filter(UV1 == "I")

data_F1_avg <- data_F1 %>%
  group_by(Treatment, Year.F) %>%
  summarize(mean = mean(RowCount, na.rm = TRUE), 
            sd = sd(RowCount, na.rm = TRUE),
            .groups = "keep")

# Set levels to match figure 1 in poster
data_F3_avg$Treatment <- factor(data_F3_avg$Treatment, 
                                levels = c("BURN", "CONTROL", "MECHANICAL"))

# Make graph
Figure1 <- ggplot(data_F1_avg, aes(x = Treatment, y = mean, fill = Year.F)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("white", "lightblue", "green4")) +
  labs(y = "Average #",
       fill = "Year") +
  scale_y_continuous(limits = c(0, 50)) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20), # Bold x-axis label
    axis.title.y = element_text(face = "bold", size = 20), # Bold y-axis label
    axis.text = element_text(size = 17),
    legend.title = element_text(face = "bold", size = 19), # Bold legend title
    legend.text = element_text(face = "plain", size = 17) # Lowercase text
  )

#View graph
Figure1


# Recreate Figure 2 from the Poster

# Make year a factor
Lupin_data$Year <- as.factor(Lupin_data$Year)

# The poster only shows the years 2010, 2011, and 2013
data_F2 <- filter(Lupin_data, Year %in% c("2010", "2011", "2013")) %>%
  mutate("Cover" = (Total_Lupin/Total_Count)*100)

data_F2_avg <- data_F2 %>%
  group_by(Treatment, Year) %>%
  summarize(mean = mean(Cover, na.rm = TRUE), 
            sd = sd(Cover, na.rm = TRUE),
            .groups = "keep")

# Set levels to match figure 2 in poster
data_F2_avg$Treatment <- factor(data_F2_avg$Treatment, 
                                levels = c("BURN", "CONTROL", "MECHANICAL"))

# Make graph
Figure2 <- ggplot(data_F2_avg, aes(x = Treatment, y = mean, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("white", "lightblue", "green4")) +
  labs(y = "Percent Cover Lupine",
       fill = "Year") +
  scale_y_continuous(limits = c(0, 10)) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20), # Bold x-axis label
    axis.title.y = element_text(face = "bold", size = 20), # Bold y-axis label
    axis.text = element_text(size = 17),
    legend.title = element_text(face = "bold", size = 19), # Bold legend title
    legend.text = element_text(face = "plain", size = 17) # Lowercase text
  )

#View graph
Figure2




# Recreate Figure 3 from the Poster

source("Code/1c_Data_setup_GLMER.R")

# Make year a factor
Nativity_data$Year <- as.factor(Nativity_data$Year)

# The poster only shows the years 2010, 2011, and 2013
data_F3 <- filter(Nativity_data, Year %in% c("2010", "2011", "2013")) %>%
  mutate("Cover" = (Total_Native/Total_Count)*100)

data_F3_avg <- data_F3 %>%
  group_by(Treatment, Year) %>%
  summarize(mean = mean(Cover, na.rm = TRUE), 
            sd = sd(Cover, na.rm = TRUE),
            .groups = "keep")

# Set levels to match figure 3 in poster
data_F3_avg$Treatment <- factor(data_F3_avg$Treatment, 
                                 levels = c("BURN", "CONTROL", "MECHANICAL"))

# Make graph
Figure3 <- ggplot(data_F3_avg, aes(x = Treatment, y = mean, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("white", "lightblue", "green4")) +
  labs(y = "Percent Cover Native",
       fill = "Year") +
  scale_y_continuous(limits = c(0, 100)) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title.x = element_text(face = "bold", size = 20), # Bold x-axis label
    axis.title.y = element_text(face = "bold", size = 20), # Bold y-axis label
    axis.text = element_text(size = 17),
    legend.title = element_text(face = "bold", size = 19), # Bold legend title
    legend.text = element_text(face = "plain", size = 17) # Lowercase text
  )

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
