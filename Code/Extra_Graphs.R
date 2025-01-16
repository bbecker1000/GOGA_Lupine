source("Code/1a_alldata.R")

# Graph of Precipitation in cm by year
Precip_by_Year <- ggplot(CL_Complete, aes(x = Year, y = yearly_rain)) +
  geom_point() +
  labs(y = "Precipitation (cm)") +
  theme_bw()

# View the graph
Precip_by_Year

# Recreating Figure 2 from the Poster

source("Code/LupinCensus.R")

# Make year a factor
Lupin_Census$Year.F <- as.factor(Lupin_Census$Year)

# The poster only shows the years 2010, 2011, and 2013
data_F2 <- filter(Lupin_Census, Year.F %in% c("2010", "2011", "2013"))

# Make graph
Figure2 <- ggplot(data_F2, aes(x = Treatment, y = RowCount, fill = Year.F)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("white", "lightblue", "green4")) +
  labs(y = "Lupine Count",
       fill = "Year") +
  theme()

#View graph
Figure2

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






