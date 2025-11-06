source("Code_with_2015/0a_UpdatedTransectData_Setup.R")


# Find the rainfall rnage within the study period
Precip_filtered <- Precip_cm %>%
  filter(Year %in% c("2009", "2010", "2011", "2012", "2013", "2015"))

max(Precip_filtered$yearly_rain, na.rm = TRUE)
min(Precip_filtered$yearly_rain, na.rm = TRUE)

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





source("Code_with_2015/3a_TransectGLMER_Setup.R")

Avg_Cover_Shrubs <- Shrubs_data_2015 %>%
  mutate(Cover = Total_Shrubs/Total_Count) %>%
  group_by(Year, Treatment) %>%
  summarize(mean_shrub_cover = mean(Cover, na.rm = TRUE), 
            min_shrub_cover = min(Cover, na.rm = TRUE),
            max_shrub_cover = max(Cover, na.rm = TRUE),
            .groups = "keep")
  
 
Avg_Cover_Shrubs$mean_shrub_cover <- round(Avg_Cover_Shrubs$mean_shrub_cover, 2) * 100
Avg_Cover_Shrubs$min_shrub_cover <- round(Avg_Cover_Shrubs$min_shrub_cover, 2) * 100
Avg_Cover_Shrubs$max_shrub_cover <- round(Avg_Cover_Shrubs$max_shrub_cover, 2) *100



# Cover of native summary table
Avg_Cover_Invasive <- Invasive_data_2015 %>%
  mutate(Cover = Total_Native/Total_Count) %>%
  group_by(Year, Treatment) %>%
  summarize(mean_native_cover = mean(Cover, na.rm = TRUE), 
            median_native_cover = median(Cover, na.rm = TRUE),
            min_native_cover = min(Cover, na.rm = TRUE),
            max_native_cover = max(Cover, na.rm = TRUE),
            .groups = "keep")


Avg_Cover_Nativity$mean_native_cover <- round(Avg_Cover_Nativity$mean_native_cover, 2) * 100
Avg_Cover_Nativity$median_native_cover <- round(Avg_Cover_Nativity$median_native_cover, 2) * 100
Avg_Cover_Nativity$min_native_cover <- round(Avg_Cover_Nativity$min_native_cover, 2) * 100
Avg_Cover_Nativity$max_native_cover <- round(Avg_Cover_Nativity$max_native_cover, 2) *100
  

# Cover of invasives summary table
Avg_Cover_Invasive <- Invasive_data_2015 %>%
  mutate(Cover = Total_Invasive/Total_Count) %>%
  group_by(Year, Treatment) %>%
  summarize(mean_invasive_cover = mean(Cover, na.rm = TRUE), 
            median_invasive_cover = median(Cover, na.rm = TRUE),
            min_invasive_cover = min(Cover, na.rm = TRUE),
            max_invasive_cover = max(Cover, na.rm = TRUE),
            .groups = "keep")


Avg_Cover_Invasive$mean_invasive_cover <- round(Avg_Cover_Invasive$mean_invasive_cover, 2) * 100
Avg_Cover_Invasive$median_invasive_cover <- round(Avg_Cover_Invasive$median_invasive_cover, 2) * 100
Avg_Cover_Invasive$min_invasive_cover <- round(Avg_Cover_Invasive$min_invasive_cover, 2) * 100
Avg_Cover_Invasive$max_invasive_cover <- round(Avg_Cover_Invasive$max_invasive_cover, 2) *100





# CHECKING HOW RICHNESS CHANGES 

# Set up a dataframe that contains data for a model on nativity
NatHerb_2015_UniqueSpecies <- CLComplete_2015 %>%
  # 1. Filter for Native species that are Grass or Forb
  filter(Native == TRUE,
         Default_LF %in% c("Grass", "Forb")) %>%
  # 2. Group by all desired variables (including Native and Default_LF)
  group_by(Year,
           Site,
           Plot,
           Treatment,
           MacroPlot,
           yearly_rain,
           Native,
           Default_LF) %>%
  # 3. Summarize by counting the number of distinct species
  summarise(NatHerb_Richness = n_distinct(Species), .groups = "keep") %>%
  # 4. Remove the grouping structure
  ungroup()


# NATIVE HERB RICHNESS MODEL 

# Run binomial model
m_NatHerbRich_Year_2015 <- glmer(NatHerb_Richness ~ 
                                   Treatment *
                                   Year +  
                                   (1|Plot), 
                                 family = poisson,
                                 data = NatHerb_2015_UniqueSpecies)

# View model output
sum_NatHerbRich <- summary(m_NatHerbRich_Year_2015)


library(sjPlot)
plot_model(m_NatHerbRich_Year_2015, terms = c("Year", "Treatment"), type = "pred")


# Create a histogram of the unique species counts
ggplot(NatHerb_2015_UniqueSpecies, aes(x = NatHerb_Richness)) +
  geom_histogram(binwidth = 1, fill = "darkblue", color = "white") +
  labs(title = "Distribution of Unique Native Herb Species Counts",
       x = "Number of Unique Species",
       y = "Frequency (Number of Plots)") +
  theme_minimal()


# Boxplot of Native Herb richness
ggplot(NatHerb_2015_UniqueSpecies, aes(y = NatHerb_Richness, x = Year, fill = Treatment)) +
  geom_boxplot() +
  labs(x = "Year",
       y = "Native Herbaceous Richness") +
  theme_minimal()
