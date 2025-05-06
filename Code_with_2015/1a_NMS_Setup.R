source("Code_with_2015/0a_UpdatedTransectData_Setup.R")

# # Data set up for an NMS that contains species groupings

# Create a new column with the descriptive name of species groups
CLComplete_2015 <- CL_Complete_2015 %>%
  mutate(
    Nativity = case_when(
      Native == "TRUE" ~ "Native",
      Invasive == "TRUE" ~ "Invasive",
      Native == "FALSE" & Invasive == "FALSE" ~ "Introduced"
    )
  ) %>%
  mutate(
    spp_groupings = case_when(
      Default_LF == "substrate" ~ "Substrate",
      TRUE ~ paste0(Nativity, " ",
                    substr(Lifecycle, 1, 1),
                    substr(Default_LF, 1, 1))
    )
  )

# Group all Lupine species together
CLComplete_2015$spp_groupings <- ifelse(CLComplete_2015$Species == "LUAL", "Lupine",
                                   ifelse(CLComplete_2015$Species == "LUVA", "Lupine", 
                                          ifelse(CLComplete_2015$Species == "LUPINa", "Lupine",
                                                 CLComplete_2015$spp_groupings)))


# Calculate the total count of species groups for each macroplot and year
sum_groupings_2015 <- CLComplete_2015 %>%
  group_by(MacroPlot,
           Year,
           Trt_Status,
           Site,
           Plot,
           Treatment,
           yr_trt,
           yearly_rain,
           Time_Since_Trt,
           spp_groupings) %>%
  summarise(Total_Count = sum(Count), .groups = "keep") %>%
  ungroup()

# Pivot the data wider
wide_data_groupings_2015 <- sum_groupings_2015 %>%
  pivot_wider(names_from = spp_groupings, 
              values_from = Total_Count)

# Replace NAs with 0
wide_data_groupings_2015[is.na.data.frame(wide_data_groupings_2015)] <- 0

# Make sure all rows >0
wide_data_groupings_2015.test <- wide_data_groupings_2015 %>%
  rowwise() %>% 
  mutate(TOTAL = rowSums(across(where(is.numeric)))) %>%
  select(TOTAL)

# view(wide_data_groupings.test)

# Remove all data this not species counts
wide_data_groupings_2015.nms <- wide_data_groupings_2015 %>%
  select(
    -MacroPlot, 
    -Year, 
    -Trt_Status, 
    -Site, 
    -Plot, 
    -Treatment,
    -yr_trt,
    -yearly_rain,
    -Time_Since_Trt
  )


# Create a dateframe with only the plot and environmental data 
data_plot_groupings_2015 <- wide_data_groupings_2015 %>%
  select(
    MacroPlot, 
    Year, 
    Trt_Status, 
    Site, 
    Plot, 
    Treatment,
    yr_trt,
    yearly_rain,
    Time_Since_Trt
  )

# scale yearly rainfall
data_plot_groupings_2015$yearly_rain <- scale(data_plot_groupings_2015$yearly_rain)

data_plot_groupings_2015$Trt_Status <- factor(data_plot_groupings_2015$Trt_Status, 
                                    levels = c("before", "after"))

data_plot_groupings_2015$Year <- factor(data_plot_groupings_2015$Year, 
                                        levels = c("2010", "2009", "2011", "2012", "2013", "2015"))

