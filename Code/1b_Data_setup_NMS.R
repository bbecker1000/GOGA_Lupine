# # Data set up for an NMS that contains all species

# Get data to have the total count of species for each macroplot and year
sum_allspp <- CL_Complete %>%
  group_by(Year,
           Site,
           Plot,
           Treatment,
           MacroPlot,
           yearly_rain, 
           Species) %>%
  summarise(Total_Count = sum(Count), .groups = "keep")

# Pivot the data wider
wide_data_allspp <- sum_allspp %>%
  pivot_wider(names_from = Species, 
              values_from = Total_Count)

# Replace NAs with 0
wide_data_allspp[is.na.data.frame(wide_data_allspp)] <- 0

# Make sure all rows >0
wide_data_allspp.test <- wide_data_allspp %>%
  rowwise() %>% 
  mutate(TOTAL = rowSums(across(where(is.numeric)))) %>%
  select(TOTAL)

# Remove all data this not species counts
wide_data_allspp.nms <- wide_data_allspp[,-c(1:6)]

# saveRDS(wide_data_allspp.nms, file = "wide_data_allspp.nms")
  
# Create a dateframe with only the plot and environmental data 
data_plot_allspp <- wide_data_allspp[,c(1:6)]

# Add a new variable in the plot data that is year and treatment
data_plot_allspp$yr_trt <- paste(data_plot_allspp$Year, 
                                 "_", 
                                 data_plot_allspp$Treatment)

# saveRDS(data_plot_allspp, file = "data_plot_allspp")

# # Set up data file for an NMS that contains species groups
data.spp1 <- CL_Complete %>%
  select(Species, Lifecycle, Preferred_LF, Default_LF, Native, Invasive)

data.spp <- data.spp1[!duplicated(data.spp1),]

# saveRDS(data.spp, file = "data.spp")
