source("Code/1a_alldata.R")

# # Data set up for an NMS that contains all species

# Get data to have the total count of species for each macroplot and year
sum_allspp <- CL_Complete %>%
  group_by(MacroPlot,
           Year,
           Site,
           Plot,
           Treatment,
           yr_trt,
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
wide_data_allspp.nms <- as.matrix(wide_data_allspp[,-c(1:7)])

# saveRDS(wide_data_allspp.nms, file = "wide_data_allspp.nms")
  
# Create a dateframe with only the plot and environmental data 
data_plot_allspp <- wide_data_allspp[,c(1:7)]

# Create a dataframe with only the covariates I want in the envfit
data_env_allspp <- wide_data_allspp[,c(2,5,7)]
data_env_allspp$Year.numeric <- as.numeric(data_env_allspp$Year)
data_env_allspp$Time_Since_Treatment <- paste(data_env_allspp$Year.numeric - 2010)
data_env_allspp$Time_Since_Treatment <- as.numeric(data_env_allspp$Time_Since_Treatment)
data_env_allspp_final <- data_env_allspp[,c(1,2,3,5)]


# saveRDS(data_plot_allspp, file = "data_plot_allspp")

# # Data set up for an NMS that contains species groupings

CLComplete <- CL_Complete %>% 
  mutate(spp_groupings = paste(Lifecycle, 
                               "_", 
                               Default_LF,
                               "-",
                               Native))

# Get data to have the total count of species groups for each macroplot and year
sum_groupings <- CLComplete %>%
  group_by(MacroPlot,
           Year,
           Site,
           Plot,
           Treatment,
           yr_trt,
           yearly_rain, 
           spp_groupings) %>%
  summarise(Total_Count = sum(Count), .groups = "keep")

# Pivot the data wider
wide_data_groupings <- sum_groupings %>%
  pivot_wider(names_from = spp_groupings, 
              values_from = Total_Count)

# Replace NAs with 0
wide_data_groupings[is.na.data.frame(wide_data_groupings)] <- 0

# Make sure all rows >0
wide_data_groupings.test <- wide_data_groupings %>%
  rowwise() %>% 
  mutate(TOTAL = rowSums(across(where(is.numeric)))) %>%
  select(TOTAL)

# Remove all data this not species counts
wide_data_groupings.nms <- wide_data_groupings[,-c(1:7)]

# saveRDS(wide_data_allspp.nms, file = "wide_data_allspp.nms")

# Create a dateframe with only the plot and environmental data 
data_plot_groupings <- wide_data_groupings[,c(1:7)]

# Create a dataframe with only the covariates I want in the envfit
data_env_groupings <- wide_data_groupings[,c(2,5,7)]
data_env_groupings$Year.numeric <- as.numeric(data_env_groupings$Year)
data_env_groupings$Time_Since_Treatment <- paste(data_env_groupings$Year.numeric - 2010)
data_env_groupings$Time_Since_Treatment <- as.numeric(data_env_groupings$Time_Since_Treatment)
data_env_groupings_final <- data_env_groupings[,c(1,2,3,5)]

# Add a new variable in the plot data that is year and treatment
data_plot_groupings$yr_trt <- paste(data_plot_groupings$Year, 
                                 "_", 
                                 data_plot_groupings$Treatment)

