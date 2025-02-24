source("Code/1a_alldata.R")

# # Data set up for an NMS that contains all species

# Get data to have the total count of species for each macroplot and year
sum_allspp <- CL_Complete %>%
  group_by(MacroPlot,
           Year,
           Trt_Status,
           Site,
           Plot,
           Treatment,
           yr_trt,
           yearly_rain, 
           Time_Since_Trt,
           Species) %>%
  summarise(Total_Count = sum(Count), .groups = "keep") %>%
  ungroup()

# Pivot the data wider
wide_data_allspp <- sum_allspp %>%
  pivot_wider(names_from = Species, 
              values_from = Total_Count)

# Replace NAs with 0
wide_data_allspp[is.na.data.frame(wide_data_allspp)] <- 0

# Create a column that has the treatment status and treatment
wide_data_allspp$Trt_trt_Status <- paste(wide_data_allspp$Trt_Status,
                                         "-",
                                         wide_data_allspp$Treatment)

# Make sure all rows >0
wide_data_allspp.test <- wide_data_allspp %>%
  rowwise() %>% 
  mutate(TOTAL = rowSums(across(where(is.numeric)))) %>%
  select(TOTAL)

# view(wide_data_allspp.test)

# Remove all data this not species counts
wide_data_allspp.nms <- wide_data_allspp %>%
    select(-MacroPlot, 
           -Year, 
           -Trt_Status, 
           -Site, 
           -Plot, 
           -Treatment,
           -yr_trt,
           -yearly_rain,
           -Trt_trt_Status,
           -Time_Since_Trt)

# saveRDS(wide_data_allspp.nms, file = "wide_data_allspp.nms")
  
# Create a dateframe with only the plot and environmental data 
data_plot_allspp <- wide_data_allspp %>%
  select(Site, 
         Treatment,
         Trt_Status,
         yearly_rain,
         Time_Since_Trt)

# # Data set up for an NMS that contains species groupings
CL_Complete$Lifecycle[is.na.data.frame(CL_Complete$Lifecycle)] <- "Not Defined"


CLComplete <- CL_Complete %>% 
  mutate(Nativity = case_when(
    Native == "TRUE" ~ "Native",
    Invasive == "TRUE" ~ "Invasive",
    Native == "FALSE" & Invasive == "FALSE" ~ "Introduced")) %>%
  mutate(spp_groupings = paste0(Nativity, " ",
                                substr(Lifecycle, 1, 1),
                                substr(Default_LF, 1, 1)
                                ))

CLComplete$spp_groupings <- ifelse(CLComplete$Species == "LUAL", "Lupine",
                                   ifelse(CLComplete$Species == "LUVA", "Lupine", 
                                          ifelse(CLComplete$Species == "LUPINa", "Lupine",
                                                 CLComplete$spp_groupings)))
  
# Get data to have the total count of species groups for each macroplot and year
sum_groupings <- CLComplete %>%
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

# view(wide_data_groupings.test)

# Remove all data this not species counts
wide_data_groupings.nms <- wide_data_groupings %>%
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

# saveRDS(wide_data_groupings.nms, file = "wide_data_groupings.nms")

# Create a dateframe with only the plot and environmental data 
data_plot_groupings <- wide_data_groupings %>%
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

view(CLComplete)
