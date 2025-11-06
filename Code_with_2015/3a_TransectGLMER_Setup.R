source("Code_with_2015/0a_UpdatedTransectData_Setup.R")
source("Code_with_2015/1a_NMS_Setup.R")



# Create a table that has the macroplot, the year, and the total detections
total_detections_2015 <- CLComplete_2015 %>%
  group_by(Year, MacroPlot) %>%
  summarise(Total_Count = sum(Count), .groups = "keep") %>%
  ungroup()


# FOR LUPINE DATA # # # # # # # # # # # # # # # #

# Calculate the total count of species groups for each macroplot and year
sum_allspp_2015 <- CLComplete_2015 %>%
  group_by(MacroPlot,
           Year,
           Trt_Status,
           Site,
           Plot,
           Treatment,
           yearly_rain,
           Species) %>%
  summarise(Total_Count = sum(Count), .groups = "keep") %>%
  ungroup()

# Need to make implicitly missing data explicitly missing ie count = 0 
# Otherwise when we create the data set Lupin_2015 we have missing plots
sum_allspp_zeros_2015 <- sum_allspp_2015 %>%
  ungroup() %>%
  complete(nesting(Year, Trt_Status, Site, Plot, Treatment, MacroPlot, yearly_rain), 
           Species,
           fill = list(Total_Count = 0))


# now we can get lupin counts even including macroplots with the count of zero
Lupin_2015 <- sum_allspp_zeros_2015 %>%
  filter(Species %in% c("LUAL", "LUFO", "LUVA")) %>%
  group_by(Year,
           Trt_Status,
           Site,
           Plot,
           Treatment,
           MacroPlot,
           yearly_rain) %>%
  summarise(Total_Lupin = sum(Total_Count), .groups = "keep")


# we want a dataset that includes total detections for each macroplot in each year
Lupin_data_2015 <- merge(x = Lupin_2015, y = total_detections_2015, 
                    by.x = c("MacroPlot", "Year"),
                    by.y = c("MacroPlot", "Year"), 
                    all = TRUE)


# set Control as base level
Lupin_data_2015$Treatment <- factor(Lupin_data_2015$Treatment, 
                          levels = c("Control", "Burn", "Mechanical"))

# set Pre-treatment as base level
Lupin_data_2015$Trt_Status <- factor(Lupin_data_2015$Trt_Status, 
                                     levels = c("before", "after"))

# Set 2010 as the baseline year
Lupin_data_2015$Year <- factor(Lupin_data_2015$Year, 
                                     levels = c("2010", "2009", "2011", "2012", "2013", "2015"))




# FOR NATIVE DATA #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #

# Set up a dataframe that contains data for a model on nativity
Nativity_2015 <- CLComplete_2015 %>%
  filter(Native == TRUE) %>%
  group_by(Year,
           Trt_Status,
           Site,
           Plot,
           Treatment,
           MacroPlot,
           yearly_rain,
           Native) %>%
  summarise(Total_Native = sum(Count), .groups = "keep")

# Add in total counts for each plot
Nativity_data_2015 <- merge(x = Nativity_2015, y = total_detections_2015, 
                            by.x = c("MacroPlot", "Year"),
                            by.y = c("MacroPlot", "Year"), 
                            all = TRUE)

# set Control as base level
Nativity_data_2015$Treatment <- factor(Nativity_data_2015$Treatment, 
                             levels = c("Control", "Burn", "Mechanical"))

# set Pre-treatment as base level
Nativity_data_2015$Trt_Status <- factor(Nativity_data_2015$Trt_Status, 
                              levels = c("before", "after"))

# Set year levels so that 2010 is treated as baseline
Nativity_data_2015$Year <- factor(Nativity_data_2015$Year, 
                               levels = c("2010", "2009", "2011", "2012", "2013", "2015"))



# FOR INVASIVE DATA #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #

# Set up a dataframe that contains data for a model on invasives
Invasive_2015 <- CLComplete_2015 %>%
  filter(Invasive == TRUE) %>%
  group_by(Year,
           Trt_Status,
           Site,
           Plot,
           Treatment,
           MacroPlot,
           yearly_rain, 
           Invasive) %>%
  summarise(Total_Invasive = sum(Count), .groups = "keep")


# we want a dataset that includes total detections for each macroplot in each year
Invasive_data_2015 <- merge(x = Invasive_2015, y = total_detections_2015, 
                       by.x = c("MacroPlot", "Year"),
                       by.y = c("MacroPlot", "Year"), 
                       all = TRUE)

# set Control as base level
Invasive_data_2015$Treatment <- factor(Invasive_data_2015$Treatment, 
                             levels = c("Control", "Burn", "Mechanical"))

# set Pre-treatment as base level
Invasive_data_2015$Trt_Status <- factor(Invasive_data_2015$Trt_Status, 
                              levels = c("before", "after"))

# Set year levels so that 2010 is treated as baseline
Invasive_data_2015$Year <- factor(Invasive_data_2015$Year, 
                                  levels = c("2010", "2009", "2011", "2012", "2013", "2015"))



# FOR SHRUB DATA #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #

# Set up a dataframe that contains data for a model on shrubs
Shrubs_2015 <- CLComplete_2015 %>%
  filter(Default_LF == "Shrub") %>%
  group_by(Year,
           Trt_Status,
           Site,
           Plot,
           Treatment,
           MacroPlot,
           yearly_rain, 
           Default_LF) %>%
  summarise(Total_Shrubs = sum(Count), .groups = "keep") %>%
  ungroup()


total_detections_2015_2 <- CLComplete_2015 %>%
  group_by(Year, Trt_Status, Site, Plot, Treatment, MacroPlot, yearly_rain) %>%
  summarise(Total_Count = sum(Count), .groups = "keep") %>%
  ungroup()

# Step 3: Merge them, and fill NAs in Total_Shrubs with 0
Shrubs_data_2015 <- total_detections_2015_2 %>%
  left_join(Shrubs_2015 %>% select(-Trt_Status, -Site, -Plot, -Treatment, -yearly_rain, -Default_LF), 
            by = c("MacroPlot", "Year")) %>%
  mutate(
    Total_Shrubs = replace_na(Total_Shrubs, 0)
  )

# set Control as base level
Shrubs_data_2015$Treatment <- factor(Shrubs_data_2015$Treatment, 
                           levels = c("Control", "Burn", "Mechanical"))

# set Pre-treatment as base level
Shrubs_data_2015$Trt_Status <- factor(Shrubs_data_2015$Trt_Status, 
                            levels = c("before", "after"))

# Set year levels so that 2010 is treated as baseline
Shrubs_data_2015$Year <- factor(Shrubs_data_2015$Year, 
                                  levels = c("2010", "2009", "2011", "2012", "2013", "2015"))



# NATIVE HERB #  #   #   #   #   #   #   #   #   #   #   #   #   #   #   #

# Set up a dataframe that contains data for a model on nativity
NatHerb_2015 <- CLComplete_2015 %>%
  filter(Native == TRUE,
         Default_LF %in% c("Grass", "Forb")) %>%
  group_by(Year,
           Site,
           Plot,
           Treatment,
           MacroPlot,
           yearly_rain,
           Native,
           Default_LF) %>%
  summarise(Total_NatHerb = sum(Count), .groups = "keep") %>%
  ungroup()


NatHerb_data_2015 <- merge(x = NatHerb_2015, y = total_detections_2015, 
                            by.x = c("MacroPlot", "Year"),
                            by.y = c("MacroPlot", "Year"), 
                            all = TRUE)


# set Control as base level
NatHerb_data_2015$Treatment <- factor(NatHerb_data_2015$Treatment, 
                                       levels = c("Control", "Burn", "Mechanical"))


# Set year levels so that 2010 is treated as baseline
NatHerb_data_2015$Year <- factor(NatHerb_data_2015$Year, 
                                  levels = c("2010", "2009", "2011", "2012", "2013", "2015"))



# EXOTIC HERBS #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #

# Set up a dataframe that contains data for a model on invasives
ExoHerb_2015 <- CLComplete_2015 %>%
  filter(Native == FALSE,
         Default_LF %in% c("Grass", "Forb")) %>%
  group_by(Year,
           Site,
           Plot,
           Treatment,
           MacroPlot,
           yearly_rain, 
           Native,
           Default_LF) %>%
  summarise(Total_ExoHerb = sum(Count), .groups = "keep") %>%
  ungroup()


# we want a dataset that includes total detections for each macroplot in each year
ExoHerb_data_2015 <- merge(x = ExoHerb_2015, y = total_detections_2015, 
                            by.x = c("MacroPlot", "Year"),
                            by.y = c("MacroPlot", "Year"), 
                            all = TRUE)

# set Control as base level
ExoHerb_data_2015$Treatment <- factor(ExoHerb_data_2015$Treatment, 
                                       levels = c("Control", "Burn", "Mechanical"))

# Set year levels so that 2010 is treated as baseline
ExoHerb_data_2015$Year <- factor(ExoHerb_data_2015$Year, 
                                  levels = c("2010", "2009", "2011", "2012", "2013", "2015"))


