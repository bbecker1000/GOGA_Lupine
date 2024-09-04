library(tidyverse)

CL_All <- read_csv("Data/Cover_Lifeform_All.csv")
#View(CL_All)

sum <- CL_All %>%
  mutate(YearMacroPlot = paste(Year, MacroPlot, sep = "_")) %>%
  group_by(YearMacroPlot, Species) %>%
  summarize(total_count = sum(Count))
#view(sum)

# Pivot the data wider 
wide_data <- sum %>%
  pivot_wider(names_from = Species, values_from = total_count)
#view(wide_data)

#why are there 28 rows of data missing? why did they do 5 sets of plots instead of 4?


