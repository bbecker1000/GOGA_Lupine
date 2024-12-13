# Upload the data we will need
source("Code/1a_alldata.R")
source("Code/1c_Data_setup_GLMER.R")



Lupin_data_power <-
  filter(Lupin_data, Trt_Status == "after"))

# For Lupin Data

## set up data for power analysis
Lupin_power <- supplyData(
  Lupin_data,
  variableType = "binomial",
  design.matrix = list(Response = "Total_Lupine", 
                       Trials = "MacroPlot", 
                       Location = "Site", 
                       sublocation = "Plot", 
                       Time = "Year", 
                       subtime = NA, 
                       BvA = "Trt_Status", 
                       CvI = "Treatment"
  ),
  levels.dat = list(Before ="before", 
                    Control = "CONTROL", 
                    After = "after", 
                    Impact = c("BURN", "MECHANICAL")),
  scenario.data = list(Number.of.iterations = 5,
                       filename = "Lupin_data_comps",
                       Number.of.Impact.Locations = 82 ,
                       Number.of.Control.Locations = 40,
                       Number.of.sublocations.within.Location = 4,
                       Number.of.sample.times.Before = 51,
                       Number.of.sample.times.After = 71,
                       Number.of.subtimes.within.Time = 1,
                       Number.of.trials = 122,
                       Number.of.replicate.measurements = 5
  ),
  effect.info = list(Multiplicative = 1, Fixed.change = 0, Effect.values = "0;-0.3"),
  ncores = 1
) 

View(Lupin_power)

# Run the power analysis for Lupin
powerScenario(inputData = "Lupin_data_comps")




