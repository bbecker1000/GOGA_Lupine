#### Set up data for epower

# Upload the data we will need
source("Code/1a_alldata.R")
source("Code/1c_Data_setup_GLMER.R")


# For Lupin Data

## set up data for power analysis
supplyData(
  Lupin_data,
  variableType = "gaussian",
  design.matrix = c(Response("Total_Lupine"), 
                    Trials("MacroPlot"), 
                    Location("Site"), 
                    sublocation("Plot"), 
                    Time(Year), 
                    subtime(NA), 
                    BvA(Trt_Status), 
                    Cvl(Treatment)
                    ),
  levels.dat = c("before", "CONTROL", "after", "BURN", "MECHANICAL"),
  scenario.data = c(Number.of.iterations(),
                      filename= "Lupin_data_comps",
                      Number.of.Impact.Locations(),
                      Number.of.Control.Locations(),
                      Number.of.sublocations.within.Location(),
                      Number.of.sample.times.Before(),
                      Number.of.sample.times.After(),
                      Number.of.subtimes.within.Time(),
                      Number.of.trials(),
                      Number.of.replicate.measurements() 
                      ),
    effect.info = list(Multiplicative = 1, Fixed.change = 0, Effect.values = "0;-0.3"),
    ncores = 1
  )





