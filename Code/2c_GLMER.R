source("Code/1c_Data_setup_GLMER.R")
library(lme4)

# Run a model on lupin changes over time by treatment
m_Lupin <- glmer(cbind(Total_Count, 400-Total_Count) ~ 
                   #yr_trt + 
                   Year +
                   Treatment +
                  # yearly_rain +
                   (1|MacroPlot), 
                 family = binomial, 
                 data = Lupin_data)

print(summary(m_Lupin), correlation = FALSE) 

# Run a model on native species changes over time by treatment

m_Nativity <- glmer(cbind(Total_Count, 400-Total_Count) ~ 
                      Treatment + 
                      #yearly_rain + 
                      Year + 
                      (1|MacroPlot), 
                    family = binomial, 
                    data = Nativity_data)

print(summary(m_Nativity), correlation = FALSE) 
