source("Code/1c_Data_setup_GLMER.R")
library(lme4)

# Run a model on lupin changes over time by treatment
m_Lupin <- glmer(cbind(Total_Count, 400-Total_Count) ~ 
                   Treatment + 
                   yearly_rain + 
                   Year + 
                   (1|Site/Plot), 
                 family = binomial, 
                 data = Lupin_data)

# Run a model on native species changes over time by treatment

m_Nativity <- glmer(cbind(Total_Count, 400-Total_Count) ~ 
                      Treatment + 
                      yearly_rain + 
                      Year + 
                      (1|Site/Plot), 
                    family = binomial, 
                    data = Nativity_data)


