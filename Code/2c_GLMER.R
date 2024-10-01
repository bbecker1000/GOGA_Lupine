library(lme4)

# Run a model on lupin changes over time by treatment
m_Lupin <- glmer(cbind(Total_Count, 100-Total_Count) ~ 
                   Treatment + 
                   yearly_rain + 
                   Year + 
                   (1|Site/Plot), 
                 family = binomial, 
                 data = Lupin_data)
