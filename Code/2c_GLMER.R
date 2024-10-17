source("Code/1c_Data_setup_GLMER.R")
library(lme4)
library(sjPlot)

## take a look at the data
ggplot(Lupin_data, aes(x = Year, y = Total_Count)) + 
  geom_boxplot() +
  geom_point() + 
  facet_wrap(~Treatment)

# Run a model on lupin changes over time by treatment
m_Lupin <- glmer(cbind(Total_Count, 400-Total_Count) ~ 
                   Treatment + 
                   Year_Time_since_trt +
                   yearly_rain +
                   (1|Site/Plot), 
                 family = binomial, 
                 data = Lupin_data_2011_2013)

summary(m_Lupin)


# Run a model on native species changes over time by treatment
# Need to filter similar as above.

m_Nativity <- glmer(cbind(Total_Count, 400-Total_Count) ~ 
                      Treatment +
                      Year_Time_since_trt +  
                      scale(yearly_rain) + 
                      (1|Site/Plot), 
                    family = binomial, 
                    data = Nativity_data_2011_2013)


summary(m_Nativity)
