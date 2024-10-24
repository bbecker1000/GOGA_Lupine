source("Code/1c_Data_setup_GLMER.R")
library(lme4)
library(sjPlot)


# FOR LUPIN

## take a look at the data
ggplot(Lupin_data, aes(x = Year, y = Total_Count)) + 
  geom_boxplot() +
  geom_point() + 
  facet_wrap(~Treatment)

# Run a model on lupin changes over time by treatment
m_Lupin <- glmer(cbind(Total_Count_Lupin, total_detections-Total_Count_Lupin) ~ 
                   Treatment + 
                   Year_Time_since_trt +
                   yearly_rain +
                   (1|Site/Plot), 
                 family = binomial, 
                 data = Lupin_merge_2011_2013_zeros)

summary(m_Lupin)

# FOR NATIVE SPECIES

# Run a model on native species changes over time by treatment
m_Nativity <- glmer(cbind(Total_Count, 400-Total_Count) ~ 
                      Treatment +
                      Year_Time_since_trt +  
                      scale(yearly_rain) + 
                      (1|Site/Plot), 
                    family = binomial, 
                    data = Nativity_data_2011_2013)


summary(m_Nativity)

# FOR INVASIVE SPECIES

# Run a model on invasive species changes over time by treatment
m_Invasive <- glmer(cbind(Total_Count, 400-Total_Count) ~ 
                      Treatment +
                      Year_Time_since_trt +  
                      scale(yearly_rain) + 
                      (1|Site/Plot), 
                    family = binomial, 
                    data = Invasive_data_2011_2013)


summary(m_Invasive)
