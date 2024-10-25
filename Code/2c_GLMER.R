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
# [YEAR AS NUMERIC, BASE 400]
m1_Lupin_numeric <- glmer(cbind(Total_Lupin, 400 - Total_Lupin) ~ 
                           Treatment + 
                           Year_Time_since_trt +
                           yearly_rain +
                           (1|Site/Plot), 
                         family = binomial, 
                         data = Lupin_data_2011_2013)

summary(m1_Lupin_numeric)

# Run a model on lupin changes over time by treatment 
#[YEAR AS CATEGORICAL, BASE 400]
m1_Lupin_cat <- glmer(cbind(Total_Lupin, 400 - Total_Lupin) ~ 
                       Treatment + 
                       Year +
                       yearly_rain +
                       (1|Site/Plot), 
                     family = binomial, 
                     data = Lupin_data)

summary(m1_Lupin_cat)

# Run a model on lupin changes over time by treatment 
# [YEAR AS NUMERIC, BASE TOTAL COUNT]
m2_Lupin_numeric <- glmer(cbind(Total_Lupin, Total_Count - Total_Lupin) ~ 
                   Treatment + 
                   Year_Time_since_trt +
                   yearly_rain +
                   (1|Site/Plot), 
                 family = binomial, 
                 data = Lupin_data_2011_2013)

summary(m2_Lupin_numeric)

# Run a model on lupin changes over time by treatment 
# [YEAR AS CATEGORICAL, BASE TOTAL COUNT]
m2_Lupin_cat <- glmer(cbind(Total_Lupin, Total_Count - Total_Lupin) ~ 
                           Treatment + 
                           Year +
                           yearly_rain +
                           (1|Site/Plot), 
                         family = binomial, 
                         data = Lupin_data)

summary(m2_Lupin_cat)


# FOR NATIVE SPECIES

# Run a model on native species changes over time by treatment [BASE 400]
m1_Nativity <- glmer(cbind(Total_Native, 400-Total_Native) ~ 
                      Treatment +
                      Year_Time_since_trt +  
                      scale(yearly_rain) + 
                      (1|Site/Plot), 
                    family = binomial, 
                    data = Nativity_data_2011_2013)


summary(m1_Nativity)

# Run a model on native species changes over time by treatment [BASE TOTAL COUNT]
m2_Nativity <- glmer(cbind(Total_Native, Total_Count - Total_Native) ~ 
                       Treatment +
                       Year_Time_since_trt +  
                       scale(yearly_rain) + 
                       (1|Site/Plot), 
                     family = binomial, 
                     data = Nativity_data_2011_2013)


summary(m2_Nativity)

# FOR INVASIVE SPECIES

# Run a model on invasive species changes over time by treatment [BASE 400]
m1_Invasive <- glmer(cbind(Total_Invasive, 400-Total_Invasive) ~ 
                      Treatment +
                      Year_Time_since_trt +  
                      scale(yearly_rain) + 
                      (1|Site/Plot), 
                    family = binomial, 
                    data = Invasive_data_2011_2013)


summary(m1_Invasive)

# Run a model on invasive species changes over time by treatment [BASE TOTAL COUNT]
m2_Invasive <- glmer(cbind(Total_Invasive, Total_Count - Total_Invasive) ~ 
                      Treatment +
                      Year_Time_since_trt +  
                      scale(yearly_rain) + 
                      (1|Site/Plot), 
                    family = binomial, 
                    data = Invasive_data_2011_2013)


summary(m2_Invasive)
