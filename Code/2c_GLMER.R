source("Code/1c_Data_setup_GLMER.R")
library(lme4)
library(sjPlot)


# FOR LUPIN

## take a look at the data
ggplot(Lupin_data, aes(x = Year, y = Total_Count)) + 
  geom_boxplot() +
  geom_point(position = position_dodge(width = .75)) + 
  facet_wrap(~Treatment)


# Run a model on lupin changes over time by treatment 
# [YEAR AS NUMERIC, BASE TOTAL COUNT]
m2_Lupin_numeric <- glmer(cbind(Total_Lupin, Total_Count) ~ 
                   Treatment + 
                   Year_Time_since_trt +
                   yearly_rain +
                   (1|Site/Plot), 
                 family = binomial, 
                 data = Lupin_data_2011_2013)

summary(m2_Lupin_numeric)

# Run a model on lupin changes over time by treatment 
# [YEAR AS CATEGORICAL, BASE TOTAL COUNT]
m2_Lupin_cat <- glmer(cbind(Total_Lupin, Total_Count) ~ 
                           Treatment * 
                           Year +
                           yearly_rain +
                           (1|Site/Plot), 
                         family = binomial, 
                         data = Lupin_data)

summary(m2_Lupin_cat)

# Run a model on lupin changes over time by treatment 
# [TIME AS PRE AND POST, BASE TOTAL COUNT]
m2_Lupin_PrePost <- glmer(cbind(Total_Lupin, Total_Count) ~ 
                        Treatment *
                        Trt_Status +
                        yearly_rain +
                        (1|Site/Plot), 
                      family = binomial, 
                      data = Lupin_data)

summary(m2_Lupin_PrePost)


# FOR NATIVE SPECIES

# Run a model on native species changes over time by treatment 
# [YEAR AS NUMERIC,BASE TOTAL COUNT]
m2_Nativity_numeric <- glmer(cbind(Total_Native, Total_Count) ~ 
                       Treatment +
                       Year_Time_since_trt +  
                       scale(yearly_rain) + 
                       (1|Site/Plot), 
                     family = binomial, 
                     data = Nativity_data_2011_2013)


summary(m2_Nativity_numeric)

# Run a model on native species changes over time by treatment 
# [YEAR AS CHARACTER,BASE TOTAL COUNT]
m2_Nativity_cat <- glmer(cbind(Total_Native, Total_Count) ~ 
                               Treatment +
                               Year +  
                               #scale(yearly_rain) + 
                               (1|Plot), 
                             family = binomial, 
                             data = Nativity_data)

summary(m2_Nativity_cat)

Nativity_data$yearly_rain_scaled <- scale(Nativity_data$yearly_rain)

# Run a model on native species changes over time by treatment 
# [YEAR AS PRE AD POST, BASE TOTAL COUNT]
m2_Nativity_PrePost <- glmer(cbind(Total_Native, Total_Count) ~ 
                           Treatment *
                           Trt_Status +  
                           yearly_rain_scaled + 
                           (1|Plot), 
                         family = binomial, 
                         data = Nativity_data)

summary(m2_Nativity_PrePost)



# FOR INVASIVE SPECIES

# Run a model on invasive species changes over time by treatment 
# # [YEAR AS NUMERIC, BASE TOTAL COUNT]
m2_Invasive_numeric <- glmer(cbind(Total_Invasive, Total_Count) ~ 
                      Treatment +
                      Year_Time_since_trt +  
                      scale(yearly_rain) + 
                      (1|Site/Plot), 
                    family = binomial, 
                    data = Invasive_data_2011_2013)

summary(m2_Invasive_numeric)


# Run a model on invasive species changes over time by treatment 
# # [YEAR AS CHARACTER, BASE TOTAL COUNT]
m2_Invasive_cat <- glmer(cbind(Total_Invasive, Total_Count) ~ 
                       Treatment +
                       Year +  
                       #scale(yearly_rain) + 
                       (1|Plot), 
                     family = binomial, 
                     data = Invasive_data)

summary(m2_Invasive_cat)

# Run a model on invasive species changes over time by treatment 
# # [YEAR AS PRE AD POST, BASE TOTAL COUNT]
m2_Invasive_PrePost <- glmer(cbind(Total_Invasive, Total_Count) ~ 
                           Treatment *
                           Trt_Status +  
                           scale(yearly_rain) + 
                           (1|Plot), 
                         family = binomial, 
                         data = Invasive_data)

summary(m2_Invasive_PrePost)
