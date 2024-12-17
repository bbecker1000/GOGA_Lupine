source("Code/1c_Data_setup_GLMER.R")
library(lme4)
library(sjPlot)


# FOR LUPIN

## take a look at the data
ggplot(Lupin_data, aes(x = Year, y = Total_Lupin)) + 
  geom_boxplot() +
  geom_point(position = position_dodge(width = .75)) + 
  facet_wrap(~Treatment)


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


# FOR INVASIVE DATA

# Run a model on invasive species changes over time by treatment 
# # [YEAR AS PRE AND POST, BASE TOTAL COUNT]
m2_Invasive_PrePost <- glmer(cbind(Total_Invasive, Total_Count) ~ 
                           Treatment *
                           Trt_Status +  
                           scale(yearly_rain) + 
                           (1|Plot), 
                         family = binomial, 
                         data = Invasive_data)

summary(m2_Invasive_PrePost)


# FOR SHRUB DATA

# Run a model on shrubs species changes over time by treatment 
# # [YEAR AS PRE AND POST, BASE TOTAL COUNT]
m2_Shrub_PrePost <- glmer(cbind(Total_Shrubs, Total_Count) ~ 
                               Treatment *
                               Trt_Status +  
                               scale(yearly_rain) + 
                               (1|Plot), 
                             family = binomial, 
                             data = Shrubs_data)

summary(m2_Shrub_PrePost)
