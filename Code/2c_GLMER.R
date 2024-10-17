source("Code/1c_Data_setup_GLMER.R")
library(lme4)
library(sjPlot)

## take a look at the data
ggplot(Lupin_data, aes(x = Year, y = Total_Count)) + 
  geom_boxplot() +
  geom_point() + 
  facet_wrap(~Treatment)


# set Control as base level
Lupin_data$Treatment <- factor(Lupin_data$Treatment, levels = c("CONTROL", "BURN", "MECHANICAL"))
#make year numeric
Lupin_data$Year.numeric <- as.numeric(Lupin_data$Year)
Lupin_data$Year_Time_since_trt <- Lupin_data$Year.numeric-2011
#we think that treatment didn't happen until after 2010 sampling, so start with 2011 data
Lupin_data_2011_2013 <- Lupin_data %>% filter(Year >  2010)

# Run a model on lupin changes over time by treatment
m_Lupin <- glmer(cbind(Total_Count, 400-Total_Count) ~ 
                   Treatment + 
                   Year_Time_since_trt +
                   yearly_rain +
                   (1|Site/Plot), 
                 family = binomial, 
                 data = Lupin_data_2011_2013)

summary(m_Lupin)

plot_model(m_Lupin, type = "diag")
plot_model(m_Lupin, type = "est") + 
  geom_hline(yintercept = 1, linetype = 2)
plot_model(m_Lupin, type = "int", terms = c("Treatment", "Year"))

# Run a model on native species changes over time by treatment
# Need to filter similar as above.

m_Nativity <- glmer(cbind(Total_Count, 400-Total_Count) ~ 
                      Treatment +
                      Year +  
                      scale(yearly_rain) + 
                      (1|Site/Plot), 
                    family = binomial, 
                    data = Nativity_data)


summary(m_Nativity)
