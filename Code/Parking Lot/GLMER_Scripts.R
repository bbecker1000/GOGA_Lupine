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
                        Treatment * 
                        Year +
                        yearly_rain +
                        (1|Site/Plot), 
                      family = binomial, 
                      data = Lupin_data)

summary(m2_Lupin_cat)


# FOR NATIVE SPECIES

# Run a model on native species changes over time by treatment 
# [YEAR AS NUMERIC, BASE 400]
m1_Nativity_numeric <- glmer(cbind(Total_Native, 400-Total_Native) ~ 
                               Treatment +
                               Year_Time_since_trt +  
                               scale(yearly_rain) + 
                               (1|Site/Plot), 
                             family = binomial, 
                             data = Nativity_data_2011_2013)

summary(m1_Nativity_numeric)

# Run a model on native species changes over time by treatment 
# [YEAR AS NUMERIC,BASE TOTAL COUNT]
m2_Nativity_numeric <- glmer(cbind(Total_Native, Total_Count - Total_Native) ~ 
                               Treatment +
                               Year_Time_since_trt +  
                               scale(yearly_rain) + 
                               (1|Site/Plot), 
                             family = binomial, 
                             data = Nativity_data_2011_2013)


summary(m2_Nativity_numeric)

# Run a model on native species changes over time by treatment 
# [YEAR AS CHARACTER, BASE 400]

m1_Nativity_cat <- glmer(cbind(Total_Native, 400-Total_Native) ~ 
                           Treatment *
                           Year +  
                           #scale(yearly_rain) + 
                           (1|Plot), 
                         family = binomial, 
                         data = Nativity_data)


summary(m1_Nativity_cat)

# Run a model on native species changes over time by treatment 
# [YEAR AS CHARACTER,BASE TOTAL COUNT]
m2_Nativity_cat <- glmer(cbind(Total_Native, Total_Count - Total_Native) ~ 
                           Treatment +
                           Year +  
                           #scale(yearly_rain) + 
                           (1|Site/Plot), 
                         family = binomial, 
                         data = Nativity_data_2011_2013)


summary(m2_Nativity_cat)

# FOR INVASIVE SPECIES

# Run a model on invasive species changes over time by treatment 
# [YEAR AS NUMERIC, BASE 400]
m1_Invasive_numeric <- glmer(cbind(Total_Invasive, 400-Total_Invasive) ~ 
                               Treatment +
                               Year_Time_since_trt +  
                               scale(yearly_rain) + 
                               (1|Site/Plot), 
                             family = binomial, 
                             data = Invasive_data_2011_2013)


summary(m1_Invasive_numeric)

# Run a model on invasive species changes over time by treatment 
# # [YEAR AS NUMERIC, BASE TOTAL COUNT]
m2_Invasive <- glmer(cbind(Total_Invasive, Total_Count - Total_Invasive) ~ 
                       Treatment +
                       Year_Time_since_trt +  
                       scale(yearly_rain) + 
                       (1|Site/Plot), 
                     family = binomial, 
                     data = Invasive_data_2011_2013)


summary(m2_Invasive_numeric)

# Run a model on invasive species changes over time by treatment 
# [YEAR AS CHARACTER, BASE 400]
m1_Invasive_cat <- glmer(cbind(Total_Invasive, 400-Total_Invasive) ~ 
                           Treatment +
                           Year +  
                           scale(yearly_rain) + 
                           (1|Site/Plot), 
                         family = binomial, 
                         data = Invasive_data_2011_2013)


summary(m1_Invasive_cat)

# Run a model on invasive species changes over time by treatment 
# # [YEAR AS CHARACTER, BASE TOTAL COUNT]
m2_Invasive_cat <- glmer(cbind(Total_Invasive, Total_Count - Total_Invasive) ~ 
                           Treatment +
                           Year +  
                           scale(yearly_rain) + 
                           (1|Site/Plot), 
                         family = binomial, 
                         data = Invasive_data_2011_2013)


# # Plotting m2_Lupin_numeric
plot_model(m2_Lupin_numeric, type = "pred", 
           terms = c("Year_Time_since_trt", "Treatment")) +
  theme_classic() +
  labs(x = "Time Since Treatment in Years",
       y = "Percent Cover of Lupin",
       title = NULL)

# # Plotting m2_Lupin_cat
plot_model(m2_Lupin_cat, type = "pred", 
           terms = c("Year", "Treatment")) +
  theme_classic() +
  labs(x = "Year of Data Collection",
       y = "Percent Cover of Lupin",
       title = NULL)

# # Plotting m2_Lupin_numeric
plot_model(m2_Lupin_numeric, type = "pred", 
           terms = c("Year_Time_since_trt", "Treatment")) +
  theme_classic() +
  labs(x = "Time Since Treatment in Years",
       y = "Percent Cover of Lupin",
       title = NULL)

# # Plotting m2_Lupin_cat
plot_model(m2_Lupin_cat, type = "pred", 
           terms = c("Year", "Treatment")) +
  theme_classic() +
  labs(x = "Year of Data Collection",
       y = "Percent Cover of Lupin",
       title = NULL)

# Plot Residuals for m2_Nativity_cat
plot_model(m2_Nativity_cat, type = "diag")

# Forest Plot for m2_Nativity_cat
plot_model(m2_Nativity_cat, type = "est") + 
  geom_hline(yintercept = 1, linetype = 2)

# Prediction Plot m2_Nativity_cat
plot_model(m2_Nativity_cat, type = "pred", 
           terms = c("Year", "Treatment")) +
  theme_classic() +
  labs(x = "Year of Data Collection",
       y = "Percent Cover of Native Species",
       title = NULL)


# Plot Residuals for m2_Nativity_numeric
plot_model(m2_Nativity_numeric, type = "diag")

# Forest Plot for m2_Nativity_numeric
plot_model(m2_Nativity_numeric, type = "est") + 
  geom_hline(yintercept = 1, linetype = 2)

# Prediction Plot m2_Nativity_numeric
plot_model(m2_Nativity_numeric, type = "pred", 
           terms = c("Year_Time_since_trt", "Treatment")) +
  theme_classic() +
  labs(x = "Time Since Treatment in Years",
       y = "Percent Cover of Native Species",
       title = NULL)


