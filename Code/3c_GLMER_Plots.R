source("Code/2C_GLMER.R")

# FOR LUPIN DATA #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #

# Plot Residuals Lupin for m2_Lupin_PrePost
plot_model(m2_Lupin_PrePost, type = "diag")

# Forest Plot Lupin for m2_Lupin_PrePost
plot_model(m2_Lupin_PrePost, type = "est") + 
  geom_hline(yintercept = 1, linetype = 2)

# Prediction Plot m2_Lupin_PrePost
p1 <- plot_model(m2_Lupin_PrePost, type = "pred", 
           terms = c("Trt_Status", "Treatment")) +
  theme_classic() +
  labs(x = "Treatment Status",
       y = "Percent Cover of Lupin",
       title = NULL)

p1pred <- p1[["plot_env"]][["plot_data"]][["predicted"]]

# Prediction Plot m2_Lupin_PrePost
plot_model(m2_Lupin_PrePost, type = "int", 
           terms = c("Trt_Status", "Treatment")) +
  theme_classic() +
  labs(x = "Treatment Status",
       y = "Percent Cover of Lupin",
       title = NULL)

# Predicted change in percent cover lupin with annual precip
plot_model(m2_Lupin_PrePost, type = "pred", terms = c("yearly_rain")) +
  theme_classic() +
  labs(x = "Annual Precipitation (cm)",
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


# FOR NATIVE DATA #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #

# Plot Residuals for m2_Nativity_PrePost
plot_model(m2_Nativity_PrePost, type = "diag")

# Forest Plot for m2_Nativity_PrePost
plot_model(m2_Nativity_PrePost, type = "est") + 
  geom_hline(yintercept = 1, linetype = 2)

# Prediction Plot m2_Nativity_PrePost
plot_model(m2_Nativity_PrePost, type = "int", 
           terms = c("Trt_Status", "Treatment")) +
  theme_classic() +
  labs(x = "Treatment Status",
       y = "Percent Cover of Native Species",
       title = NULL)

# Prediction Plot m2_Nativity_PrePost with rainfall
plot_model(m2_Nativity_PrePost, type = "eff", 
           terms = c("Trt_Status", "Treatment", "yearly_rain_scaled")) +
  theme_classic() +
  labs(x = "Treatment Status",
       y = "Percent Cover of Native Species",
       title = NULL)

# Predicted change in percent cover native with annual precip
plot_model(m2_Nativity_PrePost, type = "pred", 
           terms = c("yearly_rain")) +
  theme_classic() +
  labs(x = "Annual Precipitation (cm)",
       y = "Percent Cover of Native Species",
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


# FOR INVASIVE DATA #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #

# Plot Residuals Invasive Species
plot_model(m2_Invasive_PrePost, type = "diag")

# Forest Plot Invasive Species
plot_model(m2_Invasive_PrePost, type = "est") + 
  geom_hline(yintercept = 1, linetype = 2)

# Predicted change in percent cover invasive with time since treatment for each treatment
plot_model(m2_Invasive_PrePost, type = "pred", 
           terms = c("Treatment", "Trt_Status")) +
  theme_classic() +
  labs(x = "Treatment Status",
       y = "Percent Cover of Invasive Species",
       title = NULL)

# Predicted change in percent cover invasive with annual precip
plot_model(m2_Invasive_PrePost, type = "pred", terms = c("yearly_rain")) +
  theme_classic() +
  labs(x = "Annual Precipitation (cm)",
       y = "Percent Cover of Invasive Species",
       title = NULL)

