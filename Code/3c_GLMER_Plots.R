source("Code/2C_GLMER.R")

# FOR LUPIN DATA

# Plot Residuals Lupin
plot_model(m_Lupin, type = "diag")

# Forest Plot Lupin
plot_model(m_Lupin, type = "est") + 
  geom_hline(yintercept = 1, linetype = 2)

# Predicted change in percent cover lupin with time since treatment for each treatment
plot_model(m_Lupin, type = "pred", 
           terms = c("Year_Time_since_trt", "Treatment")) +
  theme_classic() +
  labs(x = "Time Since Treatment in Years",
       y = "Percent Cover of Lupin",
       title = NULL)

# Plotting m2_Lupin_cat
plot_model(m2_Lupin_cat, type = "pred", 
           terms = c("Year", "Treatment")) +
  theme_classic() +
  labs(x = "Year of Data Collection",
       y = "Percent Cover of Lupin",
       title = NULL)


# Predicted change in percent cover lupin with annual precip
plot_model(m_Lupin, type = "pred", terms = c("yearly_rain")) +
  theme_classic() +
  labs(x = "Annual Precipitation (cm)",
       y = "Percent Cover of Lupin",
       title = NULL)

# FOR NATIVE DATA

# Plot Residuals Native Species
plot_model(m_Nativity, type = "diag")

# Forest Plot Native Species
plot_model(m_Nativity, type = "est") + 
  geom_hline(yintercept = 1, linetype = 2)

# Predicted change in percent cover native with time since treatment for each treatment
plot_model(m_Nativity, type = "pred", terms = c("Year_Time_since_trt", "Treatment")) +
  theme_classic() +
  labs(x = "Time Since Treatment in Years",
       y = "Percent Cover of Native Species",
       title = NULL)

# Predicted change in percent cover native with annual precip
plot_model(m_Nativity, type = "pred", terms = c("yearly_rain")) +
  theme_classic() +
  labs(x = "Annual Precipitation (cm)",
       y = "Percent Cover of Native Species",
       title = NULL)

# FOR INVASIVE DATA

# Plot Residuals Invasive Species
plot_model(m_Invasive, type = "diag")

# Forest Plot Invasive Species
plot_model(m_Invasive, type = "est") + 
  geom_hline(yintercept = 1, linetype = 2)

# Predicted change in percent cover invasive with time since treatment for each treatment
plot_model(m_Invasive, type = "pred", terms = c("Year_Time_since_trt", "Treatment")) +
  theme_classic() +
  labs(x = "Time Since Treatment in Years",
       y = "Percent Cover of Invasive Species",
       title = NULL)

# Predicted change in percent cover invasive with annual precip
plot_model(m_Invasive, type = "pred", terms = c("yearly_rain")) +
  theme_classic() +
  labs(x = "Annual Precipitation (cm)",
       y = "Percent Cover of Invasive Species",
       title = NULL)

