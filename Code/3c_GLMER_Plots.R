source("Code/2C_GLMER.R")

# FOR LUPIN DATA

# Plot Residuals
plot_model(m_Lupin, type = "diag")

# Forest Plot
plot_model(m_Lupin, type = "est") + 
  geom_hline(yintercept = 1, linetype = 2)

# Predicted change in percent cover lupin with time since treatment for each treatment
plot_model(m_Lupin, type = "pred", 
           terms = c("Year_Time_since_trt", "Treatment")) +
  theme_classic() +
  labs(x = "Time Since Treatment in Years",
       y = "Percent Cover of Lupin",
       title = NULL)


# FOR NATIVE DATA

# Plot Residuals
plot_model(m_Nativity, type = "diag")

# Forest Plot
plot_model(m_Nativity, type = "est") + 
  geom_hline(yintercept = 1, linetype = 2)

# Predicted change in percent cover native with time since treatment for each treatment
plot_model(m_Nativity, type = "pred", terms = c("Year_Time_since_trt", "Treatment")) +
  theme_classic() +
  labs(x = "Time Since Treatment in Years",
       y = "Percent Cover of Native Species",
       title = NULL)

# FOR INVASIVE DATA

# Plot Residuals
plot_model(m_Invasive, type = "diag")

# Forest Plot
plot_model(m_Invasive, type = "est") + 
  geom_hline(yintercept = 1, linetype = 2)

# Predicted change in percent cover invasive with time since treatment for each treatment
plot_model(m_Invasive, type = "pred", terms = c("Year_Time_since_trt", "Treatment")) +
  theme_classic() +
  labs(x = "Time Since Treatment in Years",
       y = "Percent Cover of Invasive Species",
       title = NULL)
