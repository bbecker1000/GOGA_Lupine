source("Code/2C_GLMER.R")

# FOR LUPIN DATA

#Plot Residuals
plot_model(m_Lupin, type = "diag")
plot_model(m_Lupin, type = "est") + 
  geom_hline(yintercept = 1, linetype = 2)
plot_model(m_Lupin, type = "int", terms = c("Treatment", "Year"))



# FOR NATIVITY DATA

#Plot Residuals
plot_model(m_Nativity, type = "diag")
plot_model(m_Nativity, type = "est") + 
  geom_hline(yintercept = 1, linetype = 2)
plot_model(m_Nativity, type = "int", terms = c("Treatment", "Year"))