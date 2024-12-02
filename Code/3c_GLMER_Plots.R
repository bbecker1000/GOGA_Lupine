source("Code/2C_GLMER.R")
library(patchwork)

# FOR LUPIN DATA #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #

# Plot Residuals Lupin for m2_Lupin_PrePost
plot_model(m2_Lupin_PrePost, type = "diag")

# Forest Plot Lupin for m2_Lupin_PrePost
plot_model(m2_Lupin_PrePost, type = "est") + 
  geom_hline(yintercept = 1, linetype = 2)

# Prediction Plot m2_Lupin_PrePost
plot_model(m2_Lupin_PrePost, type = "pred", 
           terms = c("Trt_Status", "Treatment")) +
  theme_classic() +
  labs(x = "Treatment Status",
       y = "Percent Cover of Lupin",
       title = NULL)

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

Lupin_data <- Lupin_data %>% mutate(Treatment = tolower(Treatment))

predicted_data <- Lupin_data %>%
  mutate(predicted = predict(m2_Lupin_PrePost, type = "response"))

predicted_lupin_plot <- ggplot(predicted_data, 
  aes(x = Trt_Status, y = predicted, fill = Treatment)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 0.15)) +
  labs(
    title = "Predicted Percent Cover of Lupin by Treatment Status",
    x = "Treatment Status", 
    y = "Predicted Percent Cover of Lupin",
    fill = "Treatment") +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"), # Bold x-axis label
    axis.title.y = element_text(face = "bold"), # Bold y-axis label
    legend.title = element_text(face = "bold"), # Bold legend title
    legend.text = element_text(face = "plain") # Lowercase text
    )

predicted_lupin_plot
    
Lupin_data2 <- Lupin_data %>%
  mutate(Percent_Cover_Lupin = Total_Lupin/Total_Count)
  
actual_lupin_plot <- ggplot(Lupin_data2, 
       aes(Trt_Status, Percent_Cover_Lupin, fill = Treatment)) +
  geom_boxplot() +
  geom_point(position = position_dodge(width = .75)) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 0.15)) +
  labs(
    title = "Actual Percent Cover of Lupin by Treatment Status",
    x = "Treatment Status", 
    y = "Percent Cover of Lupin",
    fill = "Treatment"
      ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"), # Bold x-axis label
    axis.title.y = element_text(face = "bold"), # Bold y-axis label
    legend.title = element_text(face = "bold"), # Bold legend title
    legend.text = element_text(face = "plain") # Lowercase text
  )

actual_lupin_plot

lupine_plots <- actual_lupin_plot | predicted_lupin_plot


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

Nativity_data %>%
  mutate(my_model = predict(m2_Nativity_PrePost)) %>%
  ggplot(aes(Trt_Status, my_model, fill = Treatment)) +
  # geom_point(position = position_dodge(width = .75)) +
  geom_boxplot()

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


Nativity_data <- Nativity_data %>% mutate(Treatment = tolower(Treatment))

predicted_data_nativity <- Nativity_data %>%
  mutate(predicted = predict(m2_Nativity_PrePost, type = "response"))

predicted_native_plot <- ggplot(predicted_data_nativity, 
  aes(x = Trt_Status, y = predicted, fill = Treatment)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::percent
                     # limits = c(0, 1)
                     ) +
  labs(
    title = "Predicted Percent Cover of Native Species by Treatment Status",
    x = "Treatment Status",
    y = "Predicted Percent Cover of Native Species",
    fill = "Treatment") +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"), # Bold x-axis label
    axis.title.y = element_text(face = "bold"), # Bold y-axis label
    legend.title = element_text(face = "bold"), # Bold legend title
    legend.text = element_text(face = "plain") # Lowercase text
  )

predicted_native_plot

Nativity_data2 <- Nativity_data %>%
  mutate(Percent_Cover_Native = Total_Native/Total_Count)

actual_native_plot <- ggplot(Nativity_data2, 
                            aes(Trt_Status, Percent_Cover_Native, fill = Treatment)) +
  geom_boxplot() +
  geom_point(position = position_dodge(width = .75)) +
  scale_y_continuous(labels = scales::percent
                    # limits = c(0, 1)
                     ) +
  labs(
    title = "Actual Percent Cover of Native Species by Treatment Status",
    x = "Treatment Status",
    y = "Percent Cover of Native Species",
    fill = "Treatment"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"), # Bold x-axis label
    axis.title.y = element_text(face = "bold"), # Bold y-axis label
    legend.title = element_text(face = "bold"), # Bold legend title
    legend.text = element_text(face = "plain") # Lowercase text
  )

actual_native_plot

native_plots <- actual_native_plot | predicted_native_plot

native_plots


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


Invasive_data <- Invasive_data %>% mutate(Treatment = tolower(Treatment))

predicted_data_invasive <- Invasive_data %>%
  mutate(predicted = predict(m2_Invasive_PrePost, type = "response"))

predicted_invasive_plot <- ggplot(predicted_data_invasive, 
                                aes(x = Trt_Status, y = predicted, fill = Treatment)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::percent
                     # limits = c(0, 0.9)
  ) +
  labs(
    title = "Predicted Percent Cover of Invasive Species by Treatment Status",
    x = "Treatment Status",
    y = "Predicted Percent Cover of Invasive Species",
    fill = "Treatment") +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"), # Bold x-axis label
    axis.title.y = element_text(face = "bold"), # Bold y-axis label
    legend.title = element_text(face = "bold"), # Bold legend title
    legend.text = element_text(face = "plain") # Lowercase text
  )

predicted_invasive_plot

Invasive_data2 <- Invasive_data %>%
  mutate(Percent_Cover_Invasive = Total_Invasive/Total_Count)

actual_invasive_plot <- ggplot(Invasive_data2, 
                             aes(Trt_Status, Percent_Cover_Invasive, fill = Treatment)) +
  geom_boxplot() +
  geom_point(position = position_dodge(width = .75)) +
  scale_y_continuous(labels = scales::percent
                      #limits = c(0, 0.9)
  ) +
  labs(
    title = "Actual Percent Cover of Invasive Species by Treatment Status",
    x = "Treatment Status",
    y = "Percent Cover of Invasive Species",
    fill = "Treatment"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"), # Bold x-axis label
    axis.title.y = element_text(face = "bold"), # Bold y-axis label
    legend.title = element_text(face = "bold"), # Bold legend title
    legend.text = element_text(face = "plain") # Lowercase text
  )

actual_invasive_plot

invasive_plots <- actual_invasive_plot | predicted_invasive_plot

invasive_plots
