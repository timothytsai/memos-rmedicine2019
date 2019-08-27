# clinic_lab_data_analytic_long %>% spread()

histograms <- clinic_lab_data_analytic_long %>% 
  ggplot(aes(x = value)) + geom_histogram(color = "grey25", fill = "white") + 
  facet_wrap(~gender + key, scales = "free", ncol = 4)

x_age_plot <- clinic_lab_data_analytic %>%
  ggplot(aes(x = age, y = enzyme_x, color = gender)) +
  geom_point() +
  geom_smooth(method = "gam")

a_age_plot <- clinic_lab_data_analytic %>%
  ggplot(aes(x = age, y = enzyme_a, color = gender)) +
  geom_point() +
  geom_smooth(method = "gam")
