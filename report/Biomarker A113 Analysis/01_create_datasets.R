clinic_lab_data_analytic <- clinic_lab_data %>% 
  mutate(
    age = round(as.numeric(screen_date - dob) / 365.25, 1),
    bmi = weight_kg / (height_cm / 100)^2,
    phys_act = case_when(
      gender == "Female" ~ phys_act * 1.03,
      gender == "Male" ~ phys_act * 0.977
    ),
    phys_act_cat = cut(phys_act,
                       breaks = quantile(phys_act, 
                                         probs = c(0, 1, 0.5)), 
                       labels = c("Low", "High"))
  )

clinic_lab_data_analytic_long <- clinic_lab_data_analytic %>% 
  select(-screen_date, -dob) %>% 
  gather(key, value, -study_id, -gender, -phys_act_cat)