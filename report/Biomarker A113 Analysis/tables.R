## one way to calculate means
# clinic_lab_data_analytic %>% 
#   group_by(gender) %>% 
#   summarize(
#     mean_age = mean(age, na.rm = TRUE),
#     mean_height = mean(height_cm, na.rm = TRUE),
#     mean_enz_a = mean(enzyme_a, na.rm = TRUE),
#     mean_bmi = mean(bmi, na.rm = TRUE))

clinic_lab_data_summary_table <- clinic_lab_data_analytic_long %>%
  nest(-gender, -phys_act_cat, -key) %>%
  filter(!is.na(phys_act_cat) & key != "diabetes") %>% 
  mutate(
    mean = map_dbl(data, ~mean(.$value, na.rm = TRUE)),
    sd = map_dbl(data, ~sd(.$value, na.rm = TRUE)),
    n = map_dbl(data, ~sum(!is.na(.$value)))
  ) %>%
  select(-data) %>% 
  mutate_at(vars(mean, sd), round, 1) %>% 
  mutate(mean_sd = paste0(mean, " (", sd, ")")) %>% 
  unite(gender_phys, c("gender", "phys_act_cat")) %>% 
  select(gender_phys, key, mean_sd) %>% 
  spread(gender_phys, mean_sd)

clinic_lab_data_summary_table <- clinic_lab_data_summary_table %>%
  arrange(
    match(key, c("age", "bmi", "height_cm" , "weight_kg", "phys_act",
                 "diabetes", "enzyme_a", "enzyme_x"))
  ) %>% 
  mutate(
    key = case_when(
      key == "age" ~ "Age, y",
      key == "bmi" ~ "BMI",
      key == "height_cm" ~ "Height, cm",
      key == "weight_kg"  ~ "Weight, kg",
      key == "phys_act" ~ "Physical Activity Scale",
      key == "diabetes" ~ "Diabetes",
      key == "enzyme_a" ~ "Enzyme A133, pg/mL",
      key == "enzyme_x" ~ "Enzyme X808, ng/dL"
    )
  )