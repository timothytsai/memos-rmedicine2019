crude <- lm(enzyme_a ~ age, data = clinic_lab_data_analytic) %>%
  summary()

gender_adj <- lm(enzyme_a ~ age + factor(gender), data = clinic_lab_data_analytic) %>%
  summary()

full_adj <- lm(enzyme_a ~ age + phys_act + bmi + factor(gender), data = clinic_lab_data_analytic) %>%
  summary()

models_out <- rbind(crude$coefficients, gender_adj$coefficients, full_adj$coefficients)