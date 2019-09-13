crude <- lm(enzyme_a ~ age, data = clinic_lab_data_analytic) %>%
  summary()

gender_adj <- lm(enzyme_a ~ age + gender, data = clinic_lab_data_analytic) %>%
  summary()

full_adj_lm <- lm(enzyme_a ~ age + phys_act + bmi + gender,
                  data = clinic_lab_data_analytic)

full_adj <- full_adj_lm %>%
  summary()

models_out_tmp <- rbind(crude$coefficients, gender_adj$coefficients,
                        full_adj$coefficients)

models_out <- models_out_tmp %>%
  as.data.frame %>%
  mutate(
`Pr(>|t|)` = format.pval(`Pr(>|t|)`, digits = 2, 1e-3),
` ` = rownames(models_out_tmp)
) %>% 
  select(` `, everything())