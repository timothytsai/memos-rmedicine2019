library(lubridate)
library(dplyr)
library(ggplot2)
library(truncnorm)
library(tidyr)
library(purrr)
library(broom)
library(equatiomatic)
library(here)

set.seed(111314113)

n_rec <- 450

base_data <- data.frame(
  study_id = seq(1, n_rec),
  redcap_event_name = "baseline_arm_1",
  screen_date = sample(seq.Date(Sys.Date() - years(1), Sys.Date() - months(1), by = "day"), n_rec, replace = TRUE),
  dob = sample(seq.Date(Sys.Date() - years(91), Sys.Date() - years(19), by = "day"), n_rec, replace = TRUE),
  gender = sample(c("Male", "Female"), n_rec, replace = TRUE, prob = c(45, 55)),
  diabetes = sample(c(1, 0), n_rec, replace = TRUE, prob = c(10, 90))
)

men_n <- 2591
women_n <- 2760

base_data <- base_data %>%
  mutate(
    # gender = factor(gender),
    # diabetes = factor(diabetes, labels = c("No", "Yes")),
    height_cm = case_when(
      gender == "Male"   ~ round(rtruncnorm(n_rec, a = 142, mean = 175.3, sd = 0.3*sqrt(men_n)), 0),
      gender == "Female" ~ round(rtruncnorm(n_rec, a = 142, mean = 161.5, sd = 0.3*sqrt(women_n)), 0)
    ),
    weight_kg = case_when(
      gender == "Male"   ~ round(rtruncnorm(n_rec, a = 45, b = 160, mean = 89.7, sd = 0.9*sqrt(men_n)), 1),
      gender == "Female" ~ round(rtruncnorm(n_rec, a = 40, b = 160, mean = 77.3, sd = 0.8*sqrt(women_n)), 1)
    ),
    enzyme_a = case_when(
      gender == "Male"   ~ round(rtruncnorm(n_rec, a = 200, mean = 600, sd = 4.9*sqrt(men_n)), 0),
      gender == "Female" ~ round(rtruncnorm(n_rec, a = 100, mean = 400, sd = 4.8*sqrt(women_n)), 0)
    ),
    phys_act   = case_when(
      gender == "Male"   ~ round(rtruncnorm(n_rec, a = 0, b = 240, mean = 100, sd = 1*sqrt(men_n)), 0),
      gender == "Female" ~ round(rtruncnorm(n_rec, a = 0, b = 240, mean = 85, sd = 1.3*sqrt(women_n)), 0)
    )
  )

fup_data <- sample_n(base_data, n_rec*.84) %>%
  select(study_id, gender, screen_date, diabetes) %>% 
  mutate(
    redcap_event_name = "follow_up_arm_1",
    screen_date = screen_date + days(sample(7:49)),
    enzyme_a = case_when(
      gender == "Male"   ~ round(rtruncnorm(nrow(.), a = 200, mean = 600, sd = 4.9*sqrt(men_n)), 0),
      gender == "Female" ~ round(rtruncnorm(nrow(.), a = 100, mean = 400, sd = 4.8*sqrt(women_n)), 0)
    ),
    phys_act   = case_when(
      gender == "Male"   ~ round(rtruncnorm(nrow(.), a = 0, b = 240, mean = 100, sd = 1*sqrt(men_n)), 0),
      gender == "Female" ~ round(rtruncnorm(nrow(.), a = 0, b = 240, mean = 85, sd = 1.3*sqrt(women_n)), 0)
    ),
    diabetes = case_when(
      diabetes == 1 ~ diabetes,
      diabetes == 0 ~ sample(c(1, 0), nrow(.), replace = TRUE, prob = c(10, 90))
    )
  ) %>% 
  select(-gender)
# %>% 
# arrange(study_id)

clinic_data <- bind_rows(base_data, fup_data)

lab_data <- base_data %>% 
  select(study_id, gender) %>% 
  mutate(
    redcap_event_name = "follow_up_arm_1",
    enzyme_x = case_when(
      gender == "Male"   ~ round(rtruncnorm(nrow(.), a = 900, mean = 1400, sd = 4.9*sqrt(men_n)), 0),
      gender == "Female" ~ round(rtruncnorm(nrow(.), a = 1200, mean = 1800, sd = 4.8*sqrt(women_n)), 0)
    )
  ) %>% 
  select(-gender)

clinic_lab_data <- left_join(clinic_data %>% select(-redcap_event_name),
                             lab_data %>% select(-redcap_event_name), by = "study_id") %>% 
  filter(!is.na(gender))

clinic_data_long <- clinic_data %>%
  group_by(redcap_event_name) %>% 
  gather(key, value, -study_id, -redcap_event_name) %>% 
  filter(!is.na(value))