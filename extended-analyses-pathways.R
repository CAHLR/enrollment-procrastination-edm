library(lme4)
library(lmerTest)
library(dplyr)
library(magrittr)
library(lavaan)

d=read.csv('/data/groups/CTd/jedm-analysis-dataset-growthcurve-v2.csv') %>%
  mutate(p = enrollment_procrastination) %>% 
  mutate(p_l = lag(enrollment_procrastination, 1)) %>%
  mutate(d = late_dropped_units) %>%
  mutate(d_l = lag(late_dropped_units, 2)) %>%
  group_by(student_id) %>%
  mutate(procrastination_centered = enrollment_procrastination - mean(enrollment_procrastination)) %>%
  mutate(cl_centered = cl - mean(cl)) %>%
  mutate(ch_centered = ch - mean(ch)) %>%
  mutate(cla_disc_centered = cla_disc - mean(cla_disc)) %>%
  mutate(drops_centered = late_dropped_units - mean(late_dropped_units, na.rm=TRUE)) %>%
  ungroup() %>%
  arrange(student_id, semester_clean) %>%
  mutate(drops_centered = scale(drops_centered)) %>%
  mutate(drops_centered = scale(drops_centered)) %>%
  mutate(lag_drops_centered = lag(drops_centered, 1)) %>%
  mutate(lag_procrastination_centered = lag(procrastination_centered, 1)) %>%
  mutate(lag_drops_centered = ifelse(student_id!=lag(student_id, 1), NA, lag_drops_centered)) %>%
  mutate(lag_procrastination_centered = ifelse(student_id!=lag(student_id, 1), NA, lag_procrastination_centered))

### CLPM

# Prepare the model object
d_model <- d %>%
  select(student_id, semester_count, cl_centered, procrastination_centered, lag_procrastination_centered,
         drops_centered, lag_drops_centered) %>%
  tidyr::drop_na()

# CLPM model formula
model <- '
  # Stability and cross-lagged effects
  procrastination_centered ~ lag_drops_centered + lag_procrastination_centered
  drops_centered ~ procrastination_centered + lag_drops_centered

  # Covariances
  procrastination_centered ~~ drops_centered
  lag_procrastination_centered ~~ lag_drops_centered
'

# Fit the model with clustering
fit <- sem(model, data = d_model, cluster = "student_id", se = "robust") # Robust SE for repeated obs

# Summarize results
summary(fit, fit.measures = TRUE, standardized = TRUE)

BIC(fit)

# CLPM Interaction model

# Compute interaction terms in your data
d_model <- d %>%
  mutate(
    interaction_lag_drops_cl = lag_drops_centered * cl_centered,
    interaction_procrastination_cl = procrastination_centered * cl_centered,
    interaction_lag_drops_semester = lag_drops_centered * semester_count,
    interaction_procrastination_semester = procrastination_centered * semester_count
  ) %>%
  select(
    student_id, procrastination_centered, lag_procrastination_centered,
    drops_centered, lag_drops_centered,
    cl_centered, semester_count,
    i_ld_cl = interaction_lag_drops_cl, i_p_cl = interaction_procrastination_cl,
    i_ld_s = interaction_lag_drops_semester, i_p_s = interaction_procrastination_semester
  ) %>%
  tidyr::drop_na()

# Model with interaction terms
model <- '
  # Stability and cross-lagged effects
  procrastination_centered ~ lag_drops_centered + lag_procrastination_centered +
                   i_ld_cl + 
                   i_ld_s
  drops_centered ~ procrastination_centered + lag_drops_centered +
                   i_p_cl +
                   i_p_s

  # Covariances
  procrastination_centered ~~ drops_centered
  lag_procrastination_centered ~~ lag_drops_centered
'

# Fit the model with clustering
fit <- sem(model, data = d_model, cluster = "student_id", se = "robust")

# Summarize results
summary(fit, fit.measures = TRUE, standardized = TRUE)

BIC(fit)

## Replication with multi-level models

# Part 1 hypothesis procrastination --> late drops
m1=lmer(drops_centered~(1|student_id)+procrastination_centered+semester_count+lag_drops_centered, d, verbose=2)
coef(summary(m1)) %>% as.data.frame() %>% mutate_if(is.numeric, round, 2)

# Part 2 hypothesis late drops --> procrastination 
m1=lmer(procrastination_centered~(1|student_id)+lag_drops_centered+semester_count+lag(procrastination_centered, 1), d, verbose=2)
coef(summary(m1)) %>% as.data.frame() %>% mutate_if(is.numeric, round, 3)

# Interaction models
m5=lmer(drops_centered~(1|student_id)+procrastination_centered*semester_count*ch_centered+lag_drops_centered, model.frame(m5), verbose=2, control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)), na.action = na.exclude, REML=FALSE)
m1=lmer(drops_centered~(1|student_id)+procrastination_centered+semester_count+lag_drops_centered, model.frame(m5), verbose=2, control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)), na.action = na.exclude, REML=FALSE)
m2=lmer(drops_centered~(1|student_id)+procrastination_centered*semester_count+lag_drops_centered, model.frame(m5), verbose=2, control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)), na.action = na.exclude, REML=FALSE)
m3=lmer(drops_centered~(1|student_id)+procrastination_centered*semester_count+ch_centered+lag_drops_centered, model.frame(m5), verbose=2, control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)), na.action = na.exclude, REML=FALSE)
m4=lmer(drops_centered~(1|student_id)+procrastination_centered*semester_count+ch_centered* procrastination_centered+lag_drops_centered, model.frame(m5), verbose=2, control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)), na.action = na.exclude, REML=FALSE)

BIC(m1);BIC(m2);BIC(m3);BIC(m4)
