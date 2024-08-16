library(lme4)
library(lmerTest)
library(dplyr)
library(magrittr)

d=read.csv('/data/groups/CTd/jedm-analysis-dataset-growthcurve-v2.csv') %>%
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
