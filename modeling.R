library(dplyr)
library(lme4)

d=read.csv('df_model-procrastination-v1.csv')
d_model=d%>%select(-semester_clean)%>%
mutate(total_dropped_units=as.integer(round(total_dropped_units)))%>%
mutate(total_late_dropped_units=as.integer(round(total_dropped_units)))
tmp=d_model%>%select(total_dropped_units,total_late_dropped_units)
d_model=d_model%>%mutate_if(is.numeric,scale)%>%select(-total_dropped_units,-total_late_dropped_units)
d_model=cbind(d_model,tmp)

# Dropped units

m_null=glm(round(total_late_dropped_units) ~        
                    phase1_add_drop_noD_SD,
              family='poisson',d_model)

m_late=glm(round(total_late_dropped_units) ~        
                    phase1_add_drop_noD_SD +
           diff_std_normalized_btw_phase1_add_drop_ddl_all_actions_all_phases +
           relative_location_btw_phase1_add_drop_ddl_all_actions_all_phases,
              family='poisson',d_model)

m_late_type=glm(round(total_late_dropped_units) ~        
                    phase1_add_drop_noD_SD +
               diff_std_normalized_btw_phase1_add_drop_ddl_all_actions_all_phases +
           relative_location_btw_phase1_add_drop_ddl_E_all_phases +
                relative_location_btw_phase1_add_drop_ddl_D_all_phases,
              family='poisson',d_model)

m_spread_type=glm(round(total_dropped_units) ~        
                    total_late_dropped_units +
           relative_location_btw_phase1_add_drop_ddl_E_all_phases +
                relative_location_btw_phase1_add_drop_ddl_D_all_phases+
             diff_std_normalized_btw_phase1_add_drop_ddl_E_all_phases +
                  diff_std_normalized_btw_phase1_add_drop_ddl_D_all_phases,
              family='poisson',d_model)

BIC(m_null) # baseline
BIC(m_late) # general activity model
BIC(m_late_type) # dual procrastination model
BIC(m_spread_type) # dual regularity model

