library(DBI)
library(RSQLite)
library(tidyverse)
library(fastDummies)

statement <- '
SELECT distinct person_id,
                recidivated,
                recidivated_violent,
                prior_arrest_overall,
                prior_arrest_2yr,
                prior_arrest_5yr,
                prior_arrest_overall_violent,
                prior_arrest_2yr_violent,
                prior_arrest_5yr_violent,
                prior_prisontime_days,
                prior_jailtime_days,
                sex,
                race,
                arrest_date,
                juv_fel_count,
                juv_misd_count,
                juv_other_count,
                arrest_age_category,
                charge_degree
  FROM casearrest
  JOIN people ON casearrest.person_id = people.id
  WHERE charge_degree != \'(0)\' AND charge_degree != \'XXXXXXXXXX\'
'

# setting up our database connection
conn <- dbConnect(drv = dbDriver('SQLite'), # tells R to use SQLite
                  'Data/compas.db') # tells R the location of our .db file

arrest_history_raw <- dbGetQuery(conn = conn,
           statement = statement)



arrest_history <- arrest_history_raw %>%
  mutate(arrest_age_category = as.factor(arrest_age_category),
         sex = as.factor(sex),
         recidivated = as.factor(case_when(is.na(recidivated) ~ NA_character_,
                                 recidivated == 1 ~ 'yes',
                                 recidivated == 0 ~ 'no')),
         recidivated_violent = as.factor(case_when(is.na(recidivated_violent) ~ NA_character_,
                                 recidivated_violent == 1 ~ 'yes',
                                 recidivated_violent == 0 ~ 'no'))) %>% 
  distinct()

arrest_history <- arrest_history %>%
  dummy_columns(select_columns = c('charge_degree'), remove_selected_columns = TRUE) %>% 
  group_by(person_id, arrest_date) %>% 
  mutate(`charge_degree_(F3)` = any(`charge_degree_(F3)` == 1),
         `charge_degree_(M1)` = any(`charge_degree_(M1)` == 1),           
         `charge_degree_(F2)` = any(`charge_degree_(F2)` == 1),
         `charge_degree_(M2)` = any(`charge_degree_(M2)` == 1),
         `charge_degree_(F7)` = any(`charge_degree_(F7)` == 1),
         `charge_degree_(F1)` = any(`charge_degree_(F1)` == 1),
         `charge_degree_(MO3)` = any(`charge_degree_(MO3)` == 1),
         `charge_degree_(TCX)` = any(`charge_degree_(TCX)` == 1),
         `charge_degree_(TC4)` = any(`charge_degree_(TC4)` == 1),
         `charge_degree_(CO3)` = any(`charge_degree_(CO3)` == 1),
         `charge_degree_(F6)` = any(`charge_degree_(F6)` == 1),
         `charge_degree_(F5)` = any(`charge_degree_(F5)` == 1),
         `charge_degree_(X)` = any(`charge_degree_(X)` == 1),
         `charge_degree_(NI0)` = any(`charge_degree_(NI0)` == 1),
         `charge_degree_(CT)` = any(`charge_degree_(CT)` == 1),
         `charge_degree_(M3)` = any(`charge_degree_(M3)` == 1)) %>% 
  ungroup() %>% 
  distinct()



colnames(arrest_history) <- make.names(colnames(arrest_history))

arrest_history_violent <- arrest_history %>% 
  filter(!is.na(recidivated_violent)) %>% 
  select(-recidivated)

arrest_history <- arrest_history %>%
  filter(!is.na(recidivated)) %>% 
  select(-recidivated_violent)


write_csv(arrest_history, 'Data/arrest_history.csv')
write_csv(arrest_history_violent, 'Data/arrest_history_violent.csv')
write_csv(arrest_history_raw, 'Data/arrest_history_raw.csv')
