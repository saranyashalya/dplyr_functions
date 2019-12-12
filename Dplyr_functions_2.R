# REf: https://suzan.rbind.io/2018/01/dplyr-tutorial-1/

library(ggplot2)
library(tidyverse)

glimpse(msleep)

# selecting columns
msleep %>% 
  select(name, genus, vore, order) %>% 
  glimpse

msleep %>% select(name:order) %>% glimpse

msleep %>% select(-conservation, -(sleep_total:awake)) %>% glimpse

msleep %>% select(starts_with('sleep')) %>% glimpse

msleep %>% select(contains('eep'), ends_with('wt')) %>% glimpse

# selecting based on regex
msleep %>% select(matches('o.+er')) %>% glimpse

#selecting columns based on preindentified columns
classification = c('name','genus')

msleep %>% select(one_of(classification)) %>% glimpse
# or
msleep %>% select(!!classification) %>% glimpse

# select_if

msleep %>% select_if(is.numeric) %>% glimpse

#If you have to add any negation or arguments, you will have to wrap your function inside funs() or add a tilde before to remake it a function.

msleep %>% select_if(~!is.numeric(.)) %>% glimpse

msleep %>% select_if(is.numeric) %>% select_if(~mean(.,na.rm = TRUE)>10)

msleep %>% select_if(~n_distinct(.)<10)

# columns reordering

msleep %>% select(conservation, sleep_total, name) %>% glimpse

msleep %>% select(conservation, sleep_total, everything()) %>% glimpse

msleep %>% select(animal = name, sleep_total, extinct_threat = conservation) %>% glimpse

# retaining all columns and renaming

msleep %>% rename(animal = name, extinct_threat = conservation) %>% glimpse

# reformatting all columns

msleep %>% select_all(toupper)

# making an unclean database:
msleep2 <- select(msleep, name, sleep_total, brainwt)
colnames(msleep2) <- c('name','sleep total', 'brain weight')

msleep2 %>% select_all(~str_replace(., " ","_"))


msleep2 <- select(msleep, name, sleep_total, brainwt)
colnames(msleep2) <- c("Q1 name",'Q2 sleep_total', 'Q3 brain_weight')

msleep2 %>% select_all(~str_replace(.,"Q[0-9]+","")) %>%
  select_all(~str_replace(.," ","_"))

# Row names to columns

mtcars %>% head

mtcars %>% tibble::rownames_to_column("car_model") %>% head

# mutate

msleep %>% select(name, sleep_total) %>% mutate(Sleep_total_min = sleep_total *60)

msleep %>% select(name, sleep_total) %>% mutate(Sleep_total_vs_avg = sleep_total - round(mean(sleep_total),1)) %>% mutate(sleep_total_vs_min = sleep_total - min(sleep_total))

msleep %>% select(name, contains('sleep')) %>% rowwise() %>% mutate(avg = mean(c(sleep_rem, sleep_cycle)))

msleep %>% select(name, brainwt) %>% mutate(brainwt2 = ifelse(brainwt >4, NA, brainwt)) %>% arrange(desc(brainwt))

msleep %>% select(name) %>% mutate(name_last_word = tolower(str_extract(name, "\\w+$")))

msleep %>% mutate_all(tolower)

msleep_ohno <- msleep %>% mutate_all(~paste(., " \n"))

msleep_ohno[,1:4]

msleep_corr <- msleep_ohno %>% mutate_all(~str_replace_all(.,"\n" ,"")) %>% mutate_all(str_trim)

msleep_corr[1:4]

msleep %>% mutate_all(round)

msleep %>% select(name, sleep_total:bodywt) %>% mutate_if(is.numeric, round)

msleep %>% select(name, sleep_total:bodywt) %>% mutate_at(vars(contains('sleep')), ~(.*60))

msleep %>% select(name, sleep_total:bodywt) %>% mutate_at(vars(contains('sleep')), ~(.*60)) %>% rename_at(vars(contains('sleep')), ~paste0(.,"_min"))
#or

msleep %>% select(name, sleep_total:bodywt) %>% mutate_at(vars(contains('sleep')), funs(min = .*60))

# recoding discrete columns

msleep %>% mutate(conservation2 = recode(conservation, 
                                         'en' = 'Endangered',
                                         'lc' = 'Least_concern',
                                         'domesticated' = 'Least_concern',
                                         .default = 'other')) %>% count(conservation2)


msleep %>% mutate(conservation2 = recode_factor(conservation, 
                                                'en' = 'Endangered',
                                                'lc' = 'Least_concern',
                                                'domesticated' = 'Least_concern',
                                                .default = 'other',
                                                .missing ='no data',
                                                .ordered = TRUE)) %>% count(conservation2)

msleep %>% select(name, sleep_total) %>% mutate(sleep_time = ifelse(sleep_total>10,"long",'short'))

msleep %>% select(name, sleep_total) %>% mutate(sleep_total_discr = case_when(
                                                                      sleep_total>13 ~ 'Very long',
                                                                      sleep_total > 10 ~ 'long',
                                                                      sleep_total >7 ~ 'limited',
                                                                      TRUE ~ 'short'
                                                                            )) %>% mutate(sleep_total_discr = factor(sleep_total_discr,
                                                                                                                     levels = c('short','limited','long','Very long')))

msleep %>% mutate(silly_groups = case_when(
            brainwt <0.001 ~'light_headed',
            sleep_total >10 ~'lazy_sleeper',
            is.na(sleep_rem) ~'absent_rem',
            TRUE ~ 'other'
)) %>% count(silly_groups)


msleep %>%
  select(name, contains("sleep")) %>%
  gather(key = "sleep_measure", value = "sleep_rem", -name)

(msleep_g <- msleep %>%
    select(name, contains("sleep")) %>%
    gather(key = "sleep_measure", value = "sleep_rem", -name, factor_key = TRUE))

head(msleep_g)


msleep_g %>% spread(sleep_measure, sleep_rem)

msleep %>% select(name:order) %>% na_if('omit')

msleep %>% select(name, sleep_total) %>% filter(sleep_total >18 )

msleep %>% select(name, sleep_total) %>% filter(between(sleep_total, 16, 18))

msleep %>% select(name, sleep_total) %>% filter(near(sleep_total, 17, tol = sd(sleep_total)))

msleep %>% select(order, name, sleep_total) %>% filter(order == 'Didelphimorphia')

msleep %>% 
  select(order, name, sleep_total) %>% 
  filter(order %in% c("Didelphimorphia", "Diprotodontia"))


remove <- c("Rodentia", "Carnivora", "Primates")

msleep %>% select(order, name, sleep_total) %>% filter(!order %in% remove)


msleep %>% select(name, sleep_total) %>% filter(str_detect(name, pattern ='mouse'))
  
msleep %>% 
  select(name, order, sleep_total:bodywt) %>% 
  filter(bodywt > 100, (sleep_total > 15 | order != "Carnivora"))                                              

msleep %>%
  select(name, bodywt:brainwt) %>% 
  filter(xor(bodywt > 100, brainwt > 1))

msleep %>% 
  select(name, sleep_total, brainwt, bodywt) %>% 
  filter(brainwt > 1, !bodywt > 100)

msleep %>% 
  select(name, conservation:sleep_cycle) %>% 
  filter(!is.na(conservation))

msleep %>% 
  select(name:order, sleep_total, -vore) %>% 
  filter_all(any_vars(str_detect(., pattern = "Ca")))

msleep %>%  
  select(name, sleep_total:bodywt) %>% 
  filter_all(any_vars(. < 0.1))

msleep %>%  
  select(name, sleep_total:bodywt, -awake) %>% 
  filter_all(all_vars(. > 1))

msleep %>% 
  select(name:order, sleep_total:sleep_rem) %>% 
  filter_if(is.character, any_vars(is.na(.)))

msleep %>% 
  select(name, sleep_total:sleep_rem, brainwt:bodywt) %>% 
  filter_at(vars(sleep_total, sleep_rem), all_vars(.>5))

msleep %>% 
  select(name, sleep_total:sleep_rem, brainwt:bodywt) %>% 
  filter_at(vars(contains("sleep")), all_vars(.>5))

msleep %>%
  count(order, sort = TRUE)

msleep %>%
  count(order, vore, sort = TRUE)

msleep %>%
  tally()

msleep %>%
  select(1:3) %>%
  add_tally()

msleep %>%
  select(name:vore) %>%
  add_count(vore)

msleep %>%
  summarise(n = n(), average = mean(sleep_total), maximum = max(sleep_total))

msleep %>%
  group_by(vore) %>%
  summarise(n = n(), average = mean(sleep_total), maximum = max(sleep_total))

msleep %>%
  group_by(vore) %>%
  summarise(avg_sleep_day = mean(sleep_total)/24)

msleep %>%
  group_by(vore) %>%
  summarise_all(mean, na.rm=TRUE)


msleep %>%
  group_by(vore) %>%
  summarise_all(~mean(., na.rm = TRUE) + 5)

msleep %>%
  group_by(vore) %>%
  summarise_if(is.numeric, mean, na.rm=TRUE)

msleep %>%
  group_by(vore) %>%
  summarise_if(is.numeric, mean, na.rm=TRUE) %>%
  rename_if(is.numeric, ~paste0("avg_", .))

msleep %>%
  group_by(vore) %>%
  summarise_at(vars(contains("sleep")), mean, na.rm=TRUE) %>%
  rename_at(vars(contains("sleep")), ~paste0("avg_", .))


msleep %>%
  group_by(vore) %>%
  summarise(avg_sleep = mean(sleep_total)) %>%
  arrange(desc(avg_sleep))


msleep %>%
  select(order, name, sleep_total) %>%
  group_by(order) %>%
  arrange(desc(sleep_total), .by_group = TRUE)

msleep %>%
  group_by(order) %>%
  summarise(average = mean(sleep_total)) %>%
  top_n(5)

msleep %>%
  group_by(order) %>%
  summarise(average = mean(sleep_total)) %>%
  top_n(-5)

msleep %>%
  group_by(order) %>%
  summarise(average_sleep = mean(sleep_total), max_sleep = max(sleep_total)) %>%
  top_n(5, average_sleep)

msleep %>%
  sample_frac(.1)

msleep %>%
  slice(50:55)
