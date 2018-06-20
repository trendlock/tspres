


library(tidyverse)
library(lubridate)

library(readxl)

library(timetk)
library(sweep)



lab_force_df <- read_xls("extdata/6202001.xls", sheet = 2) %>% 
  select(X__1, `Employed total ;  Persons ;__2`, `Employed total ;  Persons ;__1`) %>% 
  `colnames<-`(c("index", "total.lb.frc.original", "total.lb.frc.seasadj")) %>% 
  tail(nrow(.) - 9) %>% 
  mutate(index = as.POSIXct(as.numeric(index) * (60*60*24), origin="1899-12-30", format="%Y-%m-%d", tz="GMT"),
         index = floor_date(index, "month")) %>% 
  mutate_at(vars(total.lb.frc.original, total.lb.frc.seasadj), as.numeric)


lab_force_df %>% 
  gather(key, val, -index) %>% 
  ggplot(aes(index, val, col = key)) +
  geom_line()


lab_force_df %>% 
  tail(100) %>% 
  gather(key, val, -index) %>% 
  ggplot(aes(index, val, col = key)) +
  geom_line()


# convert to ts
lab_force_ts <- lab_force_df %>%
  pull(total.lb.frc.original) %>% 
  ts(start = c(1978,2), frequency = 12)


# run a few options through stl
seasadj_compare_df <- tibble(params = list(7, 9, 11, 13)) %>% 
  mutate(stl = map(params, ~ stl(lab_force_ts, s.window = .x)),
         stl = map(stl, sw_tidy_decomp),
         stl = map(stl, select, seasadj),
         stl = map(stl, mutate, index = lab_force_df$index)) %>% 
  mutate(params = as.character(params)) %>% 
  unnest(stl) %>% 
  bind_rows(
    lab_force_df %>% 
      select(index, total.lb.frc.seasadj) %>%
      rename(seasadj = total.lb.frc.seasadj) %>% 
      mutate(params = "abs")
  ) %>% 
  spread(params, seasadj) %>% 
  tail(40) %>% 
  gather(params, seasadj, -index)



ggplot(seasadj_compare_df, aes(index, seasadj, col = params)) +
  geom_line(size = 1)
