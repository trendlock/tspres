


# seasonal adjustment

library(forecast)
library(sweep)

fit_stl <- USAccDeaths %>%
  stl(s.window = "periodic")

x <- sw_tidy_decomp(fit_stl)

ggplot(x) +
  geom_line(aes(index, observed, col = "A")) +
  geom_line(aes(index, seasadj, col = "s"))


library(readxl)

abs640111 <- read_xls("extdata/640111.xls", sheet = 2)
abs640111 <- abs640111 %>% 
  select("X__1", "Index Numbers ;  Food and non-alcoholic beverages ;  Australia ;") %>% 
  `colnames<-`(c("index", "Food and non-alcoholic beverages: SeasAdj")) %>% 
  tail(nrow(.) - 10)


abs640102 <- read_xls("extdata/640102.xls", sheet = 2)
abs640102 <- abs640102 %>% 
  select("X__1", "Index Numbers ;  Food and non-alcoholic beverages ;  Australia ;") %>% 
  `colnames<-`(c("index", "Food and non-alcoholic beverages: Original")) %>% 
  tail(nrow(.) - 10)


food <- left_join(
  abs640102,
  abs640111,
  by = "index"
) %>% 
  mutate(index = as.POSIXct(as.numeric(index) * (60*60*24), origin="1923-10-1", format="%Y-%m-%d", tz="GMT"))



food <- food %>% 
  mutate_at(vars(`Food and non-alcoholic beverages: Original`, `Food and non-alcoholic beverages: SeasAdj`), as.numeric)

ggplot(food) +
  geom_line(aes(index, `Food and non-alcoholic beverages: Original`), col = ow)+
  geom_line(aes(index, `Food and non-alcoholic beverages: SeasAdj`), col = ow)
