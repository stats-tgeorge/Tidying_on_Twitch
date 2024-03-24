
# tidytuesday 3-19-2024

library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2024-03-19')

mutant_moneyball <- tuesdata$mutant_moneyball


glimpse(mutant_moneyball)

cur_clean <- function(x){
  as.numeric(trimws(str_remove_all(str_remove_all(x,","),"\\$")))
}


cleaned_mutant <- mutant_moneyball %>%
  #mutate(column_with_dollars = str_replace(column_with_dollars, "\\$", ""))
  
  mutate(across(contains(c("PPI","wiz","ostreet")), ~cur_clean(.)),
         across(contains("Appearance"),~as.numeric(str_remove(.,'\\%')))) %>%
  janitor::clean_names()

cleaned_mutant %>% ggplot(aes(x= x60s_appearance_percent))+
  geom_point(aes(y=total_value60s_heritage),alpha = .5,size=3) +
  geom_point(aes(y=total_value60s_ebay), color = "#E43137",alpha = .5,size=3) +
  geom_point(aes(y=total_value60s_wiz),color = "blue",alpha = .5,size=3)+
  geom_point(aes(y=total_value60s_o_street),color = "#FFAF00",alpha = .5,size=3)+
  theme_classic()


cleaned_mutant %>% ggplot(aes(x= x60s_appearance_percent))+
  geom_point(aes(y=ppi60s_heritage),alpha = .5,size=3) +
  geom_point(aes(y=ppi60s_ebay), color = "#E43137",alpha = .5,size=3) +
  geom_point(aes(y=ppi60s_wiz),color = "blue",alpha = .5,size=3)+
  geom_point(aes(y=ppi60s_o_street),color = "#FFAF00",alpha = .5,size=3)+
  theme_classic() +
  labs(y = "Average $ Per Issue",
       x = "Percentage each X-Men member appeared in an issue")
