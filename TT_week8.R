
tuesdata <- tidytuesdayR::tt_load(2024, week = 8)

isc_grants <- tuesdata$isc_grants


#DataExplorer::create_report(isc_grants)

library(tidyverse)
isc_grants |> count(proposed_by,sort=T)

isc_grants |> group_by(proposed_by) |>
    summarise(num_obs = n(),
              total_funding = sum(funded),
              ave_funding = total_funding/num_obs) |>
  arrange(desc(ave_funding))

# Is the length of either the title or project descripton related to the funded amount? 

isc_grants_2 <- isc_grants |> mutate(l_title=nchar(title),
                     l_summary = nchar(summary),
                     num_words_summary = str_count(summary,'\\w+'),
                     num_words_title = str_count(title,'\\w+'),
                     char_per_word_title = l_title/num_words_title,
                     char_per_word_summary = l_summary/num_words_summary
                  )


isc_grants_2 |>  pivot_longer(cols = starts_with('l'),names_to = "length_type")|>
  mutate(length_type = ifelse(length_type == "l_summary","Project Summary Length",
                              "Title Length")
         ) |>
  ggplot(aes(x=funded,y = value,color = length_type)) + 
     geom_point(size = 3,alpha=.75) +
     facet_wrap(length_type~., scales = "free_y",nrow = 2)+
      theme_bw()+
  labs(x = "Project Funding Amounts", y = "Length of Text in Number of Characters",
       title = "Comparing Lenghts of Submited Text and Project Funding")+
  theme(legend.position="none")
       
isc_grants_2 |> 
  ggplot(aes(x = l_summary,y=l_title))+
    geom_point()

cor(select(isc_grants_2,l_summary,l_title))


# Lengths of words compared to funded
# If you use more big words, do you get more money? 
isc_grants_2 |>  pivot_longer(cols = starts_with('char_'),names_to = "type")|>
  mutate(type = ifelse(type == "char_per_word_title","Number of Characters per Word in Title",
                              "Number of Characters per Word in Summary")
  ) |>
  ggplot(aes(y=funded,x = value,color = type)) + 
  geom_point(size = 3,alpha=.75) +
  facet_wrap(type~., scales = "free_x",nrow = 1)+
  theme_bw()+
  labs(y = "Project Funding Amount", x = "Number of Characters Per Word",
       title = "Complexity of Words Used Compared to Project Funding")+
  theme(legend.position="none")

