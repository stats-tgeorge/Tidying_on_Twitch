
library(tidyverse)

# Load data
tuesdata <- tidytuesdayR::tt_load(2024, week = 16)


shiny_revdeps <- tuesdata$shiny_revdeps
package_details <- tuesdata$package_details

colnames(package_details)


package_details |> count(Copyright)

package_details |> count(License)


library(ggiraph)

my_bar_plot<- package_details |>
  separate(col = License,into = c("left","right"),sep = " | ",
           extra="merge",fill="left")|>
  drop_na(left) |>
  count(left) |>
  #filter(n>10) |>
  slice_max(n=10,order_by = n)|>
  
ggplot(aes(x=n,y=reorder(left,n)))+
    geom_bar_interactive(aes(tooltip = paste0("Count: ",n)),
      stat = "identity", fill = "#007BC2")+
      labs(y = "License",x = "Frequency",title = "Top 10 Most Common Licenses",
         subtitle = "Of Shiny Child Packages",caption = "Source: Tidy Tuesday")+
      theme_bw()

girafe(ggobj = my_bar_plot)
