#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Intiial XS Plots
#Coder: Nate Jones
#Date: 1/25/2020
#Purpose: Pltos from demo script 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

xs_ele %>% 
  filter(xs_id == 172 |
         xs_id == 130) %>% 
  ggplot(aes(x=dist, y=ele)) + 
    geom_line() +
    facet_grid(xs_id~.)
