

library(tidyverse)
library(readr)
library(grid)
library(shadowtext)
library(waffle)


path <- "D:/DEV/012 Blog with Quarto/Alvaro_Garcia/posts/04_Post/"

file <- "most sustainable corporations.csv"



top_100 <-read.csv(paste0(path,file),header = TRUE)

glimpse(top_100)
glimpse(top_industrys)


top_100 %>% 
  group_by(Industry) %>%
  summarise(Quantidade = n()) %>% 
  mutate(ranking = rank(desc(Quantidade))) %>% 
  filter(ranking <= 10)->top_industrys


BLUE <- "#076fa2"
RED <- "#E3120B"
BLACK <- "#202020"
GREY <- "grey50"



plt <-ggplot(top_industrys)+
  geom_col(mapping = aes(Quantidade,y = fct_reorder(Industry, Quantidade)), width = 0.6)

plt


plt <-ggplot(top_industrys)+
  geom_col(mapping = aes(x = fct_reorder(Industry, Quantidade),Quantidade), width = 0.6)

plt

plt <- plt +
  labs(
    title = "Escape artists",
    subtitle = "Number of laboratory-acquired infections, 1970-2021"
  ) + 
  theme(
    plot.title = element_text(
      family = "Econ Sans Cnd", 
      face = "bold",
      size = 22
    ),
    plot.subtitle = element_text(
      family = "Econ Sans Cnd",
      size = 20
    )
  )
plt



waffle()



















