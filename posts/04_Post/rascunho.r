

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


industrys <- unique(top_100$Industry)
write.csv(industrys,"industrys.csv")



##-------------------------------------- Tratamento de dados---------------------------------------------------------------

# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
# Profit..,Women.on.Board..,Women.in.Leadership..,Women.in.Workforce..
library(stringr)
library(corrplot)
library(hrbrthemes)
library(viridis)


glimpse(top_100)

top_100_di <- top_100 %>%
  mutate(Revenue = str_trim(sub('.','',Revenue)))%>%
  mutate(Revenue = str_replace_all(Revenue,",","")) %>%
  mutate(Profit.. = str_trim(sub('%','',Profit..))) %>% 
  mutate(Women.on.Board.. = str_trim(sub('%','',Women.on.Board..))) %>% 
  mutate(Women.in.Leadership.. = str_trim(sub('%','',Women.in.Leadership..))) %>%
  mutate(Women.in.Workforce.. = str_trim(sub('%','',Women.in.Workforce..)))%>%
  mutate_at(c('Revenue','Profit..','Women.on.Board..','Women.in.Leadership..','Women.in.Workforce..'),as.double)


Top_100_Corr<-top_100_di %>%
  select(Rank,Revenue,Profit..,Women.on.Board..,Women.in.Leadership..,Women.in.Workforce..) %>% 
  rename(Receita = Revenue, Lucro = Profit..,'Mulheres na Diretoria'= Women.on.Board..,
         'Mulheres na Liderança' = Women.in.Leadership..,'Mulheres na Força de Trabalho'= Women.in.Workforce..) %>% 
  filter(Receita != "NA")

M <- cor(Top_100_Corr)

corrplot(M, method = 'color', order = 'alphabet',
         col = COL1('YlGn'), cl.pos = 'b', addgrid.col = 'white', addCoef.col = 'grey50')



top_100_di %>% 
  select(Climate.Grade,Women.on.Board..,Women.in.Leadership..,Women.in.Workforce..) %>% 
  group_by(Climate.Grade) %>% 
  summarise('Direção' = mean(Women.on.Board..),
            'Liderança' = mean(Women.in.Leadership..),
            'Força de Trabalho' = mean(Women.in.Workforce..)) %>% 
  pivot_longer(cols = 'Direção':'Força de Trabalho', names_to = "Variavel", values_to = "Media")->top_100_heat



ggplot(top_100_heat %>% filter(Climate.Grade != ""),
       mapping = aes(Variavel,factor(Climate.Grade,levels =c("A+","A","A-","B+","B","B-","C+","C","C-")),fill= Media))+
  geom_tile()+
  #geom_text(aes(label =format(round(Media,1),nsmall = 1)))+
  scale_y_discrete(limits=rev)+
  scale_fill_gradient2(low = "#FFFFFF",mid = "#899A5C", high="#345F3C" , midpoint = 50) +
  labs(x = "Participação Feminina na companhia",
       y = "Climate Grade",
       title = "Climate Grade e equidade de gênero",
       subtitle = "Empresas mais igualitárias apresentam melhores resultados",
       caption = "Alvaro Garcia, Fonte: Corporate knights, 2024",
       fill = "Percentual\nde participação\nFeminina (%):")+
  theme(legend.title = element_text(size = 10, face = "bold"))+
  theme_ipsum()


top_100_di %>% 
  filter(Women.in.Workforce.. >= 70)




  



