




##----------------------------- Preparacao--------------------------------


library(tidyverse)
library(readr)
library(readxl)
library(grid)
library(shadowtext)
library(waffle)
library(stringr)
library(corrplot)
library(hrbrthemes)
library(viridis)


path <- "D:/DEV/012 Blog with Quarto/Alvaro_Garcia/posts/04_Post/"

file <- "most sustainable corporations.csv"



top_100 <-read.csv(paste0(path,file),header = TRUE)

file2 <- "IND_DIM.csv"

IND_DIM_i <-read.csv2(paste0(path,file2),header = TRUE, encoding ="UTF-8")

IND_DIM <- IND_DIM_i %>% 
  rename(Industry = CKPG, Activities = `GICS Industry`, Sector = `GICS Sector`)


glimpse(top_100)

glimpse(IND_DIM)

file3 <- "Descritivo2.xlsx"

IND_DIM_i <- read_xlsx(paste0(path,file3))

glimpse(IND_DIM_i)

# Profit..,Women.on.Board..,Women.in.Leadership..,Women.in.Workforce..


top_100_di <- top_100 %>%
  mutate(Revenue = str_trim(sub('.','',Revenue)))%>%
  mutate(Revenue = str_replace_all(Revenue,",","")) %>%
  mutate(Profit.. = str_trim(sub('%','',Profit..))) %>% 
  mutate(Women.on.Board.. = str_trim(sub('%','',Women.on.Board..))) %>% 
  mutate(Women.in.Leadership.. = str_trim(sub('%','',Women.in.Leadership..))) %>%
  mutate(Women.in.Workforce.. = str_trim(sub('%','',Women.in.Workforce..))) %>% 
  mutate_at(c('Revenue','Profit..','Women.on.Board..','Women.in.Leadership..','Women.in.Workforce..'),as.double)



top_100_IND <- left_join(top_100_di,IND_DIM,by ="Industry")


top_100_IND %>% 
  group_by(Setor) %>% 
  summarise(Quantidade = n(),
            Receita_media = mean(Revenue,na.rm = TRUE),
            Lucro_Medio = mean(Profit..),
            Mulheres_lideranca = mean(Women.in.Leadership..))-> top_100_df



plt <- ggplot(top_100_df, mapping = aes(Quantidade,reorder(Setor, Quantidade)))+
  geom_col(fill = "#345F3C", width = 0.6)

plt


# The colors
BLUE <- "#076fa2"
RED <- "#E3120B"
BLACK <- "#202020"
GREY <- "grey50"


plt2 <- plt + 
  scale_x_continuous(
    limits = c(0, 40),
    breaks = seq(0,40, by = 5), 
    expand = c(0, 0), # The horizontal axis does not extend to either side
    position = "top"  # Labels are located on the top
  ) +
  # The vertical axis only extends upwards 
  scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines for the horizontal axis
    panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
    # Remove tick marks by setting their length to 0
    axis.ticks.length = unit(0, "mm"),
    # Remove the title for both axes
    axis.title = element_blank(),
    # Only left line of the vertical axis is painted in black
    axis.line.y.left = element_line(color = "black"),
    # Remove labels from the vertical axis
    axis.text.y = element_blank(),
    # But customize labels for the horizontal axis
    axis.text.x = element_text(family = "Econ Sans Cnd", size = 16)
  )

plt2




plt3 <- plt2 +
  geom_shadowtext(data = subset(top_100_df,Quantidade < 8),
                  aes(Quantidade, y = Setor, label = Setor),
                  hjust = 0,
                  nudge_x = 0.3,
                  colour = "#345F3C",
                  bg.colour = "white",
                  bg.r = 0.2,
                  family = "Econ Sans Cnd",
                  size = 7)+
  geom_text(
    data = subset(top_100_df,Quantidade >= 8),
    aes(0, y = Setor, label = Setor),
    hjust = 0,
    nudge_x = 0.3,
    colour = "white",
    family = "Econ Sans Cnd",
    size = 7
  )+
  labs(
    title = "Número de empresas por setores",
    subtitle = "Distribuição das empresas do TOP100 entre os Macro-Setores"
  )+ 
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


plt3



plt3 <- plt3 + 
  theme(
    plot.margin = margin(0.05, 0, 0.1, 0.01, "npc")
  )

# Print the ggplot2 plot
plt3

# Add horizontal line on top
# It goes from x = 0 (left) to x = 1 (right) on the very top of the chart (y = 1)
# You can think of 'gp' and 'gpar' as 'graphical parameters'.
# There we indicate the line color and width
grid.lines(
  x = c(0, 1),
  y = 1,
  gp = gpar(col = "#e5001c", lwd = 10)
)

# Add rectangle on top-left
# lwd = 0 means the rectangle does not have an outer line
# 'just' gives the horizontal and vertical justification
grid.rect(
  x = 0,
  y = 1,
  width = 0.1,
  height = 0.03,
  just = c("left", "top"),
  gp = gpar(fill = "#345F3C", lwd = 1)
)

# We have two captions, so we use grid.text instead of 
# the caption provided by  ggplot2.
grid.text(
  "Sources: Laboratory-Acquired Infection Database; American Biological Safety Association", 
  x = 0.005, 
  y = 0.06, 
  just = c("left", "bottom"),
  gp = gpar(
    col = GREY,
    fontsize = 16,
    fontfamily = "Econ Sans Cnd"
  )
)
grid.text(
  "The Economist", 
  x = 0.005, 
  y = 0.005, 
  just = c("left", "bottom"),
  gp = gpar(
    col = GREY,
    fontsize = 16,
    fontfamily = "Milo TE W01"
  )
)


##------------------- Climate Grade ------------------------------


top_100_IND %>% 
  group_by(Climate.Grade) %>% 
  summarise(Quantidade = n(),
            Receita_media = mean(Revenue,na.rm = TRUE),
            Lucro_Medio = mean(Profit..),
            Mulheres_lideranca = mean(Women.in.Leadership..))-> climate_di






##----------------------- Paises -----------------------------------------

library(rnaturalearth)
library(sf)
library(tidyverse)
library(cartogram)
library(ggforce)

#------------- tratamento da base central -----------------
top_100_IND %>% 
  mutate(Pais = sub('.*\\,','',Location)) -> top_world

glimpse(top_world)

#--------------- Mapa do mundo -----------------------------

world <- ne_countries(scale = 110, type = "countries", returnclass = "sf")%>%
  # Convert WGS84 to projected crs (here Robinson)
  sf::st_transform(world_ne, crs="ESRI:54030")

data <- read_csv('https://raw.githubusercontent.com/BjnNowak/dorling_map/main/data/FAOSTAT_land_use_2020.csv')
















