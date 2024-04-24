{r, echo=FALSE, message=FALSE, warning=FALSE}
#| fig-cap: "Anomalia da temperatura média global ( Fonte dos dados: Our Wolrd in Data)"
#| fig-cap-location: margin
#| label: fig-plotly
#| column: page-inset-right
#| echo: false
#| message: false


####--------------------------------- Primeiro grafico ----------------------------------------

library(htmlwidgets)
library(tidyverse)
library(readxl)
library(lubridate)
library(hrbrthemes)
library(plotly)

mudancas <- read.csv(file = "D:/DEV/012 Blog with Quarto/Alvaro_Garcia/posts/02_Post/climate-change.csv")

mudancas %>% 
  filter(Entity == "World") %>% 
  select(Entity, Date, Temperature.anomaly) %>%
  mutate(Date = ymd(Date)) %>% 
  drop_na()-> mud_tratado

g<-ggplot(mud_tratado)+
  geom_line(mapping = aes(x = Date , y= Temperature.anomaly, group = Entity),size= 0.2, color = "darkolivegreen")+
  scale_x_date(date_labels = "%Y", date_breaks = "10 years")+
  labs(y = "Temperature (°C)")+
  geom_hline(yintercept= 0, color="red", size=.5)+
  theme_ipsum()

fig<-ggplotly(g) %>% 
  layout(title = list(text = paste0('Anomalia da temperatura média Global',
                                    '<br>',
                                    '<sup>',
                                    'A temperatura normal é calculada com base na média do período de 1951 - 1980.',
                                    '</sup>')),
         xaxis = list(title = "Data"),
         yaxis = list (title = "Anomalia da Temperatura (°C )"))

fig


### ----------------------------- Segundo grafico ----------------------------------------


library(htmlwidgets)
library(tidyverse)
library(readxl)
library(lubridate)
library(hrbrthemes)
library(plotly)
library(viridis)


annual_emissions <- read.csv(file = "D:/DEV/012 Blog with Quarto/Alvaro_Garcia/posts/02_Post/annual-co-emissions-by-region.csv")

glimpse(annual_emissions)

annual_emissions %>% 
  filter(Code =="", Entity %in% c('Africa','Asia','Europe','North America','South America','Oceania','International aviation',
    'International shipping'),Year>=1850) %>% mutate(Emissions = Annual.COâ...emissions/10^9)->by_continent

glimpse(by_continent)
unique(by_continent$Entity)


g2<-ggplot(by_continent, mapping = aes(x = Year , y = Emissions, fill = Entity))+
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_y_continuous(label = scales::label_number(suffix = " Bilhões t"))+
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  ggtitle("Emissões anuais do mundo de CO2")+
  guides(fill=guide_legend(title="Região do mundo"))


fig2<-ggplotly(g2) %>% 
  layout(xaxis = list(title = "Ano"),
         yaxis = list (title = "Emissões de CO2"))


fig2

### ----------------------------- Segundo grafico - Mapa ----------------------------------------


library(htmlwidgets)
library(tidyverse)
library(readxl)
library(lubridate)
library(hrbrthemes)
library(plotly)
library(viridis)


emissions_countrys <- read.csv(file = "D:/DEV/012 Blog with Quarto/Alvaro_Garcia/posts/02_Post/annual-co-emissions-by-region.csv")

glimpse(emissions_countrys)

# Emissoes em milhares de toneladas de CO2
emissions_countrys %>% 
  filter(Code != "", Year %in% c(1950,1960,1970,1980,1990,2000,2010,2020,2022)) %>% 
  mutate(Emissions = Annual.COâ...emissions/10^3) %>%
  rename(Country = Entity,iso3 = Code) %>% 
  select(Country,iso3,Year,Emissions)->Country_data

fig <- plot_ly(Country_data, type='choropleth', locations=Country_data$iso3,
               z=Country_data$Emissions, text=Country_data$Country)

fig

  
# light grey boundaries
l <- list(color = toRGB("steelblue"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)



Country_data %>% filter(Country != "World" , Country !="Antarctica")-> Data_2022

fig <- plot_geo(Data_2022)
fig <- fig %>% add_trace(type="choropleth",
  z = Data_2022$Emissions,
  text = Data_2022$Country,
  locations = Data_2022$iso3,
  colorscale="Viridis",
  frame = Data_2022$Year,
  marker = list(line = l))

fig <- fig %>% colorbar(title = 'Emissoes anuais', ticksuffix = ' t CO2')
fig <- fig %>% layout(
  title = 'Emissoes Mundiais por país desde 1950',
  geo = g
)

fig <- fig %>% animation_opts(
    frame = 100, 
    transition = 100, 
    redraw = FALSE
  )

fig <-fig %>% animation_slider(
  currentvalue = list(
    prefix = "Ano "
  )
)

fig



##--------------------------- Terceiro Gráfico - Setores -----------------------


library(htmlwidgets)
library(tidyverse)
library(readxl)
library(lubridate)
library(hrbrthemes)
library(plotly)
library(viridis)



emissions_sectors <- read.csv(file = "D:/DEV/012 Blog with Quarto/Alvaro_Garcia/posts/02_Post/emissions_sector.csv")

glimpse(emissions_sectors)

unique(emissions_sectors$Year)


# Tratando os dados

emissions_sectors %>% 
  filter(Entity=="World") %>% 
  pivot_longer(cols = Agriculture : bunker.fuels ,
               names_to = "Setores",
               values_to = "Emissoes") ->Setores


unique(Setores$Setores)

Setores %>% 
  mutate(
    Sector = case_when(
      Setores == "land.use.change.and.forestry" ~ "Mudança do uso do solo e silvicultura",
      Setores == "Agriculture" ~ "Agricultura",
      Setores == "Waste" ~ "Resíduos",
      Setores == "Buildings" ~"Edificações",
      Setores == "Industry" ~ "Industria",
      Setores == "manufacturing.and.construction" ~ "Fabricação e construção",
      Setores == "transport"~ "Transporte",
      Setores == "electricity.and.heat" ~"Eletricidade e aquecimento",
      Setores == "Fugitive.emissions.energy.production" ~ "Emissões Fugitivas da produção de energia",
      Setores == "other.fuel.combustion" ~ "Combustão de outros combustíveis")
  ) %>% 
  mutate(Emissions = Emissoes/10^9)-> Setores_trat



g2<-ggplot(Setores_trat, mapping = aes(x = Year , y = Emissions, fill = Sector))+
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_y_continuous(label = scales::label_number(suffix = "Bilhões t CO2e"))+
  scale_fill_viridis(discrete = T, option = "A") +
  theme_ipsum() + 
  ggtitle("Emissões de CO2 por setores da economia")+
  guides(fill=guide_legend(title="Setor da economia"))


fig2<-ggplotly(g2) %>% 
  layout(xaxis = list(title = "Ano"),
         yaxis = list (title = "Emissões de CO2"))


fig2

##--------------------------- Quarto Grafico - Setores  -----------------------

library(htmlwidgets)
library(tidyverse)
library(readxl)
library(lubridate)
library(hrbrthemes)
library(plotly)
library(viridis)



Setores_trat %>% 
  filter(Year == 2020) -> Setores_2020



fig<- Setores_2020 %>% plot_ly()

fig<- fig %>% add_trace(x = Setores_2020$Sector, y = Setores_2020$Emissions, type = "bar")

fig

sum(Setores_2020$Emissions)
















##--------------------------- Quarto Grafico - Setores (Erro) -----------------------

library(htmlwidgets)
library(tidyverse)
library(readxl)
library(lubridate)
library(hrbrthemes)
library(plotly)
library(viridis)
library(data.table)

emissions_sectors %>%
  filter(Entity %in% c("Asia","Africa","Europe","North America","South America","Australia")) %>% 
  pivot_longer(cols = Agriculture : bunker.fuels ,
               names_to = "Setores",
               values_to = "Emissoes") ->Setores02

glimpse(Setores_trat02)
unique(Setores02$Entity)

Setores02 %>% 
  mutate(
    Sector = case_when(
      Setores == "land.use.change.and.forestry" ~ "Mudança do uso do solo e silvicultura",
      Setores == "Agriculture" ~ "Agricultura",
      Setores == "Waste" ~ "Resíduos",
      Setores == "Buildings" ~"Edificações",
      Setores == "Industry" ~ "Industria",
      Setores == "manufacturing.and.construction" ~ "Fabricação e construção",
      Setores == "transport"~ "Transporte",
      Setores == "electricity.and.heat" ~"Eletricidade e aquecimento",
      Setores == "Fugitive.emissions.energy.production" ~ "Emissões Fugitivas da produção de energia",
      Setores == "other.fuel.combustion" ~ "Combustão de outros combustíveis")
  ) %>% 
  mutate(Emissions = Emissoes/10^9) %>% 
  mutate(id =row_number()) %>% 
  filter(Year == 2020, Sector!="NA") %>% 
  select(Entity,Sector,Emissions)-> Setores_trat03


fig <- plot_ly()

fig <- fig %>% add_trace( 
  ids = Setores_trat02$id , 
  labels = Setores_trat02$Entity,
  parents = Setores_trat02$Sector,
  type = 'sunburst',)

fig


as.sunburstDF <- function(DF, value_column = NULL, add_root = FALSE){
  require(data.table)
  
  colNamesDF <- names(DF)
  
  if(is.data.table(DF)){
    DT <- copy(DF)
  } else {
    DT <- data.table(DF, stringsAsFactors = FALSE)
  }
  
  if(add_root){
    DT[, root := "Total"]  
  }
  
  colNamesDT <- names(DT)
  hierarchy_columns <- setdiff(colNamesDT, value_column)
  DT[, (hierarchy_columns) := lapply(.SD, as.factor), .SDcols = hierarchy_columns]
  
  if(is.null(value_column) && add_root){
    setcolorder(DT, c("root", colNamesDF))
  } else if(!is.null(value_column) && !add_root) {
    setnames(DT, value_column, "values", skip_absent=TRUE)
    setcolorder(DT, c(setdiff(colNamesDF, value_column), "values"))
  } else if(!is.null(value_column) && add_root) {
    setnames(DT, value_column, "values", skip_absent=TRUE)
    setcolorder(DT, c("root", setdiff(colNamesDF, value_column), "values"))
  }
  
  hierarchyList <- list()
  
  for(i in seq_along(hierarchy_columns)){
    current_columns <- colNamesDT[1:i]
    if(is.null(value_column)){
      currentDT <- unique(DT[, ..current_columns][, values := .N, by = current_columns], by = current_columns)
    } else {
      currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by=current_columns, .SDcols = "values"]
    }
    setnames(currentDT, length(current_columns), "labels")
    hierarchyList[[i]] <- currentDT
  }
  
  hierarchyDT <- rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)
  
  parent_columns <- setdiff(names(hierarchyDT), c("labels", "values", value_column))
  hierarchyDT[, parents := apply(.SD, 1, function(x){fifelse(all(is.na(x)), yes = NA_character_, no = paste(x[!is.na(x)], sep = ":", collapse = " - "))}), .SDcols = parent_columns]
  hierarchyDT[, ids := apply(.SD, 1, function(x){paste(x[!is.na(x)], collapse = " - ")}), .SDcols = c("parents", "labels")]
  hierarchyDT[, c(parent_columns) := NULL]
  return(hierarchyDT)
}


sunburstSector <-as.sunburstDF(Setores_trat03,value_column = "Emissions", add_root = TRUE)

head(sunburstSector)


plot_ly(data = sunburstSector, ids = ~ids, labels= ~labels,
        parents = ~parents, values= ~values, type='treemap', branchvalues = 'total')


su










