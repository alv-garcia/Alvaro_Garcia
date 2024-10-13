


library(tidyverse)
library(reactable)
library(readxl)
library(htmltools)
library(reactablefmtr)


certificacoes <-read_excel("D:/DEV/012 Blog with Quarto/Alvaro_Garcia/posts/03_Post/Certificcoes.xlsx")

certificacoes %>%
  rename(Nome = `Nome da Certificação`)-> CER01
  
CER01 %>% 
  reactable(defaultColDef = colDef(
    header = function(value) gsub(".", " ", value, fixed = TRUE),
    cell = function(value) format(value, nsmall = 1),
    align = "center",
    minWidth = 70,
    headerStyle = list(background = "#f7f7f8")
  ),
  columns = list(
    # Or using raw HTML
    Nome = colDef(html = TRUE, cell = function(value, index) {
      sprintf('<a href="%s" target="_blank">%s</a>', certificacoes$Link[index], value)
    })),
  bordered = TRUE,
  highlight = TRUE,
  defaultPageSize = 5,
  minRows = 5, searchable = TRUE
  )
    
glimpse(CER01)


unique(CER01$Escopo)
unique(CER01$Categoria)
unique(CER01$Subcategoria)



html <- function(x, inline = FALSE) {
  container <- if (inline) htmltools::span else htmltools::div
  container(dangerouslySetInnerHTML = list("__html" = x))
}



CER01 %>% 
  reactable(defaultColDef = colDef(
    header = function(value) gsub(".", " ", value, fixed = TRUE),
    align = "center",
    minWidth = 70,
    headerStyle = list(background = "#f7f7f8")
  ),
  columns = list(
    # Or using raw HTML
    Nome = colDef(html = TRUE, cell = function(value, index) {
      sprintf('<a href="%s" target="_blank">%s</a>', certificacoes$Link[index], value)
    }),
    Descrição = colDef(show = FALSE),
    Link = colDef(show = FALSE),
    Image = colDef(show = FALSE),
    OBS = colDef(show = FALSE)),
    details = function(index) {
        htmltools::tagList(
          html(CER01$Descrição[index])
        )
    }, 
  highlight = TRUE,
  defaultPageSize = 5,
  minRows = 5, searchable = TRUE
  )




glimpse(CER01)



CER01 %>% 
  reactable(defaultColDef = colDef(
    header = function(value) gsub(".", " ", value, fixed = TRUE),
    align = "center",
    minWidth = 70,
    headerStyle = list(background = "#f7f7f8")
  ),
  columns = list(
    # Or using raw HTML
    Nome = colDef(html = TRUE, cell = function(value, index) {
      sprintf('<a href="%s" target="_blank">%s</a>', certificacoes$Link[index], value)
    }),
    Descri??o = colDef(show = FALSE),
    Link = colDef(show = FALSE),
    Image = colDef(show = FALSE),
    OBS = colDef(show = FALSE),
    E = colDef(style = function(value) {
                  color <-ifelse(value == 1,"#76c893","#DAD7CD")
                  list(background = color, color = color)
                 })),
    details = function(index) {
      htmltools::tagList(
        html(CER01$Descri??o[index])
      )
    }, 
    highlight = TRUE,
    defaultPageSize = 5,
    minRows = 5, searchable = TRUE
  )


file_path <-"https://github.com/alv-garcia/AU_Images/blob/main/" 


CER01 %>% 
  reactable(defaultColDef = colDef(
    header = function(value) gsub(".", " ", value, fixed = TRUE),
    align = "center",
    minWidth = 70,
    headerStyle = list(background = "#f7f7f8")
  ),
  columns = list(
    # Or using raw HTML
    Nome = colDef(html = TRUE, cell = function(value, index) {
      sprintf('<a href="%s" target="_blank">%s</a>', certificacoes$Link[index], value)
    }),
    Descrição = colDef(show = FALSE),
    Link = colDef(show = FALSE),
    OBS = colDef(show = FALSE),
    E = colDef(style = function(value) {
      color <-ifelse(value == 1,"#76c893","#DAD7CD")
      list(background = color, color = color)
    }),
    Image = colDef(cell = image <- img(src = image_link, align = "center"))),
  details = function(index) {
    htmltools::tagList(
      html(CER01$Descrição[index])
    )
  }, 
  highlight = TRUE,
  defaultPageSize = 5,
  minRows = 5, searchable = TRUE
  )

CER01 %>% 
  select(Image_link,Nome ,Escopo,Categoria,Subcategoria,Link,Descrição,E,S,G)-> CER01


CER01 %>% 
  reactable(theme = fivethirtyeight(),
    defaultColDef = colDef(
    header = function(value) gsub(".", " ", value, fixed = TRUE),
    align = "center",
    minWidth = 70,
    headerStyle = list(background = "#f7f7f8")
  ),
  columns = list(
    # Or using raw HTML
    Nome = colDef(html = TRUE, cell = function(value, index) {
      sprintf('<a href="%s" target="_blank">%s</a>', certificacoes$Link[index], value)
    }),
    Image_link = colDef(name="logo", align="left", width= 220,cell = function(value){
      image <- img(src = paste0(value), style = "height: 50px;", alt = value)
      tagList(
        div(style = "display: inline-block;vertical-align:middle;width:70px", image),
        )}),
    Descrição = colDef(show = FALSE),
    Link = colDef(show = FALSE),
    Image = colDef(show = FALSE),
    OBS = colDef(show = FALSE),
    E = colDef(style = function(value) {
      color <-ifelse(value == 1,"#76c893","#DAD7CD")
      list(background = color, color = color)
    })),
  details = function(index) {
    htmltools::tagList(
      html(CER01$Descrição[index])
    )
  }, 
  highlight = TRUE,
  defaultPageSize = 5,
  minRows = 5, searchable = TRUE
  )

















































