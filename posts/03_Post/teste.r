

## If no image links are in the original dataset, you need to assign them like so:
library(dplyr)
data <- iris %>%
  mutate(
    img = case_when(
      Species == "setosa" ~
        "https://upload.wikimedia.org/wikipedia/commons/d/d9/Wild_iris_flower_iris_setosa.jpg",
      Species == "versicolor" ~
        "https://upload.wikimedia.org/wikipedia/commons/7/7a/Iris_versicolor.jpg",
      Species == "virginica" ~
        "https://upload.wikimedia.org/wikipedia/commons/9/9f/Iris_virginica.jpg",
      TRUE ~ "NA"))

## Then use embed_img() to display images
reactable(data,
          columns = list(
            img = colDef(cell = embed_img())))

## By default, images are given a size of 24px by 24px,
## but you can adjust the size using height and width:
reactable(data,
          columns = list(
            img = colDef(cell = embed_img(height = 50, width = 45))))

## Optionally assign a label to the image from another column
reactable(data,
          columns = list(
            img = colDef(cell = embed_img(data, label = "Species"))))
