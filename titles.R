library(magick)
library(MetBrewer)
library(colorspace)
library(glue)
library(stringr)
library(ggplot2)


image <- image_read("images/finalplot1.png")
image

colors <- met.brewer("Tam")
swatchplot(colors)

text_color <- darken(colors[6], .25)
swatchplot(text_color)

annot <- glue("This map shows population density of Sachsen. ",
              "Population estimates are bucketed into 400 meter (about 1/4 mile) ",
              "hexagons.") %>% 
  str_wrap(45)



image %>% 
  image_crop(gravity = "center",
             geometry = "6000x4000") %>% 
  image_annotate("Sachsen Population Density",
                 gravity = "northwest",
                 location = "+300+300",
                 color = text_color,
                 size = 250,
                 font = "serif",
                 style = "italic",
                 weight = 700) %>% 
  image_annotate(annot,
                 gravity = "southeast",
                 location = "+250+600",
                 color = text_color,
                 size = 125,
                 font = "Palatino") %>% 
  image_annotate(glue("By Rajashekar Komatireddy |",
                      " Data: Kontur Population (Released 2022-06-30) |",
                      " Inspired by Spencer Schien"),
                 gravity = "south",
                 location = "+0+100",
                 font = "Palatino",
                 style = "italic",
                 color = alpha(text_color, .5),
                 size = 70) %>% 
  image_write("images/titled_final_plot.png")

