library(ggplot2)
library(hexSticker)
library(dplyr)
library(cowplot)

noaa <- magick::image_read("https://www.noaa.gov/sites/default/files/2022-03/noaa_digital_logo-2022.png") %>%
  magick::image_colorize(65,"white")

test_image1 <- ggdraw() +
  draw_image(noaa)

microscope <- magick::image_read("https://static.vecteezy.com/system/resources/previews/007/165/321/original/microscope-icon-microscope-icon-simple-sign-microscope-icon-free-free-vector.jpg") %>%
  magick::image_resize("400x400")

test_image2 <- ggdraw() +
  draw_image(microscope)

hexSticker::sticker(
  filename = "inst/figures/logo.png",
  subplot = test_image2,
  s_width = 1.25, s_height = 1.5,
  s_x = 1, s_y = .8,
  package = "cellcount",
  p_size = 24,
  p_color = "orange",
  h_size = 1,
  h_fill = "white",
  h_color = "black",
  url = "HABF-SDI-NCCOS-NOAA/HABF_cellcount",
  u_size = 4,
  u_color = "black"
) %>% plot()
