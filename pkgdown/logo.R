library(ggplot2)
library(hexSticker)
library(dplyr)
library(cowplot)

noaa <- magick::image_read("https://www.noaa.gov/sites/default/files/2022-03/noaa_digital_logo-2022.png") %>%
  magick::image_colorize(65,"white")

test_image1 <- ggdraw() +
  draw_image(noaa)

test_image1

microscope <- magick::image_read("https://cdn.icon-icons.com/icons2/1465/PNG/512/708microscope_100683.png") %>%
  magick::image_resize("400x400")

test_image2 <- ggdraw() +
  draw_image(microscope)

hexSticker::sticker(
  filename = "inst/figures/logo.png",
  subplot = test_image2,
  s_width = .95, s_height = 1.5,
  s_x = 1, s_y = .75,
  package = "cellcount",
  p_size = 26,
  p_color = "royalblue",
  h_size = 1,
  h_fill = "grey95",
  h_color = "black",
  url = "HABF-SDI-NCCOS-NOAA/HABF_cellcount",
  u_size = 4,
  u_color = "royalblue"
) %>% plot()
