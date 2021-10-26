
## ---- art01
library(ggplot2)
library(mathart)
df1 <- harmonograph(A1 = 1, A2 = 1.5, A3 = 1, A4 = 1,
                    d1 = 0.004, d2 = 0.0065, d3 = 0.008, d4 = 0.019,
                    f1 = 3.001, f2 = 2, f3 = 3, f4 = 2,
                    p1 = 0, p2 = pi/4, p3 = pi/2, p4 = 3*pi/2)
ggplot(df1, aes(x, y)) +
  geom_path(alpha = 0.4, size = 0.4, color = "#181818") +
  coord_equal() +
  xlim(min(df1$x) - 5, max(df1$x) + 1) +
  annotate("text", x = max(df1$x) + 0.2, y = max(df1$y) - 0.2, 
           label = "Harmonograph", color = "gray",
           family = "Atma", size = 6) + 
  theme_void()

## ---- save-art01
ggsave("images/art-01.png", width = 1210/24, height = 681/24, units = "cm")


## ---- art02
library(ggplot2)
library(mathart)
df2 <- fractal_fern(n = 250000,
                    a = c(0, 0.85, 0.2, -0.15),
                    b = c(0, 0.04, -0.26, 0.28),
                    c = c(0, -0.04, 0.23, 0.26),
                    d = c(0.16, 0.85, 0.22, 0.24),
                    e = c(0, 0, 0, 0),
                    f = c(0, 1.6, 1.6, 0.44),
                    p = c(0.01, 0.85, 0.07, 0.07))
ggplot(df2, aes(x, y)) +
  geom_point(size = 0.03, alpha = 0.06, color = "white") +
  coord_equal() +
  labs(caption = "Fractal ferns") +
  theme_blankcanvas(margin_cm = 1) +
  theme(plot.caption = element_text(family = "Atma", size = 20, color = "gray"),
        plot.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"))

## ---- save-art2
ggsave("images/art-02.png", width = 1210/4 * 0.4, height = 681/4, units = "mm")

## ---- art03
library(tidyverse)
library(mathart)
set.seed(2021)
df3 <- map_dfr(1:100, ~lissajous(a = runif(1, 0, 10), A = runif(1, 0, 1)), .id = "id")
ggplot(df3, aes(x, y)) +
  geom_path(color = "#027EB6", size = 0.25, lineend = "round", alpha = 0.07) +
  facet_wrap(~id, nrow = 8) +
  coord_equal() +
  labs(caption = "Lissajous curves") +
  theme_blankcanvas(margin_cm = 1) +
  theme(plot.caption = element_text(family = "Atma", size = 20, color = "gray"))

## ---- save-art3
ggsave("images/art-03.png", width = 1210/4, height = 681/4, units = "mm")

## ---- art04
library(tidyverse)
library(mathart)
set.seed(2021)
colors <- c(blue = "#027EB6", purple = "#746FB2", fuchsia = "#9651A0", 
            ruby = "#C8008F", pink = "#ee64a4", red = "#EE0220", orange = "#D93F00", 
            umber = "#795549", olive = "#6F7C4D", green = "#008A25")
df4 <- map_dfr(1:16, function(n) {
  map_dfr(1:7, function(d) {
    rose_curve(n, d) %>% 
      mutate(id = paste(n, d), color = sample(colors, 1))
  })
}) %>% 
  mutate(id = factor(id, levels = sample(unique(id))))

ggplot(df4, aes(x, y, color = I(color))) +
  geom_path(size = 0.35, lineend = "round", alpha = 0.07) +
  coord_equal() +
  facet_wrap(~id, nrow = 7) + 
  labs(caption = "Rose curves") +
  theme_blankcanvas(margin_cm = 1) +
  theme(plot.caption = element_text(family = "Atma", size = 20, color = "gray"))

## ---- save-art4
ggsave("images/art-04.png", width = 1210/4, height = 681/4, units = "mm")


