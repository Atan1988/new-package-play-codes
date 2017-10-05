library(billboarder)
library(dplyr)

billboarder() %>% 
  bb_barchart(table(sample(letters[1:6], 50, TRUE))) %>% 
  bb_title(text = "My title", position = "center")

data("prod_par_filiere")
prod_par_filiere[, c(1, 3, 4, 5, 6, 8)]

# Default
billboarder() %>% 
  bb_barchart(data = prod_par_filiere[, c(1, 3, 4, 5, 6, 8)])

prod_par_filiere %>% 
  select(annee, prod_therm) %>%
  mutate(annee = as.factor(annee)) %>%
  
billboarder() %>% 
  bb_linechart(data = prod_par_filiere[, c(1, 3)], x="annee") %>%
  bb_y_axis(min = 0, tick = list(start = 0, fit = FALSE))

data("economics", package = "ggplot2")

billboarder() %>%
  bb_linechart(data = economics[, c("date", "psavert")]) %>% 
  bb_x_axis(tick = list(format = "%Y-%m", fit = FALSE)) %>%
  bb_y_axis(tick = list(format = suffix("%")), 
            label = list(text = "Personal savings rate")) %>% 
  bb_legend(show = FALSE) %>% 
  bb_x_grid(show = TRUE) %>% 
  bb_y_grid(show = TRUE) %>% 
  bb_subchart(show = TRUE)


df <- data.frame(
  cos = cos(seq(-pi, pi, length.out = 30))
)

# No legend
billboarder() %>% 
  bb_linechart(data = df) %>% 
  bb_legend(show = FALSE)


df$sin <- sin(seq(-pi, pi, length.out = 30))

billboarder() %>% 
  bb_linechart(data = df) %>% 
  bb_legend(position = "right") %>%
  bb_x_axis(tick = list(
    label = seq(-pi, pi, length.out = 30),
    outer = TRUE
  )
  )
