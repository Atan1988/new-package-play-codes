library(ggplot2)
library(treemapify)

setwd("~/GitHub/new package play codes/visual/treemapify")

G20

ggplot(G20, aes(area = gdp_mil_usd, fill = hdi)) +
  geom_treemap()

ggplot(G20, aes(area = gdp_mil_usd, fill = hdi, label = country)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                    grow = TRUE)

ggplot(G20, aes(area = gdp_mil_usd, fill = hdi, label = country,
                subgroup = region)) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                               "black", fontface = "italic", min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T)

ggplot(G20, aes(area = gdp_mil_usd, fill = region, label = country)) +
  geom_treemap() +
  geom_treemap_text(grow = T, reflow = T, colour = "black") +
  facet_wrap( ~ econ_classification) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom") +
  labs(
    title = "The G-20 major economies",
    caption = "The area of each country is proportional to its relative GDP
    within the economic group (advanced or developing)",
    fill = "Region"
  )


library(tweenr)
library(gganimate)

G20_alt <- G20
G20_alt$gdp_mil_usd <- sample(G20$gdp_mil_usd, nrow(G20))
G20_alt$hdi <- sample(G20$hdi, nrow(G20))

tweened <- tween_states(list(G20, G20_alt, G20), tweenlength = 8,
                        statelength = 5, ease = 'cubic-in-out', nframes = 31)

animated_plot <- ggplot(tweened, aes(area = gdp_mil_usd, fill = hdi,
                                     label = country, subgroup = region,
                                     frame = .frame)) +
  geom_treemap(fixed = T) +
  geom_treemap_subgroup_border(fixed = T) +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5,
                             colour = "black", fontface = "italic", min.size = 0,
                             fixed = T) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T, fixed = T)

animation::ani.options(interval = 1/10)
gganimate(animated_plot, "animated_treemap.gif", title_frame = F,
          ani.width = 200, ani.height = 200)