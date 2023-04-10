
install.packages("gganimate")
install.packages("gifski")
library(gganimate)
library(gifski)

p <- ggplot(iris, aes(Sepal.Length, Petal.Length)) + geom_point()
p <- p + transition_states(Species)

anim <- animate(p, renderer = gifski_renderer(), fps = 10, width = 400, height = 300)

anim_save("my_gif.gif")
