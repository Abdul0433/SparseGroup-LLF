#####################
## Create Figure 3 ##
#####################

green = rgb(23/255,94/255,84/255)

n <- 1000
p <- 5
sigma <- 5

mu <- function( x ) {
  10 * sin(pi * x[1] * x[2]) + 20 * ((x[3] - 0.5) ** 2) + 10 * x[4] + 5 * x[5]
}

X <- matrix(runif(n * p, 0 ,1), nrow = n)
Y <- apply(X, MARGIN = 1, FUN = mu) + sigma * rnorm(n)
forest <- regression_forest(X, Y, mtry = 3)

max.depth <- 4
freqs <- split_frequencies(forest, max.depth = max.depth)
d <- data.frame(freqs)
dm <- data.frame(variable = sort(rep(names(d), nrow(d))),
                 value = as.vector(as.matrix(d)),
                 depth = rep(1:max.depth, p))

# normalize value by sum of value at depth and store. 
for(i in 1:max.depth){
  tot.depth <- sum(dm[dm$depth == i,]$value)
  dm[dm$depth == i,]$value <- dm[dm$depth == i,]$value / tot.depth
}

ll.forest <- ll_regression_forest(X, Y, enable.ll.split = TRUE, ll.split.weight.penalty = TRUE,
                                 # ll.split.lambda = 0.1,
                                  mtry = 3)

freqs <- split_frequencies(ll.forest, max.depth = max.depth)
d <- data.frame(freqs)
dm.ll <- data.frame(variable = sort(rep(names(d), nrow(d))),
                    value = as.vector(as.matrix(d)),
                    depth = rep(1:max.depth, p))

for(i in 1:max.depth){
  tot.depth <- sum(dm.ll[dm.ll$depth == i,]$value)
  dm.ll[dm.ll$depth == i,]$value <- dm.ll[dm.ll$depth == i,]$value / tot.depth
}

color_range = range(c(0, dm$value, dm.ll$value))

g <- ggplot(dm, aes(x = variable, y = -depth, fill = value)) + 
  geom_tile() + 
  xlab("Variable") + 
  ylab("Depth") + 
  scale_fill_gradient(low = "white", high = green, limits = color_range, "Frequency \n") + 
  ggtitle("") + 
  theme(text = element_text(size = 15))
ggsave("results/splits_cart.jpeg", g)

g2 <- ggplot(dm.ll, aes(x=variable, y=-depth, fill = value)) + 
  geom_tile() + 
  xlab("Variable") + 
  ylab("Depth") + 
  scale_fill_gradient(low = "white", high = green, limits = color_range, "Frequency \n") + 
  ggtitle("") + 
  theme(text = element_text(size = 15))
ggsave("results/splits_ridge.jpeg", g2)
