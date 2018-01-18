library(tidyverse)

X <- rnorm(4000, mean = 0, sd =1)
Y <- 1:4000
dataframe <- tibble(X, Y)

hist(dataframe$X)


ggplot(dataframe, aes(X)) +
  geom_abline() +
  scale_y_log10()
