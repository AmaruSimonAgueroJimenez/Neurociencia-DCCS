# libs ----
pacman::p_load(
    tidyverse,
    ggplot2,
    ggthemes
)
setwd(this.path::here())


# load data ----
data <- read_rds(file = "../data/data_dirty_fast.rds")

# figures ----

p1 <- data %>%
    filter(`Event Type` == "Response") %>%
    ggplot(aes(
        RT_ms
    )) +
    geom_density() +
    theme_par()
p1
