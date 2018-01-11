library(tidyverse)

theses <- read.csv("https://www.dropbox.com/s/ontvhfnnz9nl3xd/sample_theses.csv?dl=1")
library <- read.csv("https://www.dropbox.com/s/41645e7d56i6al3/library_theses.csv?dl=1")

theses <- theses %>%
  mutate(age = 2017 - year) %>%
  filter(year > 1994)

m2 <- glm(checkouts ~ age, data = theses, family = "poisson")

library <- library %>%
  mutate(age = 2017 - year) %>%
  filter(year > 1994)
ml <- glm(checkouts ~ age, data = library, family = "poisson")

ggplot(theses, aes(x = age, y = checkouts)) +
  geom_jitter() +
  stat_function(fun = function(age) {exp(coef(m2)[1] + coef(m2)[2] * age)},
                color = "red")

ggplot(library, aes(x = age, y = checkouts)) +
  geom_jitter() +
  stat_function(fun = function(age) {exp(coef(ml)[1] + coef(ml)[2] * age)},
                color = "red")




m3 <- glm(checkouts ~ age + division, data = theses, family = "poisson")

summary(m3)

ggplot(theses, aes(x = age, y = checkouts)) +
  geom_jitter() +
  stat_function(fun = function(age) {exp(coef(m3)[1] + coef(m3)[2] * age)},
                color = 1) +
  stat_function(fun = function(age) {exp(coef(m3)[1] + coef(m3)[3] + coef(m3)[2] * age)},
                color = 2) +
  stat_function(fun = function(age) {exp(coef(m3)[1] + coef(m3)[4] + coef(m3)[2] * age)},
                color = 3) +
  stat_function(fun = function(age) {exp(coef(m3)[1] + coef(m3)[5] + coef(m3)[2] * age)},
                color = 4) +
  stat_function(fun = function(age) {exp(coef(m3)[1] + coef(m3)[6] + coef(m3)[2] * age)},
                color = 5)
  