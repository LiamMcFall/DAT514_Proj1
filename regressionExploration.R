library(caret)
library(tidyverse)


health <- read_xlsx("/Users/liammcfall/Documents/GitHub/DAT514_Proj1/Health.xlsx")

# Plots

attach(health)

plot(X2, X1)
plot(X3, X1)
plot(X4, X1)
plot(X5, X1)

pairs(health)

# Relationships wiht X1 are alomsot entirely non-linear
# Linear regression probably won't perform well.

health.poly <- glm(X1 ~ poly(X2, 4) + poly(X3, 4) + poly(X4, 4) + poly(X5, 4), data = health)
summary(health.poly)
