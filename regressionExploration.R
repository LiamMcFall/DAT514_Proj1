library(tidyverse)
library(readxl)
library(splines)

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

# GAM

library(gam)

health.gam1 <- gam(X1 ~ s(X2, 4), data = health)
health.gam2 <- gam(X1 ~ s(X3, 5), data = health)
health.gam3 <- gam(X1 ~ s(X4, 5), data = health)
health.gam4 <- gam(X1 ~ s(X5, 5), data = health)
anova(health.gam1, health.gam2, health.gam3, health.gam4)

health.gam <- gam(X1 ~ X2 + X3 + s(X4) + s(X5), data = health)
plot(health.gam, se= TRUE, col = "blue")
summary(health.gam)

# Splines

health.s <- lm(X1 ~ X2 + X3 + bs(X4, df = 3) + bs(X5, df = 4), data = health)
summary(health.s)

# Regression tree

library(tree)

health.tree <- tree(X1 ~ X2 + X3 + bs(X4, df = 3) + bs(X5, df = 4), data = health)
summary(health.tree)
plot(health.tree)
text(health.tree ,pretty=0)

health.tree.cv <- cv.tree(health.tree)
plot(health.tree.cv$size ,health.tree.cv$dev ,type='b')

health.prune <- prune.tree(health.tree ,best=4)
plot(health.prune)
text(health.prune ,pretty=0)

# Random forest

library(randomForest)

# RF is bagging when m = p

set.seed(7)
health.bag <- randomForest(X1 ~ ., data = health,
                        mtry=4,importance =TRUE)

importance(health.bag)
varImpPlot(health.bag)
