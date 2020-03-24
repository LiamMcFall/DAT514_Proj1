health <- read_xlsx("/Users/liammcfall/Documents/GitHub/DAT514_Proj1/Health.xlsx")

# Staright up glm with nothing else

fit <- glm(X1 ~ ., data = health)
summary(fit)