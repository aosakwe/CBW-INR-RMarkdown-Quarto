library(ggplot2)
library(dplyr)

#Filtering out versicolor to run a binary example
mini_iris <- iris %>%
  filter(Species != 'versicolor')

ggplot(mini_iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(size = 3, alpha = 0.7) +  # Add points with some transparency
  theme_minimal() +  # Use a clean theme
  labs(
    title = "Iris Dataset: Sepal Dimensions by Species",
    x = "Sepal Length",
    y = "Sepal Width"
  )

iris.fit <- glm(Species ~ Sepal.Length +  Sepal.Width,
                family = 'binomial',
                data = mini_iris)

mini_iris$Prob <- predict.glm(iris.fit,
                              mini_iris[,c('Sepal.Length','Sepal.Width')],
                              type = 'response')

mini_iris$Pred <- ifelse(mini_iris$Prob >= 0.5,"virginica", "setosa")

mini_iris$Result <- mini_iris$Species == mini_iris$Pred


ggplot(mini_iris, aes(x = Sepal.Length, y = Sepal.Width, color = Pred)) +
  geom_point(size = 3, alpha = 0.7) +  # Add points with some transparency
  theme_minimal() +  # Use a clean theme
  labs(
    title = "Iris Dataset: Training Classification Performance",
    x = "Sepal Length",
    y = "Sepal Width"
  )


mini_iris$Num_Species <- ifelse(mini_iris$Species == 'setosa', 0,1)
iris.lm <- lm(Num_Species ~ Sepal.Length +  Sepal.Width,
              data = mini_iris)

mini_iris$Num_pred <- predict.lm(iris.lm,mini_iris)

summary(mini_iris$Num_pred)

summary(mini_iris$Prob)


mini_iris <- iris %>%
  filter(Species != 'setosa')
ggplot(mini_iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point(size = 3, alpha = 0.7) +  # Add points with some transparency
  theme_minimal() +  # Use a clean theme
  labs(
    title = "Iris Dataset: Dimensions by Species",
    x = "Petal Length",
    y = "Petal Width"
  )

iris.fit <- glm(Species ~ Sepal.Length + Sepal.Width,
                family = 'binomial',
                data = mini_iris)

iris_pred <- predict.glm(iris.fit,
                         mini_iris[,c('Sepal.Length','Sepal.Width')],
                         type = 'response')
table(mini_iris$Species == ifelse(iris_pred >= 0.5, 'virginica','versicolor'))