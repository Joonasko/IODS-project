library(MASS)
library(tidyr)
library(corrplot)
data("Boston")

head(Boston)
pairs(Boston, pch = 20)

cor_matrix <- round(cor(Boston),digits=5)
cor_matrix
corrplot(cor_matrix, method="circle")

boston_scaled <- scale(Boston)
summary(boston_scaled)
class(boston_scaled)

boston_scaled <- as.data.frame(boston_scaled)
boston_scaled$crim <- as.numeric(boston_scaled$crim)

# Work with the exercise in this chunk, step-by-step. Fix the R code!
# Boston and boston_scaled are available

# summary of the scaled crime rate
summary(boston_scaled$crim)

# create a quantile vector of crim and print it
bins <- quantile(boston_scaled$crim)
bins

# create a categorical variable 'crime'
crime <- cut(boston_scaled$crim, breaks = bins, 
             label = c("low","med_low","med_high","high"), 
             include.lowest = TRUE)

# look at the table of the new factor crime
table(crime)

# remove original crim from the dataset
boston_scaled <- dplyr::select(boston_scaled, -crim)

# add the new categorical value to scaled data
boston_scaled <- data.frame(boston_scaled, crime)

n <- nrow(boston_scaled)
ind <- sample(n, size = n * 0.8)
train <- boston_scaled[ind,]
test <- boston_scaled[-ind,]
correct_classes <- test$crime
test <- dplyr::select(test, -crime)

lda.fit <- lda(crime~., data = train)

lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "red", tex = 0.75, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex, col=color, pos=3)
}

# target classes as numeric
classes <- as.numeric(train$crime)

# plot the lda results
plot(lda.fit, dimen = 2)
lda.arrows(lda.fit, myscale = 1)

lda.pred <- predict(lda.fit, newdata = test)

table(correct = correct_classes, predicted = lda.pred$class)




