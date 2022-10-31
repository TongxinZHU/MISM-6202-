# 5
# a
result <- 0
for (i in 10:25){
  tem = i^4 + i^5
  result = result + tem
}
print(result)
# b
tem <- 10:25
print(sum(tem^4 + tem^5))

# 6
# a
paste("label", 1:30, sep = " ")
# b
paste("fn", 1:30, sep = "")
sprintf("fn%d", 1:30)

# 7
# a
1:10 > 5
# The result of this line is a vector with ten boolean values.
# This execution is to compare every number from 1 to 10 with 5,
# so if the number is larger than 5, the result will be True,
# and if the result is smaller then 5, the result will be False,
# since 1, 2, 3, 4, 5 are not larger than 5, the first five boolean values are False,
# and since 6, 7, 8, 9, 10 are larger than 5, the rest of the vector are True.
# b
1:(10 > 5)
# The result of this line is 1 because this execution is to show all integers from 1 to (10>5).
# And since the value of (10>5) is True which is not a numeric variable, the only integer from
# 1 to (10>5) is 1, so the result is 1.
# Same as if we execute 1:1.

# 8
install.packages("GGally")
library(tree)
library(ggplot2)
library(GGally)
library(MASS)
head(Boston)
?Boston
# a
# The Boston data frame has 506 rows and 14 columns.
# Each row in the data frame represents observations of a Boston suburb or town,
# Each column represents a predictor variable of those 506 areas such as per capita crime rate,
# pupil-teacher ratio, and so on.
# b
ggpairs(data = Boston, title = "Boston Data Pairwise Scatterplots", 
        upper = list(continuous = wrap("cor", size = 2))) + 
        theme_bw() + 
        theme(axis.text = element_text(size = 4)) 
# We can use pairs() as well
# I like to use ggpairs() rather than pairs() because ggpairs() is easier to read 
# and it can show all correlation numbers of each two columns 
# which is helpful to define the relationship within each column.
pairs(Boston)
# Check data type of each column
str(Boston)
# Change other data type to numeric
Boston$chas <- as.numeric(Boston$chas)
Boston$rad <- as.numeric(Boston$rad)
Boston$newChas <- as.numeric(Boston$newChas)
# Correlation matrix of Boston data frame
cor(Boston)
# From the results we can see all relationships between each two columns and we can use these
# to know which column is the most relevant to another one. For example, "rad" has the highest
# positive correlation of "crim". Also, every graph in pairs plots shows how much a column is
# relevant to another. For example, "nox" and "chas" are not very correlated since the plot is
# parallel. We also can know variable distribution on the diagonal of ggpairs plots. 
# ("newCrime" and "newChas" were added for following questions.)
# c
# Use linear regression to test if other columns have linear relationship with "crim"
fit <- lm(crim ~ ., Boston)
fit
summary(fit)
# From the linear regression summary, we can see "zn", "nox", "dis" and "medv" are 
# significant for predicting "crim". And since the coefficients are provided, 
# we can know how much will variable crim change when other variables change.
# From the results we got for part b, we can see all correlation values of "crim" with other
# predictors. "zn", "chas", "rm", "dis", "black", and "medv" have negative correclation 
# with "crim, whereas "indus", "nox", "age", "rad", "tax", "ptratio", and "lstat" have
# positive correclation with "crim". 
# d
# Calculate mean and sd of "crim"
mean(Boston$crim)
sd(Boston$crim, na.rm = FALSE) 
# Histogram of "crim" with normal density function
p <- ggplot(Boston, aes(crim)) +
  geom_histogram(aes(y = ..density..), bins = 10) +
  geom_function(fun = dnorm, args = list(mean = 3.613524, sd = 8.601545), color = "red") 
p + ggtitle("Histogram of crim") +
  xlab("Crim value") + ylab("Density")
# The histogram shows varible crim is not normally distributed.
# We can change "crim" values to log10(crim values) to make it more like a normal distribution.
Boston$newCrime <- log10(Boston$crim)
mean(Boston$newCrim)
sd(Boston$newCrim, na.rm = FALSE) 
q <- ggplot(Boston, aes(newCrime)) +
  geom_histogram(aes(y = ..density..), bins = 10) +
  geom_function(fun = dnorm, args = list(mean = -0.3389392, sd = 0.9389665), color = "red") 
q + ggtitle("Histogram of log10(crim)") +
  xlab("New crim value") + ylab("Density")
# e
# Change variable "chas" to a factor
# "chas" has two values: 1 represents tract bounds river, 0 otherwise.
Boston$newChas <- as.factor(Boston$chas)
# Set half of the whole data set to training data, and another half to testing data
train <- Boston[ 1:253,]       
test  <- Boston[ 254:506,]
# Create a decision tree model by training data 
# Set all variables except newChas in the training data to predictors
fit <- tree(newChas ~ crim + zn + indus + nox + rm + age + 
              dis + rad + tax+ ptratio + black + lstat + medv, train)
plot(fit)
text(fit)
# Use testing data to test the accuracy of the prediction model
pred <- predict(fit, test, type = "class")
pred
# Create a table to evaluate model fit
tt <- table(pred, test$newChas)
tt
# Calculate the model accuracy
print((tt[1,1] + tt[2,2])/nrow(test))

