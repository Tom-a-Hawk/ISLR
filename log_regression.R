library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket) #this will throw an error bc direction is qualitative
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)


market_glm <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
                  data = Smarket, 
                  family = binomial)

summary(market_glm)

coef(market_glm)
coefficients(market_glm) #pull out coefficents
summary(market_glm)$coef #pull out coefficents + std. error, z value and p-value

summary(market_glm)$coef[,4] #same as above, but only pulls out p-values

market_glm_probs <- predict(market_glm, type = 'response')
market_glm_probs[1:10]


contrasts(Direction)


market_glm_pred <- rep("Down", 1250)
market_glm_pred[market_glm_probs>.5] = "Up"

market_glm_conf <- table(market_glm_pred, Direction)
market_glm_conf
acc_market_glm <- sum(diag(market_glm_conf)) / sum(market_glm_conf)
acc_market_glm

mean(market_glm_pred == Direction) #alternative way to calculate model accuracy 

training_error_rate <- 1-acc_market_glm
training_error_rate

#create a training data set

# first create a vector with observations from 2001-2004. This will be our training set.
# the 2005 data points will serve as our test set

# Will be used to make our training data set
train <- (Year<2005)  #True is less than 2005, False if 2005

# Test data set
Smarket.2005 = Smarket[!train,] #observations with the year 2005
# ^yields a sub-matrix of the data containing only the observations for "train" that are False

dim(Smarket.2005)

#we will use this later to create our confusion matrix
Direction.2005 = Direction[!train]


#fit a logistic regression model to the training data
glm_train <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                 data = Smarket,
                 family = binomial,
                 subset = train)

# run a prediction with our training data set
glm_train_probs <- predict(glm_train, Smarket.2005, type = "response")

glm_pred <- rep("Down", 252)
glm_pred[glm_train_probs > .5] = "Up"

#create a confusion matrix
table(glm_pred, Direction.2005)

#accuracy
mean(glm_pred==Direction.2005)
mean(glm_pred != Direction.2005)


###########  LDA  ##############
library(MASS)
smarket_lda <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)

smarket_lda

plot(smarket_lda)

smarket_lda_pred <- predict(smarket_lda, Smarket.2005)
names(smarket_lda_pred)

smarket_lda.class = smarket_lda_pred$class
table(smarket_lda.class, Direction.2005)

# applying a 50% threshold to the posterior probs allow us to recreate
# the predictions contained in smarket_lda_pred$class
sum(smarket_lda_pred$posterior[,1] >= .5)
sum(smarket_lda_pred$posterior[,1] < .5)

#notice the posterior prob output by the model corresponds to the probability
# the market will decrease
smarket_lda_pred$posterior[1:20, 1]
smarket_lda.class[1:20]



















