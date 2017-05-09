
# Boston Housing Data

set.seed(10676565)
library(MASS)
data <- Boston

index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]

# General Linear Model

lm.fit <- glm(medv~., data=train)
lm.nullmodel = lm(medv ~ 1, data = train)
lm.fullmodel = lm(medv ~ ., data = train)

model.lm = step(lm.nullmodel, scope = list(lower = lm.nullmodel, upper = lm.fullmodel), 
                direction = "both")
summary(model.lm)
AIC(model.lm)
2309
BIC(model.lm)
2360


pr.lm <- predict(model.lm,train)
MSEi.lm <- sum((pr.lm - train$medv)^2)/nrow(train)
MSEi.lm
# 23.85
pr.lm <- predict(model.lm,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)
MSE.lm
# 16.70

# Tree

library(rpart)

boston.rpart <- rpart(formula = medv ~ ., data = train)
plot(boston.rpart)
text(boston.rpart)

boston.largetree <- rpart(formula = medv ~ ., data = train, cp = 0.001)



plotcp(boston.largetree)


boston.train.pred.tree = predict(boston.rpart, train)


MSEt = mean((boston.train.pred.tree - train$medv)^2)

MSEt
# 14.920

boston.test.pred.tree = predict(boston.rpart, test)



MSEte = mean((boston.test.pred.tree - test$medv)^2)

MSEte
# 28.66  # Overfitting


# Generalized Additive Models (GAM)

library(mgcv)



gam_formula <- as.formula(paste("medv~s(crim)+s(zn)+s(indus)+", paste(colnames(train)[4:13], 
                                                                    collapse = "+")))

boston.gam <- gam(formula = gam_formula, data = train)


summary(boston.gam)


plot(boston.gam, shade = TRUE, , seWithMean = TRUE, scale = 0)

pred.gam <- predict(boston.gam, train)

MSEg = mean((pred.gam - train$medv)^2)
MSEg

# 21.1

pred.gam <- predict(boston.gam, test)

MSEgt = mean((pred.gam - test$medv)^2)
MSEgt

# 16.65

AIC(boston.gam)
#2291
BIC(boston.gam)
#2396

# Neural Nets

maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

library(neuralnet)
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),act.fct="tanh",linear.output=T)

plot(nn)

# Insample 

pr.nn <- compute(nn,train_[,1:13])

pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
train.r <- (train_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)

MSEi.nn <- sum((train.r - pr.nn_)^2)/nrow(train_)

MSEi.nn

# 3.99

# Outsample

pr.nn <- compute(nn,test_[,1:13])

pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)

MSEo.nn <- sum((test.r - pr.nn_)^2)/nrow(test_) 

MSEo.nn

# 14.56 # Highly overfitting but still better than other's
