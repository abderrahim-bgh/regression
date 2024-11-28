
car.df<-read.csv("/home/abderrahim/R/ToyotaCorolla.csv")
dim(car.df)

# use first 1000 rows
car.df<-car.df[1:1000,]
head(car.df,2)
dim(car.df)

# select variables for regression
selected.var<-c(3,4,7,8,9,10,12,13,14,17,18)
head(car.df[, selected.var], 2)

# Price (la 3e col. est devenue la 1ere); 10 Predicteurs (incluant Fuel_Type)

# partition data
set.seed(1)
train.index<-sample(c(1:1000), 600)
train.index
class(train.index) #"integer"

# aside
# cf. WestRoxbury
xxx <- sample(row.names(car.df), 5)
xxx
class(xxx) #"character"
# aside; end

#aside
zzz0 <- model.matrix(~ 0 + Fuel_Type, car.df)
zzz0[c(1,9,224), ]


# aside
zzz1 <- model.matrix(~ 1 + Fuel_Type, car.df)
zzz1[c(1,9,190), ]


train.df<-car.df[train.index, selected.var]
valid.df<-car.df[-train.index, selected.var]
head(train.df,2)
dim(train.df)
head(valid.df,2)
dim(valid.df)

#aside
xxx
# [1] "979" "209" "148" "704" "501"
train.xxx <-car.df[xxx,selected.var]
train.xxx
valid.xxx <-car.df[setdiff(row.names(car.df),xxx),selected.var]
valid.xxx

#1          210   1165
dim(train.xxx)   # 5    11
dim(valid.xxx)   # 995  11
#aside; end

car.lm <- lm(Price ~ ., data=train.df)
options(scipen=999)
summary(car.lm)
predict(car.lm)

  
  ##########################################
#########################################
# set and summary predictive measures for entire validation set (called
# test set in R)
install.packages("forecast")
library(forecast)
# Use predict()
car.lm.pred <- predict(car.lm, valid.df)
options(scipen=999, digits = 2)

some.residuals <- valid.df$Price[1:20] - car.lm.pred[1:20]
data.frame("Predicted" = car.lm.pred[1:20], "Actual" = valid.df$Price[1:20],
           "Residual" = some.residuals)

accuracy(car.lm$fitted.values,train.df$Price)
#                         ME RMSE  MAE MPE MAPE
# Test set 0.000000000000037 1378 1010  -1  9.1

accuracy(car.lm.pred, valid.df$Price)
#          ME RMSE  MAE   MPE MAPE

# Alternatively
# rmse = sqrt(...)



#######################################
#####################################
# Page 161 
all.residuals <- valid.df$Price - car.lm.pred

all.residuals[which(all.residuals > -1406 & all.residuals < 1406)]

length(all.residuals[which(all.residuals > -1406 & all.residuals < 1406)]) #289


options(scipen=999, digits=1)
length(all.residuals[which(all.residuals > -1406 & all.residuals < 1406)]) / 400

hist(all.residuals, breaks = 25, xlab = "Residuals", main ="")



#################################################
###############################################
# Unlike with lm, categorical predictors must be turned into dummies manually.
install.packages("leaps")
library(leaps)
# Create dummies for fuel type
head(train.df,2)

# Attention: FuelType et Fuel_Type

# Attention: matrix(~ 0 + Fuel_Type)  

Fuel_Type <- as.data.frame(model.matrix(~ 0 + Fuel_Type, data = train.df))
head(Fuel_Type,3)

# Replace Fuel_Type column with 2? dummies as prescribed
train.df2 <- cbind(train.df[, -4], Fuel_Type[,])
head(train.df2,3)
# 12 "predictors" for Price: 10 + 3 (The 3 levels of Fuel_Type) -1 (Fuel_Type)


##### Comparaison avec ce qui a ete fait plus haut
z<-lm(Price ~ ., data=train.df2)
z             # Remarquer: Fuel_TypePetrol: NA

head(train.df2,2)


search <- regsubsets(Price ~ ., data=train.df2, nbest = 1, nvmax =
                       dim(train.df2)[2], method = "exhaustive")
search

#Selection Algorithm: exhaustive

sum <- summary(search)
sum

# str(sum)

# show models
sum$which

# show metrics
options(scipen=999, digits=7)
sum$rsq #Be careful with options(scipen=...)  

sum$adjr2 


options(scipen=999, digits = 2)
sum$cp    

############
# Complement
# See: http://www.science.smith.edu/~jcrouser/SDS293/labs/lab8-r.html
plot(sum$rsq, xlab="number of variables", ylab="RSq", type="l")
plot(sum$adjr2, xlab="number of variables", ylab="Adjusted RSq", type="l")
# We will now plot a red dot to indicate the model with the largest adjusted R^2 statistic.
# The which.max() function can be used to identify the location of the maximum point of a vector
adj_r2_max = which.max(sum$adjr2)
adj_r2_max # 7

# The points() command works like the plot() command, except that it puts points 
# on a plot that has already been created instead of creating a new plot
points(adj_r2_max, sum$adjr2[adj_r2_max], col ="red", cex = 2, pch = 20)
# cex and pch
# http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r
# pch = symbols: cross, diamond, square, etc; 
# cex = size of the symbols

# We'll do the same for C_p and BIC, this time looking for the models with the SMALLEST statistic
plot(sum$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
cp_min = which.min(sum$cp) # 7
points(cp_min, sum$cp[cp_min], col = "red", cex = 2, pch = 20)

plot(sum$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
bic_min = which.min(sum$bic) # 6
points(bic_min, sum$bic[bic_min], col = "red", cex = 2, pch = 20)
#Complement; end
###############


#############################################
##########################################
# 
# PAGE 167
# Table 6.6   BACKWARD ELIMINATION
# Code for stepwise regression
# use step() to run stepwise regression.
# set directions = to either "backward", "forward", or "both".
car.lm.step <- step(car.lm, direction = "backward")
summary(car.lm.step) 


# Which variables did it drop? Met_Color, Automatic, CC, Doors
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)
#          ME RMSE  MAE   MPE MAPE
# Test set 20 1328 1055 -0.74  9.4
#
###########################


###########################
####################
#
# Page 168
# Table 6.7. Forward selection
car.lm.step <- step(car.lm, direction = "forward")
summary(car.lm.step)


# 11 predictors, including 2 fuel types
# Which variables have been dropped? None
##################################

##################
#
#Page 168
#Table 6.8
#Stepwise regression
car.lm.step <- step(car.lm, direction = "both")
summary(car.lm.step)




