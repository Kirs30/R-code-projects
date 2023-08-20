
#### Multiple Linear Regression on Pizza dataset

# Read in the dataset
Pizza=read.csv("Pizza.csv")

# Observe the data to ensure it is labelled properly
dim(Pizza)
head(Pizza)
names(Pizza)
attach(Pizza)

# Additional exploration
summary(Pizza)
install.packages('ggplot')
library(ggplot)
ggplot(Pizza_long,aes(x = value)) +geom_histogram() +  facet_wrap(name ~ ., scales = "free")

# using a 0.1 level of significance
# Perform MLR
full.reg = lm(cal ~ mois + prot + fat + ash + sodium + carb)
summary(full.reg)


# Omnibus F test 
# to determine if model is significant   
# qf(1 - α, k, n - k - 1)    #n = 300, k= 6
qf(0.90,6,293)
# The model is significant because this value is greater than the F-statistic from the regression 
# Therefore we can continue

# Individual hypothesis testing
# tells us which features are insignificant and which to remove
anova(full.reg) 
# mois prot fat ash and carbs are significant at 0.1 level. bc p value is smaller than 0.1
# Reduce the model and remove sodium as it is insignificant at the 0.1 level of sig

### REDUCED MODEL
# check if predictors in reduced model are significant
reduced.model = lm(cal ~ mois + prot + fat + ash + carb)
summary(reduced.model)
anova(reduced.model)  
# at a 0.1 level these five predictors are significant

# Partial F test on reduced model
# to see which model is better (full or reduced)
anova(reduced.model,full.reg)
qf(0.90,2,25)  # if p value is less than full model. then reject.  but it ought to be higher
# p value of reduced model is 0.7462 (p value of full model was <2.2e^-16)
# Since the p value of the reduced model is higher, the reduced model is better


# Checking the assumptions of the residuals
windows()
split.screen(c(2,2))
screen(1) # Histogram of residuals
# Looks a little odd but maybe normal
hist(reduced.model$residuals, main = "Histogram of Residuals", col = "steelblue")
screen(2) # QQ plot
# This confirms that it is in fact mostly normal
qqnorm(reduced.model$residuals, pch = 20) 
qqline(reduced.model$residuals) 
screen(3) # Residuals vs fitted values
# We see homoscedasticity i.e. no shape/pattern 
# horizontal band around  0
plot(reduced.model$fitted.values, reduced.model$residuals, main = "Residuals versus Fitted",pch = 20,  col = "steelblue")
abline(h=0, lty=2)
screen(4) # Residuals vs time
#sub 300 for sample size
# points oscillate around zero with no apparent pattern
plot(1:300,reduced.model$residuals, main = "Residuals versus time order", pch = 20,  col = "steelblue")
abline(h=0, lty=2) 
# The assumptions hold.

### Checking for Collinearity
# 1 Scatterplot matrix
pairs(~ cal + mois + prot + fat + ash + carb, main="Scatterplot Matrix")

# 2 Correlation matrix
datamatrix = cbind(cal, mois, prot, fat, ash, carb)
cor(datamatrix)

# 3 Correlation tests
cor.test(mois, prot)# 0.36
cor.test(mois, fat) #-0.17
cor.test(mois, ash) #0.27
cor.test(mois, carb) #-0.59
cor.test(prot, fat) #0.498
cor.test(prot, ash)  #0.82
cor.test(prot, carb) #-0.85
cor.test(fat, ash) #0.79
cor.test(fat, carb) #-0.64
cor.test(ash, carb) #-0.899

#dev.off()
ggpairs(Pizza)
## The model suffers from collinearity