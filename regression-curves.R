#
# Linear Regression
#
#

# Retrieve R2 and p values
summary(lm(random_fyp_dataset));

# Draw the curve
abline(lm(random_fyp_dataset), lwd=2, col="red");


#
# Quadratic Regression
# (Adapted from code published by David Lillis, Ph.D. @ https://www.theanalysisfactor.com/r-tutorial-4/)
#

# Housekeeping/setting some variables to simplify things
time <- random_fyp_dataset$`Relative Time`;
apoe <- random_fyp_dataset$`APOE`;
apoe2 <- apoe^2;
apoevalues <- seq(-150,150,1);

# Retrieve R2 and p values
summary(lm(time ~ apoe + apoe2));

# Draw the curve
lines(apoevalues, predict(lm(time ~ apoe + apoe2), list(apoe=apoevalues, apoe2=apoevalues^2)), lwd=2, col="green");
# For higher power functions, apoe3 <- apoe^; lm(time ~ apoe + apoe2 + apoe3); would be used, and so on for 4, 5, 6, etc... powers


#
# LOESS Regression
#
#

# Housekeeping/setting some variables to simplify things
lx <- sort(time);
ly <- apoe[order(time)];

# Retrieve information on fit
summary(loess(ly ~ lx));

# Draw the curve
lines(predict(loess(ly ~ lx)), x=lx, lwd=2, col="purple");


#
# Sigmoid Model
#
#

# Sigmoid fitting, adapted from Kyriakos Chatzidimitriou @ http://kyrcha.info/2012/07/08/tutorials-fitting-a-sigmoid-function-in-r
sigmoid = function(params, x) {
  params[1] / (1 + exp(-params[2] * (x - params[3])))
}
fitmodel <- nlsLM(y~a/(1 + exp(-b * (x-c))), start=list(a=150,b=10,c=1))
params=coef(fitmodel)
y2 <- sigmoid(params,x)

# Retrieve information on fit
sigma(fitmodel);

# Draw the curve
lines(seq(-14.9999,15,30/2716), y2, lwd=2, col="yellow");
