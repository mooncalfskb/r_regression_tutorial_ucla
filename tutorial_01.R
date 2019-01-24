rm(list = ls())
#https://stats.idre.ucla.edu/r/dae/logit-regression/
#install.packages("aod")
#install.packages("ggplot2")

library(aod)
library(ggplot2)

#load the data from ucla
#downloaded it so loading from csv below
#mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

base_dir = "~/Dropbox/skb/DataProjectsBankruptcy/2019/Tutorials/r_regression_tutorial_ucla/"

#training data
mydata <- read.csv(paste(base_dir, "binary.csv", sep = ""))

## view the first few rows of the data
head(mydata)

#learn about the quarties
summary(mydata)

#sapply sd (standard deviation function to entire dataset)
sapply(mydata, sd)
#    admit         gre         gpa        rank 
#0.4660867 115.5165364   0.3805668   0.9444602

sapply(mydata, mean)
#admit      gre      gpa     rank 
#0.3175 587.7000   3.3899   2.4850 

#so mean admit is only 31%. so admit of .466 is one standard deviation above this. so admit above .46 is strong.
#so mean GRE is 587, so GRE of 702 is exceptional. GRE of 472 is low. Some one scored 220, which is low.
#so mean GPA is 3.390. So GPA of 3.77 is one sd. GPA of 3.01 is low.
#Median Rank is 2.0. So Rank of 2.94 is high sd and rank of 1.1 is low sd

##my understanding of this is that it's a way to check your data is okay.
## should get exact numbers of admit for each rank (1-4) 4 is worst rank.
## if you get any holes, then you know your data is bad.
## two-way contingency table of categorical outcome and predictors we want
## to make sure there are not 0 cells
xtabs(~admit + rank, data = mydata)

#admit  1  2  3  4
#    0 28 97 93 55
#    1 33 54 28 12

#The code below estimates a logistic regression model using the glm (generalized linear model) function. 
#First, we convert rank to a factor to indicate that rank should be treated as a categorical variable. 
#(categorial, I think means 0/1 or perhaps the most important)

mydata$rank <- factor(mydata$rank)

#glm = generalized linear model
#As a reminder, Generalized Linear Models are an extension of linear regression models 
#that allow the dependent variable to be non-normal.

mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")

#Since we gave our model a name (mylogit), R will not produce any output from our regression. 
#In order to get the results we use the summary command:

#my understanding of this is that first you do this glm and get the summary which tells
#you whether something is statistically significant. who the fuck knows why, but whatever.
#this table below is the log odds, not the final number you are looking for. 
#but you have to call this first in order for it to work.

summary(mylogit)

# Call:
#   glm(formula = admit ~ gre + gpa + rank, family = "binomial", 
#       data = mydata)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.6268  -0.8662  -0.6388   1.1490   2.0790  
# 
# Coefficients:
#   Estimate      Std.      Error    z value Pr(>|z|)    
# (Intercept)   -3.989979   1.139951  -3.500 0.000465 ***
#   gre          0.002264   0.001094   2.070 0.038465 *  
#   gpa          0.804038   0.331819   2.423 0.015388 *  
#   rank2       -0.675443   0.316490  -2.134 0.032829 *  
#   rank3       -1.340204   0.345306  -3.881 0.000104 ***
#   rank4       -1.551464   0.417832  -3.713 0.000205 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 499.98  on 399  degrees of freedom
# Residual deviance: 458.52  on 394  degrees of freedom
# AIC: 470.52
# 
# Number of Fisher Scoring iterations: 4

# In the output above, the first thing we see is the call, 
# this is R reminding us what the model we ran was, 
# what options we specified, etc.
# 
# Next we see the deviance residuals, which are a measure of model fit. 
# This part of output shows the distribution of the deviance residuals 
# for individual cases used in the model. 
# Below we discuss how to use summaries of the deviance statistic to assess model fit.
# 
# The next part of the output shows the coefficients, their standard errors, 
# the z-statistic (sometimes called a Wald z-statistic), and the associated p-values. 
# Both gre and gpa are statistically significant, as are the three terms for rank. 
# The logistic regression coefficients give the change in the log odds of the outcome 
# for a one unit increase in the predictor variable.
# 
# For every one unit change in gre, the log odds of admission (versus non-admission) increases by 0.002.
# For a one unit increase in gpa, the log odds of being admitted to graduate school increases by 0.804.
# 
# The indicator variables for rank have a slightly different interpretation. 
# For example, having attended an undergraduate institution with rank of 2, 
# versus an institution with a rank of 1, changes the log odds of admission by -0.675.
# Below the table of coefficients are fit indices, including the null and deviance residuals and the AIC. 
# Later we show an example of how you can use these values to help assess model fit.

## CIs using profiled log-likelihood
confint(mylogit)

# Coefficients:
#   Estimate      Std.      Error    z value Pr(>|z|)    
# (Intercept)   -3.989979   1.139951  -3.500 0.000465 ***
#   gre          0.002264   0.001094   2.070 0.038465 *  
#   gpa          0.804038   0.331819   2.423 0.015388 *  
#   rank2       -0.675443   0.316490  -2.134 0.032829 *  
#   rank3       -1.340204   0.345306  -3.881 0.000104 ***
#   rank4       -1.551464   0.417832  -3.713 0.000205 ***

#I think what this is saying below is that gre is a good fit, because the value 
#in the table above is .002 which is  between .0001 and .004
#I'm not sure what it means for GPA, which (.804) is different than the table below.

#I think what this means is that the deviation between 2.5% and 97.5% is shown below
#the remaining 2.5% on either end is unknowable. Like standard deviation graph.


#Waiting for profiling to be done...
#              2.5 %        97.5 %
#(Intercept) -6.2716202334 -1.792547080
#gre          0.0001375921  0.004435874
#gpa          0.1602959439  1.464142727
#rank2       -1.3008888002 -0.056745722
#rank3       -2.0276713127 -0.670372346
#rank4       -2.4000265384 -0.753542605

## CIs using standard errors
confint.default(mylogit)

# confint.default(mylogit)
#              2.5 %         97.5 %
# (Intercept) -6.2242418514 -1.755716295
# gre          0.0001202298  0.004408622
# gpa          0.1536836760  1.454391423
# rank2       -1.2957512650 -0.055134591
# rank3       -2.0169920597 -0.663415773
# rank4       -2.3703986294 -0.732528724

#We use the wald.test function. b supplies the coefficients, 
#while Sigma supplies the variance covariance matrix of the error terms, 
#finally Terms tells R which terms in the model are to be tested, 
#in this case, terms 4, 5, and 6, are the three terms for the levels of rank.
#what this means is that if you count down, starting with intercept, the ranks are number 4,5, and 6.
#I think.

wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)

# Wald test:
#   ----------
#   
#   Chi-squared test:
#   X2 = 20.9, df = 3, P(> X2) = 0.00011


#test the difference bewteen rank 2 and 3 (somehow?!!)
l <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), L = l)

# Wald test:
#   ----------
#   
#   Chi-squared test:
#   X2 = 5.5, df = 1, P(> X2) = 0.019

#okay now you are looking for numbers from your glm that you can use to communicate with humans.
## odds ratios only
exp(coef(mylogit))

## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))

# Waiting for profiling to be done...
#             OR        2.5 %       97.5 %
# (Intercept) 0.0185001 0.001889165 0.1665354
# gre         1.0022670 1.000137602 1.0044457
# gpa         2.2345448 1.173858216 4.3238349
# rank2       0.5089310 0.272289674 0.9448343
# rank3       0.2617923 0.131641717 0.5115181
# rank4       0.2119375 0.090715546 0.4706961
> 
#I think what they're saying this means is that if gpa increase of one unit
#makes you more likely to get into grad school by factor of 2.23, 
#then people in the 97.5% have an increase by a factor of 4.32% I think.
#fuck knows what OR is..? lol.
  

#TESTING THE MODEL
#We will start by calculating the predicted probability of admission at each value of rank, 
#holding gre and gpa at their means. First we create and view the data frame. 
#struggling to understand this. 

newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))

#so this data frame is literally only 4 rows long. 
## view data frame
newdata1
#    gre    gpa rank
#1 587.7 3.3899    1
#2 587.7 3.3899    2
#3 587.7 3.3899    3
#4 587.7 3.3899    4
length(newdata1)
#don't know why it's 3? why isn't it 4? 3 = number of columns
#[1] 3

#okay predict the possibility by using the predict function. 
#give it the glm, the newdata? 
#newdata should contain a column for each of your predictive variables, alt and sdist. 
#https://stats.stackexchange.com/questions/91168/r-multiple-linear-regression-model-and-prediction-model
#so this is the prediction with mean gre and gpa. 
#the prediction is taking all three together, so rank is important, as we can see from the prediction.
newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
newdata1
#     gre    gpa rank     rankP
# 1 587.7 3.3899    1 0.5166016
# 2 587.7 3.3899    2 0.3522846
# 3 587.7 3.3899    3 0.2186120
# 4 587.7 3.3899    4 0.1846684

#In the above output we see that the predicted probability of being accepted into a graduate program 
#is 0.52 for students from the highest prestige undergraduate institutions (rank=1), 
#and 0.18 for students from the lowest ranked institutions (rank=4), holding gre and gpa at their means. 
#We can do something very similar to create a table of predicted probabilities varying the value of gre and rank. 
#We are going to plot these, so we will create 100 values of gre between 200 and 800, at each value of rank (i.e., 1, 2, 3, and 4).

newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100), 4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))

#breakdown: start at 200, go to 800, break into 100 pieces, and repeat 4 times.
#rep(<thing>, 4) 
#seq(from = A, to = B, length.out = <times_to_cut>)
rep(seq(from = 200, to = 800, length.out = 100), 4)
# 200.0000
# 206.0606
# 212.1212
# 218.1818

#breakdown. Repeat the numbers 1 through 4, 100 times each.
rep(1:4, each = 100)
# [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# [57] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2
# [113] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
# [169] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
# [225] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
# [281] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
# [337] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
# [393] 4 4 4 4 4 4 4 4


head(newdata2)
length(newdata2)
nrow(newdata2)
#400 rows. 100 of each rank, with different GRE and same GPA.
#okay so here we have a table with 100 variations of gre mapped to rank 1-4 
#(remember 1 is harvard and 4 is pellisippi state)
#so we have 
#        gre    gpa rank
# 1 200.0000 3.3899    1
# 2 206.0606 3.3899    1
# 3 212.1212 3.3899    1
# 4 218.1818 3.3899    1
# 5 224.2424 3.3899    1
# 6 230.3030 3.3899    1

#magical stuff to create predications. who the fuck knows how this works?

newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link", se = TRUE))
#se = TRUE is A switch indicating if standard errors are required.
head(newdata3)
#okay, here is what is happening. newdata3 is a dataframe that binds newdata2 itself 
#to the prediction for mydata2. this time predict uses type = "link" 
#to create the fit, se.fit and residual.scale information.
#fit	vector or matrix as above
#se.fit	#standard error of predicted means
#residual.scale	  #residual standard deviations
#df	 #degrees of freedom for residual

#Above, with newdata1, they used type="response" to just get the predictor variable.
#now there is a fit, se.fit and residual.scale variable for each permutation of gre, gpa and rank
#       gre    gpa rank        fit    se.fit residual.scale
# 1 200.0000 3.3899    1 -0.8114870 0.5147714              1
# 2 206.0606 3.3899    1 -0.7977632 0.5090986              1
# 3 212.1212 3.3899    1 -0.7840394 0.5034491              1
# 4 218.1818 3.3899    1 -0.7703156 0.4978239              1
# 5 224.2424 3.3899    1 -0.7565919 0.4922237              1
# 6 230.3030 3.3899    1 -0.7428681 0.4866494              1

#magic code to do predicted probabilities. Adding these three cols to dataframe. 
#this is like a magic lambda from python. within means use on the data you have.
#okay here we are adding three new columns, using the "fit" and "se.fit" columns that
#we just created with the c(bind) on predict. 

#plogis is part of Density, distribution function, quantile function and random generation 
#for the logistic distribution with parameters location and scale.
#plogis takes argument q, a vector of quantiles. So I guess that fit is a vector of quantiles.

#The code to generate the predicted probabilities (the first line below) is the same as before, 
#except we are also going to ask for standard errors so we can plot a confidence interval. 
#We get the estimates on the link scale and back transform both the predicted values 
#and confidence limits into probabilities.

#translation: using "type = link" we get a scale that we can use in the plogis argument.
#so I think that (fit + (1.96 * se.fit)) creates the colored band. 
#UL = upper limit and LL = lower limit. 
#so just to talk through 1 example, the first one in the dataframe below.
#the upper limit is .54 (red) and the lowerlimit is (.1393) and the predicted is .307
#for person with gre 200, gpa is mean 3.3899 and rank of school is 1. 


newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

## view first few rows of final dataset
head(newdata3)
# now has predicted probabilities. I think it's saying that a person with 200 GRE from top ranked school has 30% chance of getting in.
#        gre    gpa rank        fit    se.fit residual.scale        UL        LL PredictedProb
# 1 200.0000 3.3899    1 -0.8114870 0.5147714              1 0.5492064 0.1393812     0.3075737
# 2 206.0606 3.3899    1 -0.7977632 0.5090986              1 0.5498513 0.1423880     0.3105042
# 3 212.1212 3.3899    1 -0.7840394 0.5034491              1 0.5505074 0.1454429     0.3134499
# 4 218.1818 3.3899    1 -0.7703156 0.4978239              1 0.5511750 0.1485460     0.3164108
# 5 224.2424 3.3899    1 -0.7565919 0.4922237              1 0.5518545 0.1516973     0.3193867
# 6 230.3030 3.3899    1 -0.7428681 0.4866494              1 0.5525464 0.1548966     0.3223773

#make magic plot!
ggplot(newdata3, aes(x = gre, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
ymax = UL, fill = rank), alpha = 0.2) + geom_line(aes(colour = rank), size = 1)

#graph shows that top tier school with 800 GRE has an admit possibility of over .6 (duh) 
#Standard deviation is .46, so .6 is very good odds. Low GRE from bottom tier school is lower than .2

##### 
#testing model fit

#We may also wish to see measures of how well our model fits. 
#This can be particularly useful when comparing competing models. 
#The output produced by summary(mylogit) included indices of fit (shown below the coefficients), 
#including the null and deviance residuals and the AIC. 
#One measure of model fit is the significance of the overall model. 
#This test asks whether the model with predictors fits significantly 
#better than a model with just an intercept (i.e., a null model). 
#The test statistic is the difference between the residual deviance 
#for the model with predictors and the null model. 
#The test statistic is distributed chi-squared with degrees 
#of freedom equal to the differences in degrees of freedom 
#between the current and the null model (i.e., the number of predictor variables in the model). 
#To find the difference in deviance for the two models (i.e., the test statistic) we can use the command:

with(mylogit, null.deviance - deviance)
#[1] 41.45903

#The degrees of freedom for the difference between the two models is equal 
#to the number of predictor variables in the mode, and can be obtained using:

with(mylogit, df.null - df.residual)
#[1] 5

#Finally, the p-value can be obtained using:
with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
#[1] 7.578194e-08

#i don't understand how 7.57etc = 0.001
#excel says that number is 0.00000007578194, so much less than 0.001

#The chi-square of 41.46 with 5 degrees of freedom and an associated p-value 
#of less than 0.001 tells us that our model as a whole fits significantly better than an empty model. 
#This is sometimes called a likelihood ratio test (the deviance residual is -2*log likelihood). 
#To see the model’s log likelihood, we type:

#this is the log likelihood. whatever the fuck that is. omg. 
logLik(mylogit)
#'log Lik.' -229.2587 (df=6)


