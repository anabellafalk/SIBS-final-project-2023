setwd("~/SIBS_hack")

library(NHANES)
library(scales)
library(car)
library(bestglm)
library(glmnet)

data <- read.csv("imputed_data.csv", header=TRUE) #reading in the data
data <- subset(data, select = -c(1) ) #removing duplicate column



library(fastDummies) #package to create dummy vars for factorized vars w/ multiple levels
data <- dummy_cols(
  data,
  select_columns = c('DLIT_AG', 'ZSN_A', 'ant_im', 'lat_im', 'inf_im', 'post_im', 'TIME_B_S', 'R_AB_1_n', 'R_AB_2_n', 'R_AB_3_n', 'NA_R_1_n', 'NA_R_2_n', 'NA_R_3_n', 'NOT_NA_1_n','NOT_NA_2_n', 'NOT_NA_3_n'  ),
  remove_first_dummy = TRUE, #avoids multicollinearity
  remove_most_frequent_dummy = FALSE,
  ignore_na = FALSE,
  split = NULL,
  remove_selected_columns = FALSE,
  omit_colname_prefix = FALSE
)

data <- subset(data,select =-c(DLIT_AG, ZSN_A, ant_im, lat_im, inf_im, post_im, TIME_B_S, R_AB_1_n, 
                               R_AB_2_n, R_AB_3_n, NA_R_1_n, NA_R_2_n, NA_R_3_n, NOT_NA_1_n,NOT_NA_2_n, NOT_NA_3_n))

yy = data[ , "FIBR_PREDS" ]
afib_index <- which.max(names(data)=="FIBR_PREDS")
xx = as.matrix (subset(data, select = -c(afib_index) ))



#Running Lasso regression to try to weed out variables that aren't useful
reg <- glmnet(xx, yy, alpha=1, standardize=TRUE) 
plot(reg, xvar="lambda") #solutions paths

set.seed(0131) #same random split each time
cv.lasso <- cv.glmnet(xx, yy, alpha=1, standardize=TRUE, nfolds=10, family="binomial")
plot (cv.lasso)

cv.lasso$lambda.min

#lasso.coef <- coef(cv.lasso, s=cv.lasso$lambda.min) #minimizing the binomial deviance
lasso.coef <- coef(cv.lasso, s=0.02) #trying diff values of lambda to get a reasonable # of predictors

as.vector(lasso.coef) #showing coefficients that have been shrunk to 0 as 0 instead of .'s

lasso_df <- data.frame(c("Intercept", colnames(xx)), as.vector(lasso.coef)) #coefficients
 
zero_coeff_index <- which(lasso_df[,2]==0)
#creating data frame of nonzero coefficients
lasso_df <- lasso_df [-(zero_coeff_index),]

#arranging them in decreasing order of absolute value
order <- order(abs(lasso_df[,2]), decreasing=TRUE) 
ordered_df <- lasso_df[order,]
colnames(ordered_df) <- c("Covariate (or Intercept)", "Coefficient")
ordered_df
length(ordered_df[,1])


ordered_df[-2,1]


#Checking I can get the individual names of the significant covariates and they're correct:
for (i in lasso_df[-1,1])
{
  print (i)
}
#Separating out the useful columns in our original data frame and making a new data frame:
data_sig <- subset (data, select=c("FIBR_PREDS",lasso_df[-1,1]))

#Descriptive stats: split into have Afib and don't have afib, see the "mode" for each
head(data_sig)
afib_yes <- data_sig[data_sig$FIBR_PREDS==1,]
afib_no <- data_sig[data_sig$FIBR_PREDS==0,]

#Finding the mode of each column for the categorical variables
#NOTE: THIS IS NO LONGER NEEDED SINCE I SWITCHED TO USING PROPORTION FOR BINARY VARS.
#Defining function for statistical mode since R doesn't have one:
# modeof <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }

#function for writing proportions:
prop <- function (vec)
{
  yes = sum(vec)
  return (yes/length(vec))
}
prop (afib_yes[,3]) #testing it on a random column

#creating a list of mean (numeric) and proportion (categorical) for each of the variables in afib_yes
afib_yes_stats = data.frame(matrix(nrow = 1, ncol = length(data_sig[1,])))  #empty matrix to include the desired descriptive stats for each covariate
colnames(afib_yes_stats) <- colnames(afib_yes)
for (i in c(1:length(colnames(data_sig))))
{
  if(i %in% c(2,41,42))
  {
    afib_yes_stats[i]<-(mean(afib_yes[,i]))
  }
  else
  {
    afib_yes_stats[i]<-(prop(afib_yes[,i]))  
  }
}


#creating a list of mean (numeric) and proportion (categorical) for each of the variables in afib_yes
afib_no_stats = data.frame(matrix(nrow = 1, ncol = length(data_sig[1,])))  #empty matrix to include the desired descriptive stats for each covariate
colnames(afib_no_stats) <- colnames(afib_no)
for (i in c(1:length(colnames(data_sig))))
{
  if(i %in% c(2,41,42))
  {
    afib_no_stats[i]<-(mean(afib_no[,i]))
  }
  else
  {
    afib_no_stats[i]<-(prop(afib_no[,i]))  
  }
}


