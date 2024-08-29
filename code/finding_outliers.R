

mydata <- read.csv('~/SIBS_hack/MI_orig.csv')

cont_vars <- c (2, 35, 36, 37, 38, 84, 86, 87, 88, 89, 90, 91)

for (i in cont_vars)
{
  boxplot(mydata[,i], main = i)
  hist(mydata[,i], main=i)
}

#box plots with prominent outliers: 35 (S_AD_KBRIG), 36 (D_AD_KBRIG), 91 (ROE)
# - 35 and 36 were already removed, so 91 is what we need to worry about

