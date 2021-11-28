### 0. Install libraries
if(!require(tibble)) 
        install.packages('tibble',repos = "http://cran.us.r-project.org")
if(!require(dplyr)) 
        install.packages('dplyr',repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) 
        install.packages('ggplot2',repos = "http://cran.us.r-project.org")
if(!require(leaps)) 
        install.packages('leaps',repos = "http://cran.us.r-project.org")
library(tibble)
library(dplyr)
library(ggplot2)
library(leaps)


### 1. Overview of Data
red <- read.csv("../data/winequality-red.csv", header = TRUE, sep = ";")
# white <- read.csv("../data/winequality-white.csv", header = TRUE, sep = ";")

        # 1) summary of the data
summary(red)

        # 2) histogram: distribution of each variable
varnames = colnames(red)

par(mfrow = c(3, 4))

for(name in varnames){
        hist(red[[name]], main = name, xlab = NULL, ylab = NULL )
}

        # 3) scatter plot: 
        #       average distribution of each variable based on the quality
mean_summary = red %>% group_by(quality) %>% summarise_all(mean)

par(mfrow = c(3, 4))

for(name in varnames){
        plot(mean_summary$quality,
             mean_summary[[name]],
             xlab = "quality",
             ylab = name
             )
}

        # 4)
ggplot(data = red) + geom_histogram(aes(x = total.sulfur.dioxide, 
                                        fill = as.factor(quality)))

        # 5)
lm_red <- lm(quality ~ ., red)
summary(lm_red)

par(mfrow = c(2, 2))
plot(lm_red)

cor(red)

### 3 Transformations

colnames(red)

logred = red

logred$logFA = log(red$fixed.acidity)
logred$logVA = log(red$volatile.acidity)
logred$logCA = red$citric.acid
logred$logRS = log(red$residual.sugar)
logred$logCL = log(red$chlorides)
logred$logFS = log(red$free.sulfur.dioxide)
logred$logTS = log(red$total.sulfur.dioxide)
logred$logDE = log(red$density)
logred$logPH = red$pH
logred$logSP = log(red$sulphates)
logred$logAL = log(red$alcohol)

lm_all = lm(quality ~., red)

lm_log = lm(quality~. , logred[,12:23])


df1 = logred[,1:12]
lm1 = lm(quality~., df1)
aic1 = AIC(lm1)
for(i in 1:11){
        df2 = df1
        df2[,i] = logred[12+i]
        colnames(df2)[i] = colnames(logred[12+i])
        lm2 = lm(quality~., df2)
        aic2 = AIC(lm2)
        if(aic2<aic1){
                df1 = df2
                lm1 = lm2
                aic1 = aic2
                print(paste0('switching ', i))
        }
}

head(df1)

summary(lm1)


### 4. Variable Selection
lm_red <- lm(quality ~ ., df1)
summary(lm_red)

require(leaps)
b <- regsubsets(quality ~ ., df1, nvmax = 12)
rs <- summary(b)

par(mfrow = c(1, 3))
          # AIC
rs$which

# There's a function AIC, we shouldn't use this variable name
p = 11
AIC <- nrow(red)*log(rs$rss/nrow(red)) + (2:(p+1))*2
plot(AIC ~ I(1:p), 
     #main = "AIC",
     ylab = 'AIC',
     xlab = 'Number of Predictors', pch = 20)
text(I(1:p), AIC, round(AIC, 2), cex = 0.5, pos = 4)
which.min(AIC)

          # Adjusted R-Squre
plot(1:p, rs$adjr2,
     xlab = 'Number of Predictors',
     ylab='Adjusted R-Square', pch = 20)
which.max(rs$adjr2)

          # Mellow's Cp
plot(2:(p+1), rs$cp,
     xlab = 'Number of Parameters',
     ylab = 'Cp Statistic', pch = 20)
abline(0,1)

par(mfrow = c(1, 1))

# LASSO

library(glmnet)

cv_lasso = cv.glmnet(as.matrix(df1[,-1]), df1[,12],
                     type.measure = 'mae')
plot(cv_lasso)

# No need for penalization

### 5. Estimation of Error

n = nrow(df1)
df_train = df1[(1:n)%%10!=0,]
df_test = df1[(1:n)%%10==0,]

lm_train = lm(quality ~., data = df_train)
test_pred = predict(lm_final, df_test)
plot(test_pred, df_test$quality)

mean((test_pred-df_test$quality)^2)
mean((test_pred-df_test$quality)^2) - 1/12

table(round(test_pred), df_test$quality)

lm_final = lm(quality~., data = df1)
table(round(predict(lm_final, data = df1)), df1$quality)


### 6. Resampling?


wt = 1/sqrt(table(df1$quality))
wts = wt[df1$quality-2]

lm_wt = lm(quality~., data = df1, weights = wts)
table(round(predict(lm_wt, data = df1)), df1$quality)
