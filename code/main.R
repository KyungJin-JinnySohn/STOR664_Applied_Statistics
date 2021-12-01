### 0. Install libraries
if(!require(tibble)) 
        install.packages('tibble', repos = "http://cran.us.r-project.org")
if(!require(dplyr)) 
        install.packages('dplyr', repos = "http://cran.us.r-project.org")
if(!require(reshape2)) 
        install.packages('reshape2', repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) 
        install.packages('ggplot2', repos = "http://cran.us.r-project.org")
if(!require(leaps)) 
        install.packages('leaps', repos = "http://cran.us.r-project.org")
if(!require(pls)) 
        install.packages('pls', repos = "http://cran.us.r-project.org")
if(!require(glmnet)) 
        install.packages('glmnet', repos = "http://cran.us.r-project.org")
library(tibble)
library(dplyr)
library(reshape2)
library(ggplot2)
library(leaps)
library(pls)
library(glmnet)


### 1. Overview of Data
red = read.csv("../data/winequality-red.csv", header = TRUE, sep = ";")
# white = read.csv("../data/winequality-white.csv", header = TRUE, sep = ";")

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

# 4) histogram: distribution of each variable grouped by quality
ggplot(gather(red, key, value, -c(quality), factor_key = TRUE), 
       aes(value, fill = factor(quality))) + 
        geom_histogram(bins = 30) + 
        facet_wrap(~ key, scales = 'free_x')


# 5) linear regression analysis of original data
lm_ori = lm(quality ~ ., red)
summary(lm_ori)

par(mfrow = c(2, 2))
plot(lm_ori)

# 6) Check multicolinearity
cor(red)
ifelse(abs(cor(red))>0.5, cor(red), 0)

### 2. Transformations
colnames(red)

logred = red
# 1) add 11 new columns that transform each column data into log(data) 
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

# 2) compare the AIC (lm_ori vs lm using the log(data) as a predictor)
lm_log = lm(quality ~ ., logred[,12:23])

comp_AIC = data.frame(lm = c("Ori.", "Log."), 
                      AIC = c(AIC(lm_ori), AIC(lm_log)))
comp_AIC$AIC = round(comp_AIC$AIC, 3)
comp_AIC

# 3) select the predictor to use with the log transformation.
boolExpand = expand.grid(c(0,1), c(0,1), c(0,1), c(0,1), c(0,1), 
                          c(0,1), c(0,1), c(0,1), c(0,1))
boolExpand = cbind(boolExpand[,1:2], 0, boolExpand[,3:7], 0, boolExpand[,8:9])

df1 = logred[,1:12] # ori. data
lm1 = lm(quality ~ ., df1) # lm model
aic1 = AIC(lm1) # ori. AIC

for (i in 1:nrow(boolExpand)) { 
        bool = as.numeric(boolExpand[i,])  
        df_trans = logred[, c(bool*12 + 1:11, 12)]
        
        lm2 = lm(quality ~ ., df_trans)
        aic2 = AIC(lm2)
        if(aic2 < aic1){
                df1 = df_trans
                lm1 = lm2
                aic1 = aic2
                print(paste0('switching ', i))
        }
}

head(df1) 
names(df1) # final columns selected

# 4) compare the AIC (lm_ori vs lm_log vs final model after transformation)
comp_AIC = rbind(comp_AIC, c("Trans.", round(AIC(lm1), 3)))
comp_AIC

# 5) Map dependent variable to the real line.
logitlm = lm(log(quality/(11-quality)) ~., data = df1)

summary(logitlm)
summary(lm1)

AIC_correction = sum(log(11/df1$quality/(11-df1$quality))) *2

comp_AIC = rbind(comp_AIC, c("Logit.", round(AIC(logitlm) - AIC_correction, 3)))
comp_AIC


### 3. Variable Selection
colnames(df1)

quadred = df1
# 1) add 11 new columns that transform each column data into (data)^2
quadred$quadFA = (quadred$logFA)^2
quadred$quadVA = (quadred$volatile.acidity)^2
quadred$quadCA = (quadred$citric.acid)^2
quadred$quadRS = (quadred$logRS)^2
quadred$quadCL = (quadred$chlorides)^2
quadred$quadFS = (quadred$logFS)^2
quadred$quadTS = (quadred$total.sulfur.dioxide)^2
quadred$quadDE = (quadred$logDE)^2
quadred$quadPH = (quadred$pH)^2
quadred$quadSP = (quadred$logSP)^2
quadred$quadAL = (quadred$alcohol)^2


lm_trans = lm(quality ~ ., quadred)
summary(lm_trans)

# 1) AIC
require(leaps)
b = regsubsets(quality ~ ., quadred, nvmax = 22)
rs = summary(b)

rs$which

par(mfrow = c(1, 3))

p = 22
eval_aic = nrow(red)*log(rs$rss/nrow(red)) + (2:(p+1))*2
plot(eval_aic ~ I(1:p), 
     ylab = 'AIC',
     xlab = 'Number of Predictors', pch = 20)
text(I(1:p), eval_aic, round(eval_aic, 2), cex = 0.5, pos = 4)
paste("min AIC: ", which.min(eval_aic))

min_aic = which.min(eval_aic)
boolCol_step = as.vector(rs$which[min_aic, -1]) # columns selected by min AIC

# 2) Adjusted R-Squre
plot(1:p, rs$adjr2,
     xlab = 'Number of Predictors',
     ylab='Adjusted R-Square', pch = 20)
paste("max Adjusted R-Squre: ", which.max(rs$adjr2))

# 3) Mellow's Cp
plot(2:(p+1), rs$cp,
     xlab = 'Number of Parameters',
     ylab = 'Cp Statistic', pch = 20)
abline(0,1)

par(mfrow = c(1, 1))

# 4) Principal component regression
require(pls)
set.seed(664)
lm_pcr = pcr(quality ~ ., data = quadred, validation = "CV", ncomp = 22)
cv_pcr = RMSEP(lm_pcr, estimate = "CV")

min_pcr = which.min(cv_pcr$val) - 1

plot(cv_pcr, main = "")

# 5) Partial least squares regression
set.seed(664)
lm_pls = plsr(quality ~ ., data = quadred, validation = "CV", ncomp = 22)
cv_pls = RMSEP(lm_pls, estimate = "CV")

min_pls = which.min(cv_pls$val) -1

par(mfrow = c(1,2))
coefplot(lm_pls, ncomp = min_pls, xlab = "Frequency")
plot(cv_pls, main = "")
par(mfrow = c(1,1))


# 6) Ridge
set.seed(664)
cv_ridge = cv.glmnet(as.matrix(quadred[,-12]), quadred[,12],
                     alpha = 0, type.measure = 'mae',
                     lambda = seq(0, 0.1, len = 100))
plot(cv_ridge)
opt_lambda_ridge = cv_ridge$lambda.min
lm_ridge = cv_ridge$glmnet.fit

opt_lambda_ridge # optimum lambda by ridge regression

# 7) LASSO
set.seed(664)
cv_lasso = cv.glmnet(as.matrix(quadred[,-12]), quadred[,12],
                     alpha = 1, type.measure = 'mae',
                     lambda = seq(0, 0.1, len = 100))
plot(cv_lasso)
opt_lambda_lasso = cv_lasso$lambda.min
lm_lasso = cv_lasso$glmnet.fit

opt_lambda_lasso # optimum lambda by LASSO
lasso_beta = lm_lasso$beta[,which(lm_lasso$lambda == opt_lambda_lasso)]
boolCol_lasso = as.vector(lasso_beta != 0) # column selected by LASSO

# 8) comparing final model selected using AIC vs LASSO
find_hierarchical <- function(boolCol, p){
        return(c(boolCol[1:p]|boolCol[(p+1):(2*p)], T, boolCol[(p+1):(2*p)]))
}

boolCol_step = find_hierarchical(boolCol_step, 11)
boolCol_lasso = find_hierarchical(boolCol_lasso, 11)

df_step_hier = quadred[,boolCol_step]
df_lasso_hier = quadred[,boolCol_lasso]

lm_step_hier = lm(quality ~., data = df_step_hier)
lm_lasso_hier = lm(quality ~., data = df_lasso_hier)

summary(lm_step_hier)
summary(lm_lasso_hier)

comp_AIC = rbind(comp_AIC, c("quad_step", round(AIC(lm_step_hier), 3)))
comp_AIC = rbind(comp_AIC, c("quad_lasso", round(AIC(lm_lasso_hier), 3)))
comp_AIC


### 4. Weighted Least Squares for imbalanced data
df_final = df_step_hier
n = nrow(df_final)
df_train = df_final[(1:n)%%5 != 0,]
df_test = df_final[(1:n)%%5 == 0,]

wt1 = 1/sqrt(table(df_train$quality))
wts1 = wt1[df_train$quality-2]

wt2 = 1/table(df_train$quality)
wts2 = wt2[df_train$quality-2]

lm_train = lm(quality~., data = df_train)
table(round(predict(lm_train, newdata = df_test)), df_test$quality)

lm_wt1 = lm(quality~., data = df_train, weights = wts1)
table(round(predict(lm_wt1, newdata = df_test)), df_test$quality)

lm_wt2 = lm(quality~., data = df_train, weights = wts2)
table(round(predict(lm_wt2, newdata = df_test)), df_test$quality)

wt = 1/table(df_final$quality)
wts = wt[df_final$quality-2]

lm_final = lm(quality ~ ., data = df_final, weights = wts)
table(round(predict(lm_final, data = df_final)), df_final$quality)
summary(lm_final)

par(mfrow = c(1, 2))

plot(lm_final, which = 1)
plot(lm_final, which = 2)

par(mfrow = c(1, 1))


### 5. Estimation of Error
lm_train = lm(quality ~ ., data = df_train, weights = wts2)
test_pred = predict(lm_train, df_test)
plot(test_pred, df_test$quality)

mean((test_pred-df_test$quality)^2)
mean((test_pred-df_test$quality)^2) - 1/12

table(round(test_pred), df_test$quality)


### 6. Optimal Wine Design
summary(df_final)

optx = -lm_final$coefficients[c(2,3,7:10)]/lm_final$coefficients[c(12:17)]/2

optwine = data.frame(logFA = optx[1], quadFA = optx[1]^2, 
                     volatile.acidity = min(df_final$volatile.acidity), 
                     quadVA = min(df_final$volatile.acidity)^2,
                     logRS = min(df_final$logRS),
                     chlorides = max(df_final$chlorides),
                     logFS = max(df_final$logFS),
                     total.sulfur.dioxide = optx[3], quadTS = optx[3]^2,
                     logDE = min(df_final$logDE), quadDE = min(df_final$logDE)^2,
                     pH = min(df_final$pH), quadPH = min(df_final$pH)^2,
                     logSP = optx[6], quadSP = optx[6]^2,
                     alcohol = max(df_final$alcohol)
                     )

predict(lm_final, newdata = optwine)
