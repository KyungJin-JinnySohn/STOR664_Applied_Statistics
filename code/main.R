### 0. Install libraries
#install.packages("leaps")
library(leaps)


### 1. Reading data and dealing with NA values 
red <- read.csv("./data/winequality-red.csv", header = TRUE, sep = ";")
#white <- read.csv("./data/winequality-white.csv", header = TRUE, sep = ";")

          # Delete rows with NA values
red <- red[complete.cases(red), ]

          # Delete duplicated rows
red <- red[!duplicated(red), ]
# red[duplicated(red), ]

### 2. Variable Selection
lm_red <- lm(quality ~ ., red)
summary(lm_red)

require(leaps)
b <- regsubsets(quality ~ ., red)
rs <- summary(b)

par(mfrow = c(1, 3))
          # AIC
rs$which
AIC <- nrow(red)*log(rs$rss/nrow(red)) + (2:9)*2
plot(AIC ~ I(1:8), 
     #main = "AIC",
     ylab = 'AIC',
     xlab = 'Number of Predictors', pch = 20)
text(I(1:8), AIC, round(AIC, 2), cex = 0.5, pos = 4)

          # Adjusted R-Squre
plot(1:8, rs$adjr2,
     xlab = 'Number of Predictors',
     ylab='Adjusted R-Square', pch = 20)
which.max(rs$adjr2)

          # Mellow's Cp
plot(2:9, rs$cp,
     xlab = 'Number of Parameters',
     ylab = 'Cp Statistic', pch = 20)
abline(0,1)
par(mfrow = c(1, 1))