#############################
# Thomas Cerbelaud | Shravan Kumar Chandrasekaran | Mathilde Sirbu
# SIEO 4150 
# Homework 1 , Group problem
# Due date: 09/15
#############################

getwd()
dir = "Historical prices/"

# Loading the data sets
cnx<-read.csv(paste(dir, "cnx.csv", sep = ""))
cop<-read.csv(paste(dir, "cop.csv", sep = ""))
cvx<-read.csv(paste(dir, "cvx.csv", sep = ""))
ipwr<-read.csv(paste(dir, "ipwr.csv", sep = ""))
jks<-read.csv(paste(dir, "jks.csv", sep = ""))
scty<-read.csv(paste(dir, "scty.csv", sep = ""))
slb<-read.csv(paste(dir, "slb.csv", sep = ""))
spwr<-read.csv(paste(dir, "spwr.csv", sep = ""))
vlo<-read.csv(paste(dir, "vlo.csv", sep = ""))
xom<-read.csv(paste(dir, "xom.csv", sep = ""))
brent <- read.csv(paste(dir, "brent.csv", sep = ""))

# Calculating the log returns
col = c("CNX","COP","CVX","IPWR","JKS","SCTY","SLB","SPWR","VLO","XOM","BRENT")
df_prices = data.frame(CNX=cnx["Close"], COP=cop["Close"], CVX=cvx["Close"], 
                       IPWR=ipwr["Close"], JKS=jks["Close"], SCTY=scty["Close"], 
                       SLB=slb["Close"], SPWR=spwr["Close"], VLO=vlo["Close"], 
                       XOM=xom["Close"],BRENT=brent["Close"])
colnames(df_prices) <- col
df_log_return <- data.frame(matrix(NA, nrow = 250, ncol = 11))
colnames(df_log_return) <- col

for (i in 2:250) {
  for (k in 1:11)
  {
    df_log_return[i, k] <- log(df_prices[i, k]) - log(df_prices[i-1, k])
  }
}

for (k in 1:11) {
  df_log_return[1, k] <- 0
}

#Plotting histograms
for (i in col)
  {
  hist(df_log_return[,i], plot = TRUE, breaks=100, main=i)
}

# Computing mean and standard deviation of each stocks, and plotting them.
l <- list("Standard deviation", "Mean")
df_log_meansd <- data.frame(matrix(NA, nrow = 2, ncol = 11), 
                            row.names = l)
colnames(df_log_meansd) <- col

for (k in 1:11) {
  df_log_meansd[1, k] <- sd(df_log_return[,k])
  df_log_meansd[2, k] <- mean(df_log_return[,k])
}
df_log_meansd<-t(df_log_meansd)
plot(df_log_meansd[,c(2,1)])

# Randomness test
# For more insight about the test, its hypothesis and statistic, see 
# http://www.itl.nist.gov/div898/handbook/eda/section3/eda35d.htm
library(lawstat)
df_rand_test <- data.frame()
for (c in col) {
  p <- runs.test(df_log_return[[paste(c)]])$p.value
  df_rand_test[c,1] <- p
}
colnames(df_rand_test) <- "p-value"
df_rand_test

# Performs the Shapiro-Wilk test of normality. "Since the test is biased by sample size,
# the test may be statistically significant from a normal distribution in any large samples. 
# Thus a Q–Q plot is required for verification in addition to the test. See Wikipedia page one the test for more
# insight about the statistic: https://en.wikipedia.org/wiki/Shapiro%E2%80%93Wilk_test.

l <- list("      W", "     p-value")
df_log_shap <- data.frame(matrix(NA, nrow = 2, ncol = 10), 
                          row.names = l)
colnames(df_log_shap) <- col [! col %in% c("BRENT")]
df_log_shap

for (k in col [! col %in% c("BRENT")]) {
  df_log_shap[1, k] <- shapiro.test(df_log_return[,k])[1]
  df_log_shap[2, k] <- shapiro.test(df_log_return[,k])[2]
  qqnorm(y = df_log_return[, k], plot.it = TRUE, 
         ylab = paste(k, "distribution quantiles", sep = " "))
  abline(v = 0)
  abline(h = 0)
  qqline(df_log_return[, k])
}
df_log_shap
df_log_shap<-t(df_log_shap)
df_log_shap
plot(df_log_shap[,c(2,1)])

