#########################################################
#--------------- MATH 189: Case study 3 ----------------#
# Names: Matin Ghaffari, James Lu                       #
# PID  : A16617005, A16687580                           #
#########################################################

library(ggplot2)
library(lattice)
library(reshape2)
library(dplyr)

data <- read.csv("hcmv.txt")

n = 296
N = 229354

########################################################
#                        q1
#---------------------------------------------
# The Dataset Compared to Random Scattering
########################################################

hist(data$location, breaks=58, xlim=c(0, 240000), ylim=c(0, 20),
     main = 'Frequency of Palindromes in CMV DNA',
     xlab='Location on Chain of Base Pairs')

#three simulations of 296 from 229354
ran1 <- runif(n, 0, N)
ran2 <- runif(n, 0, N)
ran3 <- runif(n, 0, N)

#sample from our simulations
hist(ran1, breaks=58, xlim=c(0, 240000), ylim=c(0, 20),
     main = 'First Simulation of Uniformly Random Palindromes in CMV DNA',
     xlab='Location on Chain of Base Pairs')
hist(ran2, breaks=58, xlim=c(0, 240000), ylim=c(0, 20),
     main = 'Second Simulation of Uniformly Random Palindromes in CMV DNA',
     xlab='Location on Chain of Base Pairs')
hist(ran3, breaks=58, xlim=c(0, 240000), ylim=c(0, 20),
     main = 'Third Simulation of Uniformly Random Palindromes in CMV DNA',
     xlab='Location on Chain of Base Pairs')

########################################################
#                        q2
#-----------------------------------------
#Analysis of Locations and Spacings of the Palindromes 
########################################################

#consecutive palindromes
spacings <- diff(data$location, lag=1)
lambda <- 1 / mean(spacings)
labl = paste("Distance between", "Consecutive","Palindromes")

hist(spacings, col=rgb(1,0,0,0.5), 
     main="Palindrome Spacings Distribution Comparison", 
     breaks=seq(0, 8000, 500),
     xlab=labl)
hist(rgamma(n, shape=1, rate=lambda), add=T, breaks=seq(0, 10000, 500),
     col=rgb(0,1,0,0.5))
legend('topright', legend=c('Observed','Exponential / Gamma'), 
       lwd = c(4, 4), col=c(rgb(1,0,0,0.5), rgb(0,1,0,0.5)))

#sum of pairs of palindromes
spacings <- diff(data$location, lag=2)
lambda <- 1 / mean(spacings)
labl = paste("Distance between", "Paired","Palindromes")

hist(spacings, col=rgb(1,0,0,0.5), 
     main="Palindrome Spacings Distribution Comparison", 
     breaks=seq(0, 15000, 1000),
     xlab=labl)
hist(rgamma(n, shape=2, rate=lambda), add=T, breaks=seq(0, 30000, 1000),
     col=rgb(0,1,0,0.5))
legend('topright', legend=c('Observed','Exponential / Gamma'), 
       lwd = c(4, 4), col=c(rgb(1,0,0,0.5), rgb(0,1,0,0.5)))

#sum of triplets of palindromes
spacings <- diff(data$location, lag=3)
lambda <- 1 / mean(spacings)
labl = paste("Distance between", "Triplet","Palindromes")

hist(spacings, col=rgb(1,0,0,0.5), 
     main="Palindrome Spacings Distribution Comparison", 
     breaks=seq(0, 25000, 1000),
     xlab=labl)
hist(rgamma(n, shape=3, rate=lambda), add=T, breaks=seq(0, 40000, 1000),
     col=rgb(0,1,0,0.5))
legend('topright', legend=c('Observed','Exponential / Gamma'),
       lwd = c(4, 4), col=c(rgb(1,0,0,0.5), rgb(0,1,0,0.5)))

#bin size 55, 58, 60 for residuals and histograms of locations
for (br in c(4200, 4000, 3800)) {
  labl1 = paste('Frequency of Palindromes in CMV DNA', 
                "( Regions of Length", br, 
                ")")
  labl2 = paste('Standardized Residuals', "( Regions of Length", br, 
                ")")
  
  obs <- hist(data$location, breaks=seq(0, 240000, br), xlim=c(0, 240000), 
              ylim=c(0, 20),
       main = labl1,
       xlab='Location on Chain of Base Pairs', col=rgb(1,0,0,0.5))
  unif <- hist(ran1, breaks=seq(0, 240000, br), xlim=c(0, 240000), 
               ylim=c(0, 20), xlab='Location on Chain of Base Pairs', 
               col=rgb(0,1,0,0.5), add=T)
  legend('topright', legend=c('Observed','Uniform'),
         lwd = c(4, 4), col=c(rgb(1,0,0,0.5), rgb(0,1,0,0.5)))
  residual <- (obs$counts - unif$counts) / sqrt(unif$counts)
  plot(residual, type='h', main = labl2, ylab="Standard Residual")
  abline(h=3, col=rgb(1,0,0,.5))
}
########################################################
#                        q3
#---------------------------------------
# Analysis of Counts of Palindromes in Various
# Regions of Equal Length of DNA
########################################################

##################################
#        q3 58 intervals
#################################
counts <- as.vector(table(cut(data$location,  breaks=58, include.lowest = TRUE)))
c= c(sort(counts))
c
df <- data.frame(c)
f=rep("Observed" , 17)
b = df %>% count(sort(c))
b
n = c(0,5,7,4,9,8,6,13,2,2,0,0,1,0,0,0,1)
vv1 = c(0,5,7,4,9,8,6,13,2,2,0,0,1,0,0,0,1)
c=c(0:16)
b2 = data.frame(f,c,n)
b5=b2

lambda <- mean(counts)
theoriticalPoission = c()
for (i in c(0:15)) {
  theoriticalPoission = c(theoriticalPoission, dpois(i, lambda))
}
theoriticalPoission = c(theoriticalPoission,1 - sum(theoriticalPoission))

n1 = (theoriticalPoission * 58)
#c1= c(1,2,3,4,5,6,7,8,9,12,16)
c1= c(0:16)
f1=rep("Expected" , 17)
b6= data.frame(f1,c1,n1)
c = c(b2$c, c1)
Count = c(b2$f, f1)
n = c(b2$n, n1)
b2 <- data.frame(Count,c, n)

ggplot(b2, aes(fill=Count, y=n, x=c)) + 
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Expected Vs. Observed Counts for Interval of 58")+
  xlab("Pallindrome Count ") +
  ylab("Interval Counts")+ 
  geom_text(aes(label = round(n,1)), position = position_dodge(width=0.9)) +
  theme( legend.position = c(0,1), legend.justification=c(0,1))+
  theme(plot.title = element_text(hjust = 0.5))



tab <- table(cut(data$location, breaks= seq(0, 230000, length.out =59), include.lowest = 1));
counts <- as.vector(tab);
hist(counts, breaks=seq(0,16, by=1), col = rgb(1,0,0,0.5),
     probability = 1,
     main =  "Histogram Expected Vs.Observed Counts for 58 Intervals",
     xlab = "Palindrome Count Within 58 Sub Intervals",
     ylim = c(0,0.3), include.lowest = 1, right = 0)
lines(density(counts, adjust= 2), col = rgb(1,0,0,0.5))
Pois <- rpois(296, lambda = mean(counts))
hist(Pois, breaks=seq(0,16, by=1), col = rgb(0,0,1,0.5), add = 1, probability = 1,
     include.lowest = 1, right = 0);
lines(density(Pois, adjust = 2), col = rgb(0,0,1,0.5))+
  legend("topright", legend= c("observed counts", "expected counts"), lty = c(1,1), col= c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))

chisq.test(vv1,(n1))

##################################
#        q3 28 intervals
#################################
counts <- as.vector(table(cut(data$location,  breaks=28, include.lowest = TRUE)))
c= c(sort(counts))
df <- data.frame(c)
f=rep("Observed" , 17)
b = df %>% count(sort(c))
b
n = c(0,0,0,0,0,1,2,1,3,4,5,4,2,2,0,2,2)
vv1 = c(0,0,0,0,0,1,2,1,3,4,5,4,2,2,0,2,2)
length(n)
c=c(0:16)
b2 = data.frame(f,c,n)
b5=b2

lambda <- mean(counts)
theoriticalPoission = c()
for (i in c(0:15)) {
  theoriticalPoission = c(theoriticalPoission, dpois(i, lambda))
}
theoriticalPoission = c(theoriticalPoission,1 - sum(theoriticalPoission))


n1 = (theoriticalPoission * 28)
#c1= c(1,2,3,4,5,6,7,8,9,12,16)
c1= c(0:16)
f1=rep("Expected" , 17)
b6= data.frame(f1,c1,n1)
c = c(b2$c, c1)
Count = c(b2$f, f1)
n = c(b2$n, n1)
b2 <- data.frame(Count,c, n)


tab <- table(cut(data$location, breaks= seq(0, 230000, length.out =29), include.lowest = 1));
counts1 <- as.vector(tab)
c = 0
counts1
counts= c()
for (i in seq_along(counts1)) {
  if (counts1[i] >= 16) {
    counts = c(counts, 16)
  } else {
    counts = c(counts, counts1[i])
  }
}
counts
hist(counts,breaks=seq(0,16, by=1), col = rgb(1,0,0,0.5),
     probability = 1,
     main =  "Histogram Expected Vs.Observed Counts for 28 Intervals",
     xlab = "Palindrome Count Within 28 Sub Intervals",
     include.lowest = 1, right = 0)
lines(density(counts, adjust= 2), col = rgb(1,0,0,0.5))
Pois <- rpois(296, lambda = mean(counts))
counts1 <- as.vector(Pois)
c = 0
counts1
counts2= c()
for (i in seq_along(counts1)) {
  if (counts1[i] >= 16) {
    counts2 = c(counts2, 16)
  } else {
    counts2 = c(counts2, counts1[i])
  }
}
Pois = counts2
hist(counts2, breaks=seq(0,16, by=1), col = rgb(0,0,1,0.5), probability= 1, add = 1,
     include.lowest = 1, right = 0);
lines(density(Pois, adjust = 2), col = rgb(0,0,1,0.5))+
  legend("topleft", legend= c("observed counts", "expected counts"), lty = c(1,1), col= c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))

chisq.test(vv1,n1)

##################################
#        q3 88 intervals
#################################

counts <- as.vector(table(cut(data$location,  breaks=88, include.lowest = TRUE)))
c= c(sort(counts))
df <- data.frame(c)
f=rep("Observed" , 17)
b = df %>% count(sort(c))

n = c(3, 12, 14, 23, 18,  8,  3,  6, 0,0,0,0,0,0,0,1,0)
vv1 =  c(3, 12, 14, 23, 18,  8,  3,  6, 0,0,0,0,0,0,0,1,0)
length(n)
c=c(0:16)
b2 = data.frame(f,c,n)
b5=b2

lambda <- mean(counts)
theoriticalPoission = c()
for (i in c(0:15)) {
  theoriticalPoission = c(theoriticalPoission, dpois(i, lambda))
}
theoriticalPoission = c(theoriticalPoission,1 - sum(theoriticalPoission))


n1 = (theoriticalPoission * 88)
#c1= c(1,2,3,4,5,6,7,8,9,12,16)
c1= c(0:16)
f1=rep("Expected" , 17)
b6= data.frame(f1,c1,n1)
c = c(b2$c, c1)
Count = c(b2$f, f1)
n = c(b2$n, n1)
b2 <- data.frame(Count,c, n)


tab <- table(cut(data$location, breaks= seq(0, 230000,length.out =89), include.lowest = 1));
counts1 <- as.vector(tab)
c = 0
counts1
counts= c()
for (i in seq_along(counts1)) {
  if (counts1[i] >= 16) {
    counts = c(counts, 16)
  } else {
    counts = c(counts, counts1[i])
  }
}
counts
hist(counts,breaks=seq(0,16, by=1), col = rgb(1,0,0,0.5),
     probability = 1,
     main =  "Histogram Expected Vs.Observed Counts for 88 Intervals",
     xlab = "Palindrome Count Within 88 Sub Intervals",
     include.lowest = 1, right = 0)
lines(density(counts, adjust= 2), col = rgb(1,0,0,0.5))
Pois <- rpois(296, lambda = mean(counts))
counts1 <- as.vector(Pois)
c = 0
counts1
counts2= c()
for (i in seq_along(counts1)) {
  if (counts1[i] >= 16) {
    counts2 = c(counts2, 16)
  } else {
    counts2 = c(counts2, counts1[i])
  }
}
Pois = counts2
density(Pois)
hist(counts2, breaks=seq(0,16, by=1), col = rgb(0,0,1,0.5), probability= 1, add = 1,
     include.lowest = 1, right = 0);
lines(density(Pois, adjust = 2), col = rgb(0,0,1,0.5))+
  legend("topright", legend= c("observed counts", "expected counts"), lty = c(1,1), col= c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))


chisq.test(vv1,(n1))


##################################
#        q3 43 intervals
#################################
counts <- as.vector(table(cut(data$location,  breaks=43, include.lowest = TRUE)))
c= c(sort(counts))
c
df <- data.frame(c)
f=rep("Observed" , 17)
b = df %>% count(sort(c))
b
n = c(0,0,1, 0, 7,4,10,5,7,5,2,0,0,1,0,0,1)
vv1 = c(0,0,1, 0, 7,4,10,5,7,5,2,0,0,1,0,0,1)
c=c(0:16)
b2 = data.frame(f,c,n)
b5=b2

lambda <- mean(counts)
theoriticalPoission = c()
for (i in c(0:15)) {
  theoriticalPoission = c(theoriticalPoission, dpois(i, lambda))
}
theoriticalPoission = c(theoriticalPoission,1 - sum(theoriticalPoission))

n1 = (theoriticalPoission * 43)
#c1= c(1,2,3,4,5,6,7,8,9,12,16)
c1= c(0:16)
f1=rep("Expected" , 17)
b6= data.frame(f1,c1,n1)
c = c(b2$c, c1)
Count = c(b2$f, f1)
n = c(b2$n, n1)
b2 <- data.frame(Count,c, n)

#b2$c <- factor(b$c,levels =as.character(c(1:32)))
ggplot(b2, aes(fill=Count, y=n, x=c)) + 
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Expected Vs. Observed Counts for Interval of 43")+
  xlab("Pallindrome Count ") +
  ylab("Interval Counts")+ 
  geom_text(aes(label = round(n,1)), position = position_dodge(width=0.9)) +
  theme( legend.position = c(0,1), legend.justification=c(0,1))+
  theme(plot.title = element_text(hjust = 0.5))



tab <- table(cut(data$location, breaks= seq(0, 230000, length.out =44), include.lowest = 1));
counts1 <- as.vector(tab)
c = 0
counts1
counts= c()
for (i in seq_along(counts1)) {
  if (counts1[i] >= 16) {
    counts = c(counts, 16)
  } else {
    counts = c(counts, counts1[i])
  }
}
counts
hist(counts,breaks=seq(0,16, by=1), col = rgb(1,0,0,0.5),
     probability = 1,
     main =  "Histogram Expected Vs.Observed Counts for 43 Intervals",
     xlab = "Palindrome Count Within 43 Sub Intervals",
     include.lowest = 1, right = 0)
lines(density(counts, adjust= 2), col = rgb(1,0,0,0.5))
Pois <- rpois(296, lambda = mean(counts))
counts1 <- as.vector(Pois)
c = 0
counts1
counts2= c()
for (i in seq_along(counts1)) {
  if (counts1[i] >= 16) {
    counts2 = c(counts2, 16)
  } else {
    counts2 = c(counts2, counts1[i])
  }
}
Pois = counts2
density(Pois)
hist(counts2, breaks=seq(0,16, by=1), col = rgb(0,0,1,0.5), probability= 1, add = 1,
     include.lowest = 1, right = 0);
lines(density(Pois, adjust = 2), col = rgb(0,0,1,0.5))+
  legend("topright", legend= c("observed counts", "expected counts"), lty = c(1,1), col= c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))


chisq.test(vv1,(n1))

##################################
#        q3 73 intervals
#################################
counts <- as.vector(table(cut(data$location,  breaks=73, include.lowest = TRUE)))

c= c(sort(counts))
c
df <- data.frame(c)

f=rep("Observed" , 17)
b = df %>% count(sort(c))
b
n = c(1, 11, 7, 12, 14, 13,  6,  5, 1,1,1,0,0,0,0,0,1)
length(n)
vv1 =  c(1, 11, 7, 12, 14, 13,  6,  5, 1,1,1,0,0,0,0,0,1)
length(n)
c=c(0:16)
b2 = data.frame(f,c,n)
b5=b2

lambda <- mean(counts)
theoriticalPoission = c()
for (i in c(0:15)) {
  theoriticalPoission = c(theoriticalPoission, dpois(i, lambda))
}
theoriticalPoission = c(theoriticalPoission,1 - sum(theoriticalPoission))


n1 = (theoriticalPoission * 73)
#c1= c(1,2,3,4,5,6,7,8,9,12,16)
c1= c(0:16)
f1=rep("Expected" , 17)
b6= data.frame(f1,c1,n1)
c = c(b2$c, c1)
Count = c(b2$f, f1)
n = c(b2$n, n1)
b2 <- data.frame(Count,c, n)


tab <- table(cut(data$location, breaks= seq(0, 230000,length.out =74), include.lowest = 1));
counts1 <- as.vector(tab)
c = 0
counts1
counts= c()
for (i in seq_along(counts1)) {
  if (counts1[i] >= 16) {
    counts = c(counts, 16)
  } else {
    counts = c(counts, counts1[i])
  }
}
counts
hist(counts,breaks=seq(0,16, by=1), col = rgb(1,0,0,0.5),
     probability = 1,
     main =  "Histogram Expected Vs.Observed Counts for 73 Intervals",
     xlab = "Palindrome Count Within 73 Sub Intervals",
     include.lowest = 1, right = 0)
lines(density(counts, adjust= 2), col = rgb(1,0,0,0.5))
Pois <- rpois(296, lambda = mean(counts))
counts1 <- as.vector(Pois)
c = 0
counts1
counts2= c()
for (i in seq_along(counts1)) {
  if (counts1[i] >= 16) {
    counts2 = c(counts2, 16)
  } else {
    counts2 = c(counts2, counts1[i])
  }
}
Pois = counts2
density(Pois)
hist(counts2, breaks=seq(0,16, by=1), col = rgb(0,0,1,0.5), probability= 1, add = 1,
     include.lowest = 1, right = 0);
lines(density(Pois, adjust = 2), col = rgb(0,0,1,0.5))+
  legend("topright", legend= c("observed counts", "expected counts"), lty = c(1,1), col= c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))


chisq.test(vv1,(n1))
########################################################
#                        q4
#----------------------------------------------
# Analysis of Palindrome Clusters Within Intervals 
# in Various Regions of Equal DNA Length
########################################################
#init arrays where array index is the number of breaks k
p_val <- array(dim = c(100, 1))
interval_length <- array(dim = c(100, 1))
start_interval <- array(dim = c(100, 1))
end_interval <- array(dim = c(100, 1))
lambda_hat <- array(dim = c(100, 1))
max_count <- array(dim = c(100, 1))

for (k in 20 : 90){
  tab <- table(cut(data$location, breaks=seq(0, N, length.out = k + 1), 
                   include.lowest=TRUE))
  
  tab <- as.vector(tab)
  lambda_hat[k,] <-sum(tab) / k
  max_count[k,] <- max(tab)
  tmp <- 0
  interval_length[k,] <- N / k
  start_interval[k,] <- (N / k) * which(tab == max(tab))[1] - (N / k)
  end_interval[k,] <- (N / k) * which(tab == max(tab))[1]
  
  for (i in 0:(max(tab) - 1)){
    tmp <- tmp + ( (lambda_hat[k] ** i) * exp( -lambda_hat[k] ) / 
                         factorial(i) )
  }
  
  p_val[k,] <- 1 - tmp ** k
  
}

table <- data.frame(lambda_hat,interval_length, p_val, max_count, 
                    start_interval, end_interval)


########################################################
#                 Advanced Analysis
#-------------------------------------------------------
# Simulating differences in theoritical data and using
# absolute difference in counts of 2 intervals to compare
# the distribution with our observed data's test statistic
########################################################
install.packages("sjPlot")
library(sjPlot)
data <- read.csv("hcmv.txt")

n = 296
N = 229354

countsActual <- as.vector(table(cut(data$location,  breaks=2, include.lowest = TRUE)))
countsActual

s = chisq.test(x=c(countsActual[1], countsActual[2]),p= c(0.5, 0.5))$stat
chisq.test(x=c(countsActual[1], countsActual[2]),p= c(0.5, 0.5))

actDiff = (countsActual[1] - countsActual[2])
actDiff
actDiffAbs = abs(actDiff)
simDiffs <- c()
ss <- c()

for (x in 1:10000) {
  ranLoc <- sample.int(N, size=n, replace=FALSE) # locations uniformly randomly generated
  ranLoc = sort(ranLoc)
  
  simCounts <- as.vector(table(cut(ranLoc,  breaks=2, include.lowest = TRUE)))
  s1 = chisq.test(x=c(simCounts[1], simCounts[2]),p= c(0.5, 0.5))$stat
  
  simDiff = (abs(simCounts[1] - simCounts[2]))
  simDiffs = c(simDiffs, simDiff)
  ss = c(ss, s1)
}
mean(ss)
simDiffsAbs = abs(simDiffs)
boolsGTorLTactual = simDiffsAbs > actDiffAbs
pvalue =  length(boolsGTorLTactual[boolsGTorLTactual== TRUE]) / 10000
pvalue

# Graph shown on report (most informative)
hist(simDiffs, xlab = "Count Difference Between First and Second Interval \n(1st interval count - 2nd interval count)", main = "Histogram of Count Differences Between First and Second Interval \nOver 50,000 Simulations For The Theoritical Distribution",xlim=c(-70,70),seq(-90,90,by =2))
abline(v = actDiff,col="red")
legend("topright", legend=c("Actual Data Count Difference Between First and Second Interval"),
       col=c("red"),lty=1:2, cex=0.65)

# Extra graphs (in Appendix for these next 2 graphs below)
hist(ss, xlab = "X-Squared Statistic ", main = "Histogram of X-Squared Statistic Over 50,000 Simulations For The Theoritical Distribution", probability  = 1)
abline(v = s,col="red")
legend("topright", legend=c("Actual Data X-Squared Statistic"),
       col=c("red"),lty=1:2, cex=0.8)

dist_chisq(chi2 = s , deg.f = 1, p = chisq.test(x=c(countsActual[1], countsActual[2]),p= c(0.5, 0.5))$p.value, geom.colors = NULL, geom.alpha = 0.7)

hist(simDiffsAbs, xlab = "Count Difference Between First and Second Interval \n(1st interval count - 2nd interval count)", main = "Histogram of Count Differences Between First and Second Interval \nOver 50,000 Simulations For The Theoritical Distribution",xlim=c(-70,70),seq(-90,90,by =2))
abline(v = actDiffAbs,col="red")
legend("topright", legend=c("Actual Data Count Difference Between First and Second Interval"),
       col=c("red"),lty=1:2, cex=0.65)

