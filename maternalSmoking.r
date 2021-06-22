#########################################################
#--------------- MATH 189: Case study 1 ----------------#
# Names: Matin Ghaffari, James Lu                       #
# PID  : A16617005, A16687580                           #                                                      #
#########################################################

# Function used to ensure proper directory and workspace
wd <- function() {
        getSrcDirectory(wd)
}

# Function used to get the mode
getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Skew function, using mean and SD to and getting Z stat
skew = function(X) {
        Xbar = mean(X); S = sd(X)
        Z = (X - Xbar)/S
        mean(Z^3)
}

# Kurtosis function
kurt = function(X) {
        Xbar = mean(X); S = sd(X)
        Z = (X - Xbar)/S
        mean(Z^4)
}

# Read in data
data <- read.csv(paste(wd(),"babies.txt", sep = '/'), header=TRUE)
lbw_threshold = 88.1849

####################################################################################
# 1. Numerical Analysis, summary statistics and other important data measurements  #
####################################################################################

#non smoking entries, excluding possible errors where smoke is not a binary
#indicator (0 or 1)
non_smoker <- data[data$smoke == 0,]

#smoking entries, excluding possible errors where smoke is not a binary
#indicator (0 or 1)
smoker <- data[data$smoke == 1,]

#summary statistics of non smoker data
non_smoker_summary = summary(non_smoker$bwt.oz..)
print(non_smoker_summary)

#number of entries in non smoker data
non_smoker_count = length(non_smoker$bwt.oz..)

#mode of non smoker data
non_smoker_mode = getmode(non_smoker$bwt.oz..)

#mean of non smoker data
non_smoker_mean = mean(non_smoker$bwt.oz..)

#standard deviation of non smoker data
non_smoker_sd = sd(non_smoker$bwt.oz..)

#skew of non smoker data
non_smoker_skew = skew(non_smoker$bwt.oz..)

#kurtosis of non smoker data
non_smoker_kurtosis = kurt(non_smoker$bwt.oz..)

#incidence rate of lbw babies
non_smoker_incidence = (length(non_smoker[non_smoker$bwt.oz.. < 
                        lbw_threshold, 1]) / dim(non_smoker)[1]) * 100
non_smoker_incidence_minus = (length(non_smoker[non_smoker$bwt.oz.. < 
                        lbw_threshold - 2, 1]) / dim(non_smoker)[1]) * 100
non_smoker_incidence_plus = (length(non_smoker[non_smoker$bwt.oz.. < 
                        lbw_threshold + 2, 1]) / dim(non_smoker)[1]) * 100

#summary statistics of non smoker data
smoker_summary = summary(smoker$bwt.oz..)
print(smoker_summary)

#number of entries in non smoker data
smoker_count = length(smoker$bwt.oz..)

#mode of non smoker data
smoker_mode = getmode(smoker$bwt.oz..)

#mean of non smoker data
smoker_mean = mean(smoker$bwt.oz..)

#standard deviation of non smoker data
smoker_sd = sd(smoker$bwt.oz..)

#skew of non smoker data
smoker_skew = skew(smoker$bwt.oz..)

#kurtosis of non smoker data
smoker_kurtosis = kurt(smoker$bwt.oz..)

#incidence rate of lbw babies
smoker_incidence = (length(smoker[smoker$bwt.oz.. < lbw_threshold, 1]) / 
                          dim(smoker)[1]) * 100
smoker_incidence_minus = (length(smoker[smoker$bwt.oz.. < 
                         lbw_threshold - 2, 1]) / dim(smoker)[1]) * 100
smoker_incidence_plus = (length(smoker[smoker$bwt.oz.. < 
                        lbw_threshold + 2, 1]) / dim(smoker)[1]) * 100


########################################################################################
# 2. Graphical Analysis -> Overlayed Histograms, Boxplots, and Quantile-Quantile Plots #
########################################################################################

#------- Overlayed Histograms with their normal respective density curcve ---------

hist(non_smoker[, 1], 
     main = 'Density of Birth Weight from Non-Smoking and Smoking Mothers',
     xlab = "Birth Weight (oz.)",
     xlim = c(50, 200),
     col = rgb(1,0,0,0.5), freq = F)
hist(smoker[, 1], col = rgb(0,0,1,0.5), add=T, freq = F) #overlayed histogram
legend("topright", c("Non-Smoking", "Smoking"), 
       col=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), lwd=5)

#normal curve line over histogram
curve(dnorm(x, mean = non_smoker_mean, sd = non_smoker_sd), 
      50, 200, col= rgb(1,0,0,0.5), lwd=2, add=T)
curve(dnorm(x, mean = smoker_mean, sd = smoker_sd), 50, 200, 
      col=rgb(0,0,1,0.5), lwd=2, add=T)

#------- Box Plot ---------

boxplot(non_smoker[, 1], smoker[, 1],
        main = "Boxplot of Birth Weight from Non-Smoking and Smoking Mothers",
        names = c("Non-Smoking", "Smoking"),
        xlab = "Birth Weight (oz.)",
        ylim = c(50, 180),
        horizontal = TRUE)

#------- QQ Plots ---------

# q-q plot of just smoker sample
qqnorm(smoker$bwt, ylim=c(40, 180),
       main = "Quantile-Quantile Plot of Birth Weight of Smoker Mothers",
       ylab = "Birth Weight (oz.)") # q-q plot against normal distribution
qqline(smoker$bwt, col = rgb(1,0,0,1))

# q-q plot of just non-smoker sample
qqnorm(non_smoker$bwt, ylim=c(40, 180),
       main = "Quantile-Quantile Plot of Birth Weight of Non-Smoker Mothers",
       ylab = "Birth Weight (oz.)") # q-q plot against normal distribution
qqline(non_smoker$bwt, col = rgb(1,0,0,1))

# q-q plot of smoker and non-smoker samples
qqplot(non_smoker$bwt, smoker$bwt, xlim=c(40, 180), 
       ylim=c(40, 180), 
       xlab = "Birth Weight (Non-Smoking)", 
       ylab = "Birth Weight (Smoking)",
       main = "Quantile-Quantile Plot of Birth Weight Between Non-Smoking
       and Smoking Mothers") 
abline(c(0,1), col = rgb(1,0,0,1)) # reference line


#########################################################################################
# 3. Incidence and Its Reliability -> Barplots of adjusted thresholds and porportions   #
#########################################################################################
bp <- barplot(c(non_smoker_incidence, smoker_incidence),
        main = "Proportion of Low-birth-weight Babies Using 88 oz. Threshold (Claim)",
        xlab = "Smoking Status of Pregnant Women",
        ylab = "Percentage of Birthweights Less Than 88 oz. Threshold (%)",
        names.arg = c("Non-Smoker During Pregnancy", "Smoker During Pregnancy"),
        col = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))
text(bp, 0, round(c(non_smoker_incidence, smoker_incidence),3),cex=1,pos=3)
bp

bp <- barplot(c(non_smoker_incidence_minus, smoker_incidence_minus),
        main = "Proportion of Low-birth-weight Babies Using 86 oz. Threshold (Claim-2)",
        xlab = "Smoking Status of Pregnant Women",
        ylab = "Percentage of Birthweights Less Than 86 oz. Threshold (%)",
        names.arg = c("Non-Smoker During Pregnancy", "Smoker During Pregnancy"),
        col = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))
text(bp, 0, round(c(non_smoker_incidence_minus, smoker_incidence_minus),3),cex=1,pos=3)
bp

bp <- barplot(c(non_smoker_incidence_plus, smoker_incidence_plus),
        main = "Proportion of Low-birth-weight Babies Using 90 oz. Threshold (Claim+2)",
        xlab = "Smoking Status of Pregnant Women",
        ylab = "Percentage of Birthweights Less than 90 oz. Threshold (%)",
        names.arg = c("Non-Smoker During Pregnancy", "Smoker During Pregnancy"),
        col = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))
text(bp, 0, round(c(non_smoker_incidence_plus, smoker_incidence_plus),3),cex=1,pos=3)
bp

############################################################################
# Advanced Analysis -> two sample t-test -> h0: mu = 0  vs h1: mu > 0   #
############################################################################
t.test(non_smoker$bwt, smoker$bwt, mu = 0, alternative = "greater")


#------------------------Side work (not used in report)-----------------------
#-----------------------------------------------------------------------------
# Simulate Smoker skew and kurtosis to see the variability in our skew and kurt measures
mu = smoker_mean; sigma = smoker_sd
kurt_smoker=0
skew_smoker=0
for (i in c(0:1000)) {
        X = rnorm(smoker_count, mean = mu, sd = sigma)
        Z = (X - mean(X)) / sd(X)
        kurt_smoker= kurt_smoker+ Z
        skew_smoker= skew_smoker+ Z
}
kurt(kurt_smoker/1000)
skew(skew_smoker/1000)

# Simulate Non-Smoker skew and Kurt to see their variability
mu = non_smoker_mean; sigma = non_smoker_sd
kurt_nonSmoker=0
skew_nonSmoker=0
for (i in 1000) {
        X = rnorm(non_smoker_count, mean = mu, sd = sigma)
        Z = (X - mean(X)) / sd(X)
        kurt_nonSmoker = kurt_nonSmoker + Z
        skew_nonSmoker = kurt_nonSmoker + Z
}
kurt(kurt_nonSmoker/1000)
skew(skew_nonSmoker/1000)
#-------------------------------------------------------------------------------


