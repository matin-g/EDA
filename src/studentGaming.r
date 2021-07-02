#########################################################
#--------------- MATH 189: Case study 2 ----------------#
# Names: Matin Ghaffari, James Lu                       #
# PID  : A16617005, A16687580                           #
#########################################################

#library to calculate kurt and skewness
library(moments)

#import data into r
data <- read.csv("videodata.txt", header = TRUE)
data2 <- read.csv("videoMultiple.txt", header = TRUE)

#dataframes for students who did and did not play video games
#the week prior to the survey
played_games <- data[data$time > 0 & data$time < 99,]
didnt_play <- data[data$time == 0,]

#sample size n
n = dim(played_games)[1] + dim(didnt_play)[1]
#correction factor
correction_factor = (314 - n) / (314 - 1)

#q1

#find prop of students who played games
prop = dim(played_games)[1] / n

#margin of error for CI
me = qnorm(.975) * sqrt((prop * (1-prop)) / n)
#population corrected margin of error
pop_corr_me = qnorm(.975) * sqrt((prop * (1-prop)) / (n-1)) * 
  sqrt(correction_factor)

#calculate ci's
ci = c(prop - me, prop + me)
pop_corr_ci = c(prop - pop_corr_me, prop + pop_corr_me)

#number of simulations for bootstrapping
num_sims = 1000

#init dataframe for the simulation distribution
sim_dist <- c(length(num_sims))
#init population for simulation
pop <- rep(data$time, length.out = 314)

#run boostrapping procedure
for (i in 1:num_sims){
  sim = sample(pop, n, replace = FALSE)
  sim_prop = length(sim[sim > 0]) / n
  
  sim_dist[i] = sim_prop
}

#generate histogram for the prop of students who played video games the week 
#prior to the survey
hist(sim_dist, breaks = 15, 
     xlab = "Proportion of Students Who Played a Video Game During Exam Week", 
     main = "Distribution of 1000 Bootstrapped Proportions")
abline(v = mean(sim_dist), col = 'red', lwd = 2)

#calculate the mean and CI's of the new bootstrap sample
bs_mean_q1 = mean(sim_dist)
bs_me_q1 = qnorm(.975) * sqrt((bs_mean_q1 * (1-bs_mean_q1)) / n)
bs_pop_corr_me_q1 = qnorm(.975) * sqrt((bs_mean_q1 * (1-bs_mean_q1)) / (n-1)) * 
  sqrt(correction_factor)
bs_ci_q1 = c(bs_mean_q1 - bs_me_q1, bs_mean_q1 + bs_me_q1)
bs_ci_corr_q1 = c(bs_mean_q1 - bs_pop_corr_me_q1, bs_mean_q1 + bs_pop_corr_me_q1)

#q2
#clean time variable from 99s
cleaned_time = data[data$time >= 0 & data$time < 99 & data$freq < 99,]
#adjust plot dimensions
par(mar=c(5, 6, 4, 4))
#generate boxplot based on # hrs played and freq of play
boxplot(cleaned_time$time~cleaned_time$freq, data, 
        xlab = "Frequency of Play", ylab = "Number of Hours Played the 
        Week Before Survey",
        main = "Time Playing Games Based on Frequency",
        names = c("Daily", "Weekly", "Monthly", "Semesterly"))
#find the avg hrs played and prop of players who play when busy 
#for each category in freq of play
for (i in 1:4){
  data_freq = data[data$freq == i,]
  no_play_if_busy_prop = dim(data_freq[data_freq$busy == 0,])[1] / dim(data_freq)[1]
  play_if_busy_prop = dim(data_freq[data_freq$busy == 1,])[1] / dim(data_freq)[1]
  
  avg_hrs <- mean(data_freq$time)
  
  print(c(i, avg_hrs))
  print(c(i, no_play_if_busy_prop, play_if_busy_prop))
}

#q3
#find average time played
avg_time_played = mean(data$time)

#create histogram for time played
hist(data$time, breaks = 25, main = "Frequency of Time Played", 
     xlab = "Time Played the Week Before Survey (Hours)")

#computations for confidence intervals
time_played_me = qnorm(.975) * (sd(data$time) / sqrt(n))
time_played_corr = time_played_me * sqrt(correction_factor)
time_played_ci = c(avg_time_played - time_played_me, 
                   avg_time_played + time_played_me)
time_played_ci_corr = c(avg_time_played - time_played_corr, 
                   avg_time_played + time_played_corr)

#create dataframes for simulations
bs_means <- c(length(num_sims))
norm_kurt <- c(length(num_sims))
norm_skew <- c(length(num_sims))

#bootstrap for sample mean and normal skew / kurtosis
pop <- rep(data$time, length.out = 314)
for (i in 1:num_sims){
  sim = sample(pop, n, replace = FALSE)
 
  sim_mean = mean(sim)
  
  norm_kurt[i] = kurtosis(rnorm(n))
  norm_skew[i] = skewness(rnorm(n))
  bs_means[i] = sim_mean
}

#compute kurtosis / skew for bootstrap mean and normal distribution
bs_kurt = kurtosis(bs_means)
bs_skew = skewness(bs_means)
kurt_mean = mean(norm_kurt)
skew_mean = mean(norm_skew)

#create histograms for each distribution
hist(norm_kurt, main = "Distribution of 1000 Bootstrapped Time Played Kurtosis", 
     xlab = "Kurtosis")

hist(norm_skew, main = "Distribution of 1000 Bootstrapped Time Played Skewness", 
     xlab = "Skewness")

hist(bs_means, main = "Distribution of 1000 Bootstrapped Time Played Means", 
     xlab = "Time Played the Week Before Exam (Hours)")

#calculations for CI's for simulated means and kurtosis / skewness
bs_me = qnorm(.975) * (sd(bs_means) / sqrt(n))
bs_mean = mean(bs_means)
bs_me_corr = qnorm(.975) * (sd(bs_means) / sqrt(n)) * sqrt(correction_factor)
bs_ci = c(bs_mean - bs_me, bs_mean + bs_me)
bs_ci_corr = c(bs_mean - bs_me_corr, bs_mean + bs_me_corr)

#4
#compute proportion of sample that like games
like_games = data[data$like == 2 | data$like == 3,]
like_games_prop = dim(like_games)[1] / n

# sim_like_prop <- c(length(num_sims))
# 
# pop <- rep(data$like, length.out = 314)
# for (i in 1:num_sims){
#   sim = sample(pop, n, replace = FALSE)
#   sim_prop = length(sim[sim == 2 | sim == 3]) / n
#   
#   sim_like_prop[i] = sim_prop
# }

#sim_like_mean = mean(sim_like_prop)

#compute proportion of students who dislike games / didnt answer
dislike_games = data[data$like == 4 | data$like == 5,]
dislike_games_prop = dim(dislike_games)[1] / n

#-----------------------------  q5  ---------------------------------#
# Male and Female grouped bar plot
#-----------------------------------------------------------------------
# This method groups by like if the are either a liking
# category or a disliking category to observe the count of likes and
# dislikes for male and female

library(ggplot2)

played_games_girl <- data[data$sex == 0,]
played_games_girl = played_games_girl[, 2]

played_games_boy <- data[data$sex == 1,]
played_games_boy = played_games_boy[, 2]


specie <- c( rep("Like" , 2) , rep("Dislike" , 2) )
Gender <- rep(c("male" , "female") , 2)
value <- c(length(played_games_boy[played_games_boy == 2]) + length(played_games_boy[played_games_boy == 3]),
             length(played_games_girl[played_games_girl == 2]) + length(played_games_girl[played_games_girl == 3]),
           length(played_games_boy[played_games_boy == 4]) + length(played_games_boy[played_games_boy == 5]),
           length(played_games_girl[played_games_girl == 4]) + length(played_games_girl[played_games_girl == 5]))
temp_data <- data.frame(specie,Gender,value)

ggplot(temp_data, aes(fill=Gender, y=value, x=specie)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("How Many Male Vs.Female Students Like to Play Video Games") +
  xlab("How Much Students Like to play") +
  ylab("Number of Students")+ geom_text(aes(label = value), position = position_dodge(width=0.9))+
  theme(legend.position = c(0,1),legend.justification=c(0,1))+
  theme(plot.title = element_text(hjust = 0.5))

#-------------------------------- q5  -----------------------------------------
# Work vs Non-work grouped bar plot
#-----------------------------------------------------------------------
# This method groups by like if the are either a liking
# category or a disliking category to observe the count of likes and
# dislikes for those who work and dont work

played_games_workd <- data[data$work > 0,]
played_games_work = played_games_workd[, 2]

played_games_noWorkd <- data[data$work == 0,]
played_games_noWork = played_games_noWorkd[, 2]

specie <- c( rep("Like" , 2) , rep("Dislike" , 2) )
Work_Status <- rep(c("Work For Pay" , "Doesn't Work For Pay") , 2)
value <- c(length(played_games_work[played_games_work == 2]) + length(played_games_work[played_games_work == 3]),
           length(played_games_noWork[played_games_noWork == 2]) + length(played_games_noWork[played_games_noWork == 3]),
           length(played_games_work[played_games_work == 4]) + length(played_games_work[played_games_work == 5]),
           length(played_games_noWork[played_games_noWork == 4]) + length(played_games_noWork[played_games_noWork == 5]))
temp_data <- data.frame(specie,Gender,value)

temp_data <- data.frame(specie,Work_Status,value)

ggplot(temp_data, aes(fill=Work_Status, y=value, x=specie)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("How Many Working Vs.Non-Working Students Like to Play Video Games") +
  xlab("How Much Students Like to play") +
  ylab("Number of Students")+ geom_text(aes(label = value), position = position_dodge(width=0.9))+
  theme(legend.position = c(0,1),legend.justification=c(0,1))+
  theme(plot.title = element_text(hjust = 0.5))

#-------------------------------- q5  -----------------------------------------
# Own PC vs Dont Own PC grouped bar plot
#-----------------------------------------------------------------------
# This method groups by like if the are either a liking
# category or a disliking category to observe the count of likes and
# dislikes for those who own a PC and those who dont

played_games_pcd <- data[data$own == 1,]
played_games_pc = played_games_pcd[, 2]

played_games_noPcd <- data[data$own == 0,]
played_games_noPc = played_games_noPcd[, 2]

specie <- c( rep("Like" , 2) , rep("Dislike" , 2) )
Work_Status <- rep(c("Own a PC" , "Doesn't Own a PC") , 2)
value <- c(length(played_games_pc[played_games_pc == 2]) + length(played_games_pc[played_games_pc == 3]),
           length(played_games_noPc[played_games_noPc == 2]) + length(played_games_noPc[played_games_noPc == 3]),
           length(played_games_pc[played_games_pc == 4]) + length(played_games_pc[played_games_pc == 5]),
           length(played_games_noPc[played_games_noPc == 4]) + length(played_games_noPc[played_games_noPc == 5]))
temp_data <- data.frame(specie,Work_Status,value)

gg <- ggplot(temp_data, aes(fill=Work_Status, y=value, x=specie)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("How Many Own a PC Vs. Don't Own a PC in The Students who Like to Play Video Games") +
  xlab("How Much Students Like to play") +
  ylab("Number of Students")+ geom_text(aes(label = value), position = position_dodge(width=0.9)) +
  theme(legend.position = c(0,1),legend.justification=c(0,1))+
  theme(plot.title = element_text(hjust = 0.5))
gg

#-------------------------------- q5  -----------------------------------------
# hours of Work vs like and disklike area chart
#-----------------------------------------------------------------------
# This method groups by like if the are either a liking
# category or a disliking category and we generate two line graphs in order
# to show how these categories differ as they work more or less hours

 library(dplyr)
 library("ggrepel")
 library(viridis)
 data3= data[data$work < 99 & data$like < 99 & data$like !=1,]

 ll <- data3[data3$like < 4,]
 dl <- data3[data3$like >= 4,]

 by_vs_am1 <- ll %>% group_by(like, work)
 by_vs1 <- by_vs_am1 %>% summarise(n = n())
 by_vs_am2 <- dl %>% group_by(like, work)
 by_vs2 <- by_vs_am2 %>% summarise(n =n())

 library(data.table)
 by_vs1=as.data.table(by_vs1)[, .SD[which.max(n)], by=work]
 by_vs2=as.data.table(by_vs2)[, .SD[which.max(n)], by=work]
 
 b = ggplot(NULL, aes(x = work,y =n))+
   geom_area(data = by_vs1, aes(fill = "Like"), alpha =0.5) +
   geom_area(data = by_vs2, aes(fill = "Dislike"), alpha =0.5)
 

 b = b + ggtitle("Number of Hours Working Vs. Like to Play Video Games")+
   theme(plot.title = element_text(hjust = 0.5))+
   labs(fill="Liking:", x = "Number of weekly hours Worked", y="Number of Students in Like to Play Categories with Given Number of Work Hours")+
  theme(legend.position = "top")

b

#---------------------  q6  --------------------------
# Grade Distribution pie chart --> observed data
#---------------------------------------------------------------
# Accessing the count of each category to calculate the proportion
# of grades for each letter grade with the observed values

Adf <- data[data$grade == 4,]
Bdf<-data[data$grade == 3,]
Cdf<-data[data$grade == 2,]
Ddf<-data[data$grade == 1,]
Fdf<-data[data$grade == 0,]

aProp = nrow(Adf)/nrow(data)
bProp = nrow(Bdf)/nrow(data)
cProp = nrow(Cdf)/nrow(data)
dProp = nrow(Ddf)+nrow(Fdf)/nrow(data)
aProp
bProp
cProp
dProp
library("viridis")
library(plotrix)
library(wesanderson)
library(scales)
slices <- c(aProp,bProp,cProp)
lbls <- c("A =", "B =", "          C = ")
pct <- round(slices/sum(slices)*100,2)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie3D(slices,labels=lbls,explode=0.005,theta=1.3,radius =1,
      main="Expected Grade Distribution (Observed)", labelpos=c(1.070213, 3.935622, 6.117001), col = wes_palette("GrandBudapest2", n = 3))

#---------------------  q6  --------------------------------------
# Grade Distribution pie chart --> adjusted with target data
#---------------------------------------------------------------
# Accessing the count of each category to calculate the proportion
# of grades for each letter grade with the

aProp = nrow(Adf)/(nrow(data)+4)
bProp = nrow(Bdf)/(nrow(data)+4)
cProp = nrow(Cdf)/(nrow(data)+4)
dProp = (nrow(Ddf)+nrow(Fdf)+4)/(nrow(data)+4)
aProp
bProp
cProp
dProp
slices <- c(aProp,bProp,cProp,dProp)
lbls <- c("A =", "B =", "        C = ", "                         D or Lower = ")
pct <- round(slices/sum(slices)*100,2)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie3D(slices,labels=lbls,explode=0.005,theta=1.3,radius =1,
      main="Grade Distribution With Non-Respondents (Adjusted)", col = wes_palette("GrandBudapest2", n = 4))


#--------------- Advanced Analysis ------------------------------
# Ruunning a chi - squared goodnwss of fit test to see if the distributions
# that we observe match with the expected

chisq.test(x = c(nrow(Adf), nrow(Bdf), nrow(Cdf), (nrow(Ddf)+nrow(Fdf))), p = c(0.2,0.3,0.4,0.1))

