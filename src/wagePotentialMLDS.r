library(dplyr)
library(ggplot2)

#########################################################
#--------------- MATH 189: Final Proj ----------------#
# Names: Matin Ghaffari, James Lu                       #
# PID  : A16617005, A16687580                           #
#########################################################

cf = read.csv("cleanData.csv")
data = read.csv('cleanData.csv')

################################################################################
#                 DATA CLEANING                                                #
################################################################################

kaggle_survey_2020_responses <- read.csv("kaggle_survey_2020_responses.csv", stringsAsFactors=0, header = 1)
df = kaggle_survey_2020_responses
df1 = kaggle_survey_2020_responses

df = subset(df, select = c(Q1, Q2, Q3, Q4, Q5, Q6, Q7_Part_1, Q7_Part_2, Q7_Part_3, Q15, Q22, Q24, Q25))
df <- df[as.character(df$Q1)!= "" ,]
df <- df[as.character(df$Q2)!= "" ,]
df <- df[as.character(df$Q2)!= "Nonbinary" ,]
df <- df[as.character(df$Q2)!= "Prefer not to say" ,]
df <- df[as.character(df$Q2)!= "Prefer to self-describe" ,]
df <- df[as.character(df$Q3)!= "Other" ,]
df <- df[as.character(df$Q3)!= "" ,]
df <- df[as.character(df$Q4)!= "" ,]
df <- df[as.character(df$Q4)!= "I prefer not to answer" ,]
df <- df[as.character(df$Q24)!= "" ,]
df = df[-1,]
df = df[]

ages = df$Q1
gender = df$Q2
country = df$Q3
deg = df$Q4
status = df$Q5
codeExp = df$Q6
python = df$Q7_Part_1
r = df$Q7_Part_2
sql = df$Q7_Part_3
mlYrsExp = df$Q15

################################################################################
#                 P1 --> Graphical methods Appendix section graphs          
################################################################################
library(dplyr)
library(ggplot2)

ageGroups = unique(df[c("Q1")])
ageGroups
category1 = unique(df[c("Q24")])
category1 = c( "$0-999", "1,000-1,999", "2,000-2,999", "3,000-3,999", "4,000-4,999", "5,000-7,499" ,  "7,500-9,999",  "10,000-14,999",
               "15,000-19,999","20,000-24,999","25,000-29,999", "30,000-39,999", "40,000-49,999", "50,000-59,999", "60,000-69,999" ,
               "70,000-79,999" , "80,000-89,999" ,"90,000-99,999" ,"100,000-124,999",  "125,000-149,999", "150,000-199,999",
               "200,000-249,999",  "250,000-299,999","300,000-500,000",  "> $500,000")
bDf <- df %>% group_by(Q1)
aDf <- bDf %>% summarise(Counts = n()) 

ggplot(aDf, aes(y=Counts, x=Q1)) +
  geom_bar(stat="identity", fill = rgb(.3,.7,.9,0.6), position = "stack") +
  ggtitle("Distribution of the Number of Surveys For Various Age Groups") +
  xlab("Age Groupings") +
  ylab("Number of Responses")+ geom_text(aes(label = Counts))+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data=cf, aes(Q1))+ geom_density()

gender = unique(df[c("Q2")])
gender
gDf <- df %>% group_by(Q2)
gDf <- gDf %>% summarise(Counts = n())

ggplot(gDf, aes(fill=c( rgb(.1,.5,.8,0.6), rgb(.7,0,.2,0.6)), y=Counts, x=Q2)) +
  geom_bar(stat="identity" , fill=c( rgb(.1,.5,.8,0.6), rgb(.7,0,.2,0.6)))+
  ggtitle("Distribution of the Number of Surveys For Male vs. Female") +
  xlab("Gender") +
  ylab("Number of Responses")+ geom_text(aes(label = Counts))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = c(0,1))

country = unique(df[c("Q3")])
country
cDf <- df %>% group_by(Q3)
cDf <- cDf %>% summarise(Counts = n())

cDf[[52,1]] = "UK and Northern Ireland"
cDf[[18,1]] = "Iran"

ggplot(cDf, aes(y=Counts , x=Q3)) +
  geom_bar(stat="identity", fill = rgb(.5,.3,.7,0.6))+coord_flip()+
  ggtitle("Distribution of the Number of Surveys For Various Countries") +
  xlab("Country") +
  ylab("Number of Responses")+ geom_text(aes(label = (Counts)), size=2.5)+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = c(0,1))

edu = unique(df[c("Q4")])
edu
eDf <- df %>% group_by(Q4)
eDf <- eDf %>% summarise(Counts = n())

eDf[[6,1]] = "College/University study w/o Bachelors"

ggplot(eDf, aes(y=Counts , x=Q4)) +
  geom_bar(stat="identity",fill = rgb(.3,.7,.5,0.6))+coord_flip()+
  ggtitle("Distribution of the Number of Surveys For Various Education Levels") +
  xlab("Education Level") +
  ylab("Number of Responses")+ geom_text(aes(label = (Counts)),size=2.5)+
  theme(plot.title = element_text(hjust = 0.5))

prog = unique(df[c("Q6")])
prog
pDf <- df %>% group_by(Q6)
pDf <- pDf %>% summarise(Counts = n())

pDf[[7,1]] = "No Experience"

ggplot(pDf, aes(y=Counts, x=Q6)) +
  geom_bar(stat="identity", fill = rgb(1,.8,.2,0.6)) +
  ggtitle("Distribution of the Number of Surveys For Various Amounts of Programming Experience") +
  xlab("Years Programming Experience Groupings") +
  ylab("Number of Responses")+ geom_text(aes(label = Counts))+
  theme(plot.title = element_text(hjust = 0.7))

sDf <- df %>% group_by(Q24)
sDf <- sDf %>% summarise(Counts = n())

ggplot(sDf, aes(y=Counts, x=Q24)) +
  geom_bar(stat="identity", fill = rgb(.3,.4,.5,0.6)) +coord_flip()+
  ggtitle("Distribution of the Number of Surveys For Various Amounts of Salary") +
  xlab("Yearly Compensation ($USD)") +
  ylab("Number of Responses")+ geom_text(aes(label = Counts))+
  theme(plot.title = element_text(hjust = 0.7))

bDf <- df %>% group_by(Q15)
aDf <- bDf %>% summarise(Counts = n())

ggplot(aDf, aes(y=Counts, x=Q15)) +
  geom_bar(stat="identity", fill = rgb(.3,.7,.9,0.6)) +coord_flip()+
  ggtitle("Distribution of the Number of Surveys For Various Age Groups") +
  xlab("Age Groupings") +
  ylab("Number of Responses")+ geom_text(aes(label = Counts))+
  theme(plot.title = element_text(hjust = 0.5))

################################################################################
#                 P1 --> Graphical methods Plots in report        
################################################################################
gender <- df %>% group_by(Q2) %>% summarise(count = n()) %>% arrange(desc(count))
age_gender<-df %>% group_by(Q24,Q2) %>% summarise(count=n())

ggplot(age_gender, aes(fill=Q2, y=count, x=Q24)) +
  geom_bar(position="dodge", stat="identity") +coord_flip()+
  ggtitle("Distribution of Genders Vs. Yearly Compensation") +
  ylab("Frequency (Number of Responses)") +
  xlab("Yearly Compensation ($USD)")+ geom_text(aes(label = count), position = position_dodge(width=0.9))+
  theme(legend.position = c(0,1),legend.justification=c(-5.5,1))+
  theme(plot.title = element_text(hjust = 0.5))+ guides(fill=guide_legend(title="Gender"))


gender <- df %>% group_by(Q1) %>% summarise(count = n()) %>%arrange(desc(count))
age_gender <- df %>% group_by(Q24,Q1) %>% summarise(count=n())

ggplot(age_gender, aes(fill=Q1, y=count, x=Q24)) +
  geom_bar(position="dodge", stat="identity") +coord_flip()+
  ggtitle("Distribution of Ages Vs. Yearly Compensation") +
  ylab("Frequency (Number of Responses)") +
  xlab("Yearly Compensation ($USD)")+# geom_text(aes(label = count), position = position_dodge(width=0.9))+
  theme(legend.position = c(0,1),legend.justification=c(-5,1))+
  theme(plot.title = element_text(hjust = 0.5))+ guides(fill=guide_legend(title="Age Groups"))

gender <- df %>% group_by(Q4) %>% summarise(count = n()) %>%arrange(desc(count))
age_gender<-df %>% group_by(Q24,Q4) %>% summarise(count=n())

ggplot(age_gender, aes(fill=Q4, y=count, x=Q24)) +
  geom_bar(position="dodge", stat="identity") +coord_flip()+
  ggtitle("Distribution of Education Levels Vs. Yearly Compensation") +
  ylab("Frequency (Number of Responses)") +
  xlab("Yearly Compensation ($USD)")+# geom_text(aes(label = count), position = position_dodge(width=0.9))+
  theme(legend.position = c(0,1),legend.justification=c(-.64,1))+
  theme(plot.title = element_text(hjust = 0.5))+ guides(fill=guide_legend(title="Education Level"))

gender <- df %>% group_by(Q6) %>% summarise(count = n()) %>%arrange(desc(count))
age_gender<-df %>% group_by(Q24,Q6) %>% summarise(count=n())


ggplot(age_gender, aes(fill=Q6, y=count, x=Q24)) +
  geom_bar(position="dodge", stat="identity") +coord_flip()+
  ggtitle("Distribution of Amount of Programming Expereince Vs. Yearly Compensation") +
  ylab("Frequency (Number of Responses)") +
  xlab("Yearly Compensation ($USD)")+# geom_text(aes(label = count), position = position_dodge(width=0.9))+
  theme(legend.position = c(0,1),legend.justification=c(-2.5,1))+
  theme(plot.title = element_text(hjust = 0.5))+ guides(fill=guide_legend(title="Experience (Years)"))

library(RColorBrewer)

qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

gender <- df %>% group_by(Q3) %>% summarise(count = n()) %>%arrange(desc(count))
age_gender<-df %>% group_by(Q24,Q3) %>% summarise(count=n())

ageGroups = unique(df[c("Q3")])
ggplot(age_gender, aes(fill=Q3, y=count, x=Q24)) +
  geom_bar(position="stack", stat="identity" )+scale_fill_manual(values =sample(col_vector, 65)) +coord_flip()+
  ggtitle("Distribution of Countries Vs. Yearly Compensation") +
  ylab("Frequency (Number of Responses)") +
  xlab("Yearly Compensation ($USD)")+# geom_text(aes(label = count), position = position_dodge(width=0.9))+
  theme(legend.position = c(0,1),legend.justification=c(-0.45,1))+
  theme(plot.title = element_text(hjust = 0.5))+ guides(fill=guide_legend(title="Countries"))


################################################################################
#                 P2 --> Accounting for missing data      
################################################################################
sDf <- df %>% group_by(Q24)
sDf <- sDf %>% summarise(Counts = n())
d = nrow(df)
c = sDf$Counts/(d)
c

kaggle_survey_2020_responses <- read.csv("kaggle_survey_2020_responses.csv", stringsAsFactors=0, header = 1)
df1 = kaggle_survey_2020_responses

df1 = subset(df1, select = c(Q1, Q2, Q3, Q4, Q5, Q6, Q7_Part_1, Q7_Part_2, Q7_Part_3, Q15, Q22, Q24, Q25))
df1 <- df1[as.character(df1$Q1)!= "" ,]
df1 <- df1[as.character(df1$Q2)!= "" ,]
df1 <- df1[as.character(df1$Q2)!= "Nonbinary" ,]
df1 <- df1[as.character(df1$Q2)!= "Prefer not to say" ,]
df1 <- df1[as.character(df1$Q2)!= "Prefer to self-describe" ,]
df1 <- df1[as.character(df1$Q3)!= "Other" ,]
df1 <- df1[as.character(df1$Q3)!= "" ,]
df1 <- df1[as.character(df1$Q4)!= "" ,]
df1<- df1[as.character(df1$Q4)!= "I prefer not to answer" ,]

df1 = df1[-1,]
sals = unique(df[c("Q24")])
rand_gender <- sample(sals$Q24, nrow(df1), replace=TRUE, prob=c(rep(0.04, 25)))

df1$Q24 <- ifelse(df1$Q24=="", rand_gender, df1$Q24)

sDf <- df1 %>% group_by(Q24)
sDf <- sDf %>% summarise(Counts = n())
b = 17593
a=(sDf$Counts)/(b)
a

chisq.test(x = sDf$Counts, p=c)



################################################################################
#                 Regression (rest of parts) 
################################################################################
#q3
#multi regression model
initial_model = lm(data$Q24 ~ data$Q1 + factor(data$Q2) + 
                     factor(data$Q3) + factor(data$Q4) + data$Q6)
init_sumamry = summary(initial_model)
#plot generation for our multi regression model
plot(initial_model, col=rgb(0,0,0,.3))

#q4
#multi regression excluding the countries variables
model_no_countries = lm(data$Q24 ~ data$Q1 + factor(data$Q2)
                        + factor(data$Q4) + data$Q6)

#compares the different models
model_comp = anova(initial_model, model_no_countries)

#groupby education level
gdf = group_by(data, Q4) %>% summarize(mean_age = mean(Q1),
                                       mean_code_exp = mean(Q6), wage=mean(Q24))
#age lm
a = lm(gdf$wage ~ gdf$mean_age)
p = ggplot(gdf, aes(x = mean_age, y = wage, colour = Q4)) +
  geom_point() + 
  geom_abline(slope=a$coefficients[2], intercept=a$coefficients[1], 
              colour='#FF0000') +
  labs(title="Average Ages vs Average Wage (By Education Level)")
print(summary(a))
print(p)

#code exp lm
b = lm(gdf$wage ~ gdf$mean_code_exp)
p = ggplot(gdf, aes(x = mean_code_exp, y = wage, colour = Q4)) + 
  geom_point() + 
  geom_abline(slope=b$coefficients[2], intercept=b$coefficients[1], 
              colour='#FF0000') +
  labs(title="Average Coding Experience (Years) vs Average Wage (By Education Level)")

print(summary(b))
print(p)

#advanced analysis
#convert wage to binary
binary_wage = ifelse(data$Q24 > 40000,1,0)
age_wage = data.frame(Age=data$Q1, Wage=binary_wage)
code_wage = data.frame(Code_Exp=data$Q6, Wage=binary_wage)

#age wage logit
age_wage_logit <- glm(Wage ~ Age, data = age_wage, family = "binomial")
#code wage logit
code_exp_wage_logit <- glm(Wage ~ Code_Exp, data = code_wage, family = "binomial")
print(summary(age_wage_logit))
print(summary(code_exp_wage_logit))

#plot age logit
print(ggplot(data.frame(Age=data$Q1, Wage=binary_wage), aes(x=Age, y=Wage)) +
        geom_point(alpha=.3) +
        stat_smooth(method='glm', method.args = list(family=binomial)) +
  labs(title='Age Logit Model', y='Probability'))

#plot code logit
print(ggplot(data.frame(Coding_Experience_Years=data$Q6, Wage=binary_wage),
             aes(x=Coding_Experience_Years, y=Wage)) + geom_point(alpha=.3)
      + stat_smooth(method='glm', method.args = list(family=binomial)) +
        labs(title='Coding Experience Logit Model', y='Probability'))

#predictions and observed values for age
pred_20 = predict(age_wage_logit, data.frame(Age=20), type="response")
actual_prop20 = nrow(data[data$Q1 <= 20 & data$Q24 >= 40000,]) / nrow(data)

pred_40 = predict(age_wage_logit, data.frame(Age=40), type="response")
actual_prop40 = nrow(data[data$Q1 <= 40 & data$Q24 >= 40000,]) / nrow(data)

pred_60 = predict(age_wage_logit, data.frame(Age=60), type="response")
actual_prop60 = nrow(data[data$Q1 <= 60 & data$Q24 >= 40000,]) / nrow(data)

print(pred_20)
print(actual_prop20)

print(pred_40)
print(actual_prop40)

print(pred_60)
print(actual_prop60)

#predictions and observed values for code experience
pred_2 = predict(code_exp_wage_logit, data.frame(Code_Exp=2), type="response")
actual_prop2 = nrow(data[data$Q6 <= 2 & data$Q24 >= 40000,]) / nrow(data)

pred_12 = predict(code_exp_wage_logit, data.frame(Code_Exp=12), type="response")
actual_prop12 = nrow(data[data$Q6 <= 12 & data$Q24 >= 40000,]) / nrow(data)

pred_22 = predict(code_exp_wage_logit, data.frame(Code_Exp=22), type="response")
actual_prop22 = nrow(data[data$Q6 <= 22 & data$Q24 >= 40000,]) / nrow(data)

print(pred_2)
print(actual_prop2)

print(pred_12)
print(actual_prop12)

print(pred_22)
print(actual_prop22)
