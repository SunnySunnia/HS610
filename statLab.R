##Lexical Decision##################
lexical_decision <- read.csv("C:/PSYD755/lexical_decision.csv", stringsAsFactors=FALSE)
#1
n=nrow(subset(lexical_decision,lexical_decision$Condition=="Word"))
#ifelse(lexical_decision$AOA=="--",NA,as.numeric(lexical_decision$AOA))
sumAOA=sum(as.numeric(subset(lexical_decision$AOA,lexical_decision$AOA!="--")))
sumsqAOA=sum((as.numeric(subset(lexical_decision$AOA,lexical_decision$AOA!="--")))^2)
sumRT=sum(subset(lexical_decision$Rtime,lexical_decision$Condition=="Word"))
sumsqRT=sum(subset(lexical_decision$Rtime,lexical_decision$Condition=="Word")^2)

lexical_decision$AcrossRT = ifelse(lexical_decision$Condition=="Word", as.numeric(lexical_decision$AOA) * lexical_decision$Rtime, NA)
sumproduct_AR_T=sum(lexical_decision$AcrossRT,na.rm = T)

#numerator of correlation formula:
n*sumproduct_AR_T - sumAOA * sumRT

#denominator of correlation formula:
sqrt((n*sumsqAOA-sumAOA^2)*(n*sumsqRT-sumRT^2))

install.packages("Rmpfr")
library(Rmpfr)
mpfr(sqrt((n*sumsqAOA-sumAOA^2)*(n*sumsqRT-sumRT^2)),100)
##accurate to the 0.01 :(



#####################################################################
##Speeded Reaction####
setwd("C:/PSYD755")
SpeededReaction <- read.csv("C:/PSYD755/SpeededReaction.csv", stringsAsFactors=FALSE)
mean_rt=mean(SpeededReaction$Reaction.time..ms.)
g = ggplot(SpeededReaction, aes(x=Trial, y = Reaction.time..ms.))
g+geom_point()

g+geom_point()+geom_hline(yintercept=mean_rt)
ggsave("reaction_time.pdf")
ggsave("reaction_time.jpg")

Exam.Anxiety <- read.delim("C:/PSYD755/Exam Anxiety.dat", stringsAsFactors=FALSE)
mean(Exam.Anxiety$Exam)
sd(Exam.Anxiety$Exam)
g = ggplot(Exam.Anxiety, aes(x = Anxiety, y = Exam))
g + geom_point(aes(color=Gender))
ggsave("exam_anxiety.jpg")

lecturerData = read.delim("Lecturer Data.dat", header = TRUE)
lecturerData$job=factor(lecturerData$job, levels=c(1:2), labels = c("lecturer","student"))
#install.packages("Hmisc")
#library(Hmisc)
bar = ggplot(lecturerData, aes(job, friends))
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", colour = "black",width =0.2) + 
  labs(x = "Group", y = "Mean Number of Friends")
ggsave("error_friend.jpg")


bar <- ggplot(lecturerData, aes(job, alcohol))
bar + stat_summary(fun.y = "mean", geom = "bar", fill = "White", colour = "Black") +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", colour = "Red", width =
                 0.2) + labs(x = "Job", y = "Mean Alcohol Consumption")
ggsave("error_alcohol.jpg")

line <- ggplot(lecturerData, aes(job, income))
line + stat_summary(fun.y = "mean", geom = "point") + 
  stat_summary(fun.data = "mean_cl_normal", geom= "errorbar", width = 0.2) + 
  labs(x = "Job", y = "Mean Income")+
  stat_summary(fun.y = "mean", geom = "line", aes(group=1),colour = "Red", linetype ="dashed")
ggsave("error_income.jpg")

line <- ggplot(lecturerData, aes(job, neurotic))
line + stat_summary(fun.y = "mean", geom = "point") + 
  stat_summary(fun.y = "mean",geom = "line", aes(group=1),colour = "Red", linetype = "dashed")+
  stat_summary( fun.data = "mean_cl_normal", geom = "errorbar", width = 0.2) + 
  labs(x ="Job", y = "Mean Neuroticism")
ggsave("error_neuro.jpg")


g <- ggplot(lecturerData, aes(y = neurotic, x =alcohol, colour = job))
g + geom_point() + geom_smooth(method = "lm", aes(fill = job), alpha = 0.1) +
  labs(y = "Neuroticism", x = "Alcohol Consumption", colour = "job")
ggsave("scatter_alcohol.jpg")


#4
library(dplyr)
library(reshape)
Infidelity <- read.csv("Infidelity.csv", stringsAsFactors=FALSE)
Bullets = reshape(Infidelity, idvar = c("X", "Gender"), varying = c("Partner", "Self"), v.names = "Number_of_Bullets", timevar = "Recipient", times = c(0:1),direction = "long")
Bullets = mutate(Bullets, Recipient = as.factor(ifelse(Recipient==0,"Partner", "Self")))
Bullets = mutate(Bullets, Gender = as.factor(Gender))
g = ggplot(Bullets, aes(x=Recipient, y=Number_of_Bullets, fill = Gender))
g+ stat_summary(fun.y = "mean", geom = "bar", position="dodge") + 
  stat_summary(fun.data = "mean_cl_normal", 
               geom = "errorbar", 
               position=position_dodge(width=0.90), 
               width = 0.2) + 
  labs(x ="Recipient", y ="Number of Bullets", fill = "Gender")
ggsave("error_recipient.jpg")


#####################################################################
##Illusion###
illusion <- read.csv("C:/PSYD755/illusion.csv", stringsAsFactors=FALSE)
mean(illusion$radius)
sd(illusion$radius)
0.4531759/sqrt(20)
t.test(illusion$radius,mu=20)


##Judging faces##############
face <- read.csv("C:/PSYD755/judging_faces.csv", stringsAsFactors=FALSE)
mean(subset(face$rating,face$age=="Young"))
var(subset(face$rating,face$age=="Young"))
mean(subset(face$rating,face$age=="Old"))
var(subset(face$rating,face$age=="Old"))
pool_variance=((sum(face$age=="Young")-1)*var(subset(face$rating,face$age=="Young")) + (sum(face$age=="Old")-1)*var(subset(face$rating,face$age=="Old")))/(nrow(face)-2)
sqrt(pool_variance*((1/sum(face$age=="Young"))+(1/sum(face$age=="Old"))))
t.test(subset(face$rating,face$age=="Young"),subset(face$rating,face$age=="Old"))

###############################################
##Quiz 2##
setwd("C:/PSYD755")
library(ggplot2)
Supermodel <- read.delim("C:/Users/ho200/Downloads/Supermodel.dat.txt", stringsAsFactors=FALSE)
pubs <- read.delim("C:/Users/ho200/Downloads/pubs.dat.txt", stringsAsFactors=FALSE)
fit_pubs = lm(mortality~pubs, data = pubs)
summary(fit_pubs)
g = ggplot(data = pubs, aes(x=pubs, y=mortality))+ggtitle("Mortality~pubs")
g+geom_point()+geom_smooth(method = "lm", se=F)
ggsave("mort_pub_reg.jpg")
 
fit_model_all= lm(salary~ ., data = Supermodel)
summary(fit_model_all)
fit_model_nobeauty = lm (salary~ age+years, data = Supermodel)
summary(fit_model_nobeauty)
fit_model_beauty = lm(salary~beauty, data = Supermodel)
summary(fit_model_beauty)
fit_model_age = lm(salary~age, data = Supermodel)
summary(fit_model_age)
fit_model_year = lm(salary~years, data = Supermodel) 
summary(fit_model_year)


##R-squred = sqrt(t^2/t^2+df) from t-test