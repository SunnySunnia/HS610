setwd("C:/HS616/LectureCodes")

# babies.txt Variable Descriptions:
# bwt Birth weight in ounces 
# gestation Length of pregnancy in days 
# parity 0 = first born, 1 = otherwise 
# age mother's age in years 
# height mother's height in inches 
# weight Mother's pre-pregnancy weight in pounds 
# smoke Smoking status of mother: 0 = not now, 1 = yes now

#install.packages("downloader")
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)


#1 : replace with a loop rather than separate calls to hist for each column
# Note that histograms show some variables are not normally distributed and some that are unreasonable
hist (babies$bwt,50 ) # normally distributed
hist (babies$gestation, 100)
hist (babies$parity)  # not normally distributed ( boolean: 0,1)
hist (babies$age)  # not normally distributed: right-tailed and band of 99s
hist (babies$height, 35) # not normally distributed: right-tailed and band of 99s
hist (babies$weight) # not normally distributed: band of 999s
hist (babies$smoke,20) # not normally distributed {0,1,9}

# Ex: The variable gestation contains values of 999 days, which is unreasonable
# Assume this is a code for missing data. This can be rectified
#  by substituting NA for all 999 in this column.


name_list=colnames(babies)
for (i in 1:ncol(babies)){
  hist(babies[,i],main = paste("Histogram of ", name_list[i]),xlab = name_list[i])
}

#2  state the max value of gestation before and after data modification below
#     (note that NA is not a max)
#  Check the number of NAs in the gestation column before and after data modification
#  After data modification use 'any' to check if there are any NAs in column gestation
#  After data modification use 'all' to check if there are all NAs in column gestation
max(babies$gestation)    #999
sum(is.na(babies$gestation))    #0
is.na(babies$gestation) <- which(babies$gestation == 999) 
max(babies$gestation, na.rm = T)   #353
sum(is.na(babies$gestation))  #13
any(is.na(babies$gestation))   #True
all(is.na(babies$gestation))    #False

#3 Replace these with ifelse
# Then show that max age is now reasonable
is.na(babies$age) <- which(babies$age == 99)
is.na(babies$height) <- which(babies$height == 99)
is.na(babies$weight) <- which(babies$weight == 999)
is.na(babies$smoke) <- which(babies$smoke== 9)

babies$age <- ifelse(babies$age == 99,NA,babies$age)
max(babies$age, na.rm = T)
babies$height <- ifelse(babies$height==99, NA, babies$height)
babies$weight <- ifelse(babies$weight==999, NA, babies$weight)
babies$smoke <- ifelse(babies$smoke==9, NA, babies$smoke)


#4 Write a function having a parameter dat. Your function will determine 
# for each column of dat the min, max and number of values that are NA 
# Note that NA is not a max value, and should be ignored unless the entire column is NA
min_max_na=function(dat){
  res=data.frame()
  for (i in 1:ncol(dat)){
    if(all(is.na(dat[,i]))){
      min=NA
      max=NA
      na=sum(is.na(dat[,i]))
    }
    else{
      min=min(dat[,i],na.rm = T)
      max=max(dat[,i],na.rm = T)
      na=sum(is.na(dat[,i]))
    }
    res=rbind(res,c(min, max, na))
    rownames(res)[i]=colnames(dat)[i]
  } 
  colnames(res)=c("min","max","#NA")
  return(res)
} 

min_max_na(babies)


#5 Create babies2 so that you may modify data further without affecting babies
#  Demonstrate that you can modify babies2 without affecting babies
print(sum(is.na(babies$age)))  #babies$age has 2 NA's
babies2=babies[,]               #copy babies
print(sum(is.na(babies2$age)))     #babies2$age has 2 NA's too.
babies2[is.na(babies2$age),"age"]=99      #change babies2$age's NA's into 99
print(sum(is.na(babies$age)))            #babies$age still has 2 NA's
print(sum(is.na(babies2$age)))            #babies2$age have no NA's now
is.na(babies2$age) <- which(babies2$age == 99)      #change 99 back to NA's.




#6 Write a function having a parameter dat. Your function will plot a 
# histogram of each column of dat.
# Call your function to plot all the columns of babies2
# We notice that the bands of 9,99, and 999 are not there
colhist=function(dat){
  name_list=colnames(dat)
  for (i in 1:ncol(dat)){
    hist(dat[,i],main = paste("Histogram of ", name_list[i]),xlab = name_list[i])
    
  }
} 
colhist(babies2)



#7 Find the max and min values of gestation (NA is not a max)
# Modify babies2 to remove outliers from gestation.
# Use a compound test (see Lander 9.4). We will consider all 
#  values of gestation<220 and values gestation>330 to be outliers
max(babies$gestation,na.rm=T)
min(babies$gestation,na.rm = T)
babies2=subset(babies2, babies2$gestation<330 & babies2$gestation>220)

#8 Add a new variable (ie: column) named age.level to babies2.
# age.level will be a categorical variable based on the age of each mother
# Your variable should be ordered and have 6 factor levels: teens, early.20s
#  late.20s, early.30s, late.30s and forty.plus
age.level=ifelse(babies2$age<20, "teens", ifelse(babies2$age<25,"early.20s",ifelse(babies2$age<30,"late.20s",ifelse(babies$age<35,"early.30s",ifelse(babies2$age<40, "late.30s", "forty.plus")))))
age.level=factor(age.level, levels = c("teens", "early.20s","late.20s", "early.30s", "late.30s","forty.plus"), ordered = T)
babies2=cbind(babies2,age.level)

#9 Create an interesting plot using ggplot2 where you color by age.level
g=ggplot(babies2,aes(x=gestation, y=bwt))
g+geom_point(aes(color=age.level))

#10 Create a subset named nonsmokers of babies2 whose smoke values are zero
# Create a subset named smokers of babies2 whose smoke values are 1
# Fit a linear model of birthweight as a function of gestation for nonsmokers
# Fit a linear model of birthweight as a function of gestation for smokers
# Fit a linear model of birthweight as a function of gestation for babies2
nonsmokers=subset(babies2, babies2$smoke==0)
smokers = subset(babies2, babies2$smoke==1)
fit_bwt_gest_nonsmoke=lm(bwt~gestation, data = nonsmokers)
fit_bwt_gest_smokers = lm(bwt~gestation, data = smokers)
fit_bwt_gest_babies2 = lm(bwt~gestation, data = babies2)

#11 Create a scatterplot (not a residual plot) of birthweight as a function of
#  gestation for babies2, color by smoke. 
# Create 3 ablines on this plot: one for each linear model
g=ggplot(babies2, aes(y=bwt, x=gestation))
g+geom_point(aes(color=smoke))+geom_abline(color=2,slope = fit_bwt_gest_babies2$coefficients[2],intercept = fit_bwt_gest_babies2$coefficients[1])+
  geom_abline(color=1,slope = fit_bwt_gest_nonsmoke$coefficients[2],intercept = fit_bwt_gest_nonsmoke$coefficients[1])  +
  geom_abline(color=4,slope = fit_bwt_gest_smokers$coefficients[2],intercept = fit_bwt_gest_smokers$coefficients[1])



#4_1########################################################
#install.packages("dplyr")
library(dplyr)
library(ggplot2)

### available on github
#The mammals sleep data set contains the 
# sleeptimes and weights for a set of mammals 
# column name	Description
# name	common name
# genus	taxonomic rank
# vore	carnivore, omnivore or herbivore?
# order	taxonomic rank
# conservation	the conservation status of the mammal
# sleep_total	total amount of sleep, in hours
# sleep_rem	rem sleep, in hours
# sleep_cycle	length of sleep cycle, in hours
# awake	amount of time spent awake, in hours
# brainwt	brain weight in kilograms
# bodywt	body weight in kilograms

setwd("C:/HS616")
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- "msleep_ggplot2.csv"
if (!file.exists(filename)) download(url,filename)
msleep <- read.csv("msleep_ggplot2.csv")
head(msleep)

#apply family vectorized functions
apply(msleep[,6:9],2,mean, na.rm=T)

# dplyr tutorial adapted from:
# http://genomicsclass.github.io/book/pages/dplyr_tutorial.html
# The dplyr package uses efficient data storage backends, is fast
# dplyr functions take a data frame and return a data frame:
# select (return new df that is subset of cols: like select in SQL)
# filter (return  new df that is subset of rows that meet logical condition: like where clause)
# arrange: (return new df that has reordered rows: like  orderby, can use desc)
# rename:  (return new df that with variable(s) renamed) 
# mutate: (return  new df with transformed or new variables)  

# More infofrom: Introduction to dplyr
# https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
library(dplyr)

#select columns: select name, sleep_total from msleep
# (return new df that is subset of cols: like select in SQL)

#select columns 3:7 from msleep
select(msleep,3:7)
select(msleep,-(3:7))
select(msleep,genus)

#select all columns *except* genus
select(msleep,-genus)

#select all columns *except* genus, vore, order
###select(msleep, -(genus, vore, order))  ## wrong
select(msleep,-genus,-vore,-order)

#select all columns *except* columns 3:7 
select(msleep, -(3:7)) 

#select all columns that start with (starts_with) the character string "sl"
select(msleep, starts_with("sl"))

# distinct:  select   the distinct orders
distinct(select(msleep, order))

#filter the rows for mammals that sleep between 12 and 15 hours inclusive
# Here are 2 base R ways to do this, replace with filter
msleep[msleep$sleep_total >= 12 & msleep$sleep_total<=15, ]
subset(msleep, sleep_total >= 12 & msleep$sleep_total<=15)

filter(msleep,sleep_total >=12 & sleep_total<=15)

#filter() works similarly to subset()
# you can give it any number of filtering conditions, 
# which are joined together with & (not &&). 
# You can also use other boolean operators

#filter the rows for mammals that sleep <12 hours or are awake>12 hours
filter(msleep, sleep_total<12 | awake >12)

# use pipe to filter as above then select columns sleep_total and awake, display head
msleep %>% filter(sleep_total <12 | awake >12) %>% select(sleep_total, awake) %>% head


#investigate relationship between sleep_total and awake

all(msleep$sleep_total+msleep$awake ==24)
which(msleep$sleep_total+msleep$awake !=24)

#Filter the rows for mammals that are in the Perissodactyla and Primates taxonomic orders
filter(msleep, order %in% c("Perissodactyla", "Primates"))

#select columns  name and order, arrange (ie: re-order) rows by taxonomic order, display head
msleep %>% select(name,order) %>% arrange(order) %>% head

#select columns name, order, sleep_total from msleep, 
# arrange rows by taxonomic order, then by sleep_total, display head
msleep%>% select(name, order, sleep_total) %>% arrange(order, sleep_total) %>% head

# # Similar to SQL:
# select name, order, sleep_total
#   from msleep
#   order by order,sleep_total
#   limit 6

#Same as above except filter the rows for mammals that 
# sleep for 16+ hours instead of displaying head,
#  arrange rows by taxonomic order, then by descending sleep_total
msleep%>% select(name, order, sleep_total) %>% filter(sleep_total>=16) %>% arrange(order, desc(sleep_total)) 


#Create a new column named rem_proportion which is 
# the ratio of rem sleep to total amount of sleep.
msleep <- mutate(msleep,rem_proportion=sleep_rem/sleep_total)

#SQL: alter table <tablename> add column <columnname> type

# NB: mutate returns a  new df with transformed or new variables 
#  mutate does not mutate the original data frame in place


# summarize values
msleep %>% summarize(avg_sleep=mean(sleep_total))

#group_by: takes an existing tbl and converts it into a grouped tbl
# where operations are performed "by group"


#group by order, summarize mean, min, max, std and totals of sleep_total by group
msleep %>% group_by(order) %>% summarize(avg_sleep=mean(sleep_total))

msleep %>% group_by(order) %>% summarize(avg_sleep=mean(sleep_total),
                                         min_Sleep=min(sleep_total),
                                         max_sleep=max(sleep_total),
                                         std_sleep=sd(sleep_total),
                                         total = n())

sprintf("Hello %s %s!", "first", "last")
cat("hello","first","last")


#4_2################################################################
#preliminaries:rnorm; cbind and rbind
# rnorm randomly generates numbers from the normal distribution
rnums <- rnorm(10)
mean(rnums)
sd(rnums)
# What happens when we increase sample size? Rerun with 10000 rows:


rnums1 <- rnorm(10000)
hist(rnums1, ylim=c(0,3000))
mean(rnums1)
sd(rnums1)

rnums2 = rnorm(10000,100)
hist(rnums2, xlim=c(50,150),ylim=c(0,3000))

rnums3 = rnorm(10000,100,10)
hist(rnums3, xlim=c(50,150),ylim=c(0,3000))

rnums_cols<-cbind(rnums1, rnums2, rnums3)
head(rnums_cols)
rnums_rows<- rbind(rnums1, rnums2, rnums3)
head(rnums_rows)

# apply family of functions: 
# apply operates on every row or every column of an array (including matrix, dataframe)
#   will coerce all elements in the row of column to be of the same type
# lapply operates on a list, vector or data frame and returns a list
# sapply operates on a list,vector or data frame and attempts to return a vector
#  sapply is a wapper function for lapply, to simplify the output when possible

# review matrix positional args: data, nrow, ncol
matrix(1:12, 4, 2)# positional args: row before column
m1 <- matrix(1:12, 4) # if only 2 positional args: data, nrows
m1
m2 <- matrix(1:12, ncol=4)# keyword arg
m2

# apply mean to rows 
apply(m1,2, mean)

#what happpens with lapply or sapply?
lapply(m2,mean) #operates on every element of list or vector, returns a list
sapply(m2,mean) #operates on every element of list or vector, returns a vector
# show  matrix can be coerced to list or vector: above is equivalent to:


# matrix with randomly generated numbers:
A <-rnorm(10)
B <-rnorm(10,5)
C <-rnorm(10,5,3)
# create matrix having columns of A,B,C  *
m = matrix(cbind(A,B,C), nrow = 10, ncol =3)
#m = cbind(A,B,C)
#apply the mean to columns  *
apply(m, 2, mean)
apply(m, 2, sd)

v <- 1:10 # vector: apply functions


a<-array(1:9,dim=c(3,3)) #array


lst1 <- list("hello", c(T, F), 1:10) #list
lapply(lst1, nchar)
sapply(lst1, nchar)
lapply(lst1, min)
sapply(lst1, min)

lst2 <- list(A = 1:7, B = matrix(1:12, 4), D = 12)#list with named elements
#lapply operates on each item of a list and returns a list


# apply with a user-defined function:
f <- function(x){
  x^2 + 6*x - 1
}
sapply(1:10, f)

# data frame
data (mtcars)
min(mtcars$mpg)
which(mtcars$mpg == min(mtcars$mpg))
# cell 1  to access the first occurrence of the min value:
which(mtcars$mpg == min(mtcars$mpg))[1]

# do above, but apply to each column of the data frame:
apply(mtcars, 2,min) 
apply (mtcars, 2, function(x) which(x == min(x))[1])

# break apart the above function:
f2<-function(x){
  which(x == min(x)) [1] 
} 
apply (mtcars, 2, f2) # applies f2 to each column of mtcars

library(dplyr)
mtcars2<- mtcars # copy mtcars
#explore
mtcars2 %>% filter(cyl>=6, disp<150 )
mtcars2 %>% filter(cyl<6, disp>145 )
mtcars2$engine_size <- ifelse(mtcars$disp<145, "small", "large")
# make engine_size ordered with small < large


#compare apply functions


#Assign4############################################################
#install.packages("dplyr")
library(dplyr)
library(ggplot2)

# Variable Descriptions:
# bwt Birth weight in ounces 
# gestation Length of pregnancy in days 
# parity 0 = first born, 1 = otherwise 
# age mother's age in years 
# height mother's height in inches 
# weight Mother's pre-pregnancy weight in pounds 
# smoke Smoking status of mother: 0 = not now, 1 = yes now

#install.packages("downloader")
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)

# Substitute NA for the codes used to indicate missing data
babies$gestation <- ifelse(babies$gestation==999, NA,babies$gestation)
babies$age <- ifelse(babies$age == 99, NA,babies$age)
babies$height <- ifelse(babies$height == 99, NA,babies$height)
babies$weight <- ifelse(babies$weight == 999, NA,babies$weight)
babies$smoke <- ifelse(babies$smoke == 9, NA,babies$smoke)
max(babies$age, na.rm = TRUE) # max age is now reasonable

babies2 = babies
# Leave out outliers  x<220 and x>330
babies2$gestation <- ifelse(babies2$gestation<220 | babies2$gestation> 330, NA,babies2$gestation) # alt: now 20 NA

babies2$age.level <- factor( rep("early.20s",nrow(babies2)), ordered=T,levels =c("teens", "early.20s", "late.20s", "early.30s", "late.30s", "forty.plus"))
babies2$age.level[babies2$age<20] <- "teens"
babies2$age.level[babies2$age>=25 & babies2$age<30] <- "late.20s"
babies2$age.level[babies2$age>=30 & babies2$age<35] <- "early.30s"
babies2$age.level[babies2$age>=35 & babies2$age<40] <- "late.30s"
babies2$age.level[babies2$age>=40] <- "forty.plus"
babies2$age.level#optional, shows ordering


#1 save babies2 as babies2.Rdata
save(babies2,file = "babies2.Rdata")
#   save babies2 as babies2.csv
write.csv(babies2, file = "babies2.csv")

#2
library(dplyr)
#select returns new df with subset of columns: like select in SQL
# 2a select the 5 columns of babies2 from age to age.level inclusive
select(babies2,4:8)

# 2b select columns parity, smoke, age.level from  babies2 
select(babies2, parity, smoke, age.level)

# 2c select all columns except those 5 you selected in 2a
select(babies2, -(4:8))

# 2d select all columns except those 3 you selected in 2b
select(babies2, -parity, -smoke, -age.level)

#3
#filter returns  new df that is subset of rows that meet logical condition: like where in SQL
#filter babies2 to include all rows with age between 28 and 32 inclusive
filter(babies2, age>=28 & age <=32)



#4
#arrange: return new df with rows reordered:: like  orderby in SQL, can use desc
# Create babies3 by reordering babies2 by age level, then by descending gestation
babies3=babies2 %>% arrange(age.level, desc(gestation))


#5
#rename: returns new df with variable(s) renamed *****************
# 5a: Rename parity to is.firstborn, rename smoke to is.smoker
babies3 = rename(babies3, is.firstborn = parity, is.smoker = smoke)
# 5b: Use a non dplyr way: rename bwt to birthweight
names(babies3)=ifelse(names(babies3)=="bwt","birthweight", names(babies3))


#6
#mutate: return new df with transformed or new variables 
# Mutate babies3 so that the mean height is subtracted from each value for height
babies3=mutate(babies3,height=height-mean(height,na.rm = T))

# Look at your babies3 and make sure you have modifed it but haven't rendered the data unusable



#7 new variable:
#Mutate babies2to add a new factor variable named birth.size
# such that bwt>120 is large and  bwt <= 120 is small
babies2 = mutate(babies2, birth.size = factor(ifelse(bwt>120,"large", "small")))


#8
# group_by:takes an existing tbl and converts it into a grouped tbl
#  where operations are performed "by group". 
#  Returns the converted grouped tbl
# 8a: Create group babies2 by age.level and store in new variable resultset 
resultset = babies2 %>% group_by(age.level)
# 8B: Summarize resultset: provide for each group the med.weight and 
#  med.gest, which are output names for median weight and median gestation.
# Look at your output and make sure you do not have NAs.
resultset %>% summarize(med.weight=median(weight, na.rm=T),
                        med.gest=median(gestation, na.rm=T))
# 8c: Now use the same command to summarize babies2 
babies2 %>% summarize(med.weight=median(weight, na.rm=T),
                      med.gest=median(gestation, na.rm=T))
# 8d: write a brief sentence on the difference in summarizing babies2 and resultset

#8d#Summarizing babies2 will give the medians of the whole dataset, however, resultset
##has been grouped, so summarizing resultset will give medians to each group of the dataset.


#9 pipeline operator %>% 
#Use a piped expression to selct the following information:
# select birthweight , gestation 
#  from babies3 
#  where weight > median(weight) and is.smoker == T and age.level==early.20s
#   order by gestation
babies3 %>% filter(weight > median(weight,na.rm=T) & 
                     is.smoker==1 & age.level == "early.20s")%>% 
  select(birthweight, gestation)  %>% arrange(gestation)


#10 boxplots
#Create 3 boxplot for the variable gestation as follows:
# 10a: create a plot containing side-by-side boxplots for each of the 6 age.levels
ggplot(babies2, aes(x=age.level,y=gestation, fill=age.level)) + geom_boxplot()

# 10b: create a plot containing side-by-side boxplots for the 2 smoke levels
ggplot(babies2, aes(x=smoke,y=gestation, fill=as.factor(smoke))) + geom_boxplot()

# 10c: create a plot containing side-by-side boxplots for the 2 parity levels
ggplot(babies2, aes(x=parity,y=gestation, fill=as.factor(parity))) + geom_boxplot()



#11 apply functions:
# 11a Use one of the apply functions to find the min of each column of babies2
apply(babies2, 2, min,na.rm=T)

# 11b Use one of the apply functions to find the max of each column of babies2
apply(babies2, 2, max, na.rm=T)

# 11c Use one of the apply functions to find the mean of each column of babies2
apply(apply(babies2,2,as.numeric), 2, mean, na.rm=T)

# 11d Use one of the apply functions to find the median of each column of babies2
apply(apply(babies2,2,as.numeric), 2, median, na.rm=T)

# 11e Use one of the apply functions to find the standard deviation of each column of babies2
apply(apply(babies2,2,as.numeric), 2, sd,na.rm=T)

#12 
#12a: Create a vector named A of 20 randomly generated numbers
#  drawn from the normal distribution
A=rnorm(20)

#12b: Create a vector named B containing the string "hello world!"
B=c("hello world!")

#12c: Create a vector named C that is a matrix of numbers
#  from 1 to 20  in 4 rows and 5 columns
C=matrix(1:20, nrow=4, ncol=5)

#12d: Create a list named lst that is composed of items A,B and C 
lst=list(A,B,C)

#12e: Use one of the functions in the apply family to determine
#  the mean of each element of lst
lapply(lst, mean)

#13 Create a user-defined function to print out the min, max, mean and median
# of a parameter you name dat. Call your function passing in babies
f13=function(dat){
  return(c(min(dat,na.rm=T), max(dat,na.rm=T), mean(as.numeric(dat),na.rm=T),median(as.numeric(dat),na.rm = T)))
}
apply(babies, 2, f13)
#it prints the results from one function of each column in a row
#-->4 rows, one row for one function from {min, max, mean, median}



#5_1###########################################################
setwd("C:/HS616/LectureCodes")
# debugging: reproducibility set seed for pseudo-random numbers
set.seed=1
# if commented out, seed is taken from the clock, different each time

r1<- rnorm(10000)   #normal distribution (0,1)
hist (r1, 10, ylim=c(0,4000), xlim=c(-10,30))  #####note the spread of the function#####
hist(r1, 45) 
# pnorm is the cumulative distribution function
# pnorm gives you the probablity that a random variable (RV)
#  x is less than the first argument 
pnorm(q=0)
pnorm(1) #probability that random x < 1
pnorm(2) 
pnorm(3)  


qnorm(.5)  # expect 50% of randomly generated values to be below this value
qnorm(0.8413447)
qnorm(0.9772499)
qnorm(0.9986501)

#How to we get the prob that RV x is 1 standard deviation from the mean?
pnorm(1)-pnorm(-1)   #proportion between -1 and 1

#How to we get the prob that RV x is 2 standard deviation from the mean?
pnorm(2)-pnorm(-2)    #within 2 sd

#How to we get the prob that RV x is 3 standard deviation from the mean?
pnorm(3)-pnorm(-3)    #within 3 sd

#We can pass in mean and/or sd rather than use default of 0 and 1 
pnorm(0,0,1)
pnorm(q=0,mean=0,sd=1)
pnorm(0,10,4)    #probability < 0 in normal distribution(mean=10, sd=4)
hist(rnorm(10000,10,4), 40, ylim=c(0,4000), xlim=c(-10,30))  #####note the spread of the function#####

# dnorm returns the height of the normal curve at a RV.
hist(r1, freq = F)
dnorm(0)
hist(rnorm(10000,10,4),freq = F)
dnorm(0,10,4)
dnorm(10,10,4)

# Create a data frame of the size you need, fill in as you go
data.frame(matrix(NA, nrow = 2, ncol = 3))

# melt from wide to long format (ex: for faceted plotting)
# id columns 
#default:  all columns that are not id variables will be measured variables
# measured variables are stacked into a single column of data
library (reshape2)
b <- melt(babies3)
head(b)
tail(b)
ggplot(b,aes(x = value)) + 
  # comparing diverse rather than similar data, so scales should range freely to display data best
  facet_wrap(~variable,scales = "free") +  # also try scales = "free"
  geom_histogram(color = 2,fill=5)

b <- melt(babies2) 
b <- melt(babies2, id.vars = "birth.size") 


# Histograms and density plots
g <- ggplot(babies3, aes(x=birthweight, fill=age.level))

# Overlaid histograms with position="identity"
g + geom_histogram(binwidth=10, alpha=.5, position="identity")

# Interleaved histograms with position="dodge"
g + geom_histogram(binwidth=10, position="dodge")

# Density is used compare distributions with very different counts
g + geom_density()

# Density plots with semi-transparent fill
g + geom_density(alpha=.4)


# look over assignment
#if time
runif()

lapply(1:4, runif)
lapply(1:4, runif, min=-1,max=1)

#5_2##############################################################
data (mtcars) 

# creates a vector of labels for 1000 elements 
rep("label1",10)

# combines two vectors made up of repetitions into one
c((rep("label1",10)),(rep("label2",5)))

rnorm1 <- rnorm(1000, mean=100, sd=15)
hist(rnorm1, probability=TRUE)
#we want to draw a line to approximate the density function
# generate 100 xxvalues from min(x) to max(x)
xx <- seq(min(rnorm1), max(rnorm1), length=100)
yy <- dnorm(xx, mean=100, sd=15)
# draws a line on the most recent plot (similarly to abline)
lines(xx, yy)# creates a line from the (xx,yy) points


# Plots for the standard normal distribution
# source: ___________________
set.seed(3000)
xseq<-seq(-4,4,.01)
densities<-dnorm(xseq, 0,1)
cumulative<-pnorm(xseq, 0, 1) # always increasing
randomdeviates<-rnorm(1000,0,1)

par(mfrow=c(1,3), mar=c(3,4,4,2))
plot(xseq, densities, col="darkgreen",xlab="", ylab="Density", type="l",lwd=2, cex=2, main="PDF of Standard Normal", cex.axis=.8)
plot(xseq, cumulative, col="darkorange", xlab="", ylab="Cumulative Probability",type="l",lwd=2, cex=2, main="CDF of Standard Normal", cex.axis=.8)
hist(randomdeviates, main="Random draws from Std Normal", cex.axis=.8, xlim=c(-4,4))



binom1 <- rbinom(1000, 100, .5)
# Since this is a discrete function we have a probability function (PF) 
#     (also known as probability mass function)
#  rather than a probablility density function (PDF)
# dbinom looks up P(X = 20) when X is drawn from 
#    binomial(100, 0.2) distribution.
dbinom(50, 100, .5) # probability that number of successes=20
# over 100 trials, each trial having probability of success = .2
hist(binom1) # does this surprise you?
# Will a different histogram be more informative about dbinom?
hist(binom1, freq = F, 100)
dbinom(27, size=100, prob=0.25)




# Shapiro-wilk test for normality 
# non-missing values must be between 3 and 5000
# p-value gives us probablilty that the distribution is normal
# tests the NULL hypothesis that the samples came from a Normal distribution
#  This means that if your p-value <= 0.05, then reject the NULL hypothesis 
#   and conclude your sample is likely to be from a non-normal distribution
r1 <- rnorm(5000,10,4)
shapiro.test(r1)
#Normal Q-Q plot: plot against theoretical quantiles
#  qqline draw straight line, color=red
qqnorm(r1); qqline(r1, col = 2,lwd=2)
par(mfrow=c(1,1))
qqnorm(rnorm(1000,10,4)); qqline(rnorm(1000,10,4), col = 2,lwd=2)
qqnorm(runif(1000)); qqline(runif(1000), col = 2,lwd=2)
qqnorm(rpois(1000,1)); qqline(rpois(1000,1), col = 2,lwd=2) 
qqnorm(rbinom(1000,30, .5)); qqline(rbinom(1000,30, .5), col = 2,lwd=2) 


#A poisson distribution is for count data, 
# lambda is both mean and variance.
pois1<- rnorm(100,1)
pois2<- rnorm(100,10)
par(mfrow=c(1,2))
hist(pois1)
hist(pois2)

###################################################################
##Assign 5 #######################
#setwd("C:/Users/vhasfczengs/Downloads")
library(ggplot2)

#1a
norm1 = rnorm(1000, 0,1)
#1b
norm2 = rnorm(1000, 5,3)
#1c
par(mfrow=c(1,2))
hist(norm1,xlim = c(-6, 16),ylim=c(0,260))
hist(norm2,xlim=c(-6,16),ylim=c(0,260))

#2a
d1=data.frame(norm1)
names(d1)="norm"
d1$index=rep("norm1",1000)
d2=data.frame(norm2)
names(d2)="norm"
d2$index=rep("norm2",1000)
data4hist=data.frame(rbind(d1, d2))

g = ggplot(data4hist,aes(norm, fill =index ))
g+geom_histogram(binwidth=.5, position="dodge")

#2b
g+geom_histogram(binwidth=.5, position="identity")

#3
norm3 = rnorm(100,20,10)

#4a
df=data.frame(norm1,norm3)
data4hist2=melt(df)
g = ggplot(data4hist2,aes(value, fill =variable ))
g+geom_density()

d1=data.frame(norm1)
names(d1)="norm"
d1$index=rep("norm1",1000)
d3=data.frame(norm3)
names(d3)="norm"
d3$index=rep("norm3",100)
data4hist2=data.frame(rbind(d1, d3))

g = ggplot(data4hist2,aes(norm, fill =index ))
g+geom_density()

#4b
g+geom_density(alpha=0.4)

#5a
pnorm(1, mean(norm1),sd(norm1))

#5b
pnorm(1,mean(norm2),sd(norm2))

#5c
##Because norm1(0,1) and norm2(5,3) have different distributions. 
##norm1 is centered at 0 and spread with sd =1 while norm2 is centered at 5 and spread with sd=3.
##1 will be greater than more than half of the ditribution in norm1 while is relatively further less from the center in norm2. 
##So it is more likely a random number from norm1 will be less than 1 than one from norm2.


#6
cumnorm1=pnorm(norm1,mean(norm1),sd(norm1))
cumnorm2=pnorm(norm2,mean(norm2),sd(norm2))
dennorm1=dnorm(norm1,mean(norm1),sd(norm1))
dennorm2=dnorm(norm2,mean(norm2),sd(norm2))
par(mfrow=c(1,2))
plot(norm1,dennorm1, main = "PDF of norm1", col = "purple", pch=20)
abline(v=1)
plot(norm1, cumnorm1, main="CDF of norm1", col="light green", pch=20)
abline(h=pnorm(1, mean(norm1),sd(norm1)))
abline(v=1)

par(mfrow=c(1,2))
plot(norm2,dennorm2, main = "PDF of norm2", col = "purple", pch=20)
abline(v=1)
plot(norm2, cumnorm2, main="CDF of norm2", col="light green", pch=20)
abline(h=pnorm(1, mean(norm2),sd(norm2)))
abline(v=1)
##The PDF plots showed that 1 will be greater than more than half of the ditribution in norm1 while is relatively further less from the center in norm2. 
##The CDF plots actually showed the probabilties of have a random number that is less than 1 in the two distribution. 


#7
pnorm(1, 0, 1)
qnorm(pnorm(1,0,1))
pnorm(qnorm(pnorm(1,0,1)))
##Applying qnorm to the result from pnorm of 1 on Normal(0,1) will return back 1.

#8 
dnorm(0,mean(norm1),sd(norm1))

#9a
pnorm(mean(norm3)+sd(norm3),mean(norm3), sd(norm3))-pnorm(mean(norm3)-sd(norm3),mean(norm3), sd(norm3))

#9b
pnorm(mean(norm3)+2*sd(norm3),mean(norm3), sd(norm3))-pnorm(mean(norm3)-2*sd(norm3),mean(norm3), sd(norm3))

#9c
pnorm(mean(norm3)+3*sd(norm3),mean(norm3), sd(norm3))-pnorm(mean(norm3)-3*sd(norm3),mean(norm3), sd(norm3))


#10a
unif1=runif(1000,0,1)

#10b
unif2=runif(1000,20,30)

#11
par(mfrow=c(1,1))
boxplot(unif1,unif2, main="Comparison of unif1 vs unif2", names=c("unif1","unif2"))

#12a
pois1 = rpois(1000,1)
#12b
pois2 = rpois(1000,30)


#13a
par(mfrow=c(1,1))
boxplot(pois1, pois2, main="Comparison of pois1 vs pois2", names=c("pois1","pois2"))

d1=data.frame(pois1)
names(d1)="norm"
d1$index=rep("pois1",1000)
d2=data.frame(pois2)
names(d2)="norm"
d2$index=rep("pois2",1000)
data4hist=data.frame(rbind(d1, d2))

g = ggplot(data4hist,aes(norm, fill =index ))
g+geom_histogram(binwidth=0.8, position="dodge")+ggtitle("Distributions")

#13b
##pois1 has a more skewed shape while pois2 has a more symmetric shape.

#14a
binom1=rbinom(1000,100, 0.2)
#14b
binom2=rbinom(1000,100, 0.8)


#15
d1=data.frame(binom1)
names(d1)="norm"
d1$index=rep("binom1",1000)
d2=data.frame(binom2)
names(d2)="norm"
d2$index=rep("binom2",1000)
data4hist=data.frame(rbind(d1, d2))

g = ggplot(data4hist,aes(norm, fill =index ))
g+geom_histogram(binwidth=0.8, position="dodge")+ggtitle("Distributions")


#16
bern1 = rbinom(1000,1000,0.5)

#17
rand.dist = data.frame(norm1, unif1, pois1, binom1, bern1)

#18
summary(rand.dist)

#19
library (reshape2)
rand.long=melt(rand.dist)
ggplot(rand.long,aes(x = value)) + 
  facet_wrap(~variable,scales = "free") +  # NB: scales = "free"
  geom_histogram(binwidth=0.3, fill="#9999CC")


#20
apply(rand.dist, 2, shapiro.test)
##Cannot reject the hypothesis that norm1 and bern1 are from a normal distribution.
##But we cannot reject the hypothesis that unif1, pois1, and binom1 are from a normal distrubution.


#############################################################
####6_1 #############
setwd("C:/HS616/LectureCodes")
norm1 <- rnorm(1000)
norm2 <- rnorm(1000,5,3) 
norm3 <- rnorm(100, mean=20, sd=10) 

df_norm <- data.frame(norm1,norm2, norm3)

# Get summary statistics for df_norm
summary(df_norm)  ######## ######## ######## ######## ########

# Use apply to get the varianceof each distibution
#  NB: that apply must have a function arguement to apply: use variance
apply(df_norm,2,var)   ######## ######## ######## ######## ########

library (reshape2)
babies2 = babies
#babies2 <- read.csv("babies2.csv")
# BTW, summary automatically removes NAs from min, max, mean, etc
summary(babies2)

cor(babies2[2:7]) #default is "everything"
cor(babies2[2:7], use="everything") #NA if either column has any NA
cor(babies2[2:7], use="all.obs") # Error if a single NA in any column
# Next two lines keep only *rows* where every entry is not NA
cor(babies2[2:7], use="na.or.complete")
cor(babies2[2:7], use="complete.obs") # Error if there is no row that is complete

#install.packages("GGally")
library (GGally)

# cat vs cat:  bar, box
# cat vs numeric: boxplot , hist
# numeric vs numeric: scatter, cor
babies2$smoke <- factor(babies2$smoke)
ggpairs(babies2[6:7])

# One sample t-test to test null hypothesis that true mean is
# equal to an expected value
# Reject null hypothesis if p-value < alpha
t.test(babies2$age, mu=28) # reject null hypothesis? yes
t.test(babies2$age, mu=15) # reject null hypothesis? yes
t.test(babies2$age, mu=27) # reject null hypothesis? no

# draw a random sample from a distibution
norm1 <- rnorm(10000) # defaults to standard normal distribution

# Density plot shows the density of the 10,000 points sampled from the distribution
# shows density of points near a given value on x axis
hist (norm1,100, freq=F)
hist (norm1,100)

#calculated on the standard normal distribution, not the *sample* norm1
#returns the quantile, ie: the number
# whose cumulative distribution matches the probability
# what quantile has cumulative distribution of .25?
qnorm(.25) # -0.6744898 is the quantile returned
pnorm(-0.6744898) # pnorm of quantile gives us back the cum prob 
# both preceding lines default to standard normal distribution

# Kernel density plot: estimate density function of unknown distribution
#  based on data sample: data smoothing, inferences about population are made 
plot(density(norm1)) # calculated on sample norm1

# Density plot: Utilizing the distribution from which sample is drawn
# We want to draw a smooth line for the density function:
# Generate 200 xxvalues from min(norm1) to max(norm1)
xx <- seq(min(norm1), max(norm1), length=200)
# generate the height of the density function at each of these points
# So we have 200 (xx,yy) points that **span the range of
#  the density function** for this distribution (standard normal)
yy <- dnorm(xx, mean=0, sd=1)
# draws a line on the most recent plot (similarly to abline)
hist (norm1,100, freq=F)
lines(xx, yy, col=2)# creates a smooth line from the (xx,yy) points

# With large sample size we get a simlar plot with ggplot2 density
#  plot, but not usually with small sample size  ***********
normtemp<- rnorm(10)
plot(density(normtemp)) # calculated on sample normtemp
r_df <- data.frame(norm1, normtemp)
ggplot(r_df, aes(x=norm1)) + geom_density() # density of sample norm1
ggplot(r_df, aes(x=normtemp)) + geom_density() # density of sample normtemp
# There is a difference between a sample and 
#  the population it is drawn from  *****************************


#################################################################
### 6_2 ##########################
# z-score (standard score)
# Can compare two scores from different *normal* distributions
# We are basically mapping the distribution to the standard normal

# standard normal distribution
norm2 <- rnorm(10000,10,4)
summary(norm2)
m <- mean(norm2)
s <- sd(norm2)
# z-score(1): val is how many SDs above the mean?
val <- 18
z <- (val - m)/s  
z

# normal distribution with mean=30 and sd=3
norm3 <- rnorm(10000, 30,3)
summary(norm3)
m <- mean(norm3)
s <- sd(norm3)
# What value in this distribution is comparable to val of norm1?
# Use formula for z z-score, solve for s
x <- z*s + m
x
# Check:
z <- (x - m)/s  # 33 is how many SDs above the mean?
z


#The p-value is the probability that the observed data could happen,
# under the condition that the null hypothesis is true
#alpha (significance level) the probability of making a type1 error
# type I error is detecting an effect that is not present
# alpha, simply stated, is the probability of making a wrong decision

#setwd("/Users/Pat/Documents/R/HS_616/assign")
babies <- read.table("babies.txt", header=TRUE)

babies2 <- read.csv("babies2.csv")
babies2<-babies2[, -1]  
summary(babies2)

s <- sd(babies2$bwt)
stats<-summary(babies2$bwt)
m <- stats[4]  
m

# One sample t-test to test null hypothesis that true mean is
# equal to an expected value
# Reject null hypothesis if p-value < alpha
t.test(babies2$age, mu=28) # reject null hypothesis?
t.test(babies2$age, mu=15) # reject null hypothesis?
t.test(babies2$age, mu=27) # reject null hypothesis?

t.test(babies$bwt, babies2$bwt)


###########################################################################
####7_1#############
set.seed(100)

m <- 1
b <- 0
x <- rnorm(20)	
#Generate line with slope m,intercept b, and random noise
y <- m*x + b + rnorm(length(x), mean=0, sd=1)

#
plot(x,y) # with noise
abline(b, m, lty=2, col="green") # original line
#qqplot can show how normally the residuals is distrubuted.

fit<- lm (y~ x)
c <-coef(fit)
abline(b=c[2],a=c[1])
summary(fit) # data fits linear model with R^2 0.6696

# Good fit to a linear model
# Not as good a fit to the line data originated from (green)
library (ggplot2)
df <- data.frame(x,y)
g <- ggplot(df,aes(x=x, y=y))
g + geom_point() + 
  geom_abline(intercept=c[1],slope=c[2]) +
  geom_abline(intercept=b, slope=m, lty=2, col="green")


###########################################################
##7_1##
#test whether the variances of two samples are the same.
var.test()

########################################################
##Assign 7###
#setwd("C:/Users/vhasfczengs/Downloads")
#1
load("height_weight1.rdata")
load("height_weight2.rdata")
load("sampWXXXX.rdata")

#2
explore=function(vec1, vec2, expected){ 
  print (paste("length 1:",length(vec1)))
  print (paste("length 2:",length(vec2)))
  if(length(vec1)!=length(vec2)) stop("Error: variables have different length!")
  par(mfrow=c(1,2))
  hist(vec1,xlim = c(min(vec1,vec2),max(vec1,vec2)))
  hist(vec2,xlim = c(min(vec1,vec2),max(vec1,vec2)))
  print(shapiro.test(vec1))
  print(shapiro.test(vec2))
  print(paste("variance of vec 1:", var(vec1)))
  print(paste("variance of vec 2:", var(vec2)))
  print(paste("Correlation:",cor(vec1,vec2)))
  print(t.test(vec1,mu=expected))
  print(t.test(vec2,mu=expected))
  if(var(vec1)==var(vec2)){
    print(t.test(vec1,vec2,var.equal=TRUE))
  }
  else{
    print(t.test(vec1,vec2,var.equal=F))
  }
} 

#3
test1=rnorm(100)
test2=runif(102)
explore(test1,test2,0)

#4
explore(sampH2010_1, sampH2010_2,1.76)

#5
explore(sampH1960_1, sampH1960_2 ,1.76)

#6
explore(sampH2010_1, sampH1960_1 ,1.76)
explore(sampH2010_2, sampH1960_2, 1.76)
##The null hypothesis: The mean height of males in 2010 equals the mean height of those in 1960.
##Both tests failed to reject the null hypothesis. 

#7
explore(sampW2010_1, sampW2010_2,88.7)

#8
explore(sampW1960_1, sampW1960_2 ,88.7)

#9
explore(sampW2010_1, sampW1960_1,88.7)
explore(sampW2010_2, sampW1960_2,88.7)
##The null hypothesis: The mean weight of males in 2010 equals the mean weight of those in 1960.
##The first test will reject the null hypothesis at a 10% confidence level and the second test will reject at 5% confidence level. 
##With only 10% and 5% false positive rate respectively, one could believe that males in 2010 are on average heavier than males in 1960.


#10
d1=data.frame(sampH1960_1)
names(d1)="values"
d1$index=rep("sampH1960_1",length(sampH1960_1))
d2=data.frame(sampH1960_2)
names(d2)="values"
d2$index=rep("sampH1960_2",length(sampH1960_2))
d3=data.frame(sampH2010_1)
names(d3)="values"
d3$index=rep("sampH2010_1",length(sampH2010_1))
d4=data.frame(sampH2010_2)
names(d4)="values"
d4$index=rep("sampH2010_2",length(sampH2010_2))
data4hist=data.frame(rbind(d1, d2, d3,d4))

g = ggplot(data4hist,aes(values, fill =index ))
g+geom_density(alpha=0.4)
##The modes of the 4 samples are located around the same center.
##So it is convincing that their means and not significantly different from each.



#11
d1=data.frame(sampW1960_1)
names(d1)="values"
d1$index=rep("sampW1960_1",length(sampW1960_1))
d2=data.frame(sampW1960_2)
names(d2)="values"
d2$index=rep("sampW1960_2",length(sampW1960_2))
d3=data.frame(sampW2010_1)
names(d3)="values"
d3$index=rep("sampW2010_1",length(sampW2010_1))
d4=data.frame(sampW2010_2)
names(d4)="values"
d4$index=rep("sampW2010_2",length(sampW2010_2))
data4hist=data.frame(rbind(d1, d2, d3,d4))

g = ggplot(data4hist,aes(values, fill =index ))
g+geom_density(alpha=0.4)
##From the density plots, the samples are not normally distrubuted.
##Their modes are more varient.
## The density curves of weights of males in 2010 are generally above the ones of 1960 on the right side.
## That agrees that weights of males in 2010 are averagely greater than in 1960.

#12
cor(sampWXXXX,sampW1960_1)  #-0.1933038
cor(sampWXXXX,sampW1960_2)  #0.06146935
cor(sampWXXXX,sampW2010_1)  #-0.4769623
cor(sampWXXXX,sampW2010_2)  #1
fit1 = lm(sampWXXXX~sampW2010_2)
summary(fit1)
##As the correlation between sampWXXXX and sampW2010_2 is 1.
##There will be a very strong linear relationship.
##the formula for the fitted line: 
### sampWXXXX_hat = -1.819e-13 + 2.205 * sampW2010_2

#13
fit2 = lm(sampWXXXX~sampW2010_2+sampH2010_2)
##The adjusted r-squared is 1.
##The p-value for sampW2010_2 is <2e-16, it is a significant estimator of sampWXXXX,
##however, sampH2010_2 is not significant.
### sampWXXXX_hat = -1.819e-13 + 2.205e+00 * sampW2010_2 -3.463e-15 * sampH2010_2)

#14
fit3 = lm(sampWXXXX~sampW2010_2*sampH2010_2)
summary(fit3)
##sampWXXXX_hat = 5.002e-13+2.205*sampW2010_2-3.959e-13*sampH2010_2+4.657e-15*(sampW2010_2:sampH2010_2)

fit4 = lm(sampWXXXX~sampW2010_2 + sampH2010_2 + sampW2010_2:sampH2010_2)
##sampWXXXX_hat = 5.002e-13+2.205*sampW2010_2-3.959e-13*sampH2010_2+4.657e-15*(sampW2010_2:sampH2010_2)

#adjusted r-squred @fit1: 1
#adjusted r-squred @fit2: 1
#adjusted r-squred @fit3: 1
#adjusted r-squred @fit4: 1
##Their adjusted r-squred are the same, so the models are equally good. It is not suprising because the first 
###model-the single linear regrassion--already has r-squred=1, adding extra 
###variables will not help too much.

#15
par(mfrow=c(2,2))
plot(fit1)
plot(fit4)
##fit4 has a slightly more scattered residual-fited plot around 0 and 
###a slightly more straight pattern on the Q_Q plot than fit1 does.

#16
permute_w = sample(sampWXXXX, replace=F)
cor(data.frame(permute_w, sampW1960_1, sampW1960_2, sampW2010_1, sampW2010_2))[1,]
#  permute_w  sampW1960_1  sampW1960_2  sampW2010_1  sampW2010_2 
# 1.00000000  0.18378073   0.05092684   -0.47713542  0.26351573 
fit5 = lm(permute_w~sampW2010_2*sampH2010_2)
summary(fit5)
## permute_w_hat = -992.341+13.044*sampW2010_2+648.003*sampH2010_2+(-7.137)*(sampW2010_2*sampH2010_2)
## adjusted r-squared = 0.02322 
## This model certainly fits not as good as the previous ones given its low coefficient of determination.
## Futhermore, none of the variables is a significant predictor to permute_w.

#17
new_w = 0.4536*sampWXXXX - .3*sampW2010_2*sampH2010_2 + .6*permute_w
cor(new_w,permute_w) #0.9338298
## It is a strong correlation because new_w is made with the largest protion from permute_w.

cor(new_w,sampWXXXX) #0.5671837
## It is a moderately strong correlation, new_w is made partially from sampWXXXX.

cor(new_w,sampW2010_2*sampH2010_2) #0.5200085
## It is a moderately strong correaltion as new_w is also made from sampW2010_2*sampH2010_2.

cor(data.frame(new_w,sampW1960_1, sampW1960_2, sampW2010_1, sampW2010_2))[1,]
#  new_w     sampW1960_1  sampW1960_2  sampW2010_1  sampW2010_2 
# 1.00000000  0.05466535  0.07260677  -0.52155254   0.56718368 
##> cor(sampW1960_1,sampW2010_1)
##[1] -0.2541292
## Since sampW1960s have weak correaltion with sampW2010s and only sampW2010_2 has contribution in new_w,
## new_w has really weak correlation with sampW1960s but ralatively strong correlation with sampW2010s.

#18
fit6 = lm(new_w~permute_w*sampW2010_2*sampH2010_2)
summary(fit6)
## adjusted r-squred = 1.
## new_w_hat = -3.295e-11 + 0.6*permute_w + 1*sampW2010_2 + 1.931e-11*sampH2010_2 + 
###   (-2.226e-15)*(permute_w*sampW2010_2) + (-1.052e-13)*(permute_w*sampH2010_2) +
###   (-0.3)*(sampW2010_2*sampH2010_2) + (1.302e-15)*(permute_w*sampW2010_2*sampH2010_2)


#19
fit7 = lm(new_w~permute_w*sampW2010_2*sampH2010_2*sampWXXXX)
summary(fit7)
## adjusted r-squared = 1.
## new_w_hat = (1.589e-10) + 0.6*permute_w + 1*sampW2010_2 + (-9.614e-11)*sampH2010_2
###    + (2.247e-14)*(permute_w*sampW2010_2) + (5.062e-13)*(permute_w*sampH2010_2)
###    + (0.3)*(sampW2010_2*sampH2010_2) + (1.278e-14)*(sampW2010_2*sampWXXXX) 
###    + (-1.354e-14)*(permute_w*sampW2010_2*sampH2010_2)
###    + (-6.813e-17)*(permute_w*sampW2010_2*sampWXXXX)
###    + (-7.644e-15)*(sampW2010_2*sampH2010_2*sampWXXXX)
###    + (4.080e-17)*(permute_w*sampW2010_2*sampH2010_2*sampWXXXX)

## There are 4 coefficients not defined because of singularities due to too weak and 
## too strong correlations between sampWXXXX and permute_w, and sampW2010_2, respectively.



#################################################################
###Assign8###
#1
testdat = read.delim("sample.txt", stringsAsFactors=FALSE)

#2
fit1 = lm(Pass~Hours, data=testdat)

#3
summary(fit1)
## Pass_hat = -0.15394 + 0.23460*Hours

#4
##For every unit increase in hours studies, Pass increases by 0.23460.

#5
pred1 = predict(fit1, type="response")

#6
df4plt=data.frame(testdat,pred1)
g = ggplot(data = df4plt,aes(x=Hours, y=Pass))
g+geom_point()+geom_point(aes(x=Hours, y = pred1), color="red")
##Since the true Pass is binary, the ones with value 0 have negative residuals while the ones
###with value 1 have positive residuals.

#7
plot(fit1)
##The residual vs fitted graph is a sinusoid. It matches to the plot from #6: there are
### 2 discete groups of points giving opposite residuals. 

#8
#(i) No, the relationship between dependent and independent variable is not linear.
#(iv) No, the error distribution, from the residual qq-plot, is not normal.

#9
fit2 = glm(Pass ~ Hours, data = testdat, family = binomial())

#10
summary(fit2)
#log(p(pass)/p(fail) ) = 1.5046 * Hours + (-4.0777)

#11
## For every unit increase in hours studied, the log odds of passing increases by 1.5046.

#12
pred2 = predict(fit2, type="response")

#13
df4plt=data.frame(testdat,pred2)
g = ggplot(data = df4plt,aes(x=Hours, y=Pass))
g+geom_point()+geom_point(aes(x=Hours, y = pred2), color="red")

#14
logisticPseudoR2s(fit2)
#Pseudo R^2 for logistic regression
#Hosmer and Lemeshow R^2   0.421 
#Cox and Snell R^2         0.442 
#Nagelkerke R^2            0.589 

#fit1 adjusted R^2 0.4459
##It seems reasonable. The predicted line is curved towards the two groups for 
##the glm model, it will capture more variance than the lm model with the staright line.
###


#15
#Survival analysis: time from birth to marriage.
#Graphical representation of survival curve for each cohort.
#Kaplan-Meier estimation.
#predicting hazard function from the cohort of the previous decade: the hazard function hasn't 
#change much for late marriagers.


###############################################################
##quiz 8##
setwd("C:/Users/ho200/Downloads")
quiz8_dat = read.delim("quiz8_dat.txt", stringsAsFactors=FALSE)

library(ggplot2)
library(dplyr)
#1
class(quiz8_dat$Scored)
quiz8_dat = mutate(quiz8_dat, scored_bin = ifelse(quiz8_dat$Scored=="Scored ",1,0))
hist(quiz8_dat$scored_bin)  
plot(quiz8_dat$PSWQ,quiz8_dat$scored_bin)
plot(quiz8_dat$Anxious, quiz8_dat$scored_bin)
plot(quiz8_dat$Previous,quiz8_dat$scored_bin)

#2
cor(quiz8_dat[,1:3])


#3

#Because the response variable is binary, use logistic regression


fit2 = glm(scored_bin ~ PSWQ + Anxious + Previous, data = quiz8_dat, family = binomial())
summary(fit2)
pred2 = predict(fit2, type="response")
df4plt=data.frame(quiz8_dat,pred2)
g = ggplot(data = df4plt,aes(x=PSWQ, y=scored_bin))
g+geom_point()+geom_point(aes(x=PSWQ, y = pred2), color="red")

#Since only the PSWQ is significant, consider another model only uses PSWQ as the predictor
fit2 = glm(scored_bin ~ PSWQ, data = quiz8_dat, family = binomial())
summary(fit2)
pred2 = predict(fit2, type="response")
df4plt=data.frame(quiz8_dat,pred2)
g = ggplot(data = df4plt,aes(x=PSWQ, y=scored_bin))
g+geom_point()+geom_point(aes(x=PSWQ, y = pred2), color="red")


#4 
summary(fit2)
## only PSWQ is significant

#5
##For full model
#log(p(scored)/p(missed) ) = -0.25137 * PSWQ + 0.27585 * Anxious +0.20261 * Previous -11.49256 



#############################################################
##9_1##
#logistic regression: for categorical response variable.
#does not make assumption of normality, linearity, and homogeneity of variance
#needs twice as many observation as linear regression
# binary and ordinal logistic regression.
# error terms independent: independent variables.
# log(p(disease)/(1-p(disease))) = beta_0 +beta_1 * X_1 + .....
# 



#############################################################
##9_2###
#setwd(UsersPatDocumentsRHS_616lecture_scripts)
setwd("C:/Users/ho200/Downloads")

#install.packages("ppcor")
library (ppcor)
library(ggplot2)
library(reshape2)
library(dplyr)


logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <-  length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}

# PSWQ: degree to which the player worries in general
# Anxious: new measure of the player's anxiety just before the penalty kick
# Previous: percentage of penalties scored by the player over their career
# Scored: outcome: whether the penalty kick scored

#penalties <- read.table(file="penalty.txt", header = T)  # tab-delimited file with a header
penalties =  read.delim("quiz8_dat.txt", stringsAsFactors=FALSE)


# Exploratory visualizations
g <- ggplot(data=penalties)
g + geom_histogram(aes(x=PSWQ),binwidth=1, color = 5)  

g + geom_histogram(aes(x=PSWQ, fill=Scored),binwidth=1,position="dodge")  # position="identity" for overlaid
g + geom_histogram(aes(x=Anxious, fill=Scored), binwidth=1,position="dodge")
g + geom_histogram(aes(x=Previous, fill=Scored), binwidth=1,position="dodge")

g + geom_density(aes(x=PSWQ, fill=Scored), alpha=.5)   
g + geom_density(aes(x=Anxious, fill=Scored), alpha=.5)  
g + geom_density(aes(x=Previous, fill=Scored), alpha=.5)   

# Tests for normality of data
shapiro.test(penalties$PSWQ)
shapiro.test(penalties$PSWQ[penalties$Scored=="Scored "])
shapiro.test(penalties$PSWQ[penalties$Scored=="Missed "])
shapiro.test(penalties$Anxious[penalties$Scored=="Scored "])
shapiro.test(penalties$Anxious[penalties$Scored=="Missed "])
shapiro.test(penalties$Previous[penalties$Scored=="Scored "])
shapiro.test(penalties$Previous[penalties$Scored=="Missed "])

# T-tests to compare values of scored with missed
t.test(PSWQ ~ Scored, data=penalties) #default var.equal=F
t.test(Anxious ~ Scored, data=penalties)
t.test(Anxious ~ Scored, data=penalties, var.equal=T)
t.test(Previous ~ Scored, data=penalties)



# Look for Correlations and partial correlations
cor(penalties[-4]) 
pcor(penalties[-4])     
# accounts for the effect of controlled-for variables on both of the compared vars   
pcor(penalties)  
penalties = mutate(penalties, Scored = ifelse(penalties$Scored=="Scored ",1,0))

#cor and pcor require that dichotomous variables be coded as 0,1
cor.test(penalties$Scored, penalties$Previous) 

#recode column Scored as 0,1

cor(penalties)
pcor(penalties) 
cor.test(penalties$Scored, penalties$Anxious)
cor.test(penalties$Scored, penalties$PSWQ) # method="spearman" alsmost same
# r= -0.6676552 so R^2= 0.4457635: PSWQ accounts for 44.6% of the variabliltiy of Scored

# Variable selection: start with all variables, then see how the 
#  model is effected if we leave one out
# AIC (Akaike information criterion): relative estimate of information lost 
#    smaller is good: less informaiton lost
fit_all <- glm(Scored ~ ., family=binomial(link="logit"), data=penalties)
summary(fit_all)  #AIC: 55.416
# log (p(Scored)/p(Missed) ) = -0.25137 PSWQ 
#  for other estimated coef, they are likely to have this fit by chance

fit.minus.prev <- glm(Scored ~ . -Previous, family=binomial(link="logit"), data=penalties)
summary(fit.minus.prev) #AIC: 56.074
# log (p(Scored)/p(Missed) ) = -0.2264 PSWQ -0.1190 Anxious 

fit.minus.anxious <- glm(Scored ~ . -Anxious, family=binomial(link="logit"), data=penalties)
summary(fit.minus.anxious) # AIC: 54.662 # best of these models that have intercept
# log (p(Scored)/p(Missed) ) = -0.23009  PSWQ + 0.06480 Previous 

# fit.minus.anxious.inter <- glm(Scored ~ . -Anxious -1, family=binomial(link="logit"), data=penalties)
# summary(fit.minus.anxious.inter) # AIC: 53.255  # best of these models
# # log (p(Scored)/p(Missed) ) = -0.18223  PSWQ + 0.07639 Previous **********
# # Visualize the relationship of PSWQ to Scored  ******
# # overlaid with the probablilities of scoring as predicted by model
# pred_best <- predict(fit.minus.anxious.inter, type="response") 
# gbest <- ggplot(penalties, aes(x=I(-0.18223*PSWQ+0.07639*Previous), y=Scored))
# gbest +  geom_point()  +
#   geom_point(aes(x=I(-0.18223*PSWQ+0.07639*Previous), y= pred_best), color="red") 

fit.minus.PSWQ <- glm(Scored ~ . -PSWQ, family=binomial(link="logit"), data=penalties)
summary(fit.minus.PSWQ) # AIC: 67.141
# all estimated coef are likely to have this fit by chance

fit.PSWQ <- glm(Scored ~ PSWQ, family=binomial(link="logit"), data=penalties)
summary(fit.PSWQ) # AIC: 64.516
# log (p(Scored)/p(Missed) ) = -0.29397  PSWQ + 4.90010
# Use this model to pridict the probablilities of scoring vs PSWQ
pred <- predict(fit.PSWQ, type="response") 
g<- ggplot(penalties, aes(x=PSWQ, y=Scored))
g +  geom_point()  +
  geom_point(aes(x=PSWQ, y= pred), color="red") 
# Look at the distribution of the model residuals
ggplot(fit.PSWQ, aes(x=.resid)) + geom_histogram(binwidth=.2)
# Investigate goodness-of-fit of model using only PSQW
logisticPseudoR2s(fit.PSWQ)

# Ex of other models that would take time to investigate
fit.part<- glm(Scored ~ PSWQ + Previous + Anxious:PSWQ + Anxious:Previous, family=binomial(link="logit"), data=penalties)
summary(fit.part) #AIC: 57.797 
# all estimated coef are likely to have this fit by chance

# Use built-in step fucntion (with caution) to select variables 
#  to try differenct models and minimize AIC:
fit.null <- glm(Scored ~ 1, family=binomial(link="logit"), data=penalties)
summary(fit.null) #AIC:105.64

fit.full <- glm(Scored ~ Previous*Anxious*PSWQ, family=binomial(link="logit"), data=penalties)  
summary(fit.full) #AIC: 58.392 
# all estimated coef are likely to have this fit by chance

# Caution in using the step function to automate the selection of variables
# Is known to sometimes miss the best model
fit.step <- step(fit.null, 
                 scope=list(lower=fit.null, upper=fit.full), direction=  "both")
# best fit: Scored ~ PSWQ + Previous + PSWQ:Previous  AIC=54.25

fit_found <- glm(Scored ~ PSWQ*Previous,family=binomial(link="logit"), data=penalties)
summary(fit_found)
# log (p(Scored)/p(Missed) ) = -0.584338 PSWQ, other coeficients insignificant





# From above:
# fit.minus.anxious <- glm(Scored ~ . -Anxious, family=binomial(link="logit"), data=penalties)
# log (p(Scored)/p(Missed) ) = -0.23009  PSWQ + 0.06480 Previous 

# Investigate goodness-of-fit
logisticPseudoR2s(fit.minus.anxious)


# Use the model to estimate the probability of scoring if 
# both PSWQ and Previous are their mean values
newdf = data.frame( PSWQ=mean(penalties$PSWQ), Previous=mean(penalties$Previous), Anxious=mean(penalties$Anxious))
predict(fit.minus.anxious, newdf, type="response") 


library (stats)
# fit.minus.anxious improvement over null   *****************************
modelChi <- fit.minus.anxious$null.deviance - fit.minus.anxious$deviance
chidf <- fit.minus.anxious$df.null - fit.minus.anxious$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob


#############################################################
##Assign 9 diabetes###
setwd("/Users/Pat/Documents/R/HS_616/assign")

library (ggplot2)
#install.packages("foreign")
library(foreign)
library (stats)


logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <-  length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}


diab <- read.arff("http://www.cs.usfca.edu/~pfrancislyon/uci-diabetes.arff")
summary(diab)

# 1. Number of times pregnant
# 2. Plasma glucose concentration a 2 hours in an oral glucose tolerance test
# 3. Diastolic blood pressure (mm Hg)
# 4. Triceps skin fold thickness (mm)
# 5. 2-Hour serum insulin (mu U/ml)
# 6. Body mass index (weight in kg/(height in m)^2)
# 7. Diabetes pedigree function
# 8. Age (years)
# 9. Class variable (tested negative or tested positive)

#1
# Correct zeros that should be NA
# Note that zero pregnancies is valid, but the following are not
diab$plas <- ifelse(diab$plas==0,NA, diab$plas)
diab$pres <- ifelse(diab$pres==0,NA, diab$pres)
diab$skin <- ifelse(diab$skin==0,NA, diab$skin)
diab$insu <- ifelse(diab$insu==0,NA, diab$insu)
diab$mass <- ifelse(diab$mass==0,NA, diab$mass)
summary(diab)


#2 t-tests



#3 density distributions of each variable with fill color determined by diabetes status
g <- ggplot(data=diab)
g + geom_histogram(aes(x=preg),binwidth=1, color = 5)  

g + geom_histogram(aes(x=preg, fill=class),binwidth=1,position="dodge")  # position="identity" for overlaid
g + geom_histogram(aes(x=plas, fill=class), binwidth=1,position="dodge")
g + geom_histogram(aes(x=pres, fill=class), binwidth=1,position="dodge")

g + geom_histogram(aes(x=skin, fill=class), binwidth=1,position="dodge")
g + geom_histogram(aes(x=insu, fill=class), binwidth=1,position="dodge")
g + geom_histogram(aes(x=mass, fill=class), binwidth=1,position="dodge")

g + geom_density(aes(x=preg, fill=class), alpha=.5)    
g + geom_density(aes(x=plas, fill=class), alpha=.5)   
g + geom_density(aes(x=pres, fill=class), alpha=.5)    
g + geom_density(aes(x=skin, fill=class), alpha=.5)    
g + geom_density(aes(x=insu, fill=class), alpha=.5)   
g + geom_density(aes(x=mass, fill=class), alpha=.5)    
g + geom_density(aes(x=pedi, fill=class), alpha=.5)   
g + geom_density(aes(x=age, fill=class), alpha=.5)    

#4 correlations


#5 Fit a regression model of diabetes as a function of the other variables.
fit_all <- glm(class ~ ., family = binomial(), data = diab)
summary(fit_all) #AIC: 362.02, Nagelkerke R^2 0.452
# Null deviance: 498.10  on 391  degrees of freedom 
# Residual deviance: 344.02  on 383  degrees of freedom
logisticPseudoR2s(fit_all) 

fit_b<- glm(class ~ preg+ plas + mass + pedi, family = binomial(), data = diab)
summary(fit_b) #AIC: 714.72, Nagelkerke R^2 0.415
# Null deviance: 974.75  on 751  degrees of freedom   
# Residual deviance: 704.72  on 747  degrees of freedom  
logisticPseudoR2s(fit_b) 

fit_c <- glm(class ~ plas + mass + pedi, family = binomial(), data = diab)
summary(fit_c ) #AIC: 737.76,  Nagelkerke R^2  0.383 
# Null deviance: 974.75  on 751  degrees of freedom
# Residual deviance: 729.76  on 748  degrees of freedom
logisticPseudoR2s(fit_c ) 



#Extra: replace  NAs with either medians or means
diab2 <- diab
diab2$plas[is.na(diab2$plas)] <- mean(diab2$plas,na.rm=T)
diab2$pres[is.na(diab2$pres)] <- mean(diab2$pres,na.rm=T)
diab2$skin[is.na(diab2$skin)] <- mean(diab2$skin,na.rm=T)
diab2$insu[is.na(diab2$insu)] <- mean(diab2$insu,na.rm=T)
diab2$mass[is.na(diab2$mass)] <- mean(diab2$mass,na.rm=T)
summary(diab2)


# NB: with imputed data, null DF and null deviances are same
#  for all models since they use the same observations
fit_all2 <- glm(class ~ ., family = binomial(), data = diab2)
summary(fit_all2) #AIC: 731.3, Nagelkerke R^2 0.421 
# Null deviance: 993.48  on 767  degrees of freedom
# Residual deviance: 713.30  on 759  degrees of freedom
logisticPseudoR2s(fit_all2)

fit_b2<- glm(class ~ preg+ plas + mass + pedi, family = binomial(), data = diab2)
summary(fit_b2) #AIC: 726.18, Nagelkerke R^2 0.418   ***slightly lower AICs, pseudo R^2s
# Null deviance: 993.48  on 767  degrees of freedom  
# Residual deviance: 716.18  on 763  degrees of freedom  
logisticPseudoR2s(fit_b2)


# test fit_all improvement over null model  *****************************
modelChi <- fit_all2$null.deviance - fit_all2$deviance
chidf <- fit_all2$df.null - fit_all2$df.residual
# pchisq is cumulative distribution function for the chi-squared 
#  (chi^2) distribution with chidf degrees of freedom for
#   quantile = modelChi = null deviance - model deviance
# So (1 - pchisq) is the prob of a test stastistic this good or better by chance
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob # chisq.prob = 0
# indicates improved fit in this model over intercept only (null model) is significant

# test fit_all improvement over fit_b2 model  *****************************
modelChi2 <- fit_b2$deviance - fit_all2$deviance
chidf2 <- fit_b2$df.residual - fit_all2$df.residual
chisq.prob2 <- 1 - pchisq(modelChi2, chidf2)
modelChi2; chidf2; chisq.prob2 # chisq.prob = 0.5781859
# indicates improved fit of model fit_all over fit_b2 is insignificant

anova(fit_b2,fit_all2, test="Chisq") # p-value = 0.5782 (as above)



fit_null <- glm(class~1,family = binomial(), data = diab2)
summary(fit_null)  # AIC=995.48

# cross validation
#install.packages("boot")
library(boot)
cv0 <- cv.glm(diab2, fit_null2, K=5)
cv1 <- cv.glm(diab2, fit_b2, K=5)
cv2 <- cv.glm(diab2, fit_all2, K=5)
cv0$delta # raw error, adjusted error (for not using leave-one-out)
cv1$delta 
cv2$delta


fit_c2 <- glm(class ~ plas + mass + pedi, family = binomial(), data = diab2)
summary(fit_c2 ) #AIC: 752.39,  Nagelkerke R^2  0.382 
# Null deviance: 993.48  on 767  degrees of freedom
# Residual deviance: 744.39  on 764  degrees of freedom
logisticPseudoR2s(fit_c2 ) 


fit_null <- glm(class~1,family = binomial(), data = diab2)
summary(fit_null)  # AIC=995.48

fit_full <- glm(class ~ preg*plas* pres*skin*insu*mass*pedi*age, family = binomial(), data = diab2)
summary(fit_full) # AIC=9739.2  Nagelkerke R^2 -62412.69
# Null deviance:  993.48  on 767  degrees of freedom
# Residual deviance: 9227.18  on 512  degrees of freedom
logisticPseudoR2s(fit_full) 


# Caution in using the step function to automate the selection of variables
# Is known to sometimes miss the best model
fit.step <- step(fit_null, 
                 scope=list(lower=fit_null, upper=fit_full), direction=  "both")
# best fit: class ~ plas + mass + preg + pedi + plas:pedi + plas:preg
fit_found <- glm(class ~ plas + mass + preg + pedi + plas:pedi + plas:preg, family = binomial(), data = diab2)
summary(fit_found)#AIC: 719.26, Nagelkerke R^2 0.431
# Null deviance: 993.48  on 767  degrees of freedom
# Residual deviance: 705.26  on 761  degrees of freedom  
logisticPseudoR2s(fit_found)
anova(fit_found,fit_all2, test="Chisq") # p-value = 0.5782 (as below)

confint(fit_found) # 95% CI for the coefficients
exp(coef(fit_found)) # exponentiated coefficients
exp(confint(fit_found)) # 95% CI for exponentiated coefficients





# linear regression models
diab$class <- ifelse(diab$class=="tested_positive",1, 0)

# mass as a function of other variables
fit_lm_mass <- lm(mass ~ pres+skin+age + class,  data = diab2)
summary(fit_lm_mass) # Adjusted R-squared:   0.37493 
plot(fit_lm_mass)

fit_lm_mass_null <- lm(mass ~ 1,  data = diab2)
fit_lm_mass_fullish <- lm(mass ~ pres*skin*age* class,  data = diab2)

fit.step3 <- step(fit_lm_mass_null, 
                  scope=list(lower=fit_lm_mass_null, upper=fit_lm_mass_fullish), direction=  "both")
fit_mass_find <- lm(mass ~ skin + class + pres + age + skin:age + skin:pres, data = diab2)
summary(fit_mass_find) # Adjusted R-squared:  0.3912 
plot(fit_mass_find)






#6 regression model with diastolic blood pressure as a function of two to four of the other variables
fit_lm_all <- lm(pres ~ .,  data = diab2)# plas, mass, age signif, pedi borderline
summary(fit_lm_all) # Adjusted R-squared:  0.1853 
plot(fit_lm_all)

fit_lm_part <- lm(pres ~ age*plas*skin*mass*pedi,  data = diab2)
summary(fit_lm_part) # Adjusted R-squared:  0.1855 

fit_lm_null <- lm(pres ~ 1,  data = diab2)
summary(fit_lm_part) # Adjusted R-squared:  0.1855 

fit_lm_full <- lm(pres ~ age*plas*skin*mass*pedi * preg*insu* class,  data = diab2)
summary(fit_lm_full) # Adjusted R-squared:  0.2237, all coef NOT significant

fit.step2 <- step(fit_lm_null, 
                  scope=list(lower=fit_lm_null, upper=fit_lm_full), direction=  "both")
# best fit: pres ~ age + mass + plas + pedi + insu + age:mass + mass:insu + plas:insu
# AIC = 3669.7
fit_lm_best <- lm(pres ~ age + mass + plas + pedi + insu + age:mass + mass:insu + plas:insu,  data = diab2)
summary(fit_lm_best) # Adjusted R-squared:  Adjusted R-squared:  0.1969 
# signif coef are: plas, mass:insu , plas:insu

fit_lm_best_sig <- lm(pres ~ plas + mass:insu + plas:insu,  data = diab2)
summary(fit_lm_best_sig)# Adjusted R-squared:  0.1117 <- not best
plot(fit_lm_best_sig)



#################################################################
##10_1 ######
setwd("/Users/Pat/Documents/R/HS_616/lecture_scripts")

temper <- read.csv("weather.data.csv")
plot(temper$month,temper$upper)
plot(temper$yr,temper$upper)

fit_month <- lm(temper$upper~ temper$month)
summary(fit_month)
plot(fit_month)

temper2 <- temper   ##################


plot(temper2$month,temper2$upper)
plot(temper2$yr,temper2$upper)

fit_month <- lm(temper2$upper~ temper2$month)
summary(fit_month)
plot(fit_month)

fit_month_1 <- lm(temper2$upper~ temper2$month -1)
summary(fit_month_1)
plot(fit_month_1)

fit_month_yr<- lm(temper2$upper~ temper2$month + temper2$yr)
summary(fit_month_yr)


#####################################################################
#####10_2##

# set the working directory
setwd("C:/HS616")

# There is a MySQL database for public access at genome-mysql.cse.ucsc.edu.
# This server allows MySQL access to the same set of data currently available on the public UCSC Genome Browser site

###install.packages("RMySQL")  # Run the first time
###install.packages("sqldf")
library(RMySQL) # Database Interface and 'MySQL' Driver for R
library(sqldf)  # Perform SQL selects on R data frames

# Adapted from: http://playingwithr.blogspot.com/2011/05/accessing-mysql-through-r.html
#Establish a connection to the UCSC genone browser
con = dbConnect(MySQL(), user='genome', dbname='hg19', host='genome-mysql.cse.ucsc.edu')
# Return a list of the tables in our connection
dbListTables(con)
# Return a list of the fields in a specific table
dbListFields(con, 'knownGene')
dbListFields(con, 'refGene')

#Run a query
# To retrieve results a chunk at a time, use dbSendQuery, dbFetch, then dbClearResult
# Alternatively, if you want all the results (and they'll fit in memory) use dbGetQuery
#  which sends, fetches and clears for you.
resultSet <- dbSendQuery(con, 'SELECT * FROM refGene')
#res = dbSendQuery(con, 'SELECT name chrom strand FROM refGene WHERE ....')

# Fetch records from a previously executed query
#  and save as a data frame object. 
#  n specifies the number of records to retrieve, n=-1 retrieves all pending records
hg19_refgene = dbFetch(resultSet,n=-1, stringsAsFactors=F)
str(hg19_refgene)
head(hg19_refgene)

# hg19_refgeneF = dbFetch(resultSet,n=-1, stringsAsFactors=T)
# str(hg19_refgeneF)
# head(hg19_refgeneF)


# A data frame is used for storing data tables. It is a list of vectors of equal length
# You can think of the vectors as columns in a database or excel spreadsheet
typeof(hg19_refgene)
colnames(hg19_refgene)
dbClearResult(resultSet)

# Disconnect when done with the mysql database
dbDisconnect(con)


# Add a new column to hold transcription start site (tss) relative to the strand
#  on the + strand, this is the left end of the range (txStart), on the - strand it is the right end (txEnd)
hg19_refgene$tss <- ifelse(hg19_refgene$strand == '+', hg19_refgene$txStart, hg19_refgene$txEnd)

write.table(hg19_refgene, file = "hg19_refgene2.txt", append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")

# Create a subset of the data
hg19 <- hg19_refgene[, c("name", "chrom", "strand", "tss")]
summary(hg19)


################################################################
#####Assign_10###
setwd( "C:/Users/vhasfczengs/Downloads")
library(ggplot2)

#1 
temper <- read.csv("weather.data.csv")
plot(temper$month,temper$upper)
plot(temper$yr,temper$upper)

#2
fit_month0 <- lm(temper$upper~ temper$month)
summary(fit_month0)
##We are predicting average high temperature of the month.
##temper$upper_hat = 0.32559 * temper$month +12.82991.
##According to predicting formula, January will have the lowest predicted temperature, 
###December will have the highest.

#3
plot(fit_month0,1)
##There are 12 different output values predicted by the model, they represent the predicted 
##values for each month (12 input values).
table(temper$month)
##There are about 500 different values in the x-axis, they represent the difference of each high
##temperature in a month comparing to the predicted value of each month.

#4
summary(fit_month0$residuals)
##the range of the values on the y-axis = (-19.96, 21.37)
##A negative y value signifies the actual temperature is less than the predicted value.
##A positive y value signifies the actual temperature is greater than the predicted value.

#5
##There is a downward concaved parabolic pattern in the plot which indicated there is variation 
##in the data that is not explained by the model. The parabolic pattern indicated that the model
##over-estimated (negative residuals) the temperatures in the cold months 
##(Jan, Feb, March, Oct, Nov, and Dec) but under-estimated (positive residuals) the 
##temperatures in the warm months (Apr-Sept).

#6
summary(fit_month0)
##Even though the coefficient is significant, the r-squared is 0.03 which is very low,
##indicating that the model only explains very little variation of the data.
##So the model is not a good fit to the data.

#7
temper2 <- temper   ##################
## fill in your code here to correct the problem with this dataset
temper2$month = factor(temper2$month, ordered = F)

plot(temper2$month,temper2$upper)
plot(temper2$yr,temper2$upper)

#8
fit_month <- lm(temper2$upper~ temper2$month)
summary(fit_month)
plot(fit_month, 1)
##there are 11 predictor variables.
##predicted line equation:
##temper$upper_hat = temper2$month2*0.74112 + temper2$month3*3.2705 + temper2$month4*5.8837
##                  + temper2$month5*9.9508 + temper2$month6*12.3404 + temper2$month7*14.7438
##                  + temper2$month8*15.1749 + temper2$month9*11.4142 + temper2$month10*7.1959
##                  + temper2$month11*2.7907 + temper2$month12*0.3698 + 7.9301
###This prediction equation is more comprehensive than the last one as it predicts the temperature
###with respect to the month.

#9
###This model fits much better that the last in terms of no patterns in residual-fitted plot and 
### a much higher r-squared, 0.70.

#10
fit_month_1 <- lm(temper2$upper~ temper2$month -1)
summary(fit_month_1)
plot(fit_month,1)
plot(fit_month_1,1)
##From the two residual-fitted plots, the residuals are the same for the two models.
fit_month$coefficients
fit_month_1$coefficients
##The coefficients of the two models are different with respect each month, but the differences 
##are approximately the intercept of the fit_month.
summary(fit_month)$sigma
summary(fit_month_1)$sigma
###The residual standard errors are the same to both of the model.

#11
summary(fit_month)
summary(fit_month_1)
#There are 11 coefficients in fit_month and 12 in fit_month_1 but no intercept.
##There are 12 variables for both models.
###The intecept in fit_month and the first coefficient temper2$month1 in fit_month_1 have same
###same Estimate, Std. Error, tvalue, and Pr(>|t|).
###So the the intercept of fit_month is the contribution of the estimation to the first month.
###In other words, the intercept in fit_month is the first predictor in fit_month_1 which also 
###predicts for the first month.

#12 
fit_month$coefficients[6]+fit_month$coefficients[1]
fit_month_1$coefficients[6]
##The sum of the two coefficients for fit_month: temper2$month6 (ie: 12.3404) and the 
##intercept (ie: 7.930) approximately equals the coefficient for fit_month_1 temper2$month6 
##(ie: 20.2705).


fit_month_yr<- lm(temper2$upper~ temper2$month + temper2$yr)
summary(fit_month_yr)
plot(fit_month_yr,1)

fit_month_yr_1<- lm(temper2$upper~ temper2$month + temper2$yr -1)
summary(fit_month_yr_1)
plot(fit_month_yr_1,1)

#13
##fit_month represents the effect on response variable resulting from unit changes
##in predictor variables from the fitted value for January (b0=value for January).

##fit_month_1 represents the effect on response variable resulting from unit changes
##in predictor variables from the origin (b0=0).

#14
summary(fit_month)$adj.r.squared
summary(fit_month_1)$adj.r.squared
##From Residuals and Residual standard error, the two models are really similar.
##However, their r-squared's 0.7084385 and 0.9547667, respectively, do not agree with Residuals 
##and Residual standard error.
##Removing intercept term did increase r-squared in a large amount even without any inprovement
##in residuals.

#15
par(mfrow=c(1,2))
plot(fit_month0,1)
plot(fit_month,1)
summary(fit_month0$residuals)
summary(fit_month$residuals)
##The residual ranges are (-19.96, 21.37) and (-20.27, 13.70), respectively.
##Because fit_month has considered each month separately, for point 6067 from the month of August,
##the fitted value for this point had been increased which resulted in its displacement along the x-axis.

#16
temper2$yr = factor(temper2$yr, ordered = F)
fit_month_yr<- lm(temper2$upper~ temper2$month + temper2$yr)
summary(fit_month_yr)
plot(fit_month_yr,1)
##There are 29 predictor variables(intercept not included).
##This model predicts the high temperature with respect to each month and each year.

#17
fit_month_yr_1<- lm(temper2$upper~ temper2$month + temper2$yr -1)
summary(fit_month_yr_1)
plot(fit_month_yr_1,1)
fit_month_yr$coefficients[6]+fit_month_yr$coefficients[1]
fit_month_yr_1$coefficients[6]
##The sum of the two coefficients for fit_month_yr: temper2$month6 (ie: 12.34087) and the 
##intercept (ie: 6.242108  ) approximately equals the coefficient for fit_month_1 temper2$month6 
##(ie: 18.58298 ).
##The base month is month1(or January). The value for any of the other months is the effect
##on the response variable resulting from a unit change in that predictor relative to the 
##value for the base month with all other predictor variables held constant.
##1987 is missing from both of the models.
##intercept of fit_month_yr is comprised by values for January and year1987.


# fill in your exploratory code here
#18
ggplot(temper2,aes(x = upper, fill = month)) +  geom_density(alpha=.4) +
  ggtitle("Density plots of high temperature of each month")

ggplot(temper2,aes(x = lower, fill = month)) +  geom_density(alpha=.4) +
  ggtitle("Density plots of low temperature of each month")

ggplot(temper2,aes(x = rain, fill = month)) +  geom_density(alpha=.4) +
  ggtitle("Density plots of meansures of rain of each month")
##Most of the density plots for high and low temperatures are not normal (bimodal or skewed).
##Some months have a more notable difference (a horizontal shift) in temperatures than other months.
##For the measure of rainfall, most of the data points are 0 for all months. And its range is 
###very wide.They behave like poisson distributions.

#19
ggplot(temper2,aes(x = upper, fill = yr)) +  geom_density(alpha=.4) +
  ggtitle("Density plots of high temperature of each year")

ggplot(temper2,aes(x = lower, fill = yr)) +  geom_density(alpha=.4) +
  ggtitle("Density plots of low temperature of each year")

ggplot(temper2,aes(x = rain, fill = yr)) +  geom_density(alpha=.4) +
  ggtitle("Density plots of meansures of rain of each year")
##Similar to plots by months, most of the density plots for high and low temperatures are 
##not normal (bimodal or skewed).
##But density plots by years did not show notable differences in temperatures across the years.
##For the measure of rainfall, most of the data points are 0 for all months. And its range is 
###very wide. They behave as poisson distributions except the density curves are not very smooth.

#20
ggplot(temper2,aes(x=month, y=rain, col=month))+geom_point()+
  ggtitle("Rainfall distribution over the months")
##From the data over the years, the heak rainfall is on average during the months of October. 
##The low rainfall of the year is on average during Febuary. 




###############################################################
####11_1#####
#Practical session: Introduction to SVM in R
#Jean-Philippe Vert
#https://escience.rpi.edu/data/DA/svmbasic_notes.pdf

#install.packages("kernlab")
#install.packages("ROCR")

n <- 150 # number of data points
p <- 2 # dimension

sigma <- 1 # variance of the distribution
meanpos <- 0 # centre of the distribution of positive examples
meanneg <- 3 # centre of the distribution of negative examples
npos <- round(n/2) # number of positive examples
nneg <- n-npos # number of negative examples

# Generate the positive and negative examples
xpos <- matrix(rnorm(npos*p,mean=meanpos,sd=sigma),npos,p)
xneg <- matrix(rnorm(nneg*p,mean=meanneg,sd=sigma),npos,p)
x <- rbind(xpos,xneg)

# Generate the labels
y <- matrix(c(rep(1,npos),rep(-1,nneg)))

# Visualize the data
plot(x,col=ifelse(y>0,1,2))
legend("topleft",c('Positive','Negative'),col=seq(2),pch=1,text.col=seq(2))

#Now we split the data into a training set (80%) and a test set (20%):
## Prepare a training and a test set ##
ntrain <- round(n*0.8) # number of training examples
tindex <- sample(n,ntrain) # indices of training samples
xtrain <- x[tindex,]
xtest <- x[-tindex,]
ytrain <- y[tindex]
ytest <- y[-tindex]
istrain=rep(0,n)
istrain[tindex]=1

# Visualize
plot(x,col=ifelse(y>0,1,2),pch=ifelse(istrain==1,1,2))
legend("topleft",c('Positive Train','Positive Test','Negative Train','Negative Test'),
       col=c(1,1,2,2),pch=c(1,2,1,2),text.col=c(1,1,2,2))

# load the kernlab package
library(kernlab)

# train the SVM
svp <- ksvm(xtrain,ytrain,type="C-svc",kernel='vanilladot',C=100,scaled=c())

# General summary
svp

# Attributes that you can access
attributes(svp)
# For example, the support vectors
alpha(svp) # alpha vector (possibly scaled)
# indices of support vectors in data matrix
#  after the possible effect of na.omit and subset
alphaindex(svp)
coef(svp) #The corresponding coefficients times the training labels
# Use the built-in function to pretty-plot the classifier
plot(svp,data=xtrain)
#              plot(scale(x), col=y+2, pch=y+2, xlab="", ylab="")
w <- colSums(coef(svp)[[1]] * x[unlist(alphaindex(svp)),])
b <- b(svp)

# Predict labels on test
ypred = predict(svp,xtest)
table(ytest,ypred)
# Compute accuracy
sum(ypred==ytest)/length(ytest)
# Compute at the prediction scores
ypredscore = predict(svp,xtest,type="decision")

# in SVM, you get more and more detail in gamma and C gets bigger 
## risk of over-fitting  




###########################################################
####Midterm review##

f <- function(x){       
  f <- function(x){             
    x ^ 2       }       
  f(x) + 1         
  } 
f(5)

v.test <- c(1, 0, NA, 0, 2, 1)
ifelse(v.test == 0, v.test + 8, "No")

library(ggplot2) 
library(reshape2)
randNorm <- rnorm(10000,10,4) 
randPois <- rpois(10000, 5)

df4plot = data.frame(randNorm,randPois)
df4plot = melt(df4plot)
#randNorm = cbind(randNorm, rep("Norm", 10000))
#randPois = cbind(randPois, rep("Pois", 10000))
#df4plot=data.frame(rbind(randNorm,randPois))
#colnames(df4plot)= c("values", "labels") 

g=ggplot(df4plot, aes(x=value, fill=variable)) 
g+geom_density( position="identity")

runif(10, 1,10)



##############################################################
##13_1########
#install.packages("tidyr")

setwd("/Users/Pat/Documents/R/HS_616/assign/DB")

data_dir <- "FNDDS_2011"

fortification <- c(`0`="none", `1`="fortified_product", `2`="contains fortified ingredients")

fndds_tables <- list(
  AddFoodDesc = list(
    title="Additional Food Descriptions",
    column_types=c(
      food_code="integer", # foreign key
      seq_num="integer", 
      start_date="date", 
      end_date="date", 
      additional_food_description="text"),
    sep="^"
  ),
  FNDDSNutVal = list(
    title="FNDDS Nutrient Values",
    column_types=c(
      food_code="integer",
      nutrient_code="integer",	# Nutrient Descriptions table
      start_date="date", 
      end_date="date", 
      nutrient_value="double"
    ),
    sep="^"
  ),
  FNDDSSRLinks = list(
    title="FNDDS-SR Links",	# see p34 of fndds_2011_2012_doc.pdf
    column_types=c(
      food_code="integer",
      start_date="date", 
      end_date="date", 
      seq_num="integer",
      sr_code="integer",
      sr_descripton="text",
      amount="double",
      measure="char[3]",	# lb, oz, g, mg, cup, Tsp, qt, fluid ounce, etc
      portion_code="integer",
      retention_code="integer",
      flag="integer",
      weight="double",
      change_type_to_sr_code="char[1]",	# D=data change; F=food change
      change_type_to_weight="char[1]",
      change_type_to_retn_code="char[1]"
    ),
    sep="^"
  ),
  FoodPortionDesc = list(
    title="Food Portion Descriptions",
    column_types=c(
      portion_code="integer", 	# foreign key
      start_date="date",
      end_date="date",
      portion_description="text",
      change_type="char[1]"
    ),
    sep="^"
  ),
  FoodSubcodeLinks = list(
    title="Food code-subcode links",
    column_types=c(
      food_code="integer",
      subcode="integer",
      start_date="date",
      end_date="date"
    ),
    sep="^"
  ),
  FoodWeights = list(
    title="Food Weights",
    column_types=c(
      food_code="integer",	# foreign key
      subcode="integer",
      seq_num="integer",
      portion_code="integer",	# food portion description id
      start_date="date",
      end_date="date",
      portion_weight="double",	# missing values = -9
      change_type="char[1]"	# D=data change, F=food change
    ),
    sep="^"
  ),
  MainFoodDesc = list(
    title="Main Food Descriptions",
    column_types=c(
      food_code="integer", 
      start_date="date", 
      end_date="date", 
      main_food_description="character", 
      fortification_id="integer"),
    sep="^"
  ),
  ModDesc = list(
    title="Modifications Descriptons",
    column_types=c(
      modification_code="integer",
      start_date="date", 
      end_date="date", 
      modification_description="text",
      food_code="integer"
      
    ),
    sep="^"
  ),
  ModNutVal = list(
    title="Modifications Nutrient Values",
    column_types=c(
      modification_code="integer",
      nutrient_code="integer",
      start_date="date", 
      end_date="date", 
      nutrient_value="double"
    ),
    sep="^"
  ),
  MoistNFatAdjust = list(
    title="Moisture & Fat Adjustments",	# to account for changes during cooking
    column_types=c(
      food_code="integer",
      start_date="date", 
      end_date="date", 
      moisture_change="double",
      fat_change="double",
      type_of_fat="integer"	# SR code or food code				
    ),
    sep="^"
  ),
  NutDesc = list(
    title="Nutrient Descriptions",
    column_types=c(
      nutrient_code="integer",
      nutrient_description="text",
      tagname="text",
      unit="text",
      decimals="integer"	# decimal places
    ),
    sep="^"
  ),
  SubcodeDesc = list(
    title="Subcode Descriptions",
    column_types=c(
      subcode="integer",	# key; 0=use default gram weights
      start_date="date",
      end_date="date",
      subcode_description="text"
    ),
    sep="^"
  )
)

# flat file to a data frame: call for each table
assign_data_frame <- function(tbl_name){
  tbl <- read.table(
    file.path(data_dir, paste0(tbl_name, ".txt")), 
    sep="^",
    quote="~",
    stringsAsFactors=FALSE)
  # drop last (empty) column
  tbl <- tbl[1:(length(tbl)-1)]
  names(tbl) <- names(fndds_tables[[tbl_name]][["column_types"]])
  assign(tbl_name, tbl, envir = .GlobalEnv)
}

# flat file to database
fndds2sqlite <- function(data_dir, table_details, sqlite_filename){
  
  library("RSQLite")
  con <- dbConnect(SQLite(), sqlite_filename)
  
  for (tbl_name in names(table_details)){
    file_name <- paste0(tbl_name, ".txt")
    assign_data_frame(tbl_name)
    tbl <- get(tbl_name)
    dbWriteTable(con, tbl_name, tbl, row.names = FALSE)
  }
  
  dbDisconnect(con)
}

fndds2sqlite("FNDDS_2011", fndds_tables, "fndds.sqlite")
library(DBI)

for (tbl in c("FNDDSNutVal", "MainFoodDesc", "NutDesc"))
  assign_data_frame(tbl)

library(dplyr)
library(tidyr)

# Make a simplified selection of foods.
# TO DO: have MainFoodDesc be a tbl sourced from SQLite.
get_selected_foods <- function(){
  # Pull out all "Not Further Specified" foods as a wide selection of reasonably generic items.
  generics <- MainFoodDesc %>% 
    filter( grepl(", NFS", main_food_description )) %>%
    filter(!grepl("infant formula", main_food_description, ignore.case = TRUE ) )
  
  # Raw fruits
  # Berries are covered by "Berries, raw, NFS" and "Berries, frozen, NFS"
  fruits <- MainFoodDesc %>% 
    filter( grepl("^6", food_code) ) %>%
    filter( grepl("^([^,\\(]+), raw$", main_food_description) ) %>% 
    filter( !grepl("berries", main_food_description) )
  
  # Raw vegetables
  # Potatoes are covered by "White potato, NFS", "Sweet potato, NFS", etc.
  vegetables <- MainFoodDesc %>% 
    filter( grepl("^7", food_code) ) %>%
    filter(!grepl("potato", main_food_description)) %>%
    filter( grepl(", raw$", main_food_description))
  
  # 4="legumes, nuts, and seeds"
  nuts_and_seeds <- MainFoodDesc %>% 
    filter( grepl("^4", food_code) ) %>%
    mutate( firstWord = strsplit(main_food_description, " ")[[1]][1] )
  
  # Selected alcoholic beverages
  # All alcoholic beverages: grepl("^93", food_code))
  # "Cocktail, NFS" already gives us "Cocktail"
  alcoholic_beverages <- MainFoodDesc %>% 
    filter( main_food_description %in% c("Beer", "Wine, table, red", "Wine, table, white", 
                                         "Whiskey", "Gin", "Rum", "Vodka") )
  
  # Collect them all into one table
  rbind(generics, fruits, vegetables, alcoholic_beverages) %>%
    select( food_code, main_food_description, fortification_id )  %>% 
    filter( nchar(main_food_description) < 20 ) %>%
    mutate( main_food_description = gsub("(, NFS|, raw)", "", main_food_description) ) 
  
}

foods <- get_selected_foods()	# 163 items

library(sqldf)
long_food_nutrients <- sqldf("SELECT f.main_food_description, nd.nutrient_description, nv.nutrient_value 
                             FROM foods f 
                             INNER JOIN FNDDSNutVal nv ON f.food_code = nv.food_code 
                             INNER JOIN NutDesc nd ON nv.nutrient_code = nd.nutrient_code") 

nutrient_food_df <- spread(long_food_nutrients, main_food_description, nutrient_value, fill=0)

food_nutrient_mat <- t(as.matrix(nutrient_food_df[-1]))
colnames(food_nutrient_mat) <- nutrient_food_df$nutrient_description

save(food_nutrient_mat, file="../week_03_linear_algebra/food_nutrient_mat.Rdata")
saveRDS(foods, file="../week_03_linear_algebra/foods.rds")

################################################
###13_1_2##
# A database interface (DBI) definition for communication between R and relational database management systems. 
# All classes in this package are virtual and need to be extended by the various R/DBMS implementations
library(DBI)
library(RSQLite)
# Create temporary in-memory db
con <- dbConnect(RSQLite::SQLite(), ":memory:")
# imports a local data frame or file into the database.
dbWriteTable(con, "mtcars", mtcars, row.names = FALSE) # mtcars is part of base R
dbListTables(con)
sqliteCopyDatabase(con, "mtcars.db")   # save database to datbase file
dbDisconnect(con)

# This package embeds the SQLite database engine in R and
#  provides an interface compliant with the DBI package


# all data frames in the datasets package are bundled with RSQLite; use connection datasetsDb()
dsets <- datasetsDb() 
dbListTables(dsets)

# dbGetQuery is combination of dbSendQuery, dbFetch and dbClearResult
dbGetQuery(dsets, "select * from iris limit 10")


res <- dbSendQuery(dsets, "select * from iris limit 10")  # limits the resultset itself
dbGetRowCount(res)
dbFetch(res, n = 02)  # fetches first 2 rows of resultset, can get more later
dbGetRowCount(res)
dbHasCompleted(res)
dbFetch(res)  #  fetches the rest of the resultset

res <- dbGetPreparedQuery(dsets, "SELECT * FROM USArrests WHERE Murder < ?", data.frame(x = 3)) 
# where murder < 3
head(res) # res us a data  frame

res <- dbSendQuery(dsets, "SELECT * FROM mtcars WHERE cyl = 4")
while(!dbHasCompleted(res)){
  chunk <- dbFetch(res, n = 10) # fetches 10 rows at at time from resultset
  print(nrow(chunk))
}

res <- dbSendQuery(dsets, "SELECT * FROM mtcars WHERE cyl = 4")
dbFetch(res)  # fetches all rows from resultset
dbClearResult(res)
#alt:use dbGetQuery which sends, fetches and  clears for you.

sqliteCopyDatabase(dsets, "datasets.sqlite")   # save database to datbase file
dbDisconnect(dsets) ### moved

#####################################################
con <- dbConnect(RSQLite::SQLite(), "mtcars.db") # existing database file, not flat file
dbListTables(con)
dbReadTable(con, "mtcars")

dbExistsTable(con, "mtcars")
res <- dbSendQuery(con, "select * from mtcars")  
dbFetch(res)  #  fetches the entire resultset
dbClearResult(con)
dbDisconnect(con)


# http://stackoverflow.com/questions/38549/difference-between-inner-and-outer-joins
library(RSQLite)
con <- dbConnect(SQLite(), "inner_outer.sqlite")

a <- data.frame(A=1:4)
b <- data.frame(B=3:6)

# imports a local data frame or file into the database.
dbWriteTable(con, "a", a, row.names = FALSE)
dbWriteTable(con, "b", b)
dbGetQuery(con, "select * from a INNER JOIN b on a.a = b.b;")
dbGetQuery(con, "select a.*,b.*  from a,b where a.a = b.b;")
dbGetQuery(con, "select * from a LEFT OUTER JOIN b on a.a = b.b;")
##everything in the left table and the matching from the right table. 
dbDisconnect(con)


# Sandy Muspratt: Creating SQLite databases from R
# http://sandymuspratt.blogspot.com/2012/11/r-and-sqlite-part-1.html
# Two ways in which R can communicate with SQLite databases: 
#   using the RSQLite package and using the sqldf package. 
# Both packages use reasonably standard versions of SQL to administer and manage the database
#  but they differ in the way meta statements are constructed.

# First, the required packages are loaded. Both RSQLite and sqldf
# (and others too) are loaded by the following command.
library(sqldf)

#setwd("/Users/Pat/Documents/R/HS_616/assign/DB")
db <- dbConnect(SQLite(), dbname="babies.sqlite")
#install.packages("tcltk")
library(tcltk)
# The following sqldf command creates babies.sqlite in R's working directory.
sqldf("attach 'babies.sqlite' as new")

# 1: import data from csv file into data frame, from data frame to table babies
babies2 <- read.csv("babies2.csv", header=T)
names(babies2)[9] <- "age_level"
dbWriteTable(conn = db, name = "babies", value = babies2, row.names = FALSE) # leave out header
dbListFields(db, "babies")         # The columns in a table
dbReadTable(db, "babies")      # The data in a babies table
dbDisconnect(db)

# 2: import data directly from csv file into the table babies
dbWriteTable(conn = db, name = "babies", value = "babies2.csv",
             row.names = FALSE, header = TRUE)
dbListTables(db)                   # The tables in the database
dbListFields(db, "babies")         # The columns in a table
dbReadTable(db, "babies")          # The data in a table
# if the file displays  \r line endings then it was created on a windows machine:
# drop the tables and re-import with eol = "\r\n"
dbRemoveTable(db, "babies")   # Remove the table
dbWriteTable(conn = db, name = "babies", value = "babies2.csv",
             row.names = FALSE, header = TRUE, eol = "\r\n")
dbReadTable(db, "babies")          # The data in a table
dbGetQuery(db, "select age_level from babies")
dbGetQuery(db, "select distinct(age_level) from babies")

#####################################################################
##13_1_2
#install.packages("tidyr")
library(tidyr)
setwd("C:/HS616/LectureCodes")

data_dir <- "FNDDS_2011"

fortification <- c(`0`="none", `1`="fortified_product", `2`="contains fortified ingredients")

fndds_tables <- list(
  AddFoodDesc = list(
    title="Additional Food Descriptions",
    column_types=c(
      food_code="integer", # foreign key
      seq_num="integer", 
      start_date="date", 
      end_date="date", 
      additional_food_description="text"),
    sep="^"
  ),
  FNDDSNutVal = list(
    title="FNDDS Nutrient Values",
    column_types=c(
      food_code="integer",
      nutrient_code="integer",	# Nutrient Descriptions table
      start_date="date", 
      end_date="date", 
      nutrient_value="double"
    ),
    sep="^"
  ),
  FNDDSSRLinks = list(
    title="FNDDS-SR Links",	# see p34 of fndds_2011_2012_doc.pdf
    column_types=c(
      food_code="integer",
      start_date="date", 
      end_date="date", 
      seq_num="integer",
      sr_code="integer",
      sr_descripton="text",
      amount="double",
      measure="char[3]",	# lb, oz, g, mg, cup, Tsp, qt, fluid ounce, etc
      portion_code="integer",
      retention_code="integer",
      flag="integer",
      weight="double",
      change_type_to_sr_code="char[1]",	# D=data change; F=food change
      change_type_to_weight="char[1]",
      change_type_to_retn_code="char[1]"
    ),
    sep="^"
  ),
  FoodPortionDesc = list(
    title="Food Portion Descriptions",
    column_types=c(
      portion_code="integer", 	# foreign key
      start_date="date",
      end_date="date",
      portion_description="text",
      change_type="char[1]"
    ),
    sep="^"
  ),
  FoodSubcodeLinks = list(
    title="Food code-subcode links",
    column_types=c(
      food_code="integer",
      subcode="integer",
      start_date="date",
      end_date="date"
    ),
    sep="^"
  ),
  FoodWeights = list(
    title="Food Weights",
    column_types=c(
      food_code="integer",	# foreign key
      subcode="integer",
      seq_num="integer",
      portion_code="integer",	# food portion description id
      start_date="date",
      end_date="date",
      portion_weight="double",	# missing values = -9
      change_type="char[1]"	# D=data change, F=food change
    ),
    sep="^"
  ),
  MainFoodDesc = list(
    title="Main Food Descriptions",
    column_types=c(
      food_code="integer", 
      start_date="date", 
      end_date="date", 
      main_food_description="character", 
      fortification_id="integer"),
    sep="^"
  ),
  ModDesc = list(
    title="Modifications Descriptons",
    column_types=c(
      modification_code="integer",
      start_date="date", 
      end_date="date", 
      modification_description="text",
      food_code="integer"
      
    ),
    sep="^"
  ),
  ModNutVal = list(
    title="Modifications Nutrient Values",
    column_types=c(
      modification_code="integer",
      nutrient_code="integer",
      start_date="date", 
      end_date="date", 
      nutrient_value="double"
    ),
    sep="^"
  ),
  MoistNFatAdjust = list(
    title="Moisture & Fat Adjustments",	# to account for changes during cooking
    column_types=c(
      food_code="integer",
      start_date="date", 
      end_date="date", 
      moisture_change="double",
      fat_change="double",
      type_of_fat="integer"	# SR code or food code				
    ),
    sep="^"
  ),
  NutDesc = list(
    title="Nutrient Descriptions",
    column_types=c(
      nutrient_code="integer",
      nutrient_description="text",
      tagname="text",
      unit="text",
      decimals="integer"	# decimal places
    ),
    sep="^"
  ),
  SubcodeDesc = list(
    title="Subcode Descriptions",
    column_types=c(
      subcode="integer",	# key; 0=use default gram weights
      start_date="date",
      end_date="date",
      subcode_description="text"
    ),
    sep="^"
  )
)

# flat file to a data frame: call for each table
assign_data_frame <- function(tbl_name){
  tbl <- read.table(
    file.path(data_dir, paste0(tbl_name, ".txt")), 
    sep="^",
    quote="~",
    stringsAsFactors=FALSE)
  # drop last (empty) column
  tbl <- tbl[1:(length(tbl)-1)]
  names(tbl) <- names(fndds_tables[[tbl_name]][["column_types"]])
  assign(tbl_name, tbl, envir = .GlobalEnv)
}

# flat file to database
fndds2sqlite <- function(data_dir, table_details, sqlite_filename){
  
  library("RSQLite")
  con <- dbConnect(SQLite(), sqlite_filename)
  
  for (tbl_name in names(table_details)){
    file_name <- paste0(tbl_name, ".txt")
    assign_data_frame(tbl_name)
    tbl <- get(tbl_name)
    dbWriteTable(con, tbl_name, tbl, row.names = FALSE)
  }
  
  dbDisconnect(con)
}

library(DBI)
fndds2sqlite("FNDDS_2011", fndds_tables, "fndds.sqlite")


for (tbl in c("FNDDSNutVal", "MainFoodDesc", "NutDesc"))
  assign_data_frame(tbl)

library(dplyr)
library(tidyr)

# Make a simplified selection of foods.
# TO DO: have MainFoodDesc be a tbl sourced from SQLite.
get_selected_foods <- function(){
  # Pull out all "Not Further Specified" foods as a wide selection of reasonably generic items.
  generics <- MainFoodDesc %>% 
    filter( grepl(", NFS", main_food_description )) %>%
    filter(!grepl("infant formula", main_food_description, ignore.case = TRUE ) )
  
  # Raw fruits
  # Berries are covered by "Berries, raw, NFS" and "Berries, frozen, NFS"
  fruits <- MainFoodDesc %>% 
    filter( grepl("^6", food_code) ) %>%
    filter( grepl("^([^,\\(]+), raw$", main_food_description) ) %>% 
    filter( !grepl("berries", main_food_description) )
  
  # Raw vegetables
  # Potatoes are covered by "White potato, NFS", "Sweet potato, NFS", etc.
  vegetables <- MainFoodDesc %>% 
    filter( grepl("^7", food_code) ) %>%
    filter(!grepl("potato", main_food_description)) %>%
    filter( grepl(", raw$", main_food_description))
  
  # 4="legumes, nuts, and seeds"
  nuts_and_seeds <- MainFoodDesc %>% 
    filter( grepl("^4", food_code) ) %>%
    mutate( firstWord = strsplit(main_food_description, " ")[[1]][1] )
  
  # Selected alcoholic beverages
  # All alcoholic beverages: grepl("^93", food_code))
  # "Cocktail, NFS" already gives us "Cocktail"
  alcoholic_beverages <- MainFoodDesc %>% 
    filter( main_food_description %in% c("Beer", "Wine, table, red", "Wine, table, white", 
                                         "Whiskey", "Gin", "Rum", "Vodka") )
  
  # Collect them all into one table
  rbind(generics, fruits, vegetables, alcoholic_beverages) %>%
    select( food_code, main_food_description, fortification_id )  %>% 
    filter( nchar(main_food_description) < 20 ) %>%
    mutate( main_food_description = gsub("(, NFS|, raw)", "", main_food_description) ) 
  
}

foods <- get_selected_foods()	# 163 items

library(sqldf)
long_food_nutrients <- sqldf("SELECT f.main_food_description, nd.nutrient_description, nv.nutrient_value 
                             FROM foods f 
                             INNER JOIN FNDDSNutVal nv ON f.food_code = nv.food_code 
                             INNER JOIN NutDesc nd ON nv.nutrient_code = nd.nutrient_code") 

nutrient_food_df <- spread(long_food_nutrients, main_food_description, nutrient_value, fill=0)

food_nutrient_mat <- t(as.matrix(nutrient_food_df[-1]))
colnames(food_nutrient_mat) <- nutrient_food_df$nutrient_description

save(food_nutrient_mat, file="../week_03_linear_algebra/food_nutrient_mat.Rdata")
saveRDS(foods, file="../week_03_linear_algebra/foods.rds")

long_food_nutrients1$categories = 0
for (i in 1:nrow(long_food_nutrients1)){
  if (grep("^1", long_food_nutrients1$food_code[i])>0){
    long_food_nutrients1$categories[i]="dairy"
  }
  else if (grep("^2", long_food_nutrients1$food_code[i])>0){
    long_food_nutrients1$categories[i]="meat,fish"
  }
  else if (grep("^3", long_food_nutrients1$food_code[i])>0){
    long_food_nutrients1$categories[i]="eggs"
  }
  else if (grep("^4", long_food_nutrients1$food_code[i])>0){
    long_food_nutrients1$categories[i]="legumes,nuts,seeds"
  }
  else if (grep("^5", long_food_nutrients1$food_code[i])>0){
    long_food_nutrients1$categories[i]="grains"
  }
  else if (grep("^6", long_food_nutrients1$food_code[i])>0){
    long_food_nutrients1$categories[i]="fruits"
  }
  else if (grep("^7", long_food_nutrients1$food_code[i])>0){
    long_food_nutrients1$categories[i]="vegetables"
  }
  else if (grep("^8", long_food_nutrients1$food_code[i])>0){
    long_food_nutrients1$categories[i]="fats"
  }
  else if (grep("^91", long_food_nutrients1$food_code[i])>0 |grep("^92", long_food_nutrients1$food_code[i])>0){
    long_food_nutrients1$categories[i]="sugars"
  }
  else if (grep("^93", long_food_nutrients1$food_code[i])>0){
    long_food_nutrients1$categories[i]="alcohol"
  }
  else if(grep("^95", long_food_nutrients1$food_code[i])>0){
    long_food_nutrients1$categories[i]="protein powder"
  }
  print(i)
}



food_nutrient_df = mutate(food_nutrient_df, category = ifelse(length(grep("^1", food_nutrient_df$food_code))>0, "dairy", ifelse(length(grep("^2", food_nutrient_df$food_code))>0, "meat,fish", ifelse(length(grep("^3", food_nutrient_df$food_code))>0, "eggs", ifelse(length(grep("^4", food_nutrient_df$food_code))>0, "legumes,nuts,seeds", ifelse(length(grep("^5", food_nutrient_df$food_code))>0,"grains",ifelse(length(grep("^6", food_nutrient_df$food_code))>0, "fruits", ifelse(length(grep("^7", food_nutrient_df$food_code))>0,"vegetables",ifelse(length(grep("^8", food_nutrient_df$food_code))>0,"fats", ifelse(length(grep("^91", food_nutrient_df$food_code))>0|length(grep("^92", food_nutrient_df$food_code))>0,"sugars", ifelse(length(grep("^93", food_nutrient_df$food_code))>0,"alcohol", "protein powder")))))))))))




#################################################
##Last lecture##  


# Data simulation
# Assignment 6 involved data simulation
# Below is information on simulating a boolean variable 
#  taken Tom DeNatale's data simulation project:
#  https://github.com/tdenatale/hs616/blob/master/Final_project.Rmd

logistic <- function(t) 1 / (1 + exp(-t))

vec <- .1* (1:200) - 10
logistic(vec)
plot(vec)
plot(logistic(vec))
plot(logistic(vec))


logistic(-10)
logistic(0.01)
logistic(1)
logistic(10)

set.seed(123)
N<- 100 # out of 100 roll of the dice
age1 <- 20
age2 <- 50
age3 <- 80
# .08 reduces the value more than .19
# so fewer of the uniform random numbers will be below the value
# runif(N) is the default: numbers between 0 and 1
DIAB1 <- runif(N)< .08*logistic((age1-50)/10) 
DIAB1
DIAB2 <- runif(N)< .08*logistic((age2-50)/10) #logistic(0)= .5
DIAB2
DIAB3 <- runif(N)< .08*logistic((age3-50)/10)
DIAB3
CANC1 <- runif(N)< .19*logistic((age1-40)/10) # higher values since subtracting less
CANC1
CANC2 <- runif(N)< .19*logistic((age2-40)/10) # not reduced by as much
CANC2
CANC3 <- runif(N)< .19*logistic((age3-40)/10) # so more runif fall below
CANC3                            # here there are more people with cancer than diabetes


#################################################################
##PCA/Decision Tree/RFE
library (ggplot2)
#install.packages("foreign")
#install.packages("useful")
#install.packages("rpart.plot")
#install.packages("randomForest")
library(foreign)
library (stats)


logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <-  length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}


diab <- read.arff("http://www.cs.usfca.edu/~pfrancislyon/uci-diabetes.arff")
summary(diab)

# 1. Number of times pregnant
# 2. Plasma glucose concentration a 2 hours in an oral glucose tolerance test
# 3. Diastolic blood pressure (mm Hg)
# 4. Triceps skin fold thickness (mm)
# 5. 2-Hour serum insulin (mu U/ml)
# 6. Body mass index (weight in kg/(height in m)^2)
# 7. Diabetes pedigree function
# 8. Age (years)
# 9. Class variable (tested negative or tested positive)

# Correct zeros that should be NA
# Note that zero pregnancies is valid, but the following are not
diab$plas <- ifelse(diab$plas==0,NA, diab$plas)
diab$pres <- ifelse(diab$pres==0,NA, diab$pres)
diab$skin <- ifelse(diab$skin==0,NA, diab$skin)
diab$insu <- ifelse(diab$insu==0,NA, diab$insu)
diab$mass <- ifelse(diab$mass==0,NA, diab$mass)
summary(diab)


# density distributions of each variable with fill color determined by diabetes status
g <- ggplot(data=diab)
g + geom_histogram(aes(x=preg),binwidth=1, color = 5)  

g + geom_histogram(aes(x=preg, fill=class),binwidth=1,position="dodge")  # position="identity" for overlaid
g + geom_histogram(aes(x=plas, fill=class), binwidth=1,position="dodge")
g + geom_histogram(aes(x=pres, fill=class), binwidth=1,position="dodge")

g + geom_histogram(aes(x=skin, fill=class), binwidth=1,position="dodge")
g + geom_histogram(aes(x=insu, fill=class), binwidth=1,position="dodge")
g + geom_histogram(aes(x=mass, fill=class), binwidth=1,position="dodge")

g + geom_density(aes(x=preg, fill=class), alpha=.5)    
g + geom_density(aes(x=plas, fill=class), alpha=.5)   
g + geom_density(aes(x=pres, fill=class), alpha=.5)    
g + geom_density(aes(x=skin, fill=class), alpha=.5)    
g + geom_density(aes(x=insu, fill=class), alpha=.5)   
g + geom_density(aes(x=mass, fill=class), alpha=.5)    
g + geom_density(aes(x=pedi, fill=class), alpha=.5)   
g + geom_density(aes(x=age, fill=class), alpha=.5)    


#5 Fit a regression model of diabetes as a function of the other variables.
fit_all <- glm(class ~ ., family = binomial(), data = diab)
summary(fit_all) #AIC: 362.02, Nagelkerke R^2 0.452
# Null deviance: 498.10  on 391  degrees of freedom <- about half the observations ***
# Residual deviance: 344.02  on 383  degrees of freedom
logisticPseudoR2s(fit_all) 

fit_b<- glm(class ~ preg+ plas + mass + pedi, family = binomial(), data = diab)
summary(fit_b) #AIC: 714.72, Nagelkerke R^2 0.415
# Null deviance: 974.75  on 751  degrees of freedom   
# Residual deviance: 704.72  on 747  degrees of freedom  
logisticPseudoR2s(fit_b) 

#Above models are not camparable. Difference in DF due to missing data

summary(diab)
# For PCA I will use all the attributes. 
#  I don't want to cut ~half of the rows due to NAs, so impute:
# replace  NAs with either medians or means
diab2 <- diab
diab2$plas[is.na(diab2$plas)] <- mean(diab2$plas,na.rm=T)
diab2$pres[is.na(diab2$pres)] <- mean(diab2$pres,na.rm=T)
diab2$skin[is.na(diab2$skin)] <- mean(diab2$skin,na.rm=T)
diab2$insu[is.na(diab2$insu)] <- mean(diab2$insu,na.rm=T)
diab2$mass[is.na(diab2$mass)] <- mean(diab2$mass,na.rm=T)
summary(diab2)


# NB: with imputed data, null DF and deviances are same
#  for all models since they use the same observations
fit_all2 <- glm(class ~ ., family = binomial(), data = diab2)
summary(fit_all2) #AIC: 731.3, Nagelkerke R^2 0.421 
# Null deviance: 993.48  on 767  degrees of freedom
# Residual deviance: 713.30  on 759  degrees of freedom
logisticPseudoR2s(fit_all2)

fit_b2<- glm(class ~ preg+ plas + mass + pedi, family = binomial(), data = diab2)
summary(fit_b2) #AIC: 726.18, Nagelkerke R^2 0.418   ***slightly lower AICs, pseudo R^2s
# Null deviance: 993.48  on 767  degrees of freedom  
# Residual deviance: 716.18  on 763  degrees of freedom  
logisticPseudoR2s(fit_b2)

# test fit_all improvement over fit_b2 model  
anova(fit_b2,fit_all2, test="Chisq") # p-value = 0.5782 
# indicates improved fit of model fit_all over fit_b2 is insignificant

#PCA
#install.packages("psych")
library("psych")
d2_attrib <- diab2[,1:8]

# PCs, no rotation:
# Max number of PCs is the number of dimensions in the data:
fa_all <- principal(d2_attrib, nfactors =8, rotate = "none")
fa_all # difficult to interpret with no rotation
rotationall <- data.frame(fa_all$score, class=diab2[,"class"])
# Often good for modeling with no rotation
logisticModAll <- glm(class ~  PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, data = rotationall, family = "binomial")
summary(logisticModAll) #AIC: 731.3, Nagelkerke R^2 0.421 
logisticPseudoR2s(logisticModAll)

# Removing the insignificant PCs from the model 
logisticMod5<- glm(class ~  PC1 + PC2 + PC3 + PC6 + PC7, data = rotationall, family = "binomial")
summary(logisticMod5) #AIC: 728.5 Nagelkerke R^2 0.417 
logisticPseudoR2s(logisticMod5)


# PCs with varimax rotation, max number of PCs:
fa_all_vm <- principal(d2_attrib, nfactors =8, rotate = "varimax")
fa_all_vm  #each has its own PC: no structure revealed

# Here we use fewer PCs so we can see some structure in the data:
# Variables that are correlated share a PC
fa4_all_vm <- principal(d2_attrib, nfactors =4, rotate = "varimax")
fa4_all_vm  #
fa4_all_vm$scale

# Can predict with the PCs, sometimes get a better result or 
# understanding of the data
#NB: component scores are standard scores (mean=0, sd = 1) of the standardized input
rotation4 <- data.frame(fa4_all_vm$score, class=diab2[,"class"])
logisticMod <- glm(class ~  PC1 + PC2 + PC3 + PC4, data = rotation4, family = "binomial")
summary(logisticMod) #AIC: 782.51, Nagelkerke R^2 0.345 
logisticPseudoR2s(logisticMod)
# Not as good a predictor, but fitted coeficients are revealing 
# PC3 0.98343 (blood sugar),  PC4 0.41118 (pedigree)

# K-means clustering
#install.packages("useful")
library (useful) # for plot.kmeans: attributes reduced to 2 dimensions
# k-means clustering:
# for numeric data only, is susceptible to outliers
clustKM1 <- kmeans(x=diab2[,1:8], centers=2)
plot(clustKM1,data=diab2[,1:8])
plot(clustKM1,data=diab2, class="class")

clustKM2 <- kmeans(x=diab2[,1:8], centers=4)
plot(clustKM2,data=diab2[,1:8])
plot(clustKM2,data=diab2, class="class")

# recommend that you scale before you cluster *********************
# also imp for PCA if using prcomp {stats package} instead of above
scaled_diab2 <- scale(diab2[,1:8])
clustKM1_s <- kmeans(x=scaled_diab2, centers=2)
plot(clustKM1_s, data=scaled_diab2)
plot(clustKM1_s, data=diab2, class="class")


# Hierarchical clustering:
# method = "single", "complete", "average", "centroid"
#  default is "complete"
# Complete defines the cluster distance between two clusters 
#   to be the max distance between their individual components. 
# At every stage of the clustering process, 
# the two nearest clusters are merged into a new cluster. 
#The process is repeated until the whole data set is 
#  agglomerated into one single cluster.
hc1 <- hclust(dist(diab2[,1:8]))
# plot the dendrogram
plot(hc1)

hc1s <- hclust(dist(scaled_diab2))
# plot the dendrogram
plot(hc1s)


# original (not imputed) data
hc2 <- hclust(dist(diab[,1:8]))
plot(hc2)

hc1 <- hclust(dist(diab2[,1:8]), method = "average")
# plot the dendrogram
plot(hc1)

# original (not imputed) data
hc2 <- hclust(dist(diab[,1:8]), method = "average")
plot(hc2)


# decision Trees
library(rpart)
library(rpart.plot)
diab_tree <- rpart(class ~ ., data=diab2)
# text version of the resulting tree:
# great for serialization, difficult to understand
diab_tree 
# extra=1: Display the number of observations that fall in the node
rpart.plot(diab_tree, extra = 1)

# extra=4: Class models: Display probability per class
# of observations in the node (conditioned on the node,
# sum across a node is 1).
rpart.plot(diab_tree, extra = 4)


# RF: random forests
library(randomForest)

diab_rf <- randomForest(class ~ ., data=diab2)
diab_rf

# if the formula above fails, try building individual predictor and response matrices
# build.x and bulid.y require pachage ("Useful") as does plot.kmeans above
formula <- class ~ .
diab2_x <- build.x(formula, data=diab2)
diab2_y <- build.y(formula, data=diab2)

diab_rf2 <- randomForest(x=diab2_x, y=diab2_y )
diab_rf2 # randomness in RF algorithm, so some variation in runs

# 3d plot that can be rotated
#install.packages("rgl")
library(rgl)
plot3d(diab2$mass,diab2$skin, diab2$age,col=as.numeric(diab2$class))


########################################################
##manipulate
#install.packages("manipulate")
library("manipulate")
library("ggplot2")

load("body_dat.rda")

body_dat$wrist_group <- cut(body_dat$wrist_diameter, 
                            quantile(body_dat$wrist_diameter, probs=0:5/5), 
                            include.lowest=TRUE)

body_dat$age_group <- cut(body_dat$age, 
                          quantile(body_dat$age, probs=0:5/5), 
                          include.lowest=TRUE)
manipulate(
  ggplot(data=body_dat, aes_string(x=x_col, y=y_col, col=color)) + geom_point() + geom_smooth(),
  x_col = picker("weight", "knee_diameter"), 
  y_col = picker("wrist_group", "height"), 
  color=picker("gender", "age_group"))



#############################################################
##Shiny
# Bob found this on stackoverflow: http://stackoverflow.com/questions/27072346/r-shiny-dev-on-rstudio-server-shiny-crashes-when-app-launch
# See also: http://shiny.rstudio.com/articles/function.html
# Paste into R terminal session to run shiny app without RStudio.

#install.packages("shiny")
library(shiny)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs), col = 'darkgray', border = 'white')
  })
}

ui <- shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs", "Number of observations:", min = 10, max = 500, value = 100)
    ),
    mainPanel(plotOutput("distPlot"))
  )
))

shinyApp(ui = ui, server = server)


