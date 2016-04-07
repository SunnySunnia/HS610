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
save(babies2, file = "babies2.csv")

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
summary(fit.null) #AIC:

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











