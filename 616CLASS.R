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

