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
dnorm(0)
dnorm(0,10,4)


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

