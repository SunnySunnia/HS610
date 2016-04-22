**Know what's in the quizzes and assignments:**  
big picture of statistics  
significance  
p-values:likelihood that the current result from the data appears  
confidence interval: repeat the method for 100 times 1-alpha change of the time will have the result within the CI.  

**Types of data:**  
Dichotomous (Binary), Categorical(Nominal), Ordinal(categorical data with ordered relationship, cannot devide the effect--interval data), Discrete(integer data), Continuous  

**Plots:**  
plots of theoretical distributions vs smooth density distribution(own sample)  
plot of dnorm is theoretical  
geom_density is smooth density estimate    

    /Histograms and density plots/  
    g <- ggplot(babies3, aes(x=birthweight, fill=age.level))  
    
    /Overlaid histograms with position="identity"/  
    g + geom_histogram(binwidth=10, alpha=.5, position="identity")  

    /Interleaved histograms with position="dodge"/  
    g + geom_histogram(binwidth=10, position="dodge")  

    /Density is used compare distributions with very different counts/  
    g + geom_density()  
    
    /Density plots with semi-transparent fill/  
    g + geom_density(alpha=.4)  
    
  `qqnorm(rnorm(1000,10,4)); qqline(rnorm(1000,10,4), col = 2,lwd=2)`  
  
when we dont have equal set to compare--use density plots(pdf), easier to compare   
~~kernel density: density(x, ...), not theoretical~~  

**Data Structures**  
vectors, dataframes  
matrices, lists   
Reading Data into R  
Statistical Graphics   
understanding R functions  
Writing R Functions   
Control Statements  
  if, ifelse(vectors, for cleaning data), loops   
dplyr - Introduction-HD 720p.mov
    select (return new df that is subset of **columns**: like select in SQL)  
    `select(msleep, starts_with("sl"))`   
    `distinct(select(msleep, order))`  
    filter (return  new df that is subset of **rows** that meet logical condition: like where clause)  
    `filter(msleep,sleep_total >=12 & sleep_total<=15)`  
    `msleep %>% filter(sleep_total <12 | awake >12) %>% select(sleep_total, awake) %>% head` 
    arrange: (return new df that has reordered rows: like  orderby, can use desc)  
    `msleep%>% select(name, order, sleep_total) %>% arrange(order, sleep_total)`
    rename:  (return new df that with variable(s) renamed) 
    mutate: (return  new df with transformed or new variables)  
    `msleep <- mutate(msleep,rem_proportion=sleep_rem/sleep_total)`  
    summarize values: 
    `msleep %>% summarize(avg_sleep=mean(sleep_total))`
    group_by: takes an existing tbl and converts it into a grouped tbl
    ```
    msleep %>% group_by(order) %>% summarize(avg_sleep=mean(sleep_total),
                                         min_Sleep=min(sleep_total),
                                         max_sleep=max(sleep_total),
                                         std_sleep=sd(sleep_total),
                                         total = n())
    ```
    %in%: whether a set is a subset of another set  
    ```
    > 1:6 %in% 0:36
    [1] TRUE TRUE TRUE TRUE TRUE TRUE
    ```
 
Data Reshaping with melt: (look at assignments)  
    --> variable, value  

**Manipulating Strings:**   
 sprintf
 ```
 > sprintf("%s is %f and %s", "Now", 5, "a half")
  [1] "Now is 5.000000 and a half"
 ```  
 rep (predict output)  
 ```
  > rep('hello',3)
  [1] "hello" "hello" "hello"
 ```
 Simulation - Generating Random Numbers-HD 720p.mov  
   
**Distributions:**  

     uniform  
        - dunif(x, min = 0, max = 1, log = FALSE)  
        - punif(q, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)  
        - qunif(p, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)  
        - runif(n, min = 0, max = 1)  
        
     normal  
        - dnorm(x, mean = 0, sd = 1, log = FALSE)  
        - pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)  
        - qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)  
        - rnorm(n, mean = 0, sd = 1)  
        
     poisson  
        - dpois(x, lambda, log = FALSE)  
        - ppois(q, lambda, lower.tail = TRUE, log.p = FALSE)  
        - qpois(p, lambda, lower.tail = TRUE, log.p = FALSE)  
        - rpois(n, lambda)
        
     bernoulle (sp?)  
        - dbinom(x, size, prob, log = FALSE)  
        - pbinom(q, size, prob, lower.tail = TRUE, log.p = FALSE)  
        - qbinom(p, size, prob, lower.tail = TRUE, log.p = FALSE)  
        - rbinom(n, size, prob)  

**Basic Statistics:**  
Variables:  
Predictor, Response Variables  
Correlated, Confounding variables(people have critical conditions tend to get vaccines, causing people had vaccine to have longer LOS  ****************  
`pcor()` screens out  

**Descriptive Statistics:**   
  Statistics that describe a *dataset rather than infer information about a related *population  
     central tendency: mean, median, mode*  
     spread: variance, standard deviation  
     dispersion: range, quartiles  


**Inferential Statistics:**   
  Statistics that infer information about a population from which a sample is drawn  
     population estimates  
        normality test: `shapiro.test()`  
        t-test     
        paired t-test: when the two samples are relating to each other, same leangths  
     hypothesis testing:  
        p-values  
        confidence intervals  
 
**Regression Models:**   
When is it appropriate to use:  
**Linear Regression**  
      Response/outcome variable is numeric  
      `y_hat = = beta_0 + beta_1* X1 + beta_2* X2`
   fitted coeficients, p-values  
      will try to eliminate significant predictors  
   equation of fit  
   meaning of equation  
   R^2, residual plots  
   anova: measure eof goodness of fit  
   
**Logistic Regression**  
      Response/outcome variable is categorical  
      `log (p(positive)/p(negative) ) = beta_0 + beta_1* X1 + beta_2* X2` 
   fitted coeficients, p-values  
   equation  of fit  
   meaning of equation  
   AIC, Residual error, model error  
     

~~SQL~~  

**Codes**  
[R codes](R_Cheat_Data.pdf)  

