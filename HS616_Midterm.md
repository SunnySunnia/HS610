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
 select, %in%, **************************  
Data Reshaping with melt: (look at assignments)  


**Manipulating Strings:**   
 sprintf, rep (predict output)  
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


    

 
