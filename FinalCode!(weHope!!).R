---
title: "Untitled"
author: "Henry Dworkin,Talia Foley, and Krishna Nayar"
date: "2024-04-21"
output: html_document
---

```{r}

#louad the libreary "data.table" so that I so that we can create a data tables
library("data.table") 
```

```{r}
# We will first create data tabels containing the data we inted to use for testing
dataTest1= data.frame(c(60, 40), c(50, 50))
dataTest2= data.frame(c(70, 30), c(50, 50)) 
dataTest3 = data.frame(c(30, 20), c(75, 75))
dataTest4= data.frame(c(35, 15), c(75, 75))

```
```{r}
#Now for the actual permutation test. 

#Notice that Treatment~Binom(#of treatments, pi1)
# and Control~Binom(# of controls, pi2)

#Ho: pi1=pi2, we will estimate pi1=pi2=success/total

#Ha: pi1 != pi2

#Permuting the data should essentially mean generating a random number of success
# Under the null hypotheses, 
# we can use rbinom to generate random responses 

permTest <- function(x, n){
  
  p<- sum(x[1,])/sum(x) #Proportion of successes
  
  treatmentPermData <- rbinom(n, sum(x[1]), p) #n = # of permutations
  controlPermData <- rbinom(n, sum(x[2]), p)
  
  #Now we need to use each of the permutations to create an estimate of 
  # the proportion of successes for the treatment and control
  #To do this, we will divide the number of successes by the total number 
  #of subjects for each group, and have that be my estimated proportion. 
  treatmentPermProps<-treatmentPermData / sum(x[1])
  controlPermProps<-controlPermData / sum(x[2])
  
  # now to get our sample of test statistics (or our null distribution)
  # we will want to take the differince of the estimated proportion of 
  # successes for the treatment group minus the estimated proportion of 
  # successes for the control group for each of our permutations 
  TestStatistics <- treatmentPermProps-controlPermProps
  
  #Now we will find the test statistic for our actual data. 
  t <- (sum(x[1,1])/sum(x[1])) - (sum(x[1,2])/sum(x[2]))
  
  minus_t = -1*t #Negative of our test statistic, for comparison in a 2-sided test
  count <- 0 #Count variable of responses that contribute to p-value
#For loop checks how our calculated p-values compare to the test statistic - counts up the number of values that exceed it, are less than its negative, or are equivalent to it
  for (i in 1:length(TestStatistics)) {
    if(TestStatistics[i] > t || TestStatistics[i]<minus_t || TestStatistics[i]== t || TestStatistics[i]== minus_t) { 
      count<- count + 1 #
    }
  }
  return(count/n) #Prints our count that meet the above criteria
}

#Runs the function defined above q times.
permTestVec <- function(x,n,q){
  a <- c()
  for(i in 1:q){
    a[i] <- permTest(x,n)
  }
  return(a)
}
```


```{r}

dataTest1= data.frame(c(60, 40), c(50, 50))

dataTest2= data.frame(c(70, 30), c(50, 50)) 

dataTest3 = data.frame(c(30, 20), c(75, 75))

dataTest4= data.frame(c(35, 15), c(75, 75))


b <- c()
for(j in 1:30){
  perms <- 1000
  d <- 1
  while(d >= 0.0025){
    x <-permTestVec(dataTest, perms, 100)
    d <- sd(x)
    if(d>= 0.0025){
      perms <- perms + 250
    }
    else{
      b[j] = perms
      break
    }
  }
}

m<- mean(b)
s <- sd(b)

e <- qt(.975, df = 29)*s/sqrt(30)


m
s
m-e
m+e

#x

#(sum(MainDataSet[1,1])/sum(MainDataSet[1])) - (sum(MainDataSet[1,2])/sum(MainDataSet[2]))

#MainDataSet
```
