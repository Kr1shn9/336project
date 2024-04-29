#in this file I hope to do a simple permutation test on some made up data just 
# to hopefully work through the mechanics of it. 

#louad the libreary "data.table" so that I will be able to create a data table
library("data.table") 

# create a data table using some noncese data.
# this data frame has to collomes and two rows representing two groups and 
# two results, repsectivly. We can think of the two groups as treatment and 
# control and the two results as success and failure.
dataTest= data.table(c(57, 33), c(101, 90))

# add row and colomn names to dataTest. Even though it isn't based on 
# anthing I am naming the columns treatment and control and the 
# rows sucess and failure to that it will hopfully make it easer to think 
# through what I am doing. 
colnames(dataTest) = c('treatment', 'control')
rownames(dataTest) <- c('succsess', 'failure')

#call dataTest to make sure everything worked. 
dataTest
# treatment control
# 1:        57     101
# 2:        33      90

#okay, so the "success" and "failure " labels didn't seem to happen.
# no idea why and not worth it to deal with now, but consider the 
# first row success and the second row failure. 

#Now for the actual permutation test. 

#Notice that Treatment~Binom(57+33=90, pi1)
# and Control~Binom(101+90=191, pi2)

#Ho: pi1=pi2, we will estimate pi1=pi2=success/total
( 57 + 101)/(57 + 101+33+90) # 0.5622776 aprox. 0.56
#there must be a more automated way to do that, but whatever for now

#Ha: pi1 != pi2

#Permuting the data should esentaly mean generating a random number of success
# under the null hypothoses 
# we can use rbinom to generate random responces 
# will do 30 permutations 
treatmentPermData <- rbinom(30, 90, 0.56)
controlPermData <- rbinom(30, 191, 0.56)

#Now we need to use each of the permutations to create an estimate of 
# the proportion of sucesses for the treatment and control
#I'm not sure if this is how I should do it, but I am just going to
# devide the number of successes by the total number of subjects
# for each group, and have that be my estimated proportion. 
treatmentPermProps<-treatmentPermData / 90
controlPermProps<-controlPermData / 191

# now to get our sample of test statistics (or our null distrabultion)
# we will want to take the differince of the estimated proportion of 
# sucesses for the treatment group minus the estimated proportion of 
# sucesses for the control group for each of our permutations 
TestStatistics <- treatmentPermProps-controlPermProps

#call test TestStatistics just to make sure everthing worked out. 
#It seems resonable. 
TestStatistics

#Now I will find the test statistic for our actual data. 
(57/90) - (101/90) #-0.4888889 aprox. -0.49

#Now I need to find a way to test if that is unusaul for the null distribultion 
# I have created. (just looking, we can see that it is, but still)

#I'm note sure if this is what I'm supposed to do but I'm going to estimate
# the mean and standered deviation just by using the mean and sd of the 
# sample.
mean(TestStatistics) #-0.01166958
sd(TestStatistics) #0.06674174

pnorm(-0.49, -0.01166958, 0.06674174) #3.836153e-13

#so, this p-value seems to indicate that the true proportions 
# are not equal. Of course, this is only the begining, bucause that is not 
# what we are actully trying to test. What we really want to do is do this 
# a ton more times, get a ton of p-values and anilize the distribution of 
# p-values. 
