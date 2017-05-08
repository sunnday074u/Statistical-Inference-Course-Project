# Statistical-Inference-Course-Project
in-depth understanding of satistics
title: 'Peer Graded Assignment: Statistical Inference Course Project'
author: "Sunday"
date: "May 7, 2017"
output: pdf_document
---


## Instructions
The project consists of two parts:
1.	A simulation exercise.
2.	Basic inferential data analysis.
Part 1: Simulation Exercise Instructions
Overview
In this project you will investigate the exponential distribution in R and compare it with the Central Limit Theorem. The exponential distribution can be simulated in R with rexp(n, lambda) where lambda is the rate parameter. The mean of exponential distribution is 1/lambda and the standard deviation is also 1/lambda. Set lambda = 0.2 for all of the simulations. You will investigate the distribution of averages of 40 exponentials. Note that you will need to do a thousand simulations.
Question 1 : Show the sample mean and compare it to the theoretical mean distribution
```{r }
n <- 40
Simulations <- 1000
Lambda <- 0.2

SampleMean <- NULL
for(i in 1:Simulations) {
  SampleMean <- c(SampleMean, mean(rexp(n, Lambda)))
}
mean(SampleMean)
```

So, as we can see, compared to the theoretical mean distribution of 5, our mean 5 is close.
Question 2: Show the sample is (via variance) and compares it to the theoretical variance of the distribution.
The theoretical standard deviation of the distribution is also 1/lambda , which, for a lambda of 0.2 , equates to 5 . The variance is the square of the standard deviation, which is 25 .
```{r }
Variance <- var(SampleMean)
```
0.6 is close to the theoretical distribution.
Show that the distribution is approximately normal


```{r }
hist(SampleMean, breaks = n, prob = T, col = "blue", xlab = "Means")
x <- seq(min(SampleMean), max(SampleMean), length = 100)
lines(x, dnorm(x, mean = 1/Lambda, sd = (1/Lambda/sqrt(n))), pch = 25, col = "green")
```

```{r }
qqnorm(SampleMean)
qqline(SampleMean, col = "blue")
```

The distribution averages of 40 exponentials is very close to a normal distribution
Part 2: Basic Inferential Data Analysis Instructions
Now in the second portion of the project, we’re going to analyze the ToothGrowth data in the R datasets package.
1.	Load the ToothGrowth data and perform some basic exploratory data analysis
```{r }
library(datasets)
data(ToothGrowth)
library(ggplot2)

str(ToothGrowth)


summary(ToothGrowth)
```

```{r }
ggplot(data=ToothGrowth, aes(x=as.factor(dose), y=len, fill=supp)) +
    geom_bar(stat="identity") +
    facet_grid(. ~ supp) +
    xlab("Dose(mg)") +
    ylab("Tooth length")
```

3. Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose. (Only use the techniques from class, even if there’s other approaches worth considering)
```{r }
hypoth1 <- t.test(len ~ supp, data = ToothGrowth)
hypoth1$conf.int
```
attr(,"conf.level")
```{r }
hypoth1$p.value
```


```{r }
hypoth2<-t.test(len ~ supp, data = subset(ToothGrowth, dose == 0.5))
hypoth2$conf.int
hypoth2$p.value
```

```{r }
hypoth3<-t.test(len ~ supp, data = subset(ToothGrowth, dose == 1))
hypoth3$conf.int
```
attr(,"conf.level")
```{r }
hypoth3$p.value

hypoth4<-t.test(len ~ supp, data = subset(ToothGrowth, dose == 2))
hypoth4$conf.int
```

attr(,"conf.level")
```{r }
hypoth4$p.value
```
Conclusions
OJ ensures more tooth growth than VC for dosages 0.5 & 1.0. OJ and VC gives the same amount of tooth growth for dose amount 2.0 mg/day. For the entire trail we cannot conclude OJ is more effective that VC for all scenarios.


