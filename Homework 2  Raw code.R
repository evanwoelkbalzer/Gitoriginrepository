a) State the null and alternative hypotheses?
  b) Create a histogram of the data.
c) Explain what type of test should be used to test the hypothesis.
d) Run the appropriate test, and display the summary output.
e) Explicitly state the critical and observed value of the test statistic. 
f) Indicatewhatconclusionyouwoulddrawfromthehypothesistest.


#Question 1#
a)
sc <- read_csv("shrimp_catch.csv")
sc

#histogram of 2009 with fancy breaks#
a<- ggplot(sc, aes(`Two thousand nine`))
brx <- pretty(range(sc$`Two thousand nine`), n = nclass.Sturges(sc$`Two thousand nine`),min.n = 1)
NINE <- a+geom_histogram(breaks = brx, alpha = 0.5, color = "black")+labs(x = "Catches (tonnes)", y = "Count")+theme_classic()
NINE

#2009 histogram with base r#
ninehist <- hist(sc$`Two thousand nine`)
ninehist

#2009 with basic ggplot commands#
a<- ggplot(sc, aes(`Two thousand nine`))
NEIN <- a+geom_histogram(binwidth = 20000)
NEIN

#getting a vector of catch data#
nin <- sc$`Two thousand nine`
nin

tin <- sc$`Two thousand ten`
tin

#making a paired t test#
PairedTCatches <- t.test(nin, tin, paired = TRUE, alternative = "g")
PairedTCatches

#getting the same breaks as given by the standard hist command#
brx <- pretty(range(sc$`Two thousand nine`), n = nclass.Sturges(sc$`Two thousand nine`),min.n = 1)


#QUESTION 2#
#2a
H0: The mean phosphorous level in Lake Erie is not different than 15μg/L
HA: The mean phosphorous level in Lake Eerie is greater than 15μg/L

#2b
EeriePhos <- read_csv("EeriePhos.csv")
EP <- EeriePhos[1:10,1:2]
EP

b<- ggplot(EP, aes(EP$Phosphorous))
epbrx <- pretty(range(EP$Phosphorous), n = nclass.Sturges(EP$Phosphorous),min.n = 1)
ephist <- b+geom_histogram(breaks = epbrx, alpha = 0.5, color = "black")+labs(x = "Phosphorous Level (15μg/L)", y = "Count")+theme_classic()
ephist

#2c
We only have data from one set of observations and wish to compare an observed mean to a theoretical one.
These conditions meet the criteria for a one-sample t-test. Since we want to know whether the mean is higher
than the theoretical mean and have evidence to support that prediction, the test should also be one-sided.

#2d
eptest <- t.test(EP$Phosphorous, mu = 15)
eptest

#2e
qt(0.95, 9)

tcrit = 1.833113
tobs = 1.8462

#2f
Since tobs > tcrit, we reject the null hypothesis. There is evidence that the observed mean
phosphorous level (17.3415μg/L) in Lake Erie is significantly higher than the acceptable level (15μg/L).

#####################QUESTION 3#################################
#3a
H0: There is no difference between the growth rates of salmon with vegetarian and carnivorous diets.
HA: There is a difference between the growth rates of salmon with vegetarian and carnivorous diets

#3b
```{r Question 2 b}
SalmonDiet <- read_csv("SalmonDiet.csv")
d <- ggplot(SalmonDiet, aes(SalmonDiet$Carnivorous))
SDCbrx <- pretty(range(SalmonDiet$Carnivorous), n = nclass.Sturges(SalmonDiet$Carnivorous),min.n = 1)
SDChist <- d+geom_histogram(breaks = SDCbrx, alpha = 0.5, color = "black")+labs(x = "Growth Rate (% body weight/day)", y = "Count")+theme_classic()
SDChist
```

```{r Question 2 b}
e <- ggplot(SalmonDiet, aes(SalmonDiet$Vegetarian))
SDVbrx<- pretty(range(SalmonDiet$Vegetarian), n = nclass.Sturges(SalmonDiet$Vegetarian),min.n = 1)
SDVhist <- e+geom_histogram(breaks = sdvbrx, alpha = 0.5, color = "black")+labs(x = "Growth Rate (% body weight/day)", y = "Count")+theme_classic()
SDVhist
```

#3c
Because the samples were not taken from the same population across the trials, these data are not paired. Since we are interested in whether there is a difference between the two groups, the appropriate test is an independent two-sample t-test.

#3d
```{r Question 3 d}
sdtest <- t.test(SalmonDiet$Carnivorous, SalmonDiet$Vegetarian)
sdtest
```

#2e
```{r Question 3 e}
qt(0.95, 27.862)
```
tcrit = 1.701419
tobs = 12.237

#2f
Since tcrit >>> t obs, we reject the null hypothesis. There is evidence that the growth rate of salmon on vegetarian and carnivorous diets have different growth rates.




##################QUESTION 4###############
#4a
H0: Trophic level and PCB poison symptoms are independent
HA: Trophic level and PCB poison symptoms are not independent
  
#4b
```{r Question 4 b}
trophicPCB <- read_csv("PCB.csv")
trophicPCB
f <- ggplot(trophicPCB, aes(Outcome, Total, fill = Group ))
tPCBbar <- f + geom_bar(stat = "identity", position = "dodge")+theme_classic()
tPCBbar
```

#4c
We want to know whether categorical variables (PCB poison symptoms and trophic level) are independent from one another. Accordingly, the appropriate test is a chi-square test of independence.

#4d


```{r Question 4 d}
first_trophic <- c(133, 100, 17)
top_predator <- c(4, 61, 52)
prechiPCB <- data.frame('FT'=first_trophic,'TP'= top_predator)
chisq.test(prechiPCB)
```

#4e
```{r Question 4 e}
qchisq(0.95, 2)
```
X2obs = 115.66
X2crit = 5.991465

#4f
Since X2obs > X2crit, we reject the null hypothesis. We have evidence that PCB poison symptoms and trophic level of organisms in the St. Lawrence River are not independent.

