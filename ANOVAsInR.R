#1. some significant concepts 
#CASES: TREATMENT
#   CBT           EMDR
#   s1             s6
#   s2             s7
#   s3             s8
#   s4             s9
#   s5             s10

#say we are interested in studying the treatment of anxiety. Two therapies are CBT and EMDR
#recruit 10 anxious individuals and randomly assign half of them to receive five weeks of CBT, a half gets five weeks EMDR
#at the conclusion of therapy, each patient is asked to complete the State-Trait Anxiety Inventory (STAI) to report measure of anxiety


#Between-groups factor: patients are assigned to one and only one group, the treatment is a example of between-groups factor with two levels(CBT, EMDR)

#subjects: like patients are the subjects in this case

#dependent variable: STAI is the dependent variable of Treatment(independent variable)

#balanced design: there is an equal number of observations in each treatment condition, when the sample sizes are unequal across the cells of a design, we have an unbalanced design

#one-way ANOVA: the ANOVA with a single classification variable

#one-way between-groups ANOVA

#if we were interested in the effect of CBT on anxiety over time, we could place all 10 patients in the CBT group and assess them ast the conclusion of therapy and again six months later
#        Time
#   5 weeks  6 months
#s1           
#s2             
#s3            
#s4             
#s5  
#s6
#s7
#s8
#s9
#s10

#within-groups factor: patients are measured under both levels, time is a within-groups factor with two levels

#one-way within-groups ANOVA

#repeated measures: each subject is measured more than once

#if we were interested in both treatment differences and change over time, we could combine these two

#by including both therapy and time as fatcors, we are able to examine the impact of Therapy, Time and the interaction of Therapy and Time. The first two are called the main effects, whereas the interaction is called an interaction effect

#when crossing two or more factors, as is done here, we have a factorial ANOVA design. Crossing two factors produces a two-way ANOVA, crossing three factors produces a three-way ANOVA 

#when a factorial design includes both between-groups and within-groups factors, it is also called a mixed-model ANOVA

#confounding factor: like depression often co-occur with the anxiety, could also explain the group differences on the dependent variable

#nuisance variable: the variable that we are not interested in

#when we record depression levels using a self-report depression measure such as the Beck Depression InVentory BIS when patients were recruited
#we could staitiscally adjust for any treatment group differences in depression before assessing the impact of therapy type
#BDI would be called a covariate and the design would be called an analysis of covariance

#when there is more than one dependent variable, the design is called a multivariate analysis of variance(MANOVA)
#if there are covariates present, it is called a multivariate analysis of covariance(MANCOVA)

#2. fitting ANOVA models
lm() #analyze ANOVA models

aov() #presents results in a format that is more familiar to ANOVA methodologists
#~ sepeates response variables on the left from the explanatory variables on the right
#: denotes an interaction between variables
#* denotes the complete crossing variables
#^ denotes crossing to a specified degree
#. denotes all remaining variables. the code y~. expands to y~A+B+C


#one-way anova : y~A
#one-way ancova with 1 covariate: y~ x+A
#two-way factorial anova: y~A+B
#two-way factorial ancova with 2 covariates: Y~ x1+x2+A*B
#randomized block: y~ B +A (where B i a blocking factor)
#one-way within-groups anova : y~ A +Error (Subject/A)
#repeated measures anova with 1 within-groups factor(W) and 1 between-groups factor(B) :y~ B*W +Error(Subject/w)

#3. the order of formula terms
#in a two-way ANOVA with unequal numbers of observations in the treatment combinations, the model y~A*B will not produce the same results as the model y~B*A
#by default, R employs the Type I(sequential) approach to calculating ANOVA effects, the first model can be written as y~A+B+A:B
# y~A+B+A:B will assess: the impact of A on y, the impact of B on y, controlling for A
#the interaction of A and B, controlling for the A and B main effects

Y ~A+B+A:B
#there are three typical approaches for partitioning the variance in y among the effects on the right side of this equation

#type 1(sequential): the effects are adjusted for those that appear earlier in the formula. A is unadjusted, B is adjusted for A. The A:B interaction is adjusted for both A and B

#type 2(hierarchical): effects are adjusted for other effects at the same or lower level. A is adjusted for B. B is adjusted for B. B is adjusted for A. The A:B interaction is adjusted for both A and B

#type 3(marginal): each effect is adjusted for every other effect in the model. like A is adjusted fro B and A:B. B is adjusted for A and A:B and the A:B interaction is adjusted for A and B

#R employes the type 1 approach by default. other programs such as SAS and SPSS employ the Type 3 approach by default

#the greater the imbalance in sample sizes, the greater the impact that the order of the terms will have on the results

#in general, more fundamental effects should be listed earlier in the formula, in particular, covariates should be listed first, followed by main effects and then two-way interactions, three-way interactions and so on

#for main effects, more fundamental variables should be listed first. Thus gender would be listed before treatment.

Anova() #in the car package provides the option of using the type 2 and type 3 approach, rather than the type 1 approach used by the aov() function

help(Anova, package="car")


#4. one-way anova
#comparing the dependent variable means of two or more groups defined by a categorical grouping factor
install.packages("multcomp")
library(multcomp)
attach(cholesterol)
table(trt)

#group means
aggregate(response, by=list(trt), FUN=mean)#drug E produces the greatest cholesterol reduction, whereas 1time produces the least
#group standard deviation
aggregate(response, by=list(trt), FUN=sd)#sd comes relatively constant across the five groups from 2.88 to 3.48

fit <- aov(response ~ trt) #p<.0001, the difference is significant among the five treatment
summary(fit)

install.packages("gplots")
library(gplots)
plotmeans(response ~ trt, xlab="treatment", ylab="response", main="mean plot\nwith 95% CI")#produce a graph of group means and their confidence intervals
detach(cholesterol)

#5. multiple comparisons

#TukeyHSD() provides a test of all pairwise differences between group means

TukeyHSD(fit)

par(las=2)#rotates the axis labels
par(mar=c(5,8,4,2))#increase the left margin area so that the labels fit
plot(TukeyHSD(fit))

#for example, the mean cholesterol reductions for 1time and 2times arent significantly different from each other(p=.138)
#confidential intervals that include 0 indicate treatments that arent significantly different
install.packages("multcomp") #glht() function in the multcomp package provides a much more comprehensive set of methods for multiple mean comparisons that we can use for both linear models and generalized linear models
library(multcomp)
par(mar=c(5,4,6,2))
tuk <-glht(fit, linfct=mcp(trt="Tukey"))
plot(cld(tuk, level=.05), col="lightgrey")

#par statement increases the top margin to fit the letter array
#the level option in the cld() function proves the significance level to use  
#groups that have the same letter dont have significantly different means, for excample 1time and 2times


#Multiple comparisons methodology is a complex and rapidly changing area of study. lean more see Bretz, Hothorn and Westfall(2010)

#6. Assessing test assumptions

#Q-Q plot to assess the normality assumption
library(car)
qqPlot(lm(response~ trt, data=cholesterol), 
       simulate=T, main="Q-Q Plot", labels=F)
#note that qqPlot() requires an lm() fit
#it shows that the normality assumption has been met fairly well

#tests for the equality(homogeneity) of variances
#one of the tests is Barlett's test 
bartlett.test(response ~ trt, data=cholesterol)
#BArtlett's test indicates that the variances in the five groups dont differ significantly(p= .97)

#other possible tests include the Fligner-Killeen test which provded by the fligner.test() function and the Brown-Forsythe test which provided by the hov() function in the HH package, they reach the same conclusion

#test for outliers using the outlierTest() function in the car package
library(car)
outlierTest(fit)

#there is no indication of outliers in the cholesterol data(NA occurs when p >1)


#7. one-way ANCOVA
#example comes from the litter dataset in the multcomppackage

data(litter, package="multcomp")
attach(litter)
table(dose)#there is an unequal number of litters at each dosage level
aggregate(weight, by=list(dose), FUN=mean)#the no-drug group had the higest mean litter weight(32.3)
fit <-aov(weight~gesttime + dose)#gestation time was related to birth weight, and drug dosage wa related to birth weight after controlling for gestation time. the mean birth weight isnt the same for each of the drug dosages, after controlling for gestation time
summary(fit)

#obtaing group means after partialing out the effects of the covariate
library(effects)
effect("dose",fit)

#in this case, the adjusted means are similar to the unadjusted means produced by the aggregate()
#but this wont always be the case
#the effects pakcage provides a powerful method of obtaining adjusted means for complex research designs and presenting them visually

#use the multiple comparison procedures provided by the multcomp package can be used to test sepcific user-defined hypotheses about the means


#suppose that we are interested in whether the no-drug condition differs from the three drug condition
library(multcomp)
contrast <-rbind("no drug vs. drug" = c(3, -1,-1,-1))#specifies a comparison of the first group with the average of the other three
summary(glht(fit, linfct=mcp(dose=contrast)))
#the hypothesis is tested with a t statistic 25.581 in this case which is significant at the p<.05 level
#we can ocnclude that the no-drug group has a higher birth weight than drug conditions
#other contrasts can be added to the rbind() function

#8. ANCOVA assessing test assupmtion
#testing for homogeneity of regression slopes

#a significant interaction would imply that the relationship between gestation and birth weight depends on the level of the dose variable
library(multcomp)
fit2 <- aov(weight ~ gesttime*dose, data=litter)
summary(fit2)
#the interaction is nonsignificant, supporting the assumption of equality of slopes
#if the assumption is untenable, we could try transforming the covariate or dependent vairbalem using a model that accounts for separate slopes or employing a nonparamtric ANCOVA  method tha tdeosnt require homogeneity or regression slopes
#see the sm.ancova() funtion in the sm package 

#9. visualizing the result
#ancova() function in the HH package provides a lot of the relationship between the dependent vairbale, the covariate and the factor
install.packages("HH")
library(HH)
ancova(weight ~ gesttime + dose, data=litter)
#predicting birth weight from gestation time are parallel in each group but different intercepts
#zero-dose group has the alrgest intercept and five-dose group has thelowest 
#the lines are parallel because they have been specified to be
#if we use the statement ancova(wight ~ gesttime*dose), we can generate a plot that allows both slopes and intercepts to vary by group. 
#this apparoach is very useful for visualizing the case where the homogeneity of regression slopes doesnt hold

#two-way factorial ANOVA 
attach(ToothGrowth)
table(supp, dose) #we have a balanced design(equal sample sizes in each cell of design) 
aggregate(len, by=list(supp, dose), FUN=mean)#cell means
aggregate(len, by=list(supp, dose), FUN=sd)#cell sd
dose <-factor(dose)#covert to a factor so that the aov() function will treat it as a grouping variable rather than a numeric covariate
fit <-aov(len~ supp*dose)#both main effects and the interaction between the factors are significant
summary(fit)
detach(ToothGrowth)


#use the interaction.plot() to display the interaction in a two-way ANOVA
attach(ToothGrowth)
interaction.plot(dose, supp, len, type="b",
                 col=c("red","blue"), pch=c(16,18),
                 main ="Interaction between Dose and Supplement Type")
detach(ToothGrowth)
#the resulting plot is presented of  the mean tooth lenth for each supplement at each dosage

#with a little finesse, we can get an interaction plot out of the plotmeans() function in the gpots package

library(gplots)
plotmeans(len ~ interaction(supp, dose, sep = ""),
          connect=list(c(1,3,5),c(2,4,6),
                       col=c("red","darkgreen"), 
                       main = "Interaction Plot with 95% CIs",
                       xlab="Treatment and Dose Combination"))

#finally we can use the interaction2wt() funtion in the HH package to produce a plot of both main effects and two-way interactions for any factorial design of any order
library(HH)
interaction2wt(len~supp*dose)
#all three graphs indicate that tooth growth increases with the dose of asscorbic acid for both orange juice and Vitamin C
#it provides both the main effects and the two-way interactions for designs of any complexity(two-way ANOVA, three-way Anova and so on)
#the order here is balanced so that we dont have to worry about the order of effects

#10. repeated measures ANOVA, more than one dependent variable, we can test simultaneously using a multivariate analysis of vairance(MANOVA)
library(MASS)
attach(UScereal)
shelf <-factor(shelf)#convert to a factor so that it can represent a grouping variable in the analysis
y<-cbind(calories, fat, sugars)#form a matrix of the three dependent variable
aggregate(y, by=list(shelf), FUN=mean)#mean
cov(y)#covariance
fit <- manova(y~ shelf)#provide the multivariate test of group differences 
summary(fit)
summary.aov(fit)
#F test shows that three groups differ on each nutritional measure considered separately
#because the multivariate test is significant, we can use the summary.aov() function to obtain the univariate one-way ANOVAs
#here we an know the three groups differ on each nutritional measure considered separately
#then use a mean comparison procedure to determine which shelves differ from each other for each of the three dependent variables

#11. assessing test assumptions
#QQ plot for assessing normality

center <- colMeans(y)
n<- nrow(y)
p<-ncol(y)
cov <-cov(y)
d<- mahalanobis(y, center, cov)
coord <-qqplot(qchisq(ppoints(n), df=p),
               d, main="Q-Q Plot Assessing Multivariate Normality",
               ylab="Mahalanobis D2")
abline(a=0, b=1)
identify(coord$x, coord$y, labels=row.names(UScereal))
#if the data follow a multivariate distribution, then points will fall on the line

#covariance matrix for homogeneity
#with a Box's M test
#the test is sensitive to violations of normality, leading to rejection in most typical cases.
#this means that we dont yet have a good working method for evaluating this important assumption(but see Anderson [2006] and Silva et al. [2008])

#multivariate outliers using the aq.plot() function in the mvoutlier package
install.packages("mvoutlier")
library(mvoutlier)
outliers <- aq.plot(y)
outliers

#12. robust manova
#if the assumptions of multivariate normality or homogeneity of variance-covariance matrices are untenable, or if we are concerned about multivariate outliers
#we may use a robust or nonparametric version of the MANOVA test instead

#a robust version is provided by the Wilks.test() function in the rrcov package
#the adonis() fucntion in the vegan package can provide the equivalent of a nonparametric MANOVA 

library(rrcov)
Wilks.test(y, shelf, method="mcd")
#a robust test that is insensitive to both outliers and violations of MANOVA  assumptions 
#still indicates that the cereals on the top, middle and bottom store shelves differ in their nutritional profiles

#13. ANOVA as regression
#understand how R deal with categorical variables when fitting models

library(multcomp)
levels(cholesterol$trt)
fit.aov <-aov(response ~ trt, data=cholesterol)
summary(fit.aov)

fit.lm <-lm(response~trt, data=cholesterol)
summary(fit.lm)
#because linear models require numeric predictors, when the lm() function encounters a factor, it replaces that factor with a set
#a set of numeric variables representing contrats among the levels 
#if the factor has k levels, k-1 contrast variables are created


#14. five built-in methods for creating the contrast variables
contr.helmert() #contrast the second level with the first, the third with the average of the first two, the fourth level with the average of the first three, and so forth
contr.poly()#contrasts are used for trend analysis(linear, quadratic, cubic and so on)based on orthogonal polymials. use for ordered factors with equally spaced levels
contr.sum()#contrast are constrained to sum to zero. also called deviation contrasts. it compares the mean of each level to the overall mean across levels
contr.treatment()#contrasts each level with the baseline level(first level by default) also called dummy coding
contr.SAS()#similar to contr.treatment. but the baseline level is the last level. this produces coefficients similar to contrasts used in most SAS procedures


#with treatment contrasts, the first level of the factor becomes the reference group, and each subsequent level is compared with it
contrasts(cholesterol$trt)

#if a patient is in the drugD condition, then the variable drugD equals 1 and the variables 2times, 4times and drugE each equal zero


#we can change the default conrasts used in lm() by specifying a contrasts option
fit.lm <-lm(response ~trt, data=cholesterol, contrasts="contr.helmert")
summary(fit.lm)

#we also can change teh default contrasts used in lm() by specifying a contrasts option
options(contrasts=c("contr.SAS", "contr.helmert"))

