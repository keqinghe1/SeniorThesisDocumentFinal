#This document contains all the code used in the Data Analysis Section of the Thesis
#Please download the Senior Thesis Document to your Desktop, so that this R code could access the .txt files 
#for analysis


#The following code is for the Analysis Section for the Multinomial Logit Model Section 4.1
closeAllConnections()
rm(list=ls())

setwd("/Users/keqinghe/Desktop/Senior Thesis Document/morpho_Data")
morphoData=read.table("morphoMultiNomialSecondDraft.txt", header=TRUE)
attach(morphoData)
names(morphoData)
Cf = as.factor(C)
library(stats4)
library(splines)
library(VGAM)

#Maineffect Model
modelmaineffect=vglm(Cf~X1+X2+X3,family=multinomial)
summary(modelmaineffect)

nullModel = vglm(formula = Cf ~ 1, family = multinomial)
summary(nullModel)

modelmaineffectMissX3=vglm(Cf~X1+X2,family=multinomial)
summary(modelmaineffectMissX3)

modelmaineffectMissX2=vglm(Cf~X1+X3,family=multinomial)
summary(modelmaineffectMissX2)

modelmaineffectMissX1=vglm(Cf~X2+X3,family=multinomial)
summary(modelmaineffectMissX1)

modelX3=vglm(Cf~X3,family=multinomial)

modelX2=vglm(Cf~X2,family=multinomial)


#The NULL model is the null
1-pchisq( deviance(nullModel) - deviance(modelmaineffect), (382-376))

#The Maineffect model is the null
1-pchisq( deviance(modelmaineffectMissX3) - deviance(modelmaineffect), (378-376))

1-pchisq( deviance(modelmaineffectMissX1) - deviance(modelmaineffect), (378-376))

1-pchisq( deviance(modelmaineffectMissX2) - deviance(modelmaineffect), (378-376))

#The X_2 + X_3 model is the null
1-pchisq( deviance(modelX2) - deviance(modelmaineffectMissX1), 2)

1-pchisq( deviance(modelX3) - deviance(modelmaineffectMissX1), 2)

#Obtain the AIC indices
AIC(modelmaineffectMissX1)
AIC(modelmaineffectMissX3)
AIC(modelmaineffectMissX2)
AIC(modelmaineffect)
AIC(nullModel)
AIC(modelX2)
AIC(modelX3)

#Analysis of the probability functions \pi_red, \pi_yellow, and \pi_clear with graphs

x_2 = seq(0.12,2.91,0.01) #range of x_2 value
x_3 = seq(0.03,0.81,0.01) #range of x_3 value
pi_red = function(x_2, x_3)((exp(1.7353-0.5767*x_2 -2.0062*x_3))/(1+exp(-4.0029  -1.2694 *x_2 +5.8773 *x_3)+exp(1.7353-0.5767*x_2 -2.0062*x_3))) #the probability function derived from the multinomial logit model

pi_yellow = function(x_2, x_3)((1)/(1+exp(-4.0029  -1.2694 *x_2 +5.8773 *x_3)+exp(1.7353-0.5767*x_2 -2.0062*x_3)))

pi_clear = function(x_2, x_3)((exp(-4.0029  -1.2694* x_2 +5.8773 *x_3))/(1+exp(-4.0029  -1.2694 *x_2 +5.8773 *x_3)+exp(1.7353-0.5767*x_2 -2.0062*x_3)))

prob_red = outer(x_2,x_3,pi_red) #a line of code to bring the variables and the function together for "persp" graph generation
prob_yellow = outer(x_2,x_3,pi_yellow)
prob_clear = outer(x_2,x_3,pi_clear)

persp(x_2, x_3, prob_clear) # generate the graphs
persp(x_2, x_3, prob_red)
persp(x_2, x_3, prob_yellow)

#Analyze the probility function of \pi_clear
#when x2 = 0.15M, if we increase x3 from 0.5 to 0.6, the probability of getting clear- colored outcomes increases from 0.08932628 to 0.1669813 
> (exp(-4.0029  -1.2694* 0.15 +5.8773 *0.5))/(1+exp(-4.0029  -1.2694 *0.15 +5.8773 *0.5)+exp(1.7353-0.5767*0.15 -2.0062*0.5))
[1] 0.08932628
> (exp(-4.0029  -1.2694* 0.15 +5.8773 *0.6))/(1+exp(-4.0029  -1.2694 *0.15 +5.8773 *0.6)+exp(1.7353-0.5767*0.15 -2.0062*0.6))
[1] 0.1669813
> 0.1669813-0.08932628
[1] 0.07765502

#when x2 = 1.5M, if we again increase x3 from 0.5 to 0.6, the probability of getting clear-colored outcomes increases from 0.02666759 to 0.05113155. 
> (exp(-4.0029  -1.2694* 1.5 +5.8773 *0.5))/(1+exp(-4.0029  -1.2694 *1.5 +5.8773 *0.5)+exp(1.7353-0.5767*1.5 -2.0062*0.5))
[1] 0.02666759
> (exp(-4.0029  -1.2694* 1.5 +5.8773 *0.6))/(1+exp(-4.0029  -1.2694 *1.5 +5.8773 *0.6)+exp(1.7353-0.5767*1.5 -2.0062*0.6))
[1] 0.05113155
> 0.05113155-0.02666759
[1] 0.02446396

#Analyze the probility function of \pi_yellow
#when x2 = 0.1M, if we increase x3 from 0.5 to 0.6, the probability of getting yellow- colored outcomes decreases from 0.3060941 to 0.3171403 
 (1)/(1+exp(-4.0029  -1.2694 *0.1 +5.8773 *0.5)+exp(1.7353-0.5767*0.1 -2.0062*0.5))
[1] 0.3060941
> (1)/(1+exp(-4.0029  -1.2694 *0.1 +5.8773 *0.6)+exp(1.7353-0.5767*0.1 -2.0062*0.6))
[1] 0.3171403

#when x2 = 1.5M, if we again increase x3 from 0.5 to 0.6, the probability of getting yellow- colored outcomes decreases from 0.5189455 to 0.552814 
> (1)/(1+exp(-4.0029  -1.2694 *1.5 +5.8773 *0.5)+exp(1.7353-0.5767*1.5 -2.0062*0.5))
[1] 0.5189455
> (1)/(1+exp(-4.0029  -1.2694 *1.5 +5.8773 *0.6)+exp(1.7353-0.5767*1.5 -2.0062*0.6))
[1] 0.552814
> 0.3171403-0.3060941
[1] 0.0110462
> 0.552814-0.5189455
[1] 0.0338685

#Analyze the probility function of \pi_red
#when x2 = 0.1M, if we increase x3 from 0.5 to 0.6, the probability of getting red-colored outcomes decreases from 0.6008964 to 0.5094106 
(exp(1.7353-0.5767*0.1 -2.0062*0.5))/(1+exp(-4.0029  -1.2694 *0.1 +5.8773 *0.5)+exp(1.7353-0.5767*0.1 -2.0062*0.5))
[1] 0.6008964
> (exp(1.7353-0.5767*0.1 -2.0062*0.6))/(1+exp(-4.0029  -1.2694 *0.1 +5.8773 *0.6)+exp(1.7353-0.5767*0.1 -2.0062*0.6))
[1] 0.5094106

#when x2 = 1.5M, if we again increase x3 from 0.5 to 0.6, the probability of getting red-colored outcomes decreases from 0.4543869 to 0.3960544 
> (exp(1.7353-0.5767*1.5 -2.0062*0.5))/(1+exp(-4.0029  -1.2694 *1.5 +5.8773 *0.5)+exp(1.7353-0.5767*1.5 -2.0062*0.5))
[1] 0.4543869
> (exp(1.7353-0.5767*1.5 -2.0062*0.6))/(1+exp(-4.0029  -1.2694 *1.5 +5.8773 *0.6)+exp(1.7353-0.5767*1.5 -2.0062*0.6))
[1] 0.3960544
> 0.4543869-0.3960544
[1] 0.0583325

#For Figure 6, we generated the graph using Mathematica, the Mathematica Code is:
Plot3D[{Exp[
    1.7353 - 0.5767*x - 2.0062*y]/(1 + 
     Exp[-4.0029 - 1.2694*x + 5.8773*y] + 
     Exp[1.7353 - 0.5767*x - 2.0062*y]), 
  1/(1 + Exp[-4.0029 - 1.2694*x + 5.8773*y] + 
     Exp[1.7353 - 0.5767*x - 2.0062*y]), 
  Exp[-4.0029 - 1.2694*x + 5.8773*y]/(1 + 
     Exp[-4.0029 - 1.2694*x + 5.8773*y] + 
     Exp[1.7353 - 0.5767*x - 2.0062*y])}, {x, 0.12, 2.91}, {y, 0, 
  0.81} , PlotStyle -> Directive[ Opacity[0.5]], 
 PlotLegends -> {"P(red)", "P(yellow)", "P(Clear)"}, 
 AxesLabel -> {"x_2", "x_3"}]
 
#For Figure 7 - 10, 7, 8, 9, 10
G1 = Plot3D[
  Exp[1.7353 - 0.5767*x - 2.0062*y]/(1 + 
     Exp[-4.0029 - 1.2694*x + 5.8773*y] + 
     Exp[1.7353 - 0.5767*x - 2.0062*y]), {x, 0.12, 2.91}, {y, 0, 
   0.81}, PlotRange -> {0, 1}, 
  PlotStyle -> Directive[Red, Specularity[White, 40], Opacity[0.5]], 
  Mesh -> None, PlotPoints -> 25, AxesLabel -> {"x_2", "x_3"}]
  
G2 = Plot3D[
  1/(1 + Exp[-4.0029 - 1.2694*x + 5.8773*y] + 
     Exp[1.7353 - 0.5767*x - 2.0062*y]), {x, 0.12, 2.91}, {y, 0, 
   0.81}, PlotRange -> {0, 1}, 
  PlotStyle -> 
   Directive[Yellow, Specularity[White, 40], Opacity[0.5]], 
  Mesh -> None, PlotPoints -> 25, AxesLabel -> {"x_2", "x_3"}]
  
G3 = Plot3D[
  Exp[-4.0029 - 1.2694*x + 5.8773*y]/(1 + 
     Exp[-4.0029 - 1.2694*x + 5.8773*y] + 
     Exp[1.7353 - 0.5767*x - 2.0062*y]), {x, 0.12, 2.91}, {y, 0, 
   0.81}, PlotRange -> {0, 1}, 
  PlotStyle -> Directive[Green, Specularity[White, 40], Opacity[0.5]],
   Mesh -> None, PlotPoints -> 25, AxesLabel -> {"x_2", "x_3"}]

Show[G3, G2, G1]
# See the Mathematica document plotforThesisPresentation.nb for full detail
x_3 <= 0.5, x_2 <= 1.5; x_3<= 0.5, x_2 > 1.5; 
x_3 > 0.5, x_2 <= 1.5; x_3 > 0.5, x_2 > 1.5; 

{x, 0.12, 1.5}, {y, 0, 0.5}; {x, 1.5, 2.91}, {y, 0, 0.5}; {x, 0.12, 1.5}, {y, 0.5, 0.81}; {x, 1.5, 2.91}, {y, 0.5, 0.81};

#The following code is for the Analysis Section for the log linear model and the Chi-square Test, Section 4.3
closeAllConnections()
rm(list=ls())
setwd("/Users/keqinghe/Desktop/Senior Thesis Document/morpho_Data")
ColorQualityTable =read.table("ChiSquareTest.txt", header=TRUE)
#there might be a warning message but it is ok
ColorQualityTable
#the following are just output to check, not part of the input
        clear red yellow
crystal     4  52     26
powder      1  23     16
none       26   3     41
#the followings are input code again
chisq.test(ColorQualityTable, correct=F)$expected
#the following are just output to check, not part of the input
            clear     red   yellow
crystal 13.239583 33.3125 35.44792
powder   6.458333 16.2500 17.29167
none    11.302083 28.4375 30.26042
#the followings are input code again
chisq.test(ColorQualityTable, correct=F)
#the following are just output to check, not part of the input
Pearsons Chi-squared test

data:  ColorQualityTable
X-squared = 72.643, df = 4, p-value = 6.278e-15
#the followings are input code again
#The G^2 likelihood ratio test statistic value based on the table input
2*(4*log(4/chisq.test(ColorQualityTable, correct=F)$expected[1,1])+52 * log(52/chisq.test(ColorQualityTable, correct=F)$expected[1,2])+26*log(26/chisq.test(ColorQualityTable, correct=F)$expected[1,3])+1*log(1/chisq.test(ColorQualityTable, correct=F)$expected[2,1])+23*log(23/chisq.test(ColorQualityTable, correct=F)$expected[2,2])+16*log(16/chisq.test(ColorQualityTable, correct=F)$expected[2,3])+26*log(26/chisq.test(ColorQualityTable, correct=F)$expected[3,1])+3*log(3/chisq.test(ColorQualityTable, correct=F)$expected[3,2])+41*log(41/chisq.test(ColorQualityTable, correct=F)$expected[3,3]))

1-pchisq(85.11715, (3-1)*(3-1))

#The following code is for setting up the log linear function input
color = gl(3,3,9)
quality = gl(3,1,9)
Counts = c(4,52,26,1,23,16,26,3,41)
library(stats4)
library(splines)
library(VGAM)
#The model without the interaction term which assume independence between color and quality
modelIndep= glm(Counts~color+ quality, family = poisson)
summary(modelIndep)

#The model includes the interaction term which is also the satureated model
modelAssoci = glm(Counts~color+ quality + color *quality, family = poisson)
summary(modelAssoci)

#The following code is for the Analysis Section for the log linear model that tests the conditional independence , Section 4.4

closeAllConnections()
rm(list=ls())
X3_X2Combo = gl(4,9,36)
Color = gl(3,3,36)
Quality = gl(3,1,36)
Counts = c(1,	0,	0,	2,	3,	23,	8,	0,	2,	0,	0,	0,	0,	0,	6,	5,	0,	1,	24,	1,	2,	1,	18,	17,	28,	7,	11,	1,	0,	2,	0,	2,	6,	9,	0,	12)
library(stats4)
library(splines)
library(VGAM)
#Test the conditional independence hypothesis of Quality and Concentration Combo given different levels of Color
modelQualityX3_Color = glm(Counts ~ Color * Quality + Color * X3_X2Combo) #X3_X2Combo + Color + Quality are included with this code
summary(modelQualityX3_Color)

#Test the conditional independence model agains the homogenous model
modelQualityX3Associ = glm(Counts ~ Color * Quality + Color * X3_X2Combo + Quality * X3_X2Combo)
summary(modelQualityX3Associ)
1-pchisq(deviance(modelQualityX3_Color) - deviance(modelQualityX3Associ), (18-12))
deviance(modelQualityX3_Color) - deviance(modelQualityX3Associ)

#Test the conditional independence hypothesis of Quality and Color given different levels of Concentration Combo (4 levels)
modelQualityColor_X3 = glm(Counts ~ Color * X3_X2Combo + Quality * X3_X2Combo)
summary(modelQualityColor_X3)

#Test the conditional independence model agains the homogenous model
1-pchisq(deviance(modelQualityColor_X3) - deviance(modelQualityX3Associ), (16-12))
deviance(modelQualityColor_X3) - deviance(modelQualityX3Associ)

#The Follow code is for generating Figure 1 whose values on the x-axis are based on the level of X3_X2Combo (with data collected in Table 5)

dratio <- data.frame(Concentration_Ratio=rep(c('x_3<= 0.5, x_2<=1.5M', 'x_3<= 0.5, x_2>1.5M','x_3> 0.5, x_2<=1.5M', 'x_3> 0.5, x_2>1.5M'), each=3), quality=rep(c('Clear',  'Red', 'Yellow'), times=4), percent=c(1/(39), 28/(39),10/(39),0/(12),6/(12),6/(12),27/(27+36+46), 36/(27+36+46),46/(27+36+46), 3/(3+8+21),8/(3+8+21), 21/(3+8+21)))
#The followings are just display of the output
dratio
    Concentration_Ratio quality    percent
1  x_3<= 0.5, x_2<=1.5M   Clear 0.02564103
2  x_3<= 0.5, x_2<=1.5M     Red 0.71794872
3  x_3<= 0.5, x_2<=1.5M  Yellow 0.25641026
4   x_3<= 0.5, x_2>1.5M   Clear 0.00000000
5   x_3<= 0.5, x_2>1.5M     Red 0.50000000
6   x_3<= 0.5, x_2>1.5M  Yellow 0.50000000
7   x_3> 0.5, x_2<=1.5M   Clear 0.24770642
8   x_3> 0.5, x_2<=1.5M     Red 0.33027523
9   x_3> 0.5, x_2<=1.5M  Yellow 0.42201835
10   x_3> 0.5, x_2>1.5M   Clear 0.09375000
11   x_3> 0.5, x_2>1.5M     Red 0.25000000
12   x_3> 0.5, x_2>1.5M  Yellow 0.65625000

#The followings are input code
library(ggplot2)

ggplot(dratio, aes(fill=quality, y=percent, x=Concentration_Ratio)) + geom_bar(position='stack', stat='identity') +theme_minimal() +labs(x='Concentration_Tuple', y='Percent',title='ColorDistribution_by_Concentration_Tuple') +theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) +scale_fill_manual('color', values=c('gray', 'red', 'yellow'))
