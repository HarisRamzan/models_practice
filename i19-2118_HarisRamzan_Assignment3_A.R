#haris ramzan
#i19-2118 section A

Datasetfile = read.table(file.choose(),header = TRUE,sep = ',',stringsAsFactors = FALSE) #choosing dataset file from menu
View(Datasetfile)


#scatter plot of each variable with respect to output varriable
########################################FIRST QUESTION question####################################################
plot(Datasetfile$var1,Datasetfile$output,main = "Scatter plot of variable 1 and output",xlab = "Variable1",ylab = "Outputvariable")
plot(Datasetfile$var2,Datasetfile$output,main = "Scatter plot of variable 2 and output",xlab = "Variable2",ylab = "Outputvariable")
plot(Datasetfile$var3,Datasetfile$output,main = "Scatter plot of variable 3 and output",xlab = "Variable3",ylab = "Outputvariable")
###########################################################################################################
correlationbwtweenv1andoutput<-cor(Datasetfile$var1,Datasetfile$output)
#correlation between var1 and output is 0.1825475 which is weak
correlationbwtweenv2andoutput<-cor(Datasetfile$var2,Datasetfile$output)
#correlation between var2 and output is 0.1467508 which is weak
correlationbwtweenv3andoutput<-cor(Datasetfile$var3,Datasetfile$output)
#correlation between var3 and output is  -0.006380052 weak nagative

###################################################SECOND QUESTION##############################
#linier model of first variable with respect to output variable
lmModelv1andoutput<-lm(Datasetfile$output ~ Datasetfile$var1)
summary(lmModelv1andoutput)
#intercept is 34.89164 when var1=0 && and for every additional v1 there is increase of average 0.11974 in output
abline(lmModelv1andoutput)#abline of linier model of v1 and output
#linier regression using model equation 
#output= 34.89164 + 0.11974 * var1

#linier model of second variable with respect to output variable
lmModelv2andoutput<-lm(Datasetfile$output ~ Datasetfile$var2)
summary(lmModelv2andoutput)
#intercept is 38.30037 when var2=0 && and for every additional v2 there is increase of average 0.09531 in output
abline(lmModelv2andoutput)#abline of linier model of v2 and output
#linier regression using model equation 
#output= 38.30037  + 0.09531 * var2

#linier model of third variable with respect to output variable
lmModelv3andoutput<-lm(Datasetfile$output ~ Datasetfile$var3)
summary(lmModelv3andoutput)
#intercept is 51.300437 when var3=0 && and for every additional v3 there is decrease of average -0.003806 in output
abline(lmModelv3andoutput)#abline of linier model of v3 and output
#linier regression using model equation 
#output= 51.300437  + -0.003806 * var3
############################################################################


###########################################THIRD QUESTION##############################
#polynomial model level 2 of first variable with respect to output variable
lmModelv1andoutputlevel2 <- lm(Datasetfile$output ~ poly(Datasetfile$var1,2))
summary(lmModelv1andoutputlevel2)
#calculating linier reression model using equeation of level 2
#output=50.9333 + 7.1482*v1 + (-8.9534)*v1^2
#output=50.9333 + 7.1482*v1 -8.9534*v1^2
#polynomial model level 2 of second variable with respect to output variable

lmModelv2andoutputlevel2 <- lm(Datasetfile$output ~ poly(Datasetfile$var2,2))
summary(lmModelv2andoutputlevel2)
#calculating linier reression model using equeation of level 2
##output=50.933 + 5.746*v2 + (4.591)*v2^2
#output=50.9333 + 5.746*v2 +4.591*v2^2

#polynomial model level 2 of third variable with respect to output variable
lmModelv3andoutputlevel2 <- lm(Datasetfile$output ~ poly(Datasetfile$var3,2))
summary(lmModelv3andoutputlevel2)
#calculating linier reression model using equeation of level 2
##output=50.9333 + (-0.2498)*v3 + (-0.3017)*v3^2
#output=50.9333 - 0.2498*v3 -0.3017*v3^2


#polynomial model level 3 of first variable with respect to output variable
lmModelv1andoutputlevel3 <- lm(Datasetfile$output ~ poly(Datasetfile$var1,3))
summary(lmModelv1andoutputlevel3)
#calculate linier reression model using equeation of level 3  for variable 1
#output=50.9333+7.1482*v1 - 8.9534v1^2 + 0.2906*v1^3

#polynomial model level 3 of second variable with respect to output variable
lmModelv2andoutputlevel3 <- lm(Datasetfile$output ~ poly(Datasetfile$var2,3))
summary(lmModelv2andoutputlevel3)
#calculate linier reression model using equeation of level 3  for variable 2
#output=50.9333 +5.7464*v2 + 4.5908v2^2 + 0.4758*v2^3
#polynomial model level 3 of third variable with respect to output variable
lmModelv3andoutputlevel3 <- lm(Datasetfile$output ~ poly(Datasetfile$var3,3))
summary(lmModelv3andoutputlevel3)
#calculate linier reression model using equeation of level 3 for variable 3
#output=50.9333 - 0.2498*v3 - 0.3017v3^2 + 1.4889*v3^3
############################################################################

##########################################FOURTH QUESTION###########################

#plotting regression model of level 2
plot(fitted(lmModelv1andoutputlevel2),residuals(lmModelv1andoutputlevel2))
#describing the high volume of scatter plots are above 51
plot(fitted(lmModelv2andoutputlevel2),residuals(lmModelv2andoutputlevel2))
#describing the high volume of scatter plots are below 51
plot(fitted(lmModelv3andoutputlevel2),residuals(lmModelv3andoutputlevel2))
#describing the high volume of scatter plots are at 50

#plotting regression model of level 3
plot(fitted(lmModelv1andoutputlevel3),residuals(lmModelv1andoutputlevel3))
#describing the high volume of scatter plots are above 51 
plot(fitted(lmModelv2andoutputlevel3),residuals(lmModelv2andoutputlevel3))
#describing the high volume of scatter plots are below 51 and outliars as well
plot(fitted(lmModelv3andoutputlevel3),residuals(lmModelv3andoutputlevel3))
#describing the high volume of scatter plots are at 51 and some outliars

###############################################################################
