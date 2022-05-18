library("tidyverse")
library("e1071")
library("dplyr")
library("rpart")
library(reshape2)
library("caret")
library("caTools")
banco = read.csv("UniversalBank.csv")
banco$Personal.Loan = as.factor(banco$Personal.Loan)
banco$Online = as.factor(banco$Online)
banco$CreditCard = as.factor(banco$CreditCard)
set.seed(1)
train.index <- sample(row.names(banco), 0.6*dim(banco)[1])
test.index <- setdiff(row.names(banco), train.index)
train.df <- banco[train.index, ]
test.df <- banco[test.index, ]
train <- banco[train.index, ]
test = banco[train.index,]
# inciso A)
funcmelt.banco = melt(train,id=c("CreditCard","Personal.Loan"),variable= "Online")
funccast.banco=dcast(funcmelt.banco,CreditCard+Personal.Loan~Online)
funccast.banco[,c(1:2,14)]
# Inciso b) La probabilidad de aceptación del Préstamo teniendo una tarjeta de crédito bancaria y usuario de servicios en línea es 77/3000 = 2.6%
#Inciso C) crear 2 tablas por separado
funcmelt.banco1 = melt(train,id=c("Personal.Loan"),variable = "Online")
funcmelt.banco2 = melt(train,id=c("CreditCard"),variable = "Online")
funccast.banco1=dcast(funcmelt.banco1,Personal.Loan~Online)
funccast.banco2=dcast(funcmelt.banco2,CreditCard~Online)
Loan1=funccast.banco1[,c(1,13)]
Loan2=funccast.banco2[,c(1,14)]
Loan1
Loan2
#inciso d
table(train[,c(14,10)])
table(train[,c(13,10)])
table(train[,c(10)])
#Los resultados serian los siguientes:
#i. 77/(77+198)=28%
#ii. 166/(166+109)= 60.3%
#iii.275/(275+2725)=9.2%
#iv. 801/(801+1924)=29.4%
#v. 1588/(1588+1137) = 58.3%
#vi. 2725/(2725+275) = 90.8%
#Inciso e) Utiliza las cantidades calculadas anteriormente para calcular la probabilidad naïve
((77/(77+198))*(166/(166+109))*(275/(275+2725)))/(((77/(77+198))*(166/(166+109))*(275/(275+2725)))+((801/(801+1924))*(1588/(1588+1137))*2725/(2725+275)))
#Inciso f Compara este valor con el obtenido de la tabla dinámica en (b).
# Son bastante similares Una tiene el 9,05% al 9,07%
# Inciso g) 
naive.train = train.df[,c(10,13:14)]
naive.test = test.df[,c(10,13:14)]
naive1 = naiveBayes(Personal.Loan~.,data=naive.train)
naive1
#En este caso es el mismo resultado obtenido en los métodos anteriores,
