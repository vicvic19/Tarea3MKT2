rm(list=ls()) # clear the list of objects
graphics.off() # clear the list of graphs
options(digits = 3) # number of digits to display
options(scipen=999)

# Directorio 
data <- read.csv("../data/panel_vinos.csv",sep=";")

summary(data)
head(data)

data <- data[,-1]
data3 <- data[1:48,]

data_wide<-reshape(data, idvar=c("expert","test"), timevar="alternative", direction="wide",
                   v.names=c("choice", "fixed.acidity","volatile.acidity","citric.acid",
                             "residual.sugar","chlorides","free.sulfur.dioxide",
                             "total.sulfur.dioxide","density","pH","sulphates","alcohol"))


choice= data_wide[,c(3,15,27,39)]
atributos= data_wide[,c(4:14,16:26,28:38,40:50)]
atributos=atributos[,order(names(atributos))]
data_wide=cbind.data.frame(data_wide[,1:2], choice, atributos)
# View(data_wide)

id=data_wide[,1]
obsnum=data_wide[,2]
eleccion=data_wide[,3:6]
alcohol=data_wide[,7:10]
chlorides=data_wide[,11:14]
citric_acid=data_wide[,15:18]
density=data_wide[,19:22]
fixed_acidity=data_wide[,23:26]
free_sulfur_dioxide=data_wide[,27:30]
pH=data_wide[,31:34]
residual_sugar=data_wide[,35:38]
sulphates=data_wide[,39:42]
total_sulfur_dioxide=data_wide[,43:46]
volatile_acidity=data_wide[,47:50]

n=nrow(data_wide)
nclientes=length(unique(id))
nopciones=ncol(eleccion)
ones=rep(1,n)

alt1=cbind(alcohol[,1], chlorides[,1], citric_acid[,1], density[,1],
           fixed_acidity[,1], free_sulfur_dioxide[,1], pH[,1],
           residual_sugar[,1], sulphates[,1], total_sulfur_dioxide[,1],
           volatile_acidity[,1])

alt2=cbind(alcohol[,2], chlorides[,2], citric_acid[,2], density[,2],
           fixed_acidity[,2], free_sulfur_dioxide[,2], pH[,2],
           residual_sugar[,2], sulphates[,2], total_sulfur_dioxide[,2],
           volatile_acidity[,2])

alt3=cbind(alcohol[,3], chlorides[,3], citric_acid[,3], density[,3],
           fixed_acidity[,3], free_sulfur_dioxide[,3], pH[,3],
           residual_sugar[,3], sulphates[,3], total_sulfur_dioxide[,3],
           volatile_acidity[,3])

alt4=cbind(alcohol[,4], chlorides[,4], citric_acid[,4], density[,4],
           fixed_acidity[,4], free_sulfur_dioxide[,4], pH[,4],
           residual_sugar[,4], sulphates[,4], total_sulfur_dioxide[,4],
           volatile_acidity[,4])

#install.packages("lmtest")
library(lmtest)
library(mlogit)	
library(Formula) 
# load package to compare
# format the data according to what mlogit requires
n=nrow(data_wide)
tmpchoice = apply(eleccion,1,which.max)
mychoice = array(NA, dim=n)

for(i in 1:n){
  if (tmpchoice[i] == 1) mychoice[i]="alt1"
  if (tmpchoice[i] == 2) mychoice[i]="alt2" 
  if (tmpchoice[i] == 3) mychoice[i]="alt3" 
  if (tmpchoice[i] == 4) mychoice[i]="alt4"	
}
rm(tmpchoice)
levels(mychoice) = c("alt1", "alt2", "alt3", "alt4")


mywine = data.frame(ch=mychoice, 
                    Alcohol.alt1= alt1[,1],
                    Chlorides.alt1= alt1[,2],
                    Citric_acid.alt1= alt1[,3],
                    Density.alt1= alt1[,4],
                    Fixed_acidity.alt1= alt1[,5],
                    Free_sulfur_dioxide.alt1= alt1[,6],
                    PH.alt1= alt1[,7],
                    Residual_sugar.alt1= alt1[,8],
                    Sulphates.alt1= alt1[,9],
                    Total_sulfur_dioxide.alt1= alt1[,10],
                    Volatile_acidity.alt1=alt1[,11],
                    Alcohol.alt2= alt2[,1],
                    Chlorides.alt2= alt2[,2],
                    Citric_acid.alt2= alt2[,3],
                    Density.alt2= alt2[,4],
                    Fixed_acidity.alt2= alt2[,5],
                    Free_sulfur_dioxide.alt2= alt2[,6],
                    PH.alt2= alt2[,7],
                    Residual_sugar.alt2= alt2[,8],
                    Sulphates.alt2= alt2[,9],
                    Total_sulfur_dioxide.alt2= alt2[,10],
                    Volatile_acidity.alt2=alt2[,11],
                    Alcohol.alt3= alt3[,1],
                    Chlorides.alt3= alt3[,2],
                    Citric_acid.alt3= alt3[,3],
                    Density.alt3= alt3[,4],
                    Fixed_acidity.alt3= alt3[,5],
                    Free_sulfur_dioxide.alt3= alt3[,6],
                    PH.alt3= alt3[,7],
                    Residual_sugar.alt3= alt3[,8],
                    Sulphates.alt3= alt3[,9],
                    Total_sulfur_dioxide.alt3= alt3[,10],
                    Volatile_acidity.alt3=alt3[,11],
                    Alcohol.alt4= alt4[,1],
                    Chlorides.alt4= alt4[,2],
                    Citric_acid.alt4= alt4[,3],
                    Density.alt4= alt4[,4],
                    Fixed_acidity.alt4= alt4[,5],
                    Free_sulfur_dioxide.alt4= alt4[,6],
                    PH.alt4= alt4[,7],
                    Residual_sugar.alt4= alt4[,8],
                    Sulphates.alt4= alt4[,9],
                    Total_sulfur_dioxide.alt4= alt4[,10],
                    Volatile_acidity.alt4=alt4[,11], id=id)


myfrmywine = mlogit.data(mywine, shape="wide", varying=2:45, choice="ch", id.var="id")


### CLASES LATENTES
library(gmnl)
logit2clases = gmnl(ch ~ Alcohol + Chlorides + Citric_acid + Density + Fixed_acidity + Free_sulfur_dioxide + PH + Residual_sugar + Sulphates + Total_sulfur_dioxide + Volatile_acidity| 0| 0| 0| 1 ,
                    data = myfrmywine, model="lc", Q=2, panel=TRUE)

summary(logit2clases)


betas <- c(0.276,-1.874,-0.183,-17.881,0.025,0.004,-0.414,0.016,0.916,-0.003,-1.084)
betas2 <- betas*-1


coef2 <- logit2clases$coefficients
clase1 <- coef2[1:11]
clase2 <- coef2[12:22]

se2clases <- sqrt(diag(vcov(logit2clases)))
seclase1 <- se2clases[1:11]
seclase2 <- se2clases[12:22]

Inf1 <- clase1-seclase1*1.96
Inf2 <- clase2-seclase2*1.96
Sup1 <- clase1+seclase1*1.96
Sup2 <- clase2+seclase2*1.96



ICclase1 <- data.frame(Inf1,betas2,clase1,Sup1)
ICclase2 <- data.frame(Inf2,betas,clase2,Sup2)

ICclase1$Esta <- FALSE
ICclase1$Ancho <- NULL
ICclase1$Error <- 0
for (i in 1:nrow(ICclase1)){
  if (ICclase1$Inf1[i]<=ICclase1$betas2[i] & ICclase1$Sup1[i]>=ICclase1$betas2[i]){ICclase1$Esta[i]=TRUE}
  ICclase1$Ancho[i]=ICclase1$Sup1[i]-ICclase1$Inf1[i]
  ICclase1$Error[i]=(ICclase1$betas2[i]/ICclase1$clase1[i])-1
}


ICclase2$Esta <- FALSE
ICclase2$Ancho <- NULL
ICclase2$Error <- 0
for (i in 1:nrow(ICclase2)){
  if (ICclase2$Inf2[i]<=ICclase2$betas[i] & ICclase2$Sup2[i]>=ICclase2$betas[i]){ICclase2$Esta[i]=TRUE}
  ICclase2$Ancho[i]=ICclase2$Sup2[i]-ICclase2$Inf2[i]
  ICclase2$Error[i]=(ICclase2$betas[i]/ICclase2$clase2[i])-1
}


########## UNA CLASE

logit = mlogit(ch ~ Alcohol + Chlorides + Citric_acid + Density + Fixed_acidity + Free_sulfur_dioxide + PH + Residual_sugar + Sulphates + Total_sulfur_dioxide + Volatile_acidity| 0 ,
               data = myfrmywine, model="mnl")
summary(logit)


coef=logit$coefficients
se <- sqrt(diag(vcov(logit)))

Inferior <- coef-se*1.96
Superior <- coef+se*1.96
IC <- data.frame(Inferior,betas,coef,Superior)
IC$Esta <- FALSE
IC$Ancho <- NULL
IC$Error <- 0
for (i in 1:nrow(IC)){
  if (IC$Inferior[i]<=IC$betas[i] & IC$Superior[i]>=IC$betas[i]){IC$Esta[i]=TRUE}
  IC$Ancho[i]=IC$Superior[i]-IC$Inferior[i]
  IC$Error[i]=(IC$betas[i]/IC$coef[i])-1
}
