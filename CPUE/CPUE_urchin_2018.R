
rm(list=ls())

# setup the type of contrast matrix use for the glm ----------------
options(contrast=c("contr.SAS","contr.poly"))
options()["contrast"]    # checking 

# Set directory -------------------------------
Dir<-("C:/Users/mauricio.mardones/Documents/IFOP/Eval_Stock/ERIZO/2019/CPUE")
setwd(Dir)
Data_original<-read.csv("captura erizo historica X XI region con georreferencia.csv",sep=";",header=T)
head(Data_original)
summary(Data_original)

# remove sets with zero effort 
Data<-Data_original[Data_original$ESFUERZO>0,]
summary(Data)
str(Data)

# remove columns if necesary 
myfile<-Data[,c(2,3, 5, 6,7,8, 19, 30, 31, 33, 34)]  
head(myfile)
# vars to use: Year, Quarter, Area 
names(myfile)<-c("Zona","Poligono","Puerto","Año","Mes","Trim","Catch","Prof","RangoProf","Effort","CPUE")
head(myfile)

# create variable of cpue and log(cpue) in myfile if does not have
# myfile$cpue <- myfile$Catch/myfile$Effort
# str(myfile)
# summary(myfile)
# filter: remove cpue greater than 350 
myfile<-myfile[myfile$CPUE<350,]
paste("Number of observations: ",length(myfile$Catch))  #5599
paste("Number of positive obs with catch: ",length(myfile$Catch[myfile$Catch>0])) #155247


#number of observations per year, total catch and effort by year
mysum1 <- aggregate(myfile[,"Año"],by=list(Año=myfile$Año),length)     # function to summarize data by aggregating on a set of variables (1 or more)
names(mysum1) <- c("Año","nobs")
tmp<-aggregate(myfile[,c("Catch","Effort")],by=list(Año=myfile$Año),sum,na.rm=TRUE)
head(tmp)
names(tmp)<-c("Año","Catch","Effort")
mysum1 <- merge(mysum1,tmp,by="Año")     # merging two objects (tables) by a common variable
head(mysum1,10)
mysum1$CPUE=mysum1$Catch/mysum1$Effort
mysum1

#elegir zona de evaluación-------------------------------------
# GLM X norte (1 y 2)

myfile<- subset(myfile, Poligono == 1 | Poligono == 2)



# GLM X sur (3, 4,5,6 y 13)
#myfile<- subset(myfile, Poligono == 3 | Poligono == 4| Poligono == 5| Poligono == 6| Poligono == 13)

# GLM XI (7, 8, 9, 10, 11 y 12)
myfile<- subset(myfile, Poligono == 7 | Poligono == 8| Poligono == 9| Poligono == 10| Poligono == 11| Poligono == 12)

#number of observations per year, total catch and effort by year
mysum1 <- aggregate(myfile[,"Año"],by=list(Año=myfile$Año),length)     # function to summarize data by aggregating on a set of variables (1 or more)
names(mysum1) <- c("Año","nobs")
tmp<-aggregate(myfile[,c("Catch","Effort")],by=list(Año=myfile$Año),sum,na.rm=TRUE)
head(tmp)
names(tmp)<-c("Año","Catch","Effort")
mysum1 <- merge(mysum1,tmp,by="Año")     # merging two objects (tables) by a common variable
head(mysum1,10)
mysum1$CPUE=mysum1$Catch/mysum1$Effort
mysum1


# create plots ---------------------
pdf("Figure 1 XI.pdf",height = 6, width = 8)
#x11
par(mfrow=c(2,2),mar=c(2,4,1,1),oma=c(3,1,1,1))
plot(mysum1$Año,mysum1$nobs,ylab="Number of sets",xlab="Year",type="l",col='black',main="",lwd=2)
plot(mysum1$Año,mysum1$Effort,xlab="",ylab="Effort (hrs)",type="l",main="",col='black',lwd=2)
plot(mysum1$Año,mysum1$Catch,xlab="",ylab="Catch (kg)",type="l",main="",col='black',lwd=2)
plot(mysum1$Año,mysum1$CPUE,xlab="",ylab="CPUE (kg/hr) ",type="l",main="",col='black',lwd=2)
mtext(side=1,line=1,outer=T,text="Year")
dev.off()

myfile$logCPUE <- ifelse(myfile$CPUE>0,log(myfile$CPUE),NA)
head(myfile,10)
summary(myfile$logCPUE)
#  more plots .... check cpue distribution 
pdf("Figure 2 XI.pdf",height = 6, width = 8)
#x11()
par(mfrow=c(1,2),mar=c(4,4,1,1),oma=c(1,1,1,1))
hist(myfile$CPUE,col='grey',breaks="Sturges",main="",xlab="Nominal CPUE")
hist(myfile$logCPUE,col='grey',xlab="Log-nominal CPUE",ylab="",breaks=50,main="")
dev.off()

pdf("Figure 3 XI.pdf",height = 6, width = 8)
#x11()
par(mfrow=c(1,2),mar=c(4,4,1,1),oma=c(1,1,1,1))
qqnorm(myfile$logCPUE)   
qqline(myfile$logCPUE,col='red')
boxplot(logCPUE ~ as.factor(Año),data=myfile,na.rm=T,main="Boxplot nominal logCPUE by year", 
        ylab="log CPUE",col='light grey',boxwex=0.65,xlab="Year")
dev.off()


## Transfor continuos variables to factors meses a trimestres
# cut_Quarter<-c(0,3,6,9,12)
# Levels<-c("1","2","3","4") # this could change becasue there is no effort in months 1 and 2, maybe only 2 seasons? 
# myfile$Quarter<-cut.default(myfile$Month,cut_Quarter,labels=Levels)
# head(myfile)

# Areas 
# myfile<-myfile[myfile$Latitude<0,] # eliminate sets with Latitude = 0
# par(mfrow=c(1,1))
# with(myfile,hist(Latitude,col="grey", xlab="Latitude",cex.main=0.85))
# cut_Lat<-c(-56,-51,-53,max(myfile$Latitude))
# Levels<-c("A1","A2","A3")
# myfile$Area<-cut.default(myfile$Latitude,cut_Lat,labels=Levels)
# summary(myfile)
# str(myfile)

# change those variables that need to be factors
# myfile$Year<-as.factor(myfile$Year)
# myfile$Quarter<-as.factor(as.character(myfile$Quarter))

# change those variables that need to be factors
myfile$Year<-as.factor(myfile$Año)
myfile$Quarter<-as.factor(as.character(myfile$Trim))
myfile$RangoProf<-as.factor(myfile$RangoProf)

###plot observations by factors
pdf("Figure 4 XI.pdf",height = 6, width = 7)
#x11()
par(mar=c(4,5,2,2),oma=c(1,1,1,1))
mat <- matrix(c(1,1,2,3), 2, 2,byrow = TRUE)
disp1 <- layout(mat=mat)
layout.show(disp1) 
barplot(table(myfile$Year),xlab="Year",ylab="number of sets")
barplot(table(myfile$Quarter),xlab="Trim",ylab="number of sets")
barplot(table(myfile$RangoProf),xlab="Rango Profundidad")
dev.off()

pdf("Figure 5 XI.pdf",height = 6, width = 11)
#x11()
par(mfrow=c(1,2))
plot.design(myfile[c(2,12:14)],fun=mean,cex=0.7)       # example of informative plot..
plot.design(myfile[c(2,12:14)],fun=median,cex=0.7)
dev.off()
#
pdf("Figure 6 XI.pdf",height = 6, width = 11)
#x11()
par(mfrow=c(3,1))
interaction.plot(myfile$Año,myfile$Quarter,myfile$logCPUE,fun=median,col=seq(1:10),trace.label="Month",lty=1,xlab="year",ylab="median logCPUE")
interaction.plot(myfile$Año,myfile$RangoProf,myfile$logCPUE,fun=median,col=seq(1:10),trace.label="Area",lty=1,xlab="year",ylab="median logCPUE")
interaction.plot(myfile$Quarter,myfile$RangoProf,myfile$logCPUE,fun=median,col=seq(1:10),trace.label="Area",lty=1,xlab="Quarter",ylab="median logCPUE")
dev.off()
# reconsider area and quarter categorization 

par(mar=c(4,5,2,2),oma=c(1,1,1,1))
mat <- matrix(c(1,1,2,3), 2, 2,byrow = TRUE)
disp1 <- layout(mat=mat)
barplot(table(myfile$Año,myfile$Quarter))
barplot(table(myfile$Año,myfile$RangoProf))

#
# GLM--------------------------------
#
glm0 <- glm(formula=logCPUE ~ Year+Quarter+RangoProf,
            family = gaussian, data = myfile,na.action=na.exclude,
            contrasts=list(Year="contr.SAS",Quarter="contr.SAS",RangoProf="contr.SAS"),
            control=list(epsilon=0.00001,maxit=50,trace=F))

tmp <- anova(glm0,test="Chisq")
tmp <- as.data.frame(tmp)
tmp$PercDevExp <- 100*(tmp$Deviance/(max(tmp[,4])-min(tmp[,4])))
row.names(tmp)[tmp$PercDevExp >= 5.0]   
DevTablePosObs <- tmp
DevTablePosObs
							

glm1 <- glm(formula=logCPUE ~ Year+Quarter+RangoProf+Quarter:RangoProf,
            family = gaussian, data = myfile,na.action=na.exclude,
            contrasts=list(Year="contr.SAS",Quarter="contr.SAS",RangoProf="contr.SAS"),
            control=list(epsilon=0.00001,maxit=50,trace=F))

tmp <- anova(glm1,test="Chisq")
tmp <- as.data.frame(tmp)
tmp$PercDevExp <- 100*(tmp$Deviance/(max(tmp[,4])-min(tmp[,4])))
row.names(tmp)[tmp$PercDevExp >= 5.0]   
tmp=tmp[5,]
DevTablePosObs= rbind(DevTablePosObs,tmp)
DevTablePosObs

glm2 <- glm(formula=logCPUE ~ Year+Quarter+RangoProf+Year:Quarter,
            family = gaussian, data = myfile,na.action=na.exclude,
            contrasts=list(Year="contr.SAS",Quarter="contr.SAS",RangoProf="contr.SAS"),
            control=list(epsilon=0.00001,maxit=50,trace=F))
glm2

tmp <- anova(glm2,test="Chisq")
tmp <- as.data.frame(tmp)
tmp$PercDevExp <- 100*(tmp$Deviance/(max(tmp[,4])-min(tmp[,4])))
row.names(tmp)[tmp$PercDevExp >= 5.0]   
tmp=tmp[5,]
DevTablePosObs= rbind(DevTablePosObs,tmp)
DevTablePosObs

glm3 <- glm(formula=logCPUE ~ Year+Quarter+RangoProf+Year:RangoProf,
            family = gaussian, data = myfile,na.action=na.exclude,
            contrasts=list(Year="contr.SAS",Quarter="contr.SAS",RangoProf="contr.SAS"),
            control=list(epsilon=0.00001,maxit=50,trace=F))
glm3

tmp <- anova(glm3,test="Chisq")
tmp <- as.data.frame(tmp)
tmp$PercDevExp <- 100*(tmp$Deviance/(max(tmp[,4])-min(tmp[,4])))
row.names(tmp)[tmp$PercDevExp >= 5.0]   
tmp=tmp[5,]
DevTablePosObs= rbind(DevTablePosObs,tmp)
DevTablePosObs

write.table(DevTablePosObs,file="DevTablePosObs.csv",sep=",",row.names=T,col.names=T,na='.')

##De acuerdo a los resultados en la tabla DevTablePosObs.csv, las variables que 
#un valor mayor a 5% son Year, Quarter, Area, Gear, Year:Quarter, Year:Area, 
#Year:SST & Year:Gear. SST tiene un 0.02 pero al estar en interaccion 
#con el Year la incluyo tambien.

summary(glm2)
anova(glm2,test="Chisq")
x11()
pdf("Figure 9 X Sur.pdf",height = 6, width = 11)
par(mfrow=c(2,4))
termplot(glm2,se=T,ylim="free")
plot(glm2)
dev.off()


# GLMM==============================================
install.packages("lme4")

library(lme4)
glmm1 <- lmer(logCPUE ~ Year + Quarter + RangoProf + 
                (1|Year:Quarter) + (1|Year:RangoProf),
              data = myfile,REML=F,na.action=na.exclude)
glmm2 <- lmer(logCPUE ~ Year + Quarter + RangoProf +
                (1|Year:Quarter),
              data = myfile,REML=F,na.action=na.exclude)
glmm3 <- lmer(logCPUE ~ Year + Quarter + RangoProf + (1|Year) +
                (1|RangoProf),data = myfile,REML=F,na.action=na.exclude)
glmm4 <- lmer(logCPUE ~ Year + Quarter + RangoProf +
                (1|Year),data = myfile,REML=F,na.action=na.exclude)

tmp<-anova(glmm4,glmm3,glmm2,glmm1); tmp
write.table(tmp,"Mixed.effect.csv",sep=",")

summary(glmm1)    #AIC mas bajo
myfinalmodel<-lmer(logCPUE ~ Year + Quarter + RangoProf + 
                     (1|Year:Quarter) + (1|Year:RangoProf),
                   data = myfile,REML=T,na.action=na.exclude)

pdf("Figure 7 XI.pdf",height = 7, width = 6)
#x11()
par(mfrow=c(5,1),mar=c(4,4,2,1),oma=c(1,1,0,1))
res <- resid(myfinalmodel,type = "pearson")
fit <- fitted(myfinalmodel)
plot(fit,res,ylab="Pearson residuals")
abline(h=0,col="red",lty=2)
qqnorm(res,main="")
qqline(res,lty=2,col="red")
plot(res~myfile$Year,col="grey",xlab="Year",ylab="Pearson residuals")
abline(h=0,col="red",lty=2)
plot(res~myfile$Quarter,col="grey",xlab="month",ylab="Pearson residuals")
abline(h=0,col="red",lty=2)
plot(res~myfile$RangoProf,col="grey",xlab="Area",ylab="Pearson residuals")
abline(h=0,col="red",lty=2)
dev.off() 

summary(myfinalmodel)

install.packages("emmeans")
library("emmeans")
install.packages("Matrix")
library("Matrix")
install.packages("lsmeans")
library("lsmeans")
install.packages("pbkrtest")
library("pbkrtest")
install.packages("lmerTest")
library("lmerTest")

lsm<- lsmeansLT(model=myfinalmodel,test.effs="Year")
head(lsm)

Nyears<-length(unique(myfile$Year) )#from 2002 to 2016

# final plot --------------------------------

pdf("Figure 8 XI.pdf", height = 5, width = 6)
#x11()
par(mfrow=c(1,1),mar=c(4,4,2,2),oma=c(1,1,1,1))
plot(mysum1$Año,mysum1$CPUE, col = "blue",pch=16,cex=1.5,ylab="CPUE (kg/hr)",xlab="Year",ylim=c(20,200))
lines(mysum1$Año,exp(lsm$Estimate[1:Nyears]),lty=1,col=34,lwd=2.5) 
lines(mysum1$Año,exp(lsm$lower[1:Nyears]),lty=2,col="grey33",lwd=2)    
lines(mysum1$Año,exp(lsm$upper[1:Nyears]),lty=2,col="grey33",lwd=2)  
legend(2010, 50,c("nominal", "standardized"), col = c("blue",34),
       text.col = 1, pch = c(16,-1),lty=c(0,1),lwd=c(-1,2.5),bty="n")          
dev.off()

output<-data.frame(mysum1$Año,mysum1$CPUE,exp(lsm$Estimate[1:Nyears]),exp(lsm$lower[1:Nyears]),
                   exp(lsm$upper[1:Nyears]))
names(output)<-c("Year","Nominal CPUE","Standard CPUE","CI_low","CI upp")
head(output)
write.table(output,"output.final XI.csv",sep=",",row.names=F)

