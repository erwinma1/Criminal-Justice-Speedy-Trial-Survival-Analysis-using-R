# This R Code demonstrates the Queens Speedy Trial Survival Analysis. 
# All identifiers or reference to any organization has been removed.
# See read me and technical documentation for details.
# By Erwin Ma. Copyright Prohibited. 


mydata<- read.csv("C:/Users/EM/Felony 2014-18.csv",header=T)


attach(mydata)

library(survival)
library(dplyr)
library(tidyr)
library(ggplot2)
library(survminer)
library(rcompanion)

###########################################
######## Clean and Filter Raw Data ######## 
###########################################

#filter only cases surviving arraignment
mydata<-filter(mydata, mydata$Dispo.at.AR.2 == 0)

mydata<-filter(mydata,appear.type == 'Arraignment')


#make arraignment release status into release/detained groups
mydata$release.status <- as.factor(as.character(mydata$release.status))
mydata<-filter(mydata, mydata$release.status !="Unknown")
mydata<-filter(mydata, mydata$release.status !="Not reported")
mydata<-filter(mydata, mydata$release.status !="Not arrested at time of indictment")
mydata<-filter(mydata, mydata$release.status !="NULL")
mydata<-filter(mydata, mydata$release.status != "Fugitive (warrant ordered)")
mydata<-filter(mydata, mydata$release.status !="Other")
#mydata<-filter(mydata, mydata$release.status != "Remand")#remand is like infinite bail

mydata<- mutate(mydata, rel.group = ifelse(mydata$release.status == "ROR"|
                                             mydata$release.status == "Supervised Release","Released","Bail"))

mydata<- mutate(mydata,Ar.group = ifelse(mydata$rel.group == "Bail",1,0))
mydata$Ar.group <- as.numeric(as.character(mydata$Ar.group))


#make final dispo binary event
mydata<-filter(mydata, mydata$final.dispo != "NULL")



mydata<- mutate(mydata, dispo.Event = ifelse(mydata$final.dispo == "730 COMMIT"|
                                               mydata$final.dispo =="CONVICTED" |
                                               mydata$final.dispo == "EXTRADITED"|
                                               mydata$final.dispo == "PG"|
                                               mydata$final.dispo == "DISMISSED"|
                                               mydata$final.dispo ==  "DISMISSED WITH LEAVE"|
                                               mydata$final.dispo ==  "DISMISSED, DA HAS 45 DAYS"|
                                               mydata$final.dispo == "ACQUITTED", 1,0))

mydata<- mutate(mydata, censored = ifelse(mydata$final.dispo == "Relieved",1,0))
mydata$censored <- as.factor(as.character(mydata$censored))

#init charge binary
mydata<- mutate(mydata, Felony = ifelse(mydata$init.charge.type =="F"|mydata$init.charge.type =="f",1,0))
mydata<- mutate(mydata, Misdemeanor = ifelse(mydata$init.charge.type =="M",1,0))
mydata<- mutate(mydata, V.Other = ifelse(mydata$init.charge.type =="?"|
                                           mydata$init.charge.type =="V"|
                                           mydata$init.charge.type =="NULL",1,0))


#County Clean up and grouping
mydata$county <-as.factor(as.character(county))
mydata<-filter(mydata, mydata$county != "NULL")
mydata<-filter(mydata, mydata$county != "RICHMOND")
mydata<-filter(mydata, mydata$county != "Albany")
mydata<- mutate(mydata, Bronx = ifelse(mydata$county =="Bronx",1,0))
mydata<- mutate(mydata, Kings = ifelse(mydata$county =="Kings",1,0))
mydata<- mutate(mydata, New.York = ifelse(mydata$county =="New York",1,0))
mydata<- mutate(mydata, Queens = ifelse(mydata$county =="Queens",1,0))
mydata<- mutate(mydata, Richmond = ifelse(mydata$county =="Richmond"|
                                            mydata$county =="RICHMOND",1,0))

#Gender Binary
mydata<- mutate(mydata, Male = ifelse(mydata$gender =="Male",1,0))
mydata<- mutate(mydata, Female = ifelse(mydata$gender =="Female",1,0))
mydata<- mutate(mydata, OtherGender = ifelse(mydata$gender =="NULL"|
                                               mydata$gender =="Other"| mydata$gender =="Unknown" ,1,0))

#LAS Race binary
mydata<- mutate(mydata, White = ifelse(mydata$las.race =="White",1,0))
mydata<- mutate(mydata, Black = ifelse(mydata$las.race =="Black",1,0))
mydata<- mutate(mydata, Other = ifelse(mydata$las.race =="Asian or Pacific Islander"|
                                         mydata$las.race =="American Indian or Alaskan Native"|
                                         mydata$las.race =="Hispanic"|
                                         mydata$las.race =="Latino"|
                                         mydata$las.race =="NULL"|
                                         mydata$las.race =="Other"|
                                         mydata$las.race =="Two or more races (not Hispanic orLatino)"|
                                         mydata$las.race =="Unknown",1,0))

#EthnRace2
mydata<- mutate(mydata, ApiNatO = ifelse(mydata$EthnRace2 =="API.Nativ.Other",1,0))
mydata<- mutate(mydata, Black.Hisp = ifelse(mydata$EthnRace2 =="Black Hispanic",1,0))
mydata<- mutate(mydata, Black.Only = ifelse(mydata$EthnRace2 =="Black Only",1,0))
mydata<- mutate(mydata, White.Hisp = ifelse(mydata$EthnRace2 =="White Hispanic",1,0))
mydata<- mutate(mydata, White.Only = ifelse(mydata$EthnRace2 =="White Only",1,0))
mydata<- mutate(mydata, Hispanic = ifelse(mydata$EthnRace2 =="Hispanic Only"|
                                            mydata$EthnRace2 =="Hispanic Other",1,0))


#mydata<- filter(mydata, Days.Detained <= 30)
mydata<-filter(mydata,mydata$init.top.charge != "EXTRADITION")


#Felonies Only
mydata<-filter(mydata,mydata$init.charge.type == 'F' )


#FinalDispo
mydata<-filter(mydata, mydata$final.dispo =="PG"|mydata$final.dispo =="Relieved")
mydata<-filter(mydata, mydata$final.dispo =="DISMISSED"| mydata$final.dispo == "Relieved")
mydata<-filter(mydata, mydata$final.dispo !="DISMISSED")


#Bail Only
mydata<-filter(mydata,mydata$Ar.group == 1 )

attach(mydata)

############################################
######### Case Length Kaplan Meier #########
############################################
time0<-mydata$Days.Detained
as.numeric(as.character(time0))
group<-mydata$county
event<-mydata$dispo.Event


#Kaplan Meier Case Length
mydatakm<- mydata
mydatakm<-filter(mydatakm,mydatakm$Case.Length != "#VALUE!")
mydatakm<-filter(mydatakm,mydatakm$Case.Length != "Active")
mydatakm<-filter(mydatakm,mydatakm$Case.Length != "NA")
mydatakm$Case.Length<-as.numeric(as.character(mydatakm$Case.Length))
mydatakm <- filter(mydatakm, mydatakm$Case.Length >=0)
mydatakm <- filter(mydatakm, mydatakm$Case.Length<=370)

#county spread
mydatakm <-filter(mydatakm, mydatakm$Queens==1)

timekm<-mydatakm$Case.Length
eventkm<-mydatakm$dispo.Event

kmsurvival <- survfit(Surv(timekm,eventkm)~1)
summary(kmsurvival)
plot(kmsurvival,main = "Kaplan-Meier Case Duration Survival", xlab="time (days)", ylab="survival prob")

###################################################
######### Pretrial Detention Kaplan Meier #########
###################################################
attach(mydata)
mydatakm2<- mydata
mydatakm2<-filter(mydatakm2,mydatakm2$Days.Detained !="NULL")
mydatakm2$Days.Detained<-as.numeric(as.character(mydatakm2$Days.Detained))
mydatakm2<-filter(mydatakm2, mydatakm2$Days.Detained <=350)

#county spread
mydatakm2 <-filter(mydatakm2, mydatakm2$Queens==1)

timekm2<-mydatakm2$Days.Detained
eventkm2<-mydatakm2$dispo.Event

kmsurvival2 <- survfit(Surv(timekm2,eventkm2)~1)
summary(kmsurvival2)
plot(kmsurvival2,main = "Kaplan-Meier Detention Survival", xlab="time (days)", ylab="survival prob")

#Survival Disposition/ Event Only
attach(mydata)
mydata1<-mydata
mydata1<-filter(mydata1,mydata1$Days.Detained !="NULL")
mydata1$Days.Detained<-as.numeric(as.character(mydata1$Days.Detained))
mydata1<-filter(mydata1, mydata1$Days.Detained <=350)

time0<-mydata1$Days.Detained
as.numeric(as.character(time0))
group<-mydata1$county
event<-mydata1$dispo.Event


nasurvival2 <- survfit(Surv(time0,event)~group)
summary(nasurvival2)
plot(nasurvival2,col = c("grey","orange","blue","forest green","brown"), main = "Pre-Trial Detention Survival", xlab="Days Detained", ylab="Pre-trial Survival")
legend("topright", 
       legend = c("Bronx","Kings","New York","Queens","Richmond"), 
       col = c("grey","orange","blue","forest green","brown"), 
       pch = c(15,15),
       bty = "n",
       pt.cex = 2,
       cex = 1.2,
       text.col = "black",
       horiz = F,
       inset = c(0.1, 0.1))


######################################### 
####### Interrupted rearrest Model ######
#########################################
mydata2<-mydata

mydata2<-filter(mydata2,mydata2$Jail.Start.Days >=0)
mydata2$Jail.Start.Days<-as.numeric(as.character(mydata2$Jail.Start.Days))
mydata2<-filter(mydata2,mydata2$Jail.Start.Days <=370)
mydata2<- filter(mydata2,mydata2$End.Jail.Days !="#VALUE!")
mydata2$End.Jail.Days <-as.numeric(as.character(mydata2$End.Jail.Days))
mydata2<- filter(mydata2,mydata2$End.Jail.Days <= 370)

time1<-mydata2$Jail.Start.Days
as.numeric(as.character(mydata2$time1))

time2<-mydata2$End.Jail.Days
as.numeric(as.character(mydata2$time2))

event2<-mydata2$dispo.Event
group2<-mydata2$county

attach(mydata2)

nasurvival <- survfit(Surv(time2, time1, event2, type= 'interval')~group2)
summary(nasurvival)
plot(nasurvival,col = c("grey","orange","blue","forest green","brown"),main = "Pre-Trial Detention Survival (w/ re-arrest)", xlab="Days Detained", ylab="Pre-trial Survival")
legend("topright", 
       legend = c("Bronx","Kings","New York","Queens","Richmond"), 
       col = c("grey","orange","blue","forest green","brown"), 
       pch = c(15,15),
       bty = "n",
       pt.cex = 2,
       cex = 1.2,
       text.col = "black",
       horiz = F,
       inset = c(0.1, 0.1))


################################## 
######## Case Length Model #######
##################################
mydata3<-mydata

attach(mydata3)

#caselength
mydata3<-filter(mydata3,mydata3$Case.Length != "#VALUE!")
mydata3<-filter(mydata3,mydata3$Case.Length != "Active")
mydata3<-filter(mydata3,mydata3$Case.Length != "NA")
mydata3$Case.Length<-as.numeric(as.character(mydata3$Case.Length))
mydata3 <- filter(mydata3, mydata3$Case.Length >=0)
mydata3 <- filter(mydata3, mydata3$Case.Length<=370)
timecase<-mydata3$Case.Length
timecase<-as.numeric(as.character(mydata3$Case.Length))
group3<-mydata3$county
event3<-mydata3$dispo.Event

nasurvival3 <- survfit(Surv(timecase,event3)~group3)
summary(nasurvival3)
plot(nasurvival3,col = c("grey","orange","blue","forest green","brown"), main = "Case Duration Survival",xlab="Case Length", ylab="Case Survival")
legend("topright", 
       legend = c("Bronx","Kings","New York","Queens","Richmond"), 
       col = c("grey","orange","blue","forest green","brown"), 
       pch = c(15,15),
       bty = "n",
       pt.cex = 2,
       cex = 1.2,
       text.col = "black",
       horiz = F,
       inset = c(0.1, 0.1))

################################### 
####### Queens Waiver Modesl ######
###################################
mydataQW<-mydata

attach(mydataQW)

mydataQW<-filter(mydataQW,mydataQW$Queens==1)
mydataQW<-filter(mydataQW,mydataQW$Days.Detained !="NULL")
mydataQW$Days.Detained<-as.numeric(as.character(mydataQW$Days.Detained))
mydataQW<-filter(mydataQW, mydataQW$Days.Detained <=350)
mydataQW<-filter(mydataQW, mydataQW$Ar.group == 1)

timeQW<-mydataQW$Days.Detained
as.numeric(as.character(timeQW))
groupQW<-mydataQW$waiver
eventQW<-mydataQW$dispo.Event


nasurvivalQW <- survfit(Surv(timeQW,eventQW)~groupQW)
summary(nasurvivalQW)
plot(nasurvivalQW,col = c("purple","forest green"), main = "Pre-Trial Detention Survival Queens", xlab="Days Detained", ylab="Pre-trial Survival")
legend("topright", 
       legend = c("no waiver","waiver"), 
       col = c("purple","forest green"), 
       pch = c(15,15),
       bty = "n",
       pt.cex = 2,
       cex = 1.2,
       text.col = "black",
       horiz = F,
       inset = c(0.1, 0.1))


#Waiver On Case Duration
mydataQW2<- mydataQW
attach(mydataQW2)

mydataQW2$Case.Length <- as.numeric(as.character(mydataQW2$Case.Length))
mydataQW2<- filter(mydataQW2, mydataQW2$Case.Length <= 350)
mydataQW2<- filter(mydataQW2, mydataQW2$Case.Length >=0)

timeQW2<-mydataQW2$Case.Length
as.numeric(as.character(timeQW2))
groupQW2<-mydataQW2$waiver
eventQW2<-mydataQW2$dispo.Event



nasurvivalQW2 <- survfit(Surv(timeQW2,eventQW2)~groupQW2)
summary(nasurvivalQW2)
plot(nasurvivalQW2,col = c("purple","forest green"), main = "Case Duration Queens", xlab="Case Duration", ylab="Case Survival")
legend("topright", 
       legend = c("no waiver","waiver"), 
       col = c("purple","forest green"), 
       pch = c(15,15),
       bty = "n",
       pt.cex = 2,
       cex = 1.2,
       text.col = "black",
       horiz = F,
       inset = c(0.1, 0.1))

# WAIVER vs 4 Boros
attach(mydata)
mydata4<-mydata
mydata4<-filter(mydata4,mydata4$Days.Detained !="NULL")
mydata4$Days.Detained<-as.numeric(as.character(mydata4$Days.Detained))
mydata4<-filter(mydata4, mydata4$Days.Detained <=350)


time4<-mydata4$Days.Detained
as.numeric(as.character(time4))
group4<-as.factor(as.character(mydata4$county.waiver))
event4<-mydata4$dispo.Event


nasurvival4 <- survfit(Surv(time4,event4)~group4)
summary(nasurvival4)
plot(nasurvival4,col = c("grey","orange","blue","purple","forest green","brown"), main = "Pre-Trial Detention Survival", xlab="Days Detained", ylab="Pre-trial Survival")
legend("topright", 
       legend = c("Bronx","Kings","New York","Queens waiver","Queens no waiver","Richmond"), 
       col = c("grey","orange","blue","forest green","purple","brown"), 
       pch = c(15,15),
       bty = "n",
       pt.cex = 2,
       cex = 1.2,
       text.col = "black",
       horiz = F,
       inset = c(0.1, 0.1))



####################################################### 
############## Survival Regression Model ##############
####################################################### 

attach(mydata1)

#independent variables
Xd<- cbind(client.age.at.incident, Male, Kings, Queens, Richmond, Bronx, New.York,
           ApiNatO,Black.Hisp, Black.Only, Hispanic, White.Hisp, White.Only, waiver
           )

#dependent variables
coxphd <- coxph(Surv(time0, event)~ Xd, method= "breslow")
summary(coxphd)


attach(mydata3)
#independent variables
Xc<- cbind(client.age.at.incident, Male, Kings, Queens, Richmond, Bronx, New.York,
           ApiNatO,Black.Hisp, Black.Only, Hispanic, White.Hisp, White.Only, Ar.group
)

#dependent variables
coxphc <- coxph(Surv(timecase, event3)~ Xc, method= "breslow")
summary(coxphc)


####################################################### 
############### Probit Regression Model ###############
####################################################### 


attach(mydata)
mydatad<- mydata

mydatad<- filter(mydatad, mydatad$final.dispo =="PG"| final.dispo=="DISMISSED")
mydatad$final.dispo <- as.factor(as.character(mydatad$final.dispo))

mydatad<- mutate(mydatad, PG = ifelse((mydatad$final.dispo == "PG"),1,0))
mydatad$PG <- as.numeric(as.character(mydatad$PG))
attach(mydatad)

X<- cbind(mydatad$client.age.at.incident, mydatad$Male, mydatad$Kings, mydatad$Queens, mydatad$Richmond, mydatad$Bronx, mydatad$New.York,
          mydatad$ApiNatO,mydatad$Black.Hisp, mydatad$Black.Only, mydatad$Hispanic, mydatad$White.Hisp, mydatad$White.Only, mydatad$waiver)

lmreg<- glm(PG ~  Kings+ Queens+ Richmond+ Bronx+ New.York +
              ApiNatO+Black.Hisp+ Black.Only+Hispanic+ White.Hisp+ White.Only+ waiver, family = binomial(link="probit"), data = mydatad)
summary(lmreg)
nagelkerke(lmreg)




#ggsurvplot(survfit(coxph), color = "#2E9FDF", ggtheme = theme_minimal())