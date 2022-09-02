library(tidyverse)

mydata<-df_merge
mydata<-mydata[,2:20]
summary(mydata)
View(mydata)
View(sampledata)

#-------------------------------------------------------------------------------

# For Outliers
#library(dlookr)
# diagnose_outlier(mydata)
# plot_outlier(mydata)

#-------------------------------------------------------------------------------

#converting rist status to 0,1,2 format
risk_status<-mydata$STATUS

for(i in 1:9709)
{
  if(risk_status[i]==2) 
    risk_status[i]<-1
  else if(risk_status[i]>2) 
    risk_status[i]<-2
}

mydata$risk_status<-risk_status
mydata<-mydata[,-19]
mydata<-mydata[,-13]


#-------------------------------------------------------------------------------


#converting children thing to binary format of whether a family has children
for(i in 1:9709)
{
 if(mydata$CNT_CHILDREN[i]>0)
   mydata$CNT_CHILDREN[i]<-100
}

mydata$family_with_child<-mydata$CNT_FAM_MEMBERS-mydata$CNT_CHILDREN

for(i in 1:9709)
{
  if(mydata$family_with_child[i]<0)
    mydata$family_with_child[i]<-1
  else
    mydata$family_with_child[i]<-0
  
}
mydata<-mydata[,c(-5,-17)]

#-------------------------------------------------------------------------------

# using sample data
sampledata<-mydata

#-------------------------------------------------------------------------------

# Annual Income

for(i in 1:9709)
{
  sampledata$AMT_INCOME_TOTAL[i]<-sampledata$AMT_INCOME_TOTAL[i]/1000
  
}

summary(sampledata$AMT_INCOME_TOTAL)

quantile(sampledata$AMT_INCOME_TOTAL, probs = c(0.3333,0.6666))

for(i in 1:9709)
{
  if(sampledata$AMT_INCOME_TOTAL[i]<=135)
    sampledata$AMT_INCOME_TOTAL[i]<-"Low"
  else if(sampledata$AMT_INCOME_TOTAL[i]>135 && sampledata$AMT_INCOME_TOTAL[i]<=202.5)
    sampledata$AMT_INCOME_TOTAL[i]<-"Medium"
  else
    sampledata$AMT_INCOME_TOTAL[i]<-"High"
  
}

#-------------------------------------------------------------------------------

# age

for(i in 1:9709)
{
  sampledata$DAYS_BIRTH[i]<-abs(sampledata$DAYS_BIRTH[i]/365)
  sampledata$DAYS_BIRTH[i]<-round(sampledata$DAYS_BIRTH[i])
}

table(sampledata$DAYS_BIRTH)
quantile(sampledata$DAYS_BIRTH, probs = c(0.2,0.4,0.6,0.8))

for(i in 1:9709)
{
  if(sampledata$DAYS_BIRTH[i]<=32)
    sampledata$DAYS_BIRTH[i]<-"Lowest"
  else if(sampledata$DAYS_BIRTH[i]>32 && sampledata$DAYS_BIRTH[i]<=39)
    sampledata$DAYS_BIRTH[i]<-"Low"
  else if(sampledata$DAYS_BIRTH[i]>39 && sampledata$DAYS_BIRTH[i]<=47)
    sampledata$DAYS_BIRTH[i]<-"Medium"
  else if(sampledata$DAYS_BIRTH[i]>47 && sampledata$DAYS_BIRTH[i]<=56)
    sampledata$DAYS_BIRTH[i]<-"High"
  else
    sampledata$DAYS_BIRTH[i]<-"Highest"
    
}

#-------------------------------------------------------------------------------

# working years to working months

for(i in 1:9709)
{
  sampledata$DAYS_EMPLOYED[i]<-abs(sampledata$DAYS_EMPLOYED[i]/12)
}

summary(sampledata$DAYS_EMPLOYED)

quantile(sampledata$DAYS_EMPLOYED, probs = c(0.2,0.4,0.6,0.8))

for(i in 1:9709)
{
  if(sampledata$DAYS_EMPLOYED[i]<=68.8)
    sampledata$DAYS_EMPLOYED[i]<-"Little Experience"
  else if(sampledata$DAYS_EMPLOYED[i]>68.8 && sampledata$DAYS_EMPLOYED[i]<=144.5000)
    sampledata$DAYS_EMPLOYED[i]<-"Less Experience"
  else if(sampledata$DAYS_EMPLOYED[i]>144.5000 && sampledata$DAYS_EMPLOYED[i]<=266.8333)
    sampledata$DAYS_EMPLOYED[i]<-"Medium Experience"
  else if(sampledata$DAYS_EMPLOYED[i]>266.8333 && sampledata$DAYS_EMPLOYED[i]<=710.8500)
    sampledata$DAYS_EMPLOYED[i]<-"High Experience"
  else
    sampledata$DAYS_EMPLOYED[i]<-"Very Experienced"
  
}

table(sampledata$DAYS_EMPLOYED)

#-------------------------------------------------------------------------------
#income type

table(sampledata$NAME_INCOME_TYPE)

for(i in 1:9709)
{
 if(sampledata$NAME_INCOME_TYPE[i]=="Student" || sampledata$NAME_INCOME_TYPE[i]=="Pensioner" || sampledata$NAME_INCOME_TYPE[i]=="State servant")
   sampledata$NAME_INCOME_TYPE[i]<-"State Servant"
}

#-------------------------------------------------------------------------------

#occupation type

for(i in 1:9709)
{
  if(is.na(sampledata$OCCUPATION_TYPE[i]))
    sampledata$OCCUPATION_TYPE[i]<-"Undefined"
}

for(i in 1:9709)
  {  
  if(sampledata$OCCUPATION_TYPE[i]=="Cleaning staff" || sampledata$OCCUPATION_TYPE[i]=="Cooking staff" || sampledata$OCCUPATION_TYPE[i]=="Drivers" ||sampledata$OCCUPATION_TYPE[i]=="Laborers" || sampledata$OCCUPATION_TYPE[i]=="Low-skill Laborers" || sampledata$OCCUPATION_TYPE[i]=="Security staff"|| sampledata$OCCUPATION_TYPE[i]=="Waiters/barmen staff")
    sampledata$OCCUPATION_TYPE[i]<-"Labour Work"
  else if(sampledata$OCCUPATION_TYPE[i]=="Accountants" || sampledata$OCCUPATION_TYPE[i]=="Core staff" || sampledata$OCCUPATION_TYPE[i]=="HR staff" ||sampledata$OCCUPATION_TYPE[i]=="Medicine staff" || sampledata$OCCUPATION_TYPE[i]=="Private service staff" || sampledata$OCCUPATION_TYPE[i]=="Realty agents"|| sampledata$OCCUPATION_TYPE[i]=="Sales staff"|| sampledata$OCCUPATION_TYPE[i]=="Secretaries")           
    sampledata$OCCUPATION_TYPE[i]<-"Office Workers"
  else if(sampledata$OCCUPATION_TYPE[i]=="Managers" || sampledata$OCCUPATION_TYPE[i]=="High skill tech staff" || sampledata$OCCUPATION_TYPE[i]=="IT staff")           
    sampledata$OCCUPATION_TYPE[i]<-"Tech Workers"
  else
    sampledata$OCCUPATION_TYPE[i]<-"Not Defined"
    
}



#-------------------------------------------------------------------------------

# education

for(i in 1:9709)
{
  if(sampledata$NAME_EDUCATION_TYPE[i]=="Academic degree")
    sampledata$NAME_EDUCATION_TYPE[i]<-"Higher education"
}

table(sampledata$NAME_EDUCATION_TYPE)

#-------------------------------------------------------------------------------

#gender

for(i in 1:9709)
{
  if(sampledata$CODE_GENDER[i]=="M")
    sampledata$CODE_GENDER[i]<-0
  else 
    sampledata$CODE_GENDER[i]<-1
}

#-------------------------------------------------------------------------------

#car

for(i in 1:9709)
{
  if(sampledata$FLAG_OWN_CAR[i]=="Y")
    sampledata$FLAG_OWN_CAR[i]<-1
  else 
    sampledata$FLAG_OWN_CAR[i]<-0
}

#-------------------------------------------------------------------------------

#realty

for(i in 1:9709)
{
  if(sampledata$FLAG_OWN_REALTY[i]=="Y")
    sampledata$FLAG_OWN_REALTY[i]<-1
  else 
    sampledata$FLAG_OWN_REALTY[i]<-0
}


write.csv(sampledata,"~/Desktop/IST 707/Project/mydata.csv", row.names = FALSE)
