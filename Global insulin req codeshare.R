# Global insulin requirements
# (c) 2018, Sanjay Basu, basus@stanford.edu

install.packages("tidyverse")
install.packages("survey")
install.packages("fitdistrplus")
install.packages("rworldmap")
install.packages("ggplot2")
install.packages('RColorBrewer')
install.packages('classInt')
install.packages('mgcv')
rm(list=ls())
library(tidyverse)
setwd("~/Data/insulin")
dmprev <- read_csv("DM_age_standardised_countries.csv")
cntlist = as.vector(dmprev$`Country/Region/World`)
cntlist = unique(cntlist)
save(cntlist,file="cntlist")
dmcnt = read_csv(URLencode(paste("individual-countries/",cntlist[1],".csv",sep="")))
for (i in 2:length(cntlist)){
  url = URLencode(paste("individual-countries/",cntlist[i],".csv",sep=""))
  newdat=read_csv(url)
  dmcnt <- dmcnt %>%
    bind_rows(newdat)
}
save(dmcnt,file="dmcnt")
bmi=read_csv("BMI_age_standardised_country.csv",
             local = locale(encoding = "latin1"))
save(bmi,file="bmi")
height=read_csv("height_countries.csv")
save(height,file="height")


rm(list=ls())
setwd("~/Data/insulin")
load("~/Data/insulin/dmcnt")
library(tidyverse)
library(mgcv)
dmcnt=dmcnt[complete.cases(dmcnt),]
cntlist = as.vector(dmcnt$Country)
cntlist = unique(cntlist)
year = dmcnt$Year[dmcnt$Country=="Afghanistan"&dmcnt$Sex=="Men"]
dmcnt_samp = dmcnt$`Number of adults with diabetes`[dmcnt$Country=="Afghanistan"&dmcnt$Sex=="Men"]
mod=gam(dmcnt_samp ~ s(year))
dmcnt_samphi = dmcnt$`Number_Upper 95% uncertainty interval`[dmcnt$Country=="Afghanistan"&dmcnt$Sex=="Men"]
modhi=gam(dmcnt_samphi ~ s(year))
dmcnt_samplo = dmcnt$`Number_Lower 95% uncertainty interval`[dmcnt$Country=="Afghanistan"&dmcnt$Sex=="Men"]
modlo=gam(dmcnt_samplo ~ s(year))
x = 1980:2018
modpred = predict(mod, data.frame(year = x), se = TRUE)
modpredhi = predict(modhi, data.frame(year = x), se = TRUE)
modpredlo = predict(modlo, data.frame(year = x), se = TRUE)
dmcnt_est_m = modpred$fit
dmcnt_esthi_m = modpredhi$fit
dmcnt_estlo_m = modpredlo$fit
plot.new()
png("plotset10_men.png",width=838,height=1152)
par(mfrow=c(5,2))
plot(x,modpred$fit,type='l',xlab="Year",ylab="Adults with diabetes",main="Afghanistan, men",ylim=c(0,max(1.1*(modpredhi$fit))))
lines(year,dmcnt_samp,type='o')
lines(x,modpredhi$fit,lty=5)
lines(x,modpredlo$fit,lty=5)
for (i in 2:length(cntlist)){
  year = dmcnt$Year[dmcnt$Country==paste(cntlist[i])&dmcnt$Sex=="Men"]
  dmcnt_samp = dmcnt$`Number of adults with diabetes`[dmcnt$Country==paste(cntlist[i])&dmcnt$Sex=="Men"]
  mod=gam(dmcnt_samp ~ s(year))
  dmcnt_samphi = dmcnt$`Number_Upper 95% uncertainty interval`[dmcnt$Country==paste(cntlist[i])&dmcnt$Sex=="Men"]
  modhi=gam(dmcnt_samphi ~ s(year))
  dmcnt_samplo = dmcnt$`Number_Lower 95% uncertainty interval`[dmcnt$Country==paste(cntlist[i])&dmcnt$Sex=="Men"]
  modlo=gam(dmcnt_samplo ~ s(year))
  modpred = predict(mod, data.frame(year = x), se = TRUE)
  modpredhi = predict(modhi, data.frame(year = x), se = TRUE)
  modpredlo = predict(modlo, data.frame(year = x), se = TRUE)
  if((max(i==(seq(11,length(cntlist),10)))==1)|(i==length(cntlist))){
    png(paste("plotset",i+9,"_men.png",sep=""),width=838,height=1152)
    par(mfrow=c(5,2))
  }
  plot(x,modpred$fit,type='l',xlab="Year",ylab="Adults with diabetes",main=paste(cntlist[i],", men",sep=""), ylim=c(0,max(1.1*(modpredhi$fit))))
  lines(year,dmcnt_samp,type='o')
  lines(x,modpredhi$fit,lty=5)
  lines(x,modpredlo$fit,lty=5)
  dmcnt_est_m <- dmcnt_est_m %>%
    bind_rows(modpred$fit)
  dmcnt_esthi_m <- dmcnt_esthi_m %>%
    bind_rows(modpredhi$fit)
  dmcnt_estlo_m <- dmcnt_estlo_m %>%
    bind_rows(modpredlo$fit)
  if((max(i==(seq(10,length(cntlist),10)))==1)|(i==length(cntlist))){
         dev.off()
  }
}
dmcnt_samp = dmcnt$`Number of adults with diabetes`[dmcnt$Country=="Afghanistan"&dmcnt$Sex=="Women"]
mod=gam(dmcnt_samp ~ s(year))
dmcnt_samphi = dmcnt$`Number_Upper 95% uncertainty interval`[dmcnt$Country=="Afghanistan"&dmcnt$Sex=="Women"]
modhi=gam(dmcnt_samphi ~ s(year))
dmcnt_samplo = dmcnt$`Number_Lower 95% uncertainty interval`[dmcnt$Country=="Afghanistan"&dmcnt$Sex=="Women"]
modlo=gam(dmcnt_samplo ~ s(year))
modpred = predict(mod, data.frame(year = x), se = TRUE)
modpredhi = predict(modhi, data.frame(year = x), se = TRUE)
modpredlo = predict(modlo, data.frame(year = x), se = TRUE)
dmcnt_est_w = modpred$fit
dmcnt_esthi_w = modpredhi$fit
dmcnt_estlo_w = modpredlo$fit
plot.new()
png("plotset10_women.png",width=838,height=1152)
par(mfrow=c(5,2))
plot(x,modpred$fit,type='l',xlab="Year",ylab="Adults with diabetes",main="Afghanistan, women",ylim=c(0,max(1.1*(modpredhi$fit))))
lines(year,dmcnt_samp,type='o')
lines(x,modpredhi$fit,lty=5)
lines(x,modpredlo$fit,lty=5)
for (i in 2:length(cntlist)){
  year = dmcnt$Year[dmcnt$Country==paste(cntlist[i])&dmcnt$Sex=="Women"]
  dmcnt_samp = dmcnt$`Number of adults with diabetes`[dmcnt$Country==paste(cntlist[i])&dmcnt$Sex=="Women"]
  mod=gam(dmcnt_samp ~ s(year))
  dmcnt_samphi = dmcnt$`Number_Upper 95% uncertainty interval`[dmcnt$Country==paste(cntlist[i])&dmcnt$Sex=="Women"]
  modhi=gam(dmcnt_samphi ~ s(year))
  dmcnt_samplo = dmcnt$`Number_Lower 95% uncertainty interval`[dmcnt$Country==paste(cntlist[i])&dmcnt$Sex=="Women"]
  modlo=gam(dmcnt_samplo ~ s(year))
  modpred = predict(mod, data.frame(year = x), se = TRUE)
  modpredhi = predict(modhi, data.frame(year = x), se = TRUE)
  modpredlo = predict(modlo, data.frame(year = x), se = TRUE)
  if((max(i==(seq(11,length(cntlist),10)))==1)|(i==length(cntlist))){
    png(paste("plotset",i+9,"_women.png",sep=""),width=838,height=1152)
    par(mfrow=c(5,2))
  }
  plot(x,modpred$fit,type='l',xlab="Year",ylab="Adults with diabetes",main=paste(cntlist[i],", women",sep=""), ylim=c(0,max(1.1*(modpredhi$fit))))
  lines(year,dmcnt_samp,type='o')
  lines(x,modpredhi$fit,lty=5)
  lines(x,modpredlo$fit,lty=5)
  dmcnt_est_w <- dmcnt_est_m %>%
    bind_rows(modpred$fit)
  dmcnt_esthi_w <- dmcnt_esthi_m %>%
    bind_rows(modpredhi$fit)
  dmcnt_estlo_w <- dmcnt_estlo_m %>%
    bind_rows(modpredlo$fit)
  if((max(i==(seq(10,length(cntlist),10)))==1)|(i==length(cntlist))){
    dev.off()
  }
}
save(dmcnt_est_m,file="dmcnt_est_m")
save(dmcnt_esthi_m,file="dmcnt_esthi_m")
save(dmcnt_estlo_m,file="dmcnt_estlo_m")
save(dmcnt_est_w,file="dmcnt_est_w")
save(dmcnt_esthi_w,file="dmcnt_esthi_w")
save(dmcnt_estlo_w,file="dmcnt_estlo_w")


# sample from distribution of A1c values

rm(list=ls())
library(fitdistrplus)
setwd("~/Data/insulin")
load("cohorts")
cohorts$dm = (cohorts$DIQ010==1) | 
  (cohorts$LBXGH>=6.5) | 
  (cohorts$LBXGLU>=126)
cohorts <- cohorts[which(cohorts$dm==1), ]
cohortscomp = cohorts[(complete.cases(cohorts$WTSAF2YR)&complete.cases(cohorts$LBXGH)),]
table(cohortscomp$dm)

# a1c values among diagnosed and treated with insulin
a1cdist_dx_rxins = fitdist(cohortscomp$LBXGH[cohortscomp$DIQ050==1], "lnorm", method="mme",weights=round(cohortscomp$WTSAF2YR[cohortscomp$DIQ050==1]/3))
set.seed(100)
a1csamp_dx_rxins=rlnorm(1e5,meanlog=a1cdist_dx_rxins$estimate[1],sdlog = a1cdist_dx_rxins$estimate[2])
quantile(a1csamp_dx_rxins,c(.025,.5,.975))
mean(a1csamp_dx_rxins)

# a1c values among diagnosed and treated with only orals
a1cdist_dx_rxoral = fitdist(as.numeric(na.omit(cohortscomp$LBXGH[(cohortscomp$DIQ070==1)&(cohortscomp$DIQ050==2)])), "lnorm", method="mme",weights=as.numeric(na.omit(round(cohortscomp$WTSAF2YR[(cohortscomp$DIQ070==1)&(cohortscomp$DIQ050==2)]/3))))
set.seed(100)
a1csamp_dx_rxoral=rlnorm(1e5,meanlog=a1cdist_dx_rxoral$estimate[1],sdlog = a1cdist_dx_rxoral$estimate[2])
quantile(a1csamp_dx_rxoral,c(.025,.5,.975))
mean(a1csamp_dx_rxoral)

# a1c values among diagnosed and untreated
a1cdist_dx_norx = fitdist(as.numeric(na.omit(cohortscomp$LBXGH[(cohortscomp$DIQ070==2)&(cohortscomp$DIQ050==2)])), "lnorm", method="mme",weights=as.numeric(na.omit(round(cohortscomp$WTSAF2YR[(cohortscomp$DIQ070==2)&(cohortscomp$DIQ050==2)]/3))))
set.seed(100)
a1csamp_dx_norx=rlnorm(1e5,meanlog=a1cdist_dx_norx$estimate[1],sdlog = a1cdist_dx_norx$estimate[2])
quantile(a1csamp_dx_norx,c(.025,.5,.975))
mean(a1csamp_dx_norx)

# a1c values among undiagnosed 
a1cdist_nodx = fitdist(as.numeric(na.omit(cohortscomp$LBXGH[(cohortscomp$DIQ010==2)])), "lnorm", method="mme",weights=as.numeric(na.omit(round(cohortscomp$WTSAF2YR[(cohortscomp$DIQ010==2)]/3))))
set.seed(100)
a1csamp_nodx=rlnorm(1e5,meanlog=a1cdist_nodx$estimate[1],sdlog = a1cdist_nodx$estimate[2])
quantile(a1csamp_nodx,c(.025,.5,.975))
mean(a1csamp_nodx)



# simple approach ----

rm(list=ls())
library(mgcv)
library(tidyverse)
library(readxl)
undx <- read_excel("~/Documents/Epi/Research/NCDs/T2DM/Global insulin req/Global insulin req refs & data/IDF estimates.xlsx",
sheet = "undx", col_types = c("text",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric"))

number <- read_excel("~/Documents/Epi/Research/NCDs/T2DM/Global insulin req/Global insulin req refs & data/IDF estimates.xlsx",
sheet = "number", col_types = c("text",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "text", "numeric", "numeric",
"numeric", "numeric", "numeric"))

lower <- read_excel("~/Documents/Epi/Research/NCDs/T2DM/Global insulin req/Global insulin req refs & data/IDF estimates.xlsx",
sheet = "lower", col_types = c("text",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "text", "numeric", "numeric",
"numeric", "numeric", "numeric"))


upper <- read_excel("~/Documents/Epi/Research/NCDs/T2DM/Global insulin req/Global insulin req refs & data/IDF estimates.xlsx",
sheet = "upper", col_types = c("text",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "text", "numeric", "numeric",
"numeric", "numeric", "numeric"))


cntreg <- read_excel("~/Documents/Epi/Research/NCDs/T2DM/Global insulin req/Global insulin req refs & data/IDF estimates.xlsx",sheet = "countryregion")

undx = left_join(undx,cntreg,c("Country","Country"))
number = left_join(number,cntreg,c("Country","Country"))
lower = left_join(lower,cntreg,c("Country","Country"))
upper = left_join(upper,cntreg,c("Country","Country"))


cntnms = unique(undx$Country)

n_dm = undx
n_dmlo = undx
n_dmhi = undx

for (i in 1:length(cntnms)){
  n_dm[i,2:17]=1000*as.numeric(number[i,2:17])*0.965
  n_dmlo[i,2:17]=1000*as.numeric(lower[i,2:17])*0.92
  n_dmhi[i,2:17]=1000*as.numeric(upper[i,2:17])*0.99
}

tab00 = n_dm %>%
  group_by(undx$Region) %>%
  summarise(n_dm_18 = sum(`2018`,na.rm = T),
            n_dm_30 = sum(`2030`,na.rm = T))
tab00

tab00 = n_dmlo %>%
  group_by(undx$Region) %>%
  summarise(n_dm_18 = sum(`2018`,na.rm = T),
            n_dm_30 = sum(`2030`,na.rm = T))
tab00

tab00 = n_dmhi %>%
  group_by(undx$Region) %>%
  summarise(n_dm_18 = sum(`2018`,na.rm = T),
            n_dm_30 = sum(`2030`,na.rm = T))
tab00


           
proponins = data.table::data.table(propins = c(4.4,22.2,13.9,12.4,13.2),Region = c("Africa","Americas","Asia","Europe","Oceania"))
proponins = left_join(undx,proponins,c("Region","Region"))
proponins$propins[is.na(proponins$propins)==1]=13.23

prop_ins=undx
prop_inslo=undx
prop_inshi=undx

for (i in 1:length(cntnms)){
  prop_ins[i,2:17]=as.vector(100-as.numeric(undx[i,2:17]))/100*proponins$propins[i]/100
  prop_inslo[i,2:17]=as.vector(100-as.numeric(undx[i,2:17]))/100*proponins$propins[i]/100
  prop_inshi[i,2:17]=as.vector(100-as.numeric(undx[i,2:17]))/100*proponins$propins[i]/100
}

tab0 = prop_ins %>%
  group_by(Region) %>%
  summarise(prop_ins = sum(`2030`,na.rm = T))
tab0


n_ins=undx
n_inslo=undx
n_inshi=undx

for (i in 1:length(cntnms)){
  n_ins[i,2:17]=1000*as.numeric(number[i,2:17])*0.965*as.vector(100-as.numeric(undx[i,2:17]))/100*proponins$propins[i]/100
  n_inslo[i,2:17]=1000*as.numeric(lower[i,2:17])*0.92*as.vector(100-as.numeric(undx[i,2:17]))/100*proponins$propins[i]/100
  n_inshi[i,2:17]=1000*as.numeric(upper[i,2:17])*0.99*as.vector(100-as.numeric(undx[i,2:17]))/100*proponins$propins[i]/100
  # number with T2DM * diagnosed * prop on insulin
}

load("~/Data/insulin/bmi")
load("~/Data/insulin/height")
weight_m = bmi$`Mean BMI`[(bmi$Year==2016)&(bmi$Sex=="Men")]*((height$`Mean height (cm)`[(height$`Year of birth`==(2016-40))&(height$Sex=="Men")]/100)^2)
weight_f = bmi$`Mean BMI`[(bmi$Year==2016)&(bmi$Sex=="Women")]*((height$`Mean height (cm)`[(height$`Year of birth`==(2016-40))&(height$Sex=="Women")]/100)^2)
weightmean = cbind(unique(bmi$`Country/Region/World`),as.numeric(weight_m)+as.numeric(weight_f)/2)
colnames(weightmean)=c("Country","Weight")
weightmean=as.tibble(weightmean)
n_ins =as.tibble(n_ins)
n_inslo =as.tibble(n_inslo)
n_inshi =as.tibble(n_inshi)

data_one = left_join(n_ins, weightmean,c("Country"="Country"))
data_onelo = left_join(n_inslo, weightmean,c("Country"="Country"))
data_onehi = left_join(n_inshi, weightmean,c("Country"="Country"))

# The body weight-based dosing estimated that 75% of those treated with insulin 
# require basal insulin only at a dosage of 0.4 IU/kg/day, 
# while the remaining individuals require multiple dose injection therapy 
# totaling 0.6 IU/kg/day.

units_ins = cbind(data_one$Country,data_one[,5:17]*as.numeric(data_one$Weight)*(.75*.4+.25*.6),data_one$Region)
units_inslo = cbind(data_onelo$Country,data_onelo[,5:17]*as.numeric(data_onelo$Weight)*(.75*.4+.25*.6),data_onelo$Region)
units_inshi = cbind(data_onehi$Country,data_onehi[,5:17]*as.numeric(data_onehi$Weight)*(.75*.4+.25*.6),data_onehi$Region)

colnames(units_ins) = c("Country",2018:2030,"Region")
colnames(units_inslo) = c("Country",2018:2030,"Region")
colnames(units_inshi) = c("Country",2018:2030,"Region")

# per yr

tab1 = data_one %>%
  group_by(Region) %>%
  summarise(n_ins_18 = sum(`2018`,na.rm = T),
            n_ins_30 = sum(`2030`,na.rm = T))
tab1


tab1b = units_ins %>%
  group_by(Region) %>%
  summarise(vials_ins_18 = sum(`2018`,na.rm = T)*365.25/1000,
            vials_ins_30 = sum(`2030`,na.rm = T)*365.25/1000)
tab1b



tab1lo = data_onelo %>%
  group_by(Region) %>%
  summarise(n_ins_18 = sum(`2018`,na.rm = T),
            n_ins_30 = sum(`2030`,na.rm = T))
tab1lo


tab1blo = units_inslo %>%
  group_by(Region) %>%
  summarise(vials_ins_18 = sum(`2018`,na.rm = T)*365.25/1000,
            vials_ins_30 = sum(`2030`,na.rm = T)*365.25/1000)
tab1blo




tab1hi = data_onehi %>%
  group_by(Region) %>%
  summarise(n_ins_18 = sum(`2018`,na.rm = T),
            n_ins_30 = sum(`2030`,na.rm = T))
tab1hi


tab1bhi = units_inshi %>%
  group_by(Region) %>%
  summarise(vials_ins_18 = sum(`2018`,na.rm = T)*365.25/1000,
            vials_ins_30 = sum(`2030`,na.rm = T)*365.25/1000)
tab1bhi



# complex approach ----

library(haven)
setwd("~/Data/GHCoE/ghcoe_new_delhi/ra01/Data")
indiage = read_sas("ds06coe_ra01_participantdetails.sas7bdat")
indiaa1c = read_sas("ds06coe_ra01_lab.sas7bdat")
indiadm = read_sas("ds06coe_ra01_diabetes.sas7bdat")
indiawt = read_sas("ds06coe_ra01_anthropometry.sas7bdat")
library(fitdistrplus)
india = left_join(indiage,indiaa1c,by=c("PID","PID"))
india = left_join(india,indiadm,by=c("PID","PID"))
india = left_join(india,indiawt,by=c("PID","PID"))

india$dm = india$PD_DIABETES.y==1
india <- india[which(india$dm==1), ]
indiacomp = india[(complete.cases(india$LAB_HBA1C)),]
table(indiacomp$dm)

# a1c values among diagnosed 
a1cdist_dx_rxins = fitdist(indiacomp$LAB_HBA1C, "lnorm", method="mme")
set.seed(100)
a1csamp_dx_rxins=rlnorm(1e5,meanlog=a1cdist_dx_rxins$estimate[1],sdlog = a1cdist_dx_rxins$estimate[2])
quantile(a1csamp_dx_rxins,c(.025,.5,.975))
mean(a1csamp_dx_rxins)


setwd("~/Data/GHCoE/ghcoe_south_africa/ra01/data")
safricaa1c = read_sas("ra01_20140328_calculatedvars.sas7bdat")
safricadm = read_sas("ra01_20140328_2012_followup.sas7bdat")
safricawt = read_sas("ra01_20140328_2011_as2data.sas7bdat")

library(fitdistrplus)
safrica = left_join(safricaa1c,safricadm,by=c("NHLBI_ID","NHLBI_ID"))
safrica = left_join(safrica,safricawt,by=c("NHLBI_ID","NHLBI_ID"))

safrica$dm = safrica$DiabetesCohort==1
safrica <- safrica[which(safrica$dm==1), ]
safricacomp = safrica[(complete.cases(safrica$hba1c_2011_updated)),]
table(safricacomp$dm)

# a1c values among diagnosed 
a1cdist_dx_rxins = fitdist(safricacomp$hba1c_2011_updated, "lnorm", method="mme")
set.seed(100)
a1csamp_dx_rxins=rlnorm(1e5,meanlog=a1cdist_dx_rxins$estimate[1],sdlog = a1cdist_dx_rxins$estimate[2])
quantile(a1csamp_dx_rxins,c(.025,.5,.975))
mean(a1csamp_dx_rxins)

setwd("~/Data/insulin")
load("cohorts")
cohorts$dm = (cohorts$DIQ010==1) | 
  (cohorts$LBXGH>=6.5) | 
  (cohorts$LBXGLU>=126)
cohorts <- cohorts[which((cohorts$dm==1)), ]
cohortscomp = cohorts[(complete.cases(cohorts$WTSAF2YR)&complete.cases(cohorts$LBXGH)),]
table(cohortscomp$dm)


# calculate proportion above target a1c given max titration of oral hypos, A1c reduction 1.5%
cuma1cdist = c((cohorts$LBXGH),
               (safrica$hba1c_2011_updated),
               (india$LAB_HBA1C))
cumages = c((cohorts$RIDAGEYR),safrica$age,india$PD_AGE)



targeta1c = 7  #7*(cumages<75) + 8*(cumages>=75)
propa1cabovetarget = (
  ((cohorts$LBXGH*(cohorts$DIQ050==1)*(cohorts$DIQ070==1))>targeta1c)+
  ((cohorts$LBXGH*(cohorts$DIQ050==1)*(cohorts$DIQ070!=1))>targeta1c)+
  ((cohorts$LBXGH*(cohorts$DIQ050!=1)*(cohorts$DIQ070==1))>targeta1c)+
  ((cohorts$LBXGH*(cohorts$DIQ050!=1)*(cohorts$DIQ070!=1)-3)>targeta1c)
  )

propa1cabovetarget = sum(propa1cabovetarget,na.rm=T)/length(propa1cabovetarget)

for (i in 1:length(cntnms)){
  n_ins[i,2:17]=1000*as.numeric(number[i,2:17])*0.965*as.vector(100-as.numeric(undx[i,2:17]))/100*propa1cabovetarget
  n_inslo[i,2:17]=1000*as.numeric(lower[i,2:17])*0.92*as.vector(100-as.numeric(undx[i,2:17]))/100*propa1cabovetarget
  n_inshi[i,2:17]=1000*as.numeric(upper[i,2:17])*0.99*as.vector(100-as.numeric(undx[i,2:17]))/100*propa1cabovetarget
  # number with T2DM * diagnosed * prop needing insulin
}


n_ins =as.tibble(n_ins)
n_inslo =as.tibble(n_inslo)
n_inshi =as.tibble(n_inshi)

data_two = left_join(n_ins, weightmean,c("Country"="Country"))
data_twolo = left_join(n_inslo, weightmean,c("Country"="Country"))
data_twohi = left_join(n_inshi, weightmean,c("Country"="Country"))

# The body weight-based dosing estimated that 75% of those treated with insulin 
# require basal insulin only at a dosage of 0.3 IU/kg/day, 
# while the remaining individuals require multiple dose injection therapy 
# totaling 0.7 IU/kg/day.

units_ins = cbind(data_two$Country,data_two[,5:17]*as.numeric(data_two$Weight)*(.75*.4+.25*.6),data_two$Region)
units_inslo = cbind(data_twolo$Country,data_twolo[,5:17]*as.numeric(data_twolo$Weight)*(.75*.4+.25*.6),data_twolo$Region)
units_inshi = cbind(data_twohi$Country,data_twohi[,5:17]*as.numeric(data_twohi$Weight)*(.75*.4+.25*.6),data_twohi$Region)

colnames(units_ins) = c("Country",2018:2030,"Region")
colnames(units_inslo) = c("Country",2018:2030,"Region")
colnames(units_inshi) = c("Country",2018:2030,"Region")




# DALYS ----

# baseline recode microvasc outcomes



load("~/Data/accord/3-Data_Sets-Analysis/3a-Analysis_Data_Sets/accord_dm_models.RData")
setwd("~/Data/insulin")
load("cohorts")


cohorts$dm = (cohorts$DIQ010==1) | 
  (cohorts$LBXGH>=6.5) | 
  (cohorts$LBXGLU>=126)
cohorts <- cohorts[which((cohorts$dm==1)), ]

baseline_age = cohorts$RIDAGEYR
female = cohorts$RIAGENDR==2
black = cohorts$RIDRETH1==4
hisp = cohorts$RIDRETH1<3
tob = cohorts$SMQ040<3
sbp = cohorts$BPXSY1
bprx = cohorts$BPQ050A==1
oraldmrx = cohorts$DIQ070==1
anti_coag = 0
cvd_hx_baseline = (cohorts$MCQ160E==1)|(cohorts$MCQ160F==1)
set.seed(100)
hba1c = cohorts$LBXGH
#rlnorm(length(baseline_age),meanlog=a1cdist_dx_rxins$estimate[1],sdlog = a1cdist_dx_rxins$estimate[2])
chol = cohorts$LBXTC
hdl = cohorts$LBDHDD
screat = cohorts$LBXSCR
uacr = cohorts$URDACT
alt = cohorts$LBXSATSI
wt = cohorts$BMXWT
yrsdiab =  baseline_age - cohorts$DID040
yrsdiab[yrsdiab<0] = 0

###### Nephropathy ######
betax = (survcox_neph3$coefficients[1]*baseline_age+
           survcox_neph3$coefficients[2]*female+
           survcox_neph3$coefficients[3]*black+
           survcox_neph3$coefficients[4]*hisp+
           survcox_neph3$coefficients[5]*tob+
           survcox_neph3$coefficients[9]*sbp+
           survcox_neph3$coefficients[10]*bprx+
           survcox_neph3$coefficients[11]*oraldmrx+
           survcox_neph3$coefficients[12]*anti_coag+
           survcox_neph3$coefficients[13]*cvd_hx_baseline+
           survcox_neph3$coefficients[14]*hba1c+
           survcox_neph3$coefficients[15]*chol+
           survcox_neph3$coefficients[16]*hdl+
           survcox_neph3$coefficients[17]*screat+
           survcox_neph3$coefficients[18]*uacr)
esrdrisk = 1-.973^exp(betax-mean(betax,na.rm=T))





##### Retinopathy #####

betax = (survcox_retin4$coefficients[1]*baseline_age+
           survcox_retin4$coefficients[2]*female+
           survcox_retin4$coefficients[3]*black+
           survcox_retin4$coefficients[4]*sbp+
           survcox_retin4$coefficients[5]*bprx+
           survcox_retin4$coefficients[6]*oraldmrx+
           survcox_retin4$coefficients[7]*cvd_hx_baseline+
           survcox_retin4$coefficients[8]*hba1c+
           survcox_retin4$coefficients[9]*chol+
           survcox_retin4$coefficients[10]*hdl+
           survcox_retin4$coefficients[11]*screat+
           survcox_retin4$coefficients[11]*uacr)
retinrisk = 1 - .92^exp(betax-mean(betax,na.rm=T))



##### Neuropathy #####
betax = (survcox_neuro4$coefficients[1]*baseline_age+
           survcox_neuro4$coefficients[2]*female+
           survcox_neuro4$coefficients[3]*black+
           survcox_neuro4$coefficients[4]*sbp+
           survcox_neuro4$coefficients[5]*bprx+
           survcox_neuro4$coefficients[6]*oraldmrx+
           survcox_neuro4$coefficients[7]*cvd_hx_baseline+
           survcox_neuro4$coefficients[8]*hba1c+
           survcox_neuro4$coefficients[9]*chol+
           survcox_neuro4$coefficients[10]*hdl+
           survcox_neuro4$coefficients[11]*screat)
neurorisk = 1 - .87^exp(betax-mean(betax,na.rm=T))



##### hypo risk #####

load("~/Documents/Epi/Research/NCDs/T2DM/Global insulin req/hypomodel")
set.seed(100)
fvibrat = rbinom(length(baseline_age),1,neurorisk)
g2avepba = wt*(.75*.4+.25*.6)*(cohorts$DIQ050==1)
set.seed(100)
sulfonylurea = as.numeric(oraldmrx)*rbinom(length(oraldmrx),1,.414)
set.seed(100)
visloss = rbinom(length(baseline_age),1,retinrisk)
a1cbase = hba1c 
a1cdelta = 0
female = as.numeric(female)
newdat = data.frame(sbp,alt,fvibrat,g2avepba,baseline_age,sulfonylurea,female,visloss,screat,a1cdelta,a1cbase,yrsdiab)
newdat[is.na(newdat)==1]=0
hyporisk = predict.glm(mod1b,newdata = newdat,type = c("response"))



##### DALY calcs #####



n = length(baseline_age)
set.seed(100)
util_renal = rnorm(n,.573,(.573-.397)/1.96)
set.seed(100)
util_neuro = rnorm(n,.099,(.099-.066)/1.96)
set.seed(100)
util_retin = rnorm(n,.191,(.191-.129)/1.96)
set.seed(100)
util_hypog = rnorm(n,.054,(.054-.033)/1.96)
set.seed(100)
util_inj = rnorm(n,.009,(.009-.004)/1.96)

K =1
C = 0.1658
r = 0.03
a = baseline_age
beta = 0.04
lifeexp =68.33
L = lifeexp - baseline_age
L[L<0]=0

YLLs = (K*C*exp(r*a)/(r+beta)^2)*
  ((exp(-(r+beta)*(L+a)))*(-(r+beta)*(L+a)-1)-
     (exp(-(r+beta)*a))*(-(r+beta)*a-1))+
  ((1-K)/r)*(1-exp(-r*L))


DALYS_pre= (util_renal*YLLs*esrdrisk+
  util_neuro*YLLs*neurorisk+
  util_retin*YLLs*retinrisk+
  util_hypog*YLLs*hyporisk+
  util_inj*YLLs*mean(proponins$propins)/100)/L



# DALYS after insulin


#targeta1c = 7*(baseline_age<75) + 8*(baseline_age>=75)

deltaa1c = hba1c-targeta1c
deltaa1c[deltaa1c<0]=0

esrdrate= -log(1-esrdrisk)/10
neurorate= -log(1-neurorisk)/10
retinrate= -log(1-retinrisk)/10
hyporate= -log(1-hyporisk)/5

esrdrisk = 1-exp(-esrdrate*L)
neurorisk = 1-exp(-neurorate*L)
retinrisk = 1-exp(-retinrate*L)
hyporisk = 1-exp(-hyporate*L)

esrdrate2 = (esrdrate*((hba1c-deltaa1c)/hba1c)^1.14)
neurorate2 = (neurorate*((hba1c-deltaa1c)/hba1c)^1.19)
retinrate2 = (retinrate*((hba1c-deltaa1c)/hba1c)^1.29)
hyporate2 = (hyporate*((hba1c-deltaa1c)/hba1c)^-7.75)

esrdrisk2 = 1-exp(-esrdrate2*L)
neurorisk2 = 1-exp(-neurorate2*L)
retinrisk2 = 1-exp(-retinrate2*L)
hyporisk2 = 1-exp(-hyporate2*L)


DALYS_post= (util_renal*YLLs*esrdrisk2+
  util_neuro*YLLs*neurorisk2+
  util_retin*YLLs*retinrisk2+
  util_hypog*YLLs*hyporisk2+
  util_inj*YLLs*mean(propa1cabovetarget))/L

delta_DALYS = DALYS_pre-DALYS_post

sum(delta_DALYS,na.rm=T)

dalys=undx
dalyslo=undx
dalyshi=undx

for (i in 1:length(cntnms)){
  dalys[i,2:17]=1000*as.numeric(number[i,2:17])*0.965/length(delta_DALYS)*sum(delta_DALYS,na.rm=T)
  dalyslo[i,2:17]=1000*as.numeric(lower[i,2:17])*0.92/length(delta_DALYS)*sum(delta_DALYS,na.rm=T)
  dalyshi[i,2:17]=1000*as.numeric(upper[i,2:17])*0.99/length(delta_DALYS)*sum(delta_DALYS,na.rm=T)
}


tab1 = data_two %>%
  group_by(Region) %>%
  summarise(n_ins_18 = sum(`2018`,na.rm = T),
            n_ins_30 = sum(`2030`,na.rm = T))
tab1


tab1b = units_ins %>%
  group_by(Region) %>%
  summarise(vials_ins_18 = sum(`2018`,na.rm = T)*365.25/1000,
            vials_ins_30 = sum(`2030`,na.rm = T)*365.25/1000)
tab1b



tab1lo = data_twolo %>%
  group_by(Region) %>%
  summarise(n_ins_18 = sum(`2018`,na.rm = T),
            n_ins_30 = sum(`2030`,na.rm = T))
tab1lo


tab1blo = units_inslo %>%
  group_by(Region) %>%
  summarise(vials_ins_18 = sum(`2018`,na.rm = T)*365.25/1000,
            vials_ins_30 = sum(`2030`,na.rm = T)*365.25/1000)
tab1blo




tab1hi = data_twohi %>%
  group_by(Region) %>%
  summarise(n_ins_18 = sum(`2018`,na.rm = T),
            n_ins_30 = sum(`2030`,na.rm = T))
tab1hi


tab1bhi = units_inshi %>%
  group_by(Region) %>%
  summarise(vials_ins_18 = sum(`2018`,na.rm = T)*365.25/1000,
            vials_ins_30 = sum(`2030`,na.rm = T)*365.25/1000)
tab1bhi

tab1 = dalys %>%
  group_by(Region) %>%
  summarise(dalys_18 = sum(`2018`,na.rm = T),
            dalys_30 = sum(`2030`,na.rm = T))
tab1

tab1lo = dalyslo %>%
  group_by(Region) %>%
  summarise(dalys_18 = sum(`2018`,na.rm = T),
            dalys_30 = sum(`2030`,na.rm = T))
tab1lo

tab1hi = dalyshi %>%
  group_by(Region) %>%
  summarise(dalys_18 = sum(`2018`,na.rm = T),
            dalys_30 = sum(`2030`,na.rm = T))
tab1hi



summary(util_hypog*YLLs*hyporisk2)

summary(util_renal*YLLs*esrdrisk2+
          util_neuro*YLLs*neurorisk2+
          util_retin*YLLs*retinrisk2)

summary(util_hypog*YLLs*hyporisk)

summary(util_renal*YLLs*esrdrisk+
          util_neuro*YLLs*neurorisk+
          util_retin*YLLs*retinrisk)

DALYS_renal = util_renal*YLLs*esrdrisk- util_renal*YLLs*esrdrisk2
sum(DALYS_renal,na.rm=T)

DALYS_neuro = util_neuro*YLLs*neurorisk- util_neuro*YLLs*neurorisk2
sum(DALYS_neuro,na.rm=T)

DALYS_retin = util_retin*YLLs*retinrisk- util_retin*YLLs*retinrisk2
sum(DALYS_retin,na.rm=T)

DALYS_hypog = util_hypog*YLLs*hyporisk- util_hypog*YLLs*hyporisk2+
  util_inj*YLLs*mean(proponins$propins)/100- util_inj*YLLs*mean(propa1cabovetarget)
sum(DALYS_hypog,na.rm=T)

