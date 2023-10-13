---
#code for Chen, T., Oginni, O. A., Hannigan, L. J., Eley, T. C., Maggs, J. L., Linden-Carmichael, A. N., & Neiderhiser, J. M.. Developmental trajectories of child and adolescent emotional problems: Associations with early adult alcohol use behaviors.
author: "Tong Chen"
#script for sex difference test in phenotypic models for AUDIT-Total
---
  
#create separate datasets for male and female
#should first re-read the data if already extracted factor scores
TEDSF<-subset(TEDS, sex1 == 0)
TEDSM<-subset(TEDS, sex1 == 1)

#test sex differences for AUDIT-total
#fit model for female, use AUDIT-total score regressed on age, with covariates
ALCmodel1F_cov<-'
ipsanxt1~~icsanxt1
emo41~~dsdcont1
gpsanxt1~~gpscont1
ipsanxt1~~ipscont1
icsanxt1~~icscont1
lcsdqemot1~~lcsdqcont1
ipsanxt1~~icscont1
IEC~~0*SEC+0*SCC
SEC~~0*SEA+0*SCC+0*SCA
IEA~~0*SCC
SEA~~0*SCC
IEC=~1*emo41+1*gpsanxt1+1*ipsanxt1
SEC=~0*emo41+0.3*gpsanxt1+0.5*ipsanxt1
QEC=~0*emo41+0.09*gpsanxt1+0.25*ipsanxt1
IEA=~1*icsanxt1+1*lcsdqemot1+1*pcbhsdqemot1
SEA=~0*icsanxt1+0.22*lcsdqemot1+0.73*pcbhsdqemot1
QEA=~0*icsanxt1+0.05*lcsdqemot1+0.53*pcbhsdqemot1
ICC=~1*dsdcont1+1*gpscont1+1*ipscont1
SCC=~0*dsdcont1+0.3*gpscont1+0.5*ipscont1
QCC=~0*dsdcont1+0.09*gpscont1+0.25*ipscont1
ICA=~1*icscont1+1*lcsdqcont1+1*pcbhsdqcont1
SCA=~0*icscont1+0.22*lcsdqcont1+0.73*pcbhsdqcont1
QCA=~0*icscont1+0.05*lcsdqcont1+0.53*pcbhsdqcont1
emo41~0*1
gpsanxt1~0*1
ipsanxt1~0*1
icsanxt1~0*1
lcsdqemot1~0*1
pcbhsdqemot1~0*1
dsdcont1~0*1
gpscont1~0*1
ipscont1~0*1
icscont1~0*1
lcsdqcont1~0*1
pcbhsdqcont1~0*1
IEC~1
SEC~1
IEA~1
SEA~1
ICC~1
SCC~1
ICA~1
SCA~1
QEC~1
QEA~1
QCC~1
QCA~1
QEC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QEA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEC~~0*QEC
QEA~~0*QEA
QCC~~0*QCC
QCA~~0*QCA
u2calcoauditrage1~start(-0.7)*IEC+start(-0.4)*SEC+start(0.5)*IEA+start(-0.1)*SEA+start(0.7)*ICC+start(0.4)*SCC+start(-0.1)*ICA+start(0.9)*SCA+laethnic+llivs21+lrel21+lstatus21D1+lstatus21D2+lases+lu1chqualp1+lu1pchild1
laethnic=~1*aethnic
aethnic~~0*aethnic
llivs21=~1*livs21
livs21~~0*livs21
lrel21=~1*rel21
rel21~~0*rel21
lstatus21D1=~1*status21D1
status21D1~~0*status21D1
lstatus21D2=~1*status21D2
status21D2~~0*status21D2
lases=~1*ases
ases~~0*ases
lu1chqualp1=~1*u1chqualp1
u1chqualp1~~0*u1chqualp1
lu1pchild1=~1*u1pchild1
u1pchild1~~0*u1pchild1
'
ALCmodel1F_cov<-cfa(ALCmodel1F_cov,data=TEDSF,estimator="MLR",missing='ML',cluster="randomfamid",meanstructure=TRUE)
summary(ALCmodel1F_cov,standardized=T,fit.measures=T)

#fit model for male, use AUDIT-total score regressed on age, with covariates
ALCmodel1M_cov<-'
ipsanxt1~~icsanxt1
emo41~~dsdcont1
gpsanxt1~~gpscont1
ipsanxt1~~ipscont1
icsanxt1~~icscont1
lcsdqemot1~~lcsdqcont1
ipsanxt1~~icscont1
IEC~~0*SEC+0*SCC
SEC~~0*SEA+0*SCC+0*SCA
IEA~~0*SCC
SEA~~0*SCC
IEC=~1*emo41+1*gpsanxt1+1*ipsanxt1
SEC=~0*emo41+0.3*gpsanxt1+0.5*ipsanxt1
QEC=~0*emo41+0.09*gpsanxt1+0.25*ipsanxt1
IEA=~1*icsanxt1+1*lcsdqemot1+1*pcbhsdqemot1
SEA=~0*icsanxt1+0.22*lcsdqemot1+0.73*pcbhsdqemot1
QEA=~0*icsanxt1+0.05*lcsdqemot1+0.53*pcbhsdqemot1
ICC=~1*dsdcont1+1*gpscont1+1*ipscont1
SCC=~0*dsdcont1+0.3*gpscont1+0.5*ipscont1
QCC=~0*dsdcont1+0.09*gpscont1+0.25*ipscont1
ICA=~1*icscont1+1*lcsdqcont1+1*pcbhsdqcont1
SCA=~0*icscont1+0.22*lcsdqcont1+0.73*pcbhsdqcont1
QCA=~0*icscont1+0.05*lcsdqcont1+0.53*pcbhsdqcont1
emo41~0*1
gpsanxt1~0*1
ipsanxt1~0*1
icsanxt1~0*1
lcsdqemot1~0*1
pcbhsdqemot1~0*1
dsdcont1~0*1
gpscont1~0*1
ipscont1~0*1
icscont1~0*1
lcsdqcont1~0*1
pcbhsdqcont1~0*1
IEC~1
SEC~1
IEA~1
SEA~1
ICC~1
SCC~1
ICA~1
SCA~1
QEC~1
QEA~1
QCC~1
QCA~1
QEC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QEA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEC~~0*QEC
QEA~~0*QEA
QCC~~0*QCC
QCA~~0*QCA
u2calcoauditrage1~start(0.4)*IEC+start(-0.3)*SEC+start(-0.6)*IEA+start(0.7)*SEA+start(-0.9)*ICC+start(-0.6)*SCC+start(1.8)*ICA+start(0.2)*SCA+laethnic+llivs21+lrel21+lstatus21D1+lstatus21D2+lases+lu1chqualp1+lu1pchild1
laethnic=~1*aethnic
aethnic~~0*aethnic
llivs21=~1*livs21
livs21~~0*livs21
lrel21=~1*rel21
rel21~~0*rel21
lstatus21D1=~1*status21D1
status21D1~~0*status21D1
lstatus21D2=~1*status21D2
status21D2~~0*status21D2
lases=~1*ases
ases~~0*ases
lu1chqualp1=~1*u1chqualp1
u1chqualp1~~0*u1chqualp1
lu1pchild1=~1*u1pchild1
u1pchild1~~0*u1pchild1
'
ALCmodel1M_cov<-cfa(ALCmodel1M_cov,data=TEDSM,estimator="MLR",missing='ML',cluster="randomfamid",meanstructure=TRUE)
summary(ALCmodel1M_cov,standardized=T,fit.measures=T)

#multi-group models
#use AUDIT-total score regressed on age, with covariates
ALCmodel1_cov_mg<-'
ipsanxt1~~icsanxt1
emo41~~dsdcont1
gpsanxt1~~gpscont1
ipsanxt1~~ipscont1
icsanxt1~~icscont1
lcsdqemot1~~lcsdqcont1
ipsanxt1~~icscont1
IEC~~0*SEC+0*SCC
SEC~~0*SEA+0*SCC+0*SCA
IEA~~0*SCC
SEA~~0*SCC
IEC=~1*emo41+1*gpsanxt1+1*ipsanxt1
SEC=~0*emo41+0.3*gpsanxt1+0.5*ipsanxt1
QEC=~0*emo41+0.09*gpsanxt1+0.25*ipsanxt1
IEA=~1*icsanxt1+1*lcsdqemot1+1*pcbhsdqemot1
SEA=~0*icsanxt1+0.22*lcsdqemot1+0.73*pcbhsdqemot1
QEA=~0*icsanxt1+0.05*lcsdqemot1+0.53*pcbhsdqemot1
ICC=~1*dsdcont1+1*gpscont1+1*ipscont1
SCC=~0*dsdcont1+0.3*gpscont1+0.5*ipscont1
QCC=~0*dsdcont1+0.09*gpscont1+0.25*ipscont1
ICA=~1*icscont1+1*lcsdqcont1+1*pcbhsdqcont1
SCA=~0*icscont1+0.22*lcsdqcont1+0.73*pcbhsdqcont1
QCA=~0*icscont1+0.05*lcsdqcont1+0.53*pcbhsdqcont1
emo41~0*1
gpsanxt1~0*1
ipsanxt1~0*1
icsanxt1~0*1
lcsdqemot1~0*1
pcbhsdqemot1~0*1
dsdcont1~0*1
gpscont1~0*1
ipscont1~0*1
icscont1~0*1
lcsdqcont1~0*1
pcbhsdqcont1~0*1
IEC~1
SEC~1
IEA~1
SEA~1
ICC~1
SCC~1
ICA~1
SCA~1
QEC~1
QEA~1
QCC~1
QCA~1
QEC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QEA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEC~~0*QEC
QEA~~0*QEA
QCC~~0*QCC
QCA~~0*QCA
u2calcoauditrage1~start(0)*IEC+start(0)*SEC+start(0)*IEA+start(0)*SEA+start(0)*ICC+start(0)*SCC+start(0)*ICA+start(0)*SCA+start(0)*laethnic+start(0)*llivs21+start(0)*lrel21+start(0)*lstatus21D1+start(0)*lstatus21D2+start(0)*lases+start(0)*lu1chqualp1+start(0)*lu1pchild1
laethnic=~1*aethnic
aethnic~~0*aethnic
llivs21=~1*livs21
livs21~~0*livs21
lrel21=~1*rel21
rel21~~0*rel21
lstatus21D1=~1*status21D1
status21D1~~0*status21D1
lstatus21D2=~1*status21D2
status21D2~~0*status21D2
lases=~1*ases
ases~~0*ases
lu1chqualp1=~1*u1chqualp1
u1chqualp1~~0*u1chqualp1
lu1pchild1=~1*u1pchild1
u1pchild1~~0*u1pchild1
' 
ALCmodel1_cov_mg<-cfa(ALCmodel1_cov_mg,data=TEDS,estimator="MLR",missing='ML',cluster="randomfamid",meanstructure=TRUE,group="sex1")
summary(ALCmodel1_cov_mg,standardized=T,fit.measures=T)

#constrain everything related to covariates, except regression coefficients, 
#including covariance with latent growth factors, covariance among covariates, covariates intercepts and variances
ALCmodel1_cov_mg2<-'
ipsanxt1~~icsanxt1
emo41~~dsdcont1
gpsanxt1~~gpscont1
ipsanxt1~~ipscont1
icsanxt1~~icscont1
lcsdqemot1~~lcsdqcont1
ipsanxt1~~icscont1
IEC~~0*SEC+0*SCC
SEC~~0*SEA+0*SCC+0*SCA
IEA~~0*SCC
SEA~~0*SCC
IEC=~1*emo41+1*gpsanxt1+1*ipsanxt1
SEC=~0*emo41+0.3*gpsanxt1+0.5*ipsanxt1
QEC=~0*emo41+0.09*gpsanxt1+0.25*ipsanxt1
IEA=~1*icsanxt1+1*lcsdqemot1+1*pcbhsdqemot1
SEA=~0*icsanxt1+0.22*lcsdqemot1+0.73*pcbhsdqemot1
QEA=~0*icsanxt1+0.05*lcsdqemot1+0.53*pcbhsdqemot1
ICC=~1*dsdcont1+1*gpscont1+1*ipscont1
SCC=~0*dsdcont1+0.3*gpscont1+0.5*ipscont1
QCC=~0*dsdcont1+0.09*gpscont1+0.25*ipscont1
ICA=~1*icscont1+1*lcsdqcont1+1*pcbhsdqcont1
SCA=~0*icscont1+0.22*lcsdqcont1+0.73*pcbhsdqcont1
QCA=~0*icscont1+0.05*lcsdqcont1+0.53*pcbhsdqcont1
emo41~0*1
gpsanxt1~0*1
ipsanxt1~0*1
icsanxt1~0*1
lcsdqemot1~0*1
pcbhsdqemot1~0*1
dsdcont1~0*1
gpscont1~0*1
ipscont1~0*1
icscont1~0*1
lcsdqcont1~0*1
pcbhsdqcont1~0*1
IEC~1
SEC~1
IEA~1
SEA~1
ICC~1
SCC~1
ICA~1
SCA~1
QEC~1
QEA~1
QCC~1
QCA~1
QEC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QEA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEC~~0*QEC
QEA~~0*QEA
QCC~~0*QCC
QCA~~0*QCA
u2calcoauditrage1~start(0)*IEC+start(0)*SEC+start(0)*IEA+start(0)*SEA+start(0)*ICC+start(0)*SCC+start(0)*ICA+start(0)*SCA+start(0)*laethnic+start(0)*llivs21+start(0)*lrel21+start(0)*lstatus21D1+start(0)*lstatus21D2+start(0)*lases+start(0)*lu1chqualp1+start(0)*lu1pchild1
laethnic=~1*aethnic
aethnic~~0*aethnic
llivs21=~1*livs21
livs21~~0*livs21
lrel21=~1*rel21
rel21~~0*rel21
lstatus21D1=~1*status21D1
status21D1~~0*status21D1
lstatus21D2=~1*status21D2
status21D2~~0*status21D2
lases=~1*ases
ases~~0*ases
lu1chqualp1=~1*u1chqualp1
u1chqualp1~~0*u1chqualp1
lu1pchild1=~1*u1pchild1
u1pchild1~~0*u1pchild1
' 
ALCmodel1_cov_mg2<-cfa(ALCmodel1_cov_mg2,data=TEDS,estimator="MLR",missing='ML',cluster="randomfamid",meanstructure=TRUE,group="sex1",
                       group.equal = c("lv.variances","intercepts","means","lv.covariances"),
                       group.partial = c("IEC~~IEC", "SEC~~SEC","IEA~~IEA", "SEA~~SEA","ICC~~ICC", "SCC~~SCC","ICA~~ICA", "SCA~~SCA","IEC~1","SEC~1","IEA~1","SEA~1","ICC~1","SCC~1","ICA~1","SCA~1","QEC~1","QEA~1","QCC~1","QCA~1","u2calcoauditrage1~1","IEC~~IEA","IEC~~SEA","IEC~~ICC","IEC~~ICA","IEC~~SCA","SEC~~IEA","SEC~~ICC","SEC~~ICA","IEA~~SEA","IEA~~ICC","IEA~~ICA","IEA~~SCA","SEA~~ICC","SEA~~ICA","SEA~~SCA","ICC~~SCC","ICC~~ICA","ICC~~SCA","SCC~~ICA","SCC~~SCA","ICA~~SCA"))
summary(ALCmodel1_cov_mg2,standardized=T,fit.measures=T)
anova(ALCmodel1_cov_mg,ALCmodel1_cov_mg2) #sig, but since no hypotheses on covariates, going to constrain everything related to covariates to be the same across groups

#constrain everything related to covariates, except regression coefficients
#including covariance with latent growth factors, covariance among covariates, covariates intercepts and variances
#constrain means of latent growth variables
ALCmodel1_cov_mg3<-'
ipsanxt1~~icsanxt1
emo41~~dsdcont1
gpsanxt1~~gpscont1
ipsanxt1~~ipscont1
icsanxt1~~icscont1
lcsdqemot1~~lcsdqcont1
ipsanxt1~~icscont1
IEC~~0*SEC+0*SCC
SEC~~0*SEA+0*SCC+0*SCA
IEA~~0*SCC
SEA~~0*SCC
IEC=~1*emo41+1*gpsanxt1+1*ipsanxt1
SEC=~0*emo41+0.3*gpsanxt1+0.5*ipsanxt1
QEC=~0*emo41+0.09*gpsanxt1+0.25*ipsanxt1
IEA=~1*icsanxt1+1*lcsdqemot1+1*pcbhsdqemot1
SEA=~0*icsanxt1+0.22*lcsdqemot1+0.73*pcbhsdqemot1
QEA=~0*icsanxt1+0.05*lcsdqemot1+0.53*pcbhsdqemot1
ICC=~1*dsdcont1+1*gpscont1+1*ipscont1
SCC=~0*dsdcont1+0.3*gpscont1+0.5*ipscont1
QCC=~0*dsdcont1+0.09*gpscont1+0.25*ipscont1
ICA=~1*icscont1+1*lcsdqcont1+1*pcbhsdqcont1
SCA=~0*icscont1+0.22*lcsdqcont1+0.73*pcbhsdqcont1
QCA=~0*icscont1+0.05*lcsdqcont1+0.53*pcbhsdqcont1
emo41~0*1
gpsanxt1~0*1
ipsanxt1~0*1
icsanxt1~0*1
lcsdqemot1~0*1
pcbhsdqemot1~0*1
dsdcont1~0*1
gpscont1~0*1
ipscont1~0*1
icscont1~0*1
lcsdqcont1~0*1
pcbhsdqcont1~0*1
IEC~1
SEC~1
IEA~1
SEA~1
ICC~1
SCC~1
ICA~1
SCA~1
QEC~1
QEA~1
QCC~1
QCA~1
QEC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QEA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEC~~0*QEC
QEA~~0*QEA
QCC~~0*QCC
QCA~~0*QCA
u2calcoauditrage1~start(0)*IEC+start(0)*SEC+start(0)*IEA+start(0)*SEA+start(0)*ICC+start(0)*SCC+start(0)*ICA+start(0)*SCA+start(0)*laethnic+start(0)*llivs21+start(0)*lrel21+start(0)*lstatus21D1+start(0)*lstatus21D2+start(0)*lases+start(0)*lu1chqualp1+start(0)*lu1pchild1
laethnic=~1*aethnic
aethnic~~0*aethnic
llivs21=~1*livs21
livs21~~0*livs21
lrel21=~1*rel21
rel21~~0*rel21
lstatus21D1=~1*status21D1
status21D1~~0*status21D1
lstatus21D2=~1*status21D2
status21D2~~0*status21D2
lases=~1*ases
ases~~0*ases
lu1chqualp1=~1*u1chqualp1
u1chqualp1~~0*u1chqualp1
lu1pchild1=~1*u1pchild1
u1pchild1~~0*u1pchild1
' 
ALCmodel1_cov_mg3<-cfa(ALCmodel1_cov_mg3,data=TEDS,estimator="MLR",missing='ML',cluster="randomfamid",meanstructure=TRUE,group="sex1",
                       group.equal = c("lv.variances","intercepts","means","lv.covariances"),
                       group.partial = c("IEC~~IEC", "SEC~~SEC","IEA~~IEA", "SEA~~SEA","ICC~~ICC", "SCC~~SCC","ICA~~ICA", "SCA~~SCA","u2calcoauditrage1~1","IEC~~IEA","IEC~~SEA","IEC~~ICC","IEC~~ICA","IEC~~SCA","SEC~~IEA","SEC~~ICC","SEC~~ICA","IEA~~SEA","IEA~~ICC","IEA~~ICA","IEA~~SCA","SEA~~ICC","SEA~~ICA","SEA~~SCA","ICC~~SCC","ICC~~ICA","ICC~~SCA","SCC~~ICA","SCC~~SCA","ICA~~SCA"))
summary(ALCmodel1_cov_mg3,standardized=T,fit.measures=T)
anova(ALCmodel1_cov_mg2,ALCmodel1_cov_mg3)#sig
#free up constraints on latent factor means to reach a non-significant chi-square difference test result
#final model:
ALCmodel1_cov_mg3i4_5_7_12_1_2_3_10<-cfa(ALCmodel1_cov_mg3,data=TEDS,estimator="MLR",missing='ML',cluster="randomfamid",meanstructure=TRUE,group="sex1",
                                         group.equal = c("lv.variances","intercepts","means","lv.covariances"),
                                         group.partial = c("IEC~~IEC", "SEC~~SEC","IEA~~IEA", "SEA~~SEA","ICC~~ICC", "SCC~~SCC","ICA~~ICA", "SCA~~SCA","u2calcoauditrage1~1","IEC~~IEA","IEC~~SEA","IEC~~ICC","IEC~~ICA","IEC~~SCA","SEC~~IEA","SEC~~ICC","SEC~~ICA","IEA~~SEA","IEA~~ICC","IEA~~ICA","IEA~~SCA","SEA~~ICC","SEA~~ICA","SEA~~SCA","ICC~~SCC","ICC~~ICA","ICC~~SCA","SCC~~ICA","SCC~~SCA","ICA~~SCA","QEA~1","SEA~1","ICC~1","ICA~1","QCA~1","IEC~1","SEC~1","IEA~1"))
summary(ALCmodel1_cov_mg3i4_5_7_12_1_2_3_10,standardized=T,fit.measures=T)
anova(ALCmodel1_cov_mg2,ALCmodel1_cov_mg3i4_5_7_12_1_2_3_10)#non-sig

#constrain everything related to covariates, except regression coefficients
#including covariance with latent growth factors, covariance among covariates, covariates intercepts and variances
#constrain means of latent growth variables, free up "SEA~1","ICC~1","ICA~1","QCA~1","IEC~1","SEC~1","IEA~1","QEA~1"
#constrain variances of latent growth variables
ALCmodel1_cov_mg4<-'
ipsanxt1~~icsanxt1
emo41~~dsdcont1
gpsanxt1~~gpscont1
ipsanxt1~~ipscont1
icsanxt1~~icscont1
lcsdqemot1~~lcsdqcont1
ipsanxt1~~icscont1
IEC~~0*SEC+0*SCC
SEC~~0*SEA+0*SCC+0*SCA
IEA~~0*SCC
SEA~~0*SCC
IEC=~1*emo41+1*gpsanxt1+1*ipsanxt1
SEC=~0*emo41+0.3*gpsanxt1+0.5*ipsanxt1
QEC=~0*emo41+0.09*gpsanxt1+0.25*ipsanxt1
IEA=~1*icsanxt1+1*lcsdqemot1+1*pcbhsdqemot1
SEA=~0*icsanxt1+0.22*lcsdqemot1+0.73*pcbhsdqemot1
QEA=~0*icsanxt1+0.05*lcsdqemot1+0.53*pcbhsdqemot1
ICC=~1*dsdcont1+1*gpscont1+1*ipscont1
SCC=~0*dsdcont1+0.3*gpscont1+0.5*ipscont1
QCC=~0*dsdcont1+0.09*gpscont1+0.25*ipscont1
ICA=~1*icscont1+1*lcsdqcont1+1*pcbhsdqcont1
SCA=~0*icscont1+0.22*lcsdqcont1+0.73*pcbhsdqcont1
QCA=~0*icscont1+0.05*lcsdqcont1+0.53*pcbhsdqcont1
emo41~0*1
gpsanxt1~0*1
ipsanxt1~0*1
icsanxt1~0*1
lcsdqemot1~0*1
pcbhsdqemot1~0*1
dsdcont1~0*1
gpscont1~0*1
ipscont1~0*1
icscont1~0*1
lcsdqcont1~0*1
pcbhsdqcont1~0*1
IEC~1
SEC~1
IEA~1
SEA~1
ICC~1
SCC~1
ICA~1
SCA~1
QEC~1
QEA~1
QCC~1
QCA~1
QEC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QEA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEC~~0*QEC
QEA~~0*QEA
QCC~~0*QCC
QCA~~0*QCA
u2calcoauditrage1~start(0)*IEC+start(0)*SEC+start(0)*IEA+start(0)*SEA+start(0)*ICC+start(0)*SCC+start(0)*ICA+start(0)*SCA+start(0)*laethnic+start(0)*llivs21+start(0)*lrel21+start(0)*lstatus21D1+start(0)*lstatus21D2+start(0)*lases+start(0)*lu1chqualp1+start(0)*lu1pchild1
laethnic=~1*aethnic
aethnic~~0*aethnic
llivs21=~1*livs21
livs21~~0*livs21
lrel21=~1*rel21
rel21~~0*rel21
lstatus21D1=~1*status21D1
status21D1~~0*status21D1
lstatus21D2=~1*status21D2
status21D2~~0*status21D2
lases=~1*ases
ases~~0*ases
lu1chqualp1=~1*u1chqualp1
u1chqualp1~~0*u1chqualp1
lu1pchild1=~1*u1pchild1
u1pchild1~~0*u1pchild1
'  
ALCmodel1_cov_mg4<-cfa(ALCmodel1_cov_mg4,data=TEDS,estimator="MLR",missing='ML',cluster="randomfamid",meanstructure=TRUE,group="sex1",
                       group.equal = c("lv.variances","intercepts","means","lv.covariances"),
                       group.partial = c("u2calcoauditrage1~1","IEC~~IEA","IEC~~SEA","IEC~~ICC","IEC~~ICA","IEC~~SCA","SEC~~IEA","SEC~~ICC","SEC~~ICA","IEA~~SEA","IEA~~ICC","IEA~~ICA","IEA~~SCA","SEA~~ICC","SEA~~ICA","SEA~~SCA","ICC~~SCC","ICC~~ICA","ICC~~SCA","SCC~~ICA","SCC~~SCA","ICA~~SCA","QEA~1","SEA~1","ICC~1","ICA~1","QCA~1","IEC~1","SEC~1","IEA~1"))
summary(ALCmodel1_cov_mg4,standardized=T,fit.measures=T)
anova(ALCmodel1_cov_mg4,ALCmodel1_cov_mg3i4_5_7_12_1_2_3_10)#sig
#free up constraints on latent factor variances to reach a non-significant chi-square difference test result
#final model:
ALCmodel1_cov_mg4v7_5_6_2_3<-cfa(ALCmodel1_cov_mg4,data=TEDS,estimator="MLR",missing='ML',cluster="randomfamid",meanstructure=TRUE,group="sex1",
                                 group.equal = c("lv.variances","intercepts","means","lv.covariances"),
                                 group.partial = c("u2calcoauditrage1~1","IEC~~IEA","IEC~~SEA","IEC~~ICC","IEC~~ICA","IEC~~SCA","SEC~~IEA","SEC~~ICC","SEC~~ICA","IEA~~SEA","IEA~~ICC","IEA~~ICA","IEA~~SCA","SEA~~ICC","SEA~~ICA","SEA~~SCA","ICC~~SCC","ICC~~ICA","ICC~~SCA","SCC~~ICA","SCC~~SCA","ICA~~SCA","QEA~1","SEA~1","ICC~1","ICA~1","QCA~1","IEC~1","SEC~1","IEA~1","IEA~~IEA","ICA~~ICA","ICC~~ICC","SCC~~SCC","SEC~~SEC"))
summary(ALCmodel1_cov_mg4v7_5_6_2_3,standardized=T,fit.measures=T)
anova(ALCmodel1_cov_mg3i4_5_7_12_1_2_3_10,ALCmodel1_cov_mg4v7_5_6_2_3) #non-sig

#constrain everything related to covariates, except regression coefficients
#including covariance with latent growth factors, covariance among covariates, covariates intercepts and variances
#constrain means of latent growth variables, free up "SEA~1","ICC~1","ICA~1","QCA~1","IEC~1","SEC~1","IEA~1","QEA~1"
#constrain variances of latent growth variables, free up "IEA~~IEA","ICA~~ICA","ICC~~ICC","SCC~~SCC","SEC~~SEC"
#constrain covariances of latent growth variables
ALCmodel1_cov_mg5<-'
ipsanxt1~~icsanxt1
emo41~~dsdcont1
gpsanxt1~~gpscont1
ipsanxt1~~ipscont1
icsanxt1~~icscont1
lcsdqemot1~~lcsdqcont1
ipsanxt1~~icscont1
IEC~~0*SEC+0*SCC
SEC~~0*SEA+0*SCC+0*SCA
IEA~~0*SCC
SEA~~0*SCC
IEC=~1*emo41+1*gpsanxt1+1*ipsanxt1
SEC=~0*emo41+0.3*gpsanxt1+0.5*ipsanxt1
QEC=~0*emo41+0.09*gpsanxt1+0.25*ipsanxt1
IEA=~1*icsanxt1+1*lcsdqemot1+1*pcbhsdqemot1
SEA=~0*icsanxt1+0.22*lcsdqemot1+0.73*pcbhsdqemot1
QEA=~0*icsanxt1+0.05*lcsdqemot1+0.53*pcbhsdqemot1
ICC=~1*dsdcont1+1*gpscont1+1*ipscont1
SCC=~0*dsdcont1+0.3*gpscont1+0.5*ipscont1
QCC=~0*dsdcont1+0.09*gpscont1+0.25*ipscont1
ICA=~1*icscont1+1*lcsdqcont1+1*pcbhsdqcont1
SCA=~0*icscont1+0.22*lcsdqcont1+0.73*pcbhsdqcont1
QCA=~0*icscont1+0.05*lcsdqcont1+0.53*pcbhsdqcont1
emo41~0*1
gpsanxt1~0*1
ipsanxt1~0*1
icsanxt1~0*1
lcsdqemot1~0*1
pcbhsdqemot1~0*1
dsdcont1~0*1
gpscont1~0*1
ipscont1~0*1
icscont1~0*1
lcsdqcont1~0*1
pcbhsdqcont1~0*1
IEC~1
SEC~1
IEA~1
SEA~1
ICC~1
SCC~1
ICA~1
SCA~1
QEC~1
QEA~1
QCC~1
QCA~1
QEC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QEA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEC~~0*QEC
QEA~~0*QEA
QCC~~0*QCC
QCA~~0*QCA
u2calcoauditrage1~start(0)*IEC+start(0)*SEC+start(0)*IEA+start(0)*SEA+start(0)*ICC+start(0)*SCC+start(0)*ICA+start(0)*SCA+start(0)*laethnic+start(0)*llivs21+start(0)*lrel21+start(0)*lstatus21D1+start(0)*lstatus21D2+start(0)*lases+start(0)*lu1chqualp1+start(0)*lu1pchild1
laethnic=~1*aethnic
aethnic~~0*aethnic
llivs21=~1*livs21
livs21~~0*livs21
lrel21=~1*rel21
rel21~~0*rel21
lstatus21D1=~1*status21D1
status21D1~~0*status21D1
lstatus21D2=~1*status21D2
status21D2~~0*status21D2
lases=~1*ases
ases~~0*ases
lu1chqualp1=~1*u1chqualp1
u1chqualp1~~0*u1chqualp1
lu1pchild1=~1*u1pchild1
u1pchild1~~0*u1pchild1
'  
ALCmodel1_cov_mg5<-cfa(ALCmodel1_cov_mg5,data=TEDS,estimator="MLR",missing='ML',cluster="randomfamid",meanstructure=TRUE,group="sex1",
                       group.equal = c("lv.variances","intercepts","means","lv.covariances"),
                       group.partial = c("u2calcoauditrage1~1","QEA~1","SEA~1","ICC~1","ICA~1","QCA~1","IEC~1","SEC~1","IEA~1","IEA~~IEA","ICA~~ICA","ICC~~ICC","SCC~~SCC","SEC~~SEC"))
summary(ALCmodel1_cov_mg5,standardized=T,fit.measures=T)
anova(ALCmodel1_cov_mg4v7_5_6_2_3,ALCmodel1_cov_mg5)#sig
#free up constraints on latent factor covariances to reach a non-significant chi-square difference test result
#final model:
ALCmodel1_cov_mg5cv17_18_6_21_19<-cfa(ALCmodel1_cov_mg5,data=TEDS,estimator="MLR",missing='ML',cluster="randomfamid",meanstructure=TRUE,group="sex1",
                                      group.equal = c("lv.variances","intercepts","means","lv.covariances"),
                                      group.partial = c("u2calcoauditrage1~1","QEA~1","SEA~1","ICC~1","ICA~1","QCA~1","IEC~1","SEC~1","IEA~1","IEA~~IEA","ICA~~ICA","ICC~~ICC","SCC~~SCC","SEC~~SEC","SCC~~ICA","ICC~~ICA","ICC~~SCA","SEC~~IEA","ICA~~SCA"))
summary(ALCmodel1_cov_mg5cv17_18_6_21_19,standardized=T,fit.measures=T)
anova(ALCmodel1_cov_mg4v7_5_6_2_3,ALCmodel1_cov_mg5cv17_18_6_21_19)#non-sig

#constrain everything related to covariates, except regression coefficients
#including covariance with latent growth factors, covariance among covariates, covariates intercepts and variances
#constrain means of latent growth variables, free up "SEA~1","ICC~1","ICA~1","QCA~1","IEC~1","SEC~1","IEA~1","QEA~1"
#constrain variances of latent growth variables, free up "IEA~~IEA","ICA~~ICA","ICC~~ICC","SCC~~SCC","SEC~~SEC"
#constrain covariances of latent growth variables, free up "SCC~~ICA","ICC~~ICA","ICC~~SCA","SEC~~IEA","ICA~~SCA"
#constrain residual variances and covariances of indicators of latent growth variables
ALCmodel1_cov_mg6<-'
ipsanxt1~~icsanxt1
emo41~~dsdcont1
gpsanxt1~~gpscont1
ipsanxt1~~ipscont1
icsanxt1~~icscont1
lcsdqemot1~~lcsdqcont1
ipsanxt1~~icscont1
IEC~~0*SEC+0*SCC
SEC~~0*SEA+0*SCC+0*SCA
IEA~~0*SCC
SEA~~0*SCC
IEC=~1*emo41+1*gpsanxt1+1*ipsanxt1
SEC=~0*emo41+0.3*gpsanxt1+0.5*ipsanxt1
QEC=~0*emo41+0.09*gpsanxt1+0.25*ipsanxt1
IEA=~1*icsanxt1+1*lcsdqemot1+1*pcbhsdqemot1
SEA=~0*icsanxt1+0.22*lcsdqemot1+0.73*pcbhsdqemot1
QEA=~0*icsanxt1+0.05*lcsdqemot1+0.53*pcbhsdqemot1
ICC=~1*dsdcont1+1*gpscont1+1*ipscont1
SCC=~0*dsdcont1+0.3*gpscont1+0.5*ipscont1
QCC=~0*dsdcont1+0.09*gpscont1+0.25*ipscont1
ICA=~1*icscont1+1*lcsdqcont1+1*pcbhsdqcont1
SCA=~0*icscont1+0.22*lcsdqcont1+0.73*pcbhsdqcont1
QCA=~0*icscont1+0.05*lcsdqcont1+0.53*pcbhsdqcont1
emo41~0*1
gpsanxt1~0*1
ipsanxt1~0*1
icsanxt1~0*1
lcsdqemot1~0*1
pcbhsdqemot1~0*1
dsdcont1~0*1
gpscont1~0*1
ipscont1~0*1
icscont1~0*1
lcsdqcont1~0*1
pcbhsdqcont1~0*1
IEC~1
SEC~1
IEA~1
SEA~1
ICC~1
SCC~1
ICA~1
SCA~1
QEC~1
QEA~1
QCC~1
QCA~1
QEC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QEA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEC~~0*QEC
QEA~~0*QEA
QCC~~0*QCC
QCA~~0*QCA
u2calcoauditrage1~start(0)*IEC+start(0)*SEC+start(0)*IEA+start(0)*SEA+start(0)*ICC+start(0)*SCC+start(0)*ICA+start(0)*SCA+start(0)*laethnic+start(0)*llivs21+start(0)*lrel21+start(0)*lstatus21D1+start(0)*lstatus21D2+start(0)*lases+start(0)*lu1chqualp1+start(0)*lu1pchild1
laethnic=~1*aethnic
aethnic~~0*aethnic
llivs21=~1*livs21
livs21~~0*livs21
lrel21=~1*rel21
rel21~~0*rel21
lstatus21D1=~1*status21D1
status21D1~~0*status21D1
lstatus21D2=~1*status21D2
status21D2~~0*status21D2
lases=~1*ases
ases~~0*ases
lu1chqualp1=~1*u1chqualp1
u1chqualp1~~0*u1chqualp1
lu1pchild1=~1*u1pchild1
u1pchild1~~0*u1pchild1
'  
ALCmodel1_cov_mg6<-cfa(ALCmodel1_cov_mg6,data=TEDS,estimator="MLR",missing='ML',cluster="randomfamid",meanstructure=TRUE,group="sex1",
                       group.equal = c("lv.variances","intercepts","means","lv.covariances","residuals","residual.covariances"),
                       group.partial = c("u2calcoauditrage1~1","u2calcoauditrage1~~u2calcoauditrage1","QEA~1","SEA~1","ICC~1","ICA~1","QCA~1","IEC~1","SEC~1","IEA~1","IEA~~IEA","ICA~~ICA","ICC~~ICC","SCC~~SCC","SEC~~SEC","SCC~~ICA","ICC~~ICA","ICC~~SCA","SEC~~IEA","ICA~~SCA"))
summary(ALCmodel1_cov_mg6,standardized=T,fit.measures=T)
anova(ALCmodel1_cov_mg6,ALCmodel1_cov_mg5cv17_18_6_21_19) #sig
#free up constraints on residual variances and covariances of indicators to reach a non-significant chi-square difference test result
#final model:
ALCmodel1_cov_mg6v6_11_8_10<-cfa(ALCmodel1_cov_mg6,data=TEDS,estimator="MLR",missing='ML',cluster="randomfamid",meanstructure=TRUE,group="sex1",
                                 group.equal = c("lv.variances","intercepts","means","lv.covariances","residuals","residual.covariances"),
                                 group.partial = c("u2calcoauditrage1~1","u2calcoauditrage1~~u2calcoauditrage1","QEA~1","SEA~1","ICC~1","ICA~1","QCA~1","IEC~1","SEC~1","IEA~1","IEA~~IEA","ICA~~ICA","ICC~~ICC","SCC~~SCC","SEC~~SEC","SCC~~ICA","ICC~~ICA","ICC~~SCA","SEC~~IEA","ICA~~SCA","icscont1~~icscont1","pcbhsdqemot1~~pcbhsdqemot1","lcsdqcont1~~lcsdqcont1","gpscont1~~gpscont1"))
summary(ALCmodel1_cov_mg6v6_11_8_10,standardized=T,fit.measures=T)
anova(ALCmodel1_cov_mg5cv17_18_6_21_19,ALCmodel1_cov_mg6v6_11_8_10) #non-sig

#constrain everything related to covariates, except regression coefficients
#including covariance with latent growth factors, covariance among covariates, covariates intercepts and variances
#constrain means of latent growth variables, free up "SEA~1","ICC~1","ICA~1","QCA~1","IEC~1","SEC~1","IEA~1","QEA~1"
#constrain variances of latent growth variables, free up "IEA~~IEA","ICA~~ICA","ICC~~ICC","SCC~~SCC","SEC~~SEC"
#constrain covariances of latent growth variables, free up "SCC~~ICA","ICC~~ICA","ICC~~SCA","SEC~~IEA","ICA~~SCA"
#constrain residual variances and covariances of indicators of latent growth variables, free up "icscont1~~icscont1","pcbhsdqemot1~~pcbhsdqemot1","lcsdqcont1~~lcsdqcont1","gpscont1~~gpscont1"
#test whether regression coefficients are equal
ALCmodel1_cov_mg7r<-'
ipsanxt1~~icsanxt1
emo41~~dsdcont1
gpsanxt1~~gpscont1
ipsanxt1~~ipscont1
icsanxt1~~icscont1
lcsdqemot1~~lcsdqcont1
ipsanxt1~~icscont1
IEC~~0*SEC+0*SCC
SEC~~0*SEA+0*SCC+0*SCA
IEA~~0*SCC
SEA~~0*SCC
IEC=~1*emo41+1*gpsanxt1+1*ipsanxt1
SEC=~0*emo41+0.3*gpsanxt1+0.5*ipsanxt1
QEC=~0*emo41+0.09*gpsanxt1+0.25*ipsanxt1
IEA=~1*icsanxt1+1*lcsdqemot1+1*pcbhsdqemot1
SEA=~0*icsanxt1+0.22*lcsdqemot1+0.73*pcbhsdqemot1
QEA=~0*icsanxt1+0.05*lcsdqemot1+0.53*pcbhsdqemot1
ICC=~1*dsdcont1+1*gpscont1+1*ipscont1
SCC=~0*dsdcont1+0.3*gpscont1+0.5*ipscont1
QCC=~0*dsdcont1+0.09*gpscont1+0.25*ipscont1
ICA=~1*icscont1+1*lcsdqcont1+1*pcbhsdqcont1
SCA=~0*icscont1+0.22*lcsdqcont1+0.73*pcbhsdqcont1
QCA=~0*icscont1+0.05*lcsdqcont1+0.53*pcbhsdqcont1
emo41~0*1
gpsanxt1~0*1
ipsanxt1~0*1
icsanxt1~0*1
lcsdqemot1~0*1
pcbhsdqemot1~0*1
dsdcont1~0*1
gpscont1~0*1
ipscont1~0*1
icscont1~0*1
lcsdqcont1~0*1
pcbhsdqcont1~0*1
IEC~1
SEC~1
IEA~1
SEA~1
ICC~1
SCC~1
ICA~1
SCA~1
QEC~1
QEA~1
QCC~1
QCA~1
QEC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QEA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEC~~0*QEC
QEA~~0*QEA
QCC~~0*QCC
QCA~~0*QCA
u2calcoauditrage1~start(0)*IEC+start(0)*SEC+start(0)*IEA+start(0)*SEA+start(0)*ICC+start(0)*SCC+start(0)*ICA+start(0)*SCA+start(0)*laethnic+start(0)*llivs21+start(0)*lrel21+start(0)*lstatus21D1+start(0)*lstatus21D2+start(0)*lases+start(0)*lu1chqualp1+start(0)*lu1pchild1+c(bc1m,bc1f)*laethnic+c(bc2m,bc2f)*llivs21+c(bc3m,bc3f)*lrel21+c(bc4m,bc4f)*lstatus21D1+c(bc5m,bc5f)*lstatus21D2+c(bc6m,bc6f)*lases+c(bc7m,bc7f)*lu1chqualp1+c(bc8m,bc8f)*lu1pchild1+c(b1m,b1f)*IEC+c(b2m,b2f)*SEC+c(b3m,b3f)*IEA+c(b4m,b4f)*SEA+c(b5m,b5f)*ICC+c(b6m,b6f)*SCC+c(b7m,b7f)*ICA+c(b8m,b8f)*SCA
laethnic=~1*aethnic
aethnic~~0*aethnic
llivs21=~1*livs21
livs21~~0*livs21
lrel21=~1*rel21
rel21~~0*rel21
lstatus21D1=~1*status21D1
status21D1~~0*status21D1
lstatus21D2=~1*status21D2
status21D2~~0*status21D2
lases=~1*ases
ases~~0*ases
lu1chqualp1=~1*u1chqualp1
u1chqualp1~~0*u1chqualp1
lu1pchild1=~1*u1pchild1
u1pchild1~~0*u1pchild1
' 
ALCmodel1_cov_mg7r<-cfa(ALCmodel1_cov_mg7r,data=TEDS,estimator="MLR",missing='ML',cluster="randomfamid",meanstructure=TRUE,group="sex1",
                        group.equal = c("lv.variances","intercepts","means","lv.covariances","residuals","residual.covariances"),
                        group.partial = c("u2calcoauditrage1~1","u2calcoauditrage1~~u2calcoauditrage1","QEA~1","SEA~1","ICC~1","ICA~1","QCA~1","IEC~1","SEC~1","IEA~1","IEA~~IEA","ICA~~ICA","ICC~~ICC","SCC~~SCC","SEC~~SEC","SCC~~ICA","ICC~~ICA","ICC~~SCA","SEC~~IEA","ICA~~SCA","icscont1~~icscont1","pcbhsdqemot1~~pcbhsdqemot1","lcsdqcont1~~lcsdqcont1","gpscont1~~gpscont1"))
summary(ALCmodel1_cov_mg7r,standardized=T,fit.measures=T)

lavTestWald(ALCmodel1_cov_mg7r, constraints = "b1f-b1m == 0")
lavTestWald(ALCmodel1_cov_mg7r, constraints = "b2f-b2m == 0")
lavTestWald(ALCmodel1_cov_mg7r, constraints = "b3f-b3m == 0")#sig
lavTestWald(ALCmodel1_cov_mg7r, constraints = "b4f-b4m == 0")
lavTestWald(ALCmodel1_cov_mg7r, constraints = "b5f-b5m == 0")
lavTestWald(ALCmodel1_cov_mg7r, constraints = "b6f-b6m == 0")
lavTestWald(ALCmodel1_cov_mg7r, constraints = "b7f-b7m == 0")
lavTestWald(ALCmodel1_cov_mg7r, constraints = "b8f-b8m == 0")

lavTestWald(ALCmodel1_cov_mg7r, constraints = "bc1f-bc1m == 0")
lavTestWald(ALCmodel1_cov_mg7r, constraints = "bc2f-bc2m == 0")
lavTestWald(ALCmodel1_cov_mg7r, constraints = "bc3f-bc3m == 0")#sig
lavTestWald(ALCmodel1_cov_mg7r, constraints = "bc4f-bc4m == 0")#sig
lavTestWald(ALCmodel1_cov_mg7r, constraints = "bc5f-bc5m == 0")
lavTestWald(ALCmodel1_cov_mg7r, constraints = "bc6f-bc6m == 0")
lavTestWald(ALCmodel1_cov_mg7r, constraints = "bc7f-bc7m == 0")#sig
lavTestWald(ALCmodel1_cov_mg7r, constraints = "bc8f-bc8m == 0")

#constrain all non-significantly different regressions to be the same
ALCmodel1_cov_mg7r2<-'
ipsanxt1~~icsanxt1
emo41~~dsdcont1
gpsanxt1~~gpscont1
ipsanxt1~~ipscont1
icsanxt1~~icscont1
lcsdqemot1~~lcsdqcont1
ipsanxt1~~icscont1
IEC~~0*SEC+0*SCC
SEC~~0*SEA+0*SCC+0*SCA
IEA~~0*SCC
SEA~~0*SCC
IEC=~1*emo41+1*gpsanxt1+1*ipsanxt1
SEC=~0*emo41+0.3*gpsanxt1+0.5*ipsanxt1
QEC=~0*emo41+0.09*gpsanxt1+0.25*ipsanxt1
IEA=~1*icsanxt1+1*lcsdqemot1+1*pcbhsdqemot1
SEA=~0*icsanxt1+0.22*lcsdqemot1+0.73*pcbhsdqemot1
QEA=~0*icsanxt1+0.05*lcsdqemot1+0.53*pcbhsdqemot1
ICC=~1*dsdcont1+1*gpscont1+1*ipscont1
SCC=~0*dsdcont1+0.3*gpscont1+0.5*ipscont1
QCC=~0*dsdcont1+0.09*gpscont1+0.25*ipscont1
ICA=~1*icscont1+1*lcsdqcont1+1*pcbhsdqcont1
SCA=~0*icscont1+0.22*lcsdqcont1+0.73*pcbhsdqcont1
QCA=~0*icscont1+0.05*lcsdqcont1+0.53*pcbhsdqcont1
emo41~0*1
gpsanxt1~0*1
ipsanxt1~0*1
icsanxt1~0*1
lcsdqemot1~0*1
pcbhsdqemot1~0*1
dsdcont1~0*1
gpscont1~0*1
ipscont1~0*1
icscont1~0*1
lcsdqcont1~0*1
pcbhsdqcont1~0*1
IEC~1
SEC~1
IEA~1
SEA~1
ICC~1
SCC~1
ICA~1
SCA~1
QEC~1
QEA~1
QCC~1
QCA~1
QEC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QEA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEC~~0*QEC
QEA~~0*QEA
QCC~~0*QCC
QCA~~0*QCA
u2calcoauditrage1~start(0)*IEC+start(0)*SEC+start(0)*IEA+start(0)*SEA+start(0)*ICC+start(0)*SCC+start(0)*ICA+start(0)*SCA+start(0)*laethnic+start(0)*llivs21+start(0)*lrel21+start(0)*lstatus21D1+start(0)*lstatus21D2+start(0)*lases+start(0)*lu1chqualp1+start(0)*lu1pchild1+c(bc1,bc1)*laethnic+c(bc2,bc2)*llivs21+c(bc3m,bc3f)*lrel21+c(bc4m,bc4f)*lstatus21D1+c(bc5,bc5)*lstatus21D2+c(bc6,bc6)*lases+c(bc7m,bc7f)*lu1chqualp1+c(bc8,bc8)*lu1pchild1+c(b1,b1)*IEC+c(b2,b2)*SEC+c(b3m,b3f)*IEA+c(b4,b4)*SEA+c(b5,b5)*ICC+c(b6,b6)*SCC+c(b7,b7)*ICA+c(b8,b8)*SCA
laethnic=~1*aethnic
aethnic~~0*aethnic
llivs21=~1*livs21
livs21~~0*livs21
lrel21=~1*rel21
rel21~~0*rel21
lstatus21D1=~1*status21D1
status21D1~~0*status21D1
lstatus21D2=~1*status21D2
status21D2~~0*status21D2
lases=~1*ases
ases~~0*ases
lu1chqualp1=~1*u1chqualp1
u1chqualp1~~0*u1chqualp1
lu1pchild1=~1*u1pchild1
u1pchild1~~0*u1pchild1
' 
ALCmodel1_cov_mg7r2<-cfa(ALCmodel1_cov_mg7r2,data=TEDS,estimator="MLR",missing='ML',cluster="randomfamid",meanstructure=TRUE,group="sex1",
                         group.equal = c("lv.variances","intercepts","means","lv.covariances","residuals","residual.covariances"),
                         group.partial = c("u2calcoauditrage1~1","u2calcoauditrage1~~u2calcoauditrage1","QEA~1","SEA~1","ICC~1","ICA~1","QCA~1","IEC~1","SEC~1","IEA~1","IEA~~IEA","ICA~~ICA","ICC~~ICC","SCC~~SCC","SEC~~SEC","SCC~~ICA","ICC~~ICA","ICC~~SCA","SEC~~IEA","ICA~~SCA","icscont1~~icscont1","pcbhsdqemot1~~pcbhsdqemot1","lcsdqcont1~~lcsdqcont1","gpscont1~~gpscont1"))
summary(ALCmodel1_cov_mg7r2,standardized=T,fit.measures=T)
anova(ALCmodel1_cov_mg7r,ALCmodel1_cov_mg7r2)#non-sig

lavTestWald(ALCmodel1_cov_mg7r2, constraints = "b3f-b3m == 0")#sig
lavTestWald(ALCmodel1_cov_mg7r2, constraints = "bc3f-bc3m == 0")#sig
lavTestWald(ALCmodel1_cov_mg7r2, constraints = "bc4f-bc4m == 0")#sig
lavTestWald(ALCmodel1_cov_mg7r2, constraints = "bc7f-bc7m == 0")#non-sig

#further constrain bc7f=bc7m
ALCmodel1_cov_mg7r3<-'
ipsanxt1~~icsanxt1
emo41~~dsdcont1
gpsanxt1~~gpscont1
ipsanxt1~~ipscont1
icsanxt1~~icscont1
lcsdqemot1~~lcsdqcont1
ipsanxt1~~icscont1
IEC~~0*SEC+0*SCC
SEC~~0*SEA+0*SCC+0*SCA
IEA~~0*SCC
SEA~~0*SCC
IEC=~1*emo41+1*gpsanxt1+1*ipsanxt1
SEC=~0*emo41+0.3*gpsanxt1+0.5*ipsanxt1
QEC=~0*emo41+0.09*gpsanxt1+0.25*ipsanxt1
IEA=~1*icsanxt1+1*lcsdqemot1+1*pcbhsdqemot1
SEA=~0*icsanxt1+0.22*lcsdqemot1+0.73*pcbhsdqemot1
QEA=~0*icsanxt1+0.05*lcsdqemot1+0.53*pcbhsdqemot1
ICC=~1*dsdcont1+1*gpscont1+1*ipscont1
SCC=~0*dsdcont1+0.3*gpscont1+0.5*ipscont1
QCC=~0*dsdcont1+0.09*gpscont1+0.25*ipscont1
ICA=~1*icscont1+1*lcsdqcont1+1*pcbhsdqcont1
SCA=~0*icscont1+0.22*lcsdqcont1+0.73*pcbhsdqcont1
QCA=~0*icscont1+0.05*lcsdqcont1+0.53*pcbhsdqcont1
emo41~0*1
gpsanxt1~0*1
ipsanxt1~0*1
icsanxt1~0*1
lcsdqemot1~0*1
pcbhsdqemot1~0*1
dsdcont1~0*1
gpscont1~0*1
ipscont1~0*1
icscont1~0*1
lcsdqcont1~0*1
pcbhsdqcont1~0*1
IEC~1
SEC~1
IEA~1
SEA~1
ICC~1
SCC~1
ICA~1
SCA~1
QEC~1
QEA~1
QCC~1
QCA~1
QEC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QEA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEC~~0*QEC
QEA~~0*QEA
QCC~~0*QCC
QCA~~0*QCA
u2calcoauditrage1~start(0)*IEC+start(0)*SEC+start(0)*IEA+start(0)*SEA+start(0)*ICC+start(0)*SCC+start(0)*ICA+start(0)*SCA+start(0)*laethnic+start(0)*llivs21+start(0)*lrel21+start(0)*lstatus21D1+start(0)*lstatus21D2+start(0)*lases+start(0)*lu1chqualp1+start(0)*lu1pchild1+c(bc1,bc1)*laethnic+c(bc2,bc2)*llivs21+c(bc3m,bc3f)*lrel21+c(bc4m,bc4f)*lstatus21D1+c(bc5,bc5)*lstatus21D2+c(bc6,bc6)*lases+c(bc7,bc7)*lu1chqualp1+c(bc8,bc8)*lu1pchild1+c(b1,b1)*IEC+c(b2,b2)*SEC+c(b3m,b3f)*IEA+c(b4,b4)*SEA+c(b5,b5)*ICC+c(b6,b6)*SCC+c(b7,b7)*ICA+c(b8,b8)*SCA
laethnic=~1*aethnic
aethnic~~0*aethnic
llivs21=~1*livs21
livs21~~0*livs21
lrel21=~1*rel21
rel21~~0*rel21
lstatus21D1=~1*status21D1
status21D1~~0*status21D1
lstatus21D2=~1*status21D2
status21D2~~0*status21D2
lases=~1*ases
ases~~0*ases
lu1chqualp1=~1*u1chqualp1
u1chqualp1~~0*u1chqualp1
lu1pchild1=~1*u1pchild1
u1pchild1~~0*u1pchild1
' 
ALCmodel1_cov_mg7r3<-cfa(ALCmodel1_cov_mg7r3,data=TEDS,estimator="MLR",missing='ML',cluster="randomfamid",meanstructure=TRUE,group="sex1",
                         group.equal = c("lv.variances","intercepts","means","lv.covariances","residuals","residual.covariances"),
                         group.partial = c("u2calcoauditrage1~1","u2calcoauditrage1~~u2calcoauditrage1","QEA~1","SEA~1","ICC~1","ICA~1","QCA~1","IEC~1","SEC~1","IEA~1","IEA~~IEA","ICA~~ICA","ICC~~ICC","SCC~~SCC","SEC~~SEC","SCC~~ICA","ICC~~ICA","ICC~~SCA","SEC~~IEA","ICA~~SCA","icscont1~~icscont1","pcbhsdqemot1~~pcbhsdqemot1","lcsdqcont1~~lcsdqcont1","gpscont1~~gpscont1"))
summary(ALCmodel1_cov_mg7r3,standardized=T,fit.measures=T)
anova(ALCmodel1_cov_mg7r,ALCmodel1_cov_mg7r3)#non-sig

lavTestWald(ALCmodel1_cov_mg7r3, constraints = "b3f-b3m == 0")#sig
lavTestWald(ALCmodel1_cov_mg7r3, constraints = "bc3f-bc3m == 0")#sig
lavTestWald(ALCmodel1_cov_mg7r3, constraints = "bc4f-bc4m == 0")#non-sig

#further constrain bc4f=bc4m
ALCmodel1_cov_mg7r4<-'
ipsanxt1~~icsanxt1
emo41~~dsdcont1
gpsanxt1~~gpscont1
ipsanxt1~~ipscont1
icsanxt1~~icscont1
lcsdqemot1~~lcsdqcont1
ipsanxt1~~icscont1
IEC~~0*SEC+0*SCC
SEC~~0*SEA+0*SCC+0*SCA
IEA~~0*SCC
SEA~~0*SCC
IEC=~1*emo41+1*gpsanxt1+1*ipsanxt1
SEC=~0*emo41+0.3*gpsanxt1+0.5*ipsanxt1
QEC=~0*emo41+0.09*gpsanxt1+0.25*ipsanxt1
IEA=~1*icsanxt1+1*lcsdqemot1+1*pcbhsdqemot1
SEA=~0*icsanxt1+0.22*lcsdqemot1+0.73*pcbhsdqemot1
QEA=~0*icsanxt1+0.05*lcsdqemot1+0.53*pcbhsdqemot1
ICC=~1*dsdcont1+1*gpscont1+1*ipscont1
SCC=~0*dsdcont1+0.3*gpscont1+0.5*ipscont1
QCC=~0*dsdcont1+0.09*gpscont1+0.25*ipscont1
ICA=~1*icscont1+1*lcsdqcont1+1*pcbhsdqcont1
SCA=~0*icscont1+0.22*lcsdqcont1+0.73*pcbhsdqcont1
QCA=~0*icscont1+0.05*lcsdqcont1+0.53*pcbhsdqcont1
emo41~0*1
gpsanxt1~0*1
ipsanxt1~0*1
icsanxt1~0*1
lcsdqemot1~0*1
pcbhsdqemot1~0*1
dsdcont1~0*1
gpscont1~0*1
ipscont1~0*1
icscont1~0*1
lcsdqcont1~0*1
pcbhsdqcont1~0*1
IEC~1
SEC~1
IEA~1
SEA~1
ICC~1
SCC~1
ICA~1
SCA~1
QEC~1
QEA~1
QCC~1
QCA~1
QEC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QEA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEC~~0*QEC
QEA~~0*QEA
QCC~~0*QCC
QCA~~0*QCA
u2calcoauditrage1~start(0)*IEC+start(0)*SEC+start(0)*IEA+start(0)*SEA+start(0)*ICC+start(0)*SCC+start(0)*ICA+start(0)*SCA+start(0)*laethnic+start(0)*llivs21+start(0)*lrel21+start(0)*lstatus21D1+start(0)*lstatus21D2+start(0)*lases+start(0)*lu1chqualp1+start(0)*lu1pchild1+c(bc1,bc1)*laethnic+c(bc2,bc2)*llivs21+c(bc3m,bc3f)*lrel21+c(bc4,bc4)*lstatus21D1+c(bc5,bc5)*lstatus21D2+c(bc6,bc6)*lases+c(bc7,bc7)*lu1chqualp1+c(bc8,bc8)*lu1pchild1+c(b1,b1)*IEC+c(b2,b2)*SEC+c(b3m,b3f)*IEA+c(b4,b4)*SEA+c(b5,b5)*ICC+c(b6,b6)*SCC+c(b7,b7)*ICA+c(b8,b8)*SCA
laethnic=~1*aethnic
aethnic~~0*aethnic
llivs21=~1*livs21
livs21~~0*livs21
lrel21=~1*rel21
rel21~~0*rel21
lstatus21D1=~1*status21D1
status21D1~~0*status21D1
lstatus21D2=~1*status21D2
status21D2~~0*status21D2
lases=~1*ases
ases~~0*ases
lu1chqualp1=~1*u1chqualp1
u1chqualp1~~0*u1chqualp1
lu1pchild1=~1*u1pchild1
u1pchild1~~0*u1pchild1
' 
ALCmodel1_cov_mg7r4<-cfa(ALCmodel1_cov_mg7r4,data=TEDS,estimator="MLR",missing='ML',cluster="randomfamid",meanstructure=TRUE,group="sex1",
                         group.equal = c("lv.variances","intercepts","means","lv.covariances","residuals","residual.covariances"),
                         group.partial = c("u2calcoauditrage1~1","u2calcoauditrage1~~u2calcoauditrage1","QEA~1","SEA~1","ICC~1","ICA~1","QCA~1","IEC~1","SEC~1","IEA~1","IEA~~IEA","ICA~~ICA","ICC~~ICC","SCC~~SCC","SEC~~SEC","SCC~~ICA","ICC~~ICA","ICC~~SCA","SEC~~IEA","ICA~~SCA","icscont1~~icscont1","pcbhsdqemot1~~pcbhsdqemot1","lcsdqcont1~~lcsdqcont1","gpscont1~~gpscont1"))
summary(ALCmodel1_cov_mg7r4,standardized=T,fit.measures=T)
anova(ALCmodel1_cov_mg7r,ALCmodel1_cov_mg7r4)#non-sig

lavTestWald(ALCmodel1_cov_mg7r4, constraints = "b3f-b3m == 0")#sig
lavTestWald(ALCmodel1_cov_mg7r4, constraints = "bc3f-bc3m == 0")#sig

#use ALCmodel1_cov_mg7r4
#constrain everything related to covariates
#including covariance with latent growth factors, covariance among covariates, covariates intercepts and variances
#constrain means of latent growth variables, free up "SEA~1","ICC~1","ICA~1","QCA~1","IEC~1","SEC~1","IEA~1","QEA~1"
#constrain variances of latent growth variables, free up "IEA~~IEA","ICA~~ICA","ICC~~ICC","SCC~~SCC","SEC~~SEC"
#constrain covariances of latent growth variables, free up "SCC~~ICA","ICC~~ICA","ICC~~SCA","SEC~~IEA","ICA~~SCA"
#constrain residual variances and covariances of indicators of latent growth variables, free up "icscont1~~icscont1","pcbhsdqemot1~~pcbhsdqemot1","lcsdqcont1~~lcsdqcont1","gpscont1~~gpscont1"
#regressions coefficients constrainted except for IEA, rel21
#constrain intercept and residual variance of AUDIT
ALCmodel1_cov_mg8<-'
ipsanxt1~~icsanxt1
emo41~~dsdcont1
gpsanxt1~~gpscont1
ipsanxt1~~ipscont1
icsanxt1~~icscont1
lcsdqemot1~~lcsdqcont1
ipsanxt1~~icscont1
IEC~~0*SEC+0*SCC
SEC~~0*SEA+0*SCC+0*SCA
IEA~~0*SCC
SEA~~0*SCC
IEC=~1*emo41+1*gpsanxt1+1*ipsanxt1
SEC=~0*emo41+0.3*gpsanxt1+0.5*ipsanxt1
QEC=~0*emo41+0.09*gpsanxt1+0.25*ipsanxt1
IEA=~1*icsanxt1+1*lcsdqemot1+1*pcbhsdqemot1
SEA=~0*icsanxt1+0.22*lcsdqemot1+0.73*pcbhsdqemot1
QEA=~0*icsanxt1+0.05*lcsdqemot1+0.53*pcbhsdqemot1
ICC=~1*dsdcont1+1*gpscont1+1*ipscont1
SCC=~0*dsdcont1+0.3*gpscont1+0.5*ipscont1
QCC=~0*dsdcont1+0.09*gpscont1+0.25*ipscont1
ICA=~1*icscont1+1*lcsdqcont1+1*pcbhsdqcont1
SCA=~0*icscont1+0.22*lcsdqcont1+0.73*pcbhsdqcont1
QCA=~0*icscont1+0.05*lcsdqcont1+0.53*pcbhsdqcont1
emo41~0*1
gpsanxt1~0*1
ipsanxt1~0*1
icsanxt1~0*1
lcsdqemot1~0*1
pcbhsdqemot1~0*1
dsdcont1~0*1
gpscont1~0*1
ipscont1~0*1
icscont1~0*1
lcsdqcont1~0*1
pcbhsdqcont1~0*1
IEC~1
SEC~1
IEA~1
SEA~1
ICC~1
SCC~1
ICA~1
SCA~1
QEC~1
QEA~1
QCC~1
QCA~1
QEC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QEA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEC~~0*QEC
QEA~~0*QEA
QCC~~0*QCC
QCA~~0*QCA
u2calcoauditrage1~start(0)*IEC+start(0)*SEC+start(0)*IEA+start(0)*SEA+start(0)*ICC+start(0)*SCC+start(0)*ICA+start(0)*SCA+start(0)*laethnic+start(0)*llivs21+start(0)*lrel21+start(0)*lstatus21D1+start(0)*lstatus21D2+start(0)*lases+start(0)*lu1chqualp1+start(0)*lu1pchild1+c(bc1,bc1)*laethnic+c(bc2,bc2)*llivs21+c(bc3m,bc3f)*lrel21+c(bc4,bc4)*lstatus21D1+c(bc5,bc5)*lstatus21D2+c(bc6,bc6)*lases+c(bc7,bc7)*lu1chqualp1+c(bc8,bc8)*lu1pchild1+c(b1,b1)*IEC+c(b2,b2)*SEC+c(b3m,b3f)*IEA+c(b4,b4)*SEA+c(b5,b5)*ICC+c(b6,b6)*SCC+c(b7,b7)*ICA+c(b8,b8)*SCA
laethnic=~1*aethnic
aethnic~~0*aethnic
llivs21=~1*livs21
livs21~~0*livs21
lrel21=~1*rel21
rel21~~0*rel21
lstatus21D1=~1*status21D1
status21D1~~0*status21D1
lstatus21D2=~1*status21D2
status21D2~~0*status21D2
lases=~1*ases
ases~~0*ases
lu1chqualp1=~1*u1chqualp1
u1chqualp1~~0*u1chqualp1
lu1pchild1=~1*u1pchild1
u1pchild1~~0*u1pchild1
' 
ALCmodel1_cov_mg8<-cfa(ALCmodel1_cov_mg8,data=TEDS,estimator="MLR",missing='ML',cluster="randomfamid",meanstructure=TRUE,group="sex1",
                       group.equal = c("lv.variances","intercepts","means","lv.covariances","residuals","residual.covariances"),
                       group.partial = c("QEA~1","SEA~1","ICC~1","ICA~1","QCA~1","IEC~1","SEC~1","IEA~1","IEA~~IEA","ICA~~ICA","ICC~~ICC","SCC~~SCC","SEC~~SEC","SCC~~ICA","ICC~~ICA","ICC~~SCA","SEC~~IEA","ICA~~SCA","icscont1~~icscont1","pcbhsdqemot1~~pcbhsdqemot1","lcsdqcont1~~lcsdqcont1","gpscont1~~gpscont1"))
summary(ALCmodel1_cov_mg8,standardized=T,fit.measures=T) 
anova(ALCmodel1_cov_mg8,ALCmodel1_cov_mg7r4)#scaling factor negative

#free up intercept of AUDIT
ALCmodel1_cov_mg8i<-'
ipsanxt1~~icsanxt1
emo41~~dsdcont1
gpsanxt1~~gpscont1
ipsanxt1~~ipscont1
icsanxt1~~icscont1
lcsdqemot1~~lcsdqcont1
ipsanxt1~~icscont1
IEC~~0*SEC+0*SCC
SEC~~0*SEA+0*SCC+0*SCA
IEA~~0*SCC
SEA~~0*SCC
IEC=~1*emo41+1*gpsanxt1+1*ipsanxt1
SEC=~0*emo41+0.3*gpsanxt1+0.5*ipsanxt1
QEC=~0*emo41+0.09*gpsanxt1+0.25*ipsanxt1
IEA=~1*icsanxt1+1*lcsdqemot1+1*pcbhsdqemot1
SEA=~0*icsanxt1+0.22*lcsdqemot1+0.73*pcbhsdqemot1
QEA=~0*icsanxt1+0.05*lcsdqemot1+0.53*pcbhsdqemot1
ICC=~1*dsdcont1+1*gpscont1+1*ipscont1
SCC=~0*dsdcont1+0.3*gpscont1+0.5*ipscont1
QCC=~0*dsdcont1+0.09*gpscont1+0.25*ipscont1
ICA=~1*icscont1+1*lcsdqcont1+1*pcbhsdqcont1
SCA=~0*icscont1+0.22*lcsdqcont1+0.73*pcbhsdqcont1
QCA=~0*icscont1+0.05*lcsdqcont1+0.53*pcbhsdqcont1
emo41~0*1
gpsanxt1~0*1
ipsanxt1~0*1
icsanxt1~0*1
lcsdqemot1~0*1
pcbhsdqemot1~0*1
dsdcont1~0*1
gpscont1~0*1
ipscont1~0*1
icscont1~0*1
lcsdqcont1~0*1
pcbhsdqcont1~0*1
IEC~1
SEC~1
IEA~1
SEA~1
ICC~1
SCC~1
ICA~1
SCA~1
QEC~1
QEA~1
QCC~1
QCA~1
QEC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QEA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEC~~0*QEC
QEA~~0*QEA
QCC~~0*QCC
QCA~~0*QCA
u2calcoauditrage1~start(0)*IEC+start(0)*SEC+start(0)*IEA+start(0)*SEA+start(0)*ICC+start(0)*SCC+start(0)*ICA+start(0)*SCA+start(0)*laethnic+start(0)*llivs21+start(0)*lrel21+start(0)*lstatus21D1+start(0)*lstatus21D2+start(0)*lases+start(0)*lu1chqualp1+start(0)*lu1pchild1+c(bc1,bc1)*laethnic+c(bc2,bc2)*llivs21+c(bc3m,bc3f)*lrel21+c(bc4,bc4)*lstatus21D1+c(bc5,bc5)*lstatus21D2+c(bc6,bc6)*lases+c(bc7,bc7)*lu1chqualp1+c(bc8,bc8)*lu1pchild1+c(b1,b1)*IEC+c(b2,b2)*SEC+c(b3m,b3f)*IEA+c(b4,b4)*SEA+c(b5,b5)*ICC+c(b6,b6)*SCC+c(b7,b7)*ICA+c(b8,b8)*SCA
laethnic=~1*aethnic
aethnic~~0*aethnic
llivs21=~1*livs21
livs21~~0*livs21
lrel21=~1*rel21
rel21~~0*rel21
lstatus21D1=~1*status21D1
status21D1~~0*status21D1
lstatus21D2=~1*status21D2
status21D2~~0*status21D2
lases=~1*ases
ases~~0*ases
lu1chqualp1=~1*u1chqualp1
u1chqualp1~~0*u1chqualp1
lu1pchild1=~1*u1pchild1
u1pchild1~~0*u1pchild1
' 
ALCmodel1_cov_mg8i<-cfa(ALCmodel1_cov_mg8i,data=TEDS,estimator="MLR",missing='ML',cluster="randomfamid",meanstructure=TRUE,group="sex1",
                        group.equal = c("lv.variances","intercepts","means","lv.covariances","residuals","residual.covariances"),
                        group.partial = c("u2calcoauditrage1~~u2calcoauditrage1","QEA~1","SEA~1","ICC~1","ICA~1","QCA~1","IEC~1","SEC~1","IEA~1","IEA~~IEA","ICA~~ICA","ICC~~ICC","SCC~~SCC","SEC~~SEC","SCC~~ICA","ICC~~ICA","ICC~~SCA","SEC~~IEA","ICA~~SCA","icscont1~~icscont1","pcbhsdqemot1~~pcbhsdqemot1","lcsdqcont1~~lcsdqcont1","gpscont1~~gpscont1"))
summary(ALCmodel1_cov_mg8i,standardized=T,fit.measures=T)
anova(ALCmodel1_cov_mg7r4,ALCmodel1_cov_mg8i)#sig

#free up residual variance of AUDIT
ALCmodel1_cov_mg8v<-'
ipsanxt1~~icsanxt1
emo41~~dsdcont1
gpsanxt1~~gpscont1
ipsanxt1~~ipscont1
icsanxt1~~icscont1
lcsdqemot1~~lcsdqcont1
ipsanxt1~~icscont1
IEC~~0*SEC+0*SCC
SEC~~0*SEA+0*SCC+0*SCA
IEA~~0*SCC
SEA~~0*SCC
IEC=~1*emo41+1*gpsanxt1+1*ipsanxt1
SEC=~0*emo41+0.3*gpsanxt1+0.5*ipsanxt1
QEC=~0*emo41+0.09*gpsanxt1+0.25*ipsanxt1
IEA=~1*icsanxt1+1*lcsdqemot1+1*pcbhsdqemot1
SEA=~0*icsanxt1+0.22*lcsdqemot1+0.73*pcbhsdqemot1
QEA=~0*icsanxt1+0.05*lcsdqemot1+0.53*pcbhsdqemot1
ICC=~1*dsdcont1+1*gpscont1+1*ipscont1
SCC=~0*dsdcont1+0.3*gpscont1+0.5*ipscont1
QCC=~0*dsdcont1+0.09*gpscont1+0.25*ipscont1
ICA=~1*icscont1+1*lcsdqcont1+1*pcbhsdqcont1
SCA=~0*icscont1+0.22*lcsdqcont1+0.73*pcbhsdqcont1
QCA=~0*icscont1+0.05*lcsdqcont1+0.53*pcbhsdqcont1
emo41~0*1
gpsanxt1~0*1
ipsanxt1~0*1
icsanxt1~0*1
lcsdqemot1~0*1
pcbhsdqemot1~0*1
dsdcont1~0*1
gpscont1~0*1
ipscont1~0*1
icscont1~0*1
lcsdqcont1~0*1
pcbhsdqcont1~0*1
IEC~1
SEC~1
IEA~1
SEA~1
ICC~1
SCC~1
ICA~1
SCA~1
QEC~1
QEA~1
QCC~1
QCA~1
QEC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QEA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEC~~0*QEC
QEA~~0*QEA
QCC~~0*QCC
QCA~~0*QCA
u2calcoauditrage1~start(0)*IEC+start(0)*SEC+start(0)*IEA+start(0)*SEA+start(0)*ICC+start(0)*SCC+start(0)*ICA+start(0)*SCA+start(0)*laethnic+start(0)*llivs21+start(0)*lrel21+start(0)*lstatus21D1+start(0)*lstatus21D2+start(0)*lases+start(0)*lu1chqualp1+start(0)*lu1pchild1+c(bc1,bc1)*laethnic+c(bc2,bc2)*llivs21+c(bc3m,bc3f)*lrel21+c(bc4,bc4)*lstatus21D1+c(bc5,bc5)*lstatus21D2+c(bc6,bc6)*lases+c(bc7,bc7)*lu1chqualp1+c(bc8,bc8)*lu1pchild1+c(b1,b1)*IEC+c(b2,b2)*SEC+c(b3m,b3f)*IEA+c(b4,b4)*SEA+c(b5,b5)*ICC+c(b6,b6)*SCC+c(b7,b7)*ICA+c(b8,b8)*SCA
laethnic=~1*aethnic
aethnic~~0*aethnic
llivs21=~1*livs21
livs21~~0*livs21
lrel21=~1*rel21
rel21~~0*rel21
lstatus21D1=~1*status21D1
status21D1~~0*status21D1
lstatus21D2=~1*status21D2
status21D2~~0*status21D2
lases=~1*ases
ases~~0*ases
lu1chqualp1=~1*u1chqualp1
u1chqualp1~~0*u1chqualp1
lu1pchild1=~1*u1pchild1
u1pchild1~~0*u1pchild1
' 
ALCmodel1_cov_mg8v<-cfa(ALCmodel1_cov_mg8v,data=TEDS,estimator="MLR",missing='ML',cluster="randomfamid",meanstructure=TRUE,group="sex1",
                        group.equal = c("lv.variances","intercepts","means","lv.covariances","residuals","residual.covariances"),
                        group.partial = c("u2calcoauditrage1~~u2calcoauditrage1","QEA~1","SEA~1","ICC~1","ICA~1","QCA~1","IEC~1","SEC~1","IEA~1","IEA~~IEA","ICA~~ICA","ICC~~ICC","SCC~~SCC","SEC~~SEC","SCC~~ICA","ICC~~ICA","ICC~~SCA","SEC~~IEA","ICA~~SCA","icscont1~~icscont1","pcbhsdqemot1~~pcbhsdqemot1","lcsdqcont1~~lcsdqcont1","gpscont1~~gpscont1"))
summary(ALCmodel1_cov_mg8v,standardized=T,fit.measures=T)
anova(ALCmodel1_cov_mg7r4,ALCmodel1_cov_mg8v)#sig

#final model is ALCmodel1_cov_mg7r4
#use ALCmodel1_cov_mg7r4
#constrain everything related to covariates, except regression coefficients
#including covariance with latent growth factors, covariance among covariates, covariates intercepts and variances
#constrain means of latent growth variables, free up "SEA~1","ICC~1","ICA~1","QCA~1","IEC~1","SEC~1","IEA~1","QEA~1"
#constrain variances of latent growth variables, free up "IEA~~IEA","ICA~~ICA","ICC~~ICC","SCC~~SCC","SEC~~SEC"
#constrain covariances of latent growth variables, free up "SCC~~ICA","ICC~~ICA","ICC~~SCA","SEC~~IEA","ICA~~SCA"
#constrain residual variances and covariances of indicators of latent growth variables, free up "icscont1~~icscont1","pcbhsdqemot1~~pcbhsdqemot1","lcsdqcont1~~lcsdqcont1","gpscont1~~gpscont1"
#regressions coefficients constrainted except for IEA, rel21
#intercept and residual variance of AUDIT freely estimate
ALCmodel1_cov_mg7r4<-'
ipsanxt1~~icsanxt1
emo41~~dsdcont1
gpsanxt1~~gpscont1
ipsanxt1~~ipscont1
icsanxt1~~icscont1
lcsdqemot1~~lcsdqcont1
ipsanxt1~~icscont1
IEC~~0*SEC+0*SCC
SEC~~0*SEA+0*SCC+0*SCA
IEA~~0*SCC
SEA~~0*SCC
IEC=~1*emo41+1*gpsanxt1+1*ipsanxt1
SEC=~0*emo41+0.3*gpsanxt1+0.5*ipsanxt1
QEC=~0*emo41+0.09*gpsanxt1+0.25*ipsanxt1
IEA=~1*icsanxt1+1*lcsdqemot1+1*pcbhsdqemot1
SEA=~0*icsanxt1+0.22*lcsdqemot1+0.73*pcbhsdqemot1
QEA=~0*icsanxt1+0.05*lcsdqemot1+0.53*pcbhsdqemot1
ICC=~1*dsdcont1+1*gpscont1+1*ipscont1
SCC=~0*dsdcont1+0.3*gpscont1+0.5*ipscont1
QCC=~0*dsdcont1+0.09*gpscont1+0.25*ipscont1
ICA=~1*icscont1+1*lcsdqcont1+1*pcbhsdqcont1
SCA=~0*icscont1+0.22*lcsdqcont1+0.73*pcbhsdqcont1
QCA=~0*icscont1+0.05*lcsdqcont1+0.53*pcbhsdqcont1
emo41~0*1
gpsanxt1~0*1
ipsanxt1~0*1
icsanxt1~0*1
lcsdqemot1~0*1
pcbhsdqemot1~0*1
dsdcont1~0*1
gpscont1~0*1
ipscont1~0*1
icscont1~0*1
lcsdqcont1~0*1
pcbhsdqcont1~0*1
IEC~1
SEC~1
IEA~1
SEA~1
ICC~1
SCC~1
ICA~1
SCA~1
QEC~1
QEA~1
QCC~1
QCA~1
QEC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QEA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCC+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCC~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*QCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QCA~~0*IEC+0*SEC+0*IEA+0*SEA+0*ICC+0*SCC+0*ICA+0*SCA+0*laethnic+0*llivs21+0*lrel21+0*lstatus21D1+0*lstatus21D2+0*lases+0*lu1chqualp1+0*lu1pchild1
QEC~~0*QEC
QEA~~0*QEA
QCC~~0*QCC
QCA~~0*QCA
u2calcoauditrage1~start(0)*IEC+start(0)*SEC+start(0)*IEA+start(0)*SEA+start(0)*ICC+start(0)*SCC+start(0)*ICA+start(0)*SCA+start(0)*laethnic+start(0)*llivs21+start(0)*lrel21+start(0)*lstatus21D1+start(0)*lstatus21D2+start(0)*lases+start(0)*lu1chqualp1+start(0)*lu1pchild1+c(bc1,bc1)*laethnic+c(bc2,bc2)*llivs21+c(bc3m,bc3f)*lrel21+c(bc4,bc4)*lstatus21D1+c(bc5,bc5)*lstatus21D2+c(bc6,bc6)*lases+c(bc7,bc7)*lu1chqualp1+c(bc8,bc8)*lu1pchild1+c(b1,b1)*IEC+c(b2,b2)*SEC+c(b3m,b3f)*IEA+c(b4,b4)*SEA+c(b5,b5)*ICC+c(b6,b6)*SCC+c(b7,b7)*ICA+c(b8,b8)*SCA
laethnic=~1*aethnic
aethnic~~0*aethnic
llivs21=~1*livs21
livs21~~0*livs21
lrel21=~1*rel21
rel21~~0*rel21
lstatus21D1=~1*status21D1
status21D1~~0*status21D1
lstatus21D2=~1*status21D2
status21D2~~0*status21D2
lases=~1*ases
ases~~0*ases
lu1chqualp1=~1*u1chqualp1
u1chqualp1~~0*u1chqualp1
lu1pchild1=~1*u1pchild1
u1pchild1~~0*u1pchild1
' 
ALCmodel1_cov_mg7r4<-cfa(ALCmodel1_cov_mg7r4,data=TEDS,estimator="MLR",missing='ML',cluster="randomfamid",meanstructure=TRUE,group="sex1",
                         group.equal = c("lv.variances","intercepts","means","lv.covariances","residuals","residual.covariances"),
                         group.partial = c("u2calcoauditrage1~1","u2calcoauditrage1~~u2calcoauditrage1","QEA~1","SEA~1","ICC~1","ICA~1","QCA~1","IEC~1","SEC~1","IEA~1","IEA~~IEA","ICA~~ICA","ICC~~ICC","SCC~~SCC","SEC~~SEC","SCC~~ICA","ICC~~ICA","ICC~~SCA","SEC~~IEA","ICA~~SCA","icscont1~~icscont1","pcbhsdqemot1~~pcbhsdqemot1","lcsdqcont1~~lcsdqcont1","gpscont1~~gpscont1"))
summary(ALCmodel1_cov_mg7r4,standardized=T,fit.measures=T)