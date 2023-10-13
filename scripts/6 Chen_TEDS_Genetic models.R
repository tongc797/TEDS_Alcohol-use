#code for Chen, T., Oginni, O. A., Hannigan, L. J., Eley, T. C., Maggs, J. L., Linden-Carmichael, A. N., & Neiderhiser, J. M.. Developmental trajectories of child and adolescent emotional problems: Associations with early adult alcohol use behaviors.

# Adapted from F.V. Rijsdijk January 2019 by KAOginni 2020/2022
# Adapted by Tong Chen

# To determine causal effects of latent growth factors of emotional and conduct problems on alcohol use
# Heterogeneity variable: 	Sex
# Zygosity variable: 		zyg (1=MZM, 2=DZM, 3=MZF, 4=DZF, 5=OSDZ)
# Note: the script was adapted from analysis using latent factors, therefore, all factor loadings are constrained as 1, all residuals are constrained as 0, so that the factors equal the observed variables.
# ------------------------------------------------------------------------------------

#a. Predictor: 
# i. SEC
# ii. ICA
# iii. SCA

#b. Outcome:
# i. AUDIT/AUDITP

#a. Predictor: 
# i. IEA
# ii. SCA

#b. Outcome:
# i. AUDITC

# PART 1: By Zygosity groups: MZ/DZ 
#_______________________________________
# Note: In all models Sex effects are regressed out before modelling, age regressed out for all AUDIT variables

rm(list=ls())
ls()
Sys.setenv(OMP_NUM_THREADS=parallel::detectCores()) 

#check you have the packages:
search()

library(psych)
library(OpenMx)
#library(Hmisc)			
library(dplyr)
library(bestNormalize)
library(rcompanion)#https://www.rdocumentation.org/packages/rcompanion/versions/2.4.15/topics/blom
library(gmodels)

mxOption(NULL, "Default optimizer", "NPSOL") 

# ******************************************************************************************************************************************
# (1) Read in data file and check descriptive statistics
# ******************************************************************************************************************************************

# Reads data 

Data <- read.csv("TEDSfac.csv")
#only used when doing sex difference
Data$random[(Data$sexzyg == 5) & (Data$sex1 == 1)] <- 1
Data$random[(Data$sexzyg == 5) & (Data$sex1 == 0)] <- 0

attr(Data, "var.labels")
summary(Data)
psych::describe(Data)
str(Data)
dim(Data)
names(Data)

# ******************************************************************************************************************************************
# (2) Explore variable of interest: recode, regress-out age and sex, check distribution and transform if necessary.
# ******************************************************************************************************************************************

table(Data$sex1,Data$sexzyg)

#---------

Data$SEC_resid <- residuals(lm(Data$SEC ~ Data$sex1,na.action="na.exclude"))
psych::describe(Data$SEC)
psych::describe(Data$SEC_resid)
multi.hist(Data$SEC)
multi.hist(Data$SEC_resid)

Data$SEC_residT <- Data$SEC_resid +2
psych::describe(Data$SEC_residT)
multi.hist(Data$SEC_residT)

#---------

Data$ICA_resid <- residuals(lm(Data$ICA ~ Data$sex1, na.action="na.exclude"))
psych::describe(Data$ICA)
psych::describe(Data$ICA_resid)
multi.hist(Data$ICA)
multi.hist(Data$ICA_resid)

Data$ICA_residT <- Data$ICA_resid +2
psych::describe(Data$ICA_residT)
multi.hist(Data$ICA_residT)

#---------

Data$IEA_resid <- residuals(lm(Data$IEA ~ Data$sex1, na.action="na.exclude"))
psych::describe(Data$IEA)
psych::describe(Data$IEA_resid)
multi.hist(Data$IEA)
multi.hist(Data$IEA_resid)

Data$IEA_residT <- Data$IEA_resid +2
psych::describe(Data$IEA_residT)
multi.hist(Data$IEA_residT)

#---------

Data$SCA_resid <- residuals(lm(Data$SCA ~ Data$sex1, na.action="na.exclude"))
psych::describe(Data$SCA)
psych::describe(Data$SCA_resid)
multi.hist(Data$SCA)
multi.hist(Data$SCA_resid)

Data$SCA_residT <- Data$SCA_resid +2
psych::describe(Data$SCA_residT)
multi.hist(Data$SCA_residT)

#---------

Data$AUDIT_resid <- residuals(lm(Data$u2calcoaudit1 ~  Data$u2cage1+Data$sex1, na.action="na.exclude"))
psych::describe(Data$u2calcoaudit1)
psych::describe(Data$AUDIT_resid)
multi.hist(Data$u2calcoaudit1)
multi.hist(Data$AUDIT_resid)

Data$AUDIT_residT <- (Data$AUDIT_resid/5) +2
psych::describe(Data$AUDIT_residT)
multi.hist(Data$AUDIT_residT)

#---------

Data$AUDITC_resid <- residuals(lm(Data$u2calcoauditc1 ~ Data$u2cage1+Data$sex1, na.action="na.exclude"))
psych::describe(Data$u2calcoauditc1)
psych::describe(Data$AUDITC_resid)
multi.hist(Data$u2calcoauditc1)
multi.hist(Data$AUDITC_resid)

Data$AUDITC_residT <- (Data$AUDITC_resid/2) +2
psych::describe(Data$AUDITC_residT)
multi.hist(Data$AUDITC_residT)

#---------

Data$AUDITP_resid <- residuals(lm(Data$u2calcoauditp1 ~ Data$u2cage1+Data$sex1, na.action="na.exclude"))
psych::describe(Data$u2calcoauditp1)
psych::describe(Data$AUDITP_resid)
multi.hist(Data$u2calcoauditp1)
multi.hist(Data$AUDITP_resid)

Data$AUDITP_residlogT <- log(Data$AUDITP_resid + 10)*5
psych::describe(Data$AUDITP_residlogT)
multi.hist(Data$AUDITP_residlogT)

#---------
psych::describe(Data)

# ******************************************************************************************************************************************
# (3) Generate Pair-Wise File
# ******************************************************************************************************************************************
# Subset only variables of interest

SubData <-Data[,c("randomfamid","sexzyg","sex1","u1cage1","random","SEC_residT","ICA_residT","IEA_residT","SCA_residT","AUDIT_residT","AUDITC_residT","AUDITP_residlogT")]

psych::describe(SubData)
summary(SubData)
str(SubData)

#Rename variables 
colnames(SubData) <- c('family','zyg','sex','age1','TwNo','SEC','ICA','IEA','SCA','AUDIT','AUDITC','AUDITP') 
names(SubData)

TWINdata <- reshape(SubData, idvar = c("family"), timevar = "TwNo", direction = "wide")
names(TWINdata) 
psych::describe(TWINdata)

TWINdata2 <-TWINdata[,c('family','zyg.0','sex.0','age1.0','SEC.0','ICA.0','IEA.0','SCA.0','AUDIT.0','AUDITC.0','AUDITP.0',
                        'zyg.1','sex.1','age1.1','SEC.1','ICA.1','IEA.1','SCA.1','AUDIT.1','AUDITC.1','AUDITP.1')]

psych::describe(TWINdata2)

# ******************************************************************************************************************************************
# (4) Rename Variables
# Rename variables using shorter names and to get rid of the . in the names (OpenMx cannot handle . in names)
# ******************************************************************************************************************************************
colnames(TWINdata2) <- c('family','zyg2','sex2','age12','SEC2','ICA2','IEA2','SCA2','AUDIT2','AUDITC2','AUDITP2',
                         'zyg1','sex1','age11','SEC1','ICA1','IEA1','SCA1','AUDIT1','AUDITC1','AUDITP1') 

names(TWINdata2)
psych::describe(TWINdata2)

#######################################################

#******************
#2-VARIABLE MODELS (one predictor and one outcome)
#******************
#same for all predictors and outcome, just change variable names

nv			<- 2				# number of variables for a twin = 1 in Univariate
ntv			<- 2*nv			#number of variables for a pair = 2* 1 for Univariate
nlower		<- nv*(nv+1)/2 		#number of free elements in a lower matrix nv*nv
ncor			<- (nv*(nv+1)/2)-nv	#number of free elements in a correlation matrix nv*nv
nfact			<- 2				# number of Latent Factors for Causal Model per twin
nfact2		<- 2*nfact			# number of Latent Factors for Causal Model per twin pair
nfcor			<- (nfact*(nfact+1)/2)-nfact	# number of free elements in a correlation matrix nfact*nfcat
Groups		<- c("mz", "dz")
Vars			<- c("SEC","AUDIT")
selVars		<- c("SEC1","AUDIT1",
              "SEC2","AUDIT2")

mzData		<- subset(TWINdata2, zyg1%in%c(1,3)|zyg2%in%c(1,3) , selVars)
dzData		<- subset(TWINdata2, zyg1%in%c(2,4,5)|zyg2%in%c(2,4,5) , selVars)

psych::describe(mzData)
psych::describe(dzData)

#*******************************************************************************************************
# Specify the Models

# _______________________________________________________________________________________________________________________
# ACE Factor MODEL by zygosity
# NO causal paths between Phenotypic Factors; A, C and E latent factors have Cholesky Structure
# Correlation between Phenotypic Factors only due to shared A, C and E influences
#_____________________________________________________________________________________________________________________________

# CREATE LABELS & START VALUES as objects
(mLabs	<- paste("m",1:nv,sep=""))
(Stmean	<- colMeans(mzData[,1:nv],na.rm=TRUE))
(Stsd 	<- sapply(mzData[,1:nv],sd, na.rm=TRUE))
(PatM		<- c(TRUE,TRUE))

# Create Labels for Diagonal Matrices-fixed to 0
(LabEs	<- c('es1','es2'))
(LabAs	<- c('as1','as2'))
(LabCs	<- c('cs1','cs2'))

PatSp		<- c(F,F)
StSpa		<- c(0,0)
StSpc		<- c(0,0)
StSpe		<- c(0,0)

# all factor loadings fixed to 1
PatFl		<- c(F,F,			
            F,F)

StFl		<- c(1,0,
           0,1)

LabFl		<- c('l1',NA,
            NA,'l2')

PatPhC	<- c(F,F,
            F,F)

StPhC		<- c(0,0, #This is the matrix for the causal paths - all fixed to 0
            0,0)

LabPhC	<- c(NA,NA,
            NA,NA)	 

# ______________________________________________________________________________________________________
# Define matrices to hold the Means, SD, correlations
# Use Algebra to generate expected var/cov matrices and Means
# Specify: data objects, Fitfunction, the Model, 
# Run the Model 
# ______________________________________________________________________________________________________

Means		<-mxMatrix("Full", 1, ntv, free=c(PatM,PatM), values=c(Stmean,Stmean), labels=c(mLabs,mLabs), name="expMean") 

# Define matrices to specify the loadings of the dependent variables on the latent factors-fixed to 1
Load		<-mxMatrix(type="Full",	nrow=nv, ncol=nfact, free=PatFl, values=StFl, labels=LabFl, name="FactL" )
Id2		<-mxMatrix(type="Iden",	nrow=2, ncol=2, free=F, name="I2" )
LoadTw	<-mxAlgebra(I2%x%FactL, name="FactLTw")

# Define the matrix to hold the Single headed Arrows (causal paths) between the 4 latent variables  
PhCaus	<-mxMatrix(type="Full",	nrow=nfact, ncol=nfact, free=PatPhC, values=StPhC, labels=LabPhC, name="PhC" )

# Define the matrix to hold the A and C and E effects: Specific-fixed to 0 
PathsAs	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpa, labels=LabAs, name="as" )
PathsCs	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpc, labels=LabCs, name="cs" )
PathsEs	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpe, labels=LabEs, name="es" )

covAs		<-mxAlgebra( expression= as %*% t(as), name="As" )
covCs		<-mxAlgebra( expression= cs %*% t(cs), name="Cs" )
covEs		<-mxAlgebra( expression= es %*% t(es), name="Es" )

covPs		<-mxAlgebra( expression= As+Cs+Es, name="Vs" )

# Define the matrices to hold the A and C and E effects: Common 
PathsAc	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=TRUE, values=.6, labels=c("a11","a21","a22"), name="a_c" )
PathsCc	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=TRUE, values=.05, labels=c("c11","c21","c22"), name="c_c" )
PathsEc	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=TRUE, values=.5, labels=c("e11","e21","e22"), name="e_c" )

covAc		<-mxAlgebra( expression= a_c %*% t(a_c), name="Ac" )
covCc		<-mxAlgebra( expression= c_c %*% t(c_c), name="Cc" )
covEc		<-mxAlgebra( expression= e_c %*% t(e_c), name="Ec" )
covPc		<-mxAlgebra( expression= Ac+Cc+Ec, name="Vc" )

# Generate Covariance of Latent factor model Including Causal Paths between factors
Id2		<-mxMatrix(type="Iden",	nrow=nfact, ncol=nfact, name="I2" )
covFAc	<-mxAlgebra( expression= solve(I2-PhC) %&% Ac, name ="FAc") #(I2-PhC) gives the expression for the removal of the loop effect of causal relationships between the factors (1-4).
covFCc	<-mxAlgebra( expression= solve(I2-PhC) %&% Cc, name ="FCc")
covFEc	<-mxAlgebra( expression= solve(I2-PhC) %&% Ec, name ="FEc")
covFc		<-mxAlgebra( expression= FAc+FCc+FEc, name="FVc" )

# Var-Cov of measured vars in terms of latent factors and AC, Cc, and Ec
FcovMZ	<-mxAlgebra( expression= (FactLTw  %&% rbind ( cbind(FVc, FAc+FCc), cbind(FAc+FCc, FVc) )) , name="expFCovMZ" )#This traces the path from vars to factors and back to vars
FcovDZ	<-mxAlgebra( expression= (FactLTw  %&% rbind ( cbind(FVc, .5%x%FAc+FCc), cbind(.5%x%FAc+FCc, FVc) )) , name="expFCovDZ" )

SpcovMZ	<-mxAlgebra( expression= rbind (cbind(Vs, As+Cs), cbind(As+Cs, Vs) ) , name="expSpCovMZ" )
SpcovDZ	<-mxAlgebra( expression= rbind (cbind(Vs, .5%x%As+Cs), cbind(.5%x%As+Cs, Vs) ) , name="expSpCovDZ" )

TOTcovMZ	<-mxAlgebra( expression= expFCovMZ + expSpCovMZ , name="TOTexpCovMZ" )
TOTcovDZ	<-mxAlgebra( expression= expFCovDZ + expSpCovDZ , name="TOTexpCovDZ" )

# *******************************************************************************************************
# Calculator

# Calculate genetic,  environmental, and phenotypic correlations
corA		<- mxAlgebra( expression=solve(sqrt(I2*FAc))%&%FAc, name ="rA" ) 
corC		<- mxAlgebra( expression=solve(sqrt(I2*FCc))%&%FCc, name ="rC" )
corE		<- mxAlgebra( expression=solve(sqrt(I2*FEc))%&%FEc, name ="rE" )
corP		<- mxAlgebra( expression=solve(sqrt(I2*FVc))%&%FVc, name ="rP") 

# Standardize the Total var/covariances matrices of the observed variables
Id4		<-mxMatrix(type="Iden",	nrow=ntv, ncol=ntv, name="I4" )
Rfactmz	<-mxAlgebra( expression= solve(sqrt(I4*TOTexpCovMZ)) %&% TOTexpCovMZ, name="FactcorMZ" )
Rfactdz	<-mxAlgebra( expression= solve(sqrt(I4*TOTexpCovDZ)) %&% TOTexpCovDZ, name="FactcorDZ" ) #compare with phenotypic correlation results

# Standardize the Common Effects
stcovAc	<-mxAlgebra( expression= FAc/FVc, name="stAc" )
stcovCc	<-mxAlgebra( expression= FCc/FVc, name="stCc" )
stcovEc	<-mxAlgebra( expression= FEc/FVc, name="stEc" )

# Standardize the Specific Effects
stcovAs	<-mxAlgebra( expression= As/( (FactL %&% FVc) +Vs), name="stAs" )
stcovCs	<-mxAlgebra( expression= Cs/( (FactL %&% FVc) +Vs), name="stCs" )
stcovEs	<-mxAlgebra( expression= Es/( (FactL %&% FVc) +Vs), name="stEs" )

# Standardized Effects of Individual variables from the factors (Variance components) above
stAvar	<-mxAlgebra( expression= ((FactL %&% FAc)/( (FactL %&% FVc) +Vs)), name="stAvariables" )
stCvar	<-mxAlgebra( expression= ((FactL %&% FCc)/( (FactL %&% FVc) +Vs)), name="stCvariables" )
stEvar	<-mxAlgebra( expression= ((FactL %&% FEc)/( (FactL %&% FVc) +Vs)), name="stEvariables" )

# Standardized Factor Loadings
StFL		<-mxAlgebra( expression= (diag2vec( FactL %&% FVc / TOTexpCovMZ[1:2,1:2])) , name="StandFact" )

# *******************************************************************************************************

# Data objects for Multiple Groups
dataMZ	<- mxData( observed=mzData, type="raw" )
dataDZ	<- mxData( observed=dzData, type="raw" )

# Objective objects for Multiple Groups
objMZ		<- mxExpectationNormal( covariance="TOTexpCovMZ", means="expMean", dimnames=selVars)
objDZ		<- mxExpectationNormal( covariance="TOTexpCovDZ", means="expMean", dimnames=selVars)

fitFunction <- mxFitFunctionML()

# Combine Groups
pars1		<-list(Means,Load,LoadTw,PhCaus,PathsAs,PathsAs,PathsCs,PathsEs,covAs,covCs,covEs,covPs,Id4,Id2, stAvar, stCvar, stEvar)
pars2		<-list(PathsAc,PathsCc,PathsEc,covAc,covCc,covEc,covPc,covFAc,covFCc,covFEc,covFc,stcovAc,stcovCc,stcovEc, stcovAs, stcovCs, stcovEs, corA, corC, corE)
modelMZ	<-mxModel(pars1, pars2, FcovMZ, SpcovMZ, TOTcovMZ, dataMZ, objMZ, Rfactmz, fitFunction, StFL, name="MZ" )
modelDZ	<-mxModel(pars1, pars2, FcovDZ, SpcovDZ, TOTcovDZ, dataDZ, objDZ, Rfactdz, fitFunction, name="DZ" )
minus2ll	<-mxAlgebra( expression=MZ.objective + DZ.objective, name="m2LL" )
obj		<-mxFitFunctionAlgebra( "m2LL" )
cistFL	<-mxCI (c ('MZ.StandFact','MZ.rA','MZ.rE'))
cistFc	<-mxCI (c ('MZ.stAc','MZ.stEc') ) 	# standardized var comp from Common feactors	
cistVs	<-mxCI (c ('MZ.stAs','MZ.stEs') ) 	# standardized var comp from specific Factors
#cistvars	<-mxCI (c ('MZ.stAvariables','MZ.stCvariables','MZ.stEvariables'))
ACE2Model	<-mxModel("ace2", pars1, pars2, modelMZ, modelDZ, minus2ll, obj, cistFc, cistFL, cistVs) 

# --------------------------------------------------------------------------------------------------------------------------------
# RUN ACE Factor Model: Cholesky (by Zygosity)

ACE2Fit		<-mxRun(ACE2Model, intervals=F)
(ACE2Summ		<-summary(ACE2Fit, verbose=F))

# Get some output

mxEval(MZ.Vs, ACE2Fit)

mxEval(MZ.FAc, ACE2Fit)
mxEval(MZ.FCc, ACE2Fit)
mxEval(MZ.FEc, ACE2Fit)
mxEval(MZ.FVc, ACE2Fit)
mxEval(MZ.PhC, ACE2Fit)

mxEval(MZ.stAc, ACE2Fit)
mxEval(MZ.stCc, ACE2Fit)
mxEval(MZ.stEc, ACE2Fit)

mxEval(MZ.stAs, ACE2Fit)
mxEval(MZ.stCs, ACE2Fit)
mxEval(MZ.stEs, ACE2Fit)

mxEval(MZ.StandFact, ACE2Fit)

mxEval(MZ.stAvariables, ACE2Fit)
mxEval(MZ.stCvariables, ACE2Fit)
mxEval(MZ.stEvariables, ACE2Fit)



#****************************************************************************************************************************
# _______________________________________________________________________________________________________________________
# Genetic Factor MODEL with causal paths by zygosity
# 2-variable version
# Causal paths specified between the predictor and the outcome
#_____________________________________________________________________________________________________________________________

nv		<- 2				# number of variables for a twin = 1 in Univariate
ntv		<- 2*nv			# number of variables for a pair = 2* 1 for Univariate
nfact		<- 2				# number of Latent Factors for Causal Model per twin
nfact2	<- 2*nfact			# number of Latent Factors for Causal Model per twin pair
nlower	<- ntv*(ntv+1)/2 		# number of free elements in a lower matrix ntv*ntv

# CREATE LABELS & START VALUES as objects(to ease specification in the body of the model)
(mLabs	<- paste("m",1:nv,sep=""))
(Stmean	<-colMeans(mzData[,1:nv],na.rm=TRUE))
(Stsd 	<-sapply(mzData[,1:nv],sd, na.rm=TRUE))
(PatM		<- c(TRUE,TRUE))

# specific A, C, E factors-fixed to 0
(LabEs	<- c('es1','es2'))
(LabAs	<- c('as1','as2'))
(LabCs	<- c('cs1','cs2'))

PatSp		<- c(F,F)
StSpa		<- c(0,0)
StSpc		<- c(0,0)
StSpe		<- c(0,0)

# all factor loadings fixed to 1
PatFl		<- c(F,F,			
            F,F)

StFl		<- c(1,0,
           0,1)

LabFl		<- c('l1',NA,
            NA,'l2')

PatPhC	<- c(F,TRUE,
            F,F)

StPhC		<- c(0,.1,
            0,0)

LabPhC	<- c(NA,'c1on2',
            NA,NA)	 

#______________________________________________________________________________________________________
# Define matrices to hold the Means, SD, correlations
# Use Algebra to generate expected var/cov matrices and Means
# Specify: data objects, Fitfunction, the Model, 
# Run the Model 
#______________________________________________________________________________________________________

Means		<-mxMatrix("Full", 1, ntv, free=c(PatM,PatM), values=c(Stmean,Stmean), labels=c(mLabs,mLabs), name="expMean") 

# Define matrices to specify the loadings of the dependent variables on the latent factors-fixed to 1
Load		<-mxMatrix(type="Full",	nrow=nv, ncol=nfact, free=PatFl, values=StFl, labels=LabFl, name="FactL" )
Id2		<-mxMatrix(type="Iden",	nrow=2, ncol=2, free=F, name="I2" )
LoadTw	<-mxAlgebra(I2%x%FactL, name="FactLTw")

# Define the matrix to hold the Single headed Arrows (causal paths) between the 2 latent variables  
PhCaus	<-mxMatrix(type="Full",	nrow=nfact, ncol=nfact, free=PatPhC, values=StPhC, labels=LabPhC, name="PhC" )

# Define the matrix to hold the A and C and E effects: Specific-fixed to 0 
PathsAs	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpa, labels=LabAs, name="as" )
PathsCs	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpc, labels=LabCs, name="cs" )
PathsEs	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpe, labels=LabEs, name="es" )
covAs		<-mxAlgebra( expression= as %*% t(as), name="As" )
covCs		<-mxAlgebra( expression= cs %*% t(cs), name="Cs" )
covEs		<-mxAlgebra( expression= es %*% t(es), name="Es" )
covPs		<-mxAlgebra( expression= As+Cs+Es, name="Vs" )

# Define the matrices to hold the A and C and E effects: Common 
PathsAcsp	<-mxMatrix(type="Diag",	nrow=nfact, ncol=nfact, free=TRUE, values=.8, labels=c("a_csp1","a_csp2"), name="ac" )
PathsCcsp	<-mxMatrix(type="Diag",	nrow=nfact, ncol=nfact, free=TRUE, values=.1, labels=c("c_csp1","c_csp2"), name="cc" )
PathsEcsp	<-mxMatrix(type="Diag",	nrow=nfact, ncol=nfact, free=TRUE, values=.8, labels=c("e_csp1","e_csp2"), name="ec" )
covAc		<-mxAlgebra( expression= ac %*% t(ac), name="Ac" )
covCc		<-mxAlgebra( expression= cc %*% t(cc), name="Cc" )
covEc		<-mxAlgebra( expression= ec %*% t(ec), name="Ec" )
covPc		<-mxAlgebra( expression= Ac+Cc+Ec, name="Vc" )

# Generate Covariance of Latent factor model Including Causal Paths between factors
Id2		<-mxMatrix(type="Iden",	nrow=nfact, ncol=nfact, name="I2" )
covFAc	<-mxAlgebra( expression= solve(I2-PhC) %&% Ac, name ="FAc")
covFCc	<-mxAlgebra( expression= solve(I2-PhC) %&% Cc, name ="FCc")
covFEc	<-mxAlgebra( expression= solve(I2-PhC) %&% Ec, name ="FEc")
covFc		<-mxAlgebra( expression= (FAc+FCc+FEc), name="FVc" )

FcovMZ	<-mxAlgebra( expression= (FactLTw  %&% rbind ( cbind(FVc, FAc+FCc), cbind(FAc+FCc, FVc) )) , name="expFCovMZ" )
FcovDZ	<-mxAlgebra( expression= (FactLTw  %&% rbind ( cbind(FVc, .5%x%FAc+FCc), cbind(.5%x%FAc+FCc, FVc) )) , name="expFCovDZ" )

SpcovMZ	<-mxAlgebra( expression= rbind (cbind(Vs, As+Cs), cbind(As+Cs, Vs) ) , name="expSpCovMZ" )
SpcovDZ	<-mxAlgebra( expression= rbind (cbind(Vs, .5%x%As+Cs), cbind(.5%x%As+Cs, Vs) ) , name="expSpCovDZ" )

TOTcovMZ	<-mxAlgebra( expression= expFCovMZ + expSpCovMZ , name="TOTexpCovMZ" )
TOTcovDZ	<-mxAlgebra( expression= expFCovDZ + expSpCovDZ , name="TOTexpCovDZ" )
# *******************************************************************************************************
# Calculator

# Standardize the causal effects
Stcp1on2	<-mxAlgebra( expression= (PhC[2,1]* sqrt(FVc[1,1]))/sqrt(FVc[2,2]) , name="Stand_1on2" )

# Standardize the Total var/covariances matrices of the observed variables
Id4		<-mxMatrix(type="Iden",	nrow=ntv, ncol=ntv, name="I4" )
Rfactmz	<-mxAlgebra( expression= solve(sqrt(I4*TOTexpCovMZ)) %&% TOTexpCovMZ, name="FactcorMZ" )
Rfactdz	<-mxAlgebra( expression= solve(sqrt(I4*TOTexpCovDZ)) %&% TOTexpCovDZ, name="FactcorDZ" )

# Standardize the Common Effects
stcovAc	<-mxAlgebra( expression= FAc/FVc, name="stAc" )
stcovCc	<-mxAlgebra( expression= FCc/FVc, name="stCc" )
stcovEc	<-mxAlgebra( expression= FEc/FVc, name="stEc" )

# Standardize the Specific Effects
stcovAs	<-mxAlgebra( expression= (As/( (FactL %&% FVc) +Vs)), name="stAs" )
stcovCs	<-mxAlgebra( expression= (Cs/( (FactL %&% FVc) +Vs)), name="stCs" )
stcovEs	<-mxAlgebra( expression= (Es/( (FactL %&% FVc) +Vs)), name="stEs" )

# Standardized Effects of Individual variables from the factors (Variance components) above
stAvar	<-mxAlgebra( expression= (FactL %&% FAc)/( (FactL %&% FVc) +Vs), name="stAvariables" )
stCvar	<-mxAlgebra( expression= (FactL %&% FCc)/( (FactL %&% FVc) +Vs), name="stCvariables" )
stEvar	<-mxAlgebra( expression= (FactL %&% FEc)/( (FactL %&% FVc) +Vs), name="stEvariables" )

# Standardized Factor Loadings
StFL		<-mxAlgebra( expression= sqrt(diag2vec( FactL %&% FVc / TOTexpCovMZ[1:2,1:2])) , name="StandFact" )

# *******************************************************************************************************

# Data objects for Multiple Groups
dataMZ	<- mxData( observed=mzData, type="raw" )
dataDZ	<- mxData( observed=dzData, type="raw" )

# Objective objects for Multiple Groups
objMZ		<- mxExpectationNormal( covariance="TOTexpCovMZ", means="expMean", dimnames=selVars)
objDZ		<- mxExpectationNormal( covariance="TOTexpCovDZ", means="expMean", dimnames=selVars)

fitFunction <- mxFitFunctionML()

# Combine Groups
pars1		<-list(Means,Load,LoadTw,PhCaus,PathsAs,PathsCs,PathsEs,covAs,covCs,covEs,covPs,Id4,Id2, stAvar, stCvar, stEvar)
pars2		<-list(PathsAcsp,PathsCcsp,PathsEcsp,covAc,covCc,covEc,covPc,covFAc,covFCc,covFEc,covFc,stcovAc,stcovCc,stcovEc, stcovAs, stcovCs, stcovEs)
parsmed	<-list(Stcp1on2)
modelMZ	<-mxModel(pars1, pars2, parsmed, FcovMZ, SpcovMZ, TOTcovMZ, dataMZ, objMZ, Rfactmz, fitFunction, StFL, name="MZ" )
modelDZ	<-mxModel(pars1, pars2, FcovDZ, SpcovDZ, TOTcovDZ, dataDZ, objDZ, Rfactdz, fitFunction, name="DZ" )
minus2ll	<-mxAlgebra( expression=MZ.objective + DZ.objective, name="m2LL" )
obj		<-mxFitFunctionAlgebra( "m2LL" )
cistFL	<-mxCI (c ('MZ.StandFact','MZ.Stand_1on2'))
cistFc	<-mxCI (c ('MZ.stAc','MZ.stCc','MZ.stEc') ) 	# standardized var comp from Common feactors	
cistVs	<-mxCI (c ('MZ.stAs','MZ.stCs','MZ.stEs') ) 	# standardized var comp from specific Factors
cistvars	<-mxCI (c ('MZ.stAvariables','MZ.stCvariables','MZ.stEvariables'))
ACEMs2Model	<-mxModel("aceMs2", pars1, pars2, modelMZ, modelDZ, minus2ll, obj, cistFc, cistFL, cistVs, cistvars) 

# --------------------------------------------------------------------------------------------------------------------------------
# RUN ACEMs Factor Model with phenotypic causal mediation paths by Zygosity

ACEMs2Fit	<-mxRun(ACEMs2Model, intervals=F)
(ACEMs2Summ	<-summary(ACEMs2Fit, verbose=F))

# Get some output

mxEval(MZ.Vs, ACEMs2Fit)

mxEval(MZ.FAc, ACEMs2Fit)
mxEval(MZ.FCc, ACEMs2Fit)
mxEval(MZ.FEc, ACEMs2Fit)
mxEval(MZ.FVc, ACEMs2Fit)
mxEval(MZ.PhC, ACEMs2Fit)

mxEval(MZ.stAc, ACEMs2Fit)
mxEval(MZ.stCc, ACEMs2Fit)
mxEval(MZ.stEc, ACEMs2Fit)

mxEval(MZ.Ac, ACEMs2Fit)
mxEval(MZ.Cc, ACEMs2Fit)
mxEval(MZ.Ec, ACEMs2Fit)

mxEval(MZ.stAs, ACEMs2Fit)
mxEval(MZ.stCs, ACEMs2Fit)
mxEval(MZ.stEs, ACEMs2Fit)

mxEval(MZ.stAvariables, ACEMs2Fit)
mxEval(MZ.stCvariables, ACEMs2Fit)
mxEval(MZ.stEvariables, ACEMs2Fit)

mxEval(MZ.Stand_1on2, ACEMs2Fit)

#------------------------------------------------------------
# Compare Cholesky model and causal path model
#------------------------------------------------------------
# **************************************************
(mxCompare(ACE2Fit, ACEMs2Fit))

#*******************
# 4-VARIABLE MODELS
#*******************
#SEC, ICA, SCA predicting AUDIT/AUDITP
# ******************************************************************************************************************************************
# Prepare data for modelling - 4 variable Cholesky
# ******************************************************************************************************************************************

nv			<- 4				# number of variables for a twin = 1 in Univariate
ntv			<- 2*nv			#number of variables for a pair = 2* 1 for Univariate
nlower		<- nv*(nv+1)/2 		#number of free elements in a lower matrix nv*nv
ncor			<- (nv*(nv+1)/2)-nv	#number of free elements in a correlation matrix nv*nv
nfact			<- 4				# number of Latent Factors for Causal Model per twin
nfact2		<- 2*nfact			# number of Latent Factors for Causal Model per twin pairs
nfcor			<- (nfact*(nfact+1)/2)-nfact	# number of free elements in a correlation matrix nfact*nfcat
Groups		<- c("mz", "dz")
Vars			<- c("SEC","ICA","SCA","AUDIT")
selVars		<- c("SEC1","ICA1","SCA1","AUDIT1",
              "SEC2","ICA2","SCA2","AUDIT2")

mzData		<- subset(TWINdata2, zyg1%in%c(1,3)|zyg2%in%c(1,3) , selVars)
dzData		<- subset(TWINdata2, zyg1%in%c(2,4,5)|zyg2%in%c(2,4,5) , selVars)

psych::describe(mzData)
psych::describe(dzData)

#*******************************************************************************************************
# Specify the Models
#****************************************************************************************************************************
# _______________________________________________________________________________________________________________________
# ACE Factor MODEL by zygosity
# NO causal paths between Phenotypic Factors; A, C and E latent factors have Cholesky Structure
# Correlation between Phenotypic Factors only due to shared A, C and E influences
#_____________________________________________________________________________________________________________________________

# CREATE LABELS & START VALUES as objects(to ease specification in the body of the model)
(mLabs	<- paste("m",1:nv,sep=""))
(Stmean	<- colMeans(mzData[,1:nv],na.rm=TRUE))
(Stsd 	<- sapply(mzData[,1:nv],sd, na.rm=TRUE))
(PatM		<- c(TRUE,TRUE,TRUE,TRUE))

# Create Labels for Diagonal Matrices-fixed to 0
(LabEs	<- c('es1','es2','es3','es4'))
(LabAs	<- c('as1','as2','as3','as4'))
(LabCs	<- c('cs1','cs2','cs3','cs4'))

PatSp		<- c(F,F,F,F)
StSpa		<- c(0,0,0,0)
StSpc		<- c(0,0,0,0)
StSpe		<- c(0,0,0,0)

# all factor loadings fixed to 1
PatFl		<- c(F,F,F,F,			
            F,F,F,F,
            F,F,F,F,
            F,F,F,F)

StFl		<- c(1,0,0,0,
           0,1,0,0,
           0,0,1,0,
           0,0,0,1)

LabFl		<- c('l1',NA,NA,NA,
            NA,'l2',NA,NA,
            NA,NA,'l3',NA,
            NA,NA,NA,'l4')

PatPhC	<- c(F,F,F,F,
            F,F,F,F,
            F,F,F,F,
            F,F,F,F)

StPhC		<- c(0,0,0,0, #This is the matrix for the causal paths - all fixed to 0
            0,0,0,0,
            0,0,0,0,
            0,0,0,0)

LabPhC	<- c(NA,NA,NA,NA,
            NA,NA,NA,NA,	
            NA,NA,NA,NA,	
            NA,NA,NA,NA)	 

# ______________________________________________________________________________________________________
# Define matrices to hold the Means, SD, correlations
# Use Algebra to generate expected var/cov matrices and Means
# Specify: data objects, Fitfunction, the Model, 
# Run the Model 
# ______________________________________________________________________________________________________

Means		<-mxMatrix("Full", 1, ntv, free=c(PatM,PatM), values=c(Stmean,Stmean), labels=c(mLabs,mLabs), name="expMean") 

# Define matrices to specify the loadings of the dependent variables on the latent factors
Load		<-mxMatrix(type="Full",	nrow=nv, ncol=nfact, free=PatFl, values=StFl, labels=LabFl, name="FactL" )
Id2		<-mxMatrix(type="Iden",	nrow=2, ncol=2, free=F, name="I2" )
LoadTw	<-mxAlgebra(I2%x%FactL, name="FactLTw")

# Define the matrix to hold the Single headed Arrows (causal paths) between the 4 latent variables  
PhCaus	<-mxMatrix(type="Full",	nrow=nfact, ncol=nfact, free=PatPhC, values=StPhC, labels=LabPhC, name="PhC" )

# Define the matrix to hold the A and C and E effects: Specific-fixed to 0 
PathsAs	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpa, labels=LabAs, name="as" )
PathsCs	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpc, labels=LabCs, name="cs" )
PathsEs	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpe, labels=LabEs, name="es" )

covAs		<-mxAlgebra( expression= as %*% t(as), name="As" )
covCs		<-mxAlgebra( expression= cs %*% t(cs), name="Cs" )
covEs		<-mxAlgebra( expression= es %*% t(es), name="Es" )

covPs		<-mxAlgebra( expression= As+Cs+Es, name="Vs" )

# Define the matrices to hold the A and C and E effects: Common 
#SEC~~SCA fixed to 0
PathsAc	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,T,T,T,T,T,T,T), values=c(.6,.6,0,.6,.6,.6,.6,.6,.6,.6), labels=c("a11","a21","a31","a41","a22","a32","a42","a33","a43","a44"), name="a_c" )
PathsCc	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,T,T,T,T,T,T,T), values=c(.05,.05,0,.05,.05,.05,.05,.05,.05,.05), labels=c("c11","c21","c31","c41","c22","c32","c42","c33","c43","c44"), name="c_c" )
PathsEc	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,T,T,T,T,T,T,T), values=c(.5,.5,0,.5,.5,.5,.5,.5,.5,.5), labels=c("e11","e21","e31","e41","e22","e32","e42","e33","e43","e44"), name="e_c" )

covAc		<-mxAlgebra( expression= a_c %*% t(a_c), name="Ac" )
covCc		<-mxAlgebra( expression= c_c %*% t(c_c), name="Cc" )
covEc		<-mxAlgebra( expression= e_c %*% t(e_c), name="Ec" )
covPc		<-mxAlgebra( expression= Ac+Cc+Ec, name="Vc" )

# Generate Covariance of Latent factor model Including Causal Paths between factors
Id4		<-mxMatrix(type="Iden",	nrow=nfact, ncol=nfact, name="I4" )
covFAc	<-mxAlgebra( expression= solve(I4-PhC) %&% Ac, name ="FAc") #(I4-PhC) gives the expression for the removal of the loop effect of causal relationships between the factors (1-4).
covFCc	<-mxAlgebra( expression= solve(I4-PhC) %&% Cc, name ="FCc")
covFEc	<-mxAlgebra( expression= solve(I4-PhC) %&% Ec, name ="FEc")
covFc		<-mxAlgebra( expression= FAc+FCc+FEc, name="FVc" )

# Var-Cov of measured vars in terms of latent factors and AC, Cc, and Ec
FcovMZ	<-mxAlgebra( expression= (FactLTw  %&% rbind ( cbind(FVc, FAc+FCc), cbind(FAc+FCc, FVc) )) , name="expFCovMZ" )#This traces the path from vars to factors and back to vars
FcovDZ	<-mxAlgebra( expression= (FactLTw  %&% rbind ( cbind(FVc, .5%x%FAc+FCc), cbind(.5%x%FAc+FCc, FVc) )) , name="expFCovDZ" )

SpcovMZ	<-mxAlgebra( expression= rbind (cbind(Vs, As+Cs), cbind(As+Cs, Vs) ) , name="expSpCovMZ" )
SpcovDZ	<-mxAlgebra( expression= rbind (cbind(Vs, .5%x%As+Cs), cbind(.5%x%As+Cs, Vs) ) , name="expSpCovDZ" )

TOTcovMZ	<-mxAlgebra( expression= expFCovMZ + expSpCovMZ , name="TOTexpCovMZ" )
TOTcovDZ	<-mxAlgebra( expression= expFCovDZ + expSpCovDZ , name="TOTexpCovDZ" )

# *******************************************************************************************************
# Calculator

# Calculate genetic,  environmental, and phenotypic correlations
corA		<- mxAlgebra( expression=solve(sqrt(I4*FAc))%&%FAc, name ="rA" ) 
corC		<- mxAlgebra( expression=solve(sqrt(I4*FCc))%&%FCc, name ="rC" )
corE		<- mxAlgebra( expression=solve(sqrt(I4*FEc))%&%FEc, name ="rE" )
corP		<- mxAlgebra( expression=solve(sqrt(I4*FVc))%&%FVc, name ="rP") 

# Standardize the Total var/covariances matrices of the observed variables
Id8		<-mxMatrix(type="Iden",	nrow=ntv, ncol=ntv, name="I8" )
Rfactmz	<-mxAlgebra( expression= solve(sqrt(I8*TOTexpCovMZ)) %&% TOTexpCovMZ, name="FactcorMZ" )
Rfactdz	<-mxAlgebra( expression= solve(sqrt(I8*TOTexpCovDZ)) %&% TOTexpCovDZ, name="FactcorDZ" )

# Standardize the Common Effects
stcovAc	<-mxAlgebra( expression= FAc/FVc, name="stAc" )
stcovCc	<-mxAlgebra( expression= FCc/FVc, name="stCc" )
stcovEc	<-mxAlgebra( expression= FEc/FVc, name="stEc" )

# Standardize the Specific Effects
stcovAs	<-mxAlgebra( expression= As/( (FactL %&% FVc) +Vs), name="stAs" )
stcovCs	<-mxAlgebra( expression= Cs/( (FactL %&% FVc) +Vs), name="stCs" )
stcovEs	<-mxAlgebra( expression= Es/( (FactL %&% FVc) +Vs), name="stEs" )

# Standardized Effects of Individual variables from the factors (Variance components) above
stAvar	<-mxAlgebra( expression= ((FactL %&% FAc)/( (FactL %&% FVc) +Vs)), name="stAvariables" )
stCvar	<-mxAlgebra( expression= ((FactL %&% FCc)/( (FactL %&% FVc) +Vs)), name="stCvariables" )
stEvar	<-mxAlgebra( expression= ((FactL %&% FEc)/( (FactL %&% FVc) +Vs)), name="stEvariables" )

# Standardized Factor Loadings
StFL		<-mxAlgebra( expression= (diag2vec( FactL %&% FVc / TOTexpCovMZ[1:4,1:4])) , name="StandFact" )

# *******************************************************************************************************

# Data objects for Multiple Groups
dataMZ	<- mxData( observed=mzData, type="raw" )
dataDZ	<- mxData( observed=dzData, type="raw" )

# Objective objects for Multiple Groups
objMZ		<- mxExpectationNormal( covariance="TOTexpCovMZ", means="expMean", dimnames=selVars)
objDZ		<- mxExpectationNormal( covariance="TOTexpCovDZ", means="expMean", dimnames=selVars)

fitFunction <- mxFitFunctionML()

# Combine Groups
pars1		<-list(Means,Load,LoadTw,PhCaus,PathsAs,PathsAs,PathsCs,PathsEs,covAs,covCs,covEs,covPs,Id4,Id2,Id8, stAvar, stCvar, stEvar)
pars2		<-list(PathsAc,PathsCc,PathsEc,covAc,covCc,covEc,covPc,covFAc,covFCc,covFEc,covFc,stcovAc,stcovCc,stcovEc, stcovAs, stcovCs, stcovEs, corA, corC, corE,corP,rA3on4,rC3on4,rE3on4,stAres4,stCres4,stEres4)
modelMZ	<-mxModel(pars1, pars2, FcovMZ, SpcovMZ, TOTcovMZ, dataMZ, objMZ, Rfactmz, fitFunction, StFL, name="MZ" )
modelDZ	<-mxModel(pars1, pars2, FcovDZ, SpcovDZ, TOTcovDZ, dataDZ, objDZ, Rfactdz, fitFunction, name="DZ" )
minus2ll	<-mxAlgebra( expression=MZ.objective + DZ.objective, name="m2LL" )
obj		<-mxFitFunctionAlgebra( "m2LL" )
cistFL	<-mxCI (c ('MZ.StandFact','MZ.rA','MZ.rC','MZ.rE','MZ.rP'))
cistFc	<-mxCI (c ('MZ.stAc','MZ.stEc','MZ.stCc') ) 	# standardized var comp from Common feactors	
cistVs	<-mxCI (c ('MZ.stAs','MZ.stEs') ) 	# standardized var comp from specific Factors
cistvars	<-mxCI (c ('MZ.stAvariables','MZ.stCvariables','MZ.stEvariables'))
ACEModel	<-mxModel("ace", pars1, pars2, modelMZ, modelDZ, minus2ll, obj, cistFc, cistFL, cistVs,cistvars) 

# --------------------------------------------------------------------------------------------------------------------------------
# RUN ACE Factor Model: Cholesky (by Zygosity)

ACEFit		<-mxRun(ACEModel, intervals=T)
#ACEFit		<-mxTryHard(ACEModel, intervals=F, bestInitsOutput=TRUE, showInits=TRUE)
(ACESumm		<-summary(ACEFit, verbose=F))

# Get some output

mxEval(MZ.Vs, ACEFit)

mxEval(MZ.FAc, ACEFit)
mxEval(MZ.FCc, ACEFit)
mxEval(MZ.FEc, ACEFit)
mxEval(MZ.FVc, ACEFit)
mxEval(MZ.PhC, ACEFit)

mxEval(MZ.stAc, ACEFit)
mxEval(MZ.stCc, ACEFit)
mxEval(MZ.stEc, ACEFit)

mxEval(MZ.stAs, ACEFit)
mxEval(MZ.stCs, ACEFit)
mxEval(MZ.stEs, ACEFit)

mxEval(MZ.StandFact, ACEFit)

mxEval(MZ.stAvariables, ACEFit)
mxEval(MZ.stCvariables, ACEFit)
mxEval(MZ.stEvariables, ACEFit)

mxEval(MZ.FactcorMZ, ACEFit)
mxEval(DZ.FactcorDZ, ACEFit)


#****************************************************************************************************************************
# _______________________________________________________________________________________________________________________
# Genetic Factor MODEL with causal paths by zygosity
# Causal paths specified from three predictors to AUDIT outcome
#_____________________________________________________________________________________________________________________________

nv		<- 4				# number of variables for a twin = 1 in Univariate
ntv		<- 2*nv			# number of variables for a pair = 2* 1 for Univariate
nfact		<- 4				# number of Latent Factors for Mediation Model per twin
nfact2	<- 2*nfact			# number of Latent Factors for Mediation Model per twin pair
nlower	<- ntv*(ntv+1)/2 		# number of free elements in a lower matrix ntv*ntv

# CREATE LABELS & START VALUES as objects(to ease specification in the body of the model)
(mLabs	<- paste("m",1:nv,sep=""))
(Stmean	<-colMeans(mzData[,1:nv],na.rm=TRUE))
(Stsd 	<-sapply(mzData[,1:nv],sd, na.rm=TRUE))
(PatM		<- c(TRUE,TRUE,TRUE,TRUE))

# Create Labels for Diagonal Matrices-fixed to 0
(LabEs	<- c('es1','es2','es3','es4'))
(LabAs	<- c('as1','as2','as3','as4'))
(LabCs	<- c('cs1','cs2','cs3','cs4'))

PatSp		<- c(F,F,F,F)
StSpa		<- c(0,0,0,0)
StSpc		<- c(0,0,0,0)
StSpe		<- c(0,0,0,0)

# all factor loadings fixed to 1
PatFl		<- c(F,F,F,F,			
            F,F,F,F,
            F,F,F,F,
            F,F,F,F)

StFl		<- c(1,0,0,0,
           0,1,0,0,
           0,0,1,0,
           0,0,0,1)

LabFl		<- c('l1',NA,NA,NA,
            NA,'l2',NA,NA,
            NA,NA,'l3',NA,
            NA,NA,NA,'l4')

PatPhC	<- c(F,F,F,TRUE,
            F,F,F,TRUE,
            F,F,F,TRUE,
            F,F,F,F)

StPhC		<- c(0,0,0,.1,
            0,0,0,.1,
            0,0,0,.1,
            0,0,0,0)

LabPhC	<- c(NA,NA,NA,'c1on4',
            NA,NA,NA,'c2on4',	
            NA,NA,NA,'c3on4',	
            NA,NA,NA,NA)	 

#______________________________________________________________________________________________________
# Define matrices to hold the Means, SD, correlations
# Use Algebra to generate expected var/cov matrices and Means
# Specify: data objects, Fitfunction, the Model, 
# Run the Model 
#______________________________________________________________________________________________________

Means		<-mxMatrix("Full", 1, ntv, free=c(PatM,PatM), values=c(Stmean,Stmean), labels=c(mLabs,mLabs), name="expMean") 

# Define matrices to specify the loadings of the dependent variables on the latent factors
Load		<-mxMatrix(type="Full",	nrow=nv, ncol=nfact, free=PatFl, values=StFl, labels=LabFl, name="FactL" )
Id2		<-mxMatrix(type="Iden",	nrow=2, ncol=2, free=F, name="I2" )
LoadTw	<-mxAlgebra(I2%x%FactL, name="FactLTw")

# Define the matrix to hold the Single headed Arrows (causal paths) from the 3 predictors to AUDIT  
PhCaus	<-mxMatrix(type="Full",	nrow=nfact, ncol=nfact, free=PatPhC, values=StPhC, labels=LabPhC, name="PhC" )

# Define the matrix to hold the A and C effects: Specific-fixed to 0 
PathsAs	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpa, labels=LabAs, name="as" )
PathsCs	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpc, labels=LabCs, name="cs" )
PathsEs	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpe, labels=LabEs, name="es" )
covAs		<-mxAlgebra( expression= as %*% t(as), name="As" )
covCs		<-mxAlgebra( expression= cs %*% t(cs), name="Cs" )
covEs		<-mxAlgebra( expression= es %*% t(es), name="Es" )
covPs		<-mxAlgebra( expression= As+Cs+Es, name="Vs" )

# Define the matrices to hold the A and C effects: Common 
#SEC~~SCA fixed to 0
FactSD	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,F,T,T,F,T,F,T), values=c(1,.1,0,0,1,.1,0,1,0,1), labels=c('F1sd','cov21','cov31','cov41','F2res','cov32','cov42','F3res','cov43','F4res'),name="Fsd" )
PathsAcsp	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,F,T,T,F,T,F,T), values=c(.8,.1,0,0,.8,.1,0,.8,0,.8), labels=c("a_csp1","a_csp21","a_csp31","a_csp41","a_csp2","a_csp32","a_csp42","a_csp3","a_csp43","a_csp4"), name="ac" )
PathsCcsp	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,F,T,T,F,T,F,T), values=c(.1,.1,0,0,.1,.1,0,.1,0,.1), labels=c("c_csp1","c_csp21","c_csp31","c_csp41","c_csp2","c_csp32","c_csp42","c_csp3","c_csp43","c_csp4"), name="cc" )
PathsEcsp	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,F,T,T,F,T,F,T), values=c(.8,.1,0,0,.8,.1,0,.8,0,.8), labels=c("e_csp1","e_csp21","e_csp31","e_csp41","e_csp2","e_csp32","e_csp42","e_csp3","e_csp43","e_csp4"), name="ec" )
covAc		<-mxAlgebra( expression= ac %*% t(ac), name="Ac" )
covCc		<-mxAlgebra( expression= cc %*% t(cc), name="Cc" )
covEc		<-mxAlgebra( expression= ec %*% t(ec), name="Ec" )
covPc		<-mxAlgebra( expression= Ac+Cc+Ec, name="Vc" )

# Generate Covariance of Latent factor model Including Causal Paths between factors
Id4		<-mxMatrix(type="Iden",	nrow=nfact, ncol=nfact, name="I4" )
covFAc	<-mxAlgebra( expression= solve(I4-PhC) %&% Ac, name ="FAc")
covFCc	<-mxAlgebra( expression= solve(I4-PhC) %&% Cc, name ="FCc")
covFEc	<-mxAlgebra( expression= solve(I4-PhC) %&% Ec, name ="FEc")
covFc		<-mxAlgebra( expression= (FAc+FCc+FEc), name="FVc" )

FcovMZ	<-mxAlgebra( expression= (FactLTw  %&% rbind ( cbind(FVc, FAc+FCc), cbind(FAc+FCc, FVc) )) , name="expFCovMZ" )
FcovDZ	<-mxAlgebra( expression= (FactLTw  %&% rbind ( cbind(FVc, .5%x%FAc+FCc), cbind(.5%x%FAc+FCc, FVc) )) , name="expFCovDZ" )

SpcovMZ	<-mxAlgebra( expression= rbind (cbind(Vs, As+Cs), cbind(As+Cs, Vs) ) , name="expSpCovMZ" )
SpcovDZ	<-mxAlgebra( expression= rbind (cbind(Vs, .5%x%As+Cs), cbind(.5%x%As+Cs, Vs) ) , name="expSpCovDZ" )

TOTcovMZ	<-mxAlgebra( expression= expFCovMZ + expSpCovMZ , name="TOTexpCovMZ" )
TOTcovDZ	<-mxAlgebra( expression= expFCovDZ + expSpCovDZ , name="TOTexpCovDZ" )
# *******************************************************************************************************
# Calculator

# Calculate genetic,  environmental, and phenotypic correlations
corA		<- mxAlgebra( expression=solve(sqrt(I4*FAc))%&%FAc, name ="rA" ) 
corC		<- mxAlgebra( expression=solve(sqrt(I4*FCc))%&%FCc, name ="rC" )
corE		<- mxAlgebra( expression=solve(sqrt(I4*FEc))%&%FEc, name ="rE" )
corP		<- mxAlgebra( expression=solve(sqrt(I4*FVc))%&%FVc, name ="rP") 

# Standardize the causal effects
Stcp1on4	<-mxAlgebra( expression= (PhC[4,1]* sqrt(FVc[1,1]))/sqrt(FVc[4,4]) , name="Stand_1on4" )
Stcp2on4	<-mxAlgebra( expression= (PhC[4,2]* sqrt(FVc[2,2]))/sqrt(FVc[4,4]) , name="Stand_2on4" )
Stcp3on4	<-mxAlgebra( expression= (PhC[4,3]* sqrt(FVc[3,3]))/sqrt(FVc[4,4]) , name="Stand_3on4" )

# Standardize the Total var/covariances matrices of the observed variables
Id8		<-mxMatrix(type="Iden",	nrow=ntv, ncol=ntv, name="I8" )
Rfactmz	<-mxAlgebra( expression= solve(sqrt(I8*TOTexpCovMZ)) %&% TOTexpCovMZ, name="FactcorMZ" )
Rfactdz	<-mxAlgebra( expression= solve(sqrt(I8*TOTexpCovDZ)) %&% TOTexpCovDZ, name="FactcorDZ" )

# Standardize the Common Effects
stcovAc	<-mxAlgebra( expression= FAc/FVc, name="stAc" )
stcovCc	<-mxAlgebra( expression= FCc/FVc, name="stCc" )
stcovEc	<-mxAlgebra( expression= FEc/FVc, name="stEc" )

# Standardize the Specific Effects
stcovAs	<-mxAlgebra( expression= sqrt(As/( (FactL %&% FVc) +Vs)), name="stAs" )
stcovCs	<-mxAlgebra( expression= sqrt(Cs/( (FactL %&% FVc) +Vs)), name="stCs" )
stcovEs	<-mxAlgebra( expression= sqrt(Es/( (FactL %&% FVc) +Vs)), name="stEs" )

# Standardized Effects of Individual variables from the factors (Variance components) above
stAvar	<-mxAlgebra( expression= (FactL %&% FAc)/( (FactL %&% FVc) +Vs), name="stAvariables" )
stCvar	<-mxAlgebra( expression= (FactL %&% FCc)/( (FactL %&% FVc) +Vs), name="stCvariables" )
stEvar	<-mxAlgebra( expression= (FactL %&% FEc)/( (FactL %&% FVc) +Vs), name="stEvariables" )

# Standardized Factor Loadings
StFL		<-mxAlgebra( expression= sqrt(diag2vec( FactL %&% FVc / TOTexpCovMZ[1:4,1:4])) , name="StandFact" )

# ACE Standardized Path Coefficients (pre-multiplied by inverse of standard deviations) 
invSD <- mxAlgebra( expression=solve(sqrt(I4*FVc)), name="iSD")
stPathAc <- mxAlgebra( expression= iSD %*% ac, name="stPathAc" )
stPathCc <- mxAlgebra( expression= iSD %*% cc, name="stPathCc" )
stPathEc <- mxAlgebra( expression= iSD %*% ec, name="stPathEc" )

# *******************************************************************************************************

# Data objects for Multiple Groups
dataMZ	<- mxData( observed=mzData, type="raw" )
dataDZ	<- mxData( observed=dzData, type="raw" )

# Objective objects for Multiple Groups
objMZ		<- mxExpectationNormal( covariance="TOTexpCovMZ", means="expMean", dimnames=selVars)
objDZ		<- mxExpectationNormal( covariance="TOTexpCovDZ", means="expMean", dimnames=selVars)

fitFunction <- mxFitFunctionML()

# Combine Groups
pars1		<-list(Means,Load,LoadTw,PhCaus,PathsAs,PathsCs,PathsEs,covAs,covCs,covEs,covPs,Id4,Id2,Id8, stAvar, stCvar, stEvar)
pars2		<-list(PathsAcsp,PathsCcsp,PathsEcsp,covAc,covCc,covEc,covPc,covFAc,covFCc,covFEc,covFc,stcovAc,stcovCc,stcovEc, stcovAs, stcovCs, stcovEs,corA,corC,corE,corP,invSD,stPathAc,stPathCc,stPathEc,rA3on4,rC3on4,rE3on4,stAres4,stCres4,stEres4)
parsmed	<-list(Stcp1on4, Stcp2on4, Stcp3on4)
modelMZ	<-mxModel(pars1, pars2, parsmed, FcovMZ, SpcovMZ, TOTcovMZ, dataMZ, objMZ, Rfactmz, fitFunction, StFL, name="MZ" )
modelDZ	<-mxModel(pars1, pars2, FcovDZ, SpcovDZ, TOTcovDZ, dataDZ, objDZ, Rfactdz, fitFunction, name="DZ" )
minus2ll	<-mxAlgebra( expression=MZ.objective + DZ.objective, name="m2LL" )
obj		<-mxFitFunctionAlgebra( "m2LL" )
cistFL	<-mxCI (c ('MZ.StandFact','MZ.Stand_1on4','MZ.Stand_2on4','MZ.Stand_3on4','MZ.rA','MZ.rC','MZ.rE','MZ.rP'))
cistFc	<-mxCI (c ('stAc','stCc','stEc') ) 	# standardized var comp from Common feactors	
#cistVs	<-mxCI (c ('MZ.stAs','MZ.stCs','MZ.stEs') ) 	# standardized var comp from specific Factors
cistvars	<-mxCI (c ('stAvariables','stCvariables','stEvariables'))
cistaces <-mxCI(c('stPathAc','stPathCc','stPathEc'))
ACEMsModel	<-mxModel("aceMs", pars1, pars2, modelMZ, modelDZ, minus2ll, obj, cistFc, cistFL, cistVs, cistvars,cistaces) 

# --------------------------------------------------------------------------------------------------------------------------------
# RUN ACEMs Factor Model with phenotypic causal paths by Zygosity

ACEMsFit	<-mxRun(ACEMsModel, intervals=T)#intervals=T for CIs
(ACEMsSumm	<-summary(ACEMsFit, verbose=F))

# Get some output

mxEval(MZ.Vs, ACEMsFit)

mxEval(MZ.FAc, ACEMsFit)
mxEval(MZ.FCc, ACEMsFit)
mxEval(MZ.FEc, ACEMsFit)
mxEval(MZ.FVc, ACEMsFit)
mxEval(MZ.PhC, ACEMsFit)

mxEval(MZ.stAc, ACEMsFit)
mxEval(MZ.stCc, ACEMsFit)
mxEval(MZ.stEc, ACEMsFit)

mxEval(MZ.Ac, ACEMsFit)
mxEval(MZ.Cc, ACEMsFit)
mxEval(MZ.Ec, ACEMsFit)

mxEval(MZ.stAs, ACEMsFit)
mxEval(MZ.stCs, ACEMsFit)
mxEval(MZ.stEs, ACEMsFit)

mxEval(MZ.stAvariables, ACEMsFit)
mxEval(MZ.stCvariables, ACEMsFit)
mxEval(MZ.stEvariables, ACEMsFit)

mxEval(MZ.Stand_1on4, ACEMsFit)
mxEval(MZ.Stand_2on4, ACEMsFit)
mxEval(MZ.Stand_3on4, ACEMsFit)

# _______________________________________________________________________________________________________________________
# Genetic Factor MODEL with causal paths by zygosity
# Causal paths specified from predictors to AUDIT, except the path from SCA to AUDIT was changed to correlated path
#_____________________________________________________________________________________________________________________________

nv		<- 4				# number of variables for a twin = 1 in Univariate
ntv		<- 2*nv			# number of variables for a pair = 2* 1 for Univariate
nfact		<- 4				# number of Latent Factors for Causal Model per twin
nfact2	<- 2*nfact			# number of Latent Factors for Causal Model per twin pair
nlower	<- ntv*(ntv+1)/2 		# number of free elements in a lower matrix ntv*ntv

# CREATE LABELS & START VALUES as objects(to ease specification in the body of the model)
(mLabs	<- paste("m",1:nv,sep=""))
(Stmean	<-colMeans(mzData[,1:nv],na.rm=TRUE))
(Stsd 	<-sapply(mzData[,1:nv],sd, na.rm=TRUE))
(PatM		<- c(TRUE,TRUE,TRUE,TRUE))

# Create Labels for Diagonal Matrices-fixed to 0
(LabEs	<- c('es1','es2','es3','es4'))
(LabAs	<- c('as1','as2','as3','as4'))
(LabCs	<- c('cs1','cs2','cs3','cs4'))

PatSp		<- c(F,F,F,F)
StSpa		<- c(0,0,0,0)
StSpc		<- c(0,0,0,0)
StSpe		<- c(0,0,0,0)

# all factor loadings fixed to 1
PatFl		<- c(F,F,F,F,			
            F,F,F,F,
            F,F,F,F,
            F,F,F,F)

StFl		<- c(1,0,0,0,
           0,1,0,0,
           0,0,1,0,
           0,0,0,1)

LabFl		<- c('l1',NA,NA,NA,
            NA,'l2',NA,NA,
            NA,NA,'l3',NA,
            NA,NA,NA,'l4')

PatPhC	<- c(F,F,F,TRUE,
            F,F,F,TRUE,
            F,F,F,F,
            F,F,F,F)

StPhC		<- c(0,0,0,.1,
            0,0,0,.1,
            0,0,0,0,
            0,0,0,0)

LabPhC	<- c(NA,NA,NA,'c1on4',
            NA,NA,NA,'c2on4',	
            NA,NA,NA,'c3on4',	
            NA,NA,NA,NA)	 

#______________________________________________________________________________________________________
# Define matrices to hold the Means, SD, correlations
# Use Algebra to generate expected var/cov matrices and Means
# Specify: data objects, Fitfunction, the Model, 
# Run the Model 
#______________________________________________________________________________________________________

Means		<-mxMatrix("Full", 1, ntv, free=c(PatM,PatM), values=c(Stmean,Stmean), labels=c(mLabs,mLabs), name="expMean") 

# Define matrices to specify the loadings of the dependent variables on the latent factors
Load		<-mxMatrix(type="Full",	nrow=nv, ncol=nfact, free=PatFl, values=StFl, labels=LabFl, name="FactL" )
Id2		<-mxMatrix(type="Iden",	nrow=2, ncol=2, free=F, name="I2" )
LoadTw	<-mxAlgebra(I2%x%FactL, name="FactLTw")

# Define the matrix to hold the Single headed Arrows (causal paths) from predictors to AUDIT  
PhCaus	<-mxMatrix(type="Full",	nrow=nfact, ncol=nfact, free=PatPhC, values=StPhC, labels=LabPhC, name="PhC" )

# Define the matrix to hold the A and C effects: Specific-fixed to 0 
PathsAs	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpa, labels=LabAs, name="as" )
PathsCs	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpc, labels=LabCs, name="cs" )
PathsEs	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpe, labels=LabEs, name="es" )
covAs		<-mxAlgebra( expression= as %*% t(as), name="As" )
covCs		<-mxAlgebra( expression= cs %*% t(cs), name="Cs" )
covEs		<-mxAlgebra( expression= es %*% t(es), name="Es" )
covPs		<-mxAlgebra( expression= As+Cs+Es, name="Vs" )

# Define the matrices to hold the A and C effects: Common 
#SEC~~SCA fixed to 0
FactSD	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,F,T,T,F,T,T,T), values=c(1,.1,0,0,1,.1,0,1,.1,1), labels=c('F1sd','cov21','cov31','cov41','F2res','cov32','cov42','F3res','cov43','F4res'),name="Fsd" )
PathsAcsp	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,F,T,T,F,T,T,T), values=c(.8,.1,0,0,.8,.1,0,.8,.1,.8), labels=c("a_csp1","a_csp21","a_csp31","a_csp41","a_csp2","a_csp32","a_csp42","a_csp3","a_csp43","a_csp4"), name="ac" )
PathsCcsp	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,F,T,T,F,T,T,T), values=c(.1,.1,0,0,.1,.1,0,.1,.1,.1), labels=c("c_csp1","c_csp21","c_csp31","c_csp41","c_csp2","c_csp32","c_csp42","c_csp3","c_csp43","c_csp4"), name="cc" )
PathsEcsp	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,F,T,T,F,T,T,T), values=c(.8,.1,0,0,.8,.1,0,.8,.1,.8), labels=c("e_csp1","e_csp21","e_csp31","e_csp41","e_csp2","e_csp32","e_csp42","e_csp3","e_csp43","e_csp4"), name="ec" )
covAc		<-mxAlgebra( expression= ac %*% t(ac), name="Ac" )
covCc		<-mxAlgebra( expression= cc %*% t(cc), name="Cc" )
covEc		<-mxAlgebra( expression= ec %*% t(ec), name="Ec" )
covPc		<-mxAlgebra( expression= Ac+Cc+Ec, name="Vc" )

# Generate Covariance of Latent factor model Including Causal Paths between factors
Id4		<-mxMatrix(type="Iden",	nrow=nfact, ncol=nfact, name="I4" )
covFAc	<-mxAlgebra( expression= solve(I4-PhC) %&% Ac, name ="FAc")
covFCc	<-mxAlgebra( expression= solve(I4-PhC) %&% Cc, name ="FCc")
covFEc	<-mxAlgebra( expression= solve(I4-PhC) %&% Ec, name ="FEc")
covFc		<-mxAlgebra( expression= (FAc+FCc+FEc), name="FVc" )

FcovMZ	<-mxAlgebra( expression= (FactLTw  %&% rbind ( cbind(FVc, FAc+FCc), cbind(FAc+FCc, FVc) )) , name="expFCovMZ" )
FcovDZ	<-mxAlgebra( expression= (FactLTw  %&% rbind ( cbind(FVc, .5%x%FAc+FCc), cbind(.5%x%FAc+FCc, FVc) )) , name="expFCovDZ" )

SpcovMZ	<-mxAlgebra( expression= rbind (cbind(Vs, As+Cs), cbind(As+Cs, Vs) ) , name="expSpCovMZ" )
SpcovDZ	<-mxAlgebra( expression= rbind (cbind(Vs, .5%x%As+Cs), cbind(.5%x%As+Cs, Vs) ) , name="expSpCovDZ" )

TOTcovMZ	<-mxAlgebra( expression= expFCovMZ + expSpCovMZ , name="TOTexpCovMZ" )
TOTcovDZ	<-mxAlgebra( expression= expFCovDZ + expSpCovDZ , name="TOTexpCovDZ" )

# *******************************************************************************************************
# Calculator

# Calculate genetic,  environmental, and phenotypic correlations
corA		<- mxAlgebra( expression=solve(sqrt(I4*FAc))%&%FAc, name ="rA" ) 
corC		<- mxAlgebra( expression=solve(sqrt(I4*FCc))%&%FCc, name ="rC" )
corE		<- mxAlgebra( expression=solve(sqrt(I4*FEc))%&%FEc, name ="rE" )
corP		<- mxAlgebra( expression=solve(sqrt(I4*FVc))%&%FVc, name ="rP") 

# Standardize the causal effects
Stcp1on4	<-mxAlgebra( expression= (PhC[4,1]* sqrt(FVc[1,1]))/sqrt(FVc[4,4]) , name="Stand_1on4" )
Stcp2on4	<-mxAlgebra( expression= (PhC[4,2]* sqrt(FVc[2,2]))/sqrt(FVc[4,4]) , name="Stand_2on4" )
Stcp3on4	<-mxAlgebra( expression= (PhC[4,3]* sqrt(FVc[3,3]))/sqrt(FVc[4,4]) , name="Stand_3on4" )

#standardize A, C, E correlations between SCA and residual of AUDIT
rA3on4	<-mxAlgebra( expression= (Ac[4,3]/(sqrt(FAc[3,3])* sqrt(Ac[4,4]))) , name="rA_3on4" )
rC3on4	<-mxAlgebra( expression= (Cc[4,3]/(sqrt(FCc[3,3])* sqrt(Cc[4,4]))) , name="rC_3on4" )
rE3on4	<-mxAlgebra( expression= (Ec[4,3]/(sqrt(FEc[3,3])* sqrt(Ec[4,4]))) , name="rE_3on4" )

#standardize residual variances of AUDIT
stAres4	<-mxAlgebra( expression= (Ac[4,4]/FVc[4,4]) , name="stAres_4" )
stCres4	<-mxAlgebra( expression= (Cc[4,4]/FVc[4,4]) , name="stCres_4" )
stEres4	<-mxAlgebra( expression= (Ec[4,4]/FVc[4,4]) , name="stEres_4" )

# Standardize the Total var/covariances matrices of the observed variables
Id8		<-mxMatrix(type="Iden",	nrow=ntv, ncol=ntv, name="I8" )
Rfactmz	<-mxAlgebra( expression= solve(sqrt(I8*TOTexpCovMZ)) %&% TOTexpCovMZ, name="FactcorMZ" )
Rfactdz	<-mxAlgebra( expression= solve(sqrt(I8*TOTexpCovDZ)) %&% TOTexpCovDZ, name="FactcorDZ" )

# Standardize the Common Effects
stcovAc	<-mxAlgebra( expression= FAc/FVc, name="stAc" )
stcovCc	<-mxAlgebra( expression= FCc/FVc, name="stCc" )
stcovEc	<-mxAlgebra( expression= FEc/FVc, name="stEc" )

# Standardize the Specific Effects
stcovAs	<-mxAlgebra( expression= sqrt(As/( (FactL %&% FVc) +Vs)), name="stAs" )
stcovCs	<-mxAlgebra( expression= sqrt(Cs/( (FactL %&% FVc) +Vs)), name="stCs" )
stcovEs	<-mxAlgebra( expression= sqrt(Es/( (FactL %&% FVc) +Vs)), name="stEs" )

# Standardized Effects of Individual variables from the factors (Variance components) above
stAvar	<-mxAlgebra( expression= (FactL %&% FAc)/( (FactL %&% FVc) +Vs), name="stAvariables" )
stCvar	<-mxAlgebra( expression= (FactL %&% FCc)/( (FactL %&% FVc) +Vs), name="stCvariables" )
stEvar	<-mxAlgebra( expression= (FactL %&% FEc)/( (FactL %&% FVc) +Vs), name="stEvariables" )

# Standardized Factor Loadings
StFL		<-mxAlgebra( expression= sqrt(diag2vec( FactL %&% FVc / TOTexpCovMZ[1:4,1:4])) , name="StandFact" )

# *******************************************************************************************************

# Data objects for Multiple Groups
dataMZ	<- mxData( observed=mzData, type="raw" )
dataDZ	<- mxData( observed=dzData, type="raw" )

# Objective objects for Multiple Groups
objMZ		<- mxExpectationNormal( covariance="TOTexpCovMZ", means="expMean", dimnames=selVars)
objDZ		<- mxExpectationNormal( covariance="TOTexpCovDZ", means="expMean", dimnames=selVars)

fitFunction <- mxFitFunctionML()

# Combine Groups
pars1		<-list(Means,Load,LoadTw,PhCaus,PathsAs,PathsCs,PathsEs,covAs,covCs,covEs,covPs,Id4,Id2,Id8, stAvar, stCvar, stEvar)
pars2		<-list(PathsAcsp,PathsCcsp,PathsEcsp,covAc,covCc,covEc,covPc,covFAc,covFCc,covFEc,covFc,stcovAc,stcovCc,stcovEc, stcovAs, stcovCs, stcovEs, corA, corC,corE,corP,rA3on4,rC3on4,rE3on4,stAres4,stCres4,stEres4)
parsmed	<-list(Stcp1on4, Stcp2on4, Stcp3on4)
modelMZ	<-mxModel(pars1, pars2, parsmed, FcovMZ, SpcovMZ, TOTcovMZ, dataMZ, objMZ, Rfactmz, fitFunction, StFL, name="MZ" )
modelDZ	<-mxModel(pars1, pars2, FcovDZ, SpcovDZ, TOTcovDZ, dataDZ, objDZ, Rfactdz, fitFunction, name="DZ" )
minus2ll	<-mxAlgebra( expression=MZ.objective + DZ.objective, name="m2LL" )
obj		<-mxFitFunctionAlgebra( "m2LL" )
cistFL	<-mxCI (c ('MZ.StandFact','MZ.Stand_1on4','MZ.Stand_2on4','MZ.Stand_3on4','MZ.rA','MZ.rC','MZ.rE','MZ.rP'))
cistFc	<-mxCI (c ('MZ.stAc','MZ.stCc','MZ.stEc') ) 	# standardized var comp from Common feactors	
cistVs	<-mxCI (c ('MZ.stAs','MZ.stCs','MZ.stEs') ) 	# standardized var comp from specific Factors
cistvars	<-mxCI (c ('MZ.stAvariables','MZ.stCvariables','MZ.stEvariables','rA_3on4','rC_3on4','rE_3on4','stAres_4','stCres_4','stEres_4'))
ACEMsModel2	<-mxModel("aceMs", pars1, pars2, modelMZ, modelDZ, minus2ll, obj, cistFc, cistFL, cistVs, cistvars) 

# --------------------------------------------------------------------------------------------------------------------------------
# RUN ACEMs Factor Model with phenotypic causal mediation paths by Zygosity

ACEMsFit2	<-mxRun(ACEMsModel2, intervals=T)#intervals=T for CIs
(ACEMsSumm2	<-summary(ACEMsFit2, verbose=F))

# Get some output

mxEval(MZ.Vs, ACEMsFit2)

mxEval(MZ.FAc, ACEMsFit2)
mxEval(MZ.FCc, ACEMsFit2)
mxEval(MZ.FEc, ACEMsFit2)
mxEval(MZ.FVc, ACEMsFit2)
mxEval(MZ.PhC, ACEMsFit2)

mxEval(MZ.stAc, ACEMsFit2)
mxEval(MZ.stCc, ACEMsFit2)
mxEval(MZ.stEc, ACEMsFit2)

mxEval(MZ.Ac, ACEMsFit2)
mxEval(MZ.Cc, ACEMsFit2)
mxEval(MZ.Ec, ACEMsFit2)

mxEval(MZ.stAs, ACEMsFit2)
mxEval(MZ.stCs, ACEMsFit2)
mxEval(MZ.stEs, ACEMsFit2)

mxEval(MZ.stAvariables, ACEMsFit2)
mxEval(MZ.stCvariables, ACEMsFit2)
mxEval(MZ.stEvariables, ACEMsFit2)

mxEval(MZ.Stand_1on4, ACEMsFit2)
mxEval(MZ.Stand_2on4, ACEMsFit2)
mxEval(MZ.Stand_3on4, ACEMsFit2)

# fix C and E covariance between SCA and AUDIT to 0 because non-sig

ACEMsModel3  <- mxModel( ACEMsFit2, name="aceMs" )
ACEMsModel3  <- omxSetParameters( ACEMsModel3, labels=c("c_csp43","e_csp43"), free=FALSE, values=0 )
ACEMsFit3	<-mxRun(ACEMsModel3, intervals=T)#intervals=T for CIs
(ACEMsSumm3	<-summary(ACEMsFit3, verbose=F))

#------------------------------------------------------------
# Compare models
#------------------------------------------------------------
# **************************************************
(mxCompare(ACEFit, list(ACEMsFit,ACEMsFit2,ACEMsFit3)))

# **************************************************
#sex differences model based on ACEMsModel2
#run the code for sex differences at the beginning
nv		<- 4				# number of variables for a twin = 1 in Univariate
ntv		<- 2*nv			#number of variables for a pair = 2* 1 for Univariate
nlower	<- nv*(nv+1)/2 		#number of free elements in a lower matrix nv*nv
ncor		<- (nv*(nv+1)/2)-nv	#number of free elements in a correlation matrix nv*nv
nfact		<- 4				# number of Latent Factors for Causal Model per twin
nfact2	<- 2*nfact			# number of Latent Factors for Causal Model per twin pair
nfcor		<- (nfact*(nfact+1)/2)-nfact	# number of free elements in a correlation matrix nfact*nfcat
Groups	<- c("mz", "dz")
Vars		<- c("SEC","ICA","SCA","AUDIT")
selVars	<- c("SEC1","ICA1","SCA1","AUDIT1",
             "SEC2","ICA2","SCA2","AUDIT2")

mzmData	<- subset(TWINdata2, zyg1%in%c(1)|zyg2%in%c(1) , selVars)
mzfData	<- subset(TWINdata2, zyg1%in%c(3)|zyg2%in%c(3) , selVars)
dzmData	<- subset(TWINdata2, zyg1%in%c(2)|zyg2%in%c(2) , selVars)
dzfData	<- subset(TWINdata2, zyg1%in%c(4)|zyg2%in%c(4) , selVars)
dzoData	<- subset(TWINdata2, zyg1%in%c(5)|zyg2%in%c(5) , selVars) #Crosscheck that males have been selected to be twin 1 

psych::describe(mzmData)
psych::describe(mzfData)
psych::describe(dzmData)
psych::describe(dzfData)
psych::describe(dzoData)

# CREATE LABELS & START VALUES as objects(to ease specification in the body of the model)
(mLabsm	<- paste("mm",1:nv,sep=""))
(mLabsf	<- paste("mf",1:nv,sep=""))
(Stmeanm	<-colMeans(mzmData[,1:nv],na.rm=TRUE))
(Stmeanf	<-colMeans(mzfData[,1:nv],na.rm=TRUE))
#(Stsdm 	<-sapply(mzmData[,1:nv],sd, na.rm=TRUE))
#(Stsdf 	<-sapply(mzfData[,1:nv],sd, na.rm=TRUE))
(PatM		<- c(TRUE,TRUE,TRUE,TRUE))

# Create Labels for Diagonal Matrices-fixed to 0
(LabEsm	<- c('es1m','es2m','es3m','es4m'))
(LabAsm	<- c('as1m','as2m','as3m','as4m'))
(LabCsm	<- c('cs1m','cs2m','cs3m','cs4m'))

(LabEsf	<- c('es1f','es2f','es3f','es4f'))
(LabAsf	<- c('as1f','as2f','as3f','as4f'))
(LabCsf	<- c('cs1f','cs2f','cs3f','cs4f'))

PatSp		<- c(F,F,F,F)
StSpam	<- c(0,0,0,0)
StSpcm	<- c(0,0,0,0)
StSpem	<- c(0,0,0,0)

StSpaf	<- c(0,0,0,0)
StSpcf	<- c(0,0,0,0)
StSpef	<- c(0,0,0,0)

# all factor loadings fixed to 1
PatFl		<- c(F,F,F,F,			
            F,F,F,F,
            F,F,F,F,
            F,F,F,F)

StFlm		<- c(1,0,0,0,
            0,1,0,0,
            0,0,1,0,
            0,0,0,1)

StFlf		<- c(1,0,0,0,
            0,1,0,0,
            0,0,1,0,
            0,0,0,1)

LabFlm	<- c('l1m',NA,NA,NA,
            NA,'l2m',NA,NA,
            NA,NA,'l3m',NA,
            NA,NA,NA,'l4m')

LabFlf	<- c('l1f',NA,NA,NA,
            NA,'l2f',NA,NA,
            NA,NA,'l3f',NA,
            NA,NA,NA,'l4f')

PatPhC	<- c(F,F,F,TRUE,
            F,F,F,TRUE,
            F,F,F,F,
            F,F,F,F)

StPhCm	<- c(0,0,0,.1,
            0,0,0,.1,
            0,0,0,0,
            0,0,0,0)

StPhCf	<- c(0,0,0,.1,
            0,0,0,.1,
            0,0,0,0,
            0,0,0,0)

LabPhCm	<- c(NA,NA,NA,'c1on4m',
             NA,NA,NA,'c2on4m',	
             NA,NA,NA,'c3on4m',	
             NA,NA,NA,NA)	 

LabPhCf	<- c(NA,NA,NA,'c1on4f',
             NA,NA,NA,'c2on4f',	
             NA,NA,NA,'c3on4f',	
             NA,NA,NA,NA)	 

#______________________________________________________________________________________________________
# Define matrices to hold the Means, SD, correlations
# Use Algebra to generate expected var/cov matrices and Means
# Specify: data objects, Fitfunction, the Model, 
# Run the Model 
#______________________________________________________________________________________________________

Meansm	<-mxMatrix("Full", 1, ntv, free=c(PatM,PatM), values=c(Stmeanm,Stmeanm), labels=c(mLabsm,mLabsm), name="expMeanm") 
Meansf	<-mxMatrix("Full", 1, ntv, free=c(PatM,PatM), values=c(Stmeanf,Stmeanf), labels=c(mLabsf,mLabsf), name="expMeanf") 
Meanso	<-mxMatrix("Full", 1, ntv, free=c(PatM,PatM), values=c(Stmeanm,Stmeanf), labels=c(mLabsm,mLabsf), name="expMeano") 

# Define matrices to specify the loadings of the dependent variables on the latent factors
Loadm		<-mxMatrix(type="Full",	nrow=nv, ncol=nfact, free=PatFl, values=StFlm, labels=LabFlm, name="FactLm" )
Loadf		<-mxMatrix(type="Full",	nrow=nv, ncol=nfact, free=PatFl, values=StFlf, labels=LabFlf, name="FactLf" )
Id2		<-mxMatrix(type="Iden",	nrow=2, ncol=2, free=F, name="I2" )
Ze2		<-mxMatrix(type="Zero",	nrow=nv, ncol=nfact, free=F, name="Z2" )

LoadTwm	<-mxAlgebra(I2%x%FactLm, name="FactLTwm")
LoadTwf	<-mxAlgebra(I2%x%FactLf, name="FactLTwf")
LoadTwo	<-mxAlgebra(expression= rbind (cbind(FactLm, Z2), cbind(Z2, FactLf) ), name="FactLTwo")

# Define the matrix to hold the Single headed Arrows (causal paths) from predictors to AUDIT  
PhCausm	<-mxMatrix(type="Full",	nrow=nfact, ncol=nfact, free=PatPhC, values=StPhCm, labels=LabPhCm, name="PhCm" )
PhCausf	<-mxMatrix(type="Full",	nrow=nfact, ncol=nfact, free=PatPhC, values=StPhCf, labels=LabPhCf, name="PhCf" )

# Define the matrix to hold the A and C effects: Specific-fixed to 0 
PathsAsm	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpam, labels=LabAsm, name="asm" )
PathsCsm	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpcm, labels=LabCsm, name="csm" )
PathsEsm	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpem, labels=LabEsm, name="esm" )

PathsAsf	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpaf, labels=LabAsf, name="asf" )
PathsCsf	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpcf, labels=LabCsf, name="csf" )
PathsEsf	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpef, labels=LabEsf, name="esf" )


covAsm	<-mxAlgebra( expression= asm %*% t(asm), name="Asm" )
covCsm	<-mxAlgebra( expression= csm %*% t(csm), name="Csm" )
covEsm	<-mxAlgebra( expression= esm %*% t(esm), name="Esm" )

covAsf	<-mxAlgebra( expression= asf %*% t(asf), name="Asf" )
covCsf	<-mxAlgebra( expression= csf %*% t(csf), name="Csf" )
covEsf	<-mxAlgebra( expression= esf %*% t(esf), name="Esf" )

covAsmf	<-mxAlgebra( expression= asm %*% t(asf), name="Asmf" )
covCsmf	<-mxAlgebra( expression= csm %*% t(csf), name="Csmf" )
covAsfm	<-mxAlgebra( expression= asf %*% t(asm), name="Asfm" )
covCsfm	<-mxAlgebra( expression= csf %*% t(csm), name="Csfm" )

covPsm	<-mxAlgebra( expression= Asm+Csm+Esm, name="Vsm" )
covPsf	<-mxAlgebra( expression= Asf+Csf+Esf, name="Vsf" )

# Define the matrices to hold the A and C effects: Common 
#SEC~~SCA fixed to 0
PathsAcspm	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,F,T,T,F,T,T,T), values=c(.8,.1,0,0,.8,.1,0,.8,.1,.8), labels=c("a_csp1m","a_csp21m","a_csp31m","a_csp41m","a_csp2m","a_csp32m","a_csp42m","a_csp3m","a_csp43m","a_csp4m"), name="acm" )
PathsCcspm	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,F,T,T,F,T,T,T), values=c(.1,.1,0,0,.1,.1,0,.1,.1,.1), labels=c("c_csp1m","c_csp21m","c_csp31m","c_csp41m","c_csp2m","c_csp32m","c_csp42m","c_csp3m","c_csp43m","c_csp4m"), name="ccm" )
PathsEcspm	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,F,T,T,F,T,T,T), values=c(.8,.1,0,0,.8,.1,0,.8,.1,.8), labels=c("e_csp1m","e_csp21m","e_csp31m","e_csp41m","e_csp2m","e_csp32m","e_csp42m","e_csp3m","e_csp43m","e_csp4m"), name="ecm" )

PathsAcspf	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,F,T,T,F,T,T,T), values=c(.8,.1,0,0,.8,.1,0,.8,.1,.8), labels=c("a_csp1f","a_csp21f","a_csp31f","a_csp41f","a_csp2f","a_csp32f","a_csp42f","a_csp3f","a_csp43f","a_csp4f"), name="acf" )
PathsCcspf	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,F,T,T,F,T,T,T), values=c(.1,.1,0,0,.1,.1,0,.1,.1,.1), labels=c("c_csp1f","c_csp21f","c_csp31f","c_csp41f","c_csp2f","c_csp32f","c_csp42f","c_csp3f","c_csp43f","c_csp4f"), name="ccf" )
PathsEcspf	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,F,T,T,F,T,T,T), values=c(.8,.1,0,0,.8,.1,0,.8,.1,.8), labels=c("e_csp1f","e_csp21f","e_csp31f","e_csp41f","e_csp2f","e_csp32f","e_csp42f","e_csp3f","e_csp43f","e_csp4f"), name="ecf" )

covAcm	<-mxAlgebra( expression= acm %*% t(acm), name="Acm" )
covCcm	<-mxAlgebra( expression= ccm %*% t(ccm), name="Ccm" )
covEcm	<-mxAlgebra( expression= ecm %*% t(ecm), name="Ecm" )

covAcf	<-mxAlgebra( expression= acf %*% t(acf), name="Acf" )
covCcf	<-mxAlgebra( expression= ccf %*% t(ccf), name="Ccf" )
covEcf	<-mxAlgebra( expression= ecf %*% t(ecf), name="Ecf" )

covAcmf	<-mxAlgebra( expression= acm %*% t(acf), name="Acmf" )
covCcmf	<-mxAlgebra( expression= ccm %*% t(ccf), name="Ccmf" )
covAcfm	<-mxAlgebra( expression= acf %*% t(acm), name="Acfm" )
covCcfm	<-mxAlgebra( expression= ccf %*% t(ccm), name="Ccfm" )

covPcm	<-mxAlgebra( expression= Acm+Ccm+Ecm, name="Vcm" )
covPcf	<-mxAlgebra( expression= Acf+Ccf+Ecf, name="Vcf" )

# Generate Covariance of Latent factor model Including Causal Paths between factors
Id4		<-mxMatrix(type="Iden",	nrow=nfact, ncol=nfact, name="I4" )
covFAcm	<-mxAlgebra( expression= solve(I4-PhCm) %&% Acm, name ="FAcm")
covFCcm	<-mxAlgebra( expression= solve(I4-PhCm) %&% Ccm, name ="FCcm")
covFEcm	<-mxAlgebra( expression= solve(I4-PhCm) %&% Ecm, name ="FEcm")

covFAcf	<-mxAlgebra( expression= solve(I4-PhCf) %&% Acf, name ="FAcf")
covFCcf	<-mxAlgebra( expression= solve(I4-PhCf) %&% Ccf, name ="FCcf")
covFEcf	<-mxAlgebra( expression= solve(I4-PhCf) %&% Ecf, name ="FEcf")

covFAcmf	<-mxAlgebra( expression= solve(I4-PhCm) %*% Acmf %*% t(solve(I4-PhCf)), name ="FAcmf")
covFCcmf	<-mxAlgebra( expression= solve(I4-PhCm) %*% Ccmf %*% t(solve(I4-PhCf)), name ="FCcmf")
covFAcfm	<-mxAlgebra( expression= solve(I4-PhCf) %*% Acfm %*% t(solve(I4-PhCm)), name ="FAcfm")
covFCcfm	<-mxAlgebra( expression= solve(I4-PhCf) %*% Ccfm %*% t(solve(I4-PhCm)), name ="FCcfm")

covFcm	<-mxAlgebra( expression= (FAcm+FCcm+FEcm), name="FVcm" )
covFcf	<-mxAlgebra( expression= (FAcf+FCcf+FEcf), name="FVcf" )

FcovMZM	<-mxAlgebra( expression= (FactLTwm  %&% rbind ( cbind(FVcm, FAcm+FCcm), cbind(FAcm+FCcm, FVcm) )) , name="expFCovMZM" )
FcovMZF	<-mxAlgebra( expression= (FactLTwf  %&% rbind ( cbind(FVcf, FAcf+FCcf), cbind(FAcf+FCcf, FVcf) )) , name="expFCovMZF" )
FcovDZM	<-mxAlgebra( expression= (FactLTwm  %&% rbind ( cbind(FVcm, .5%x%FAcm+FCcm), cbind(.5%x%FAcm+FCcm, FVcm) )) , name="expFCovDZM" )
FcovDZF	<-mxAlgebra( expression= (FactLTwf  %&% rbind ( cbind(FVcf, .5%x%FAcf+FCcf), cbind(.5%x%FAcf+FCcf, FVcf) )) , name="expFCovDZF" )
FcovDZO	<-mxAlgebra( expression= (FactLTwo  %&% rbind ( cbind(FVcm, .5%x%FAcmf+FCcmf), cbind(.5%x%FAcfm+FCcfm, FVcf) )) , name="expFCovDZO" )

SpcovMZM	<-mxAlgebra( expression= rbind (cbind(Vsm, Asm+Csm), cbind(Asm+Csm, Vsm) ) , name="expSpCovMZM" )
SpcovMZF	<-mxAlgebra( expression= rbind (cbind(Vsf, Asf+Csf), cbind(Asf+Csf, Vsf) ) , name="expSpCovMZF" )
SpcovDZM	<-mxAlgebra( expression= rbind (cbind(Vsm, .5%x%Asm+Csm), cbind(.5%x%Asm+Csm, Vsm) ) , name="expSpCovDZM" )
SpcovDZF	<-mxAlgebra( expression= rbind (cbind(Vsf, .5%x%Asf+Csf), cbind(.5%x%Asf+Csf, Vsf) ) , name="expSpCovDZF" )
SpcovDZO	<-mxAlgebra( expression= rbind (cbind(Vsm, .5%x%Asmf+Csmf), cbind(.5%x%Asfm+Csfm, Vsf) ) , name="expSpCovDZO" )

TOTcovMZM	<-mxAlgebra( expression= expFCovMZM + expSpCovMZM , name="TOTexpCovMZM" )
TOTcovMZF	<-mxAlgebra( expression= expFCovMZF + expSpCovMZF , name="TOTexpCovMZF" )
TOTcovDZM	<-mxAlgebra( expression= expFCovDZM + expSpCovDZM , name="TOTexpCovDZM" )
TOTcovDZF	<-mxAlgebra( expression= expFCovDZF + expSpCovDZF , name="TOTexpCovDZF" )
TOTcovDZO	<-mxAlgebra( expression= expFCovDZO + expSpCovDZO , name="TOTexpCovDZO" )

# *******************************************************************************************************
# Calculator

# Standardize the causal effects
Stcp1on4m	<-mxAlgebra( expression= (PhCm[4,1]* sqrt(FVcm[1,1]))/sqrt(FVcm[4,4]) , name="Stand_1on4m" )
Stcp2on4m	<-mxAlgebra( expression= (PhCm[4,2]* sqrt(FVcm[2,2]))/sqrt(FVcm[4,4]) , name="Stand_2on4m" )
Stcp3on4m	<-mxAlgebra( expression= (PhCm[4,3]* sqrt(FVcm[3,3]))/sqrt(FVcm[4,4]) , name="Stand_3on4m" )

Stcp1on4f	<-mxAlgebra( expression= (PhCf[4,1]* sqrt(FVcf[1,1]))/sqrt(FVcf[4,4]) , name="Stand_1on4f" )
Stcp2on4f	<-mxAlgebra( expression= (PhCf[4,2]* sqrt(FVcf[2,2]))/sqrt(FVcf[4,4]) , name="Stand_2on4f" )
Stcp3on4f	<-mxAlgebra( expression= (PhCf[4,3]* sqrt(FVcf[3,3]))/sqrt(FVcf[4,4]) , name="Stand_3on4f" )

# Standardize the Total var/covariances matrices of the observed variables
Id8		<-mxMatrix(type="Iden",	nrow=ntv, ncol=ntv, name="I8" )
Rfactmzm	<-mxAlgebra( expression= solve(sqrt(I8*TOTexpCovMZM)) %&% TOTexpCovMZM, name="FactcorMZM" )
Rfactmzf	<-mxAlgebra( expression= solve(sqrt(I8*TOTexpCovMZF)) %&% TOTexpCovMZF, name="FactcorMZF" )
Rfactdzm	<-mxAlgebra( expression= solve(sqrt(I8*TOTexpCovDZM)) %&% TOTexpCovDZM, name="FactcorDZM" )
Rfactdzf	<-mxAlgebra( expression= solve(sqrt(I8*TOTexpCovDZF)) %&% TOTexpCovDZF, name="FactcorDZF" )
Rfactdzo	<-mxAlgebra( expression= solve(sqrt(I8*TOTexpCovDZO)) %&% TOTexpCovDZO, name="FactcorDZO" )

# Standardize the Common Effects
stcovAcm	<-mxAlgebra( expression= FAcm/FVcm, name="stAcm" )
stcovCcm	<-mxAlgebra( expression= FCcm/FVcm, name="stCcm" )
stcovEcm	<-mxAlgebra( expression= FEcm/FVcm, name="stEcm" )

stcovAcf	<-mxAlgebra( expression= FAcf/FVcf, name="stAcf" )
stcovCcf	<-mxAlgebra( expression= FCcf/FVcf, name="stCcf" )
stcovEcf	<-mxAlgebra( expression= FEcf/FVcf, name="stEcf" )

# Standardize the Specific Effects
stcovAsm	<-mxAlgebra( expression= sqrt(Asm/( (FactLm %&% FVcm) +Vsm)), name="stAsm" )
stcovCsm	<-mxAlgebra( expression= sqrt(Csm/( (FactLm %&% FVcm) +Vsm)), name="stCsm" )
stcovEsm	<-mxAlgebra( expression= sqrt(Esm/( (FactLm %&% FVcm) +Vsm)), name="stEsm" )

stcovAsf	<-mxAlgebra( expression= sqrt(Asf/( (FactLf %&% FVcf) +Vsf)), name="stAsf" )
stcovCsf	<-mxAlgebra( expression= sqrt(Csf/( (FactLf %&% FVcf) +Vsf)), name="stCsf" )
stcovEsf	<-mxAlgebra( expression= sqrt(Esf/( (FactLf %&% FVcf) +Vsf)), name="stEsf" )

# Standardized Effects of Individual variables from the factors (Variance components) above
stAvarm	<-mxAlgebra( expression= (FactLm %&% FAcm)/( (FactLm %&% FVcm) +Vsm), name="stAvariablesm" )
stCvarm	<-mxAlgebra( expression= (FactLm %&% FCcm)/( (FactLm %&% FVcm) +Vsm), name="stCvariablesm" )
stEvarm	<-mxAlgebra( expression= (FactLm %&% FEcm)/( (FactLm %&% FVcm) +Vsm), name="stEvariablesm" )

stAvarf	<-mxAlgebra( expression= (FactLf %&% FAcf)/( (FactLf %&% FVcf) +Vsf), name="stAvariablesf" )
stCvarf	<-mxAlgebra( expression= (FactLf %&% FCcf)/( (FactLf %&% FVcf) +Vsf), name="stCvariablesf" )
stEvarf	<-mxAlgebra( expression= (FactLf %&% FEcf)/( (FactLf %&% FVcf) +Vsf), name="stEvariablesf" )

# Standardized Factor Loadings
StFLm		<-mxAlgebra( expression= sqrt(diag2vec( FactLm %&% FVcm / TOTexpCovMZM[1:4,1:4])) , name="StandFactm" )
StFLf		<-mxAlgebra( expression= sqrt(diag2vec( FactLf %&% FVcf / TOTexpCovMZF[1:4,1:4])) , name="StandFactf" )

# *******************************************************************************************************

# Data objects for Multiple Groups
dataMZM	<- mxData( observed=mzmData, type="raw" )
dataMZF	<- mxData( observed=mzfData, type="raw" )
dataDZM	<- mxData( observed=dzmData, type="raw" )
dataDZF	<- mxData( observed=dzfData, type="raw" )
dataDZO	<- mxData( observed=dzoData, type="raw" )

# Objective objects for Multiple Groups
objMZM	<- mxExpectationNormal( covariance="TOTexpCovMZM", means="expMeanm", dimnames=selVars)
objMZF	<- mxExpectationNormal( covariance="TOTexpCovMZF", means="expMeanf", dimnames=selVars)
objDZM	<- mxExpectationNormal( covariance="TOTexpCovDZM", means="expMeanm", dimnames=selVars)
objDZF	<- mxExpectationNormal( covariance="TOTexpCovDZF", means="expMeanf", dimnames=selVars)
objDZO	<- mxExpectationNormal( covariance="TOTexpCovDZO", means="expMeano", dimnames=selVars)

fitFunction <- mxFitFunctionML()

# Combine Groups
pars1m	<-list(Meansm,Loadm,LoadTwm,PhCausm,PathsAsm,PathsCsm,PathsEsm,covAsm,covCsm,covEsm,covPsm,Id4,Id2,Id8, stAvarm, stCvarm, stEvarm)
pars1f	<-list(Meansf,Loadf,LoadTwf,PhCausf,PathsAsf,PathsCsf,PathsEsf,covAsf,covCsf,covEsf,covPsf,Id4,Id2,Id8, stAvarf, stCvarf, stEvarf)
pars2m	<-list(PathsAcspm,PathsCcspm,PathsEcspm,covAcm,covCcm,covEcm,covPcm,covFAcm,covFCcm,covFEcm,covFcm,stcovAcm,stcovCcm,stcovEcm, stcovAsm, stcovCsm, stcovEsm)
pars2f	<-list(PathsAcspf,PathsCcspf,PathsEcspf,covAcf,covCcf,covEcf,covPcf,covFAcf,covFCcf,covFEcf,covFcf,stcovAcf,stcovCcf,stcovEcf, stcovAsf, stcovCsf, stcovEsf)
parso		<-list(Meanso,Ze2,LoadTwo,covAsmf,covCsmf,covAsfm,covCsfm,covAcmf,covCcmf,covAcfm,covCcfm,covFAcmf,covFCcmf,covFAcfm,covFCcfm)
parsmedm	<-list(Stcp1on4m, Stcp2on4m, Stcp3on4m)
parsmedf	<-list(Stcp1on4f, Stcp2on4f, Stcp3on4f)
modelMZM	<-mxModel(pars1m, pars2m, parsmedm, FcovMZM, SpcovMZM, TOTcovMZM, dataMZM, objMZM, Rfactmzm, fitFunction, StFLm, name="MZM" )
modelMZF	<-mxModel(pars1f, pars2f, parsmedf, FcovMZF, SpcovMZF, TOTcovMZF, dataMZF, objMZF, Rfactmzf, fitFunction, StFLf, name="MZF" )
modelDZM	<-mxModel(pars1m, pars2m, FcovDZM, SpcovDZM, TOTcovDZM, dataDZM, objDZM, Rfactdzm, fitFunction, name="DZM" )
modelDZF	<-mxModel(pars1f, pars2f, FcovDZF, SpcovDZF, TOTcovDZF, dataDZF, objDZF, Rfactdzf, fitFunction, name="DZF" )
modelDZO	<-mxModel(pars1m, pars2m, pars1f, pars2f, parso, FcovDZO, SpcovDZO, TOTcovDZO, dataDZO, objDZO, Rfactdzo, fitFunction, name="DZO" )
minus2ll	<-mxAlgebra( expression=MZM.objective + MZF.objective + DZM.objective + DZF.objective + DZO.objective, name="m2LL" )
obj		<-mxFitFunctionAlgebra( "m2LL" )

cistFLm	<-mxCI (c ('MZM.StandFactm','MZM.Stand_1on4m','MZM.Stand_2on4m','MZM.Stand_3on4m'))
cistFLf	<-mxCI (c ('MZF.StandFactf','MZF.Stand_1on4f','MZF.Stand_2on4f','MZF.Stand_3on4f'))

cistFcm	<-mxCI (c ('MZM.stAcm','MZM.stCcm','MZM.stEcm') ) 	# standardized var comp from Common feactors	
cistFcf	<-mxCI (c ('MZF.stAcf','MZF.stCcf','MZF.stEcf') ) 	# standardized var comp from Common feactors	

cistVsm	<-mxCI (c ('MZM.stAsm','MZM.stCsm','MZM.stEsm') ) 	# standardized var comp from specific Factors
cistVsf	<-mxCI (c ('MZF.stAsf','MZF.stCsf','MZF.stEsf') ) 	# standardized var comp from specific Factors

cistvarsm	<-mxCI (c ('MZM.stAvariablesm','MZM.stCvariablesm','MZM.stEvariablesm'))
cistvarsf	<-mxCI (c ('MZF.stAvariablesf','MZF.stCvariablesf','MZF.stEvariablesf'))

HetACEMsModel2	<-mxModel("HetaceMs2", pars1m, pars2m, pars1f, pars2f, parso, modelMZM, modelMZF, modelDZM, modelDZF, modelDZO, minus2ll, obj, cistFcm, cistFcf, cistFLm, cistFLf, cistVsm, cistVsf, cistvarsm, cistvarsf) 

# --------------------------------------------------------------------------------------------------------------------------------
# RUN HetACEMs Factor Model with phenotypic causal paths by Zygosity

HetACEMsFit2	<-mxRun(HetACEMsModel2, intervals=T)#intervals=T for CIs
(HetACEMsSumm2	<-summary(HetACEMsFit2, verbose=F))

# Get some output

mxEval(MZM.Vsm, HetACEMsFit2)
mxEval(MZF.Vsf, HetACEMsFit2)

mxEval(MZM.FAcm, HetACEMsFit2)
mxEval(MZM.FCcm, HetACEMsFit2)
mxEval(MZM.FEcm, HetACEMsFit2)
mxEval(MZM.FVcm, HetACEMsFit2)
mxEval(MZM.PhCm, HetACEMsFit2)

mxEval(MZF.FAcf, HetACEMsFit2)
mxEval(MZF.FCcf, HetACEMsFit2)
mxEval(MZF.FEcf, HetACEMsFit2)
mxEval(MZF.FVcf, HetACEMsFit2)
mxEval(MZF.PhCf, HetACEMsFit2)

mxEval(MZM.stAcm, HetACEMsFit2)
mxEval(MZM.stCcm, HetACEMsFit2)
mxEval(MZM.stEcm, HetACEMsFit2)

mxEval(MZF.stAcf, HetACEMsFit2)
mxEval(MZF.stCcf, HetACEMsFit2)
mxEval(MZF.stEcf, HetACEMsFit2)

mxEval(MZM.Acm, HetACEMsFit2)
mxEval(MZM.Ccm, HetACEMsFit2)
mxEval(MZM.Ecm, HetACEMsFit2)

mxEval(MZF.Acf, HetACEMsFit2)
mxEval(MZF.Ccf, HetACEMsFit2)
mxEval(MZF.Ecf, HetACEMsFit2)

mxEval(MZM.stAsm, HetACEMsFit2)
mxEval(MZM.stCsm, HetACEMsFit2)
mxEval(MZM.stEsm, HetACEMsFit2)

mxEval(MZF.stAsf, HetACEMsFit2)
mxEval(MZF.stCsf, HetACEMsFit2)
mxEval(MZF.stEsf, HetACEMsFit2)

mxEval(MZM.stAvariablesm, HetACEMsFit2)
mxEval(MZM.stCvariablesm, HetACEMsFit2)
mxEval(MZM.stEvariablesm, HetACEMsFit2)

mxEval(MZF.stAvariablesf, HetACEMsFit2)
mxEval(MZF.stCvariablesf, HetACEMsFit2)
mxEval(MZF.stEvariablesf, HetACEMsFit2)

mxEval(MZM.Stand_1on4m, HetACEMsFit2)
mxEval(MZM.Stand_2on4m, HetACEMsFit2)
mxEval(MZM.Stand_3on4m, HetACEMsFit2)

mxEval(MZF.Stand_1on4f, HetACEMsFit2)
mxEval(MZF.Stand_2on4f, HetACEMsFit2)
mxEval(MZF.Stand_3on4f, HetACEMsFit2)

#*****************************************************************************************************
# Test for sex differences in the causal paths
# Constrain the causal paths to be equal and compare models
#*****************************************************************************************************

HomACEMsModel2	<-mxModel(HetACEMsFit2, name="HomACEMs2")
HomACEMsModel2	<-omxSetParameters(HomACEMsModel2, labels= c('c1on4m','c1on4f'), newlabels=  c('c1on4f'),free=T,values=.1)
HomACEMsModel2	<-omxSetParameters(HomACEMsModel2, labels= c('c2on4m','c2on4f'), newlabels=  c('c2on4f'),free=T,values=.1)
HomACEMsFit2		<-mxRun(HomACEMsModel2, intervals=T)
(HomACEMsSumm2	<-summary(HomACEMsFit2, verbose = F))

mxCompare(HetACEMsFit2,HomACEMsFit2)

#test sex difference again, fixing C and E covariance between SCA and AUDIT to 0
HetACEMsModel3  <- mxModel( HetACEMsFit2, name="HetACEMs3" )
HetACEMsModel3  <- omxSetParameters( HetACEMsModel3, labels=c("c_csp43m","e_csp43m","c_csp43f","e_csp43f"), free=FALSE, values=0 )
HetACEMsFit3	<-mxRun(HetACEMsModel3, intervals=T)#intervals=T for CIs
(HetACEMsSumm3	<-summary(HetACEMsFit3, verbose=F))

HomACEMsModel3	<-mxModel(HetACEMsFit3, name="HomACEMs3")
HomACEMsModel3	<-omxSetParameters(HomACEMsModel3, labels= c('c1on4m','c1on4f'), newlabels=  c('c1on4f'),free=T,values=.1)
HomACEMsModel3	<-omxSetParameters(HomACEMsModel3, labels= c('c2on4m','c2on4f'), newlabels=  c('c2on4f'),free=T,values=.1)
HomACEMsFit3		<-mxRun(HomACEMsModel3, intervals=T)
(HomACEMsSumm3	<-summary(HomACEMsFit3, verbose = F))

mxCompare(HetACEMsFit3,HomACEMsFit3)

# **************************************************

#*******************
# 3-VARIABLE MODELS
#*******************
#IEA, SCA predicting AUDITC
# ******************************************************************************************************************************************
# Prepare data for modelling - 3 variable Cholesky
# ******************************************************************************************************************************************

nv			<- 3				# number of variables for a twin = 1 in Univariate
ntv			<- 2*nv			#number of variables for a pair = 2* 1 for Univariate
nlower		<- nv*(nv+1)/2 		#number of free elements in a lower matrix nv*nv
ncor			<- (nv*(nv+1)/2)-nv	#number of free elements in a correlation matrix nv*nv
nfact			<- 3				# number of Latent Factors for Mediation Model per twin
nfact2		<- 2*nfact			# number of Latent Factors for Mediation Model per twin pair
nfcor			<- (nfact*(nfact+1)/2)-nfact	# number of free elements in a correlation matrix nfact*nfcat
Groups		<- c("mz", "dz")
Vars			<- c("IEA","SCA","AUDITC")
selVars		<- c("IEA1","SCA1","AUDITC1",
              "IEA2","SCA2","AUDITC2")

mzData		<- subset(TWINdata2, zyg1%in%c(1,3)|zyg2%in%c(1,3) , selVars)
dzData		<- subset(TWINdata2, zyg1%in%c(2,4,5)|zyg2%in%c(2,4,5) , selVars)

psych::describe(mzData)
psych::describe(dzData)

#*******************************************************************************************************
# Specify the Models
#****************************************************************************************************************************
# _______________________________________________________________________________________________________________________
# ACE Factor MODEL by zygosity
# NO causal paths between Phenotypic Factors; A, C and E latent factors have Cholesky Structure
# Correlation between Phenotypic Factors only due to shared A, C and E influences
#_____________________________________________________________________________________________________________________________

# CREATE LABELS & START VALUES as objects(to ease specification in the body of the model)
(mLabs	<- paste("m",1:nv,sep=""))
(Stmean	<- colMeans(mzData[,1:nv],na.rm=TRUE))
(Stsd 	<- sapply(mzData[,1:nv],sd, na.rm=TRUE))
(PatM		<- c(TRUE,TRUE,TRUE))

# Create Labels for Diagonal Matrices-fixed to 0
(LabEs	<- c('es1','es2','es3'))
(LabAs	<- c('as1','as2','as3'))
(LabCs	<- c('cs1','cs2','cs3'))

PatSp		<- c(F,F,F)
StSpa		<- c(0,0,0)
StSpc		<- c(0,0,0)
StSpe		<- c(0,0,0)

# all factor loadings fixed to 1
PatFl		<- c(F,F,F,			
            F,F,F,
            F,F,F)

StFl		<- c(1,0,0,
           0,1,0,
           0,0,1)

LabFl		<- c('l1',NA,NA,
            NA,'l2',NA,
            NA,NA,'l3')

PatPhC	<- c(F,F,F,
            F,F,F,
            F,F,F)

StPhC		<- c(0,0,0, #This is the matrix for the causal paths - all fixed to 0
            0,0,0,
            0,0,0)

LabPhC	<- c(NA,NA,NA,
            NA,NA,NA,	
            NA,NA,NA)	 

# ______________________________________________________________________________________________________
# Define matrices to hold the Means, SD, correlations
# Use Algebra to generate expected var/cov matrices and Means
# Specify: data objects, Fitfunction, the Model, 
# Run the Model 
# ______________________________________________________________________________________________________

Means		<-mxMatrix("Full", 1, ntv, free=c(PatM,PatM), values=c(Stmean,Stmean), labels=c(mLabs,mLabs), name="expMean") 

# Define matrices to specify the loadings of the dependent variables on the latent factors
Load		<-mxMatrix(type="Full",	nrow=nv, ncol=nfact, free=PatFl, values=StFl, labels=LabFl, name="FactL" )
Id2		<-mxMatrix(type="Iden",	nrow=2, ncol=2, free=F, name="I2" )
LoadTw	<-mxAlgebra(I2%x%FactL, name="FactLTw")

# Define the matrix to hold the Single headed Arrows (causal paths) from predictors to AUDITC  
PhCaus	<-mxMatrix(type="Full",	nrow=nfact, ncol=nfact, free=PatPhC, values=StPhC, labels=LabPhC, name="PhC" )

# Define the matrix to hold the A and C and E effects: Specific-fixed to 0 
PathsAs	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpa, labels=LabAs, name="as" )
PathsCs	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpc, labels=LabCs, name="cs" )
PathsEs	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpe, labels=LabEs, name="es" )

covAs		<-mxAlgebra( expression= as %*% t(as), name="As" )
covCs		<-mxAlgebra( expression= cs %*% t(cs), name="Cs" )
covEs		<-mxAlgebra( expression= es %*% t(es), name="Es" )

covPs		<-mxAlgebra( expression= As+Cs+Es, name="Vs" )

# Define the matrices to hold the A and C and E effects: Common 
PathsAc	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,T,T,T,T), values=c(.6,.6,.6,.6,.6,.6), labels=c("a11","a21","a31","a22","a32","a33"), name="a_c" )
PathsCc	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,T,T,T,T), values=c(.05,.05,.05,.05,.05,.05), labels=c("c11","c21","c31","c22","c32","c33"), name="c_c" )
PathsEc	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,T,T,T,T), values=c(.5,.5,.5,.5,.5,.5), labels=c("e11","e21","e31","e22","e32","e33"), name="e_c" )

covAc		<-mxAlgebra( expression= a_c %*% t(a_c), name="Ac" )
covCc		<-mxAlgebra( expression= c_c %*% t(c_c), name="Cc" )
covEc		<-mxAlgebra( expression= e_c %*% t(e_c), name="Ec" )
covPc		<-mxAlgebra( expression= Ac+Cc+Ec, name="Vc" )

# Generate Covariance of Latent factor model Including Causal Paths between factors
Id4		<-mxMatrix(type="Iden",	nrow=nfact, ncol=nfact, name="I4" )
covFAc	<-mxAlgebra( expression= solve(I4-PhC) %&% Ac, name ="FAc") #(I4-PhC) gives the expression for the removal of the loop effect of causal relationships between the factors.
covFCc	<-mxAlgebra( expression= solve(I4-PhC) %&% Cc, name ="FCc")
covFEc	<-mxAlgebra( expression= solve(I4-PhC) %&% Ec, name ="FEc")
covFc		<-mxAlgebra( expression= FAc+FCc+FEc, name="FVc" )

# Var-Cov of measured vars in terms of latent factors and AC, Cc, and Ec
FcovMZ	<-mxAlgebra( expression= (FactLTw  %&% rbind ( cbind(FVc, FAc+FCc), cbind(FAc+FCc, FVc) )) , name="expFCovMZ" )#This traces the path from vars to factors and back to vars
FcovDZ	<-mxAlgebra( expression= (FactLTw  %&% rbind ( cbind(FVc, .5%x%FAc+FCc), cbind(.5%x%FAc+FCc, FVc) )) , name="expFCovDZ" )

SpcovMZ	<-mxAlgebra( expression= rbind (cbind(Vs, As+Cs), cbind(As+Cs, Vs) ) , name="expSpCovMZ" )
SpcovDZ	<-mxAlgebra( expression= rbind (cbind(Vs, .5%x%As+Cs), cbind(.5%x%As+Cs, Vs) ) , name="expSpCovDZ" )

TOTcovMZ	<-mxAlgebra( expression= expFCovMZ + expSpCovMZ , name="TOTexpCovMZ" )
TOTcovDZ	<-mxAlgebra( expression= expFCovDZ + expSpCovDZ , name="TOTexpCovDZ" )

# *******************************************************************************************************
# Calculator

# Calculate genetic,  environmental, and phenotypic correlations
corA		<- mxAlgebra( expression=solve(sqrt(I4*FAc))%&%FAc, name ="rA" ) 
corC		<- mxAlgebra( expression=solve(sqrt(I4*FCc))%&%FCc, name ="rC" )
corE		<- mxAlgebra( expression=solve(sqrt(I4*FEc))%&%FEc, name ="rE" )
corP		<- mxAlgebra( expression=solve(sqrt(I4*FVc))%&%FVc, name ="rP") 

# Standardize the Total var/covariances matrices of the observed variables
Id8		<-mxMatrix(type="Iden",	nrow=ntv, ncol=ntv, name="I8" )
Rfactmz	<-mxAlgebra( expression= solve(sqrt(I8*TOTexpCovMZ)) %&% TOTexpCovMZ, name="FactcorMZ" )
Rfactdz	<-mxAlgebra( expression= solve(sqrt(I8*TOTexpCovDZ)) %&% TOTexpCovDZ, name="FactcorDZ" )

# Standardize the Common Effects
stcovAc	<-mxAlgebra( expression= FAc/FVc, name="stAc" )
stcovCc	<-mxAlgebra( expression= FCc/FVc, name="stCc" )
stcovEc	<-mxAlgebra( expression= FEc/FVc, name="stEc" )

# Standardize the Specific Effects
stcovAs	<-mxAlgebra( expression= As/( (FactL %&% FVc) +Vs), name="stAs" )
stcovCs	<-mxAlgebra( expression= Cs/( (FactL %&% FVc) +Vs), name="stCs" )
stcovEs	<-mxAlgebra( expression= Es/( (FactL %&% FVc) +Vs), name="stEs" )

# Standardized Effects of Individual variables from the factors (Variance components) above
stAvar	<-mxAlgebra( expression= ((FactL %&% FAc)/( (FactL %&% FVc) +Vs)), name="stAvariables" )
stCvar	<-mxAlgebra( expression= ((FactL %&% FCc)/( (FactL %&% FVc) +Vs)), name="stCvariables" )
stEvar	<-mxAlgebra( expression= ((FactL %&% FEc)/( (FactL %&% FVc) +Vs)), name="stEvariables" )

# Standardized Factor Loadings
StFL		<-mxAlgebra( expression= (diag2vec( FactL %&% FVc / TOTexpCovMZ[1:3,1:3])) , name="StandFact" )

# *******************************************************************************************************

# Data objects for Multiple Groups
dataMZ	<- mxData( observed=mzData, type="raw" )
dataDZ	<- mxData( observed=dzData, type="raw" )

# Objective objects for Multiple Groups
objMZ		<- mxExpectationNormal( covariance="TOTexpCovMZ", means="expMean", dimnames=selVars)
objDZ		<- mxExpectationNormal( covariance="TOTexpCovDZ", means="expMean", dimnames=selVars)

fitFunction <- mxFitFunctionML()

# Combine Groups
pars1		<-list(Means,Load,LoadTw,PhCaus,PathsAs,PathsAs,PathsCs,PathsEs,covAs,covCs,covEs,covPs,Id4,Id2,Id8, stAvar, stCvar, stEvar)
pars2		<-list(PathsAc,PathsCc,PathsEc,covAc,covCc,covEc,covPc,covFAc,covFCc,covFEc,covFc,stcovAc,stcovCc,stcovEc, stcovAs, stcovCs, stcovEs, corA, corC, corE)
modelMZ	<-mxModel(pars1, pars2, FcovMZ, SpcovMZ, TOTcovMZ, dataMZ, objMZ, Rfactmz, fitFunction, StFL, name="MZ" )
modelDZ	<-mxModel(pars1, pars2, FcovDZ, SpcovDZ, TOTcovDZ, dataDZ, objDZ, Rfactdz, fitFunction, name="DZ" )
minus2ll	<-mxAlgebra( expression=MZ.objective + DZ.objective, name="m2LL" )
obj		<-mxFitFunctionAlgebra( "m2LL" )
cistFL	<-mxCI (c ('MZ.StandFact','MZ.rA','MZ.rC','MZ.rE'))
cistFc	<-mxCI (c ('MZ.stAc','MZ.stCc','MZ.stEc') ) 	# standardized var comp from Common feactors	
cistVs	<-mxCI (c ('MZ.stAs','MZ.stEs') ) 	# standardized var comp from specific Factors
#cistvars	<-mxCI (c ('MZ.stAvariables','MZ.stCvariables','MZ.stEvariables'))
ACE2Model	<-mxModel("ace2", pars1, pars2, modelMZ, modelDZ, minus2ll, obj, cistFc, cistFL, cistVs) 

# --------------------------------------------------------------------------------------------------------------------------------
# RUN ACE Factor Model: Cholesky (by Zygosity)

ACE2Fit		<-mxRun(ACE2Model, intervals=T)
#ACEFit		<-mxTryHard(ACEModel, intervals=F, bestInitsOutput=TRUE, showInits=TRUE)
(ACE2Summ		<-summary(ACE2Fit, verbose=T))

# Get some output

mxEval(MZ.Vs, ACE2Fit)

mxEval(MZ.FAc, ACE2Fit)
mxEval(MZ.FCc, ACE2Fit)
mxEval(MZ.FEc, ACE2Fit)
mxEval(MZ.FVc, ACE2Fit)
mxEval(MZ.PhC, ACE2Fit)

mxEval(MZ.stAc, ACE2Fit)
mxEval(MZ.stCc, ACE2Fit)
mxEval(MZ.stEc, ACE2Fit)

mxEval(MZ.stAs, ACE2Fit)
mxEval(MZ.stCs, ACE2Fit)
mxEval(MZ.stEs, ACE2Fit)

mxEval(MZ.StandFact, ACE2Fit)

mxEval(MZ.stAvariables, ACE2Fit)
mxEval(MZ.stCvariables, ACE2Fit)
mxEval(MZ.stEvariables, ACE2Fit)

mxEval(MZ.FactcorMZ, ACE2Fit)
mxEval(DZ.FactcorDZ, ACE2Fit)


#****************************************************************************************************************************
# _______________________________________________________________________________________________________________________
# Genetic Factor MODEL with causal paths by zygosity
# Causal paths specified from predictors to AUDITC
# Asp, Csp and Esp in the bottom with constraints to Identify the model - fixed to 0
#_____________________________________________________________________________________________________________________________

nv		<- 3				# number of variables for a twin = 1 in Univariate
ntv		<- 2*nv			# number of variables for a pair = 2* 1 for Univariate
nfact		<- 3				# number of Latent Factors for Causal Model per twin
nfact2	<- 2*nfact			# number of Latent Factors for Causal Model per twin pair
nlower	<- ntv*(ntv+1)/2 		# number of free elements in a lower matrix ntv*ntv

# CREATE LABELS & START VALUES as objects(to ease specification in the body of the model)
(mLabs	<- paste("m",1:nv,sep=""))
(Stmean	<-colMeans(mzData[,1:nv],na.rm=TRUE))
(Stsd 	<-sapply(mzData[,1:nv],sd, na.rm=TRUE))
(PatM		<- c(TRUE,TRUE,TRUE))

# Create Labels for Diagonal Matrices-fixed to 0
(LabEs	<- c('es1','es2','es3'))
(LabAs	<- c('as1','as2','as3'))
(LabCs	<- c('cs1','cs2','cs3'))

PatSp		<- c(F,F,F)
StSpa		<- c(0,0,0)
StSpc		<- c(0,0,0)
StSpe		<- c(0,0,0)

# all factor loadings fixed to 1
PatFl		<- c(F,F,F,			
            F,F,F,
            F,F,F)

StFl		<- c(1,0,0,
           0,1,0,
           0,0,1)

LabFl		<- c('l1',NA,NA,
            NA,'l2',NA,
            NA,NA,'l3')

PatPhC	<- c(F,F,TRUE,
            F,F,TRUE,
            F,F,F)

StPhC		<- c(0,0,.1,
            0,0,.1,
            0,0,0)

LabPhC	<- c(NA,NA,'c1on3',
            NA,NA,'c2on3',	
            NA,NA,NA)	 

#______________________________________________________________________________________________________
# Define matrices to hold the Means, SD, correlations
# Use Algebra to generate expected var/cov matrices and Means
# Specify: data objects, Fitfunction, the Model, 
# Run the Model 
#______________________________________________________________________________________________________

Means		<-mxMatrix("Full", 1, ntv, free=c(PatM,PatM), values=c(Stmean,Stmean), labels=c(mLabs,mLabs), name="expMean") 

# Define matrices to specify the loadings of the dependent variables on the latent factors
Load		<-mxMatrix(type="Full",	nrow=nv, ncol=nfact, free=PatFl, values=StFl, labels=LabFl, name="FactL" )
Id2		<-mxMatrix(type="Iden",	nrow=2, ncol=2, free=F, name="I2" )
LoadTw	<-mxAlgebra(I2%x%FactL, name="FactLTw")

# Define the matrix to hold the Single headed Arrows (causal paths) from predictors to AUDITC  
PhCaus	<-mxMatrix(type="Full",	nrow=nfact, ncol=nfact, free=PatPhC, values=StPhC, labels=LabPhC, name="PhC" )

# Define the matrix to hold the A and C and E effects: Specific-fixed to 0 
PathsAs	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpa, labels=LabAs, name="as" )
PathsCs	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpc, labels=LabCs, name="cs" )
PathsEs	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpe, labels=LabEs, name="es" )
covAs		<-mxAlgebra( expression= as %*% t(as), name="As" )
covCs		<-mxAlgebra( expression= cs %*% t(cs), name="Cs" )
covEs		<-mxAlgebra( expression= es %*% t(es), name="Es" )
covPs		<-mxAlgebra( expression= As+Cs+Es, name="Vs" )

# Define the matrices to hold the A and C and E effects: Common 
FactSD	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,T,F,T), values=c(1,.1,0,1,0,1), labels=c('F1sd','cov21','cov31','F2res','cov32','F3res'),name="Fsd" )
PathsAcsp	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,T,F,T), values=c(.8,.1,0,.8,0,.8), labels=c("a_csp1","a_csp21","a_csp31","a_csp2","a_csp32","a_csp3"), name="ac" )
PathsCcsp	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,T,F,T), values=c(.1,.1,0,.1,0,.1), labels=c("c_csp1","c_csp21","c_csp31","c_csp2","c_csp32","c_csp3"), name="cc" )
PathsEcsp	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,T,F,T), values=c(.8,.1,0,.8,0,.8), labels=c("e_csp1","e_csp21","e_csp31","e_csp2","e_csp32","e_csp3"), name="ec" )
covAc		<-mxAlgebra( expression= ac %*% t(ac), name="Ac" )
covCc		<-mxAlgebra( expression= cc %*% t(cc), name="Cc" )
covEc		<-mxAlgebra( expression= ec %*% t(ec), name="Ec" )
covPc		<-mxAlgebra( expression= Ac+Cc+Ec, name="Vc" )

# Generate Covariance of Latent factor model Including Causal Paths between factors
Id4		<-mxMatrix(type="Iden",	nrow=nfact, ncol=nfact, name="I4" )
covFAc	<-mxAlgebra( expression= solve(I4-PhC) %&% Ac, name ="FAc")
covFCc	<-mxAlgebra( expression= solve(I4-PhC) %&% Cc, name ="FCc")
covFEc	<-mxAlgebra( expression= solve(I4-PhC) %&% Ec, name ="FEc")
covFc		<-mxAlgebra( expression= (FAc+FCc+FEc), name="FVc" )

FcovMZ	<-mxAlgebra( expression= (FactLTw  %&% rbind ( cbind(FVc, FAc+FCc), cbind(FAc+FCc, FVc) )) , name="expFCovMZ" )
FcovDZ	<-mxAlgebra( expression= (FactLTw  %&% rbind ( cbind(FVc, .5%x%FAc+FCc), cbind(.5%x%FAc+FCc, FVc) )) , name="expFCovDZ" )

SpcovMZ	<-mxAlgebra( expression= rbind (cbind(Vs, As+Cs), cbind(As+Cs, Vs) ) , name="expSpCovMZ" )
SpcovDZ	<-mxAlgebra( expression= rbind (cbind(Vs, .5%x%As+Cs), cbind(.5%x%As+Cs, Vs) ) , name="expSpCovDZ" )

TOTcovMZ	<-mxAlgebra( expression= expFCovMZ + expSpCovMZ , name="TOTexpCovMZ" )
TOTcovDZ	<-mxAlgebra( expression= expFCovDZ + expSpCovDZ , name="TOTexpCovDZ" )
# *******************************************************************************************************
# Calculator

# Calculate genetic,  environmental, and phenotypic correlations
corA		<- mxAlgebra( expression=solve(sqrt(I4*FAc))%&%FAc, name ="rA" ) 
corC		<- mxAlgebra( expression=solve(sqrt(I4*FCc))%&%FCc, name ="rC" )
corE		<- mxAlgebra( expression=solve(sqrt(I4*FEc))%&%FEc, name ="rE" )
corP		<- mxAlgebra( expression=solve(sqrt(I4*FVc))%&%FVc, name ="rP") 

# Standardize the causal effects
Stcp1on3	<-mxAlgebra( expression= (PhC[3,1]* sqrt(FVc[1,1]))/sqrt(FVc[3,3]) , name="Stand_1on3" )
Stcp2on3	<-mxAlgebra( expression= (PhC[3,2]* sqrt(FVc[2,2]))/sqrt(FVc[3,3]) , name="Stand_2on3" )

# Standardize the Total var/covariances matrices of the observed variables
Id8		<-mxMatrix(type="Iden",	nrow=ntv, ncol=ntv, name="I8" )
Rfactmz	<-mxAlgebra( expression= solve(sqrt(I8*TOTexpCovMZ)) %&% TOTexpCovMZ, name="FactcorMZ" )
Rfactdz	<-mxAlgebra( expression= solve(sqrt(I8*TOTexpCovDZ)) %&% TOTexpCovDZ, name="FactcorDZ" )

# Standardize the Common Effects
stcovAc	<-mxAlgebra( expression= FAc/FVc, name="stAc" )
stcovCc	<-mxAlgebra( expression= FCc/FVc, name="stCc" )
stcovEc	<-mxAlgebra( expression= FEc/FVc, name="stEc" )

# Standardize the Specific Effects
stcovAs	<-mxAlgebra( expression= sqrt(As/( (FactL %&% FVc) +Vs)), name="stAs" )
stcovCs	<-mxAlgebra( expression= sqrt(Cs/( (FactL %&% FVc) +Vs)), name="stCs" )
stcovEs	<-mxAlgebra( expression= sqrt(Es/( (FactL %&% FVc) +Vs)), name="stEs" )

# Standardized Effects of Individual variables from the factors (Variance components) above
stAvar	<-mxAlgebra( expression= (FactL %&% FAc)/( (FactL %&% FVc) +Vs), name="stAvariables" )
stCvar	<-mxAlgebra( expression= (FactL %&% FCc)/( (FactL %&% FVc) +Vs), name="stCvariables" )
stEvar	<-mxAlgebra( expression= (FactL %&% FEc)/( (FactL %&% FVc) +Vs), name="stEvariables" )

# Standardized Factor Loadings
StFL		<-mxAlgebra( expression= sqrt(diag2vec( FactL %&% FVc / TOTexpCovMZ[1:3,1:3])) , name="StandFact" )

#standardize residual variances of AUDITC
stAres3	<-mxAlgebra( expression= (Ac[3,3]/FVc[3,3]) , name="stAres_3" )
stCres3	<-mxAlgebra( expression= (Cc[3,3]/FVc[3,3]) , name="stCres_3" )
stEres3	<-mxAlgebra( expression= (Ec[3,3]/FVc[3,3]) , name="stEres_3" )

# *******************************************************************************************************

# Data objects for Multiple Groups
dataMZ	<- mxData( observed=mzData, type="raw" )
dataDZ	<- mxData( observed=dzData, type="raw" )

# Objective objects for Multiple Groups
objMZ		<- mxExpectationNormal( covariance="TOTexpCovMZ", means="expMean", dimnames=selVars)
objDZ		<- mxExpectationNormal( covariance="TOTexpCovDZ", means="expMean", dimnames=selVars)

fitFunction <- mxFitFunctionML()

# Combine Groups
pars1		<-list(Means,Load,LoadTw,PhCaus,PathsAs,PathsCs,PathsEs,covAs,covCs,covEs,covPs,Id4,Id2,Id8, stAvar, stCvar, stEvar)
pars2		<-list(PathsAcsp,PathsCcsp,PathsEcsp,covAc,covCc,covEc,covPc,covFAc,covFCc,covFEc,covFc,stcovAc,stcovCc,stcovEc, stcovAs, stcovCs, stcovEs,stAres3,stCres3,stEres3,corA,corC,corE)
parsmed	<-list(Stcp1on3, Stcp2on3)
modelMZ	<-mxModel(pars1, pars2, parsmed, FcovMZ, SpcovMZ, TOTcovMZ, dataMZ, objMZ, Rfactmz, fitFunction, StFL, name="MZ" )
modelDZ	<-mxModel(pars1, pars2, FcovDZ, SpcovDZ, TOTcovDZ, dataDZ, objDZ, Rfactdz, fitFunction, name="DZ" )
minus2ll	<-mxAlgebra( expression=MZ.objective + DZ.objective, name="m2LL" )
obj		<-mxFitFunctionAlgebra( "m2LL" )
cistFL	<-mxCI (c ('MZ.StandFact','MZ.Stand_1on3','MZ.Stand_2on3','MZ.rA','MZ.rC','MZ.rE'))
cistFc	<-mxCI (c ('MZ.stAc','MZ.stCc','MZ.stEc') ) 	# standardized var comp from Common feactors	
cistVs	<-mxCI (c ('MZ.stAs','MZ.stCs','MZ.stEs') ) 	# standardized var comp from specific Factors
cistvars	<-mxCI (c ('MZ.stAvariables','MZ.stCvariables','MZ.stEvariables','stAres_3','stCres_3','stEres_3'))
ACE2MsModel	<-mxModel("ace2Ms", pars1, pars2, modelMZ, modelDZ, minus2ll, obj, cistFc, cistFL, cistVs, cistvars) 

# --------------------------------------------------------------------------------------------------------------------------------
# RUN ACEMs Factor Model with phenotypic causal mediation paths by Zygosity

ACE2MsFit	<-mxRun(ACE2MsModel, intervals=T)#intervals=T for CIs
(ACE2MsSumm	<-summary(ACE2MsFit, verbose=F))

# Get some output

mxEval(MZ.Vs, ACE2MsFit)

mxEval(MZ.FAc, ACE2MsFit)
mxEval(MZ.FCc, ACE2MsFit)
mxEval(MZ.FEc, ACE2MsFit)
mxEval(MZ.FVc, ACE2MsFit)
mxEval(MZ.PhC, ACE2MsFit)

mxEval(MZ.stAc, ACE2MsFit)
mxEval(MZ.stCc, ACE2MsFit)
mxEval(MZ.stEc, ACE2MsFit)

mxEval(MZ.Ac, ACE2MsFit)
mxEval(MZ.Cc, ACE2MsFit)
mxEval(MZ.Ec, ACE2MsFit)

mxEval(MZ.stAs, ACE2MsFit)
mxEval(MZ.stCs, ACE2MsFit)
mxEval(MZ.stEs, ACE2MsFit)

mxEval(MZ.stAvariables, ACE2MsFit)
mxEval(MZ.stCvariables, ACE2MsFit)
mxEval(MZ.stEvariables, ACE2MsFit)

mxEval(MZ.Stand_1on3, ACE2MsFit)
mxEval(MZ.Stand_2on3, ACE2MsFit)

#------------------------------------------------------------
# Compare models
#------------------------------------------------------------
# **************************************************
(mxCompare(ACE2Fit, ACE2MsFit))

# **************************************************

# **************************************************
#sex differences based on ACE2MsModel
#run the code for sex differences at the beginning
nv		<- 3				# number of variables for a twin = 1 in Univariate
ntv		<- 2*nv			#number of variables for a pair = 2* 1 for Univariate
nlower	<- nv*(nv+1)/2 		#number of free elements in a lower matrix nv*nv
ncor		<- (nv*(nv+1)/2)-nv	#number of free elements in a correlation matrix nv*nv
nfact		<- 3				# number of Latent Factors for Causal Model per twin
nfact2	<- 2*nfact			# number of Latent Factors for Causal Model per twin pair
nfcor		<- (nfact*(nfact+1)/2)-nfact	# number of free elements in a correlation matrix nfact*nfcat
Groups	<- c("mz", "dz")
Vars		<- c("IEA","SCA","AUDITC")
selVars	<- c("IEA1","SCA1","AUDITC1",
             "IEA2","SCA2","AUDITC2")

mzmData	<- subset(TWINdata2, zyg1%in%c(1)|zyg2%in%c(1) , selVars)#Crosscheck that these are correct
mzfData	<- subset(TWINdata2, zyg1%in%c(3)|zyg2%in%c(3) , selVars)
dzmData	<- subset(TWINdata2, zyg1%in%c(2)|zyg2%in%c(2) , selVars)
dzfData	<- subset(TWINdata2, zyg1%in%c(4)|zyg2%in%c(4) , selVars)
dzoData	<- subset(TWINdata2, zyg1%in%c(5)|zyg2%in%c(5) , selVars)#Crosscheck that males have been selected to be twin 1

psych::describe(mzmData)
psych::describe(mzfData)
psych::describe(dzmData)
psych::describe(dzfData)
psych::describe(dzoData)


# CREATE LABELS & START VALUES as objects(to ease specification in the body of the model)
(mLabsm	<- paste("mm",1:nv,sep=""))
(mLabsf	<- paste("mf",1:nv,sep=""))
(Stmeanm	<-colMeans(mzmData[,1:nv],na.rm=TRUE))
(Stmeanf	<-colMeans(mzfData[,1:nv],na.rm=TRUE))
#(Stsdm 	<-sapply(mzmData[,1:nv],sd, na.rm=TRUE))
#(Stsdf 	<-sapply(mzfData[,1:nv],sd, na.rm=TRUE))
(PatM		<- c(TRUE,TRUE,TRUE))

# Create Labels for Diagonal Matrices-fixed to 0
(LabEsm	<- c('es1m','es2m','es3m'))
(LabAsm	<- c('as1m','as2m','as3m'))
(LabCsm	<- c('cs1m','cs2m','cs3m'))

(LabEsf	<- c('es1f','es2f','es3f'))
(LabAsf	<- c('as1f','as2f','as3f'))
(LabCsf	<- c('cs1f','cs2f','cs3f'))

PatSp		<- c(F,F,F)
StSpam	<- c(0,0,0)
StSpcm	<- c(0,0,0)
StSpem	<- c(0,0,0)

StSpaf	<- c(0,0,0)
StSpcf	<- c(0,0,0)
StSpef	<- c(0,0,0)

# all factor loadings fixed to 1
PatFl		<- c(F,F,F,			
            F,F,F,
            F,F,F)

StFlm		<- c(1,0,0,
            0,1,0,
            0,0,1)

StFlf		<- c(1,0,0,
            0,1,0,
            0,0,1)

LabFlm		<- c('l1m',NA,NA,
             NA,'l2m',NA,
             NA,NA,'l3m')

LabFlf		<- c('l1f',NA,NA,
             NA,'l2f',NA,
             NA,NA,'l3f')

PatPhC	<- c(F,F,TRUE,
            F,F,TRUE,
            F,F,F)

StPhCm		<- c(0,0,.1,
             0,0,.1,
             0,0,0)

StPhCf		<- c(0,0,.1,
             0,0,.1,
             0,0,0)

LabPhCm	<- c(NA,NA,'c1on3m',
             NA,NA,'c2on3m',	
             NA,NA,NA)	 

LabPhCf	<- c(NA,NA,'c1on3f',
             NA,NA,'c2on3f',	
             NA,NA,NA)	
#______________________________________________________________________________________________________
# Define matrices to hold the Means, SD, correlations
# Use Algebra to generate expected var/cov matrices and Means
# Specify: data objects, Fitfunction, the Model, 
# Run the Model 
#______________________________________________________________________________________________________

Meansm	<-mxMatrix("Full", 1, ntv, free=c(PatM,PatM), values=c(Stmeanm,Stmeanm), labels=c(mLabsm,mLabsm), name="expMeanm") 
Meansf	<-mxMatrix("Full", 1, ntv, free=c(PatM,PatM), values=c(Stmeanf,Stmeanf), labels=c(mLabsf,mLabsf), name="expMeanf") 
Meanso	<-mxMatrix("Full", 1, ntv, free=c(PatM,PatM), values=c(Stmeanm,Stmeanf), labels=c(mLabsm,mLabsf), name="expMeano") 

# Define matrices to specify the loadings of the dependent variables on the latent factors
Loadm		<-mxMatrix(type="Full",	nrow=nv, ncol=nfact, free=PatFl, values=StFlm, labels=LabFlm, name="FactLm" )
Loadf		<-mxMatrix(type="Full",	nrow=nv, ncol=nfact, free=PatFl, values=StFlf, labels=LabFlf, name="FactLf" )
Id2		<-mxMatrix(type="Iden",	nrow=2, ncol=2, free=F, name="I2" )
Ze2		<-mxMatrix(type="Zero",	nrow=nv, ncol=nfact, free=F, name="Z2" )

LoadTwm	<-mxAlgebra(I2%x%FactLm, name="FactLTwm")
LoadTwf	<-mxAlgebra(I2%x%FactLf, name="FactLTwf")
LoadTwo	<-mxAlgebra(expression= rbind (cbind(FactLm, Z2), cbind(Z2, FactLf) ), name="FactLTwo")

# Define the matrix to hold the Single headed Arrows (causal paths) from predictors to AUDITC  
PhCausm	<-mxMatrix(type="Full",	nrow=nfact, ncol=nfact, free=PatPhC, values=StPhCm, labels=LabPhCm, name="PhCm" )
PhCausf	<-mxMatrix(type="Full",	nrow=nfact, ncol=nfact, free=PatPhC, values=StPhCf, labels=LabPhCf, name="PhCf" )

# Define the matrix to hold the A and C and E effects: Specific-fixed to 0 
PathsAsm	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpam, labels=LabAsm, name="asm" )
PathsCsm	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpcm, labels=LabCsm, name="csm" )
PathsEsm	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpem, labels=LabEsm, name="esm" )

PathsAsf	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpaf, labels=LabAsf, name="asf" )
PathsCsf	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpcf, labels=LabCsf, name="csf" )
PathsEsf	<-mxMatrix(type="Diag",	nrow=nv, ncol=nv, free=PatSp, values=StSpef, labels=LabEsf, name="esf" )


covAsm	<-mxAlgebra( expression= asm %*% t(asm), name="Asm" )
covCsm	<-mxAlgebra( expression= csm %*% t(csm), name="Csm" )
covEsm	<-mxAlgebra( expression= esm %*% t(esm), name="Esm" )

covAsf	<-mxAlgebra( expression= asf %*% t(asf), name="Asf" )
covCsf	<-mxAlgebra( expression= csf %*% t(csf), name="Csf" )
covEsf	<-mxAlgebra( expression= esf %*% t(esf), name="Esf" )

covAsmf	<-mxAlgebra( expression= asm %*% t(asf), name="Asmf" )
covCsmf	<-mxAlgebra( expression= csm %*% t(csf), name="Csmf" )
covAsfm	<-mxAlgebra( expression= asf %*% t(asm), name="Asfm" )
covCsfm	<-mxAlgebra( expression= csf %*% t(csm), name="Csfm" )

covPsm	<-mxAlgebra( expression= Asm+Csm+Esm, name="Vsm" )
covPsf	<-mxAlgebra( expression= Asf+Csf+Esf, name="Vsf" )

# Define the matrices to hold the A and C and E effects: Common 
PathsAcspm	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,T,F,T), values=c(.8,.1,0,.8,0,.8), labels=c("a_csp1m","a_csp21m","a_csp31m","a_csp2m","a_csp32m","a_csp3m"), name="acm" )
PathsCcspm	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,T,F,T), values=c(.1,.1,0,.1,0,.1), labels=c("c_csp1m","c_csp21m","c_csp31m","c_csp2m","c_csp32m","c_csp3m"), name="ccm" )
PathsEcspm	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,T,F,T), values=c(.8,.1,0,.8,0,.8), labels=c("e_csp1m","e_csp21m","e_csp31m","e_csp2m","e_csp32m","e_csp3m"), name="ecm" )

PathsAcspf	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,T,F,T), values=c(.8,.1,0,.8,0,.8), labels=c("a_csp1f","a_csp21f","a_csp31f","a_csp2f","a_csp32f","a_csp3f"), name="acf" )
PathsCcspf	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,T,F,T), values=c(.1,.1,0,.1,0,.1), labels=c("c_csp1f","c_csp21f","c_csp31f","c_csp2f","c_csp32f","c_csp3f"), name="ccf" )
PathsEcspf	<-mxMatrix(type="Lower", nrow=nfact, ncol=nfact, free=c(T,T,F,T,F,T), values=c(.8,.1,0,.8,0,.8), labels=c("e_csp1f","e_csp21f","e_csp31f","e_csp2f","e_csp32f","e_csp3f"), name="ecf" )

covAcm	<-mxAlgebra( expression= acm %*% t(acm), name="Acm" )
covCcm	<-mxAlgebra( expression= ccm %*% t(ccm), name="Ccm" )
covEcm	<-mxAlgebra( expression= ecm %*% t(ecm), name="Ecm" )

covAcf	<-mxAlgebra( expression= acf %*% t(acf), name="Acf" )
covCcf	<-mxAlgebra( expression= ccf %*% t(ccf), name="Ccf" )
covEcf	<-mxAlgebra( expression= ecf %*% t(ecf), name="Ecf" )

covAcmf	<-mxAlgebra( expression= acm %*% t(acf), name="Acmf" )
covCcmf	<-mxAlgebra( expression= ccm %*% t(ccf), name="Ccmf" )
covAcfm	<-mxAlgebra( expression= acf %*% t(acm), name="Acfm" )
covCcfm	<-mxAlgebra( expression= ccf %*% t(ccm), name="Ccfm" )

covPcm	<-mxAlgebra( expression= Acm+Ccm+Ecm, name="Vcm" )
covPcf	<-mxAlgebra( expression= Acf+Ccf+Ecf, name="Vcf" )

# Generate Covariance of Latent factor model Including Causal Paths between factors
Id4		<-mxMatrix(type="Iden",	nrow=nfact, ncol=nfact, name="I4" )
covFAcm	<-mxAlgebra( expression= solve(I4-PhCm) %&% Acm, name ="FAcm")
covFCcm	<-mxAlgebra( expression= solve(I4-PhCm) %&% Ccm, name ="FCcm")
covFEcm	<-mxAlgebra( expression= solve(I4-PhCm) %&% Ecm, name ="FEcm")

covFAcf	<-mxAlgebra( expression= solve(I4-PhCf) %&% Acf, name ="FAcf")
covFCcf	<-mxAlgebra( expression= solve(I4-PhCf) %&% Ccf, name ="FCcf")
covFEcf	<-mxAlgebra( expression= solve(I4-PhCf) %&% Ecf, name ="FEcf")

covFAcmf	<-mxAlgebra( expression= solve(I4-PhCm) %*% Acmf %*% t(solve(I4-PhCf)), name ="FAcmf")
covFCcmf	<-mxAlgebra( expression= solve(I4-PhCm) %*% Ccmf %*% t(solve(I4-PhCf)), name ="FCcmf")
covFAcfm	<-mxAlgebra( expression= solve(I4-PhCf) %*% Acfm %*% t(solve(I4-PhCm)), name ="FAcfm")
covFCcfm	<-mxAlgebra( expression= solve(I4-PhCf) %*% Ccfm %*% t(solve(I4-PhCm)), name ="FCcfm")

covFcm	<-mxAlgebra( expression= (FAcm+FCcm+FEcm), name="FVcm" )
covFcf	<-mxAlgebra( expression= (FAcf+FCcf+FEcf), name="FVcf" )

FcovMZM	<-mxAlgebra( expression= (FactLTwm  %&% rbind ( cbind(FVcm, FAcm+FCcm), cbind(FAcm+FCcm, FVcm) )) , name="expFCovMZM" )
FcovMZF	<-mxAlgebra( expression= (FactLTwf  %&% rbind ( cbind(FVcf, FAcf+FCcf), cbind(FAcf+FCcf, FVcf) )) , name="expFCovMZF" )
FcovDZM	<-mxAlgebra( expression= (FactLTwm  %&% rbind ( cbind(FVcm, .5%x%FAcm+FCcm), cbind(.5%x%FAcm+FCcm, FVcm) )) , name="expFCovDZM" )
FcovDZF	<-mxAlgebra( expression= (FactLTwf  %&% rbind ( cbind(FVcf, .5%x%FAcf+FCcf), cbind(.5%x%FAcf+FCcf, FVcf) )) , name="expFCovDZF" )
FcovDZO	<-mxAlgebra( expression= (FactLTwo  %&% rbind ( cbind(FVcm, .5%x%FAcmf+FCcmf), cbind(.5%x%FAcfm+FCcfm, FVcf) )) , name="expFCovDZO" )

SpcovMZM	<-mxAlgebra( expression= rbind (cbind(Vsm, Asm+Csm), cbind(Asm+Csm, Vsm) ) , name="expSpCovMZM" )
SpcovMZF	<-mxAlgebra( expression= rbind (cbind(Vsf, Asf+Csf), cbind(Asf+Csf, Vsf) ) , name="expSpCovMZF" )
SpcovDZM	<-mxAlgebra( expression= rbind (cbind(Vsm, .5%x%Asm+Csm), cbind(.5%x%Asm+Csm, Vsm) ) , name="expSpCovDZM" )
SpcovDZF	<-mxAlgebra( expression= rbind (cbind(Vsf, .5%x%Asf+Csf), cbind(.5%x%Asf+Csf, Vsf) ) , name="expSpCovDZF" )
SpcovDZO	<-mxAlgebra( expression= rbind (cbind(Vsm, .5%x%Asmf+Csmf), cbind(.5%x%Asfm+Csfm, Vsf) ) , name="expSpCovDZO" )

TOTcovMZM	<-mxAlgebra( expression= expFCovMZM + expSpCovMZM , name="TOTexpCovMZM" )
TOTcovMZF	<-mxAlgebra( expression= expFCovMZF + expSpCovMZF , name="TOTexpCovMZF" )
TOTcovDZM	<-mxAlgebra( expression= expFCovDZM + expSpCovDZM , name="TOTexpCovDZM" )
TOTcovDZF	<-mxAlgebra( expression= expFCovDZF + expSpCovDZF , name="TOTexpCovDZF" )
TOTcovDZO	<-mxAlgebra( expression= expFCovDZO + expSpCovDZO , name="TOTexpCovDZO" )

# *******************************************************************************************************
# Calculator

# Standardize the causal effects
Stcp1on3m	<-mxAlgebra( expression= (PhCm[3,1]* sqrt(FVcm[1,1]))/sqrt(FVcm[3,3]) , name="Stand_1on3m" )
Stcp2on3m	<-mxAlgebra( expression= (PhCm[3,2]* sqrt(FVcm[2,2]))/sqrt(FVcm[3,3]) , name="Stand_2on3m" )

Stcp1on3f	<-mxAlgebra( expression= (PhCf[3,1]* sqrt(FVcf[1,1]))/sqrt(FVcf[3,3]) , name="Stand_1on3f" )
Stcp2on3f	<-mxAlgebra( expression= (PhCf[3,2]* sqrt(FVcf[2,2]))/sqrt(FVcf[3,3]) , name="Stand_2on3f" )

# Standardize the Total var/covariances matrices of the observed variables
Id8		<-mxMatrix(type="Iden",	nrow=ntv, ncol=ntv, name="I8" )
Rfactmzm	<-mxAlgebra( expression= solve(sqrt(I8*TOTexpCovMZM)) %&% TOTexpCovMZM, name="FactcorMZM" )
Rfactmzf	<-mxAlgebra( expression= solve(sqrt(I8*TOTexpCovMZF)) %&% TOTexpCovMZF, name="FactcorMZF" )
Rfactdzm	<-mxAlgebra( expression= solve(sqrt(I8*TOTexpCovDZM)) %&% TOTexpCovDZM, name="FactcorDZM" )
Rfactdzf	<-mxAlgebra( expression= solve(sqrt(I8*TOTexpCovDZF)) %&% TOTexpCovDZF, name="FactcorDZF" )
Rfactdzo	<-mxAlgebra( expression= solve(sqrt(I8*TOTexpCovDZO)) %&% TOTexpCovDZO, name="FactcorDZO" )

# Standardize the Common Effects
stcovAcm	<-mxAlgebra( expression= FAcm/FVcm, name="stAcm" )
stcovCcm	<-mxAlgebra( expression= FCcm/FVcm, name="stCcm" )
stcovEcm	<-mxAlgebra( expression= FEcm/FVcm, name="stEcm" )

stcovAcf	<-mxAlgebra( expression= FAcf/FVcf, name="stAcf" )
stcovCcf	<-mxAlgebra( expression= FCcf/FVcf, name="stCcf" )
stcovEcf	<-mxAlgebra( expression= FEcf/FVcf, name="stEcf" )

# Standardize the Specific Effects
stcovAsm	<-mxAlgebra( expression= sqrt(Asm/( (FactLm %&% FVcm) +Vsm)), name="stAsm" )
stcovCsm	<-mxAlgebra( expression= sqrt(Csm/( (FactLm %&% FVcm) +Vsm)), name="stCsm" )
stcovEsm	<-mxAlgebra( expression= sqrt(Esm/( (FactLm %&% FVcm) +Vsm)), name="stEsm" )

stcovAsf	<-mxAlgebra( expression= sqrt(Asf/( (FactLf %&% FVcf) +Vsf)), name="stAsf" )
stcovCsf	<-mxAlgebra( expression= sqrt(Csf/( (FactLf %&% FVcf) +Vsf)), name="stCsf" )
stcovEsf	<-mxAlgebra( expression= sqrt(Esf/( (FactLf %&% FVcf) +Vsf)), name="stEsf" )

# Standardized Effects of Individual variables from the factors (Variance components) above
stAvarm	<-mxAlgebra( expression= (FactLm %&% FAcm)/( (FactLm %&% FVcm) +Vsm), name="stAvariablesm" )
stCvarm	<-mxAlgebra( expression= (FactLm %&% FCcm)/( (FactLm %&% FVcm) +Vsm), name="stCvariablesm" )
stEvarm	<-mxAlgebra( expression= (FactLm %&% FEcm)/( (FactLm %&% FVcm) +Vsm), name="stEvariablesm" )

stAvarf	<-mxAlgebra( expression= (FactLf %&% FAcf)/( (FactLf %&% FVcf) +Vsf), name="stAvariablesf" )
stCvarf	<-mxAlgebra( expression= (FactLf %&% FCcf)/( (FactLf %&% FVcf) +Vsf), name="stCvariablesf" )
stEvarf	<-mxAlgebra( expression= (FactLf %&% FEcf)/( (FactLf %&% FVcf) +Vsf), name="stEvariablesf" )

# Standardized Factor Loadings
StFLm		<-mxAlgebra( expression= sqrt(diag2vec( FactLm %&% FVcm / TOTexpCovMZM[1:3,1:3])) , name="StandFactm" )
StFLf		<-mxAlgebra( expression= sqrt(diag2vec( FactLf %&% FVcf / TOTexpCovMZF[1:3,1:3])) , name="StandFactf" )

# *******************************************************************************************************

# Data objects for Multiple Groups
dataMZM	<- mxData( observed=mzmData, type="raw" )
dataMZF	<- mxData( observed=mzfData, type="raw" )
dataDZM	<- mxData( observed=dzmData, type="raw" )
dataDZF	<- mxData( observed=dzfData, type="raw" )
dataDZO	<- mxData( observed=dzoData, type="raw" )

# Objective objects for Multiple Groups
objMZM	<- mxExpectationNormal( covariance="TOTexpCovMZM", means="expMeanm", dimnames=selVars)
objMZF	<- mxExpectationNormal( covariance="TOTexpCovMZF", means="expMeanf", dimnames=selVars)
objDZM	<- mxExpectationNormal( covariance="TOTexpCovDZM", means="expMeanm", dimnames=selVars)
objDZF	<- mxExpectationNormal( covariance="TOTexpCovDZF", means="expMeanf", dimnames=selVars)
objDZO	<- mxExpectationNormal( covariance="TOTexpCovDZO", means="expMeano", dimnames=selVars)

fitFunction <- mxFitFunctionML()

# Combine Groups
pars1m	<-list(Meansm,Loadm,LoadTwm,PhCausm,PathsAsm,PathsCsm,PathsEsm,covAsm,covCsm,covEsm,covPsm,Id4,Id2,Id8, stAvarm, stCvarm, stEvarm)
pars1f	<-list(Meansf,Loadf,LoadTwf,PhCausf,PathsAsf,PathsCsf,PathsEsf,covAsf,covCsf,covEsf,covPsf,Id4,Id2,Id8, stAvarf, stCvarf, stEvarf)
pars2m	<-list(PathsAcspm,PathsCcspm,PathsEcspm,covAcm,covCcm,covEcm,covPcm,covFAcm,covFCcm,covFEcm,covFcm,stcovAcm,stcovCcm,stcovEcm, stcovAsm, stcovCsm, stcovEsm)
pars2f	<-list(PathsAcspf,PathsCcspf,PathsEcspf,covAcf,covCcf,covEcf,covPcf,covFAcf,covFCcf,covFEcf,covFcf,stcovAcf,stcovCcf,stcovEcf, stcovAsf, stcovCsf, stcovEsf)
parso		<-list(Meanso,Ze2,LoadTwo,covAsmf,covCsmf,covAsfm,covCsfm,covAcmf,covCcmf,covAcfm,covCcfm,covFAcmf,covFCcmf,covFAcfm,covFCcfm)
parsmedm	<-list(Stcp1on3m, Stcp2on3m)
parsmedf	<-list(Stcp1on3f, Stcp2on3f)
modelMZM	<-mxModel(pars1m, pars2m, parsmedm, FcovMZM, SpcovMZM, TOTcovMZM, dataMZM, objMZM, Rfactmzm, fitFunction, StFLm, name="MZM" )
modelMZF	<-mxModel(pars1f, pars2f, parsmedf, FcovMZF, SpcovMZF, TOTcovMZF, dataMZF, objMZF, Rfactmzf, fitFunction, StFLf, name="MZF" )
modelDZM	<-mxModel(pars1m, pars2m, FcovDZM, SpcovDZM, TOTcovDZM, dataDZM, objDZM, Rfactdzm, fitFunction, name="DZM" )
modelDZF	<-mxModel(pars1f, pars2f, FcovDZF, SpcovDZF, TOTcovDZF, dataDZF, objDZF, Rfactdzf, fitFunction, name="DZF" )
modelDZO	<-mxModel(pars1m, pars2m, pars1f, pars2f, parso, FcovDZO, SpcovDZO, TOTcovDZO, dataDZO, objDZO, Rfactdzo, fitFunction, name="DZO" )
minus2ll	<-mxAlgebra( expression=MZM.objective + MZF.objective + DZM.objective + DZF.objective + DZO.objective, name="m2LL" )
obj		<-mxFitFunctionAlgebra( "m2LL" )

cistFLm	<-mxCI (c ('MZM.StandFactm','MZM.Stand_1on3m','MZM.Stand_2on3m'))
cistFLf	<-mxCI (c ('MZF.StandFactf','MZF.Stand_1on3f','MZF.Stand_2on3f'))

cistFcm	<-mxCI (c ('MZM.stAcm','MZM.stCcm','MZM.stEcm') ) 	# standardized var comp from Common feactors	
cistFcf	<-mxCI (c ('MZF.stAcf','MZF.stCcf','MZF.stEcf') ) 	# standardized var comp from Common feactors	

cistVsm	<-mxCI (c ('MZM.stAsm','MZM.stCsm','MZM.stEsm') ) 	# standardized var comp from specific Factors
cistVsf	<-mxCI (c ('MZF.stAsf','MZF.stCsf','MZF.stEsf') ) 	# standardized var comp from specific Factors

cistvarsm	<-mxCI (c ('MZM.stAvariablesm','MZM.stCvariablesm','MZM.stEvariablesm'))
cistvarsf	<-mxCI (c ('MZF.stAvariablesf','MZF.stCvariablesf','MZF.stEvariablesf'))

HetACE2MsModel	<-mxModel("Hetace2Ms", pars1m, pars2m, pars1f, pars2f, parso, modelMZM, modelMZF, modelDZM, modelDZF, modelDZO, minus2ll, obj, cistFcm, cistFcf, cistFLm, cistFLf, cistVsm, cistVsf, cistvarsm, cistvarsf) 

# --------------------------------------------------------------------------------------------------------------------------------
# RUN HetACEMs Factor Model with phenotypic causal paths by Zygosity

HetACE2MsFit	<-mxRun(HetACE2MsModel, intervals=T)#intervals=T for CIs
(HetACE2MsSumm	<-summary(HetACE2MsFit, verbose=F))

# Get some output

mxEval(MZM.Vsm, HetACE2MsFit)
mxEval(MZF.Vsf, HetACE2MsFit)

mxEval(MZM.FAcm, HetACE2MsFit)
mxEval(MZM.FCcm, HetACE2MsFit)
mxEval(MZM.FEcm, HetACE2MsFit)
mxEval(MZM.FVcm, HetACE2MsFit)
mxEval(MZM.PhCm, HetACE2MsFit)

mxEval(MZF.FAcf, HetACE2MsFit)
mxEval(MZF.FCcf, HetACE2MsFit)
mxEval(MZF.FEcf, HetACE2MsFit)
mxEval(MZF.FVcf, HetACE2MsFit)
mxEval(MZF.PhCf, HetACE2MsFit)

mxEval(MZM.stAcm, HetACE2MsFit)
mxEval(MZM.stCcm, HetACE2MsFit)
mxEval(MZM.stEcm, HetACE2MsFit)

mxEval(MZF.stAcf, HetACE2MsFit)
mxEval(MZF.stCcf, HetACE2MsFit)
mxEval(MZF.stEcf, HetACE2MsFit)

mxEval(MZM.Acm, HetACE2MsFit)
mxEval(MZM.Ccm, HetACE2MsFit)
mxEval(MZM.Ecm, HetACE2MsFit)

mxEval(MZF.Acf, HetACE2MsFit)
mxEval(MZF.Ccf, HetACE2MsFit)
mxEval(MZF.Ecf, HetACE2MsFit)

mxEval(MZM.stAsm, HetACE2MsFit)
mxEval(MZM.stCsm, HetACE2MsFit)
mxEval(MZM.stEsm, HetACE2MsFit)

mxEval(MZF.stAsf, HetACE2MsFit)
mxEval(MZF.stCsf, HetACE2MsFit)
mxEval(MZF.stEsf, HetACE2MsFit)

mxEval(MZM.stAvariablesm, HetACE2MsFit)
mxEval(MZM.stCvariablesm, HetACE2MsFit)
mxEval(MZM.stEvariablesm, HetACE2MsFit)

mxEval(MZF.stAvariablesf, HetACE2MsFit)
mxEval(MZF.stCvariablesf, HetACE2MsFit)
mxEval(MZF.stEvariablesf, HetACE2MsFit)

mxEval(MZM.Stand_1on3m, HetACE2MsFit)
mxEval(MZM.Stand_2on3m, HetACE2MsFit)

mxEval(MZF.Stand_1on3f, HetACE2MsFit)
mxEval(MZF.Stand_2on3f, HetACE2MsFit)

#*****************************************************************************************************
# Test for sex differences in the causal paths
# Constrain the causal paths to be equal and compare models
#*****************************************************************************************************

HomACE2MsModel	<-mxModel(HetACE2MsFit, name="HomACE2Ms")
HomACE2MsModel	<-omxSetParameters(HomACE2MsModel, labels= c('c1on3m','c1on3f'), newlabels=  c('c1on3f'),free=T,values=.1)
HomACE2MsModel	<-omxSetParameters(HomACE2MsModel, labels= c('c2on3m','c2on3f'), newlabels=  c('c2on3f'),free=T,values=.1)
HomACE2MsFit		<-mxRun(HomACE2MsModel, intervals=T)
(HomACE2MsSumm	<-summary(HomACE2MsFit, verbose = F))

HomACE2MsModel2	<-mxModel(HetACE2MsFit, name="HomACE2Ms")
HomACE2MsModel2	<-omxSetParameters(HomACE2MsModel2, labels= c('c2on3m','c2on3f'), newlabels=  c('c2on3f'),free=T,values=.1)
HomACE2MsFit2		<-mxRun(HomACE2MsModel2, intervals=T)
(HomACE2MsSumm2	<-summary(HomACE2MsFit2, verbose = F))

mxCompare(HetACE2MsFit, subs<-list(HomACE2MsFit,HomACE2MsFit2))