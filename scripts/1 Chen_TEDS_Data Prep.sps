* Encoding: UTF-8.

*********
*Tong Chen
*code for Chen, T., Oginni, O. A., Hannigan, L. J., Eley, T. C., Maggs, J. L., Linden-Carmichael, A. N., & Neiderhiser, J. M. 
*Developmental trajectories of child and adolescent emotional problems: Associations with early adult alcohol use behaviors
*use data 538 Tong Chen SDQ and alcohol version 2 Oct2022_analysis.sav
*********.

*compute SDQ emotion scale 4 years with additional item "shy or timid".
COMPUTE emo41=5 * MEAN.3(dbh101, dbh191, dbh241, dbh311, dbh441).
VARIABLE LABELS  emo41 'SDQ Emotion scale with item 44 (4 Year), 0-8'.
EXECUTE.

COMPUTE emo42=5 * MEAN.3(dbh102, dbh192, dbh242, dbh312, dbh442).
VARIABLE LABELS  emo42 'SDQ Emotion scale with item 44 (4 Year), 0-8'.
EXECUTE.

*select data to be used.
USE ALL.
COMPUTE filter_$=(((MISSING (emo41) = 0) | (MISSING (gpsanxt1) = 0) | (MISSING (ipsanxt1) = 0) | (MISSING (icsanxt1) = 0) | (MISSING (lcsdqemot1) = 0) | 
    (MISSING (pcbhsdqemot1) = 0) | (MISSING (dsdcont1) = 0) | (MISSING (gpscont1) = 0) | (MISSING (ipscont1) = 0) | (MISSING (icscont1) = 0) | (MISSING (lcsdqcont1) = 0) | 
    (MISSING (pcbhsdqcont1) = 0) | (MISSING (u2calcoaudit1) = 0)) & (exclude1 = 0)).
VARIABLE LABELS filter_$ 'sample selection'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

*calculate mean child age at each wave for selected cases.
DESCRIPTIVES VARIABLES=drepage1 gpbage icpage lcqage1 pcbhage1
  /STATISTICS=MEAN STDDEV MIN MAX.

*recode covariates.
*living with parent or not.
RECODE u1clivs11 (1=0) (2=0) (3=0) (4=0) (5=1) (ELSE=Copy) INTO livs21.
VARIABLE LABELS  livs21 'living with parent or not'.
EXECUTE.
VALUE LABELS livs21 0 'No' 1 'Yes'.

*relationship status.
RECODE u1crelst1 (1=1) (2=1) (3=0) (4=0) (5=0) (6=1) (ELSE=Copy) INTO rel21.
VARIABLE LABELS  rel21 'relationship status'.
EXECUTE.
VALUE LABELS rel21 0 'In serious relationship' 1 'Single or similar status'.

*employment status: studying, working, unemployed.
RECODE u1cstatus1 (1=0) (2=1) (3=1) (4=0) (5=2) (6=1) (ELSE=Copy) INTO status21.
VARIABLE LABELS  status21 'employment status'.
EXECUTE.
VALUE LABELS status21 0 'studying/gap' 1 'Working' 2 'Unemployed'.

RECODE status21 (0=0) (1=1) (2=0) INTO status21D1.
EXECUTE.

RECODE status21 (0=0) (1=0) (2=1) INTO status21D2.
EXECUTE.

*regress out exact age at 22 on AUDIT.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT u2calcoaudit1
  /METHOD=ENTER u2cage1
  /SAVE ZRESID.
RENAME VARIABLES (ZRE_1=u2calcoauditrage1).

*compute AUDIT-consumption and AUDIT-problem scores.

* Recode number of units in Q5 to categories (0-4 scale).
* using the ranges in the published AUDIT scale.
RECODE u2calco051 
 (0 THRU 2=0) (2.1 THRU 4=1) (4.1 THRU 6=2) (6.1 THRU 9.9=3) (10 THRU HIGHEST=4)
INTO u2calco05un1.

RECODE u2calco052 
 (0 THRU 2=0) (2.1 THRU 4=1) (4.1 THRU 6=2) (6.1 THRU 9.9=3) (10 THRU HIGHEST=4)
INTO u2calco05un2.
EXECUTE.

* Now create a total AUDIT-Consumption score from items 4-6, all coded 0-4, including recoded item 5.
COMPUTE u2calcoauditc1 = 3 * MEAN.2(u2calco041, u2calco05un1, u2calco061).
EXECUTE.

COMPUTE u2calcoauditc2 = 3 * MEAN.2(u2calco042, u2calco05un2, u2calco062).
EXECUTE.

* Now create a total AUDIT-Problem score from items 7-13, all coded 0-4.
COMPUTE u2calcoauditp1 = 6 * MEAN.3(u2calco071, u2calco081, u2calco091, u2calco101, u2calco111, u2calco121, u2calco131).
EXECUTE.

COMPUTE u2calcoauditp2 = 6 * MEAN.3(u2calco072, u2calco082, u2calco092, u2calco102, u2calco112, u2calco122, u2calco132).
EXECUTE.

* Item 1 (ever had a drink) is a screening question: if 'no', other items are missing.
* The published AUDIT scale does not have a screening question.
* so assume scale value should be 0 if item 1 response is no.
IF (u2calco011 = 0) u2calcoauditc1 = 0.
EXECUTE.
IF (u2calco012 = 0) u2calcoauditc2 = 0.
EXECUTE.
IF (u2calco011 = 0) u2calcoauditp1 = 0.
EXECUTE.
IF (u2calco012 = 0) u2calcoauditp2 = 0.
EXECUTE.

*regress out exact age at 22 on AUDIT-Consumption and AUDIT-Problem.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT u2calcoauditc1
  /METHOD=ENTER u2cage1
  /SAVE ZRESID.
RENAME VARIABLES (ZRE_1=u2calcoauditcrage1).

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT u2calcoauditp1
  /METHOD=ENTER u2cage1
  /SAVE ZRESID.
RENAME VARIABLES (ZRE_1=u2calcoauditprage1).

*log transform the AUDIT-problem scale because of skewness.
COMPUTE u2calcoauditpragelg1=LG10(u2calcoauditprage1+1).
EXECUTE.
