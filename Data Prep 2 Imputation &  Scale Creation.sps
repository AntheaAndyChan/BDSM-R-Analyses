﻿* Encoding: UTF-8.

*Create dataset without subscale data.
DATASET ACTIVATE DataSet1.
SAVE OUTFILE='C:\Users\anthe\Dropbox\Important Stuff\Old Projects\BDSM & Consent\BDSM R Analyses\Kink_demos.sav'
   /KEEP id to GROUPS.


*Analyze Patterns of Missing Values: only looking at scale items.
MULTIPLE IMPUTATION  ASSERT_1 ASSERT_2 ASSERT_3 ASSERT_4 ASSERT_5 ASSERT_6 ASSERT_7
ASSERT_8 ASSERT_9 ASSERT_10 ASSERT_11 ASSERT_12 ASSERT_13 ASSERT_14 ASSERT_15 ASSERT_16 ASSERT_17 ASSERT_18 ASSERT_19 ASSERT_20 ASSERT_21 ASSERT_22
ASSERT_23 ASSERT_24 ASSERT_25 ASSUME_1 ASSUME_2 ASSUME_3 
    ASSUME_4 ASSUME_5 ASSUME_6 ASSUME_7 POS_1 POS_2 POS_3 POS_4 POS_5 POS_6 POS_7 POS_8 POS_9 PBC_1 
    PBC_2 PBC_3 PBC_4 PBC_5 PBC_6 PBC_7 PBC_8 PBC_9 RLN_1 RLN_2 RLN_3 RLN_4 RLN_5 IBA_1 IBA_2 IBA_3 
    IBA_4 IBA_5 IBA_6 AWARE_1 AWARE_2 AWARE_3 AWARE_4 RCUS_1 RCUS_2 RCUS_3 RCUS_4 RCUS_5 RCUS_6 RCUS_7 
    RCUS_8 RCUS_9 RCUS_10 RCUS_11 RCUS_12 RCUS_13 RCUS_14 RCUS_15 RCUS_16 RCUS_17 RCUS_18 
    LIKELY_2 LIKELY_3 LIKELY_5 LIKELY_6 LIKELY_8 LIKELY_9 
    LIKELY_11 LIKELY_12 LIKELY_13 LIKELY_16 LIKELY_17 LIKELY_18 
    LIKELY_23 LIKELY_24 LIKELY_26 LIKELY_27 LIKELY_28 LIKELY_29 LIKELY_31 
    SM_1 SM_2 SM_3 SM_4 SM_5 SM_6 SM_7 SM_8 SM_9 SM_10 SM_11 SM_12 SM_13 SM_14 
    SM_15 SM_16 SM_17 SM_18 SM_19 SM_20 SM_21 SM_22 SM_23 COMMUNITY_1 COMMUNITY_2 
    COMMUNITY_3 COMMUNITY_4 COMMUNITY_5 COMMUNITY_6 
   /IMPUTE METHOD=NONE
   /MISSINGSUMMARIES  OVERALL VARIABLES (MAXVARS=300 MINPCTMISSING=.01) PATTERNS.

*Step 3: Littles MCAR Test.  
*Gives MCAR test for whole dataset.
MVA VARIABLES=ASSERT_1 ASSERT_2 ASSERT_3 ASSERT_4 ASSERT_5 ASSERT_6 ASSERT_7
ASSERT_8 ASSERT_9 ASSERT_10 ASSERT_11 ASSERT_12 ASSERT_13 ASSERT_14 ASSERT_15 ASSERT_16 ASSERT_17 ASSERT_18 ASSERT_19 ASSERT_20 ASSERT_21 ASSERT_22
ASSERT_23 ASSERT_24 ASSERT_25 ASSUME_1 ASSUME_2 ASSUME_3 
    ASSUME_4 ASSUME_5 ASSUME_6 ASSUME_7 POS_1 POS_2 POS_3 POS_4 POS_5 POS_6 POS_7 POS_8 POS_9 PBC_1 
    PBC_2 PBC_3 PBC_4 PBC_5 PBC_6 PBC_7 PBC_8 PBC_9 RLN_1 RLN_2 RLN_3 RLN_4 RLN_5 IBA_1 IBA_2 IBA_3 
    IBA_4 IBA_5 IBA_6 AWARE_1 AWARE_2 AWARE_3 AWARE_4 RCUS_1 RCUS_2 RCUS_3 RCUS_4 RCUS_5 RCUS_6 RCUS_7 
    RCUS_8 RCUS_9 RCUS_10 RCUS_11 RCUS_12 RCUS_13 RCUS_14 RCUS_15 RCUS_16 RCUS_17 RCUS_18 
    LIKELY_2 LIKELY_3 LIKELY_5 LIKELY_6 LIKELY_8 LIKELY_9 
    LIKELY_11 LIKELY_12 LIKELY_13 LIKELY_16 LIKELY_17 LIKELY_18 
    LIKELY_23 LIKELY_24 LIKELY_26 LIKELY_27 LIKELY_28 LIKELY_29 LIKELY_31 
    SM_1 SM_2 SM_3 SM_4 SM_5 SM_6 SM_7 SM_8 SM_9 SM_10 SM_11 SM_12 SM_13 SM_14 
    SM_15 SM_16 SM_17 SM_18 SM_19 SM_20 SM_21 SM_22 SM_23 COMMUNITY_1 COMMUNITY_2 
    COMMUNITY_3 COMMUNITY_4 COMMUNITY_5 COMMUNITY_6 
  /EM(TOLERANCE=0.001 CONVERGENCE=0.0001 ITERATIONS=25).

*Step 4. Impute PER subscale.
DATASET DECLARE  ASSERT.
MVA VARIABLES=ASSERT_1 ASSERT_2 ASSERT_3 ASSERT_4 ASSERT_5 ASSERT_6 ASSERT_7 ASSERT_8 ASSERT_9 ASSERT_10 ASSERT_11 ASSERT_12 
    ASSERT_13 ASSERT_14 ASSERT_15 ASSERT_16 ASSERT_17 ASSERT_18 ASSERT_19 ASSERT_20 ASSERT_21 ASSERT_22  ASSERT_23 ASSERT_24 ASSERT_25 
  /EM(TOLERANCE=0.001 CONVERGENCE=0.0001 ITERATIONS=100 OUTFILE=ASSERT ).

DATASET DECLARE  ASSUME.
MVA VARIABLES=ASSUME_1 ASSUME_2 ASSUME_3 ASSUME_4 ASSUME_5 ASSUME_6 ASSUME_7
  /EM(TOLERANCE=0.001 CONVERGENCE=0.0001 ITERATIONS=100 OUTFILE=ASSUME ).

DATASET DECLARE  POS.
MVA VARIABLES=POS_1 POS_2 POS_3 POS_4 POS_5 POS_6 POS_7 POS_8 POS_9
  /EM(TOLERANCE=0.001 CONVERGENCE=0.0001 ITERATIONS=100 OUTFILE=POS ).

DATASET DECLARE  PBC.
MVA VARIABLES=PBC_1 
    PBC_2 PBC_3 PBC_4 PBC_5 PBC_6 PBC_7 PBC_8 PBC_9
  /EM(TOLERANCE=0.001 CONVERGENCE=0.0001 ITERATIONS=100 OUTFILE=PBC ).

DATASET DECLARE  RLN.
MVA VARIABLES=RLN_1 RLN_2 RLN_3 RLN_4 RLN_5
  /EM(TOLERANCE=0.001 CONVERGENCE=0.0001 ITERATIONS=100 OUTFILE=RLN ).

DATASET DECLARE  IBA.
MVA VARIABLES=IBA_1 IBA_2 IBA_3 IBA_4 IBA_5 IBA_6 
  /EM(TOLERANCE=0.001 CONVERGENCE=0.0001 ITERATIONS=100 OUTFILE=IBA ).

DATASET DECLARE  AWARE.
MVA VARIABLES=AWARE_1 AWARE_2 AWARE_3 AWARE_4
  /EM(TOLERANCE=0.001 CONVERGENCE=0.0001 ITERATIONS=100 OUTFILE=AWARE ).

DATASET DECLARE  RCUS.
MVA VARIABLES= RCUS_1 RCUS_2 RCUS_3 RCUS_4 RCUS_5 RCUS_6 RCUS_7 
    RCUS_8 RCUS_9 RCUS_10 RCUS_11 RCUS_12 RCUS_13 RCUS_14 RCUS_15 RCUS_16 RCUS_17 RCUS_18
  /EM(TOLERANCE=0.001 CONVERGENCE=0.0001 ITERATIONS=100 OUTFILE=RCUS ).

DATASET DECLARE  LIKELY.
MVA VARIABLES=
    LIKELY_2 LIKELY_3 LIKELY_5 LIKELY_6 LIKELY_8 LIKELY_9 
    LIKELY_11 LIKELY_12 LIKELY_13 LIKELY_16 LIKELY_17 LIKELY_18 
    LIKELY_23 LIKELY_24 LIKELY_26 LIKELY_27 LIKELY_28 LIKELY_29 LIKELY_31 
  /EM(TOLERANCE=0.001 CONVERGENCE=0.0001 ITERATIONS=100 OUTFILE=LIKELY ).

DATASET DECLARE  SM.
MVA VARIABLES=SM_1 SM_2 SM_3 SM_4 SM_5 SM_6 SM_7 SM_8 SM_9 SM_10 SM_11 SM_12 SM_13 SM_14 
    SM_15 SM_16 SM_17 SM_18 SM_19 SM_20 SM_21 SM_22 SM_23
  /EM(TOLERANCE=0.001 CONVERGENCE=0.0001 ITERATIONS=100 OUTFILE=SM ).

DATASET DECLARE  COMMUNITY.
MVA VARIABLES=COMMUNITY_1 COMMUNITY_2  COMMUNITY_3 COMMUNITY_4 COMMUNITY_5 COMMUNITY_6
  /EM(TOLERANCE=0.001 CONVERGENCE=0.0001 ITERATIONS=100 OUTFILE=COMMUNITY ).



*Merge subscale data. 
MATCH FILES /FILE='DataSet2'   /FILE='ASSERT'.
EXECUTE.
MATCH FILES /FILE=*  /FILE='ASSUME'.
EXECUTE.
MATCH FILES /FILE=*  /FILE='POS'.
EXECUTE.
MATCH FILES /FILE=*  /FILE='PBC'.
EXECUTE.
MATCH FILES /FILE=*  /FILE='RLN'.
EXECUTE.
MATCH FILES /FILE=*  /FILE='IBA'.
EXECUTE.
MATCH FILES /FILE=*  /FILE='AWARE'.
EXECUTE.
MATCH FILES /FILE=*  /FILE='LIKELY'.
EXECUTE.
MATCH FILES /FILE=*  /FILE='SM'.
EXECUTE.
MATCH FILES /FILE=*  /FILE='COMMUNITY'.
EXECUTE.
MATCH FILES /FILE=*  /FILE='RCUS'.
EXECUTE. 

*CREATE SCALE LEVEL VARIABLES

COMPUTE ASSERT =MEAN(ASSERT_1, ASSERT_2, ASSERT_3, ASSERT_4, ASSERT_5, ASSERT_6, ASSERT_7, ASSERT_8, ASSERT_9, ASSERT_10, ASSERT_11, ASSERT_12, ASSERT_13, ASSERT_14, 
ASSERT_15, ASSERT_16, ASSERT_17, ASSERT_18, ASSERT_19, ASSERT_20, ASSERT_21, ASSERT_22, ASSERT_23, ASSERT_24, ASSERT_25). 
VARIABLE LABELS  ASSERT 'Sexually Assertive Behavior Scale'.
EXECUTE.

COMPUTE  ASSUME=MEAN(ASSUME_1, ASSUME_2, ASSUME_3, ASSUME_4, ASSUME_5, ASSUME_6, ASSUME_7). 
VARIABLE LABELS  ASSUME 'Assuming Consent'.
EXECUTE.

COMPUTE  POS=MEAN(POS_1, POS_2, POS_3, POS_4, POS_5, POS_6, POS_7, POS_8, POS_9). 
VARIABLE LABELS  POS 'Positive Attitude Towards Establishing Consent'.
EXECUTE.

COMPUTE  PBC=MEAN(PBC_1, PBC_2, PBC_3, PBC_4, PBC_5, PBC_6, PBC_7, PBC_8, PBC_9). 
VARIABLE LABELS  PBC 'Lack of Perceived Behavioral Control'.
EXECUTE.

COMPUTE  RLN=MEAN(RLN_1, RLN_2, RLN_3, RLN_4, RLN_5). 
VARIABLE LABELS  RLN 'Relationship Length Norms'.
EXECUTE.

COMPUTE  IBA=MEAN(IBA_1, IBA_2, IBA_3, IBA_4, IBA_5, IBA_6). 
VARIABLE LABELS  IBA 'Indirect Behavioral Approach'.
EXECUTE.

COMPUTE  AWARE=MEAN(AWARE_1, AWARE_2, AWARE_3, AWARE_4). 
VARIABLE LABELS  AWARE 'Awareness of Consent'.
EXECUTE.

COMPUTE  RCUS=MEAN(RCUS_1, RCUS_2, RCUS_3, RCUS_4, RCUS_5, RCUS_6, RCUS_7, RCUS_8, RCUS_9, RCUS_10, RCUS_11, RCUS_12, RCUS_13, RCUS_14, RCUS_15, RCUS_16, RCUS_17, RCUS_18). 
VARIABLE LABELS  RCUS 'Reasons for Consenting to Unwanted Sex Scale'.
EXECUTE.

COMPUTE  LIKELY_COERCE=MEAN(LIKELY_2, LIKELY_3, LIKELY_5, LIKELY_6, LIKELY_8, LIKELY_9, LIKELY_11, LIKELY_12, LIKELY_13, 
LIKELY_16, LIKELY_17, LIKELY_18, LIKELY_23, LIKELY_24, LIKELY_26, LIKELY_27, LIKELY_28, LIKELY_29, LIKELY_31).
VARIABLE LABELS  LIKELY_COERCE 'Tactics to Obtain Sex: COERCE: How LIKELY?'.
EXECUTE.
 
RECODE SM_18, SM_19, SM_20, SM_21 (1=5) (2=4) (3=3) (4=2) (5=1).
EXECUTE.

COMPUTE YOST =MEAN(SM_1, SM_2, SM_3, SM_4, SM_5, SM_6, SM_7, SM_8, SM_9, SM_10, SM_11, SM_12, SM_13, SM_14, SM_15, SM_16, SM_17, SM_18, SM_19, SM_20, SM_21, SM_22, SM_23 ).
VARIABLE LABELS  YOST 'Attitudes Toward Sadomasochism'.
EXECUTE.

COMPUTE YOST_Wrong=MEAN(SM_1, SM_2, SM_3, SM_4, SM_5, SM_6, SM_7, SM_8, SM_9, SM_10, SM_11, SM_12).
VARIABLE LABELS  YOST_Wrong 'Socially Wrong'.
EXECUTE.

COMPUTE YOST_V =MEAN(SM_13, SM_14, SM_15, SM_16, SM_17).
VARIABLE LABELS  YOST_V 'Violence'.
EXECUTE. 

COMPUTE YOST_LoT =MEAN(SM_18, SM_19, SM_20, SM_21).
VARIABLE LABELS  YOST_LoT  'Lack of Tolerance'.
EXECUTE. 

COMPUTE YOST_RL =MEAN(SM_22, SM_23 ).
VARIABLE LABELS  YOST_RL  'Real Life'.
EXECUTE.

COMPUTE COMMUN =SUM(COMMUNITY_1,COMMUNITY_2,COMMUNITY_3,COMMUNITY_4,COMMUNITY_5,COMMUNITY_6).
VARIABLE LABELS  COMMUN 'Kink Community Positivity'.
EXECUTE. 

RENAME VARIABLE (Age Age_sex YEARS_PRACTICE YEARS_COMMUN Partners =  AGE AGE_SEX YR_PRACTICE YR_COMMUN PARTNERS).
VARIABLE LEVEL AGE AGE_SEX YR_PRACTICE YR_COMMUN PARTNERS (SCALE).


SAVE OUTFILE='C:\Users\anthe\Dropbox\Important Stuff\Old Projects\BDSM & Consent\BDSM R Analyses\Kink_with variables.sav'.