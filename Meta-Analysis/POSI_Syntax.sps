* Encoding: UTF-8.
* Encoding: .


'STEP2 
'4. ES adjustment =============================================

COMPUTE g = (1-(3/(4*(NTreat+NControl)-9)))*D . 
EXECUTE .

COMPUTE v = ((NTreat+NControl)/(NTreat*NControl))+
((g**2)/(2*(NTreat+NControl))) .

COMPUTE W = 1/v .
EXECUTE .

'Run “MeanES” SPSS macro=====================

"Undesirable Work Outcomes

DATASET ACTIVATE DataSet1.
USE ALL.
COMPUTE filter_$=(ESTypes = 1 & (Outcome = 2 OR Outcome = 4)).
VARIABLE LABELS filter_$ 'ESTypes = 1 & (Outcome = 4 OR Outcome = 4) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

"Desirable Work Outcomes

USE ALL.
COMPUTE filter_$=(ESTypes = 1 & (Outcome = 1 OR Outcome = 3 OR Outcome = 5 OR Outcome = 6)).
VARIABLE LABELS filter_$ 'ESTypes = 1 & (Outcome = 1 OR Outcome = 3 OR Outcome = 5 OR Outcome = '+
    '6) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

"Individual Outcomes (1-6)

DATASET ACTIVATE DataSet1.
USE ALL.
COMPUTE filter_$=(ESTypes = 1 & (Outcome = 6)).
VARIABLE LABELS filter_$ 'ESTypes = 1 & (Outcome =6) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


"Time Moderator Positive 

USE ALL.
COMPUTE filter_$=( (Outcome = 1 OR Outcome = 3 OR Outcome = 5 OR Outcome = 6)).
VARIABLE LABELS filter_$ ' (Outcome = 1 OR Outcome = 3 OR Outcome = 5 OR Outcome = 6) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

"Time Moderator Negative

DATASET ACTIVATE DataSet1.
USE ALL.
COMPUTE filter_$=((Outcome = 2 OR Outcome = 4)).
VARIABLE LABELS filter_$ 'ESTypes = 1 & (Outcome = 4 OR Outcome = 4) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

"Mean Effect Size

INCLUDE '/Users/ScottDonaldson/Desktop/MetaAnalysis/SPSSMacros/MeanES.sps'. 
MEANES ES = g
/W =W .

DATASET ACTIVATE DataSet1.
FREQUENCIES VARIABLES=NTreat NControl
  /STATISTICS=SUM
  /ORDER=ANALYSIS.

'Box plot

EXAMINE VARIABLES=g
/STATISTICS=NONE.


"----------

FREQUENCIES VARIABLES=g
/FORMAT=NOTABLE
/HISTOGRAM NORMAL
/ORDER=ANALYSIS.


'

'Box plot

EXAMINE VARIABLES=g
/STATISTICS=NONE.

'ANOVA ============================

INCLUDE  '/Users/ScottDonaldson/Desktop/MetaAnalysis/SPSSMacros/MetaF.sps'.
METAF ES = g/W = W 
/GROUP = PosNew.


INCLUDE  '/Users/ScottDonaldson/Desktop/MetaAnalysis/SPSSMacros/MetaF.sps'.
METAF ES = g/W = W /GROUP =  ESTypes/MODEL = REML.













USE ALL.
COMPUTE filter_$=((PosNew ~= 2 OR PosNew ~= 4)  & (Outcome = 2 OR Outcome = 4)).
VARIABLE LABELS filter_$ '(PosNew ~= 2 OR PosPrinciple ~= 4)  & (Outcome = 2 OR Outcome = '+
    '4) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.
