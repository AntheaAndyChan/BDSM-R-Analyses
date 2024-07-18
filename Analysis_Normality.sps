* Encoding: UTF-8.


INCLUDE 'C:\Users\anthe\Dropbox\Important Stuff\Old Projects\BDSM & Consent\BDSM R Analyses\normtest.sps'.
*INCLUDE 'C:\Users\anthe\Dropbox\Important Stuff\Old Projects\BDSM & Consent\New Analysis 2023\normtest.sps'.


DATASET ACTIVATE DataSet1.
NORMTEST vars= ASSERT, ASSUME, POS, PBC, RLN, IBA, AWARE, RCUS, LIKELY_COERCE.
*b2p on kink only sample (N=265) =129.65
    *With NBs: 128.45

*Remove rows 23, 53, 92,144,145. / 25,96,57,153,154->29,126,198,273,275
select if(id ~= 29 AND id ~= 126 AND id ~= 198 AND id ~= 273 AND id ~= 275).

DATASET ACTIVATE DataSet1.
select if(id ~= 29 AND id ~= 126 AND id ~= 198 AND id ~= 273 AND id ~= 275).
NORMTEST vars= ASSERT, ASSUME, POS, PBC, RLN, IBA, AWARE, RCUS, LIKELY_COERCE. 
*117.34.

EXAMINE VARIABLES= ASSERT ASSUME POS PBC RLN IBA AWARE RCUS LIKELY_COERCE
  /PLOT HISTOGRAM 
  /COMPARE GROUPS
  /CINTERVAL 95
  /STATISTICS EXTREME
  /MISSING LISTWISE
  /NOTOTAL.

*log transform the most problematic looking variables, *pbc aware RCUS   likely.
DATASET ACTIVATE DataSet1.
NORMTEST vars= ASSERT, ASSUME, POS, PBC_ln, RLN, IBA, AWARE_ln, RCUS_ln, LIKELY_ln.
*b2p=117.40

DATASET ACTIVATE DataSet1.
EXAMINE VARIABLES= PBC_ln AWARE_ln RCUS_ln  LIKELY_ln
  /PLOT HISTOGRAM
  /COMPARE GROUPS
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.
