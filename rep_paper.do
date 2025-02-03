* Get the data
use "/Users/salmahoumane/Downloads/rr_rdd_data.dta", clear
count
* initially the sample had 38691 observations
* Drop all the rows with missing long term data: drop all the rows where the variable: HasG3ST is 0
drop if HasG3ST == 0
count
*After dropping all those that did not have a grade 3 outcome, we end up with 9951
describe

*Summary Statistics
tabstat SCHYR FallTotalScore FTRL FOWT FLI FWV FHRSW FCAP GotRRfirst ///
    Fall_OSscore_Cntrd Fall_BelowCut c_Fall G3_zscore, ///
    statistics(mean sd min max) columns(statistics)





*** QUESTION 1: Plot the running variable against the treatment to confirm this is a fuzzy 
*** Running variable: Fall_OSscore_Cntrd
*** Treatment variable: GotRRfirst
*** Outcome Variable: "G3_zscore"  

* Generate bins for different resolutions
egen bin1 = cut(Fall_OSscore_Cntrd), at(-2(0.01)2) 
egen bin2 = cut(Fall_OSscore_Cntrd), at(-2(0.005)2) 
egen bin3 = cut(Fall_OSscore_Cntrd), at(-2(0.045)2)
egen bin5 = cut(Fall_OSscore_Cntrd), at(-2(0.06)2)
egen bin6 = cut(Fall_OSscore_Cntrd), at(-2(0.05)2)
* Calculate the mean within each bin for bin1
bys bin1: egen myin = mean(GotRRfirst)
* Scatter plot for bin = 0.01
twoway (scatter myin bin1), scheme(s1mono) ytitle("Probability of Receiving RR in Wave 1") xtitle("Fall OS Score (Centered)") title("Bin = 0.01") xline(0)

*** Check for a discontinuity in the outcome (G3_zscore) for different bin widths
*** Outcome: G3_zscore
bys bin1: egen mdif1 = mean(G3_zscore)
bys bin2: egen mdif2 = mean(G3_zscore)
bys bin3: egen mdif3 = mean(G3_zscore)
bys bin5: egen mdif5 = mean(G3_zscore)
bys bin6: egen mdif6 = mean(G3_zscore)
* Scatter plots for different bin widths
twoway (scatter mdif1 bin1), scheme(s1mono) ytitle("G3 Z-Score") xtitle("Fall OS Score (Centered)") title("Bin = 0.01") xline(0)
twoway (scatter mdif2 bin2), scheme(s1mono) ytitle("G3 Z-Score") xtitle("Fall OS Score (Centered)") title("Bin = 0.005") xline(0)
twoway (scatter mdif3 bin3), scheme(s1mono) ytitle("G3 Z-Score") xtitle("Fall OS Score (Centered)") title("Bin = 0.045") xline(0)
twoway (scatter mdif5 bin5), scheme(s1mono) ytitle("G3 Z-Score") xtitle("Fall OS Score (Centered)") title("Bin = 0.06") xline(0)
twoway (scatter mdif6 bin6), scheme(s1mono) ytitle("G3 Z-Score") xtitle("Fall OS Score (Centered)") title("Bin = 0.05") xline(0)
* Add smoothed fits with lpolyci
twoway (scatter mdif1 bin1) (lpolyci G3_zscore Fall_OSscore_Cntrd if Fall_OSscore_Cntrd < 0) (lpolyci G3_zscore Fall_OSscore_Cntrd if Fall_OSscore_Cntrd >= 0), ///
scheme(s1color) ytitle("G3 Z-Score") xtitle("Fall OS Score (Centered)") title("Bin = 0.01 with Smoothing") ///
xline(0) legend(label(1 "Means") label(2 "95% CI") label(3 "NP Fit") order(1 2 3))

** Testing the validity of fuzzy rdd
reg GotRRfirst Fall_OSscore_Cntrd

* ITT 
reg G3_zscore Fall_BelowCut Fall_OSscore_Cntrd, robust

*** Plot the density of the assignment variable
hist Fall_OSscore_Cntrd, scheme(s1mono)
*** Test for a discontinuity in the density of the assignment variable
rddensity Fall_OSscore_Cntrd, c(0)
rddensity Fall_OSscore_Cntrd, c(0) plot

*** Parametric  Fuzzy RDD
* Generating Variables
gen Fall_OSscore_Cntrd2 = Fall_OSscore_Cntrd^2
gen Fall_OSscore_Cntrd3 = Fall_OSscore_Cntrd^3

gen Fall_OSscore_Cntrd_BelowCut = Fall_OSscore_Cntrd * Fall_BelowCut
gen Fall_OSscore_Cntrd2_BelowCut = Fall_OSscore_Cntrd2 * Fall_BelowCut
gen Fall_OSscore_Cntrd3_BelowCut = Fall_OSscore_Cntrd3 * Fall_BelowCut

gen Fall_OSscore_Cntrd_GotRRfirst = Fall_OSscore_Cntrd * GotRRfirst
gen Fall_OSscore_Cntrd2_GotRRfirst = Fall_OSscore_Cntrd2 * GotRRfirst
gen Fall_OSscore_Cntrd3_GotRRfirst = Fall_OSscore_Cntrd3 * GotRRfirst


***2SLS Degree 1 no intersection
** First stage
reg GotRRfirst Fall_BelowCut Fall_OSscore_Cntrd, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
    Fall_OSscore_Cntrd, robust
	
***  2SLS Degree 1 
** First stage
reg GotRRfirst Fall_BelowCut Fall_OSscore_Cntrd ///
    c.Fall_OSscore_Cntrd#c.Fall_BelowCut, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
    Fall_OSscore_Cntrd ///
    c.Fall_OSscore_Cntrd#c.GotRRfirst, robust

*2SLS Degree 2
** First stage
reg GotRRfirst Fall_BelowCut Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 ///
    c.Fall_OSscore_Cntrd#c.Fall_BelowCut c.Fall_OSscore_Cntrd2#c.Fall_BelowCut, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
    Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 ///
    c.Fall_OSscore_Cntrd#c.GotRRfirst c.Fall_OSscore_Cntrd2#c.GotRRfirst, robust

*** 2SLS Degree 3
** First stage
reg GotRRfirst Fall_BelowCut Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 Fall_OSscore_Cntrd3 ///
    c.Fall_OSscore_Cntrd#c.Fall_BelowCut c.Fall_OSscore_Cntrd2#c.Fall_BelowCut ///
    c.Fall_OSscore_Cntrd3#c.Fall_BelowCut, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
    Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 Fall_OSscore_Cntrd3 ///
    c.Fall_OSscore_Cntrd#c.GotRRfirst c.Fall_OSscore_Cntrd2#c.GotRRfirst ///
    c.Fall_OSscore_Cntrd3#c.GotRRfirst, robust
	
* G3SLS Linear No Interaction
reg3 (GotRRfirst Fall_BelowCut Fall_OSscore_Cntrd) ///
     (G3_zscore GotRRfirst Fall_OSscore_Cntrd), 3sls
* G3SLS Linear 
reg3 (GotRRfirst Fall_BelowCut Fall_OSscore_Cntrd Fall_OSscore_Cntrd_BelowCut) ///
     (G3_zscore GotRRfirst Fall_OSscore_Cntrd Fall_OSscore_Cntrd_BelowCut), 3sls
* G3SLS Degree 2
reg3 (GotRRfirst Fall_BelowCut Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 ///
      Fall_OSscore_Cntrd_BelowCut Fall_OSscore_Cntrd2_BelowCut) ///
     (G3_zscore GotRRfirst Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 ///
      Fall_OSscore_Cntrd_GotRRfirst Fall_OSscore_Cntrd2_GotRRfirst), 3sls
* G3SLS Degree 3
reg3 (GotRRfirst Fall_BelowCut Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 Fall_OSscore_Cntrd3 ///
      Fall_OSscore_Cntrd_BelowCut Fall_OSscore_Cntrd2_BelowCut Fall_OSscore_Cntrd3_BelowCut) ///
     (G3_zscore GotRRfirst Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 Fall_OSscore_Cntrd3 ///
      Fall_OSscore_Cntrd_GotRRfirst Fall_OSscore_Cntrd2_GotRRfirst Fall_OSscore_Cntrd3_GotRRfirst), 3sls


	 
	 
	 
** BANDWIDTH CHANGE	 
** Get the data again
use "/Users/salmahoumane/Downloads/rr_rdd_data.dta", clear
* Drop all the rows with missing long term data: drop all the rows where the variable: HasG3ST is 0
drop if HasG3ST == 0
*Trying alternative bws: 0.5
gen in_window = (Fall_OSscore_Cntrd >= -0.5 & Fall_OSscore_Cntrd <= 0.5)
keep if in_window
count
*Generating Variables
gen Fall_OSscore_Cntrd2 = Fall_OSscore_Cntrd^2
gen Fall_OSscore_Cntrd3 = Fall_OSscore_Cntrd^3
gen Fall_OSscore_Cntrd_BelowCut = Fall_OSscore_Cntrd * Fall_BelowCut
gen Fall_OSscore_Cntrd2_BelowCut = Fall_OSscore_Cntrd2 * Fall_BelowCut
gen Fall_OSscore_Cntrd3_BelowCut = Fall_OSscore_Cntrd3 * Fall_BelowCut
gen Fall_OSscore_Cntrd_GotRRfirst = Fall_OSscore_Cntrd * GotRRfirst
gen Fall_OSscore_Cntrd2_GotRRfirst = Fall_OSscore_Cntrd2 * GotRRfirst
gen Fall_OSscore_Cntrd3_GotRRfirst = Fall_OSscore_Cntrd3 * GotRRfirst


** Get the data again
use "/Users/salmahoumane/Downloads/rr_rdd_data.dta", clear
* Drop all the rows with missing long term data: drop all the rows where the variable: HasG3ST is 0
drop if HasG3ST == 0
*Trying alternative bws: 0.25
gen in_window = (Fall_OSscore_Cntrd >= -0.25 & Fall_OSscore_Cntrd <= 0.25)
keep if in_window
count
*Generating Variables
gen Fall_OSscore_Cntrd2 = Fall_OSscore_Cntrd^2
gen Fall_OSscore_Cntrd3 = Fall_OSscore_Cntrd^3
gen Fall_OSscore_Cntrd_BelowCut = Fall_OSscore_Cntrd * Fall_BelowCut
gen Fall_OSscore_Cntrd2_BelowCut = Fall_OSscore_Cntrd2 * Fall_BelowCut
gen Fall_OSscore_Cntrd3_BelowCut = Fall_OSscore_Cntrd3 * Fall_BelowCut
gen Fall_OSscore_Cntrd_GotRRfirst = Fall_OSscore_Cntrd * GotRRfirst
gen Fall_OSscore_Cntrd2_GotRRfirst = Fall_OSscore_Cntrd2 * GotRRfirst
gen Fall_OSscore_Cntrd3_GotRRfirst = Fall_OSscore_Cntrd3 * GotRRfirst


***RUNNING VARIABLE CHANGE TO FTRL
** Get the data again
use "/Users/salmahoumane/Downloads/rr_rdd_data.dta", clear
* Drop all the rows with missing long term data: drop all the rows where the variable: HasG3ST is 0
drop if HasG3ST == 0
* Generating Variables
gen FTRL2 = FTRL^2
gen FTRL3 = FTRL^3

gen FTRL_BelowCut = FTRL * Fall_BelowCut
gen FTRL2_BelowCut = FTRL2 * Fall_BelowCut
gen FTRL3_BelowCut = FTRL3 * Fall_BelowCut

gen FTRL_GotRRfirst = FTRL * GotRRfirst
gen FTRL2_GotRRfirst = FTRL2 * GotRRfirst
gen FTRL3_GotRRfirst = FTRL3 * GotRRfirst


***2SLS Degree 1 no intersection
** First stage
reg GotRRfirst Fall_BelowCut FTRL, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
FTRL, robust

***  2SLS Degree 1
** First stage
reg GotRRfirst Fall_BelowCut FTRL ///
c.FTRL#c.Fall_BelowCut, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
FTRL ///
c.FTRL#c.GotRRfirst, robust

*2SLS Degree 2
** First stage
reg GotRRfirst Fall_BelowCut FTRL FTRL2 ///
c.FTRL#c.Fall_BelowCut c.FTRL2#c.Fall_BelowCut, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
FTRL FTRL2 ///
c.FTRL#c.GotRRfirst c.FTRL2#c.GotRRfirst, robust

*** 2SLS Degree 3
** First stage
reg GotRRfirst Fall_BelowCut FTRL FTRL2 FTRL3 ///
c.FTRL#c.Fall_BelowCut c.FTRL2#c.Fall_BelowCut ///
c.FTRL3#c.Fall_BelowCut, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
FTRL FTRL2 FTRL3 ///
c.FTRL#c.GotRRfirst c.FTRL2#c.GotRRfirst ///
c.FTRL3#c.GotRRfirst, robust

* G3SLS Linear No Interaction
reg3 (GotRRfirst Fall_BelowCut FTRL) ///
(G3_zscore GotRRfirst FTRL), 3sls
* G3SLS Linear
reg3 (GotRRfirst Fall_BelowCut FTRL FTRL_BelowCut) ///
(G3_zscore GotRRfirst FTRL FTRL_BelowCut), 3sls
* G3SLS Degree 2
reg3 (GotRRfirst Fall_BelowCut FTRL FTRL2 ///
FTRL_BelowCut FTRL2_BelowCut) ///
(G3_zscore GotRRfirst FTRL FTRL2 ///
FTRL_GotRRfirst FTRL2_GotRRfirst), 3sls
* G3SLS Degree 3
reg3 (GotRRfirst Fall_BelowCut FTRL FTRL2 FTRL3 ///
FTRL_BelowCut FTRL2_BelowCut FTRL3_BelowCut) ///
(G3_zscore GotRRfirst FTRL FTRL2 FTRL3 ///
FTRL_GotRRfirst FTRL2_GotRRfirst FTRL3_GotRRfirst), 3sls

***RUNNING VARIABLE CHANGE TO FLI
* Generating Variables
gen FLI2 = FLI^2
gen FLI3 = FLI^3

gen FLI_BelowCut = FLI * Fall_BelowCut
gen FLI2_BelowCut = FLI2 * Fall_BelowCut
gen FLI3_BelowCut = FLI3 * Fall_BelowCut

gen FLI_GotRRfirst = FLI * GotRRfirst
gen FLI2_GotRRfirst = FLI2 * GotRRfirst
gen FLI3_GotRRfirst = FLI3 * GotRRfirst


***2SLS Degree 1 no intersection
** First stage
reg GotRRfirst Fall_BelowCut FLI, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
FLI, robust

***  2SLS Degree 1
** First stage
reg GotRRfirst Fall_BelowCut FLI ///
c.FLI#c.Fall_BelowCut, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
FLI ///
c.FLI#c.GotRRfirst, robust

*2SLS Degree 2
** First stage
reg GotRRfirst Fall_BelowCut FLI FLI2 ///
c.FLI#c.Fall_BelowCut c.FLI2#c.Fall_BelowCut, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
FLI FLI2 ///
c.FLI#c.GotRRfirst c.FLI2#c.GotRRfirst, robust

*** 2SLS Degree 3
** First stage
reg GotRRfirst Fall_BelowCut FLI FLI2 FLI3 ///
c.FLI#c.Fall_BelowCut c.FLI2#c.Fall_BelowCut ///
c.FLI3#c.Fall_BelowCut, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
FLI FLI2 FLI3 ///
c.FLI#c.GotRRfirst c.FLI2#c.GotRRfirst ///
c.FLI3#c.GotRRfirst, robust

* G3SLS Linear No Interaction
reg3 (GotRRfirst Fall_BelowCut FLI) ///
(G3_zscore GotRRfirst FLI), 3sls
* G3SLS Linear
reg3 (GotRRfirst Fall_BelowCut FLI FLI_BelowCut) ///
(G3_zscore GotRRfirst FLI FLI_BelowCut), 3sls
* G3SLS Degree 2
reg3 (GotRRfirst Fall_BelowCut FLI FLI2 ///
FLI_BelowCut FLI2_BelowCut) ///
(G3_zscore GotRRfirst FLI FLI2 ///
FLI_GotRRfirst FLI2_GotRRfirst), 3sls
* G3SLS Degree 3
reg3 (GotRRfirst Fall_BelowCut FLI FLI2 FLI3 ///
FLI_BelowCut FLI2_BelowCut FLI3_BelowCut) ///
(G3_zscore GotRRfirst FLI FLI2 FLI3 ///
FLI_GotRRfirst FLI2_GotRRfirst FLI3_GotRRfirst), 3sls

***RUNNING VARIABLE CHANGE TO FWV
* Generating Variables
gen FWV2 = FWV^2
gen FWV3 = FWV^3

gen FWV_BelowCut = FWV * Fall_BelowCut
gen FWV2_BelowCut = FWV2 * Fall_BelowCut
gen FWV3_BelowCut = FWV3 * Fall_BelowCut

gen FWV_GotRRfirst = FWV * GotRRfirst
gen FWV2_GotRRfirst = FWV2 * GotRRfirst
gen FWV3_GotRRfirst = FWV3 * GotRRfirst


***2SLS Degree 1 no intersection
** First stage
reg GotRRfirst Fall_BelowCut FWV, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
FWV, robust

***  2SLS Degree 1
** First stage
reg GotRRfirst Fall_BelowCut FWV ///
c.FWV#c.Fall_BelowCut, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
FWV ///
c.FWV#c.GotRRfirst, robust

*2SLS Degree 2
** First stage
reg GotRRfirst Fall_BelowCut FWV FWV2 ///
c.FWV#c.Fall_BelowCut c.FWV2#c.Fall_BelowCut, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
FWV FWV2 ///
c.FWV#c.GotRRfirst c.FWV2#c.GotRRfirst, robust

*** 2SLS Degree 3
** First stage
reg GotRRfirst Fall_BelowCut FWV FWV2 FWV3 ///
c.FWV#c.Fall_BelowCut c.FWV2#c.Fall_BelowCut ///
c.FWV3#c.Fall_BelowCut, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
FWV FWV2 FWV3 ///
c.FWV#c.GotRRfirst c.FWV2#c.GotRRfirst ///
c.FWV3#c.GotRRfirst, robust

* G3SLS Linear No Interaction
reg3 (GotRRfirst Fall_BelowCut FWV) ///
(G3_zscore GotRRfirst FWV), 3sls
* G3SLS Linear
reg3 (GotRRfirst Fall_BelowCut FWV FWV_BelowCut) ///
(G3_zscore GotRRfirst FWV FWV_BelowCut), 3sls
* G3SLS Degree 2
reg3 (GotRRfirst Fall_BelowCut FWV FWV2 ///
FWV_BelowCut FWV2_BelowCut) ///
(G3_zscore GotRRfirst FWV FWV2 ///
FWV_GotRRfirst FWV2_GotRRfirst), 3sls
* G3SLS Degree 3
reg3 (GotRRfirst Fall_BelowCut FWV FWV2 FWV3 ///
FWV_BelowCut FWV2_BelowCut FWV3_BelowCut) ///
(G3_zscore GotRRfirst FWV FWV2 FWV3 ///
FWV_GotRRfirst FWV2_GotRRfirst FWV3_GotRRfirst), 3sls


***RUNNING VARIABLE CHANGE TO FHRSW
* Generating Variables
gen FHRSW2 = FHRSW^2
gen FHRSW3 = FHRSW^3

gen FHRSW_BelowCut = FHRSW * Fall_BelowCut
gen FHRSW2_BelowCut = FHRSW2 * Fall_BelowCut
gen FHRSW3_BelowCut = FHRSW3 * Fall_BelowCut

gen FHRSW_GotRRfirst = FHRSW * GotRRfirst
gen FHRSW2_GotRRfirst = FHRSW2 * GotRRfirst
gen FHRSW3_GotRRfirst = FHRSW3 * GotRRfirst


***2SLS Degree 1 no intersection
** First stage
reg GotRRfirst Fall_BelowCut FHRSW, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
FHRSW, robust

***  2SLS Degree 1
** First stage
reg GotRRfirst Fall_BelowCut FHRSW ///
c.FHRSW#c.Fall_BelowCut, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
FHRSW ///
c.FHRSW#c.GotRRfirst, robust

*2SLS Degree 2
** First stage
reg GotRRfirst Fall_BelowCut FHRSW FHRSW2 ///
c.FHRSW#c.Fall_BelowCut c.FHRSW2#c.Fall_BelowCut, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
FHRSW FHRSW2 ///
c.FHRSW#c.GotRRfirst c.FHRSW2#c.GotRRfirst, robust

*** 2SLS Degree 3
** First stage
reg GotRRfirst Fall_BelowCut FHRSW FHRSW2 FHRSW3 ///
c.FHRSW#c.Fall_BelowCut c.FHRSW2#c.Fall_BelowCut ///
c.FHRSW3#c.Fall_BelowCut, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
FHRSW FHRSW2 FHRSW3 ///
c.FHRSW#c.GotRRfirst c.FHRSW2#c.GotRRfirst ///
c.FHRSW3#c.GotRRfirst, robust

* G3SLS Linear No Interaction
reg3 (GotRRfirst Fall_BelowCut FHRSW) ///
(G3_zscore GotRRfirst FHRSW), 3sls
* G3SLS Linear
reg3 (GotRRfirst Fall_BelowCut FHRSW FHRSW_BelowCut) ///
(G3_zscore GotRRfirst FHRSW FHRSW_BelowCut), 3sls
* G3SLS Degree 2
reg3 (GotRRfirst Fall_BelowCut FHRSW FHRSW2 ///
FHRSW_BelowCut FHRSW2_BelowCut) ///
(G3_zscore GotRRfirst FHRSW FHRSW2 ///
FHRSW_GotRRfirst FHRSW2_GotRRfirst), 3sls
* G3SLS Degree 3
reg3 (GotRRfirst Fall_BelowCut FHRSW FHRSW2 FHRSW3 ///
FHRSW_BelowCut FHRSW2_BelowCut FHRSW3_BelowCut) ///
(G3_zscore GotRRfirst FHRSW FHRSW2 FHRSW3 ///
FHRSW_GotRRfirst FHRSW2_GotRRfirst FHRSW3_GotRRfirst), 3sls


***RUNNING VARIABLE CHANGE TO FCAP
* Generating Variables
gen FCAP2 = FCAP^2
gen FCAP3 = FCAP^3

gen FCAP_BelowCut = FCAP * Fall_BelowCut
gen FCAP2_BelowCut = FCAP2 * Fall_BelowCut
gen FCAP3_BelowCut = FCAP3 * Fall_BelowCut

gen FCAP_GotRRfirst = FCAP * GotRRfirst
gen FCAP2_GotRRfirst = FCAP2 * GotRRfirst
gen FCAP3_GotRRfirst = FCAP3 * GotRRfirst


***2SLS Degree 1 no intersection
** First stage
reg GotRRfirst Fall_BelowCut FCAP, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
FCAP, robust

***  2SLS Degree 1
** First stage
reg GotRRfirst Fall_BelowCut FCAP ///
c.FCAP#c.Fall_BelowCut, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
FCAP ///
c.FCAP#c.GotRRfirst, robust

*2SLS Degree 2
** First stage
reg GotRRfirst Fall_BelowCut FCAP FCAP2 ///
c.FCAP#c.Fall_BelowCut c.FCAP2#c.Fall_BelowCut, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
FCAP FCAP2 ///
c.FCAP#c.GotRRfirst c.FCAP2#c.GotRRfirst, robust

*** 2SLS Degree 3
** First stage
reg GotRRfirst Fall_BelowCut FCAP FCAP2 FCAP3 ///
c.FCAP#c.Fall_BelowCut c.FCAP2#c.Fall_BelowCut ///
c.FCAP3#c.Fall_BelowCut, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
FCAP FCAP2 FCAP3 ///
c.FCAP#c.GotRRfirst c.FCAP2#c.GotRRfirst ///
c.FCAP3#c.GotRRfirst, robust

* G3SLS Linear No Interaction
reg3 (GotRRfirst Fall_BelowCut FCAP) ///
(G3_zscore GotRRfirst FCAP), 3sls
* G3SLS Linear
reg3 (GotRRfirst Fall_BelowCut FCAP FCAP_BelowCut) ///
(G3_zscore GotRRfirst FCAP FCAP_BelowCut), 3sls
* G3SLS Degree 2
reg3 (GotRRfirst Fall_BelowCut FCAP FCAP2 ///
FCAP_BelowCut FCAP2_BelowCut) ///
(G3_zscore GotRRfirst FCAP FCAP2 ///
FCAP_GotRRfirst FCAP2_GotRRfirst), 3sls
* G3SLS Degree 3
reg3 (GotRRfirst Fall_BelowCut FCAP FCAP2 FCAP3 ///
FCAP_BelowCut FCAP2_BelowCut FCAP3_BelowCut) ///
(G3_zscore GotRRfirst FCAP FCAP2 FCAP3 ///
FCAP_GotRRfirst FCAP2_GotRRfirst FCAP3_GotRRfirst), 3sls



***DONUT cut: 0.05 range
gen donut = abs(Fall_OSscore_Cntrd) < 0.05   // Exclude observations within 0.05 range
keep if donut == 0  // Keep only observations outside the donut
* Generating Variables
gen Fall_OSscore_Cntrd2 = Fall_OSscore_Cntrd^2
gen Fall_OSscore_Cntrd3 = Fall_OSscore_Cntrd^3
gen Fall_OSscore_Cntrd_BelowCut = Fall_OSscore_Cntrd * Fall_BelowCut
gen Fall_OSscore_Cntrd2_BelowCut = Fall_OSscore_Cntrd2 * Fall_BelowCut
gen Fall_OSscore_Cntrd3_BelowCut = Fall_OSscore_Cntrd3 * Fall_BelowCut
gen Fall_OSscore_Cntrd_GotRRfirst = Fall_OSscore_Cntrd * GotRRfirst
gen Fall_OSscore_Cntrd2_GotRRfirst = Fall_OSscore_Cntrd2 * GotRRfirst
gen Fall_OSscore_Cntrd3_GotRRfirst = Fall_OSscore_Cntrd3 * GotRRfirst

***2SLS Degree 1 no intersection
** First stage
reg GotRRfirst Fall_BelowCut Fall_OSscore_Cntrd, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
    Fall_OSscore_Cntrd, robust
	
***  2SLS Degree 1 
** First stage
reg GotRRfirst Fall_BelowCut Fall_OSscore_Cntrd ///
    c.Fall_OSscore_Cntrd#c.Fall_BelowCut, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
    Fall_OSscore_Cntrd ///
    c.Fall_OSscore_Cntrd#c.GotRRfirst, robust

*2SLS Degree 2
** First stage
reg GotRRfirst Fall_BelowCut Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 ///
    c.Fall_OSscore_Cntrd#c.Fall_BelowCut c.Fall_OSscore_Cntrd2#c.Fall_BelowCut, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
    Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 ///
    c.Fall_OSscore_Cntrd#c.GotRRfirst c.Fall_OSscore_Cntrd2#c.GotRRfirst, robust

*** 2SLS Degree 3
** First stage
reg GotRRfirst Fall_BelowCut Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 Fall_OSscore_Cntrd3 ///
    c.Fall_OSscore_Cntrd#c.Fall_BelowCut c.Fall_OSscore_Cntrd2#c.Fall_BelowCut ///
    c.Fall_OSscore_Cntrd3#c.Fall_BelowCut, robust
** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
    Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 Fall_OSscore_Cntrd3 ///
    c.Fall_OSscore_Cntrd#c.GotRRfirst c.Fall_OSscore_Cntrd2#c.GotRRfirst ///
    c.Fall_OSscore_Cntrd3#c.GotRRfirst, robust
	
* G3SLS Linear No Interaction
reg3 (GotRRfirst Fall_BelowCut Fall_OSscore_Cntrd) ///
     (G3_zscore GotRRfirst Fall_OSscore_Cntrd), 3sls
* G3SLS Linear 
reg3 (GotRRfirst Fall_BelowCut Fall_OSscore_Cntrd Fall_OSscore_Cntrd_BelowCut) ///
     (G3_zscore GotRRfirst Fall_OSscore_Cntrd Fall_OSscore_Cntrd_BelowCut), 3sls
* G3SLS Degree 2
reg3 (GotRRfirst Fall_BelowCut Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 ///
      Fall_OSscore_Cntrd_BelowCut Fall_OSscore_Cntrd2_BelowCut) ///
     (G3_zscore GotRRfirst Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 ///
      Fall_OSscore_Cntrd_GotRRfirst Fall_OSscore_Cntrd2_GotRRfirst), 3sls
* G3SLS Degree 3
reg3 (GotRRfirst Fall_BelowCut Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 Fall_OSscore_Cntrd3 ///
      Fall_OSscore_Cntrd_BelowCut Fall_OSscore_Cntrd2_BelowCut Fall_OSscore_Cntrd3_BelowCut) ///
     (G3_zscore GotRRfirst Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 Fall_OSscore_Cntrd3 ///
      Fall_OSscore_Cntrd_GotRRfirst Fall_OSscore_Cntrd2_GotRRfirst Fall_OSscore_Cntrd3_GotRRfirst), 3sls

***DONUT cut: 0.1 range
gen donut = abs(Fall_OSscore_Cntrd) < 0.1   // Exclude observations within 0.05 range
keep if donut == 0  // Keep only observations outside the donut
* We get 657 observations dropped in comparison to 319 observations. Because that is 6.6% of the total sample, I choose to do the donut analysis with the 0.05 range which only drops  3.2% of the sample.

** PLACEBO TEST
* Get the data
use "/Users/salmahoumane/Downloads/rr_rdd_data.dta", clear
drop if HasG3ST == 0
*Generate variables
gen Fall_OSscore_Cntrd2 = Fall_OSscore_Cntrd^2
gen Fall_OSscore_Cntrd3 = Fall_OSscore_Cntrd^3

*Summary of variable
summarize Fall_OSscore_Cntrd
* Generate Placebo Cutoff Indicators
gen placebo_cut_2_pos = (Fall_OSscore_Cntrd <  0.5)
gen placebo_cut_2_neg = (Fall_OSscore_Cntrd < -0.5)
gen placebo_cut_0_4_pos = (Fall_OSscore_Cntrd <  1.5)
gen placebo_cut_0_4_neg = (Fall_OSscore_Cntrd < -1.5)
*Checking on the placebo cuts
tab placebo_cut_2_pos
tab placebo_cut_2_neg
tab placebo_cut_0_4_pos
tab placebo_cut_0_4_neg

*Regression to download
ivregress 2sls G3_zscore (GotRRfirst = placebo_cut_2_pos) ///
    Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 ///
    c.Fall_OSscore_Cntrd#c.GotRRfirst c.Fall_OSscore_Cntrd2#c.GotRRfirst, robust
outreg2 using placebo_rdd_results.doc, replace ctitle("Placebo +0.5") label

ivregress 2sls G3_zscore (GotRRfirst = placebo_cut_2_neg) ///
    Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 ///
    c.Fall_OSscore_Cntrd#c.GotRRfirst c.Fall_OSscore_Cntrd2#c.GotRRfirst, robust
outreg2 using placebo_rdd_results.doc, append ctitle("Placebo -0.5") label

ivregress 2sls G3_zscore (GotRRfirst = placebo_cut_0_4_pos) ///
    Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 ///
    c.Fall_OSscore_Cntrd#c.GotRRfirst c.Fall_OSscore_Cntrd2#c.GotRRfirst, robust
outreg2 using placebo_rdd_results.doc, append ctitle("Placebo +1.5") label

ivregress 2sls G3_zscore (GotRRfirst = placebo_cut_0_4_neg) ///
    Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 ///
    c.Fall_OSscore_Cntrd#c.GotRRfirst c.Fall_OSscore_Cntrd2#c.GotRRfirst, robust
outreg2 using placebo_rdd_results.doc, append ctitle("Placebo -1.5") label


*Run Placebo RDDs
ivregress 2sls G3_zscore (GotRRfirst = placebo_cut_2_pos) ///
    Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 ///
    c.Fall_OSscore_Cntrd#c.GotRRfirst c.Fall_OSscore_Cntrd2#c.GotRRfirst, robust

ivregress 2sls G3_zscore (GotRRfirst = placebo_cut_2_neg) ///
    Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 ///
    c.Fall_OSscore_Cntrd#c.GotRRfirst c.Fall_OSscore_Cntrd2#c.GotRRfirst, robust

ivregress 2sls G3_zscore (GotRRfirst = placebo_cut_0_4_pos) ///
    Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 ///
    c.Fall_OSscore_Cntrd#c.GotRRfirst c.Fall_OSscore_Cntrd2#c.GotRRfirst, robust

ivregress 2sls G3_zscore (GotRRfirst = placebo_cut_0_4_neg) ///
    Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 ///
    c.Fall_OSscore_Cntrd#c.GotRRfirst c.Fall_OSscore_Cntrd2#c.GotRRfirst, robust





	

*** Non parametric fuzzy rdd
* Triangular Kernel 
rdrobust G3_zscore Fall_OSscore_Cntrd, fuzzy(GotRRfirst) kernel(triangular)
scalar mse_triangular = e(MSE)
* Because the first stage is negative, we try to fix the bandwidths
rdrobust G3_zscore Fall_OSscore_Cntrd, fuzzy(GotRRfirst) kernel(triangular) h(1)
*Still negative first stage: this suggests that the non-parametric approach is estimating a very different local effect than the global OLS model.
*Adding degree 2 to the local regression
rdrobust G3_zscore Fall_OSscore_Cntrd, fuzzy(GotRRfirst) kernel(triangular) p(2)
*Still negative first stage, with error of mass points detected
rdrobust G3_zscore Fall_OSscore_Cntrd, fuzzy(GotRRfirst) kernel(triangular) cDensity
*Mass Points in Fall_OSscore_Cntrd: The running variable has too many repeated values, the local polynomial regression therefore behaves unexpectedly.
*Because the running variable has many mass points (discrete values), it behaves more like a step function rather than a continuous running variable. This makes traditional RDD assumptions (continuity of potential outcomes at the cutoff) questionable.

*** Kernel sensitivity
* Epanechnikov Kernel
rdrobust G3_zscore Fall_OSscore_Cntrd, fuzzy(GotRRfirst) kernel(epanechnikov) bwselect(mserd) vce(nn)
scalar mse_epa = e(MSE)
* Uniform Kernel
rdrobust G3_zscore Fall_OSscore_Cntrd, fuzzy(GotRRfirst) kernel(uniform)
scalar mse_uniform = e(MSE)

*** BW Sensitivity
* Default bandwidths for reference
rdrobust G3_zscore Fall_OSscore_Cntrd, fuzzy(GotRRfirst) kernel(epanechnikov)
* Test larger bandwidths
rdrobust G3_zscore Fall_OSscore_Cntrd, fuzzy(GotRRfirst) kernel(epanechnikov) h(0.05)
rdrobust G3_zscore Fall_OSscore_Cntrd, fuzzy(GotRRfirst) kernel(epanechnikov) h(0.07)
* Test smaller bandwidths
rdrobust G3_zscore Fall_OSscore_Cntrd, fuzzy(GotRRfirst) kernel(epanechnikov) h(0.02)
rdrobust G3_zscore Fall_OSscore_Cntrd, fuzzy(GotRRfirst) kernel(epanechnikov) h(0.01)


*** PLACEBO CUT OFFS
***Placebo Cutoff 0.45
* Create bins for the running variable
egen bin = cut(Fall_OSscore_Cntrd), at(-5(0.3)5) 

* Calculate the average of the outcome within each bin
bysort bin (Fall_OSscore_Cntrd): egen bin_avg = mean(G3_zscore)

* Scatter plot of binned averages with lowess fits
twoway ///
    (scatter bin_avg bin, ///
        mcolor(black) msymbol(circle) legend(label(1 "Binned Averages")) ) ///
    (lowess G3_zscore Fall_OSscore_Cntrd if Fall_OSscore_Cntrd < 0.45, ///
        bwidth(0.2) lcolor(blue) legend(label(2 "Lowess Fit Left"))) ///
    (lowess G3_zscore Fall_OSscore_Cntrd if Fall_OSscore_Cntrd >= 0.45, ///
        bwidth(0.2) lcolor(red) legend(label(3 "Lowess Fit Right"))), ///
    scheme(s1color) ///
    ytitle("G3 Z-Score") ///
    xtitle("Centered Fall OS Score") ///
    title("Placebo Cut = 0.45 with Lowess Fit") ///
    xline(0.45, lcolor(black) lpattern(dash)) ///
    legend(order(1 2 3) label(1 "Binned Averages") label(2 "Lowess Fit Left") label(3 "Lowess Fit Right"))
* Generate a placebo treatment indicator for the alternative cutoff
gen placebo_treat = Fall_OSscore_Cntrd >= 0.45
* Run RDD for the placebo cutoff using rdrobust
rdrobust G3_zscore Fall_OSscore_Cntrd, fuzzy(placebo_treat) c(0.45) kernel(triangular)

***Placebo Cutoff -2
* Step 1: Create bins for the running variable
egen bin = cut(Fall_OSscore_Cntrd), at(-5(0.3)5) 
* Step 2: Calculate the average of the outcome within each bin
bysort bin (Fall_OSscore_Cntrd): egen bin_avg = mean(G3_zscore)
* Step 3: Scatter plot of binned averages with lowess fits
twoway ///
    (scatter bin_avg bin, ///
        mcolor(black) msymbol(circle) legend(label(1 "Binned Averages")) ) ///
    (lowess G3_zscore Fall_OSscore_Cntrd if Fall_OSscore_Cntrd < -2, ///
        bwidth(0.2) lcolor(blue) legend(label(2 "Lowess Fit Left"))) ///
    (lowess G3_zscore Fall_OSscore_Cntrd if Fall_OSscore_Cntrd >= -2, ///
        bwidth(0.2) lcolor(red) legend(label(3 "Lowess Fit Right"))), ///
    scheme(s1color) ///
    ytitle("G3 Z-Score") ///
    xtitle("Centered Fall OS Score") ///
    title("Placebo Cut = -2 with Lowess Fit") ///
    xline(0.45, lcolor(black) lpattern(dash)) ///
    legend(order(1 2 3) label(1 "Binned Averages") label(2 "Lowess Fit Left") label(3 "Lowess Fit Right"))
* Generate a placebo treatment indicator for the alternative cutoff
gen placebo_treat = Fall_OSscore_Cntrd >= -2
* Run RDD for the placebo cutoff using rdrobust
rdrobust G3_zscore Fall_OSscore_Cntrd, fuzzy(placebo_treat) c(-2) kernel(triangular)




