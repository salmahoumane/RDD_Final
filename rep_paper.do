use "/Users/salmahoumane/Downloads/df_regression.dta", clear
*** QUESTION 1: Plot the running variable against the treatment to confirm this is a fuzzy 
*** Running variable: Fall_OSscore_Cntrd
*** Treatment variable: GotRRfirst
*** Outcome Variable: "G3_zscore"  

* Generate bins for different resolutions
egen bin1 = cut(Fall_OSscore_Cntrd), at(-0.5(0.01)0.5) 
egen bin2 = cut(Fall_OSscore_Cntrd), at(-0.5(0.005)0.5) 
egen bin3 = cut(Fall_OSscore_Cntrd), at(-0.5(0.001)0.5)
egen bin5 = cut(Fall_OSscore_Cntrd), at(-0.5(0.06)0.5)


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

* Scatter plots for different bin widths
twoway (scatter mdif1 bin1), scheme(s1mono) ytitle("G3 Z-Score") xtitle("Fall OS Score (Centered)") title("Bin = 0.01") xline(0)
twoway (scatter mdif2 bin2), scheme(s1mono) ytitle("G3 Z-Score") xtitle("Fall OS Score (Centered)") title("Bin = 0.005") xline(0)
twoway (scatter mdif3 bin3), scheme(s1mono) ytitle("G3 Z-Score") xtitle("Fall OS Score (Centered)") title("Bin = 0.001") xline(0)
twoway (scatter mdif5 bin5), scheme(s1mono) ytitle("G3 Z-Score") xtitle("Fall OS Score (Centered)") title("Bin = 0.06") xline(0)


* Add smoothed fits with lpolyci
twoway (scatter mdif1 bin1) (lpolyci G3_zscore Fall_OSscore_Cntrd if Fall_OSscore_Cntrd < 0) (lpolyci G3_zscore Fall_OSscore_Cntrd if Fall_OSscore_Cntrd >= 0), ///
scheme(s1color) ytitle("G3 Z-Score") xtitle("Fall OS Score (Centered)") title("Bin = 0.01 with Smoothing") ///
xline(0) legend(label(1 "Means") label(2 "95% CI") label(3 "NP Fit") order(1 2 3))

** Testing the validity of fuzzy rdd
reg GotRRfirst Fall_OSscore_Cntrd


*** QUESTION 3: Check for a discontinuity in the covariates 

*** Plot the density of the assignment variable
hist Fall_OSscore_Cntrd, scheme(s1mono)

*** Test for a discontinuity in the density of the assignment variable
rddensity Fall_OSscore_Cntrd, c(0)
rddensity Fall_OSscore_Cntrd, c(0) plot

*** Parametric  Fuzzy RDD
*** Fuzzy RDD equation: Model Degree 2
** First stage
reg GotRRfirst Fall_BelowCut Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 ///
    c.Fall_OSscore_Cntrd#c.Fall_BelowCut c.Fall_OSscore_Cntrd2#c.Fall_BelowCut, robust

** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
    Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 ///
    c.Fall_OSscore_Cntrd#c.GotRRfirst c.Fall_OSscore_Cntrd2#c.GotRRfirst, robust
	
	*** Fuzzy RDD equation: Model Degree 1
** First stage
reg GotRRfirst Fall_BelowCut Fall_OSscore_Cntrd ///
    c.Fall_OSscore_Cntrd#c.Fall_BelowCut, robust

** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
    Fall_OSscore_Cntrd ///
    c.Fall_OSscore_Cntrd#c.GotRRfirst, robust
	
		*** Fuzzy RDD equation: Model Degree 3
gen Fall_OSscore_Cntrd2 = Fall_OSscore_Cntrd^2
gen Fall_OSscore_Cntrd3 = Fall_OSscore_Cntrd^3


** First stage
reg GotRRfirst Fall_BelowCut Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 Fall_OSscore_Cntrd3 ///
    c.Fall_OSscore_Cntrd#c.Fall_BelowCut c.Fall_OSscore_Cntrd2#c.Fall_BelowCut ///
    c.Fall_OSscore_Cntrd3#c.Fall_BelowCut, robust

** Second stage
ivregress 2sls G3_zscore (GotRRfirst = Fall_BelowCut) ///
    Fall_OSscore_Cntrd Fall_OSscore_Cntrd2 Fall_OSscore_Cntrd3 ///
    c.Fall_OSscore_Cntrd#c.GotRRfirst c.Fall_OSscore_Cntrd2#c.GotRRfirst ///
    c.Fall_OSscore_Cntrd3#c.GotRRfirst, robust



*** Non parametric fuzzy rdd
rdrobust G3_zscore Fall_OSscore_Cntrd, fuzzy(GotRRfirst) kernel(epanechnikov) bwselect(mserd) vce(nn)
scalar mse_epa = e(MSE)
*** Kernel sensitivity
* Triangular Kernel (default)
rdrobust G3_zscore Fall_OSscore_Cntrd, fuzzy(GotRRfirst) kernel(triangular)
scalar mse_triangular = e(MSE)

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


* ITT 
reg G3_zscore Fall_BelowCut Fall_OSscore_Cntrd, robust

***Placebo Cutoff 0.45

* Step 1: Create bins for the running variable
egen bin = cut(Fall_OSscore_Cntrd), at(-5(0.3)5) 

* Step 2: Calculate the average of the outcome within each bin
bysort bin (Fall_OSscore_Cntrd): egen bin_avg = mean(G3_zscore)

* Step 3: Scatter plot of binned averages with lowess fits
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

***Placebo Cutoff -1

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




