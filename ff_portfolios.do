/*
ff_portfolios.do


Can you create a dataset that links firms to the appropriate Fama-French portfolios?
	-https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
	-you will compute the firm's value(s) based on the portfolio,
		and match them with the correct bin
	-You can define the portfolio variable based on the names on the website
		e.g. the size portfolios are just decile 1, decile 2, decile 3, etc.
		The 6 factor size and boot to market are called: small lo bm; small 2, small hi bm,
		and are defined in the descriptions along with the cutoff points where relevant.
	-They already have the annual portfolio returns so that doesn't need to be calculated
		and you have already created annual returns at the firm level so I think all that
		is left is assigning the firms to the appropriate portfolio.

Create the following matches (in one dataset or separate ones):
	1. size decile
	2. size and book to market 6 portfolio
	3. size and boot to market 25 portfolio
	4. size, book to market and operating profitability 32 portfolio
	5. size and 48 industry portfolio

If you click on the details for any of the portfolios it says when and how they create
	the groups and what cutoff points they use but they pretty much do the same thing
	every time just with a different number of portfolios.
*/


clear all
cap log close
pause on

global repo "C:/Users/lmostrom/Documents\GitHub\abnormal_returns"
cap cd "C:\Users\lmostrom\Dropbox\Abnormal_Returns"

local import 1 // import portfolio returns CSVs and breakpoint CSVs
local cpu_merge 1 // merge with Compustat-CRSP pre-merged dataset
local brkpt_merge 1 // merge with percentile breakpoints
local ff_merge 1 // merge with Fama-French portfolio returns datasets

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*---------------------
if `import' == 1 {
*---------------------
	* ===========================================================================
	* 							PORTFOLIO RETURNS CSVS
	* ===========================================================================
	cd FamaFrench
	*------------------------------------
	* Size Decile Portfolios
	*------------------------------------
	import delimited "Portfolios_Formed_on_ME.csv", ///
			rowr(2257:2349) varn(2257) clear // Value Weighted, Annual
		keep v1 lo10-hi10
		ren v1 fyear
		ren lo10 me10
		ren dec? me?0
		ren hi10 me100
		ren me* me*_vw

		save "portfolio_returns_10_ME_vw.dta", replace

	import delimited "Portfolios_Formed_on_ME.csv", ///
			rowr(2353:2445) varn(2353) clear // Value Weighted, Annual
		keep v1 lo10-hi10
		ren v1 fyear
		ren lo10 me10
		ren dec? me?0
		ren hi10 me100
		ren me* me*_eqw

		save "portfolio_returns_10_ME_eqw.dta", replace

	*------------------------------------
	* Size & BE/ME 6 Group Portfolios
	*------------------------------------
	import delimited "6_Portfolios_2x3.csv", ///
			rowr(2260:2352) varn(2260) clear // Value Weighted, Annual
		ren v1 fyear
		ren small* me50* // renaming in terms of percentiles
		ren big* me100*
		ren *lobm *bm30
		ren *hibm *bm100
		ren me1bm2 me50bm70
		ren me2bm2 me100bm70
		ren me* me*_vw

		save "portfolio_returns_6_ME_BM_vw.dta", replace

	import delimited "6_Portfolios_2x3.csv", ///
			rowr(2356:2448) varn(2356) clear // Equal Weighted, Annual
		ren v1 fyear
		ren small* me50* // renaming in terms of percentiles
		ren big* me100*
		ren *lobm *bm30
		ren *hibm *bm100
		ren me1bm2 me50bm70
		ren me2bm2 me100bm70
		ren me* me*_eqw

		save "portfolio_returns_6_ME_BM_eqw.dta", replace

	*------------------------------------
	* Size & BE/ME 25 Group Portfolios
	*------------------------------------
	import delimited "25_Portfolios_5x5.csv", ///
			rowr(2260:2352) varn(2260) clear // Value Weighted, Annual
		ren v1 fyear
		ren small* me20* // renaming in terms of percentiles
		ren big* me100*
		ren *lobm *bm20
		ren *hibm *bm100
		forval x = 1/5 {
			local p = `x' * 20
			ren me`x'bm* me`p'bm*
			ren *bm`x' *bm`p'
		}
		ren me* me*_vw

		save "portfolio_returns_25_ME_BM_vw.dta", replace

	import delimited "25_Portfolios_5x5.csv", ///
			rowr(2356:2448) varn(2356) clear // Equal Weighted, Annual
		ren v1 fyear
		ren small* me20* // renaming in terms of percentiles
		ren big* me100*
		ren *lobm *bm20
		ren *hibm *bm100
		forval x = 1/5 {
			local p = `x' * 20
			ren me`x'bm* me`p'bm*
			ren *bm`x' *bm`p'
		}
		ren me* me*_eqw

		save "portfolio_returns_25_ME_BM_eqw.dta", replace

	*----------------------------------------------
	* Size & BE/ME & Op Profit 32 Group Portfolios
	*----------------------------------------------
	import delimited "32_Portfolios_ME_BEME_OP_2x4x4.csv", ///
			rowr(1374:1429) varn(1374) clear // Value Weighted, Annual
		ren v1 fyear
		ren small* me50* // renaming in terms of percentiles
			ren me1bm* me50bm*
		ren big* me100*
			ren me2bm* me100bm*
		ren *lobm* *bm25*
		ren *hibm* *bm100*
		ren *loop *op25
		ren *hiop *op100
		forval x = 1/4 {
			local p = `x' * 25
			ren *bm`x'op* *bm`p'op*
			ren *op`x' *op`p'
		}
		ren me* me*_vw

		save "portfolio_returns_32_ME_BM_OP_vw.dta", replace

	import delimited "32_Portfolios_ME_BEME_OP_2x4x4.csv", ///
			rowr(1433:1488) varn(1433) clear // Equal Weighted, Annual
		ren v1 fyear
		ren small* me50* // renaming in terms of percentiles
			ren me1bm* me50bm*
		ren big* me100*
			ren me2bm* me100bm*
		ren *lobm* *bm25*
		ren *hibm* *bm100*
		ren *loop *op25
		ren *hiop *op100
		forval x = 1/4 {
			local p = `x' * 25
			ren *bm`x'op* *bm`p'op*
			ren *op`x' *op`p'
		}
		ren me* me*_eqw

		save "portfolio_returns_32_ME_BM_OP_eqw.dta", replace

	*------------------------------------------
	* Fama-French Industry 48 Group Portfolios
	*------------------------------------------
	import delimited "48_Industry_Portfolios.csv", ///
			rowr(2256:2348) varn(2256) clear // Value Weighted, Annual
		ren v1 fyear

		local i = 1
		foreach var of varlist agric-other {
			ren `var' ind`i'
			local ++i
		}
		ren ind* ind*_vw

		save "portfolio_returns_48_Ind_vw.dta", replace

	import delimited "48_Industry_Portfolios.csv", ///
			rowr(2352:2444) varn(2352) clear // Equal Weighted, Annual
		ren v1 fyear

		local i = 1
		foreach var of varlist agric-other {
			ren `var' ind`i'
			local ++i
		}
		ren ind* ind*_eqw

		save "portfolio_returns_48_Ind_eqw.dta", replace


	* ===========================================================================
	* 						PERCENTILE BREAKPOINTS CSVS
	* ===========================================================================
	*------------------
	* Size Percentiles
	*------------------
	import delimited "ME_Breakpoints.csv", rowr(2:1126) varn(nonames) clear
		ren v1 v
		split v, p(", ") destring
		drop v v2 // full string and some kind of sample size
		ren v1 fyear

		local k = 5
		foreach var of varlist v* {
			ren `var' ME_pct`k'
			local k = `k' +5
		}

		keep if mod(fyear, 100) == 12 // December only since price from Dec. closing
		replace fyear = int(fyear/100)
		isid fyear

		save "ME_breakpoints.dta", replace

	*----------------------------
	* Book-to-Market Percentiles
	*----------------------------
	import delimited "BE-ME_Breakpoints.csv", rowr(3:97) varn(3) clear
		ren v1 fyear
		drop v2 v3 // sample sizes

		local k = 5
		foreach var of varlist v* {
			ren `var' BM_pct`k'
			local k = `k' +5
		}
		isid fyear

		save "BE-ME_breakpoints.dta", replace

	*-------------------------------------
	* Operating Profitability Percentiles
	*-------------------------------------
	import delimited "OP_Breakpoints.csv", rowr(4:61) varn(4) clear
		ren v1 fyear
		drop count // sample sizes

		local k = 5
		foreach var of varlist v* {
			ren `var' OP_pct`k'
			local k = `k' +5
		}
		isid fyear

		save "OP_breakpoints.dta", replace

	cd ../
*--------------------------
} // end import section
*--------------------------
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*---------------------
if `cpu_merge' == 1 {
*---------------------
	* ------- Get Compustat-CRSP to be unique at lpermno-fyear level ------- *
	use "../Compustat-CRSP_Merged_Annual.dta", clear
	duplicates tag lpermno fyear, gen(dup) // some are not unique because
											// they change gvkeys mid-year
	bys lpermno: egen max_date = max(datadate) if dup
		drop if dup == 1 & datadate < max_date
	xtset lpermno fyear
	tempfile compucrspA
	save `compucrspA', replace
	* ---------------------------------------------------------------------- *

	*=========================================================================================
	* MERGE IN COMPUSTAT VARIABLES FOR SIZE, BE/ME, AND OPERATING PROFITABILITY
	*=========================================================================================
	use "returns_annualized.dta", clear

	keep if n_mons == 12
	keep if max_mon == 12
	keep if month == max_mon
	keep permno year date retA

	*Prepare to merge with the Merged Compustat-CRSP dataset
	ren permno lpermno
	ren year fyear
	/*	gen fyear = year if month > 6
		replace fyear = year - 1 if month <= 6 */

	* First merge in income statement variables and closing prices (year t)
	merge 1:1 lpermno fyear using `compucrspA', keep(1 3) gen(cpu_merge) ///
		keepus(sale cogs xsga xint sic gvkey)

		lab var sale "Sales ($ MM)"
		lab var cogs "Cost of Goods Sold ($ MM)"
		lab var xsga "Selling, General & Admin Expense ($ MM)"
		lab var xint "Interest Expense ($ MM)"

	* Now set fyear back 1, merge in balance sheet variables (year t-1),
	*	and move fyear back up to t
	replace fyear = fyear - 1
	merge 1:1 lpermno fyear using `compucrspA', keep(1 3) nogen	keepus(prcc_c csho seq)

		lab var prcc_c "Price, Closing - Annual (Calendar Year)"
		lab var csho "Common Shares Outstanding (year t-1) (MM)"
		lab var seq "Stockholders' Equity - Total (year t-1) ($ MM)"
	replace fyear = fyear + 1


	* Now merge in 48 Fama-French industries
	merge m:1 sic using "../FamaFrench48.dta", gen(ff_merge) keep(1 3)
	tab sic if ff_merge == 1
		/* Filling in SIC codes not assigned by the Fama French 48 industry document
		based on https://www.eeoc.gov/eeoc/statistics/employment/jobpat-eeo1/siccodes.cfm */
		* Fishing, hunting, trapping
		replace ff48 = 6 if ff48 == . & sic == 900
			replace ff48_name = "Recreation" if ff48 == 6 & ff48_name == ""
		* Miscellaneous Manufactures, Unknown
		replace ff48 = 48 if ff48 == . & inlist(sic, 3990, 6797, 9995, 9997)
			replace ff48_name = "Other" if ff48 == 48 & ff48_name == ""
		assert ff48 != . if sic != .

	*-------------------------------------------------------------------
	* Now calculate ME, BE/ME, and Operating Profitability
	gen ME = prcc_c*csho // = Price (Close) * Common Shares Outstanding
		lab var ME "Market Value of Equity ($ MM)"
	keep if ME > 0

	keep if seq > 0 // they said they used only firms with book equity >0
	gen BEtoME = seq/ME // book value of equity over market value of equity
		lab var BEtoME "Book-to-Market"

	*only calculate OP if not missing sales & at least one expense var
	egen has_costs = rownonmiss(cogs xsga xint)
		replace cogs = 0 if cogs == . & has_costs >= 1
		replace xsga = 0 if xsga == . & has_costs >= 1
		replace xint = 0 if xint == . & has_costs >= 1

	gen OP = (sale - cogs - xsga - xint)/seq if sale != . & has_costs >= 1

		lab var OP "Operating Profitability"

	save "returns_annualized_wME_BM_OP.dta", replace

*--------------------------
} // end cpu_merge section
*--------------------------
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*---------------------
if `brkpt_merge' == 1 {
*---------------------
	if `cpu_merge' == 0 use "returns_annualized_wME_BM_OP.dta", clear

	* --- Market Value of Equity --- *
	merge m:1 fyear using "FamaFrench/ME_breakpoints", nogen keep(3)
	assert ME >= 0

	gen ME_pct = 5 if inrange(ME, 0, ME_pct5)
	forval j = 10(5)100 {
		local i = `j' - 5
		replace ME_pct = `j' if inrange(ME, ME_pct`i', ME_pct`j')
	}
	
	drop ME_pct?*

	* --- Book-to-Market --- *
	merge m:1 fyear using "FamaFrench/BE-ME_breakpoints", nogen keep(3)

	gen BM_pct = 5 if inrange(BEtoME, 0, BM_pct5)
	forval j = 10(5)100 {
		local i = `j' - 5
		replace BM_pct = `j' if inrange(BEtoME, BM_pct`i', BM_pct`j')
	}
	
	drop BM_pct?*

	* --- Operating Profitability --- *
	merge m:1 fyear using "FamaFrench/OP_breakpoints", nogen keep(3)

	replace OP = OP*100

	gen OP_pct = 5 if inrange(OP, 0, OP_pct5)
	forval j = 10(5)100 {
		local i = `j' - 5
		replace OP_pct = `j' if inrange(OP, OP_pct`i', OP_pct`j')
	}
	
	drop OP_pct?*


**** Making plots of how these observations are distributed in terms of percentiles ****
	/*	-the size (ME) plot is skewed right because percentiles are calculated based on NYSE
			firms, but NASDAQ firms are included in the dataset as well (and they are on
			average much smaller). That's why the deciles from Fama-French don't reflect even
			groups covering 10% of the sample in each one. */
	cap mkdir "Distribution_Plots"

	qui summ ME_pct
		local N = r(N)
	hist ME_pct, bin(20) percent fcol(eltblue) lcol(navy) title("Market Value of Equity") ///
		xti("Percentile Bin") yti("(%)") subtitle("N = `N'") yline(5, lc(gs8))
	graph export "Distribution_Plots/ME.png", replace as(png) wid(1200) hei(700)

	qui summ BM_pct
		local N = r(N)
	hist BM_pct, bin(20) percent fcol(eltblue) lcol(navy) title("Book-to-Market") ///
		xti("Percentile Bin") yti("(%)") subtitle("N = `N'") yline(5, lc(gs8))
	graph export "Distribution_Plots/BM.png", replace as(png) wid(1200) hei(700)

	qui summ OP_pct
		local N = r(N)
	hist OP_pct, bin(20) percent fcol(eltblue) lcol(navy) title("Operating Profitability") ///
		xti("Percentile Bin") yti("(%)") subtitle("N = `N'") yline(5, lc(gs8))
	graph export "Distribution_Plots/OP.png", replace as(png) wid(1200) hei(700)

*--------------------------
} // end brkpt_merge section
*--------------------------
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*---------------------
if `ff_merge' == 1 {
*---------------------

	* --- First merge in size decile portfolios --- *
	foreach wt in "eqw" "vw" {
		merge m:1 fyear using "FamaFrench/portfolio_returns_10_ME_`wt'.dta", nogen keep(3)

		if "`wt'" == "eqw" { // just do it the first time
			gen pf_10_ME_name = "me10" if ME_pct <= 10
			forval j = 20(10)100 {
				local i = `j' - 5
				replace pf_10_ME_name = "me`j'" if inlist(ME_pct, `i', `j')
			}
		}
		gen pf_10_ME_ret_`wt' = .
		foreach var of varlist *_`wt' {
			replace pf_10_ME_ret_`wt' = `var' if "`var'" == pf_10_ME_name + "_`wt'"
		}
		drop me*_`wt'
	}
	
	* --- Next merge in Size-B/M 2x3 portfolios --- *
	foreach wt in "eqw" "vw" {
		merge m:1 fyear using "FamaFrench/portfolio_returns_6_ME_BM_`wt'.dta", nogen keep(3)

		if "`wt'" == "eqw" { // just do it the first time
			gen pf_6_ME_BM_name = "me50" if ME_pct <= 50
				replace pf_6_ME_BM_name = "me100" if inrange(ME_pct, 55, 100)
				replace pf_6_ME_BM_name = pf_6_ME_BM_name + "bm30" if BM_pct <= 30
				replace pf_6_ME_BM_name = pf_6_ME_BM_name + "bm70" if inrange(BM_pct, 35, 70)
				replace pf_6_ME_BM_name = pf_6_ME_BM_name + "bm100" if inrange(BM_pct, 75, 100)
		}
		gen pf_6_ME_BM_ret_`wt' = .
		foreach var of varlist *_`wt' {
			replace pf_6_ME_BM_ret_`wt' = `var' if "`var'" == pf_6_ME_BM_name + "_`wt'"
		}
		drop me*_`wt'
	}

	* --- Next merge in Size-B/M 5x5 portfolios --- *
	foreach wt in "eqw" "vw" {
		merge m:1 fyear using "FamaFrench/portfolio_returns_25_ME_BM_`wt'.dta", nogen keep(3)

		if "`wt'" == "eqw" { // just do it the first time
			gen pf_25_ME_BM_name = "me20" if ME_pct <= 20
			replace pf_25_ME_BM_name = pf_25_ME_BM_name + "bm20" if BM_pct <= 20
			forval j = 40(20)100 {
				local i = `j' - 15
				replace pf_25_ME_BM_name = "me`j'" + pf_25_ME_BM_name if inrange(ME_pct, `i', `j')
				replace pf_25_ME_BM_name = pf_25_ME_BM_name + "bm`j'" if inrange(BM_pct, `i', `j')
			}
		}
		gen pf_25_ME_BM_ret_`wt' = .
		foreach var of varlist *_`wt' {
			replace pf_25_ME_BM_ret_`wt' = `var' if "`var'" == pf_25_ME_BM_name + "_`wt'"
		}
		drop me*_`wt'
	}

	* --- Next merge in Size-B/M-OP 2x4x4 portfolios --- *
	foreach wt in "eqw" "vw" {
		merge m:1 fyear using "FamaFrench/portfolio_returns_32_ME_BM_OP_`wt'.dta", nogen keep(3)

		if "`wt'" == "eqw" { // just do it the first time
			gen pf_32_ME_BM_name = "me50" if ME_pct <= 50
			replace pf_32_ME_BM_name = "me100" if inrange(ME_pct, 55, 100)
			forval j = 25(25)100 {
				local i = `j' - 20
				replace pf_32_ME_BM_name = pf_32_ME_BM_name + "bm`j'" if inrange(BM_pct, `i', `j')
			}
			forval j = 25(25)100 {
				local i = `j' - 20
				replace pf_32_ME_BM_name = pf_32_ME_BM_name + "op`j'" if inrange(OP_pct, `i', `j')
			}
		}
		gen pf_32_ME_BM_ret_`wt' = .
		foreach var of varlist *_`wt' {
			replace pf_32_ME_BM_ret_`wt' = `var' if "`var'" == pf_32_ME_BM_name + "_`wt'"
		}
		drop me*_`wt'
	}	

	* --- Next merge in 48 Industry portfolios --- *
	foreach wt in "eqw" "vw" {
		merge m:1 fyear using "FamaFrench/portfolio_returns_48_Ind_`wt'.dta", nogen keep(3)

		gen pf_48_Ind_ret_`wt' = .
		forval i = 1/48 {
			replace pf_48_Ind_ret_`wt' = ind`i'_`wt' if ff48 == `i'
		}
		drop ind*_`wt'
	}


	*----------------------------------------------------------
	* Now save dataset of annual returns and portfolio returns
	*----------------------------------------------------------
	keep gvkey lpermno fyear retA pf_*
		replace retA = retA*100
	order gvkey lpermno fyear retA pf_10* pf_6* pf_25* pf_32* pf_48*
	sort lpermno fyear

	save "returns_annualized_wFF_portfolios.dta", replace

*--------------------------
} // end ff_merge section
*--------------------------