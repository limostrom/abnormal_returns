/*
data_clean.do

CRSP monthly stock file
	CRSP/Annual update/Stock/Security files/CRSP monthly stock
	date range is 1980-2018
	variables of interest include:
	Permno (just a firm identifier)
	Date (automatically included)
	Holding period return (variable name 'ret')
	Delisting code (dlstcd)
Fama French factors
	-Fama French & Liquidity Factors/Fama-French Portfolios/Fama-French Factors - Monthly Frequency
	date range is 1980-2018
	download all variables (mktrf, smb, hml, umd)
	-Fama French & Liquidity Factors/Liquidity Factors/Pastor-Stambaugh Liquidity Factors
	date range is 1980-2017
	download traded liquidity factor (ps_vwf)

-merge the two files by date
-compute compounded values for all measures
-estimate several regression models by year and save the residuals for each firm year
-we would like to estimate:
	-CAPM
	-Fama-French 3 factor model
	-Carhart 4 factor model
	-5 factor model with a liquidity factor
-once the residuals are computed, create binary measures for whether they are in the top 10% of values or the bottom 10% of values
-the final dataset can include the permno, year, and the residual under each model for all available firm years

*/

clear all
cap log close
pause on

local import 0
local clean 1
local resids 0

global repo "../Documents\GitHub\abnormal_returns"

cap cd "C:\Users\lmostrom\Dropbox\"

*------------------
if `import' == 1 {
*------------------
	*--------------------------------------------------------
	*Import and save Fama French Liquidity Factor
	*--------------------------------------------------------
	import delimited "PS_LiqFactors.csv", clear varn(1)
		lab var ps_vwf "Pastor-Stambaugh Traded Liquidity Factor (value-weighted)"

	save "FamaFrench_Liquidity.dta", replace
	*--------------------------------------------------------
	*Import and save Fama French dataset
	*--------------------------------------------------------
	import delimited "FamaFrench_all.csv", clear varn(1)
		ren dateff date
		lab var mktrf "Excess Return on the Market"
		lab var smb "Small Minus Big"
		lab var hml "High Minus Low"
		lab var rf "Risk-Free Interest Rate (1-Month T Bill Rate)"
		lab var umd "Momentum"
	merge 1:1 date using "FamaFrench_Liquidity.dta", nogen assert(1 3)
	save "FamaFrench_Monthly.dta", replace

	tostring date, replace // currently integer YYYYMMDD
	gen year = substr(date, 1, 4)
		destring year, replace
	gen month = substr(date, 5, 2)
		destring month, replace
	gen datem = ym(year, month) // date variable stored as month
		format datem %tm
	tsset datem

	foreach ret_fac in mktrf smb hml umd ps_vwf {
		gen `ret_fac'_plus1 = `ret_fac' + 1
		gen `ret_fac'_cumul = `ret_fac'_plus1 if month == 1
		sort datem
		forval x = 2/12 { // calculate cumulative return at that month of the year so far
			replace `ret_fac'_cumul = l.`ret_fac'_cumul*`ret_fac'_plus1 if month == `x'
		}
		replace `ret_fac'_cumul = `ret_fac'_cumul - 1
		gen `ret_fac'A = `ret_fac'_cumul if month == 12 // the annual return value
			bys year: ereplace `ret_fac'A = max(`ret_fac'A)
	}

	save "FamaFrench.dta", replace
	*--------------------------------------------------------
	*Import and save CRSP dataset
	*--------------------------------------------------------
	import delimited "CRSP_ret_dlstcd.csv", clear varn(1)
		lab var dlstcd "Delisting Code"
		lab var ret "Holding Period Return"

		keep if inlist(dlstcd, ., 100) // drop if dlstcd not missing or == 100
		destring ret, replace force // contains letters for missing returns
			drop if ret == .

	save "CRSP_ret_dlstcd.dta", replace
	*--------------------------------------------------------
} // end import section
*------------------------------------------------------------
if `import' == 0 & `clean' == 1 use permno date ret dlstcd using "CRSP_ret_dlstcd.dta", clear
*=======================================================================
*------------------
if `clean' == 1 {
*------------------
	tostring date, replace
	merge m:1 date using "FamaFrench.dta", nogen assert(3) ///
		keepus(year month datem *A)

	xtset permno datem

	gen ret_plus1 = ret + 1 // to compound monthly returns into annual
	bys permno year: egen n_mons = count(month)
			// so return can be scaled up if fewer than 12 months of returns in data
		bys permno year: egen min_mon = min(month)
		bys permno year: egen max_mon = max(month)
		gen ret_cumul = ret_plus1 if month == min_mon
		sort permno datem
		forval x = 2/12 { // calculate cumulative return at that month of the year so far
			replace ret_cumul = l.ret_cumul*ret_plus1 if month == `x' & month > min_mon
		}
		gen retA = ret_cumul^(12/n_mons) // compound to 12 months based on number
									// of months already factored in (e.g. 6-month
									// returns should be squared)
		replace ret_cumul = ret_cumul - 1 // want r not 1+r
		replace retA = retA - 1

	xtset permno datem
	save "Abnormal_Returns/returns_annualized.dta", replace
} // end clean section
if `clean' == 0 & `resids' == 1 use "Abnormal_Returns/returns_annualized.dta", clear

*======================================================================
* NOW SAVE RESIDUALS FOR FIRM-YEAR ABNORMAL RETURNS DATASET
*======================================================================
if `resids' == 1 {
*-------------------
	foreach A in "" "A" { // first using monthly returns then annualized returns

	*winsor ret`A', p(0.025) gen(retA_w)
	qui summ ret`A', d
		local pct01 = r(p1)
	qui summ ret`A', d
		local pct99 = r(p99) // dropping top and bottom 1% of outlier returns

	foreach model in CAPM FF3 Carhart4 Liq5 {
		if "`model'" == "CAPM" {
			local rhs "mktrf`A' if inrange(ret`A', `pct01', `pct99')"
			local x = 1
		}
		if "`model'" == "FF3" {
			local rhs "mktrf`A' smb`A' hml`A' if inrange(ret`A', `pct01', `pct99')"
			local x = 3
		}
		if "`model'" == "Carhart4" {
			local rhs "mktrf`A' smb`A' hml`A' umd`A' if inrange(ret`A', `pct01', `pct99')"
			local x = 4
		}
		if "`model'" == "Liq5" {
			local rhs "mktrf`A' smb`A' hml`A' umd`A' ps_vwf`A' if inrange(ret`A', `pct01', `pct99') & year <= 2017"
				// liquidity only available up through 2017
			local x = 5
		}

		reg ret`A' `rhs' 
			predict residM`x'`A', residuals
	}

	if "`A'" == "" { // want average residuals for each firm-year based on monthly regs
		foreach n in 1 3 4 5 {
			bys permno year: ereplace residM`n' = mean(residM`n')
		}
		keep if month == max_mon
	}

	foreach model in M1 M3 M4 M5 {
		qui summ resid`model'`A' if resid`model'`A' != ., d
		gen win`model'`A' = (resid`model'`A' >= r(p90)) if resid`model'`A' != . // top 10% of alphas
		qui summ resid`model'`A' if resid`model'`A' != ., d
		gen lose`model'`A' = (resid`model'`A' <= r(p10)) if resid`model'`A' != . // bottom 10% of alphas
	}

	lab var residM1`A' "Residual from CAPM Model"
	lab var residM3`A' "Residual from Fama-French 3-Factor Model"
	lab var residM4`A' "Residual from Carhart 4-Factor Model"
	lab var residM5`A' "Residual from 5-Factor Model w/ Liquidity"

	lab var winM1`A' "Residual in top 10% of residuals"
		lab var winM3`A' "Residual in top 10% of residuals"
		lab var winM4`A' "Residual in top 10% of residuals"
		lab var winM5`A' "Residual in top 10% of residuals"
	lab var loseM1`A' "Residual in bottom 10% of residuals"
		lab var loseM3`A' "Residual in bottom 10% of residuals"
		lab var loseM4`A' "Residual in bottom 10% of residuals"
		lab var loseM5`A' "Residual in bottom 10% of residuals"

	} // end monthly/annualized loop

	keep permno year resid* win* lose*
	order permno year residM1 residM1A winM1 winM1A loseM1 loseM1A ///
					  residM3 residM3A winM3 winM3A loseM3 loseM3A ///
					  residM4 residM4A winM4 winM4A loseM4 loseM4A ///
					  residM5 residM5A winM5 winM5A loseM5 loseM5A

	save "Abnormal_Returns/firm_year_alphas.dta", replace

	*=======================================================================
	log using "Abnormal_Returns/log_ttest_monthly_annualized.txt", text replace

	foreach var in resid win lose {
		foreach n in 1 3 4 5 {
			ttest `var'M`n' == `var'M`n'A
		}
	}

	cap log close
} // end resids section
*=======================================================================