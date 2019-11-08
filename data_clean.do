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
local clean 0
local resids 0
local variance 0
local growth 0
local portfolio 1

global repo "C:\Users\lmostrom\Documents\GitHub\abnormal_returns"

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
	*Import and save Compustat Fundamentals Q dataset
	*--------------------------------------------------------
	import delimited "CompustatQ.csv", clear varn(1)
		lab var niq "Net Income - Quarterly ($ MM)"
		lab var oibdpq "Op. Income before Depreciation - Quarterly ($ MM)"

	save "CompustatQ.dta", replace
	*--------------------------------------------------------
	*Import and save Compustat-CRSP Fundamentals Q dataset
	*--------------------------------------------------------
	import delimited "Compustat-CRSP_Merged_Quarterly.csv", clear varn(1)
		lab var niq "Net Income - Quarterly ($ MM)"
		lab var oibdpq "Op. Income before Depreciation - Quarterly ($ MM)"

	save "Compustat-CRSP_Merged_Quarterly.dta", replace
	*--------------------------------------------------------
	*Import and save Compustat-CRSP Linking Table
	*--------------------------------------------------------
	import delimited "Compustat-CRSP_Link.csv", clear varn(1)
		destring linkenddt, replace force

	save "Compustat-CRSP_Link.dta", replace
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
	merge m:1 date using "FamaFrench.dta", nogen keep(3) ///
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
		gen retA = ret_cumul^(12/n_mons) if month == max_mon 
									// compound to 12 months based on number
									// of months already factored in (e.g. 6-month
									// returns should be squared)
			bys permno year (date): ereplace retA = max(retA)
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
	keep if year >= 1960

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
			local rhs "mktrf`A' smb`A' hml`A' umd`A' ps_vwf`A' if inrange(ret`A', `pct01', `pct99') & inrange(year, 1963, 2017)"
				// liquidity only available Aug 1962 - Dec 2017
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

*======================================================================
* CREATE A DATASET OF THE SD & SKEW OF MONTHLY RETURNS AND QTR INC VARS
*======================================================================
if `variance' == 1 {
*-------------------
	use gvkey lpermno linkdt linkenddt using "Compustat-CRSP_Link.dta", clear
		bys gvkey: gen n = _n
			qui summ n
			local X = r(max)
		reshape wide lpermno linkdt linkenddt, i(gvkey) j(n)
		tempfile link
		save `link', replace

	use gvkey datadate fyr niq oibdpq using CompustatQ, clear
		duplicates tag gvkey datadate, gen(dup)
			tab dup
			local dups = r(r) == 2
		while `dups' == 1 {
			bys gvkey (datadate): gen to_drop = (fyr != fyr[_n+2])
			drop if dup & to_drop
				drop dup to_drop
			duplicates tag gvkey datadate, gen(dup)
				tab dup
				local dups = r(r) == 2
				dis "`dups'"
		}

		
		gen lpermno = .
		merge m:1 gvkey using `link', nogen keep(3)
		forval x = 1/`X' {
			replace lpermno = lpermno`x' if inrange(datadate, linkdt`x', linkenddt`x') ///
				& lpermno == .
		}
		keep if lpermno != .
		tempfile cpu
		save `cpu', replace

	use "CRSP_ret_dlstcd.dta", clear
		ren permno lpermno
		ren date datadate

	merge 1:1 lpermno datadate using `cpu', keepus(niq oibdpq)
		gen year = int(datadate/10000) // first four digits
		keep if year >= 1960
		
		bys lpermno year: egen ret_sd = sd(ret)
		rangestat (skewness) ret_skew = ret, interval(year 0 0) by(lpermno)

		rangestat (sd) niq_sd = niq, interval(year -2 0) by(lpermno)
		rangestat (sd) oibdpq_sd = oibdpq, interval(year -2 0) by(lpermno)

	collapse (last) ret_sd ret_skew niq_sd oibdpq_sd, by(lpermno year) fast

	save "Abnormal_Returns/firm_year_sds_and_skewness.dta", replace

} // end variance section
*=======================================================================

*======================================================================
* COMPUTE FIRM- AND INDUSTRY-LEVEL COMPOUNDED RETURNS & GROWTH RATES
*======================================================================
if `growth' == 1 {
*-------------------
	use permno date year retA ///
		using "Abnormal_Returns/returns_annualized.dta", clear

	ren permno lpermno
	gen month = substr(date, 5, 2)
		destring month, replace

	merge 1:1 lpermno year month using "Compustat-CRSP_Merged_Annual.dta", ///
		keepus(sic aqc capx revt xsga do csho prcc_c) nogen keep(3)

	sort lpermno year month // sometimes more than one ob per firm-year
		collapse (last) retA sic aqc capx revt xsga do csho prcc_c, by(lpermno year) fast

	gen mktcap = prcc_c*csho
		lab var mktcap "Market Capitalization ($ MM)"

	merge m:1 sic using "FamaFrench48.dta", gen(ff_merge) keep(1 3)
		/* Filling in SIC codes not assigned by the Fama French 48 industry document
		based on https://www.eeoc.gov/eeoc/statistics/employment/jobpat-eeo1/siccodes.cfm */
		* Fishing, hunting, trapping
		replace ff48 = 6 if ff48 == . & sic == 900
			replace ff48_name = "Recreation" if ff48 == 6 & ff48_name == ""
		* Miscellaneous Manufactures, Unknown
		replace ff48 = 48 if ff48 == . & inlist(sic, 3990, 6797, 9995, 9997)
			replace ff48_name = "Other" if ff48 == 48 & ff48_name == ""
		assert ff48 != .

	bys ff48 year: egen ind_mktcap_tot = total(mktcap)
		gen wt = mktcap/ind_mktcap_tot
			bys ff48 year: egen check = total(wt) // just making sure the weights add to 1
			assert inrange(check, 0.999, 1.001)
			drop check

	foreach var of varlist retA aqc capx xsga do revt {
		if !inlist("`var'", "retA", "revt") replace `var' = 0 if `var' == .
		gen `var'_wtd = `var'*wt
			lab var `var'_wtd "`var' weighted by market cap share of industry"
		bys ff48 year: egen ind_vw_`var' = total(`var'_wtd)
			lab var ind_vw_`var' "Industry value-weighted `var'"
		bys ff48 year: egen ind_m_`var' = median(`var')
			lab var ind_m_`var' "Industry median `var'"
	}

	xtset lpermno year

	* --- Returns --- *
	gen firm_ret0 = retA
		lab var firm_ret0 "Firm return over this year"
	gen ind_vw_ret0 = ind_vw_retA
		lab var ind_vw_ret0 "Industry value-weighted return over this year"

	forval i = 1/10 {
		local i_1 = `i' - 1
		
		gen firm_ret`i' = (1 + firm_ret`i_1')*(1 + F`i'.retA) - 1 ///
			if F`i'.retA != .
			lab var firm_ret`i' "Firm return over this and the next `i' year(s)"

		gen ind_vw_ret`i' = (1 + ind_vw_ret`i_1')*(1 + F`i'.ind_vw_retA) - 1 ///
			if F`i'.ind_vw_retA != .
			lab var ind_vw_ret`i' "Industry value-weighted return over this and the next `i' year(s)"
	}

	* --- Revenue Growth --- *
	gen firm_rev_g0 = revt/l.revt // current year over past year
		lab var firm_rev_g0 "Revenue growth from last year to now"
	gen ind_vw_rev_g0 = ind_vw_revt/L.ind_vw_revt
		lab var ind_vw_rev_g0 "Industry value-weighted revenue growth from last year to now"
	bys ff48 year: egen ind_m_rev_g0 = median(firm_rev_g0)
		lab var ind_m_rev_g0 "Industry median revenue growth from last year to now"

	forval i = 1/10 {
		local i_1 = `i' - 1
		
		sort lpermno year
		gen firm_rev_g`i' = F`i'.revt/L.revt
			lab var firm_rev_g`i' "Revenue growth from last year to `i' year(s) from now"

		gen ind_vw_rev_g`i' = F`i'.ind_vw_revt/L.ind_vw_revt
			lab var ind_vw_rev_g`i' "Industry value-weighted revenue growth from last year to `i' year(s) from now"

		bys ff48 year: egen ind_m_rev_g`i' = median(firm_rev_g`i')
			lab var ind_m_rev_g`i' "Industry median revenue growth from last year to `i' year(s) from now"
	}

	* --- Forward Sums for Forward-Looking Ratios --- *
	sort lpermno year
	forval i = 0/10 {
		local i_1 = `i' - 1
		foreach pref in "" "ind_vw_" "ind_m_" {
			rangestat (sum) `pref'capx_fsum`i' = `pref'capx `pref'xsga_fsum`i' = `pref'xsga ///
							`pref'aqc_fsum`i' = `pref'aqc  	`pref'do_fsum`i' = `pref'do ///
							`pref'revt_fsum`i' = `pref'revt, interval(year 0 `i') by(lpermno)
			foreach var in capx xsga aqc do revt { // set missing if missing a year in the interval
				if `i' > 0 ///
					replace `pref'`var'_fsum`i' = . ///
							if (`pref'`var'_fsum`i' == `pref'`var'_fsum`i_1' & F`i'.`pref'`var' != 0)  ///
								| `pref'`var'_fsum`i_1' == .
			}
		}
	}

	forval i = 0/10 {
		* --- Capex + SG&A / Revenue --- *
		gen firm_capxsga_rev`i' = (capx_fsum`i' + xsga_fsum`i') / revt_fsum`i'
			lab var firm_capxsga_rev`i' "Capex + SG&A over Revenue (values summed over next `i' years)"

		gen ind_vw_capxsga_rev`i' = (ind_vw_capx_fsum`i' + ind_vw_xsga_fsum`i') / ind_vw_revt_fsum`i'
			lab var ind_vw_capxsga_rev`i' "Industry value-weighted Capex + SG&A over Revenue (values summed over next `i' yrs)"

		gen ind_m_capxsga_rev`i' = (ind_m_capx_fsum`i' + ind_m_xsga_fsum`i') / ind_m_revt_fsum`i'
			lab var ind_m_capxsga_rev`i' "Industry median Capex + SG&A over Revenue (values summed over next `i' yrs)"

		* --- Acquisitions / Revenue --- *
		gen firm_aqc_rev`i' = aqc_fsum`i' / revt_fsum`i'
			lab var firm_aqc_rev`i' "Acquisitions over Revenue (values summed over next `i' years)"

		gen ind_vw_aqc_rev`i' = ind_vw_aqc_fsum`i' / ind_vw_revt_fsum`i'
			lab var ind_vw_aqc_rev`i' "Industry value-weighted Acquisitions over Revenue (values summed over next `i' yrs)"

		gen ind_m_aqc_rev`i' = ind_m_aqc_fsum`i' / ind_m_revt_fsum`i'
			lab var ind_m_aqc_rev`i' "Industry median Acquisitions over Revenue (values summed over next `i' yrs)"

		* --- Discontinued Operations / Revenue --- *
		gen firm_do_rev`i' = do_fsum`i' / revt_fsum`i'
			lab var firm_do_rev`i' "Discontinued Ops over Revenue (values summed over next `i' years)"

		gen ind_vw_do_rev`i' = ind_vw_do_fsum`i' / ind_vw_revt_fsum`i'
			lab var ind_vw_do_rev`i' "Industry value-weighted Discontinued Ops over Revenue (values summed over next `i' yrs)"

		gen ind_m_do_rev`i' = ind_m_do_fsum`i' / ind_m_revt_fsum`i'
			lab var ind_m_do_rev`i' "Industry median Discontinued Ops over Revenue (values summed over next `i' yrs)"
	}

	#delimit ;
	order lpermno year
		  firm_ret? firm_ret?? ind_vw_ret? ind_vw_ret??
		  firm_rev_g? firm_rev_g?? ind_vw_rev_g? ind_vw_rev_g?? ind_m_rev_g? ind_m_rev_g??
		  firm_capxsga_rev? firm_capxsga_rev?? ind_vw_capxsga_rev? ind_vw_capxsga_rev?? ind_m_capxsga_rev? ind_m_capxsga_rev??
		  firm_aqc_rev? firm_aqc_rev?? ind_vw_aqc_rev? ind_vw_aqc_rev?? ind_m_aqc_rev? ind_m_aqc_rev??
		  firm_do_rev? firm_do_rev?? ind_vw_do_rev? ind_vw_do_rev?? ind_m_do_rev? ind_m_do_rev??;
	#delimit cr

	save "Abnormal_Returns/firm_year_growth_and_compounded_rets.dta", replace
		  

} // end growth section
*=======================================================================

*======================================================================
* COMPUTE FIRM- AND INDUSTRY-LEVEL COMPOUNDED RETURNS & GROWTH RATES
*======================================================================
if `portfolio' == 1 {
*-------------------
	use "Abnormal_Returns/portfolio years.dta", clear
		bys gvkey: gen group = _n
		reshape long year, i(gvkey group) j(j) string
		egen id = group(gvkey group)
		xtset id year
		tsfill
			bys id (year): ereplace gvkey = mode(gvkey)
			keep gvkey year
			duplicates drop
		destring gvkey, replace
		tempfile pf_years
		save `pf_years', replace

	use permno year retA month max_mon ///
		using "Abnormal_Returns/returns_annualized.dta", clear
	ren permno lpermno
	keep if month == max_mon
	merge 1:1 lpermno year month using "Compustat-CRSP_Merged_Annual.dta", ///
		nogen keep(3) keepus(gvkey csho prcc_c)
	
	merge m:1 gvkey year using `pf_years', keep(2 3)
		gen firms_in_portfolio = _merge == 3
		gen not_merged = _merge == 2
	gen mktcap = csho*prcc_c
		lab var mktcap "Market Capitalization ($ MM)"

	bys year: egen tot_mktcap = total(mktcap)
	gen wt = mktcap/tot_mktcap
		bys year: egen check = total(wt)
		assert inrange(check, 0.999, 1.001) if _merge == 3
	gen retA_vw = retA * wt

	collapse (mean) retA_eqw = retA (sum) retA_vw ///
			 (sum) firms_in_portfolio not_merged, by(year) fast

	save "portfolio_subset_returns.dta", replace

} // end portfolio section
*=======================================================================
