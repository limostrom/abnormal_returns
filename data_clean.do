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
-estimate several regression models by year and save the residuals for each
	firm year
-we would like to estimate:
	-CAPM
	-Fama-French 3 factor model
	-Carhart 4 factor model
	-5 factor model with a liquidity factor
-once the residuals are computed, create binary measures for whether they are in
	the top 10% of values or the bottom 10% of values
-the final dataset can include the permno, year, and the residual under each
	model for all available firm years

*/

clear all
cap log close
pause on

local import 0
local clean 0
local resids 0
local variance 0
local performance 0
local volatility 0 // Pan et al. CEO learning paper
local mw_and_tlr 0 //Doyle et al. 2007 Material Weakness
local growth 0
local cfsi 0
local rem 0
local betas 0
local patents 0
local overconfidence 0
local ind_dir_to 0
local misc 0

local merge_vars 1

local portfolio 0

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
		lab var niq "Net Income - Fiscal Quarter ($ M)"
		lab var oibdpq "Op. Income before Depreciation - Fiscal Quarter ($ M)"
		lab var atq "Total Assets - Fiscal Quarter ($ M)"
		lab var ltq "Total Liabilities - Fiscal Quarter ($ M)"
		lab var revtq "Total Revenue - Fiscal Quarter ($ M)"
		lab var capxy "Capex Year-to-Date ($ M)"

	save "Compustat-CRSP_Merged_Quarterly.dta", replace
	*--------------------------------------------------------
	*Import and save Compustat-CRSP Fundamentals A dataset
	*--------------------------------------------------------
	import delimited "Compustat-CRSP_Merged_Annual.csv", clear varn(1)
		gen year = int(datadate/10000)
		gen month = int(mod(datadate/100, 100))
		lab var sale "Sales ($ M)"
		lab var revt "Revenue - Total ($ M)"
		lab var ni "Net Income ($ M)"
		lab var oibd "Operating Income Before Depreciation ($ M)"
		lab var oiad "Operating Income After Depreciation ($ M)"
		lab var oancf "Operating Activities - Net CF ($ M)"
		lab var xidoc "Ext. Items and Discont. Ops ($ M)"
		lab var capx "Capital Expenditures ($ M)"
		lab var xad "Advertising Expense ($ M)"
		lab var xrd "R&D Expenditure ($ M)"
		lab var xsga "Selling, General & Admin Expense ($ M)"
		lab var invt "Total Inventory ($ M)"
		lab var aqc "Acquisitions (Cash Flow) ($ M)"
		lab var do "Discontinued Operations ($ M)"
		lab var ib "Income Before Extraordinary Items ($ M)"
		lab var dp "Depreciation and Amortization Expense ($ M)"
		lab var ppent "Property, Plant, and Equipment, Total Net ($ M)"
		lab var ppegt "PP&E (Gross) - Total ($ M)"
		lab var lt "Total Liabilities ($ M)"
		lab var dlc "Debt in Current Liabilities ($ M)"
		lab var dltt "Long-Term Debt - Total ($ M)"
		lab var at "Total Assets ($ M)"
		lab var ch "Cash ($ M)"
		lab var aoloch "Cash from PPE Sales ($ M)"
		lab var csho "Common Shares Outstanding - Company (Millions)"
		lab var prcc_f "Price - Fiscal Year - Close ($)" 
		lab var ib "Income Before Extraordinary Items ($ M)"
		lab var fca "Foreign Currency Adjustment ($ M)"
		lab var rcp "Restructuring Costs Pretax"
		lab var emp "Employees (Thousands)"

	save "Compustat-CRSP_Merged_Annual.dta", replace
	*--------------------------------------------------------
	*Import and save Compustat-CRSP Historical Segments
	*--------------------------------------------------------
	import delimited "Compustat-CRSP_Merged_HSegments.csv", clear varn(1)
	save "Compustat-CRSP_Merged_HSegments.dta", replace
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
	*Capital IQ Key Developments
	*--------------------------------------------------------
	import delimited "CapitalIQ_KeyDevelopments_05_19.csv", clear varn(1) ///
				bindquote(strict) maxquotedrows(0)
		tempfile ciq19
		save `ciq19', replace
	import delimited "CapitalIQ_KeyDevelopments_03_04.csv", clear varn(1) ///
				bindquote(strict) maxquotedrows(0)
		tempfile ciq04
		save `ciq04', replace
	import delimited "CapitalIQ_KeyDevelopments_92_02.csv", clear varn(1) ///
				bindquote(strict) maxquotedrows(0)
		append using `ciq04'
		append using `ciq19'

	save "CapitalIQ_KeyDevelopments.dta", replace
	*--------------------------------------------------------
	*Execucomp Options
	*--------------------------------------------------------
	import delimited "Execucomp_Options.csv", clear varn(1)
		ren year fyear // confirmed, actually fiscal year not calendar year
		lab var blkshval "Black-Scholes Value"
		lab var expric "Exercise Price"
		lab var mktpric "Market Price of Stock on Date of Grant"
		lab var pcttotopt "Percent of Total Options Granted to Employees"
		lab var numsecur "Number of Options Granted"
		lab var exdate "Expiration Date"
		lab var leftofc "Date Left as CEO"
	
	save "Execucomp_Options.dta", replace
	*--------------------------------------------------------
	*Execucomp Outstanding Equity
	*--------------------------------------------------------
	import delimited "Execucomp_OutsEq.csv", clear varn(1)
		ren year fyear // confirmed, actually fiscal year not calendar year
		lab var expric "Exercise Price"
		lab var opts_unex_exer "# of unexercised options held at FY end that had vested"
		lab var opts_unex_unexer "# of unexercised options held at FY end that had NOT vested"
		lab var shrs_unvest_num "# of shares that had not vested"
		lab var shrs_unvest_val "Mkt. val. of shares that had not vested"
		
	save "Execucomp_OutsEq.dta", replace
	*--------------------------------------------------------
	*Execucomp Annual Compensation
	*--------------------------------------------------------
	import delimited "Execucomp_AnnComp.csv", clear varn(1)
		ren year fyear // confirmed, actually fiscal year not calendar year
		lab var opt_unex_exer_est_val ""
		lab var shrown_excl_opts "Shares Owned (Excl. Opts)"
		lab var shrown_excl_opts_pct "Percentage of Total Shares Owned (Excl. Opts)"
		lab var opt_unex_exer_est_val "Est. Val. of In-the-Money Unexercised Exercisable Options"
		lab var opt_unex_unexer_est_val "Est. Val. of In-the-Money Unexercised Unexercisable Options"
		
	save "Execucomp_AnnComp.dta", replace
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
	use lpermno datadate fyearq fqtr niq oibdpq ///
		using "Compustat-CRSP_Merged_Quarterly.dta", clear
	bys lpermno datadate: egen max_qtr = max(fqtr)
		drop if fqtr < max_qtr
	isid lpermno datadate

	merge m:1 lpermno datadate using "Compustat-CRSP_Merged_Monthly.dta", ///
		keep(2 3) keepus(trt1m)
		
		gen datem = ym(int(datadate/10000),mod(int(datadate/100),100))	
			format datem %tm
	gen ret_plus1 = trt1m + 1
	xtset lpermno datem
	
	gen retq = (l2.ret_plus1 * l.ret_plus1 * ret_plus1) - 1
		lab var retq "Quarter Return"
	keep if _merge == 3
	drop _merge
	
	ren fyearq fyear
		
		bys lpermno fyear: egen retq_sd = sd(retq)
		rangestat (skewness) retq_skew = retq, interval(fyear 0 0) by(lpermno)
		rangestat (kurtosis) retq_kurtosis = retq, interval(fyear 0 0) by(lpermno)

		rangestat (sd) niq_sd = niq, interval(fyear -2 0) by(lpermno)
		rangestat (sd) oibdpq_sd = oibdpq, interval(fyear -2 0) by(lpermno)

	collapse (last) retq_sd retq_skew retq_kurtosis niq_sd oibdpq_sd, by(lpermno fyear) fast
		lab var retq_sd "St Dev of Quarterly Returns"
		lab var retq_skew "Skewness of Quarterly Returns"
		lab var retq_kurtosis "Kurtosis of Quarterly Returns"
		lab var niq_sd "St Dev of Quarterly Net Income"
		lab var oibdpq_sd "St Dev of Quarterly Op. Inc. (Before Depreciation)"

	save "Abnormal_Returns/firm_year_sds_and_skewness.dta", replace

} // end variance section
*=======================================================================

*======================================================================
* CREATE A DATASET OF PERFORMANCE METRICS
*======================================================================
if `performance' == 1 {
*-------------------
	use gvkey year using "Execucomp.dta", clear
		duplicates drop
		tempfile exec_subset
		save `exec_subset', replace

	use gvkey lpermno datadate fyear sic revt ni oibdp oiadp ///
			at lt oancf rect invt aco ppent intan ao ap lco lo ///
		using "Compustat-CRSP_Merged_Annual.dta", clear
	levelsof fyear, local(years)
	
	gen year = int(datadate/10000)
	merge m:1 gvkey year using `exec_subset', gen(m_excomp) keep(1 3)
	merge m:1 gvkey fyear using "Abnormal_Returns/tnic_ind_vars.dta", nogen keep(1 3) ///
				keepus(*_tnic_indmed)
	
	drop if fyear == . & at == . & ni == .
	duplicates drop
	duplicates tag lpermno fyear, gen(dup)
		sort lpermno datadate
		drop if dup & datadate < datadate[_n+1] & lpermno == lpermno[_n+1]
		
	foreach var of varlist revt ni oibdp oiadp at lt oancf rect invt aco ///
						ppent intan ao ap lco lo {
		dis "`var'"
		replace `var' = 0 if `var' == .
	}
		
	isid lpermno fyear
	xtset lpermno fyear
	
	ffind sic, newvar(ff17) type(17)
	levelsof ff17, local(inds)
	
	gen at_p75 = .
		lab var at_p75 "Tot. Assets > 75th pctl by industry"
	foreach ind of local inds {
		foreach fy of local years {
			qui summ at if fyear == `fy' & ff17 == `ind', d
			local p75 = r(p75)
			replace at_p75 = (at > `p75') if fyear == `fy' & ff17 == `ind'
		}
	}
	
	gen roa = (ni/at)*100
		lab var roa "Return on Assets (%)"
	gen roa_exintan = (ni/(at-intan))*100
		lab var roa "Return on Assets, excl. Intangibles (%)"
	gen roe = ni/(at-lt)*100
		lab var roe "Return on Equity (%)"
	gen revgr = (revt/l.revt - 1)*100
		lab var revgr "Revenue Growth (%)"
	gen ombdp = (oibdp/revt)*100
		lab var ombdp "Operating Margin (Before Depreciation) (%)"
	gen omadp = (oiadp/revt)*100
		lab var omadp "Operating Margin (After Depreciation) (%)"
	gen earngr = (ni-l.ni)/(0.5*abs(ni)+0.5*abs(l.ni))*100
		lab var earngr "Earnings Growth (%)"
	gen cfogr = (oancf-l.oancf)/(0.5*abs(l.oancf)+0.5*abs(oancf))*100
		lab var cfogr "Cash Flow from Operations Growth (%)"
		
	*RNOA, OMADP, and ATO from Li, Lundholm, and Minnis (2012)
	gen noa = rect + invt + aco + ppent + intan + ao ///
				- ap - lco - lo
		lab var noa "Net Operating Assets"
	gen noa_exintan = noa - intan
		lab var noa_exintan "Net Operating Assets, excl. Intangibles"
	gen rnoa = oiadp/noa
		lab var rnoa "Return on Net Operating Assets (decimal)"
	gen rnoa_exintan = oiadp/noa_exintan
		lab var rnoa_exintan "Return on Net Operating Assets, excl. Intangibles (decimal)"
	gen dF_rnoa = (f.oiadp - oiadp)/(noa-l.noa)
		lab var dF_rnoa "(Change in OIADP t to t+1) / (Change in NOA t-1 to t)"
	gen dF_rnoa_exintan = (f.oiadp - oiadp)/(noa_exintan-l.noa_exintan)
		lab var dF_rnoa_exintan "(Change in OIADP t to t+1) / (Change in NOA excl. Intangibles t-1 to t)"
	gen dL_rnoa = (oiadp - l.oiadp)/(l.noa-l2.noa)
		lab var dL_rnoa "(Change in OIADP t-1 to t) / (Change in NOA t-2 to t-1)"
	gen dL_rnoa_exintan = (oiadp - l.oiadp)/(l.noa_exintan-l2.noa_exintan)
		lab var dL_rnoa_exintan "(Change in OIADP t-1 to t) / (Change in NOA excl. Intangibles t-2 to t-1)"
	gen ato = revt/noa
		lab var ato "Net Operating Asset Turnover Ratio"
	gen ato_exintan = revt/noa_exintan
		lab var ato "Net Operating Assets (excl. Intangibles) Turnover Ratio"
			
	foreach var of varlist roa roe rnoa revgr omadp ato ombdp earngr cfogr {
		
		egen `var'_pctl = xtile(`var'), nq(10) by(fyear)
		gen `var'_topdec = `var'_pctl == 10 if `var'_pctl != .
		gen `var'_bottomdec = `var'_pctl == 1 if `var'_pctl != .
		
		* TNIC
		gen `var'_tnic_indadj = `var' - `var'_tnic_indmed
			lab var `var'_tnic_indadj "`var' minus industry median (by year)"
		egen `var'_tnic_indadj_pctl = xtile(`var'_tnic_indadj), nq(10) by(fyear)
		gen `var'_tnic_indadj_topdec = `var'_tnic_indadj_pctl == 10 ///
			if `var'_tnic_indadj_pctl != .
		gen `var'_tnic_indadj_bottomdec = `var'_tnic_indadj_pctl == 1 ///
			if `var'_tnic_indadj_pctl != .
			
		* Execucomp
		bys ff17 fyear: egen `var'_exindmed = median(`var') if m_excomp == 3
			lab var `var'_exindmed "Industry Median of `var' (by year among Execucomp firms)"
		gen `var'_exindadj = `var' - `var'_exindmed if m_excomp == 3
			lab var `var'_exindadj "`var' minus industry median (by year among Execucomp firms)"
		egen `var'_exindadj_pctl = xtile(`var'_exindadj), nq(10) by(fyear)
		gen `var'_exindadj_topdec = `var'_exindadj_pctl == 10 if `var'_exindadj_pctl != .
		gen `var'_exindadj_bottomdec = `var'_exindadj_pctl == 1 if `var'_exindadj_pctl != .
		
		/* TNIC & Execucomp // Very few - industry groups as granular as 3 digit SICs
		gen `var'_tnic_exindadj = `var' - `var'_tnic_exindmed if m_excomp1 == 3 & m_excomp2 == 3
			lab var `var'_tnic_exindadj "`var' minus industry median (by year among Execucomp firms)"
		egen `var'_tnic_exindadj_pctl = xtile(`var'_tnic_exindadj), nq(10) by(fyear)
		gen `var'_tnic_exindadj_topdec = `var'_tnic_exindadj_pctl == 10 ///
			if `var'_tnic_exindadj_pctl != .
		gen `var'_tnic_exindadj_bottomdec = `var'_tnic_exindadj_pctl == 1 ///
			if `var'_tnic_exindadj_pctl != . */
		
		* 25% Biggest Firms
		bys fyear: egen `var'_atp75med = median(`var') if at_p75
			bys fyear: ereplace `var'_atp75med = max(`var'_atp75med)
			lab var `var'_atp75med "Median of `var' of Largest 25% of Firms (by year)"
		gen `var'_atp75adj = `var' - `var'_atp75med
			lab var `var'_atp75adj "`var' minus median of largest 25% of firms (by year)"
		egen `var'_atp75adj_pctl = xtile(`var'_atp75adj), nq(10) by(fyear)
		gen `var'_atp75adj_topdec = `var'_atp75adj_pctl == 10 if `var'_atp75adj_pctl != .
		gen `var'_atp75adj_bottomdec = `var'_atp75adj_pctl == 1 if `var'_atp75adj_pctl != .
	}

	save "Abnormal_Returns/firm_year_performance.dta", replace

*-----------------------------------------------------------------------
	use gvkey lpermno datadate fyearq fqtr revtq niq oibdpq atq ltq oancfy ///
		using "Compustat-CRSP_Merged_Quarterly.dta", clear
	
	drop if atq == . & ltq == . & niq == . & oibdpq == . & revtq == .
	duplicates drop
	gen fyq = yq(fyearq, fqtr)
		format fyq %tq
	duplicates tag lpermno fyq, gen(dup)
		duplicates drop lpermno fyq, force // only drops 35
		drop dup
	isid lpermno fyq
	xtset lpermno fyq
	
	gen roa = (niq/atq)*100
		lab var roa "Return on Assets (%)"
	gen roe = niq/(atq-ltq)*100
		lab var roe "Return on Equity (%)"
	gen revgr = (revtq/l4.revtq - 1)*100
		lab var revgr "Revenue Growth (%)"
	gen om = (oibdpq/revtq)*100
		lab var om "Operating Margin (Before Depreciation) (%)"
	gen earngr = (niq-l4.niq)/(0.5*abs(niq)+0.5*abs(l4.niq))*100
		lab var earngr "Earnings Growth (%)"
	gen cfogr = (oancfy-l4.oancfy)/(0.5*abs(l4.oancfy)+0.5*abs(oancfy))*100
		lab var cfogr "Cash Flow from Operations Growth (%)"
			
	foreach var of varlist roa roe revgr om earngr cfogr {
		bys lpermno fyearq: egen `var'_sd = sd(`var')
		rangestat (skewness) `var'_skew = `var', interval(fyearq 0 0) by(lpermno)
		rangestat (kurtosis) `var'_kurtosis = `var', interval(fyearq 0 0) by(lpermno)
	}

	save "Abnormal_Returns/firm_qtr_perf_skew.dta", replace
	

} // end performance section
*=======================================================================

*======================================================================
* TENURE, VOLATILITY, and INDUSTRY VARIABLES
*======================================================================
if `volatility' == 1 {
*-------------------

	use gvkey lpermno datadate sic trt1m ///
			using "Compustat-CRSP_Merged_Monthly.dta", clear
	gen year = int(datadate/10000)
	gen month = mod(int(datadate/100), 100)
	gen datem = ym(year, month)
		format %tm datem
	
	ffind sic, newvar(ff17) type(17)
	levelsof ff17, local(inds)
	
	*Tenure
	joinby gvkey year using "Execucomp.dta",
		keep gvkey lpermno datadate year month datem sic ff17 ///
				exec_fullname becameceo trt1m
	keep if becameceo <= datadate
		gen datem_ceostart = ym(int(becameceo/10000), mod(int(becameceo/100), 100))
			format %tm datem_ceostart
			
	gen tenure = (datem - datem_ceostart)/12
	
	egen firmceo_id = group(lpermno exec_fullname)
		duplicates tag lpermno exec_fullname, gen(dup)
			drop if dup & exec_fullname == "Anthony Taylor" & becameceo == 20020102
			drop dup
		bys firmceo_id: gen firmceo_N = _N
	
	*Cumulative pre-inauguration return
	gen ret_plus1 = trt1m + 1
	bys ff17 datem: egen ret_indmed = median(trt1m)
	merge m:1 gvkey datem using "Abnormal_Returns/tnic_ind_monthly.dta", nogen keep(1 3) keepus(ret_tnicmed)
		gen ret_indadj = trt1m - ret_indmed
		gen ret_tnicadj = trt1m - ret_tnicmed
		gen ret_indadj_plus1 = ret_indadj + 1
		gen ret_tnicadj_plus1 = ret_tnicadj + 1
	
	preserve
		gen pre_months = .
		gen cum_indadj_ret = .
			lab var cum_indadj_ret "Cum. Ind-Adj. Return over 12 Months Prior to CEO Start"
		gen cum_tnicadj_ret = .
			lab var cum_tnicadj_ret "Cum. TNIC-Adj. Return over 12 Months Prior to CEO Start"
		keep lpermno datem ret_indadj_plus1 ret_tnicadj_plus1 pre_months ///
			cum_indadj_ret cum_tnicadj_ret
		duplicates drop
		xtset lpermno datem
		
		forval ii = 1/12 {
			if `ii' == 1 replace cum_indadj_ret = l`ii'.ret_indadj_plus1
			if `ii' > 1 replace cum_indadj_ret = cum_indadj_ret * l`ii'.ret_indadj_plus1 ///
						if l`ii'.ret_indadj_plus1 != .
						
			if `ii' == 1 replace cum_tnicadj_ret = l`ii'.ret_tnicadj_plus1
			if `ii' > 1 replace cum_tnicadj_ret = cum_tnicadj_ret * l`ii'.ret_tnicadj_plus1 ///
						if l`ii'.ret_tnicadj_plus1 != .
			
			replace pre_months = `ii' if l`ii'.ret_indadj_plus1 != .
				if `ii' == 1 replace pre_months = 0 if l`ii'.ret_indadj_plus1 == .
		}
		gen tenure = 0
		replace cum_indadj_ret = . if pre_months <= 6 // so we don't get crazy returns
		replace cum_tnicadj_ret = . if pre_months <= 6 // so we don't get crazy returns
		replace cum_indadj_ret = cum_indadj_ret^(12/pre_months) - 1
		replace cum_tnicadj_ret = cum_tnicadj_ret^(12/pre_months) - 1
		
		tempfile inauguration
		save `inauguration', replace
	restore
	
	merge m:1 lpermno datem tenure using `inauguration', nogen keep(1 3)
		bys firmceo_id: ereplace cum_indadj_ret = max(cum_indadj_ret)
		bys firmceo_id: ereplace cum_tnicadj_ret = max(cum_tnicadj_ret)
		bys firmceo_id: ereplace pre_months = max(pre_months)
	
	*Idionsyncratic Volatility
	merge m:1 datem using "FamaFrench.dta", nogen keep(1 3) keepus(mktrf smb hml)
	reg trt1m mktrf smb hml
		predict resid_FF3F, residuals
	bys datem: egen idret_vol = sd(resid_FF3F)
		lab var idret_vol "Idionsyncratic Return Volatility"
	
	
	*Learning Speed
	gen tenure_coeff = .
	levelsof firmceo_id if firmceo_N >= 2, local(firmceo_pairs)
	foreach firmceo of local firmceo_pairs {
		reg idret_vol tenure if firmceo_id == `firmceo'
			replace tenure_coeff = e(b)[1,1] if firmceo_id == `firmceo' ///
						& idret_vol != . & tenure != .
	}
	replace tenure_coeff = tenure_coeff * (-1)
		lab var tenure_coeff "Tenure Coefficienct *(-1)"
	egen tagged = tag(firmceo_id)
	egen learn_speed = xtile(tenure_coeff) if tagged, nq(100)
		bys firmceo_id: ereplace learn_speed = max(learn_speed)
			drop tagged
		replace learn_speed = learn_speed/100
		assert inrange(learn_speed,0,1) | learn_speed == .
		assert firmceo_N == 1 if learn_speed == .
		lab var learn_speed "Percentile of Firm-CEO level Tenure Coefficient (0 to 1)"
		
	save "Abnormal_Returns/firm_ceo_month_vars.dta", replace
	*---------------------------------------------------------------------------
	*Industry-Level Annual Variables (R&D, HHI, Sales Growth, New Products)
		use gvkey announcedate using "CapitalIQ_KeyDevelopments.dta", clear
			gen year = int(announcedate/10000)
		joinby gvkey year using "Compustat-CRSP_Merged_Annual.dta"
			keep gvkey year sic
		ffind sic, newvar(ff17) type(17)
		
		collapse (count) ind_new_products = gvkey, by(ff17 year) fast
			replace ind_new_products = ind_new_products/1000
			lab var ind_new_products "New Products by Industry (Thousands)"
		tempfile newproducts
		save `newproducts', replace

	use gvkey lpermno datadate year fyear sic at sale revt xrd ///
		using "Compustat-CRSP_Merged_Annual.dta", clear
	ffind sic, newvar(ff17) type(17)
	
	duplicates tag lpermno fyear, gen(dup)
		drop if dup & (at == . | inlist(sale, ., 0))
		duplicates drop lpermno fyear, force // only drops 35
		
	xtset lpermno fyear
	*R&D
	gen rd_intensity = xrd/at
		lab var rd_intensity "R&D Expense / Total Assets"
	*HHI
	bys ff17 year: egen tot_ind_sales = total(sale)
	gen sh_ind_sales = sale/tot_ind_sales * 100
		lab var sh_ind_sales "Share of Industry Sales (%)"
	gen sh_ind_sales_sq = sh_ind_sales^2
		lab var sh_ind_sales_sq "Squared Share of Industry Sales"
	*Sales Growth
	xtset lpermno fyear
	gen salesgr = (f.sale - sale)/sale*100
		lab var salesgr "Sales Growth, (t to t+1) (%)"
		
	*--------
	collapse (mean) ind_rd = rd_intensity ind_salesgr = salesgr ///
			 (sum) ind_hhi = sh_ind_sales_sq, by(ff17 year) fast
	lab var ind_rd "Industry (FF17) Avg R&D Intensity (R&D/Assets)"
	lab var ind_salesgr "Industry (FF17) Avg Sales Growth (t to t+1) (%)"
	lab var ind_hhi "Industry (FF17) HHI"
	*--------
	
	*New Products
	merge 1:1 ff17 year using `newproducts', nogen
	
	rangestat (mean) ind_rd_3yr = ind_rd ind_salesgr_3yr = ind_salesgr ///
			ind_hhi_3yr = ind_hhi ind_new_products_3yr = ind_new_products, ///
			interval(year -2 0) by(ff17)
			
	lab var ind_rd_3yr "3-Year Industry (FF17) Avg R&D Intensity (R&D/Assets)"
	lab var ind_salesgr_3yr "3-Year Industry (FF17) Avg Sales Growth (t to t+1) (%)"
	lab var ind_hhi_3yr "3-Year Industry (FF17) Avg HHI"
	lab var ind_new_products_3yr "3-Year Avg New Products by Industry (Thousands)"
	
	save "Abnormal_Returns/firm_cyear_ind_vars.dta", replace
	

} // end volatility section
*=======================================================================
* MATERIAL WEAKNESS & TIMELY LOSS RECOGNITION
*=======================================================================
if `mw_and_tlr' == 1 {
    use lpermno gvkey fyear year sic ///
		csho prcc_f ib fca rcp sale aqc using "Compustat-CRSP_Merged_Annual.dta", clear
	duplicates drop
	duplicates tag lpermno fyear, gen(dup)
		drop if dup & (inlist(sale, 0, .) | csho == .)
		duplicates drop lpermno fyear dup, force
	ffind sic, newvar(ff17) type(17)
	
	xtset lpermno fyear 
	gen mktcap = csho*prcc_f
		lab var mktcap "Market Capitalization ($ MM)"
	gen ln_mktcap = ln(mktcap) // log of mktcap in millions
		lab var ln_mktcap "Log of Market Cap (in millions pre-log)"
	bys lpermno: egen min_fy = min(fyear)
		gen firm_age = fyear - min_fy + 1
			lab var firm_age "Number of years in CRSP"
	gen agg_loss = (l.ib + ib) < 0
		lab var agg_loss "Aggregate Loss (1 if Income t and t-1 sum to less than 0)"
	gen foreign_transact = !inlist(fca, 0, .)
		lab var foreign_transact "Foreign Currency Adjustments != {0, .}"
	gen acquisition_val = aqc/mktcap
		lab var acquisition_val "Acquisitions scaled by Market Cap"
	gen salesgr = sale/l.sale - 1
		bys ff17 fyear: egen salesgr_indmed = median(salesgr)
			gen salesgr_indadj = salesgr-salesgr_indmed
			egen salesgr_indadj_pct = xtile(salesgr_indadj), nq(5) by(fyear)
		merge m:1 gvkey fyear using "Abnormal_Returns/tnic_ind_vars.dta", nogen keep(1 3) keepus(salesgr_tnicmed)
			gen salesgr_tnicadj = salesgr-salesgr_tnicmed
			egen salesgr_tnicadj_pct = xtile(salesgr_tnicadj), nq(5) by(fyear)
	gen extr_salesgr_ind = salesgr_indadj_pct == 5
		lab var extr_salesgr_ind "Extreme Sales Growth (1 if top quintile of ind-adj salesgr)"
	gen extr_salesgr_tnic = salesgr_tnicadj_pct == 5
		lab var extr_salesgr_tnic "Extreme Sales Growth (1 if top quintile of TNIC-adj salesgr)"
		
	xtset lpermno fyear
	gen restructuring = (-1)*l.rcp/mktcap
		lab var restructuring "Restructuring Costs scaled by Market Cap"
	preserve
		use lpermno datadate snms using "Compustat-CRSP_Merged_HSegments.dta", clear
		egen seg_id = group(lpermno snms)
		gen fyear = int(datadate/10000) + 1
			drop datadate
			duplicates drop
			isid seg_id fyear
		collapse (count) n_segs = seg_id, by(lpermno fyear)
		tempfile segments
		save `segments', replace
	restore
	merge 1:1 lpermno fyear using `segments', nogen keep(1 3)
		gen ln_segs = ln(n_segs)
	
	#delimit ;
	gen ln_mw = -2.182 /* using estimates from Doyle et al. 2007, p. 211 model 1 */
				-0.080 * ln_mktcap
				-0.136 * firm_age
				+0.438 * agg_loss
				+0.161 * ln(1.879) /* Subbing in the log of mean SPEs */
				+0.269 * ln_segs
				+0.311 * foreign_transact
				+0.763 * acquisition_val
				+0.227 * extr_salesgr_ind
				+1.184 * restructuring;
	#delimit cr
	
	save "Abnormal_Returns/mat_weakness_fitted.dta", replace

*-----------------------------------------------------------------------
	use gvkey lpermno datadate fyear sic ib using "Compustat-CRSP_Merged_Annual.dta", clear
	merge 1:1 gvkey lpermno datadate using "Compustat-CRSP_Merged_Monthly.dta", ///
			keepus(trt1m) keep(2 3)
	gen year = int(datadate/10000)
	gen month = mod(int(datadate/100), 100)
	gen datem = ym(year, month)
		format %tm datem
		
	* Timely Loss Recognition based on Francis & Martin 2010
	xtset lpermno datem
	gen ret_L9_F2 = (1 + trt1m)
		gen n_months = 1
		forval ii = 1/9 {
			replace  ret_L9_F2 =  ret_L9_F2*(1 + l`ii'.trt1m) ///
						if l`ii'.trt1m != .
			replace n_months = n_months + 1 if l`ii'.trt1m != .
		}
		forval ii = 1/2 {
		    replace  ret_L9_F2 =  ret_L9_F2*(1 + f`ii'.trt1m) ///
						if f`ii'.trt1m != .
			replace n_months = n_months + 1 if f`ii'.trt1m != .
		}
		replace ret_L9_F2 = . if n_months < 6
		replace ret_L9_F2 = ret_L9_F2^(12/n_months)
			replace ret_L9_F2 = ret_L9_F2 - 1
			lab var ret_L9_F2 "Cumulative 12-m returns starting 9 months before EoFY"
		gen neg_ret = ret_L9_F2 < 0
			lab var neg_ret "Negative cumulative 12-m returns"
		gen d_X_r = neg_ret * ret_L9_F2 // interaction term from paper
		
	gen basu = .
		lab var basu "Timeliness of loss recognition in earnings rel. to gains (Basu Coefficient)"
	gen tlr_tot = .
		lab var tlr_tot "Total timeliness of loss recognition (beta3 + beta4)"
	
	drop if fyear == . | ib == .
	bys lpermno: gen n_obs = _N
	levelsof lpermno if n_obs >= 4, local(firmlist)
	
	foreach firm of local firmlist {
	    reg ib neg_ret ret_L9_F2 d_X_r if lpermno == `firm'
		replace basu = e(b)[1,3] if lpermno == `firm'
		replace tlr_tot = e(b)[1,3] + e(b)[1,2] if lpermno == `firm'
	}
	save "Abnormal_Returns/timely_loss_recognition.dta", replace
	
} // end mw_and_tlr section
*=======================================================================

*=======================================================================
* COMPUTE FIRM- AND INDUSTRY-LEVEL COMPOUNDED RETURNS & GROWTH RATES
*=======================================================================
if `growth' == 1 {
*-------------------
	use permno date year retA ///
		using "Abnormal_Returns/returns_annualized.dta", clear

	ren permno lpermno
	gen month = substr(date, 5, 2)
		destring month, replace

	merge 1:1 lpermno year month using "Compustat-CRSP_Merged_Annual.dta", ///
		keepus(sic fyear aqc capx revt xsga do csho prcc_c) nogen keep(3)

	sort lpermno fyear month // sometimes more than one ob per firm-year
		collapse (last) retA sic aqc capx revt xsga do csho prcc_c, by(lpermno fyear) fast

		drop if fyear == .
		
	gen mktcap = prcc_c*csho
		lab var mktcap "Market Capitalization ($ MM)"

	ffind sic, newvar(ff48) type(48)

	bys ff48 fyear: egen ind_mktcap_tot = total(mktcap)
		gen wt = mktcap/ind_mktcap_tot
			bys ff48 fyear: egen check = total(wt) // just making sure the weights add to 1
			assert inrange(check, 0.999, 1.001)
			drop check

	foreach var of varlist retA aqc capx xsga do revt {
		if !inlist("`var'", "retA", "revt") replace `var' = 0 if `var' == .
		gen `var'_wtd = `var'*wt
			lab var `var'_wtd "`var' weighted by market cap share of industry"
		bys ff48 fyear: egen ind_vw_`var' = total(`var'_wtd)
			lab var ind_vw_`var' "Industry value-weighted `var'"
		bys ff48 fyear: egen ind_m_`var' = median(`var')
			lab var ind_m_`var' "Industry median `var'"
	}

	xtset lpermno fyear

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
	bys ff48 fyear: egen ind_m_rev_g0 = median(firm_rev_g0)
		lab var ind_m_rev_g0 "Industry median revenue growth from last year to now"

	forval i = 1/10 {
		local i_1 = `i' - 1
		
		sort lpermno fyear
		gen firm_rev_g`i' = F`i'.revt/L.revt
			lab var firm_rev_g`i' "Revenue growth from last year to `i' year(s) from now"

		gen ind_vw_rev_g`i' = F`i'.ind_vw_revt/L.ind_vw_revt
			lab var ind_vw_rev_g`i' "Industry value-weighted revenue growth from last year to `i' year(s) from now"

		bys ff48 fyear: egen ind_m_rev_g`i' = median(firm_rev_g`i')
			lab var ind_m_rev_g`i' "Industry median revenue growth from last year to `i' year(s) from now"
	}

	* --- Forward Sums for Forward-Looking Ratios --- *
	sort lpermno fyear
	forval i = 0/10 {
		local i_1 = `i' - 1
		foreach pref in "" "ind_vw_" "ind_m_" {
			rangestat (sum) `pref'capx_fsum`i' = `pref'capx `pref'xsga_fsum`i' = `pref'xsga ///
							`pref'aqc_fsum`i' = `pref'aqc  	`pref'do_fsum`i' = `pref'do ///
							`pref'revt_fsum`i' = `pref'revt, interval(fyear 0 `i') by(lpermno)
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
	order lpermno fyear
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
* NOW COMPUTE CASH FLOW-INVESTMENT SENSITIVITY & OVER/UNDER-INVESTMENT
* (see Biddle & Hilary 2006)
*======================================================================
if `cfsi' == 1 {
*-------------------
	use lpermno sic fyear year month ///
		revt capx ib dp ppent xrd capx aqc aoloch lt at ch sale ///
	  using "Compustat-CRSP_Merged_Annual.dta", clear
	
	duplicates tag lpermno fyear, gen(dup)
	bys lpermno fyear: egen dup_himon = max(month) if dup
	drop if dup & month < dup_himon
	drop dup dup_himon month

	foreach var of varlist xrd aoloch aqc {
		replace `var' = 0 if `var' == .
	}
	
	ffind sic, newvar(ff17) type(17)

	xtset lpermno fyear
	gen cf_K = (ib+dp)/l.ppent
		lab var cf_K "Cash Flow divided by Beg-of-Pd Net Capital"
		replace cf_K = 0 if cf_K < 0 // see Biddle & Hilary p. 979
	gen inv_K = capx/l.ppent
		lab var inv_K "Capex divided by Beg-of-Pd Net Capital"
	gen lev = lt/at
		lab var lev "Leverage (TD/TA)"

	gen cash_rank = .
	gen lev_rank = .
	gen cfwai = .

	forval yr = 1975/2018 {
		local yr_10 = `yr' - 10
		bys lpermno: egen cf_past10 = total(cf_K) if inrange(fyear, `yr_10', `yr')
		gen inv_wtd = inv_K*cf_K/cf_past10
		bys lpermno: egen cfwai_temp = total(inv_wtd) if inrange(fyear, `yr_10', `yr')
		replace cfwai = cfwai_temp if fyear == `yr'

		xtile cash_xt = ch if fyear == `yr', n(10)
			replace cash_rank = cash_xt if fyear == `yr'
		xtile lev_xt = lev if fyear == `yr', n(10)
			replace lev_rank = lev_xt if fyear == `yr'

		drop cf_past10 inv_wtd cfwai_temp cash_xt lev_xt
	}

	rangestat (mean) ai = inv, i(fyear -10 0) by(lpermno)

	gen cfsi = cfwai - ai if !inlist(cfwai, 0, .)
		lab var cfsi "Cash Flow Sensitivity to Investment"
		
	preserve // ----- INVESTMENT-Q SENSITIVITY BY FIRM YEAR --------------------
		merge 1:1 lpermno fyear using "Abnormal_Returns/firm_year_performance.dta", ///
			nogen keepus(at_p75)
			
		ren fyear fyearq
		merge 1:m lpermno fyearq using "Compustat-CRSP_Merged_Quarterly.dta", nogen ///
				keepus(cshoq capxy ppentq atq ltq fqtr datadate)
		ren fyearq fyear
			gen datefq = yq(fyear, fqtr)
				format %tq datefq
			drop if datefq == .
			duplicates drop lpermno datefq, force
		xtset lpermno datefq
			gen capxq = capxy if fqtr == 1
				replace capxq = capxy - l.capxy if fqtr > 1
				
		duplicates tag lpermno datadate, gen(dup)
		bys lpermno datadate: egen dup_hiq = max(datefq)
			drop if dup & datefq < dup_hiq
			drop dup dup_hiq
		merge 1:m lpermno datadate using "Compustat-CRSP_Merged_Monthly.dta", nogen ///
				keepus(prccm) keep(1 3)
		
		gen mktcapq = cshoq * prccm
			lab var mktcap "Mkt. Cap. (Mkt. Val of Eq) (Quarterly)"
		gen bk_eqq = atq-ltq
			lab var bk_eqq "Book Val of Eq (Quarterly)"
		xtset lpermno datefq
		gen inv_Kq = capxq/l.ppentq
			lab var inv_K "Capex divided by Beg-of-Pd Net Capital (Quarterly)"
			
		bys ff17 datefq: egen ind_mktcapq = total(mktcapq) if at_p75
			bys ff17 datefq: ereplace ind_mktcapq = max(ind_mktcapq)
		bys ff17 datefq: egen ind_bkq = total(bk_eqq) if at_p75
			bys ff17 datefq: ereplace ind_bkq = max(ind_bkq)
		gen ind_mtbq_atp75 = ind_mktcapq/ind_bkq
		
		save "Abnormal_Returns/firm_qtr_inv_mtb.dta", replace
			
		levelsof lpermno if inv_Kq != . & ind_mtbq_atp75 != ., local(firms)
		gen invqs = .
		foreach lp of local firms {
			levelsof fyear if lpermno == `lp', local(years)
			foreach fy of local years {
				cap noisily reg inv_Kq ind_mtbq_atp75 if lpermno == `lp' & fyear == `fy'
				replace invqs = e(b)[1,1] if lpermno == `lp' & fyear == `fy'
			}
		}
		
		
		collapse (last) invqs, by(lpermno fyear)
		
		tempfile inv_Q_sensitivity
		save `inv_Q_sensitivity', replace
	restore

	merge 1:1 lpermno fyear using `inv_Q_sensitivity', nogen keep(1 3) 
	
	*-------------------------------------------------------------------
	* Biddle, Hilary, Verdi: investment vars & over-investment measures
	*-------------------------------------------------------------------
	gen invBHV = (xrd + capx + aqc - aoloch)/l.at * 100
	gen capexBHV = capx/l.ppent * 100
	gen noncapexBHV = (xrd + aqc)/l.at * 100

	replace cash_rank = cash_rank/10
	replace lev_rank = lev_rank/10
	assert inrange(cash_rank, 0, 1) | cash_rank == .

	// Firm-Level
	gen overfirm = (cash_rank + lev_rank)/2

	// Industry-Level
	ffind sic, newvar(ff48) type(48)
	
	preserve
		#delimit ;
		collapse (sum) invBHV_ind = invBHV
					   capexBHV_ind = capexBHV
					   noncapexBHV_ind = noncapexBHV
					   sale_ind = sale, by(ff48 year) fast;
		#delimit cr
		xtset ff48 year
		gen salesg_ind = (sale_ind - l.sale_ind)/l.sale_ind

		reg invBHV_ind salesg_ind
			predict resid_ind, residuals
			gen overind = .
			pause
			forval yr = 1975/2018 {
				xtile overind_xt = resid_ind if year == `yr', n(10)
				replace overind = overind_xt if year == `yr'
				drop overind_xt
			}
			replace overind = overind/10
			assert inrange(overind, 0, 1) | overind == .

		tempfile overindustry
		save `overindustry', replace
	restore

	merge m:1 ff48 year using `overindustry', nogen keepus(overind)

	//Aggregate-Level
	preserve
		#delimit ;
		collapse (sum) invBHV_agg = invBHV
					   capexBHV_agg = capexBHV
					   noncapexBHV_agg = noncapexBHV
					   sale_agg = sale, by(year) fast;
		#delimit cr
		tsset year
		gen salesg_agg = (sale_agg - l.sale_agg)/l.sale_agg

		reg invBHV_agg salesg_agg
			predict resid_agg, residuals
			xtile overagg = resid_agg, n(10)
			replace overagg = overagg/10
			assert inrange(overagg, 0, 1) | overagg == .

		tempfile overaggregate
		save `overaggregate', replace
	restore

	merge m:1 year using `overaggregate', nogen keepus(overagg)


	*-------------------------------------------------------------------
	* Cumulative Average Revenue
	*-------------------------------------------------------------------
	keep if fyear >= 1975
		rangestat (mean) cum_avg_rev = revt, i(fyear . 0) by(lpermno)

	*-------------------------------------------------------------------
	keep lpermno fyear cfsi invqs cum_avg_rev overfirm overind overagg
		lab var cum_avg_rev "Cumulative Avg Revenue"
		lab var invqs "Inv.-Q Sensitivity - Coefficient from reg Inv/K on MTB by Firm & Yr."
		lab var overfirm "Firm propensity to over-invest"
		lab var overind "Industry propensity to over-invest"
		lab var overagg "Economy-wide propensity to over-invest"
	save "Abnormal_Returns/investment_efficiency_measures.dta", replace

} // end cash flow-investment sensitivity section
*=======================================================================

*======================================================================
* NOW COMPUTE MEASURES OF REAL EARNINGS MANAGEMENT TO CONTROL FOR
*   NON-OPERATIONS-RELATED INVESTMENT DECISIONS
* (see Cohen Dey Lys 2008)
*======================================================================
if `rem' == 1 {
*-------------------
	use lpermno sic year month ///
		sale at oancf xidoc cogs invt xad xrd xsga ///
	  using "Compustat-CRSP_Merged_Annual.dta", clear

	duplicates tag lpermno year, gen(dup)
	bys lpermno year: egen dup_himon = max(month) if dup
	drop if dup & month < dup_himon
	drop dup dup_himon

	foreach var of varlist oancf xidoc cogs invt xad xrd xsga {
		replace `var' = 0 if `var' == .
	}

	ffind sic, newvar(ff48) type(48)
	
	xtset lpermno year

	gen dsale = sale - l.sale
	gen Lsale = l.sale
	gen dLsale = l.sale - l2.sale
	gen dinvt = invt - l.invt
	gen cfo = oancf - xidoc
	gen prod = cogs + dinvt
		lab var prod "Production Costs (COGS + dINVT) ($MM)"
	gen disx = xad + xrd + xsga
		lab var disx "Discretionary Expenses (XAD+XRD+XSGA) ($MM)"

	foreach var of varlist cfo prod disx sale dsale Lsale dLsale {
		gen `var'_scaled = `var'/l.at // scaled by lagged tot assets
	}

	bys ff48 year: reg cfo_scaled sale_scaled dsale_scaled
		predict pred_cfo_scaled, xb
		xtset lpermno year
		gen pred_cfo = pred_cfo_scaled * l.at
		gen r_cfo = cfo_scaled - pred_cfo_scaled

	bys ff48 year: reg prod_scaled sale_scaled dsale_scaled dLsale_scaled
		predict pred_prod_scaled, xb
		xtset lpermno year
		gen pred_prod = pred_prod_scaled * l.at
		gen r_prod = prod_scaled - pred_prod_scaled

	bys ff48 year: reg disx_scaled Lsale_scaled
		predict pred_disx_scaled, xb
		xtset lpermno year
		gen pred_disx = pred_disx_scaled * l.at
		gen r_disx = disx_scaled - pred_disx_scaled
		
	gen rm_proxy = r_cfo + r_prod + r_disx

	keep lpermno year r_cfo r_prod r_disx rm_proxy
	save "Abnormal_Returns/real_earnings_management_measures.dta", replace



} // end real earnings management section
*=======================================================================

*=======================================================================
* COMPUTE BETAS
*=======================================================================
if `betas' == 1 {
*------------------
use lpermno fyear datadate using "Compustat-CRSP_Merged_Annual.dta", clear

preserve
	use date vwretd using crsp, clear
	ren date datadate
	duplicates drop
	isid datadate
	tempfile vwretd
	save `vwretd', replace
restore

merge 1:1 lpermno datadate using "Compustat-CRSP_Merged_Monthly.dta", ///
	keep(2 3) keepus(trt1m)
merge m:1 datadate using `vwretd', nogen keep(1 3)
gen datem = ym(int(datadate/10000),mod(int(datadate/100),100))
	format %tm datem
merge m:1 datem using "FamaFrench.dta", nogen keep(1 3) keepus(mktrf)

levelsof lpermno, local(firms)

gen beta = .
foreach lp of local firms {
	levelsof fyear if lpermno == `lp', local(sample_years)
	foreach fyr of local sample_years {
		local l5_fyr = `fyr' - 5
		cap noisily reg trt1m mktrf if lpermno == `lp' & inrange(fyear, `l5_fyr', `fyr')
		replace beta = e(b)[1,1] if lpermno == `lp' & fyear == `fyr'
	}
}
drop if _merge == 2

save "Abnormal_Returns/firm_year_betas.dta", replace

} // end betas section
*=======================================================================

*=======================================================================
* ADD PATENT VARIABLES
*=======================================================================
if `patents' == 1 {
*-------------------
import delimited "KPSS_2019_public/KPSS_2019_public.csv", clear varn(1)
ren permno lpermno
gen year = substr(issue_date, -4, 4)
	destring year, replace

#delimit ;
collapse (count) n_patents = patent_num 
		 (sum) patent_cites = cites
				patent_val_real = xi_real patent_val_nom = xi_nominal, 
		by(lpermno year);
#delimit cr
		
lab var n_patents "Number of Patents"
lab var patent_cites "Total Patent Forward Citations"
lab var patent_val_nom "Total Nominal Value of Innovations ($ Millions)"
lab var patent_val_real "Total Real Value of Innovations (Millions of 1982 $s, CPI)"
		
save "Abnormal_Returns/firm_year_patents.dta", replace

} // end patents section
*=======================================================================

*=======================================================================
* COMPUTE OVERCONFIDENCE METRICS FROM
* 	MALMENDIER & TATE 2005: CEO OVERCONFIDENCE & CORPORATE INVESTMENT
*=======================================================================
if `overconfidence' == 1 {
*-------------------
* Net Buyer
use  "Execucomp_AnnComp.dta", clear
	drop if exec_fullname == ""
	isid co_per_rol fyear
	
	gen becameceo_yr = int(becameceo/10000)
		replace becameceo_yr = 1992 if becameceo_yr <1992
	gen tenure = fyear - becameceo_yr
		keep if tenure >= 0
	bys co_per_rol: egen yrs_stayed = max(tenure)

	xtset co_per_rol fyear
	gen buy_yr = shrown_excl_opts_pct > l.shrown_excl_opts_pct ///
					if inrange(tenure,1,5) & shrown_excl_opts_pct != .
		replace buy_yr = 0 if buy_yr == .
	bys co_per_rol: egen years_net_buy = total(buy_yr)
	gen net_buyer = years_net_buy >= 3 if yrs_stayed >= 10
	collapse (last) net_buyer, by(exec_fullname gvkey fyear)
	
	save "Abnormal_Returns/firm_ceo_netbuyers.dta", replace
	
* Longholder
use "gvkey_fyear_bydatem.dta", clear
	ren datem exp_datem
	ren fyear exp_fyear
	tempfile exp_datem_fyear
	save `exp_datem_fyear', replace
use "Execucomp_Options.dta", clear
	gen exp_datem = ym(int(exdate/10000), mod(int(exdate/100), 100))
		format exp_datem %tm
	merge m:1 gvkey exp_datem using `exp_datem_fyear', nogen keep(1 3)
	br if fyear == exp_fyear
	gen longholder = fyear == exp_fyear
	collapse (max) longholder, by(exec_fullname gvkey)

	save "Abnormal_Returns/firm_ceo_longholders.dta", replace
	
* Holder67


	
} // end overconfidence section
*=======================================================================

*=======================================================================
* COMPUTE INDEPENDENT DIRECTOR TURNOVER
*=======================================================================
if `ind_dir_to' == 1 {
*-------------------
use cik directorname rolename yearstart yearend dateendrole ///
		using "Abnormal_Returns/boardex_person-firm.dta", clear
	drop if cik == .
	gen independent = strpos(rolename, "Independent") > 0
	keep if independent
	
	gen n_expand = yearend - yearstart + 1 if yearend != .
	replace n_expand = 2020 - yearstart + 1 if yearend == . & dateendrole == "C"
		drop if n_expand == .
	drop dateendrole
	duplicates drop
	
	expand n_expand

	bys cik directorname yearstart yearend: gen fyear = yearstart - 1 + _n
	duplicates drop cik directorname fyear, force
	bys cik directorname: egen last_year = max(fyear)
		gen turnover = fyear == last_year

	collapse (sum) ind_dirs = independent turnovers = turnover, by(cik fyear)
	bys cik (fyear): drop if _n == _N // last year turnover = 100%

	save "Abnormal_Returns/cik_ind_dir_turnover.dta", replace
	
} // end independent director turnover section
*=======================================================================

*=======================================================================
* COMPUTE MISCELLANEOUS METRICS
*=======================================================================
if `misc' == 1 {
*-------------------
use lpermno gvkey sic fyear datadate ///
	at revt sale cogs dlc dltt lt ppent ppegt emp ///
	using "Compustat-CRSP_Merged_Annual.dta", clear
	
duplicates tag lpermno fyear, gen(dup)
drop if dup & (at == . | revt == .)
drop dup
duplicates tag lpermno fyear, gen(dup)
bys lpermno fyear (datadate): drop if _n == 2
drop dup gvkey

ffind sic, newvar(ff17) type(17)

xtset lpermno fyear
gen ln_at = ln(at)
	lab var ln_at "Log of Total Assets (in Millions)"
gen revt_avgassets = revt/(0.5*at + 0.5*l.at)
	lab var revt_avgassets "Total Revenue / Avg. Tot. Assets"
gen debt_avgassets = (dlc+dltt)/(0.5*at + 0.5*l.at)
	lab var debt_avgassets "(Debt in Current Liabilities + LTD) / Avg. Tot. Assets"
gen bk_eq = at-lt
	lab var bk_eq "Book Value of Equity"
bys lpermno: egen min_year = min(fyear)
gen firm_age = fyear - min_year + 1
	lab var firm_age "Number of Years in Compustat-CRSP"
gen ln_emp = ln(emp)
	lab var ln_emp "Log of Number of Employees (in Thousands)"
gen accdep_ppeg = (ppegt-ppent)/ppegt
	lab var accdep_ppeg "Ratio of Accumulated Depreciation to Gross PPE"
gen gpm = (sale-cogs)/sale
	lab var gpm "Gross Profit Margin (decimal)"

preserve
	use date vwretd using crsp, clear
	ren date datadate
	duplicates drop
	isid datadate
	tempfile vwretd
	save `vwretd', replace
restore

merge 1:1 lpermno datadate using "Compustat-CRSP_Merged_Monthly.dta", ///
	keep(2 3) keepus(trt1m)
merge m:1 datadate using `vwretd', nogen keep(1 3)
gen datem = ym(int(datadate/10000),mod(int(datadate/100),100))
	format %tm datem
merge m:1 datem using "FamaFrench.dta", nogen keep(1 3) keepus(mktrf)


rangestat (sd) sd_trt1m = trt1m, i(datem -11 0) by(lpermno)
gen ret_fyear = (1+trt1m) if fyear != .

xtset lpermno datem
gen vwretd_fyear = (vwretd + 1) if fyear != .
	replace vwretd_fyear = 1 if vwretd_fyear == . & fyear != .
	forval i = 1/11 {
		replace ret_fyear = ret_fyear * (1 + l`i'.trt1m) if l`i'.trt1m != .
		replace vwretd_fyear = vwretd_fyear * (1 + l`i'.vwretd) if l`i'.vwretd != .
	}
replace ret_fyear = ret_fyear - 1
replace vwretd_fyear = vwretd_fyear - 1
gen ret_fyear_mktadj = ret_fyear - vwretd_fyear
	lab var ret_fyear_mktadj "Market-Adjusted (vwretd) Return over Fiscal Year"


drop if _merge == 2

merge m:1 lpermno fyear using "Abnormal_Returns/firm_year_performance.dta", ///
		nogen keepus(rnoa at_p75 m_excomp) keep(1 3)

foreach if_group in "" "_atp75" "_execucomp" {
	gen resid_rnoa`if_group' = .
	
	levelsof fyear, local(sample_years)
	forval ind = 1/17 {
		foreach fyr of local sample_years {
				
			if "`if_group'" == "" ///
				local if_st "if ff17 == `ind' & fyear == `fyr'"
			if "`if_group'" == "_atp75" ///
				local if_st "if ff17 == `ind' & fyear == `fyr' & at_p75"
			if "`if_group'" == "_execucomp" ///
				local if_st "if ff17 == `ind' & fyear == `fyr' & m_excomp == 3"
			
			preserve
				keep `if_st'
				local N = _N
			restore
			if `N' > 5 {
				cap noisily reg rnoa ln_at firm_age ln_emp accdep_ppeg ///
								`if_st'
				predict resids, residuals
				replace resid_rnoa`if_group' = resids `if_st'
				drop resids
				br if e(sample)
				pause
			} // if N > 5
		} // fyear loop
	} // industry loop
} // "if group" loop

lab var resid_rnoa "Resid from reg RNOA on firm characteristics (All firms)"
lab var resid_rnoa_atp75 "Resid from reg RNOA on firm characteristics (Biggest firms)"
lab var resid_rnoa_execucomp ///
			"Resid from reg RNOA on firm characteristics (Execucomp firms)"

save "Abnormal_Returns/firm_year_miscellaneous.dta", replace


} // end miscellaneous section
*=======================================================================

*#######################################################################
* MERGE ALL FIRM-YEAR VARIABLES DATASETS TO ONE
if `merge_vars' == 1 {
*#######################################################################
use lpermno gvkey fyear date sic ME BEtoME OP ///
		using "Abnormal_Returns/returns_annualized_wME_BM_OP.dta", clear
ffind sic, newvar(ff17) type(17)
ffind sic, newvar(ff48) type(48)
destring date, replace
gen year = int(date/10000)
drop date
	merge 1:1 lpermno fyear using "Abnormal_Returns/returns_annualized_wFF_portfolios.dta", ///
		nogen keepus(ME_pct BM_pct OP_pct pf_*)
	merge 1:1 lpermno fyear using "Abnormal_Returns/firm_year_sds_and_skewness.dta", nogen keep(1 3)
	ren lpermno permno
	merge 1:1 permno year using "Abnormal_Returns/firm_year_alphas", nogen ///
		keepus(*M1* *M3* *M4* *M5*) keep(1 3)
	ren permno lpermno
	merge 1:1 lpermno fyear using "Abnormal_Returns/firm_year_performance.dta", nogen ///
		keepus (roa* roe* oiadp rnoa* dF_rnoa* dL_rnoa* revt revgr* omadp* ato* ///
					ombdp* earngr* cfogr* at_p75 m_excomp)
	merge 1:1 lpermno fyear using "Abnormal_Returns/mat_weakness_fitted.dta", nogen ///
		keepus(ln_mw)
	merge 1:1 lpermno fyear using "Abnormal_Returns/firm_year_growth_and_compounded_rets.dta", ///
		nogen keepus(aqc *ret* *rev_g* *capxsga_rev* *aqc_rev* *do_rev* wt mktcap)
	merge m:1 gvkey fyear using "Abnormal_Returns/tnic_ind_vars.dta", nogen keepus(tnic_vw_* tnic_m_*)
	merge m:1 lpermno fyear using "Abnormal_Returns/firm_year_miscellaneous.dta", ///
		nogen keepus(at ln_at emp ln_emp firm_age revt_avgassets debt_avgassets bk_eq gpm ///
						sd_trt1m ret_fyear_mktadj resid_rnoa* accdep_ppeg)
	merge m:1 lpermno fyear using "Abnormal_Returns/firm_year_betas.dta", nogen
	merge m:1 gvkey fyear using "Abnormal_Returns/firm_new_products.dta", nogen keep(1 3)
	merge m:1 ff17 year  using "Abnormal_Returns/firm_cyear_ind_vars.dta", nogen keepus(ind_*)
	merge m:1 gvkey fyear using "Abnormal_Returns/tnic_ind_vars.dta", nogen ///
		keepus(tnic_rd_avg tnic_salesgr_avg tnic_hhi tnic_new_products tnic_*_3yr)
	merge 1:1 lpermno gvkey fyear using "Abnormal_Returns/timely_loss_recognition.dta", ///
		nogen keep(1 3) keepus(basu tlr_tot)
	merge 1:1 lpermno gvkey fyear using "Compustat-CRSP_Merged_Annual.dta", ///
		nogen keep(1 3) keepus(cik xrd)
	merge m:1 cik fyear using "Abnormal_Returns/cik_ind_dir_turnover.dta", ///
		nogen keep(1 3) keepus(ind_dirs turnovers)
		gen sh_ind_dir_to = turnovers/ind_dirs
			lab var sh_ind_dir_to "Share of Ind. Dir.s turning over in the next year"
	merge m:1 lpermno fyear  using "Abnormal_Returns/investment_efficiency_measures.dta", ///
		nogen keepus(cfsi invqs cum_avg_rev overfirm overind overagg)
	merge m:1 lpermno year  using "Abnormal_Returns/real_earnings_management_measures.dta", nogen
	merge m:1 lpermno year  using "Abnormal_Returns/firm_year_patents.dta", ///
		nogen keepus(n_patents patent_*)
save "Abnormal_Returns/master_firm-year_vars.dta", replace

use "Abnormal_Returns/firm_qtr_perf_skew.dta", clear
	ren fyq datefq
	merge 1:1 lpermno datefq using "Abnormal_Returns/firm_qtr_inv_mtb.dta", nogen keepus(inv_Kq ind_mtbq_atp75)
	ren datefq fyq
	lab var inv_Kq "Quarterly Capex Scaled by Beg-of-Pd Net PPE"
save "Abnormal_Returns/master_firm-qtr_vars.dta", replace
	
use "Abnormal_Returns/firm_ceo_month_vars.dta", clear
	merge m:1 gvkey datem using "gvkey_fyear_bydatem.dta", nogen keep(1 3) keepus(fyear)
	merge m:1 gvkey exec_fullname fyear using "Abnormal_Returns/firm_ceo_netbuyers.dta", nogen keep(1 3)
	merge m:1 gvkey exec_fullname using "Abnormal_Returns/firm_ceo_longholders.dta", nogen keep(1 3)
	ren lpermno permno
	merge m:1 permno datem using "Abnormal_Returns/returns_annualized.dta", nogen keep(1 3)
	merge m:1 permno datem using "Abnormal_Returns/sortino_sharpe.dta", nogen keep(1 3)
	ren permno lpermno
	lab var cum_indadj_ret "Cum. ind-adj monthly ret. over 12 months prior to CEO start"
	lab var cum_tnicadj_ret "Cum. TNIC-adj monthly ret. over 12 months prior to CEO start"
save "Abnormal_Returns/master_firm-ceo-mon_vars.dta", replace
}
*####################################################################### 

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
