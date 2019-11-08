/*
mgr_ability.do

-See Demerjian et al. 2013 and Demerjian et al. 2012 for details
	on using a DEA model to calculate firm efficiency, regressing
	firm efficiency on firm-level covariates, and saving the
	residual as a measure of managerial ability
*/


clear all
cap log close
pause on


local import 1
local dea 0


global repo "../Documents\GitHub\abnormal_returns"

cap cd "C:\Users\lmostrom\Dropbox\"

*=======================================================================
* Import Compustat-CRSP, Execucomp, and Audit Analytics datasets
*=======================================================================
if `import' == 1 {
	import delimited "Compustat-CRSP_Merged_Monthly.csv", clear varn(1)
		replace trt1m = trt1m/100
			drop if trt1m == .

		lab var trt1m "Monthly Return"
	save "Compustat-CRSP_Merged_Monthly.dta", replace
	
	import delimited "Compustat-CRSP_Merged_Annual.csv", clear varn(1)

		gen year = int(datadate/10000)
		gen month = int(mod(datadate/100, 100))

		*For DEA Calculation of Firm Efficiency
		lab var sale "Sales ($MM)"
		lab var cogs "COGS ($MM)"
		lab var xsga "Selling, General & Admin Expense ($MM)"
		lab var ppent "Property, Plant & Equipment ($MM)"
		forval x = 1/5 {
			lab var mrc`x' "Min rental commitments due in `x' year(s) ($MM)"
		}
		lab var xrd "R&D Expense ($MM)"
		lab var gdwl "Purchase Goodwill ($MM)"
		lab var intan "Total Intangible Assets ($MM)"

		*For Calculation of Managerial Ability via Residual
		lab var at "Total Assets ($MM)"
		lab var oibdp "Op. Income before Depreciation ($ MM)"
		lab var rect "Total Receivables ($MM)"
		lab var invt "Total Inventory ($MM)"
		lab var aco "Other Current Assets ($MM)"
		lab var ap "Accounts Payable ($MM)"
		lab var lco "Other Current Liabilities ($MM)"
		lab var capx "Capital Expenditures ($MM)"
		lab var fca "Foreign Currency Adjustment (Income Account) ($MM)"

	save "Compustat-CRSP_Merged_Annual.dta", replace
	
	import delimited "Execucomp.csv", clear varn(1)
	save "Execucomp.dta", replace

	import delimited "FamaFrench48.csv", clear varn(1)
	save "FamaFrench48.dta", replace

} // end `import' section

*=======================================================================
* Use DEA to Calculate Firm Efficiency
*=======================================================================
if `dea' == 1 {
	use gvkey sic datadate fyear linkprim ///
		sale cogs xsga ppent mrc? xrd gdwl intan ///
		at oibdp rect invt aco ap lco capx fca ///
	  using "Compustat-CRSP_Merged_Annual.dta", clear
		drop if at == .
		tostring datadate, gen(date_str)
			gen datayr = substr(date_str, 1, 4) 
				destring datayr, replace
			gen datamon = substr(date_str, 5, 2)
				destring datamon, replace
			replace fyear = datayr if fyear == . & datamon > 6
			replace fyear = datayr + 1 if fyear == . & datamon <= 6

		*Want observations unique at firm-year level
		duplicates tag gvkey fyear, gen(dup)
			drop if dup & !inlist(linkprim, "P", "C")
				// keep primary security issues only (linkprim == "P" or "C")

		bys gvkey: egen min_fyear = min(fyear)

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

	xtset gvkey fyear

	gen rd_net = xrd // current year R&D expense
	lab var rd_net "R&D Net of Amortization"
	forval t = 1/4 { // amortized past 4 years of R&D expense
		replace rd_net = rd_net + l`t'.xrd*(1-0.2*`t')
	} // per Demerjian et al. 2012

	replace gdwl = 0 if gdwl == .

	gen oth_intan = intan - gdwl
	lab var oth_intan "Intangible Assets less Purchased Goodwill"

	forval x = 1/5 {
		replace mrc`x' = 0 if mrc`x' == .
		gen mrc`x'_pv = mrc`x'/(1.1^`x') // discounted at 10%
	} // per Demerjian et al. 2012
	gen ops_leases = mrc1_pv + mrc2_pv + mrc3_pv + mrc4_pv + mrc5_pv
	lab var ops_leases "Present discounted value of next 5 years of min payments on ops leases"

	keep if fyear >= 1980
	tostring gvkey, gen(gvkey_str)
	tostring fyear, gen(fyear_str)
	gen dmu = gvkey_str

	* DEA Model
	*	(www.cgdev.org/sites/default/files/archive/doc/stata/MO/DEA/dea_in_stata.pdf)
	forval ff = 1/47 {
		dea cogs xsga ppent ops_leases rd_net gdwl oth_intan = sale ///
			if ff48 == `ff', ///
			saving("Abnormal_Returns/FirmEfficiency_ind`ff'.dta", replace)
		dis("Industry `ff'")
	}

} // end `merge' section

*=======================================================================

