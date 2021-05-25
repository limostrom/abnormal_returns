/*
summ_stats_table.do
Summary Stats to Compare the Old Sample vs. New Additions & Full Sample
-CEO tenure (max by firm-CEO)
-innovation (patents, R&D expenditure, ???)
-firm age
-firm size
-distribution of years by firm-CEO-year observation
-distribution across H/L testosterone and H/L frugality groups
*/

clear all
cap log close
pause on

cap cd  "C:\Users\lmostrom\Dropbox\Abnormal_Returns\"


#delimit ; // -----------------------------------------------------------------;
use gvkey fyear ff17  firm_aqc_rev0 aqc at revt ln_at emp ln_emp accdep_ppeg firm_age xrd new_products 
		n_patents patent_cites patent_val_real patent_val_nom
		firm_ret0 ret_fyear_mktadj ind_vw_ret0 tnic_vw_ret0 residM3A winM3A
	using "master_firm-year_vars.dta", clear;
duplicates drop;

sort gvkey fyear firm_age;
bys gvkey fyear: keep if _n == _N;
* sometimes multiple because of newer securities with lower firm age;

#delimit cr
tempfile firmvars
save `firmvars', replace
*-------------------------------------------------------------------------------
use "ceo 100820.dta", clear
merge 1:1 co_per_rol using "co_per_rol original.dta", assert(1 3)
gen original_sample = _merge == 3
drop _merge

joinby gvkey using `firmvars', unm(master)
keep if inrange(fyear, year_ceo, year_left)
lab var fyear "Fiscal Year"
egen tag_ceo = tag(co_per_rol)
lab var new_products "Number of New Products"
gen ln_firm_age = ln(firm_age)

xtset co_per_rol fyear
gen rd_Lat = xrd/l.at
	lab var rd_Lat "R&D Scaled by Lagged Tot. Assets"
gen rd_revt = xrd/revt
	lab var rd_revt "R&D Scaled by Revenue"

gen tenure = fyear - year_ceo
	lab var tenure "CEO Tenure"
gen max_tenure = year_left - year_ceo
	lab var max_tenure "CEO Full Tenure"
gen tenure_over12 = max_tenure > 12

gen ret0_indvw_adj = firm_ret0 - ind_vw_ret0
	lab var ret0_indvw_adj "Return Adjusted by Industry Value-Weighted Return"
gen ret0_tnicvw_adj = firm_ret0 - tnic_vw_ret0
	lab var ret0_tnicvw_adj "Return Adjusted by TNIC Value-Weighted Return"
	
ren firm_aqc_rev0 aqc_revt
	lab var aqc_revt "Acquisitions Scaled by Revenue"
gen aqc_Lat = aqc/l.at
	lab var aqc_Lat "Acquisitions Scaled by Lagged Tot. Assets"

gen hiT = (fht == 1 | nfht == 1)
	
lab var patent_val_real "Tot. Real Value of Patents (Mil. 1982 USD from CPI)"
	
* SUMMARY STATS & TABS *********************************************************
	
#delimit ;
local summs_varlist "fyear firm_age at emp max_tenure accdep_ppeg rd_Lat 
							n_patents patent_cites patent_val_real patent_val_nom
							firm_ret0 ret_fyear_mktadj ret0_indvw_adj ret0_tnicvw_adj residM3A";
#delimit cr
/*
* Original vs. New T-Tests
local ii = 1
foreach var of local summs_varlist {
	ttest `var', by(original_sample)
	if `ii' == 1 {
		mat A = (`r(mu_1)', `r(mu_2)', `r(mu_2)' - `r(mu_1)', `r(p)')
		mat colnames A = new_mean old_mean diff pval	
	}
	else {
		mat A = (A \ `r(mu_1)', `r(mu_2)', `r(mu_2)' - `r(mu_1)', `r(p)')
	}
	local ++ ii
}
mat rownames A = `summs_varlist'
matlist A
pause

* Tenure T-Tests
local ii = 1
foreach var of local summs_varlist {
	ttest `var', by(tenure_over12)
	if `ii' == 1 {
		mat B = (`r(mu_1)', `r(mu_2)', `r(mu_2)' - `r(mu_1)', `r(p)')
		mat colnames B = under12_mean over12_mean diff pval	
	}
	else {
		mat B = (B \ `r(mu_1)', `r(mu_2)', `r(mu_2)' - `r(mu_1)', `r(p)')
	}
	local ++ ii
}
mat rownames B = `summs_varlist'
matlist B
*pause

#delimit ;				
eststo s_full: estpost summ `summs_varlist', d;
eststo s_ten12: estpost summ `summs_varlist' if max_tenure <= 12, d;
eststo s_orig: estpost summ `summs_varlist' if original_sample, d;
eststo s_new: estpost summ `summs_varlist' if !original_sample, d;

/*
eststo s_old: estpost summ tenure patents xrd firm_age at mktcap fyear tercile_t frugal ///
	if newsample == 0, d
eststo s_new: estpost summ tenure patents xrd firm_age at mktcap fyear tercile_t frugal ///
	if newsample == 1, d
*/
	
esttab s_full using "summ_stats.csv", replace
	cells("count mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))
				p1(fmt(2)) p25(fmt(2)) p50(fmt(2)) p75(fmt(2)) p99(fmt(2))")
	mtitles("Full Sample") label
	title("Summary Statistics of RHS Variables (Firm-CEO-Year)");
esttab s_ten12 using "summ_stats.csv", append
	cells("count mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))
				p1(fmt(2)) p25(fmt(2)) p50(fmt(2)) p75(fmt(2)) p99(fmt(2))")
	mtitles("CEOs with Tenure <= 12 years") label;
esttab s_orig using "summ_stats.csv", append
	cells("count mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))
				p1(fmt(2)) p25(fmt(2)) p50(fmt(2)) p75(fmt(2)) p99(fmt(2))")
	mtitles("Original Sample") label title("");
esttab s_new using "summ_stats.csv", append
	cells("count mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))
				p1(fmt(2)) p25(fmt(2)) p50(fmt(2)) p75(fmt(2)) p99(fmt(2))")
	mtitles("New Additions") label title("");
*/
#delimit cr
foreach var of varlist rd_Lat new_products n_patents patent_cites patent_val_real {
	bys ff17 fyear: egen `var'_indmed = median(`var')
	gen H`var' = `var' > `var'_indmed & `var' != .
	gen flt_X_H`var' = flt * H`var'
	gen nflt_X_H`var' = nflt * H`var'
	gen nfht_X_H`var' = nfht * H`var'
	gen f_X_H`var' = frugal * H`var'
	gen ht_X_H`var' = hiT * H`var'
}
/*
log using "ceo_type_tabs.txt", text replace
foreach sub in "" " & max_tenure <= 12" " & original_sample" " & !original_sample" {
	dis "Tagged CEO `sub'"
	tab frugal tercile_t if tag_ceo `sub'
	dis "Hpatent_val_real `sub'"
	tab frugal tercile_t if Hpatent_val_real `sub'
	dis "Hnew_products `sub'"
	tab frugal tercile_t if Hnew_products `sub'
	dis "Hrd_Lat `sub'"
	tab frugal tercile_t if Hrd_Lat `sub'
}
log close
*/
*pause
* RETURNS REGRESSIONS **********************************************************
/*
levelsof ff17, local(indlist)
foreach indicators in "flt nflt nfht" /*"frugal hiT fht"*/ "frugal" "hiT" {
	if "`indicators'" == "flt nflt nfht" local replace "replace"
	else local replace "append"
		
foreach sub in "" "_tenure12" "_orig" "_new" {
	

	if "`sub'" == "" local ifst ""
	if "`sub'" == "_tenure12" local ifst " if max_tenure <= 12"
	if "`sub'" == "_orig" local ifst " if original_sample"
	if "`sub'" == "_new" local ifst " if !original_sample"

local ii = 1
foreach Hinnov of varlist H* {
	if "`indicators'" == "flt nflt nfht" local interactions "flt_X_`Hinnov' nflt_X_`Hinnov' nfht_X_`Hinnov'"
	if "`indicators'" == "frugal" local interactions "f_X_`Hinnov'"
	if "`indicators'" == "hiT" local interactions "ht_X_`Hinnov'"
	
	local rhs `indicators' `Hinnov' `interactions' ln_at ln_emp accdep_ppeg ln_firm_age
	
	foreach dv of varlist firm_ret0 ret_fyear_mktadj ret0_indvw_adj ret0_tnicvw_adj residM3A {
		eststo r`ii': reg `dv' `rhs' i.ff17 `ifst', vce(cluster co_per_rol)
		local ++ii
	} // Y var loop
} // Innovation var loop
esttab r* using "regs_byFF17`sub'.csv", `replace' r2(%9.2f) ar2(%9.2f) b(%9.3f) p(%9.3f) ///
			title("All Industries (w/ FF17 Dummies)")
	

	if "`sub'" == "" local ifst "if "
	if "`sub'" == "_tenure12" local ifst " if max_tenure <= 12 & "
	if "`sub'" == "_orig" local ifst " if original_sample & "
	if "`sub'" == "_new" local ifst " if !original_sample & "
/*	
foreach ff of local indlist {
estimates clear
local ii = 1
	foreach Hinnov of varlist H* {
		local rhs `indicators' `Hinnov' flt_X_`Hinnov' nflt_X_`Hinnov' nfht_X_`Hinnov' ln_at ln_emp accdep_ppeg ln_firm_age
		
		foreach dv of varlist firm_ret0 ret_fyear_mktadj ret0_indvw_adj ret0_tnicvw_adj residM3A {
			eststo r`ii': reg `dv' `rhs' `ifst' ff17 == `ff', vce(cluster co_per_rol)
			local ++ii
		} // Y var loop
	} // Innovation var loop
	
	esttab r* using "regs_byFF17`sub'.csv", append r2(%9.2f) ar2(%9.2f) b(%9.3f) p(%9.3f) ///
					title("FF17 = `ff'")
	
} // Industry loop
*/
} // subsets
} // indicators

* SORTING REGRESSIONS **********************************************************
cap drop *_est_*
est clear

gen rd_at = xrd/at
	lab var rd_at "R&D Scaled by Tot. Assets"
bys ff17 fyear: egen rd_at_indmed = median(rd_at)
gen Hrd_at = rd_at > rd_at_indmed & rd_at != .

	
local ii = 1
foreach Hinnov of varlist Hrd_at Hnew_products Hn_patents Hpatent_cites Hpatent_val_real {
    eststo r`ii', title("`Hinnov'"): reg `Hinnov' flt nflt nfht if tenure == 0, vce(robust)
	local ++ii
    eststo r`ii', title("`Hinnov'         w/ Ind. Dummies"): ///
		reg `Hinnov' flt nflt nfht i.ff17 if tenure == 0, vce(cluster ff17)
	local ++ii
}

*pause


foreach Hinnov of varlist Hrd_at Hnew_products Hn_patents Hpatent_cites Hpatent_val_real {
    eststo r`ii', title("`Hinnov'"): reg `Hinnov' frugal hiT fht if tenure == 0, vce(robust)
	local ++ii
    eststo r`ii', title("`Hinnov'         w/ Ind. Dummies"): ///
		reg `Hinnov' frugal hiT fht i.ff17 if tenure == 0, vce(cluster ff17)
	local ++ii
}

foreach Hinnov of varlist Hrd_at Hnew_products Hn_patents Hpatent_cites Hpatent_val_real {
    eststo r`ii', title("`Hinnov'"): reg `Hinnov' frugal if tenure == 0, vce(robust)
	local ++ii
    eststo r`ii', title("`Hinnov'         w/ Ind. Dummies"): ///
		reg `Hinnov' frugal i.ff17 if tenure == 0, vce(cluster ff17)
	local ++ii
}

foreach Hinnov of varlist Hrd_at Hnew_products Hn_patents Hpatent_cites Hpatent_val_real {
    eststo r`ii', title("`Hinnov'"): reg `Hinnov' hiT if tenure == 0, vce(robust)
	local ++ii
    eststo r`ii', title("`Hinnov'         w/ Ind. Dummies"): ///
		reg `Hinnov' hiT i.ff17 if tenure == 0, vce(cluster ff17)
	local ++ii
}

esttab r* using "ceo_sort_regs.csv", replace r2(%9.2f) ar2(%9.2f) b(%9.3f) p(%9.3f) ///
		title("CEO Sort Regs") order(f* n* h*)

*/

* ACQUISITION INTENSITY REGS ***************************************************
est clear

local ii = 1
foreach rhs in "frugal" "hiT" "frugal hiT fht" {
	foreach aq of varlist aqc_revt aqc_Lat {
		foreach controls in "" "ln_at ln_emp accdep_ppeg ln_firm_age" {
			eststo r`ii': reg `aq' `rhs' `controls' i.fyear, vce(cluster co_per_rol)
			local ++ii
		}
	}
}

esttab r* using "aqc_intensity_regs.csv", replace r2(%9.2f) ar2(%9.2f) b(%9.3f) p(%9.3f) ///
		title("Acquisition Intensity Regs") order(frugal hiT fht) drop(*fyear)

est clear

local ii = 1
foreach rhs in "frugal" "hiT" "frugal hiT fht" {
	foreach aq of varlist rd_revt rd_Lat new_products n_patents patent_cites patent_val_real {
		foreach controls in "" "ln_at ln_emp accdep_ppeg ln_firm_age" {
			eststo r`ii': reg `aq' `rhs' `controls' i.fyear, vce(cluster co_per_rol)
			local ++ii
		}
	}
}
esttab r* using "innov_regs.csv", replace r2(%9.2f) ar2(%9.2f) b(%9.3f) p(%9.3f) ///
		title("Innovation Regs") order(frugal hiT fht) drop(*fyear)
