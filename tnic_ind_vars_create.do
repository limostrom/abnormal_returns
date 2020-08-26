/*

Using TNIC codes (Hoberg & Phillips, 2016) to calculate industry variables)

lab var tnic_at_p75 "Tot. Assets > 75th pctl by industry"

*/

clear all
cap log close
pause on


cap cd "/export/home/dor/lmostrom/Documents/Dropbox"


	*---- EXECUCOMP --------------------------------------------------------
	use gvkey year using "Execucomp.dta", clear
		ren gvkey gvkey1
		gen gvkey2 = gvkey1
		duplicates drop
		tempfile exec_subset
		save `exec_subset', replace
		
	*---- COMPUSTAT --------------------------------------------------------
	use gvkey lpermno datadate fyear sic sale revt prcc_c csho ni oibdp oiadp ///
			at lt oancf rect invt aco ppent intan ao ap lco lo ///
			aqc capx xsga do xrd ///
			using "Compustat-CRSP_Merged_Annual.dta", clear
		gen gvkey1 = gvkey
		gen gvkey2 = gvkey
		gen datem = ym(int(datadate/10000), mod(int(datadate/100),100))
			format datem %tm
		
		egen gvkey2_lpermno = group(gvkey2 lpermno)
		xtset gvkey2_lpermno fyear
		
		gen roa = (ni/at)*100
			lab var roa "Return on Assets (%)"
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
		gen rd_intensity = xrd/at
			lab var rd_intensity "R&D Expense / Sales"
		gen salesgr = (f.sale - sale)/sale*100
			lab var salesgr "Sales Growth, (t to t+1) (%)"
		gen mktcap = prcc_c*csho
			lab var mktcap "Market Capitalization ($ M)"
			
		*RNOA, OMADP, and ATO from Li, Lundholm, and Minnis (2012)
		gen noa = rect + invt + aco + ppent + intan + ao ///
					- ap - lco - lo
			lab var noa "Net Operating Assets"
		gen rnoa = oiadp/noa
			lab var rnoa "Return on Net Operating Assets (%)"
		gen ato = revt/noa
			lab var ato "Net Operating Asset Turnover Ratio"
			
		tempfile compustat
		save `compustat', replace
		
		keep gvkey datem fyear
		drop if fyear == .
		duplicates drop
		expand 12
		bys gvkey fyear: replace datem = datem + 1 - _n
		duplicates tag gvkey datem, gen(dup)
		bys gvkey datem: egen max_fyear = max(fyear)
			drop if dup & fyear < max_fyear
		isid gvkey datem
		save "gvkey_fyear_bydatem.dta", replace
	
	use gvkey datadate trt1m ///
			using "Compustat-CRSP_Merged_Monthly.dta", clear
		duplicates drop
		drop if trt1m == .
		gen year = int(datadate/10000)
		gen month = mod(int(datadate/100), 100)
		gen datem = ym(year, month)
			format %tm datem
		duplicates drop gvkey datadate, force
		gen gvkey1 = gvkey
		gen gvkey2 = gvkey
		tempfile monthly
		save `monthly', replace
	*-----------------------------------------------------------------------
	
	*---- NEW PRODUCTS -----------------------------------------------------
	use gvkey announcedate using "CapitalIQ_KeyDevelopments.dta", clear
			gen datem = ym(int(announcedate/10000),mod(int(announcedate/100),100))
		merge m:1 gvkey datem using "gvkey_fyear_bydatem.dta", nogen keep(1 3) keepus(fyear)
		gen n = _n
		
		collapse (count) new_products = n, by(gvkey fyear) fast
		replace new_products = new_products/1000
		ren gvkey gvkey1
		gen gvkey2 = gvkey1
		tempfile newproducts
		save `newproducts', replace
		
		ren gvkey1 gvkey
		keep gvkey fyear new_products
		save "Abnormal_Returns/firm_new_products.dta", replace
	*-----------------------------------------------------------------------
	
	*---- ANNUALIZED RETURNS -----------------------------------------------
	use permno date year retA ///
		using "Abnormal_Returns/returns_annualized.dta", clear

	ren permno lpermno
	gen month = substr(date, 5, 2)
		destring month, replace
	gen datem = ym(year, month)
		format datem %tm
	
	tempfile ret_ann
	save `ret_ann', replace
	*-----------------------------------------------------------------------
	
			
import delimited "Abnormal_Returns/Hoberg Phillips/tnic3_data.txt", clear varn(1)
keep if score >= 0.2132

	ren year fyear // this truly is end of fiscal (not cal) year; based on SEC filings
	levelsof fyear, local(years)
	joinby gvkey2 fyear using `compustat', _merge(m_cpu) unm(master)
		gen year = int(datadate/10000)
		merge m:1 lpermno datem using `ret_ann', nogen keep(1 3) keepus(retA)
	merge m:1 gvkey1 year using `exec_subset', gen(m_excomp1) keep(1 3)
	merge m:1 gvkey2 year using `exec_subset', gen(m_excomp2) keep(1 3)
	
	drop if fyear == . & at == . & ni == .
	duplicates drop
	
	* For Industry HHI
	bys gvkey1 fyear: egen tot_tnic_sales = total(sale)
		gen sh_tnic_sales = sale/tot_tnic_sales * 100
			lab var sh_tnic_sales "Share of TNIC Sales (%)"
		gen sh_tnic_sales_sq = sh_tnic_sales^2
			lab var sh_tnic_sales_sq "Squared Share of Industry Sales"
			
	* For New Products
	merge m:1 gvkey2 fyear using `newproducts', nogen keep(1 3)
	
	* For Performance Metrics (Industry-Adjusted) // -----------------------
	foreach var of varlist sale revt xrd ni oibdp oiadp at lt oancf rect invt aco ///
				ppent intan ao ap lco lo {
		dis "`var'"
		replace `var' = 0 if `var' == .
	}
		
	foreach var of varlist roa roe rnoa revgr omadp ato ombdp earngr cfogr {
		*by Industry and Year
		bys gvkey1 fyear: egen `var'_tnic_indmed = median(`var')
			lab var `var'_tnic_indmed "Industry (TNIC) Median of `var' (by year)"
			
		*by Industry and Year for Execucomp firms
		bys gvkey1 fyear: egen `var'_tnic_exindmed = median(`var') if m_excomp1 == 3 & m_excomp2 == 3
			lab var `var'_tnic_exindmed "Industry (TNIC) Median of `var' (by year, Execucomp firms)"
	} // -------------------------------------------------------------------
	
	* Industry Value-Weighted (Mkt Cap) Returns // -------------------------
		* --- For Rev. Growth & Forward-Looking Sums --- *
		preserve
			use gvkey lpermno datadate fyear revt capx xsga aqc do csho prcc_c ///
				using "Compustat-CRSP_Merged_Annual.dta", clear
			duplicates drop
			drop if fyear == .
			merge m:1 lpermno fyear using "Abnormal_Returns/firm_year_growth_and_compounded_rets.dta", ///
				nogen keep(1 3) keepus(firm_*)
			gen mktcap = csho * prcc_c
			bys gvkey fyear: egen tot_mktcap = total(mktcap)
				gen lp_wt = mktcap/tot_mktcap
				assert inrange(lp_wt, 0, 1) | lp_wt == .
			foreach var of varlist firm_* {
				replace `var' = `var' * lp_wt if lp_wt != .
 			}
			collapse (sum) firm_*, by(gvkey fyear) fast
			gen gvkey2 = gvkey
			tempfile firm_vars
			save `firm_vars', replace
		restore
		
	bys gvkey1 fyear: egen tnic_mktcap_tot = total(mktcap)
		gen wt_tnic = mktcap/tnic_mktcap_tot
			bys gvkey1 fyear: egen check = total(wt_tnic) // just making sure the weights add to 1
			assert inrange(check, 0.999, 1.001) if mktcap != .
			drop check
	
	merge m:1 gvkey2 fyear using `firm_vars', nogen keep(1 3) keepus(firm_*)
	
	local collapse_sums ""
	local collapse_meds ""
	foreach var of varlist retA aqc capx xsga do revt ///
				firm_ret* firm_rev_g* firm_capxsga_rev* firm_aqc_rev* firm_do_rev* {
		if !inlist("`var'", "retA", "revt") replace `var' = 0 if `var' == .
		gen `var'_tnic_wtd = `var'*wt_tnic
			lab var `var'_tnic_wtd "`var' weighted by market cap share of TNIC"
		if substr("`var'",1,5) == "firm_" local varn = substr("`var'",6,.)
		else local varn = "`var'"
		local collapse_sums "`collapse_sums' tnic_vw_`varn' = `var'_tnic_wtd"
		local collapse_meds "`collapse_meds' tnic_m_`varn' = `var'"
	}
	
	drop gvkey
	gen gvkey = gvkey1
	
	#delimit ;
	collapse (mean) tnic_rd_avg = rd_intensity tnic_salesgr_avg = salesgr
		 (sum) tnic_hhi = sh_tnic_sales_sq tnic_new_products = new_products
			tnic_mktcap_tot = mktcap `collapse_sums'
		 (first) *_tnic_indmed *_tnic_exindmed
		 (p50) `collapse_meds' salesgr_tnicmed = salesgr,
		by(gvkey fyear) fast;
	#delimit cr
	
	lab var tnic_vw_retA "TNIC value-weighted annualized return"
	lab var tnic_vw_aqc "TNIC value-weighted Acquisitions"
	lab var tnic_vw_capx "TNIC value-weighted Capex"
	lab var tnic_vw_xsga "TNIC value-weighted SG&A"
	lab var tnic_vw_do "TNIC value-weighted Discontinued Operations"
	lab var tnic_vw_revt "TNIC value-weighted Total Revenue"
	
	lab var tnic_vw_ret0 "TNIC value-weighted return over this year"
	lab var tnic_m_ret0 "TNIC median return over this year"
	lab var tnic_vw_rev_g0 "TNIC value-weighted revenue growth from last year to now"
	lab var tnic_m_rev_g0 "TNIC median revenue growth from last year to now"
	lab var tnic_vw_capxsga_rev0 "TNIC value-weighted Capex + SG&A over Revenue"
	lab var tnic_m_capxsga_rev0 "TNIC median Capex + SG&A over Revenue"
	lab var tnic_vw_aqc_rev0 "TNIC value-weighted Acquisitions over Revenue"
	lab var tnic_m_aqc_rev0 "TNIC median Acquisitions over Revenue"
	lab var tnic_vw_do_rev0 "TNIC value-weighted Discontinued Ops over Revenue"
	lab var tnic_m_do_rev0 "TNIC median Discontinued Ops over Revenue"
	
	forval i = 1/10 {
		lab var tnic_vw_ret`i' "TNIC value-weighted return over this and the next `i' year(s)"
		lab var tnic_m_ret`i' "TNIC value-weighted return over this and the next `i' year(s)"
		lab var tnic_vw_rev_g`i' "TNIC value-weighted revenue growth from last year to `i' year(s) from now"
		lab var tnic_m_rev_g`i' "TNIC median revenue growth from last year to `i' year(s) from now"
		lab var tnic_vw_capxsga_rev`i' "TNIC value-weighted Capex + SG&A over Revenue (values summed over next `i' yrs)"
		lab var tnic_m_capxsga_rev`i' "TNIC median Capex + SG&A over Revenue (values summed over next `i' yrs)"
		lab var tnic_vw_aqc_rev`i' "TNIC value-weighted Acquisitions over Revenue (values summed over next `i' yrs)"
		lab var tnic_m_aqc_rev`i' "TNIC median Acquisitions over Revenue (values summed over next `i' yrs)"
		lab var tnic_vw_do_rev`i' "TNIC value-weighted Discontinued Ops over Revenue (values summed over next `i' yrs)"
		lab var tnic_m_do_rev`i' "TNIC median Discontinued Ops over Revenue (values summed over next `i' yrs)"
	}
	
	lab var tnic_rd_avg "Industry (TNIC) Avg R&D Intensity (R&D/Assets)"
	lab var tnic_salesgr_avg "Industry (TNIC) Avg Sales Growth (t to t+1) (%)"
	lab var salesgr_tnicmed "TNIC Median Sales Growth (t to t+1) (%)"
	lab var tnic_hhi "Industry (TNIC) HHI"
	lab var tnic_new_products "Industry (TNIC) Total New Products (Thousands)"
	
	* --- 3-Year Averages --- *
	rangestat (mean) tnic_rd_3yr = tnic_rd tnic_salesgr_3yr = tnic_salesgr ///
			tnic_hhi_3yr = tnic_hhi tnic_new_products_3yr = tnic_new_products, ///
			interval(fyear -2 0) by(gvkey)
			
	lab var tnic_rd_3yr "3-Year Industry (TNIC) Avg R&D Intensity (R&D/Assets)"
	lab var tnic_salesgr_3yr "3-Year Industry (TNIC) Avg Sales Growth (t to t+1) (%)"
	lab var tnic_hhi_3yr "3-Year Industry (TNIC) Avg HHI"
	lab var tnic_new_products_3yr "3-Year Avg New Products by TNIC (Thousands)"
	
	// ---------------------------------------------------------------------
	
save "tnic_ind_vars.dta", replace

		
import delimited "Abnormal_Returns/Hoberg Phillips/tnic3_data.txt", clear varn(1)
keep if score >= 0.2132

	expand 12
		bys gvkey1 gvkey2 year: gen month = _n
		assert month <= 12
		gen datem = ym(year, month)
			format %tm datem
	
	merge m:1 gvkey2 datem using `monthly', nogen keep(1 3) keepus(trt1m datem)
		ren trt1m ret_tnic
	
	sort gvkey1 gvkey2 datem
	collapse (p50) ret_tnicmed = ret_tnic, by(gvkey1 datem) fast
		ren gvkey1 gvkey
	
save "tnic_ind_monthly.dta", replace
	
	
	
	
	
	
	
