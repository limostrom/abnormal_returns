/*

dataset_merge.do

Merging all datasets for this project into one.
Data dictionary: 

*/


clear all
cap log close
pause on

cd "C:/Users/lmostrom/Dropbox/Abnormal_Returns/"

#delimit ;

	use lpermno date fyear gvkey ME BEtoME OP
	  using returns_annualized_wME_BM_OP, clear;
	  	
	ren lpermno permno;
	gen year = substr(date, 1, 4);
	destring year, replace;

	merge 1:m permno year using returns_annualized, nogen
		keepus(month ret ret_cumul retA mktrf* smb* hml* umd* ps_vwf*);

	ren permno lpermno;
	merge m:1 lpermno fyear using returns_annualized_wFF_portfolios, nogen
		keepus(ff48 ff48_name ME_pct BM_pct OP_pct pf_*);

	ren lpermno permno;
	merge m:1 permno year using firm_year_alphas, nogen
		keepus(*M1* *M3* *M4* *M5*);

	ren permno lpermno;
	merge m:1 lpermno year using firm_year_growth_and_compounded_rets, nogen
		keepus(*ret* *rev_g* *capxsga_rev* *aqc_rev* *do_rev* wt mktcap);

	merge m:1 lpermno year using firm_year_sds_and_skewness.dta, nogen;

	merge m:1 lpermno year using ../investment_efficiency_measures.dta, nogen;

	merge m:1 lpermno year using ../real_earnings_management_measures.dta, nogen;

	drop retA_wtd ind_m_retA;

order lpermno gvkey date year month fyear ff48 ff48_name mktcap wt
		ret ret_cumul retA mktrfA smbA hmlA umdA ps_vwfA
		ret_sd ret_skew niq_sd oibdpq_sd
		*M1* *M3* *M4* *M5*
		*ret* *rev_g* *capxsga_rev* *aqc_rev* *do_rev*
		ME ME_pct BEtoME BM_pct OP OP_pct pf_*
		cfsi cum_avg_rev overfirm overind overagg
		r_cfo r_prod r_disx rm_proxy;


#delimit cr

lab var retA "Total return over the year (compounded if months missing)"
lab var ret_cumul "Cumulative Return over this year"
lab var wt "Weight based on market cap within industry (Fama-French 48)"
lab var ret_sd "standard deviation of monthly firm returns over the year"
lab var ret_skew "skewness of monthly firm returns over the year"
lab var niq_sd "standard deviation of quarterly net income over this and the past 2 years"
lab var oibdpq_sd "standard deviation of quarterly operating income over this and the past 2 years"
lab var r_cfo "Abnormal Cash Flows from Operations / L.Assets"
lab var r_prod "Abnormal Production Costs / L.Assets"
lab var r_disx "Abnormal Discretionary Expenses / L.Assets"
lab var rm_proxy "Proxy for Real Earnings Management (CFO+PROD+DISX)"

save "master.dta", replace