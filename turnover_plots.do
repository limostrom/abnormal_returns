/*
turnover_plots.do

*/

clear all
cap log close
pause on

cap cd "C:/Users/lmostrom/Dropbox/Abnormal_Returns"
local plot_dir "Turnover Plots"
cap mkdir "`plot_dir'"

local monthly 0
local annual 0
local qtrly 1


*--------------------
if `monthly' == 1 {
*--------------------
use "master_firm-ceo-mon_vars.dta", clear

duplicates tag lpermno datem, gen(dup)
	bys lpermno datem: egen min_tenure = min(tenure)
	drop if dup & tenure > min_tenure
drop dup
duplicates drop lpermno datem tenure, force
xtset lpermno datem

gen t_event = 0 if tenure == 0
forval x = 1/36 {
    replace t_event = -`x' if f`x'.t_event == 0 & t_event == .
	replace t_event = `x' if l`x'.t_event == 0 & t_event == .
}

#delimit ;
local plot_vars "ret_indadj ret_tnicadj sharpe* sortino* information* upside_dev*";

foreach var of varlist `plot_vars' {;
    preserve;
	
	/*
	gen change = 0 if t_event == 0;
	forval x = 1/36 {;
		replace change = (`var'/f`x'.`var' - 1)*100 if t_event == -`x';
		replace change = (`var'/l`x'.`var' - 1)*100 if t_event == `x';
	};
	br `var' t_event change;
	pause;
	*/
	
	collapse (mean) mean = `var'
			 (p50) median = `var'
			 (p25) p25 = `var'
			 (p75) p75 = `var'
			 (count) N = `var', by(t_event);
	*br;
	*pause;
	tw (line p25 t_event, lc(eltblue) lp(_) lw(thin))
	   (line p75 t_event, lc(eltblue) lp(_) lw(thin))
	   (line mean t_event, lc(black) lp(l0))
	   (line median t_event, lc(blue) lp(l)),
	 legend(order(3 "Mean" 4 "Median" 1 "Quartiles") r(1))
	 yti(/*"% Difference from Turnover Month"*/ "") xti("Months From Turnover")
	 title(Distribution of `var' Around CEO Turnover);
	 
	graph export "`plot_dir'/monthly_`var'.png", replace as(png) wid(1200) hei(700);
	restore;
};
#delimit cr

*--------------------
}
*--------------------

*--------------------
if `annual' == 1 {
*--------------------
cap log close
log using "`plot_dir'/annual_plot_slides.txt", text replace nomsg
cap log close

use "master_firm-year_vars.dta", clear

merge 1:m lpermno gvkey fyear year using "master_firm-ceo-mon_vars.dta", nogen keep(1 3) ///
		keepus(becameceo exec_fullname)
	duplicates drop
	drop if exec_fullname == "" & becameceo == .


duplicates tag lpermno fyear, gen(dup)
	bys lpermno fyear: egen latest_ceo = max(becameceo)
	drop if dup & becameceo < latest_ceo
drop dup
duplicates drop lpermno fyear becameceo, force
xtset lpermno fyear

gen t_event = 0 if int(becameceo/10000) == fyear
forval x = 1/6 {
    replace t_event = -`x' if f`x'.t_event == 0 & t_event == .
	replace t_event = `x' if l`x'.t_event == 0 & t_event == .
}

#delimit ;
local plot_vars "retm_sd retm_skew beta residM? resid_rnoa *_atp75adj *_exindadj firm_*_rev0
					 new_products basu sh_ind_dir_to cfsi overfirm n_patents patent_val_real";

foreach var of varlist `plot_vars' {;
    preserve;
	
	collapse (mean) mean = `var'
			 (p50) median = `var'
			 (p25) p25 = `var'
			 (p75) p75 = `var'
			 (count) N = `var', by(t_event);
	*br;
	*pause;
	tw (line p25 t_event, lc(eltblue) lp(_) lw(thin))
	   (line p75 t_event, lc(eltblue) lp(_) lw(thin))
	   (line mean t_event, lc(black) lp(l0))
	   (line median t_event, lc(blue) lp(l)),
	 legend(order(3 "Mean" 4 "Median" 1 "Quartiles") r(1))
	 yti("") xti("Years From Turnover")
	 title(Distribution of `var' Around CEO Turnover);
	 
	graph export "`plot_dir'/annual_`var'.png", replace as(png) wid(1200) hei(700);
	restore;
	
	cap log close;
	log using "`plot_dir'/annual_plot_slides.txt", text append nomsg;
		dis "%";
		dis "\begin{frame}";
		dis "	\includegraphics[width=\textwidth, height=\textheight, keepaspectratio=true]{annual_`var'.png}";
		dis "\end{frame}";
	cap log close;
	
	*pause;
};
#delimit cr

*--------------------
}
*--------------------

*--------------------
if `qtrly' == 1 {
*--------------------
cap log close
log using "`plot_dir'/qtrly_plot_slides.txt", text replace nomsg
cap log close

use "master_firm-qtr_vars.dta", clear
	drop if gvkey == . & datadate == .
	gen datem = ym(int(datadate/10000), mod(int(datadate/100), 100))
	format %tm datem
	ren fyearq fyear

merge 1:m lpermno gvkey datem fyear using "master_firm-ceo-mon_vars.dta", nogen keep(1 3) ///
		keepus(becameceo exec_fullname)
	duplicates drop
	drop if exec_fullname == "" & becameceo == .

gen dateq = yq(fyear, fqtr)
	format %tq dateq
duplicates tag lpermno dateq, gen(dup)
	bys lpermno dateq: egen latest_ceo = max(becameceo)
	drop if dup & becameceo < latest_ceo
drop dup
duplicates drop lpermno dateq becameceo, force
xtset lpermno dateq



gen t_event = 0 if inrange(becameceo, datadate - 300, datadate)
forval x = 1/20 {
    replace t_event = -`x' if f`x'.t_event == 0 & t_event == .
	replace t_event = `x' if l`x'.t_event == 0 & t_event == .
}

#delimit ;
local plot_vars "atq ltq niq oibdpq revtq inv_Kq";

foreach var of varlist `plot_vars' {;
    preserve;
	
	gen change = 0 if t_event == 0;
	forval x = 1/20 {;
		replace change = (`var'/f`x'.`var' - 1)*100 if t_event == -`x';
		replace change = (`var'/l`x'.`var' - 1)*100 if t_event == `x';
	};
	br `var' t_event change;
	pause;
	
	collapse (mean) mean = change
			 (p50) median = change
			 (p25) p25 = change
			 (p75) p75 = change
			 (count) N = change, by(t_event);
	*br;
	*pause;
	tw (line p25 t_event, lc(eltblue) lp(_) lw(thin))
	   (line p75 t_event, lc(eltblue) lp(_) lw(thin))
	   (line mean t_event, lc(black) lp(l0))
	   (line median t_event, lc(blue) lp(l)),
	 legend(order(3 "Mean" 4 "Median" 1 "Quartiles") r(1))
	 yti("% Difference from Turnover Quarter") xti("Quarters From Turnover")
	 title(Distribution of `var' Change Around CEO Turnover);
	 
	graph export "`plot_dir'/qtrly_`var'.png", replace as(png) wid(1200) hei(700);
	restore;
	
	cap log close;
	log using "`plot_dir'/qtrly_plot_slides.txt", text append nomsg;
		dis "%";
		dis "\begin{frame}";
		dis "	\includegraphics[width=\textwidth, height=\textheight, keepaspectratio=true]{qtrly_`var'.png}";
		dis "\end{frame}";
	cap log close;
	
	*pause;
};
#delimit cr

*--------------------
}
*--------------------