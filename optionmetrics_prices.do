/*
optionmetrics_prices.do

*/

cd "C:\Users\lmostrom\Dropbox\"

import delimited "OptionMetrics_CRSP_Link.csv", clear
keep secid permno
drop if permno == .
duplicates drop
keep secid

export delimited "optionm_secids.txt", replace novarnames

