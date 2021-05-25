/*
mergent_prices.do
*/
cap cd "C:/Users/lmostrom/Dropbox/Abnormal_Returns/"

import delimited "mergent_fisd_prices.csv", clear varn(1)

sort issuer_cusip trans_date
collapse (last) flat_price, by(issuer_cusip transaction_year transaction_type)

ren issuer_cusip cusip6

lab var transaction_type "BUY = purchase by insurer from dealer, SELL = insurer to dealer"

save "mergent_fisd_prices.dta", replace
