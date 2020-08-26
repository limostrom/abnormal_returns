/*
Hi Rob - Hopefully this can help you come up with a strategy to do the dropping
	industry regressions you're trying to do. Here region is essentially a stand-in
	for SIC. At the end I transpose the matrix so that each regression result has
	its own column as opposed to its own row. If the regressions you're running have
	different numbers of variables in them you might run into some errors
	regarding the matrix dimensions, but we can work together on that issue if it
	comes up. 
*/


pause on
sysuse census, clear

local ii = 1 // Just so the first time around you create the matrix and the
				// times after that you append

levelsof region, local(regions) // here would be 3-digit SIC variable instead
foreach r of local regions {
	reg divorce marriage popurban medage if region != `r'
	if `ii' == 1 mat B = e(b)
	if `ii' > 1 mat B = (B \ e(b)) // adds reg coefficients as a new row
	local ++ii
}

mat rownames B = `regions'
mat B = B' // transpose so regs fill their own columns, not rows

local newcols ""
foreach r of local regions { // we need to do this b/c we need col names that aren't just numbers
	local newcols "`newcols' sic`r'" // naming these SIC might help for your purposes
		// Note: it'll be the reg results from dropping sicX, so the colname is still slightly misleading
}

drop * // otherwise it'll just drop the matrix to the right of your dataset
svmat2 B, names(`newcols') rnames(regvars) // creating a Stata dataset from a matrix
order regvars sic*

outsheet using dropinds_test.csv, comma replace // saves a csv of all regression results

