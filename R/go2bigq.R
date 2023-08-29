# function to convert an mpfr value, or various representations of numerics, to a bigq fraction.
# Author: Carl Witthoft,  carl@witthoft.com
# let user use '...' for args to pass to formatMpfr
# Input "x" must be one of:
#  -- mpfr object
#  --  mpfr1 object
#  --  a numeric object  or vector of numerics (kind of pointless)
# --   a character string of the form '[+,-][digits][.][digits][e,E][+,-][digits]' 
#  --  a list  containing any one of the above types

# Rev 2: changed output to be a single bigq object containing all values rather than list. 

go2bigq <- function(x,  ...){
# validate
theclass <- class(x)
# Here follows all the helper functions
# This avoids all subfuncs  existing in console environment.
dompfr1 <- function(x,...){
jlen = length(x)
if (jlen == 1) {
	# it's a single mpfr1 so don't unlist
	x <- mpfr(x)
	}else{  #it's a list of mpfr1 items
		for(jc in 1:jlen){
			x[[jc]] <- mpfr(x[jc]) #don't use [[ ]] as mpfr() expects the list item
		}
	}
#now process using dompfr()
thebigq <- dompfr(x,...)
return(invisible(thebigq ))
}  

dompfr <- function(x,...) {
jlen = length(x)
xl <- list()
for (jj in 1:jlen) {
	xl[[jj]] <- formatMpfr(x[[jj]],...)
	}
thebigq <- docharacter(xl)
return(invisible(thebigq ))
}

docharacter <- function(x){
# no list!
thebigq <- as.bigq(NULL)  #list()
jlen <- length(x)
for (jj in 1:jlen)	{
	negnum = FALSE
	#count digits to right of dec point, if any 
	xs <-unlist(strsplit(x[[jj]],''))
	if ( xs[1] == '-'){
		negnum = TRUE
		xs <- xs[-1]  
	}
	if (xs[1] == '+') { xs <- xs[-1]}
	theexp <- 0  # initialize
	denom <- '1' # will use this  if there's no decimal point
# check for exponent and adjust num
	 theE <- grep('[eE]', xs)
	 modexp = 0
	if(length(theE)) {
	#exponent could be several digits, or include +/-
		 modexp <- as.numeric(paste0(xs[(theE+1):length(xs)], collapse= '') )
		 xs <- xs[1:(theE-1)] #remove exponent stuff
		} else {theE <- 0 }
	thedot <- grep('[.]', xs)
	if(length(thedot)){
		nontrail <- which(xs!=0)
		xs <- xs[1:max(nontrail)]
		}else{
			xs <- c(xs,'.')
			thedot <- length(xs) 
			}
# watch for  xs ended with a dot or some such
# then  thedot+1 >length(xs), for example  so  strip it
# And, as.bigz treats a leading '0' as a base indicator, strip lead zeros
	if ( which(xs == '.') == length(xs) ){
		theexp = -modexp   
		}else {
			theexp <- length(xs[(thedot+1):(length(xs) ) ]) - modexp
			}
	xs <- xs[-thedot]
	num <- paste0(xs,collapse='')
	num <- as.bigz(gsub('^0{1,}', '', num))
	denom <- as.bigz(10)^(theexp)	#casting order requires bigz(10)
	if (negnum) num <- -num # hah got you
	#no more list!
	thebigq <- c(thebigq, as.bigq(num,denom))
	thebigq[[jj]] <- as.bigq(num,denom)
	}
return(invisible(thebigq ))
} #end of docharacter

# Philosophical: if I just do as.bigq, it'll examine the full binary rounded numeric.Since as.numeric(as.bigq(x)) will return the same binary rounded double, that's fine. 
donumeric <- function(x){
# make NOT  list to be consistent with all other branches. 
thebigq <- (as.bigq(x))
return(invisible(thebigq ))
}
# Ok, back to doing work. 
# figure out the true class of data, and then  process each type appropriately
#If it's a list,  dive to get teh real class
# Wow, CRAN is getting really picky
#if ( class(x)== 'list'){
# rev2 fixed stupid bug - see line above
if (is(x, 'list')) {
	theclass <- class(x[[1]]) 
	if(is(theclass, 'list')) {    #if (theclass == 'list') {
	# this will catch result of, e.g.,  unlist(mpfr_class)
	#let's be explicit
		theclass = class(x[[1]][[1]])
	} 
 }
switch(theclass,
	mpfr1 =  dompfr1(x,...),
	mpfr = dompfr(x,...),
	numeric = donumeric(x),
	character = docharacter(x),
	stop('input must be numeric, mpfr, mpfr1, char,  or a list containing one of those')
	)

} #end of MAIN



