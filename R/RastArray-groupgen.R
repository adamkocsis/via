###########################################################

# Arith methods
setMethod("Arith", c(e1="RastArray", e2="numeric"), 
	definition=function(e1,e2){
		e1@stack <- methods::callGeneric(e1@stack, e2)
		e1
	}
)

setMethod("Arith", c(e2="RastArray", e1="numeric"), 
	definition=function(e1,e2){
		e2@stack <- methods::callGeneric(e2@stack, e1)
		e2
	}
)

###########################################################
# Compare method

setMethod("Compare", c(e1="RastArray", e2="SpatRaster"), 
	definition=function(e1,e2){
		e1@stack <- methods::callGeneric(e1@stack, e2)
		e1
	}
)

###########################################################
# Math method
setMethod("Math", c(x="RastArray"), 
	definition=function(x){
		x@stack <- methods::callGeneric(x@stack)
		x
	}
)

###########################################################
# Math2 method
setMethod("Math2", signature=c(x="RastArray"), 
	definition=function(x, digits){
		op=.Generic[[1]]
		switch(op,
			round = return({
					if(missing(digits)) digits <- 0
					x@stack <- round(x@stack,digits)
					x
				}),
			signif = return({
					if(missing(digits)) digits <- 6
					x@stack <- signif(x@stack,digits)
					x
				})
			
		)
	}
)

setMethod("Summary", c(x="RastArray"), 
	definition=function(x,..., na.rm=FALSE){
		op<-.Generic[[1]]
	#	if(op=="range"){

	#	}else{
			methods::callGeneric(x@stack,..., na.rm=na.rm)	
	#	}
		
	}
)
