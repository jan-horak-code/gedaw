
getmin = function (vec, val=1) {

# comments:
	# just returns minimal value by default, or n-th minimal value where n = val
	# ignores NAs

# arguments:
	# vec: Vector, i.e. input numeric vector
	# val: Value, specifies, the order of value sorted from min: i.e. 1 returns minimal value, 2 returns second minimal value, 3 returns third minimal value...
		# works also as a vector, i.e. getmin(5:10) returns 5, getmin (5:10,2) returns 6, getmin(5:10,c(1,3)) returs c(5,7)

# stop conditions

	stopit=F
	if(T%in%is.finite(val)){val=val[is.finite(val)]}else{val=NULL}
	if(is.null(val)){stopit=T}
	if(length(vec)==0){stopit=T}
	if(length(which(is.finite(vec)))==0){stopit=T}
	if(length(which(is.na(vec)))==length(vec)){stopit=T}

# returning NA, if there are no data to be processed

	if(stopit)
		{
			NA
		}
		else

# all the function it this else
	
		{
			resdf = mininfo(vec,h=max(val,na.rm=T))

			vals=as.numeric(unique(resdf$value))[val]

			vals
		}
}


