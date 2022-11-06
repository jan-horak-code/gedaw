
getmax = function (vec, val=1) {

# comments:
	# just returns maximal value by default, or n-th maximal value where n = val
	# ignores NAs

# arguments:
	# vec: Vector, i.e. input numeric vector
	# val: Value, specifies, the order of value sorted from max: i.e. 1 returns maximal value, 2 returns second maximal value, 3 returns third maximal value...
		# works also as a vector, i.e. getmax(1:10) returns 10, getmax (1:10,2) returns 9, getmax(1:10,c(1,3)) returs c(10,8)

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
			resdf = maxinfo(vec,h=max(val,na.rm=T))

			vals=as.numeric(unique(resdf$value))[val]

			vals
		}
}


