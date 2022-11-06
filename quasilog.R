
quasilog = function (vec) { 

# comments:
	# computes quasi-log of a vector
	# the same as logarithmic transformation with base 10, but it works also with negative values
	# use only for better visualisation of some rasters
	# zeros will be zeros again
	# and NAs will be NAs again


# arguments:
	# vec is for input vector

nas=is.na(vec)
negatives=vec<0
negatives[is.na(negatives)]=F

vec.qlog=vec # ql for QuasiLog
vec.qlog[nas]=0 # to deal with NAs during logar()
vec.qlog=abs(vec.qlog)
vec.qlog=vec.qlog+1
vec.qlog=log(vec.qlog, base=10)
vec.qlog[negatives]=vec.qlog[negatives]*-1
vec.qlog[nas]=NA # return of NAs
vec.qlog

}



