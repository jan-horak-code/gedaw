
plotrastql = function (r, col,axes=T,legend=T) {

# comments:
	# name "plotrastql" stands for Plot Raster with Quasi Logged values
	# just plots a raster, but the values are recomputed by quasilog() function
	# the colour palette needs to be specified - it computes needed number of colours in specified palette
		# in final, it does not need to be the same number - it depends on the specific dataset and its properties

# arguments:
	# r is a raster
	# col is a colour palette

# workflow:

	rql=setValues(r,quasilog(r@data@values))
	plot(rql, col=col, axes=axes, legend=legend)

}


