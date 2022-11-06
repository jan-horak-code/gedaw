
plotrastec = function (r, colours=c("violet", "blue", "green", "yellow", "orange", "red"), nc=30, axes=T, legend=T) {

# comments:
	# name "plotrastec" stands for Plot Raster by Equal Counts histogram
	# just plots a raster, but the colour palette is breaked into steps according to the equal count of cases in a class

# arguments:
	# r is a raster
	# cols are the colours for the palette
	# nc: Number of Colours in a palette
	# the colours and nc need to be specified - the colour palette is compute from it
	# the palette will be computed using colorRampPalette() function


# workflow:

	new.breaks=unique(comphist(r@data@values,nc=30)$ec.breaks)
	new.classes=length(new.breaks)-1
	new.palette = colorRampPalette(colours)(n = new.classes)

	plot(r,col=new.palette,breaks=new.breaks,axes=axes,legend=legend)

}


