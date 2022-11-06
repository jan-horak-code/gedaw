

pcars = function (rs, sc=T, co=T){

# comments:
	# the function takes a stack of rasters and computes PCA on it, taking every raster as a variable
	# the function returns the list containing list of pca results and stack of rasters with values of the scores of the PCA

# arguments:
	# rs = RasterStack - the stack of rasters
	# sc = scale: decision if the scaling should be performed
	# co: correlation matrix, if co=F, the pca will be performed on covariation matrix

# part 1: creating the dataframe from te stack

	idf.all=as.data.frame(rs)
	idf.nas=which(is.na(idf.all[,1])) # presumption, that all the rasters have same strucure and positions of the finites and NAs...
	idf.fin=which(is.finite(idf.all[,1]))
	idf=idf.all[idf.fin,]
	if(sc){idf=scale(idf)}else{}

# part 2: performing PCA on the dataframe

	pca=princomp(idf, cor=co)

# part 3: creating the output stack of rasters and filling it with the data

	res.st=setValues(rs,NA) # taking input stack of rasters and removing its values
	res.df=idf.all # the same with the dataframe
		res.df[]<-NA

	for (i in 1:nlayers(res.st)) # filling the blank result dataframe
		{
			res.df[idf.fin,i]<-pca$scores[,i]
		}

	for (i in 1:nlayers(res.st)) # filling the blank result stack
		{
			res.st[[i]]=setValues(res.st[[i]],res.df[,i])
		}
	names(res.st)=paste("PC",1:nlayers(res.st),sep="")

result=list(pca,res.st)

result

}





