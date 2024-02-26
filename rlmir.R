

rlmir = function (rst, ind="r", n=NULL, it=10, rin=NULL, rex=NULL, rv=NULL, na=F, nc=20, ms=T, lmmethod="qr", cmethod="pearson", limit=10, cor.round=2, lmc.round=2, intcp.round=2, rsq.round=2, adjrsq.round=2, resid.round=2,pval.round=4, stde.round=2) {

# arguments
	# input raster, specification of focal window, specification of randomization
		# rst if for input raster
		# ind is for which raster in lm will be independent, "r" means randomized, "i" means input, anything else is same as "r"
		# n is for N - the number which specifies the Focal window - the ngb argument in getFocalValues(), the number of cells in x and y direction, it can be also vector of two values
		# it stands for Iterations, means number of iterations to be performed
		# rin: Randomize type to INclude: randomize types to use
		# rex: Randomize types to EXclude: do not use these types
		# rv: Randomized Vectors: if you want to use your own randomized values, pass a list of vectors with these values to this argument; in case the list will be smaller than number of iterations (argument "it"), the rest of iterations will be filled by vectors created by function "randomize"
		# na is argument passed randomize(), by default na=F, so the NA values will not be included
		# nc: Number of Classes: passed to comphist() inside randomize()
		# ms stands for mask, by default ms=T, so the resulting rasterbricks will be masked by original raster
	# lm parameters, limit for needed minimum of values to be used
		# meth is for method, like pearson etc.
		# limit is for limit - the lm will be computed only if in both rasters are at least limit number of values (this prevents function of crashing in case ther are only NAs)
			# there is if condition - in case that the number of window cells would be smaller than limit, limit will be set to the number of window cells
		# arguments for rounding of the results


# structure of the result list:
	# list of infos:
		# original raster
		# other infos
	# brick of ranfomized rasters
	# brick for every analyzed parameter


# ----- start of the function

		starting.time=Sys.time()
		cat(paste("[starting function; at: ", format(Sys.time(), "%H:%M:%S"),"]",sep=""),"\n")





# working objects

rows.n=nrow(rst)
cols.n=ncol(rst)

# arguments checks and changes

if(is.null(n)) # just dealing with situation, when n is not specified, it basically sets n to 1/10 of the smaller dimension of the raster, the if condition is just searching for odd value
	{
		nc=ceiling(getmin(c(rows.n,cols.n))/10)
		nf=floor(getmin(c(rows.n,cols.n))/10)
		if (is.odd(nc) | is.odd(nf))
			{
				nfc=c(nf,nc)
				n=nfc[is.odd(nfc)]
			}
			else
			{
				n=nf-1
			}
	}
	else
	{
	}

if(length(n)==1){window =c(n,n)}else{window =c(n[1],n[2])}	# w is for Window - this is used in getFocalValues(); written so it has no prblem if more than two values are inputed

nwc=window[1]*window[2] # nwc is for Number of Window Cells

if(nwc<limit){limit=nwc}else{} # adjusting limit, so it will not produce just raster of NAs in case nwc<limit...


# preparation of list of results

# -- making the raster which will be used for filling with results

rr <- rst	 	# Resulting Raster is based on original raster, so everything will be the same
rr[] <- NA 		# resulting raster is just NAs

# -- making the results list

results.all=list(
	input.raster=rst,
	information=list(
		input.raster.name=deparse(substitute(rst)),
		input.variable.name=names(rst)[1],
		randomized.variable.name=paste(names(rst)[1],"random",sep="."),
		input.variable.is=NA,
		input.variable.is.abbreviated=NA,
		randomized.variable.is=NA,
		randomized.variable.is.abbreviated=NA,
		ind=ind,
		independent.variable.name=NA,
		dependent.variable=NA,
		cmethod=cmethod,
		lmmethod=lmmethod,
		limit.passed=limit,
		number.of.window.cells=nwc,		
		window.x=window[1],
		window.y=window[2],
		randomize.types=c(rep(NA,it)),
		list.of.randomized.vectors=list()
		),
	brick.of.randomizations=NA,
	brick.of.correlation.coefficient=NA,
	brick.of.lm.coefficient=NA,
	brick.of.lm.Intercept=NA,
	brick.of.r.squared=NA,
	brick.of.adjusted.r.squared=NA,
	brick.of.median.of.residuals=NA,
	brick.of.mean.of.residuals=NA,
	brick.of.sum.of.residuals=NA,
	brick.of.median.of.abs.of.residuals=NA,
	brick.of.mean.of.abs.of.residuals=NA,
	brick.of.sum.of.abs.of.residuals=NA,
	brick.of.p.value=NA,
	brick.of.standard.error=NA
	)


# --- adding info, which depends on parameters

if (ind=="i")
	{
		results.all$information$input.variable.is="independent"
		results.all$information$input.variable.is.abbreviated="indep."
		results.all$information$randomized.variable.is="dependent"
		results.all$information$randomized.variable.is.abbreviated="dep."
		results.all$information$independent.variable.name=results.all$information$input.variable.name
		results.all$information$dependent.variable=paste(results.all$information$input.variable.name, "random", sep=".")
	}
	else
	{
		results.all$information$input.variable.is="dependent"
		results.all$information$input.variable.is.abbreviated="dep."
		results.all$information$randomized.variable.is="independent"
		results.all$information$randomized.variable.is.abbreviated="indep."
		results.all$information$independent.variable.name=paste(results.all$information$input.variable.name, "random", sep=".")
		results.all$information$dependent.variable=results.all$information$input.variable.name
	}


# --- Randomization: creating the brick with rasters with randomized values

# ---- creating RasterBrick of blank rasters to be filled by randomized vectors

results.all$brick.of.randomizations=rst # the first one...

for(ii in 2:it) # stacking iterations
	{
		results.all$brick.of.randomizations=stack(results.all$brick.of.randomizations,rst)
	}

results.all$brick.of.randomizations[]<-NA

# ---- creating a list of vectors

for(iii in 1:it)
	{
		results.all$information$list.of.randomized.vectors[[iii]]=rep(NA,length(rst))
	}

# ---- checking for rv argument

if(is.list(rv))
	{
		 if(length(rv)>=it)
			{
				rv=rv[1:it]
				for.rv=c(1:it)
				for.randomize=NULL
				results.all$information$randomize.types[for.rv]=0
			}
			else
			{
				for.rv=c(1:length(rv))
				for.randomize=c((length(rv)+1):it)
				results.all$information$randomize.types[for.rv]=0
			}
	}
	else
	{
		for.rv=NULL
		for.randomize=c(1:it)
	}


# ---- filling the list of randomized vectors by argument rv

for (iiii in for.rv)
	{
		results.all$information$list.of.randomized.vectors[[iiii]]=rv[[iiii]]
	}

# ---- filling the list of randomized vectors by function randomize


# ----- choose of randomize types

r.types=c(1,2,3,4,5,6)

	if(is.null(rin))
		{
			if(is.null(rex))
				{
					types.to.choose=r.types
				}
				else
				{
					types.to.choose=r.types[which(!(r.types%in%rex))]
				}
		}
		else
		{
			types.to.choose=rin
		}

# ----- filling the values

for (iiii in for.randomize)
	{
		type.chosen=sample(types.to.choose,1)
		results.all$information$list.of.randomized.vectors[[iiii]]=randomize(rst@data@values,t=type.chosen,na=na,nc=nc)
		results.all$information$randomize.types[iiii]=type.chosen
	}

# --- filling the brick of randomizations by values from list of randomized vectors

for(iiiii in c(for.rv,for.randomize))
	{
		results.all$brick.of.randomizations[[iiiii]]=setValues(results.all$brick.of.randomizations[[iiiii]], results.all$information$list.of.randomized.vectors[[iiiii]])
	}


# --- preparation of other RasterBricks


results.all$brick.of.correlation.coefficient=results.all$brick.of.randomizations # copying randomizations brick

results.all$brick.of.correlation.coefficient[]<-NA # just making them blank and below copying it making other bricks

results.all$brick.of.lm.coefficient=results.all$brick.of.correlation.coefficient
results.all$brick.of.lm.Intercept=results.all$brick.of.correlation.coefficient
results.all$brick.of.r.squared=results.all$brick.of.correlation.coefficient
results.all$brick.of.adjusted.r.squared=results.all$brick.of.correlation.coefficient
results.all$brick.of.median.of.residuals=results.all$brick.of.correlation.coefficient
results.all$brick.of.mean.of.residuals=results.all$brick.of.correlation.coefficient
results.all$brick.of.sum.of.residuals=results.all$brick.of.correlation.coefficient
results.all$brick.of.median.of.abs.of.residuals=results.all$brick.of.correlation.coefficient
results.all$brick.of.mean.of.abs.of.residuals=results.all$brick.of.correlation.coefficient
results.all$brick.of.sum.of.abs.of.residuals=results.all$brick.of.correlation.coefficient
results.all$brick.of.p.value=results.all$brick.of.correlation.coefficient
results.all$brick.of.standard.error=results.all$brick.of.correlation.coefficient


# --- adding original raster into the brick of randomizations, as function getValuesFocal() works on one object...
		# after all is done, this will be removed

results.all$brick.of.randomizations=stack(results.all$brick.of.randomizations,rst)


# --- giving names to variables

names(results.all$brick.of.randomizations)			=c(paste(results.all$information$randomized.variable.name, 1:it, sep="."),paste(results.all$information$input.variable.name, "original", sep="."))
names(results.all$brick.of.correlation.coefficient)		=paste("cor.coef", 1:it, sep=".")
names(results.all$brick.of.lm.coefficient)			=paste("lm.coef", 1:it, sep=".")
names(results.all$brick.of.lm.Intercept)				=paste("intcp", 1:it, sep=".")
names(results.all$brick.of.r.squared)				=paste("r.sq", 1:it, sep=".")
names(results.all$brick.of.adjusted.r.squared)			=paste("adj.r.sq", 1:it, sep=".")
names(results.all$brick.of.median.of.residuals)			=paste("med.resid", 1:it, sep=".")
names(results.all$brick.of.mean.of.residuals)			=paste("mean.resid", 1:it, sep=".")
names(results.all$brick.of.sum.of.residuals)			=paste("sum.resid", 1:it, sep=".")
names(results.all$brick.of.median.of.abs.of.residuals)	=paste("med.abs.resid", 1:it, sep=".")
names(results.all$brick.of.mean.of.abs.of.residuals)		=paste("mean.abs.resid", 1:it, sep=".")
names(results.all$brick.of.sum.of.abs.of.residuals)		=paste("sum.abs.resid", 1:it, sep=".")
names(results.all$brick.of.p.value)					=paste("p.val", 1:it, sep=".")
names(results.all$brick.of.standard.error)			=paste("stde", 1:it, sep=".")


# ------ resulting list is ready to be filled
# ----------------



# ----------
# ------ now making loop, which will produce and fill in the results

for (j in 1:it)	# loop 1 j iterating through brick of randomizations
	{

		cat(paste("[starting iteration ", j, " of ", it, "; at: ", format(Sys.time(), "%H:%M:%S"),"]",sep=""),"\n")
		
		for (jj in 1:rows.n)	# loop 2 jj iterating through raster rows and applying getFocalValues, the rasters are same, so I am doing it on specified raster
			{
				fvjj <- getValuesFocal(results.all$brick.of.randomizations[[c(j,it+1)]], row=jj, nrows=1, ngb = window, array = FALSE)	# ffj is for FocalValues in loop jj; IMPORTANT: getFocalValues gives list, so as the original raster is at the end of brick of randomizations, it will be ALWAYS SECOND here, as it has to be specified later which values will be dependent and which independent
				row.jj.res.cor.c=rep(NA,cols.n)
				row.jj.res.lm.c=rep(NA,cols.n)
				row.jj.res.lm.intcp=rep(NA,cols.n)
				row.jj.res.r.sq=rep(NA,cols.n)
				row.jj.res.ad.r.sq=rep(NA,cols.n)
				row.jj.med.resid=rep(NA,cols.n)
				row.jj.mean.resid=rep(NA,cols.n)
				row.jj.sum.resid=rep(NA,cols.n)
				row.jj.med.abs.resid=rep(NA,cols.n)
				row.jj.mean.abs.resid=rep(NA,cols.n)
				row.jj.sum.abs.resid=rep(NA,cols.n)
				row.jj.res.pv=rep(NA,cols.n)
				row.jj.res.stde=rep(NA,cols.n)

				for (jjj in 1:cols.n) # loop 3 jjj iterating through columns in jj row and thus results in martrices in fvjj and performing lm on the data
					{
						if (ind=="i") # if condition deciding, what raster is dependent and independent;
							{
								indep.vals=fvjj[[2]][jjj,][which(is.finite(fvjj[[2]][jjj,]) & is.finite(fvjj[[1]][jjj,]))]	# this means, the second matrix is independent, i.e. coming from original input raster; which() filtering the input data combines both vectors, as due to "rv" argument, the data not respecting positions of NAs could be passed into the function (and thus the length of both vectors would not be the same)
								dep.vals=fvjj[[1]][jjj,][which(is.finite(fvjj[[1]][jjj,]) & is.finite(fvjj[[2]][jjj,]))]
							}
							else
							{
								indep.vals=fvjj[[1]][jjj,][which(is.finite(fvjj[[1]][jjj,]) & is.finite(fvjj[[2]][jjj,]))]	# this means, the first matrix is independent, i.e. coming from randomized rasters; which() filtering the input data combines both vectors, as due to "rv" argument, the data not respecting positions of NAs could be passed into the function (and thus the length of both vectors would not be the same)
								dep.vals=fvjj[[2]][jjj,][which(is.finite(fvjj[[2]][jjj,]) & is.finite(fvjj[[1]][jjj,]))]
							}

						if (length(indep.vals)<limit | length(dep.vals)<limit)	# if condition searching for sufficient number of values for lm - the "limit" argument, which can be chaged by nwc!!!
							{
								row.jj.res.cor.c[jjj]=NA
								row.jj.res.lm.c[jjj]=NA
								row.jj.res.lm.intcp[jjj]=NA
								row.jj.res.r.sq[jjj]=NA
								row.jj.res.ad.r.sq[jjj]=NA
								row.jj.med.resid[jjj]=NA
								row.jj.mean.resid[jjj]=NA
								row.jj.sum.resid[jjj]=NA
								row.jj.med.abs.resid[jjj]=NA
								row.jj.mean.abs.resid[jjj]=NA
								row.jj.sum.abs.resid[jjj]=NA
								row.jj.res.pv[jjj]=NA
								row.jj.res.stde[jjj]=NA
							}
							else
							{

								lm.jjj=lm(dep.vals~indep.vals, method=lmmethod)

								row.jj.res.cor.c[jjj]=round(cor(dep.vals,indep.vals,method=cmethod), cor.round)
								row.jj.res.lm.c[jjj]=round(as.numeric(lm.jjj$coefficients[2]), lmc.round)
								row.jj.res.lm.intcp[jjj]=round(as.numeric(lm.jjj$coefficients[1]), intcp.round)
								row.jj.res.r.sq[jjj]=round(as.numeric(summary(lm.jjj)$r.squared), rsq.round)
								row.jj.res.ad.r.sq[jjj]=round(as.numeric(summary(lm.jjj)$adj.r.squared), adjrsq.round)
								row.jj.med.resid[jjj]=round(median(summary(lm.jjj)$residuals,na.rm=T), resid.round)
								row.jj.mean.resid[jjj]=round(mean(summary(lm.jjj)$residuals,na.rm=T), resid.round)
								row.jj.sum.resid[jjj]=round(sum(summary(lm.jjj)$residuals,na.rm=T), resid.round)
								row.jj.med.abs.resid[jjj]=round(median(abs(summary(lm.jjj)$residuals),na.rm=T), resid.round)
								row.jj.mean.abs.resid[jjj]=round(mean(abs(summary(lm.jjj)$residuals),na.rm=T), resid.round)
								row.jj.sum.abs.resid[jjj]=round(sum(abs(summary(lm.jjj)$residuals),na.rm=T), resid.round)
								if (dim(summary(lm.jjj)$coefficients)[1]==1)
									{
										row.jj.res.pv[jjj]=NA
									}
									else
									{
										row.jj.res.pv[jjj]=round(summary(lm.jjj)$coefficients[2,4],pval.round)
								row.jj.res.stde[jjj]=round(as.numeric(summary(lm.jjj)$sigma), stde.round)
									}
							}
					}	# end of loop 3 jjj
					
				# now the vectors of resulting values for jj row are ready to be filled into jj row in resulting rasters
				
				results.all$brick.of.correlation.coefficient[[j]][jj,]=row.jj.res.cor.c
				results.all$brick.of.lm.coefficient[[j]][jj,]=row.jj.res.lm.c
				results.all$brick.of.lm.Intercept[[j]][jj,]=row.jj.res.lm.intcp
				results.all$brick.of.r.squared[[j]][jj,]=row.jj.res.r.sq
				results.all$brick.of.adjusted.r.squared[[j]][jj,]=row.jj.res.ad.r.sq
				results.all$brick.of.median.of.residuals[[j]][jj,]=row.jj.med.resid
				results.all$brick.of.mean.of.residuals[[j]][jj,]=row.jj.mean.resid
				results.all$brick.of.sum.of.residuals[[j]][jj,]=row.jj.sum.resid
				results.all$brick.of.median.of.abs.of.residuals[[j]][jj,]=row.jj.med.abs.resid
				results.all$brick.of.mean.of.abs.of.residuals[[j]][jj,]=row.jj.mean.abs.resid
				results.all$brick.of.sum.of.abs.of.residuals[[j]][jj,]=row.jj.sum.abs.resid
				results.all$brick.of.p.value[[j]][jj,]=row.jj.res.pv
				results.all$brick.of.standard.error[[j]][jj,]=row.jj.res.stde

			}	# end of loop 2 jj
		
	}	# end of loop 1 j
	


# --- removing original raster from brick of randomizations

results.all$brick.of.randomizations=results.all$brick.of.randomizations[[-c(it+1)]]


# ---- masking resulting raster bricks

if (ms)
	{
		results.all$brick.of.randomizations			=mask(results.all$brick.of.randomizations,rst)
		results.all$brick.of.correlation.coefficient	=mask(results.all$brick.of.correlation.coefficient,rst)
		results.all$brick.of.lm.coefficient			=mask(results.all$brick.of.lm.coefficient,rst)
		results.all$brick.of.lm.Intercept			=mask(results.all$brick.of.lm.Intercept,rst)
		results.all$brick.of.r.squared			=mask(results.all$brick.of.r.squared,rst)
		results.all$brick.of.adjusted.r.squared		=mask(results.all$brick.of.adjusted.r.squared,rst)
		results.all$brick.of.median.of.residuals		=mask(results.all$brick.of.median.of.residuals,rst)
		results.all$brick.of.mean.of.residuals		=mask(results.all$brick.of.mean.of.residuals,rst)
		results.all$brick.of.sum.of.residuals		=mask(results.all$brick.of.sum.of.residuals,rst)
		results.all$brick.of.median.of.abs.of.residuals	=mask(results.all$brick.of.median.of.abs.of.residuals,rst)
		results.all$brick.of.mean.of.abs.of.residuals	=mask(results.all$brick.of.mean.of.abs.of.residuals,rst)
		results.all$brick.of.sum.of.abs.of.residuals	=mask(results.all$brick.of.sum.of.abs.of.residuals,rst)
		results.all$brick.of.p.value				=mask(results.all$brick.of.p.value,rst)
		results.all$brick.of.standard.error			=mask(results.all$brick.of.standard.error,rst)
	}

# ----- end of the function

		ending.time=Sys.time()
		cat(paste("[ending function; at: ", format(Sys.time(), "%H:%M:%S"),"]",sep=""),"\n")
		cat(paste("[all analyses duration: ", duration.time(difftime(ending.time,starting.time,units="secs")),"]",sep=""),"\n")
		cat(paste("[analyses per iteration duration: ", duration.time(difftime(ending.time,starting.time,units="secs")/it),"]",sep=""),"\n")
		
# ---
		
results.all		

}


