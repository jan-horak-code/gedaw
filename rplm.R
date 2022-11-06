

rplm = function (r, p, cols, buf=1, func=median, ind="r") {

	# rplm is for Raster Points Linear Model
	
	# IMPORTANT: it is possible to input more objects into arguments "r" and "p"
		# in that case, use tibble::lst function, instead of base::list

	# arguments:
		# r input RasterLayer or list of them
		# p input SpatialPointsDataFrame or list of them
		# ind: which input will be independent, either "r", or "p", anything else leads to ind="r"
		# cols is for column number in SpatialPointsDataFrame@data
		# buf is buffer argument for extract() or vector of them
		# func is function argument for extract() or list of them
		
	# settings and working objects

	result=list(
		overview=data.frame(),
		linear.models=list())

	r.names=c()
	r.vars=c()
	p.names=c()
	p.cols=c()
	p.vars=c()
	buffers=c()
	measures=c()
	indeps=c()
	xnames=c()
	ynames=c()
	xcols=c()
	ycols=c()
	forms=c()
	rsqs=c()
	ccss=c()
	pvals=c()
	ns=c()
	ids=c()

	id.basis=0

	if(is.list(r)){}else{r=lst(r)}
	if(is.list(p)){}else{p=lst(p)}
	if(is.list(func)){}else{func=lst(func)}
	if(ind!="r" & ind!="p"){ind="r"}

	total=length(r)*length(p)*length(cols)*length(buf)*length(func)
		cat(paste("    total number of linm:  ", total,"; time:  ",Sys.time(),sep=""),"\n")

	# loop computing linear models

	for (i in 1:length(r)) # loop for rasters
		{
			for (j in 1:length(p)) # loop for points
				{
					for (k in cols) # loop for points columns
						{
							for (b in buf) # loop for buffers
								{
									for (f in 1:length(func))
										{
											id=id.basis+1
											extracted = extract(r[[i]],p[[j]],buffer=b, fun=func[[f]],na.rm=T)
											rdf = data.frame(extracted,p[[j]]@data[,k])
											colnames(rdf)=c(names(r[[i]]),colnames(p[[j]]@data)[k])

											if(ind=="p")
												{
													form=paste(names(r[[i]]),"~",colnames(p[[j]]@data)[k],sep="")
													indep="points"
													xname=colnames(p[[j]]@data)[k]
													yname=names(r[[i]])
													xcol=2
													ycol=1
												}
												else
												{
													form=paste(colnames(p[[j]]@data)[k],"~",names(r[[i]]),sep="")
													indep="raster"
													xname=names(r[[i]])
													yname=colnames(p[[j]]@data)[k]
													xcol=1
													ycol=2
												}

											# checking for input data for linear model

												# check for potential NAs and removing rows with them

													rdf[,3]=rdf[,2]*rdf[,1]	# check for possible NAs
														rdf=rdf[which(is.finite(rdf[,3])),1:2] # removing possible NAs
												
												# check for varibility in the data and performing linear model only in specified case

													if (length(unique(rdf[,1]))<2 | length(unique(rdf[,2]))<2) # linear model is not computed
														{
															linm=NA
															linm.extracted=list(
																betazero=NA,
																betaone=NA,
																rserror=NA,
																pvalue=NA,
																fstat=NA,
																numdf=NA,
																dendf=NA,
																arsq=NA,
																rsq=NA,
																ccs=NA,
																ccp=NA,
																cck=NA)
														}
														else # linear model is computed
														{
															linm=lm(form,rdf)
															linm.extracted=list(
																betazero=summary(linm)$coefficients[1,1],
																betaone=summary(linm)$coefficients[2,1],
																rserror=summary(linm)$sigma,
																pvalue=summary(linm)$coefficients[2,4],
																fstat=as.numeric(summary(linm)$fstatistic[1]),
																numdf=as.numeric(summary(linm)$fstatistic[2]),
																dendf=as.numeric(summary(linm)$fstatistic[3]),
																arsq=summary(linm)$adj.r.squared,
																rsq=summary(linm)$r.squared,
																ccs=cor(rdf[,1],rdf[,2],method="spearman"),
																ccp=cor(rdf[,1],rdf[,2],method="pearson"),
																cck=cor(rdf[,1],rdf[,2],method="kendall"))
														}

											r.names=append(r.names,names(r)[i])
											r.vars=append(r.vars,names(r[[i]])[1])
											p.names=append(p.names,names(p)[j])
											p.cols=append(p.cols, k)
											p.vars=append(p.vars, colnames(p[[j]]@data)[k])
											buffers=append(buffers,b)
											measures=append(measures,names(func)[f])
											indeps=append(indeps, indep)
											xnames=append(xnames,xname)
											ynames=append(ynames,yname)
											xcols=append(xcols,xcol)
											ycols=append(ycols,ycol)
											forms=append(forms,form)
											rsqs=append(rsqs,round(linm.extracted$rsq,3))
											ccss=append(ccss,round(linm.extracted$ccs,3))
											pvals=append(pvals,round(linm.extracted$pvalue,3))
											ns=append(ns,dim(rdf)[1])
											ids=append(ids,id)
											
											result$linear.models[[id]]=list(
												identifier=id,
												linm=linm,
												linm.extracted=linm.extracted,
												data.extracted=rdf)
											id.basis=id
											
											cat("    ",paste(id,r.names[id],p.names[id],k,p.vars[id],b,measures[id],Sys.time(),sep="; "),"\n")
										}
								}
						}
				}
		}

	result$overview=data.frame(
		r.names=r.names,
		r.vars=r.vars,
		p.names=p.names,
		p.cols=p.cols,
		p.vars=p.vars,
		buffers=buffers,
		measures=measures,
		indeps=indeps,
		xnames=xnames,
		ynames=ynames,
		xcols=xcols,
		ycols=ycols,
		forms=forms,
		rsqs=rsqs,
		ccss=ccss,
		pvals=pvals,
		ns=ns,
		ids=ids)

	cat(paste("    ending:  ",Sys.time(),sep=""),"\n")

	result
}

# -----------------------

plotrplm = function (rplm,id=1) {


	plot(rplm$linear.models[[id]]$data.extracted[,rplm$overview$ycols[id]]~rplm$linear.models[[id]]$data.extracted[,rplm$overview$xcols[id]],xlab=rplm$overview$xnames[id],ylab=rplm$overview$ynames[id])
		mtext(paste("adj. r sq.: ", round(rplm$linear.models[[id]]$linm.extracted$arsq,2), ";  m. r sq.: ", round(rplm$linear.models[[id]]$linm.extracted$rsq,2), ";  p: ", round(rplm$linear.models[[id]]$linm.extracted$pvalue,3), sep=""), 3, line=2.5)
		mtext(paste("F statistic: ", round(rplm$linear.models[[id]]$linm.extracted$fstat,2), " on ", round(rplm$linear.models[[id]]$linm.extracted$numdf,2), " and ", round(rplm$linear.models[[id]]$linm.extracted$dendf,2), " DF", sep=""), 3, line=1.3)
		if (rplm$linear.models[[id]]$linm.extracted$betaone<0)
			{
				mtext(paste("y = ", round(rplm$linear.models[[id]]$linm.extracted$betazero,2), "  ", round(rplm$linear.models[[id]]$linm.extracted$betaone,2), "x + e,  where e ~ N (0, ", round(rplm$linear.models[[id]]$linm.extracted$rserror,2), "^2)", sep=""), 3, line=0.2)
			} 
			else 
			{
				mtext(paste("y = ", round(rplm$linear.models[[id]]$linm.extracted$betazero,2), " + ", round(rplm$linear.models[[id]]$linm.extracted$betaone,2), "x + e,  where e ~ N (0, ", round(rplm$linear.models[[id]]$linm.extracted$rserror,2), "^2)", sep=""), 3, line=0.2)
			}
		abline(rplm$linear.models[[id]]$linm)
	
}

# --------------------------------

rplm.comp = function (r, p, cols, buf=1, func=median) {

if(is.list(r)){}else{r=lst(r)}
if(is.list(p)){}else{p=lst(p)}
if(is.list(func)){}else{func=lst(func)}

result=list(length(r),length(p),length(cols),length(buf),length(func),length(r)*length(p)*length(cols)*length(buf)*length(func))
result

}


