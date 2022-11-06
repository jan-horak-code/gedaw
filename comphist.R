
comphist = function (vec, nc=20, var=NA) {

# comments:
	# computes histogram, computes more details than standard r hist(), but it uses just easy classification, no advanced methods

# arguments:
	# vec: Vector, i.e. input numeric vector
	# nc: Number of Classes, i.e. desired number of histogram classes

if(is.na(var)){name=deparse(substitute(vec))}else{name=var}

	pnas=which(is.na(vec))
	pfin=which(is.finite(vec))
	nnas=length(pnas)
	nfin=length(pfin)
	lv=length(vec)
	mi=getmin(vec)
	ma=getmax(vec)

if (nnas==lv) # just in case the vec is only NAs
	{
		reslist=list(range.of.data=NA, range.of.histogram=NA, name=name, step=NA, classes=NA, breaks=NA, lows=NA, highs=NA, counts=0, classes.proportions=NA, appointed.proportions.of.classes=NA, reclass=NA)
	}
	else
	{

		# this part is computing histograms in classes based on equal steps in range		

			step=(ma-mi)/nc

			lasts=mi+step*c(1:nc)

			lasts=lasts[which(lasts < ma)]

			chops=lasts # this step does not need to be here, it is just to be same structure of script as a part computing equal count below

			breaks=c(mi, chops, ma)

			nc=length(breaks)-1

			classes=1:nc

			lows=breaks[1:nc]
			highs=breaks[2:length(breaks)]

			reclass=rep(NA, length(vec))
			counts=rep(0, nc)
			classes=c(1:nc)
			
			reclass[which(vec<=highs[1])]=1
	
			for (ii in 2:nc)
				{
					reclass[is.na(reclass) & vec<=highs[ii]]=ii
				}

			for (iii in classes)
				{
					counts[iii]=length(which(reclass==iii))
				}
	
			counts.for.plothist=counts
			counts.for.plothist[which(counts.for.plothist==0)]=NA
			classes.proportions=counts/sum(counts)
			appointed.proportions.of.classes=classes.proportions[reclass]
	
		# this part is computing histograms in classes based on equal count of cases in a class
			# not necessarily leads to equal count - it also depends number of non-unique values in a vector...		
			# useful in specific cases like plotting raster with leptokurtic histogram
			# names of objects created here start with "ec." - i.e. Equal Count

			ec.vs=sort(vec)			
			
			ec.step=ceiling(nfin/nc)
			
			ec.lasts=ec.step*c(1:nc)

			ec.lasts=ec.lasts[which(ec.lasts < nfin & ec.lasts+1 < nfin)]

			ec.chops=c(ec.vs[ec.lasts] + ec.vs[ec.lasts+1])/2

			ec.breaks=c(mi, ec.chops, ma)			

			ec.nc=length(ec.breaks)-1

			ec.classes=1:ec.nc

			ec.lows=ec.breaks[1:nc]

			ec.highs=ec.breaks[2:length(ec.breaks)]

			ec.reclass=rep(NA,lv)

			ec.counts=rep(0,ec.nc)

			ec.reclass[which(is.finite(vec) & vec <= ec.highs[1])]=1

			reclass[which(vec<=highs[1])]=1
	
			for (ii in 2:ec.nc)
				{
					ec.reclass[is.na(ec.reclass) & is.finite(vec) & vec <= ec.highs[ii]]=ii
				}

			for (iii in ec.classes)
				{
					ec.counts[iii]=length(which(ec.reclass==iii))
				}

		# creating result list
	
			reslist=list(range.of.data=range(vec), range.of.histogram=c(mi, ma), name=name, step=step, classes=classes, breaks=breaks, lows=lows, highs=highs, counts=counts, counts.for.plothist=counts.for.plothist, classes.proportions=classes.proportions, appointed.proportions.of.classes=appointed.proportions.of.classes, reclass=reclass, length.of.vector=lv, count.of.nas=nnas, count.of.finites=nfin,positions.of.nas=pnas, positions.of.finites=pfin,
				ec.step=ec.step, ec.breaks=ec.breaks, ec.lows=ec.lows, ec.highs=ec.highs, ec.nc=ec.nc, ec.classes=ec.classes,ec.reclass=ec.reclass,ec.counts=ec.counts)


	}

	reslist

}

# ---------------------------

appoint.comphist.classes = function(vec,hist){

# comments:
	# this just appoints classes and proportions of specified histogram to a numeric vector
		# what class and proportion would a number of vector had, if it would be in the histogram
	# it is needed for some randomization techniques in randomize()

# arguments:
	# vec: numeric Vector
	# hist: object produced by comphist() function

classes=rep(NA,length(vec))
proportions=rep(NA,length(vec))

for (i in 1:length(hist$highs))
	{
		classes[is.na(classes) & vec<=hist$highs[i]]=i
	}

classes[is.na(classes) & is.finite(vec)]=getmax(hist$classes)
proportions=hist$classes.proportions[classes]

result=data.frame(classes=classes,proportions=proportions)

result

}




