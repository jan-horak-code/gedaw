
plothist = function (obj, nc=20, var=NA, returnresult=F, colbars="black", bord=colbars, xaxis=T, yaxis=T, onlybars=F, labelit=T, mtx=F, mtxt="", mtxc=1, mtxs=3, mtxl=0.6) {

# comments:
	# just plots a histogram, based on function comphist()

# arguments:
	# obj: input Object; the function presumes, that it is either a numeric vector, or a product of comphist()


if (is.numeric(obj))
	{
		res=comphist(obj,nc=nc)	# in this case it computes comphist() result
		if(is.na(var)){name=deparse(substitute(obj))}else{name=var}
	}
	else 
	{
		res=obj			# here it assumes that input is comphist() result
		nc=length(obj$counts)	# in this case, it gets number of classes from the input comphist() result
		returnresult=F		# the result is already existing, so it is not returned again
		name=res$name
	}

x1=res$lows
x2=res$highs
y0=rep(0,nc)
y1=res$counts.for.plothist

# sometimes, there is a need to plot only the bars

if (onlybars)
	{
		rect(x1,y0,x2,y1, border=bord, col=colbars)
	}
	else
	{

# this else plots it all

		if (labelit)
			{
				xlabel=paste("values of ", name, sep="")
				ylabel="count"
				mainlabel=NA
			}
			else
			{
				xlabel=NA
				ylabel=NA
				mainlabel=NA
			}

		plot(res$range.of.histogram, c(0, getmax(res$counts)), type="n", axes=F, xlab=xlabel, ylab=ylabel, main=mainlabel)

		if (labelit)
		 	{
				mtext(side=3, line=2, text=paste("histogram of ", name, sep=""))
				mtext(side=3, line=1, text=paste("count: ", sum(res$counts, na.rm=T), ";  max: ", getmax(res$counts), ";  bars: ", length(res$counts), sep=""))
			}
			else
			{
			}

		if(xaxis){axis(1, labels=pretty(c(res$lows, res$highs)), at=pretty(c(res$lows, res$highs)))}
		if(yaxis){axis(2, labels=pretty(res$counts), at=pretty(res$counts))}
		if(mtx)
			{
				mtext(mtxs, line=mtxl, text=mtxt, cex=mtxc)
			}
		rect(x1,y0,x2,y1, border=bord, col=colbars)

	}


if (returnresult)
	{
		res
	}

}

