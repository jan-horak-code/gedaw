
compareclrcol = function (a,b,ce=1,co="red",vh=0.95,rh=0.85,main=NULL,main.ce=3,main.line=0.75){

# comment:
	# function compares the input and output of the clrrow function and returns plots of variables comapred and vector of R^2

# arguments:
	# a is input object
	# b is input object
	# ce is cex parameter for text label of variable and R^2
	# co, is colour of text label of variable and R^2
	# vh is a ratio of the plot height for variable label to be placed
	# rh is a ratio of the plot height for R^2 label to be placed
	# main is for text label for the title of the plot
	# main.ce is a cex paramater for the title of the plot
	# main.line is a line paramater for the title of the plot

result=c()

wn=ceiling(sqrt(dim(a)[2]))
par(mfrow=c(wn,wn))
if(is.null(main)){par(oma=c(0,0,0,0))} else{par(oma=c(0,0,4,0))}
par(mar=c(0,0,0,0))
colna=colnames(a)

for(i in 1:dim(a)[2])
	{
		result=append(result,summary(lm(as.numeric(a[,i])~as.numeric(b[,i])))$r.squared)
		plot(as.numeric(b[,i]),as.numeric(a[,i]),pch=16,cex=0.2,axes=F,xlab=NA,ylab=NA)
		text(
			getmin(as.numeric(b[,i]))+0.05*getrange(as.numeric(b[,i])),
			getmin(as.numeric(a[,i]))+vh*getrange(as.numeric(a[,i])),
			colna[i],col=co,adj=0,cex=ce)
		text(
			getmin(as.numeric(b[,i]))+0.05*getrange(as.numeric(b[,i])),
			getmin(as.numeric(a[,i]))+rh*getrange(as.numeric(a[,i])),
			round(result[i],2),col=co,adj=0,cex=ce)
		box()
		if(is.null(main))
			{}
			else
			{mtext(side=3,text=main,cex=main.ce,line=main.line,outer=T)}
	}

result

}

