
plotintegraterplm = function (intgr,file="intgr.png",d=400,col=colorRampPalette(c("violet", "blue", "green", "yellow", "orange", "red"))(n = 30),vcex=8,rcex=5) {

# comments:
	# just produces image of plots of integraterplm function

# arguments:
	# intgr: input object from integraterplm()
	# file: file name
	# d: dimension: points for one plot, will be multiplied by number of plots to be plotted
	# col: colour palette for raster

n=dim(intgr$overview)[1]
png(file,height=d*n,width=d*9)
par(mfrow=c(n,9))
for (i in 1:n)
	{
		plot(1:5,1:5,type="n",axes=F,xlab=NA,ylab=NA)
			text(3,4.5,labels=intgr$overview$p.vars[i],cex=vcex,adj=0.5)
			text(3,1.2,labels=paste("r: ",intgr$overview$r.names[i],sep=""),cex=rcex,adj=0.5)
			text(3,2.0,labels=paste("p: ",intgr$overview$p.names[i],sep=""),cex=rcex,adj=0.5)
			text(3,2.8,labels=paste("lm: ",intgr$overview$ids[i],sep=""),cex=rcex,adj=0.5)
		plot(intgr[[3]][[i]],col=col)
			mtext(side=2,text=intgr$overview$p.vars[i])		
			mtext(side=3,text="orig")
		plothist(intgr[[3]][[i]]@data@values)
		plotrplm(get(intgr$inputs$rplm),intgr$overview$ids[i])
		plot(intgr[[4]][[i]],col=col)
			mtext(side=3,text="pred")
		plot(intgr[[5]][[i]],col=col)
			mtext(side=3,text="intgr")
		plothist(intgr[[5]][[i]]@data@values)
		plotrastql(intgr[[5]][[i]],col=col)
			mtext(side=3,text="intgr ql")
		plotrastec(intgr[[5]][[i]],col=col)
			mtext(side=3,text="intgr ec")
	}
dev.off()
graphics.off()
}











