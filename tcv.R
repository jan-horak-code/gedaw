
tcv = function(r,cex=1){ 

# tcv as Text Cell Values

	m=cbind(xyFromCell(r,1:ncell(r)),round(getValues(r),1))

	text(x=m[,1],y=m[,2],labels=m[,3],adj=0.5,cex=cex,font=1)

	}

