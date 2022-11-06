
integraterplm = function (rplm,ids,rw,dw,gr,neg,idp=1) {

# comments:
	# produces batch integrations from rplm

# arguments:
	# rplm: input rplm object
	# ids: ids from rplm$overview
	# rw: randomness weight raster
	# dw: distance weight raster	
	# gr: grid for interpolation
	# neg: if predicted negative values should be left included, or changed to minimal positive value
	# idp: idp parameter for interpolation

result=list(inputs=list(rplm=deparse(substitute(rplm)),rw=deparse(substitute(rw)),dw=deparse(substitute(dw)),gr=deparse(substitute(gr))),
		overview=rplm$overview[ids,],
		interpolations=rep(list(NA),length(ids)),
		predictions=rep(list(NA),length(ids)),
		integrations=rep(list(NA),length(ids)))
	vars=result$overview$p.vars
		names(result$interpolations)=paste(vars,"_idw",sep="")
		names(result$predictions)=paste(vars,"_pred",sep="")
		names(result$integrations)=paste(vars,"_intgr",sep="")

for (i in 1:length(ids))
	{
		rplmi=rplm$overview[ids[i],]
		result$interpolations[[i]]=mask(raster(idw(get(as.character(rplmi$p.names))@data[,rplmi$p.cols]~1,get(as.character(rplmi$p.names)),gr,idp=idp)),get(as.character(rplmi$r.names)))
		result$predictions[[i]]=predict(get(as.character(rplmi$r.names)),rplm$linear.models[[ids[i]]][[2]])
			if(neg){}else{result$predictions[[i]][result$predictions[[i]][] <= 0]<-getmin(result$predictions[[i]]@data@values[which(result$predictions[[i]]@data@values>0)])}
		result$integrations[[i]]=((rw*result$predictions[[i]])+(dw*result$interpolations[[i]]))/(rw+dw)
	}

result

}



