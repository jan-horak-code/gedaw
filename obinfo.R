
obinfo = function (fun=F, info=F) {

	
	dat=ls(envir = .GlobalEnv)
	count=length(dat)
	positions=c(1:count)
	res=data.frame(name=rep(NA, count), class=rep(NA, count), position=positions, size=rep(NA, count))
	
	for(i in positions)
	{
		res[i,3]=i
		res[i,1]=dat[i]
		res[i,2]=paste(class(get(dat[i])), collapse=";  ") # beacause some objects have more classes (e.g. variogram models)
		res[i,4]=object.size(get(dat[i]))
	}
	res=res[order(res[,2], res[,1]),]
	rownames(res)=positions
	if (fun)
		{} else	{
					(res=res[-which(res[,2]=="function"),])
					rownames(res)=1:dim(res)[1]
				}
	res

	if (info){"to erase objects according to the obinfo(), write: rm(list=c(obinfo()[x,1])); where x is to specify rows"} else {res}


}


