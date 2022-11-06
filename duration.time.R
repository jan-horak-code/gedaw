
duration.time=function (ds){
	
# duration in seconds

if(ds<0){sign="-"}else{sign=""}
ch = floor(ds/3600) # computed hours
cm = floor((ds-3600*ch)/60)# computed minutes
cs = ceiling(ds-(3600*ch+60*cm)) # computed seconds

dt=paste(sign,ch,":",cm,":",cs,sep="")
dt

}

