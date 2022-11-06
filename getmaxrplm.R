getmaxrplm = function (ro, f=5, v=14, m=1, p=NULL) {

# comments:
	# this returns data.frame rows with m maximal values of vector v for every level of factor f

# arguments:
	# ro is input rplm overview data.frame
	# f is factor to be used, default is 5 which is p.vars
	# v is for vector to be used, default is 14 which is rsqs
	# m is how many maximal values of v will be returned
	# p is for p-value, if specified, linear models with value higher than p argument will be omitted

rw=ro[order(-ro[,v]),] # rw stands for result working

if(is.null(p)){}else{rw=rw[which(rw$pvals<=p),]}

r=rw[1,] # r stands for result

vars=unique(rw[,f])

for (i in 1:length(vars))
	{
		r=rbind(r,rw[which(rw[,f]==vars[i]),][1:m,])
	}

r=r[-1,]

r

}