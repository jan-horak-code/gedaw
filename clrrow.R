
clrrow = function (x, bs=10)

# arguments:
	# x is for input object with rows and columns, ideally matrix or data.frame
	# bs is base argument for log function

{ 
	x.clr=x
	for (i in c(1:dim(x.clr)[1]))
		{
			x.clr[i,]=log(x[i,]/gmean(x[i,]),base=bs)
		}
	x.clr
}
