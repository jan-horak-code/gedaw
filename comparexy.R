

comparexy = function (n="", s, o, easy=T) 
{

# comments:
	# function compares multiple data.frames and checks, if the columns for coordinates are identical in all of them
	# supposition, that all the data.frames have same name and differ only by a number (e.g. df1, df2, df3...)

# arguments:
	# n stands for name - the part of the name, the string of characters which is the same for all data.frames
	# s stands for start, the first number to which all others are compared
	# o stands for other numbers, which are to be compared
	# easy returns simple variant of the result by default set to easy=T
		# if easy==F, the function returns the data.frame with all the data.frames and specification, if they have identical, or different coordinates

	source=get(paste(n, s, sep=""))
	
	res=data.frame(i=c(s), x=c(TRUE), y=c(TRUE))
	
	for (i in o)
	{
		lengthi=dim(res)[1]+1
		tocompare=get(paste(n, i, sep=""))
		res[lengthi,1]=i
		res[lengthi,2]=identical(source[,1], tocompare[,1])
		res[lengthi,3]=identical(source[,2], tocompare[,2])
		

	}

	if(easy)
	{
		res=res[which(res[,2]==FALSE | res[,3]== FALSE),]
		if (dim(res)[1]==0)
			{
				cat("\n", "      The coordinates are identical!", "\n")
			}
			else
			{
				res
			}
	}
	else
	{
		res
	}
	
	



}