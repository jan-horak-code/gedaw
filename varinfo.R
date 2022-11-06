

varinfo = function (df, w=NULL) {

# arguments:
	# df is for data.frame object
	# w is for which rows of resulting dataframe (ie columns of input data.frame) will be shown

	positions = c(1:length(df))

	modi = c()
		for (i in positions)
			{
				modi[i] = mode(df[,i])

			}

	classi = c()
		for (i in positions)
			{
				classi[i] = class(df[,i])

			}

	variables = as.data.frame(cbind(positions = positions, variables = colnames(df), class=classi, mode=modi))

	if (is.null(w)) {} else {variables=variables[w,]}

	variables

}

