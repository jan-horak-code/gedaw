

rtrl = function (rst,lo=NA,hi=NA,buf=1,limit=3,bufadd=1){

# rst is raster
# lo is lower: lower threshold, values lower than that  will be recomputed by mean of the extracted buffer cells
# hi is higher; higher threshold, values greater than that will be recomputed by mean of the extracted buffer cells
# limit is a minimal number of finite values from which the mean will be computed
# buf is buffer around the cell to be recomputed, from which the values for recomputation will be extracted
	# buffer in the units of a raster
# bufadd is number to add to buf when searching for more values, if original buffer does not contain limit of values

cells=c(NA,NA,NA)

if (is.na(lo) & is.na(hi)) {stop(cat("\n","    arguments `lo`  and / or `hi` have to be specified by a number","\n"))}

if(is.finite(lo) & is.na(hi)) {cells=xyFromCell(rst,which(rst[]<lo));hi=getmax(rst@data@values)+1}

if(is.na(lo) & is.finite(hi)) {cells=xyFromCell(rst,which(rst[]>hi));lo=getmin(rst@data@values)-1}

if(is.finite(lo) & is.finite(hi)) {cells=xyFromCell(rst,which(rst[]<lo | rst[]>hi))}

cat("\n",paste("cells over threshold:  ", dim(cells)[1], "  of  ", length(rst@data@values), sep=""),"\n")

for (i in 1:dim(cells)[1])
	{
		buffer.values=as.numeric(extract(rst,matrix(cells[i,],1,2),buffer=buf)[[1]])
		buffer.values=buffer.values[which(is.finite(buffer.values) & buffer.values<=hi & buffer.values>=lo)]
		if (length(buffer.values)>=limit)
			{}
			else
			{
				repeat {
						buf=buf+bufadd
						buffer.values=as.numeric(extract(rst,matrix(cells[i,],1,2),buffer=buf)[[1]])
						buffer.values=buffer.values[which(is.finite(buffer.values) & buffer.values<=hi & buffer.values>=lo)]
						if (length(buffer.values)>=limit)
							{break}
					}
			}

		rst[cellFromXY(rst,cells[i,])]=mean(buffer.values)

	}

rst

}

