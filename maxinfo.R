
maxinfo= function (vec, h=10) {

# comments:
	# computes h number of maximal values and returns dataframe with those values and their positions in a vector
		# all cases of these values are returned, as duplicates are distinguished by their position...
	# ignores NAs

# arguments:
	# vec: Vector, i.e. input numeric vector
	# h: Howmany, i.e. how many maximal values shoud be returned

# stop conditions

	stopit=F
	if(is.null(h)){stopit=T}
	if(length(vec)==0){stopit=T}
	if(length(which(is.finite(vec)))==0){stopit=T}
	if(length(which(is.na(vec)))==length(vec)){stopit=T}

# returning NA, if there are no data to be processed

	if(stopit)
		{
			NA
		}
		else

# all the function it this else
	
		{
			count.of.uniques=length(unique(vec[which(is.finite(vec))]))
			if (count.of.uniques>=h)
				{
					sequence=c(1:h)
				}
				else
				{
					sequence=c(1:count.of.uniques)
				}

			value = c()
			positions = c(length(vec)+1)
			orders = c()
			sequen = c(1:length(sequence))

			for (i in sequen)
				{
					orderi = sequen[i]
					maxi = max(vec[-positions], na.rm=T)
					posi = which(vec==maxi)
					counti = length(posi)
	
					orders = append(orders, rep(sequence[sequen[i]], length.out=counti))
					value = append(value, rep(maxi, length.out=counti))
					positions = append(positions, posi)
				}

			positions = positions[-1]

			resdf = as.data.frame(cbind("orders" = orders, "value" = value, "positions" = positions))

			resdf
		}
}


