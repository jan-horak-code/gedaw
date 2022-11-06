
randomize = function (vec,t=NULL,na=F,nc=20) {

# comments:
	# this function needs input vector - specifications of generating random values comes from the input vector
		# either it is just repositioning of the vector values
		# or the normal distributed values  using rnorm - the arguments are inferred from the input vector
		# or maxima and minima of desired vector... etc...

# arguments:
	# vec is input vector
	# t is number of type of randomization
		# 1: permutation of positions
		# 2: sampling from input range without weighting, produces also decimal values (multiplies original range by 10, then later, the result is divided by 10)
		# 3: like type 2 with weighting, weights obtained from histogram of the input
		# 4: like type 3, but the probabilities change randomly
	# na: how NAs should be treated (why we would be interested in this? it can be important in some cases (raster values)
		# na=F means that NAs will be excluded from randomization, ie what was NA in the input, will be NA in the output, randomization will be done only on finite values and positions
		# na=T means that NAs will be included - randomization will be performed on all vector; finite values and NAs can be found in whatever position in output vector
	# nc: Number of Classes for comphist() where it is needed for some types


# working objects

	inp = which(is.na(vec))		# Input NAs Positions
	ifp = which(is.finite(vec))	# Input Finite Positions
	ifv = vec[ifp]			# Input Finite Values
	rv = rep(NA,length(vec))	# Randomized Vector; it will be used for resulting vector
	mf = 1000				# Multiplicative Factor

	if(is.null(t)){t=sample(1:6,1)}

# randomization approaches

	# TYPE 1: permutation of positions
		
		# comments:
			# this does not generate new values, this just "shuffles" the values of the input vector
			# the resulting vector is the same as the input from statistical perspective: histogram, mean, median, min, max...
		
		if (t==1)
			{
				if(na)
					{
						rv = sample(vec)
					}
					else
					{
						rv[ifp] = sample(ifv)
					}
			}
		
		
	# TYPE 2: sampling from the input range with no weighting of probability
		
		# comments:
			# this does generate new values
			# the resulting vector is in the same range of input values
			# the resulting vector does not regard the input histogram, the resulting histogram will tend to have nod difference of the bars height
		
		if (t==2)
			{
				if(na)
					{
						rv = sample(c((mf*getmin(vec)):(mf*getmax(vec))),length(vec),replace=T)/mf
					}
					else
					{
						rv[ifp] = sample(c((mf*getmin(ifv)):(mf*getmax(ifv))),length(ifv),replace=T)/mf
					}
			}
		


	# Type 3, 4, 5 and 6: sampling from the input range (input histogram classes) weighting of probability based on histogram
		
		# comments:
			# all types have same algorithm, they are different only by specifying the weights
				# the weights basis is a weight (proportion) which would the value have if it would be a part of the original dataset
					# it is type 3; the types 4 and 5 then change this proportion randomly, they differ in way how to find that randomness
				# type 3: the weights are proportional to the counts in histogram classes
				# type 4: the weights are changing, the rate of change between -100% and +100%, rate of change sampled randomly, all changes rates have same probability to be chosen
				# type 5: the weights are changing, same as previous, but the rates of change have normal distribution (so the changes around zero are MORE likely than far from zero)
				# type 6: the weights are changing, same as previous, but the rates of change are inverted to normal distribution (so the changes around zero are LESS likely than far from zero)

		if (t==3 | t==4 | t==5 | t==6)
			
			{
				if(na)
					{
						histo=comphist(vec,nc=nc)
						vtr = c((mf*getmin(vec)):(mf*getmax(vec)))/mf # Values To be Randomly chosen
						cpht3 = appoint.comphist.classes(vtr,histo)$proportions # Classes Proportions by Histogram for Type 3
						cpht4 = appoint.comphist.classes(vtr,histo)$proportions # Classes Proportions by Histogram for Type 4
							cpht4.changes.pool = c((-100*mf):(100*mf))/mf
							cpht4.changes = sample(cpht4.changes.pool,length(vtr),replace=T) # vector of changes to be applied on proportions; sample() function has no "prob" argument specified, so every number (rate of change) is same likely to be chosen
							cpht4 = abs(cpht4 + (cpht4*cpht4.changes)) # application of changes
						cpht5 = appoint.comphist.classes(vtr,histo)$proportions # Classes Proportions by Histogram for Type 5
							cpht5.changes.probabilities = -1*abs(sort(rnorm(length(cpht4.changes.pool),0,0.3))) # normally distributed weights around zero with approx range between -1 and 1
							cpht5.changes.probabilities = abs(getmin(cpht5.changes.probabilities))+cpht5.changes.probabilities
							cpht5.changes = sample(cpht4.changes.pool,length(vtr),prob=cpht5.changes.probabilities, replace=T) # vector of changes to be applied on proportions; sample() function has "prob" argument specified, it is designed to be normally distributed
							cpht5 = abs(cpht5 + (cpht5*cpht5.changes)) # application of changes
						cpht6 = appoint.comphist.classes(vtr,histo)$proportions # Classes Proportions by Histogram for Type 5
							cpht6.changes.probabilities = abs(sort(rnorm(length(cpht4.changes.pool),0,0.3))) # normally distributed weights around zero with approx range between -1 and 1
							cpht6.changes = sample(cpht4.changes.pool,length(vtr),prob=cpht6.changes.probabilities, replace=T) # vector of changes to be applied on proportions; sample() function has "prob" argument specified, it is designed to be inverted to normal distribution
							cpht6 = abs(cpht6 + (cpht6*cpht6.changes)) # application of changes

						if(t==3){cph = cpht3}
						if(t==4){cph = cpht4}
						if(t==5){cph = cpht5}
						if(t==6){cph = cpht6}
						
						rv = sample(vtr,length(vec),prob=cph,replace=T)
					}
					else
					{
						histo=comphist(ifv,nc=nc)
						vtr = c((mf*getmin(ifv)):(mf*getmax(ifv)))/mf # Values To be Randomly chosen
						cpht3 = appoint.comphist.classes(vtr,histo)$proportions # Classes Proportions by Histogram for Type 3
						cpht4 = appoint.comphist.classes(vtr,histo)$proportions # Classes Proportions by Histogram for Type 4
							cpht4.changes.pool = c((-100*mf):(100*mf))/mf
							cpht4.changes = sample(cpht4.changes.pool,length(vtr),replace=T) # vector of changes to be applied on proportions; sample() function has no "prob" argument specified, so every number (rate of change) is same likely to be chosen
							cpht4 = abs(cpht4 + (cpht4*cpht4.changes)) # application of changes
						cpht5 = appoint.comphist.classes(vtr,histo)$proportions # Classes Proportions by Histogram for Type 5
							cpht5.changes.probabilities = -1*abs(sort(rnorm(length(cpht4.changes.pool),0,0.3))) # normally distributed weights around zero with approx range between -1 and 1
							cpht5.changes.probabilities = abs(getmin(cpht5.changes.probabilities))+cpht5.changes.probabilities
							cpht5.changes = sample(cpht4.changes.pool,length(vtr),prob=cpht5.changes.probabilities, replace=T) # vector of changes to be applied on proportions; sample() function has "prob" argument specified, it is designed to be normally distributed
							cpht5 = abs(cpht5 + (cpht5*cpht5.changes)) # application of changes
						cpht6 = appoint.comphist.classes(vtr,histo)$proportions # Classes Proportions by Histogram for Type 5
							cpht6.changes.probabilities = abs(sort(rnorm(length(cpht4.changes.pool),0,0.3))) # normally distributed weights around zero with approx range between -1 and 1
							cpht6.changes = sample(cpht4.changes.pool,length(vtr),prob=cpht6.changes.probabilities, replace=T) # vector of changes to be applied on proportions; sample() function has "prob" argument specified, it is designed to be inverted to normal distribution
							cpht6 = abs(cpht6 + (cpht6*cpht6.changes)) # application of changes

						if(t==3){cph = cpht3}
						if(t==4){cph = cpht4}
						if(t==5){cph = cpht5}
						if(t==6){cph = cpht6}
						
						rv[ifp] = sample(vtr,length(ifv),prob=cph,replace=T)
					}

			}

# returning resulting Randomized Vector
	
	rv	
}

