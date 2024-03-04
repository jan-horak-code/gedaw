

gridpoints = function (xs, ys,  gn, stepx=1, stepy=stepx, howx=100, howy=howx, way="x", ev=5, rp=NULL, cp=NULL, sp=NULL, cf=4, pf=500, df=10, st=T, write=T, comments="no comments",dire="grids") 
{

# comments:
	# the function presumes that unit of a step is 1 meter
	# random points are generated with possibility of up to 3 decimal places (i.e. mm precision in a presumption)
	# to nicely numbered grid, set howx to 9, or 99, or 999 etc.
	# additional grids (random points, cluster points and specified points) are produced within the limit of basic grid
# arguments:
	# xs and xy - starting points coordinates
	# gn is for grid number, has to be specified - basically used mainly for the distinguishing of various grids in situation where many of them has to be stored
	# stepx, stepy - what is a dimension of a step between two adjacent points
	# howx, howy - how many steps in each direction
	# way - if the numbering should follow the x or y direction first - row-way or column-way
	# ev is for subset grid - crosspoints for every nth line and it is related to howx and howy - so not to distance from origin (that is influenced only by steps)!
	# rp is random points - vector of one or more values, each producing separate grid of randomly placed points
		# values can be specified as NA, in such case, a default value will be applied (number of basis grid points (arguments hwox * howy) divided by 100)
		# e.g. rp=c(200,100) will produce grid with 200 points and another grid with 100 points
	# cp is clustered points - vector of three (or multiplication of three) values representing in this order: number of clusters, number of points in each cluster, maximal distance of the cluster points from the cluster center
		# more values in vectors will produce more grids 
		# the cluster window is a +- maximal distance from a center in both dimensions 
		# the centers of clusters are placed randomly
		# the points in within cluster window are placed randomly (limited by the maximal distance in both directions)
		# value can be specified as NA, in such case, arguments cf, pf and df wil be applied
		# if maximal distance would be equal or higher than half of the shorter dimension of basic grid, it will be changed to a shorter dimension divided by 10 
		# e.g. cp=c(50,5,1) will produce grid with 50 clusters, each with 5 points distanced max 1 unit from the cluster center, the whole grid will have 250 points
		# e.g. cp=c(30,5,2,20,2,5,25,10,10) will produce 3 grids with 150, 40 and 250 points in clusters
	# sp is specified clustered points - similar as clustered points, but here coordinates of the cluster centers have to be specified
		# sp is therefpre vector of 4 values (or multiplication of 4):
			# x coordinates, y coordinates, number of points in cluster, maximal distances
		# NA values for number of points and maximal distance will be replaced by values same way as for cluster points
		# every cluster has its own grid
	# cf is for cluster factor
		# number of clusters in cp grid, default = 4
	# pf is for point factor
		# default = 500
		# number of points in every cluster in cp grid is computed as: number of basis grid points (arguments howx * howy) divided by pf for number of points in cluster
	# df is for distance factor
		# default = 10
		# maximal distance for cluster is computed as: shorter dimension of basic grid divided by df for maximal distance
	# st is for showing time and duration of the analysis
	# write - if the outputs should be svaed as txt and csv files
	# comments - any comments which user thinks are good to be stored
	# dire - name of the directory, into which the grids should be stored if write=T, by default it is directory called grids in working directory

# 0 - start

grids.starting.time=Sys.time()

if (st)
	{
		cat(paste("[starting function; on: ", format(grids.starting.time, "20%y-%m-%d"), ";  at: ", format(grids.starting.time, "%H:%M:%S"),"]",sep=""),"\n")
	}

# 1 - making namenum

namenum=10^c(1+(ceiling(log10(max(c(howx,howy))))))

# 2 - making basic grid

xadded=0:howx
yadded=0:howy
xdist=stepx*xadded
ydist=stepy*yadded
xcoords=c(xs+xdist)
ycoords=c(ys+ydist)


if (way=="y")
  {
    xcoords.expanded=rep(xcoords, times=howy+1)
    ycoords.expanded=rep(ycoords, each=howx+1)
    xdist.expanded=rep(xdist, times=howy+1)
    ydist.expanded=rep(ydist, each=howx+1)
  }
  else
  {
    xcoords.expanded=rep(xcoords, each=howy+1)
    ycoords.expanded=rep(ycoords, times=howx+1)
    xdist.expanded=rep(xdist, each=howy+1)
    ydist.expanded=rep(ydist, times=howx+1)
  }

grid.basis = data.frame(
		id=1:length(xcoords.expanded),
		x=xcoords.expanded,
		y=ycoords.expanded,
		xdist=xdist.expanded,
		ydist=ydist.expanded,
		gridnumber=rep(gn,length(xcoords.expanded)),
		subgridnumber=rep(2,length(xcoords.expanded)),
		gridtype=rep("basis",length(xcoords.expanded)))

# 3 - creating grid.all, which will store all of created points

grid.all=grid.basis

# 4 - subsetting border lines 

grid.borders = grid.basis[which(grid.basis[,4]==min(grid.basis[,4]) | grid.basis[,4]==max(grid.basis[,4]) | grid.basis[,5]==min(grid.basis[,5]) | grid.basis[,5]==max(grid.basis[,5])),]
grid.borders[,7]=rep(3,length(grid.borders[,7]))
grid.borders[,8]=rep("borders",length(grid.borders[,8]))


# 5 - subsetting corner points

grid.corners = grid.basis[which(
				(grid.basis[,4]==min(grid.basis[,4]) & grid.basis[,5]==min(grid.basis[,5]))
				|
				(grid.basis[,4]==min(grid.basis[,4]) & grid.basis[,5]==max(grid.basis[,5]))
				|
				(grid.basis[,4]==max(grid.basis[,4]) & grid.basis[,5]==min(grid.basis[,5]))
				|
				(grid.basis[,4]==max(grid.basis[,4]) & grid.basis[,5]==max(grid.basis[,5]))
			),]
grid.corners[,7]=rep(4,length(grid.corners[,7]))
grid.corners[,8]=rep("corners",length(grid.corners[,8]))


# 6 - creating results list

results=list(grid.all=grid.all, grid.basis=grid.basis, grid.borders=grid.borders, grid.corners=grid.corners)

# 7 - subsetting crosspoints of lines and adding it to the results list	# automatically adds also zeroth line

for (i in 1:length(ev))
	{
		in.howx=floor(howx/ev[i])	# as for "how many times it is in howx"
		in.howy=floor(howy/ev[i])	# as for "how many times it is in howy"
		xdisti=stepx*c(0, ev[i]*c(1:in.howx))
		ydisti=stepy*c(0, ev[i]*c(1:in.howy))

		gridi = grid.basis[which(grid.basis[,4]%in%xdisti & grid.basis[,5]%in%ydisti),]
		gridi[,7]=rep(length(results)+1,length(gridi[,7]))
		gridi[,8]=rep(paste("every.", ev[i], sep=""),length(gridi[,8]))
		
		results[[length(results)+1]]=gridi
		names(results)[length(results)]=paste("grid.every.", ev[i], sep="")
		
	}

# 8 - creating ID and working objects for additional sets (random points, cluster points, specified points)

if (is.null(rp)==F | is.null(cp)==F | is.null(sp)==F)
	{
		maxid=10^ceiling(log10(max(grid.basis$id)))
		minx=round(min(grid.basis$x),3)
		maxx=round(max(grid.basis$x),3)
		miny=round(min(grid.basis$y),3)
		maxy=round(max(grid.basis$y),3)
		shorter=getmin(c(howx,howy))
	}

# 9 - generating sets of random points

if (is.null(rp))
	{
	}
	else
	{
		rp[which(is.na(rp))]=ceiling(howx*howy/100)
		
		for (i in 1:length(rp))
			{
				newxi=sample(c((1000*minx):(1000*maxx)),rp[i],replace=T)/1000
				newyi=sample(c((1000*miny):(1000*maxy)),rp[i],replace=T)/1000
				idi=c((maxid+1):(maxid+rp[i]))
				maxid=maxid+rp[i]
				xdisti=newxi-minx
				ydisti=newyi-miny
		
				gridi = data.frame(
					id=idi,
					x=newxi,
					y=newyi,
					xdist=xdisti,
					ydist=ydisti,
					gridnumber=rep(gn,rp[i]),
					subgridnumber=rep(length(results)+1,rp[i]),
					gridtype=rep("random",rp[i]))
				
				results[[length(results)+1]]=gridi
				names(results)[length(results)]=paste("grid.random.",i,".", rp[i], sep="")
			}
	}


# 10 - generating clusters of points

if (is.null(cp))
	{
	}
	else
	{

		# preparation of working objects, replacing NAs and checking, if the argument vectors have the same length

			cp=cp[1:(length(cp)-length(cp)%%3)] # making cp length divisible by 3
			cpn=length(cp)/3

			# replacing NAs
			cp[which(is.na(cp))[which(is.na(cp)) %in% c(1:cpn*3-2)]]=cf
			cp[which(is.na(cp))[which(is.na(cp)) %in% c(1:cpn*3-1)]]=ceiling(howx*howy/pf)
			cp[which(is.na(cp))[which(is.na(cp)) %in% c(1:cpn*3-0)]]=shorter/df

		# creating clusters

			for (i in 1:cpn) # creating every grid
				{

					cni=cp[i*3-2]
					pni=cp[i*3-1]
					mdi=cp[i*3-0]

					npointsi=cni*pni
					
					# creating x and y coordinates of center points
						newxi=sample(c((1000*(minx+mdi)):(1000*(maxx-mdi))),cni,replace=T)/1000
						newyi=sample(c((1000*(miny+mdi)):(1000*(maxy-mdi))),cni,replace=T)/1000

					# creating x and y coordinates of clustered points
						newxi=rep(newxi,each=pni)+sample(c((-mdi*1000):(mdi*1000))/1000,npointsi,replace=T)
						newyi=rep(newyi,each=pni)+sample(c((-mdi*1000):(mdi*1000))/1000,npointsi,replace=T)

					idi=c((maxid+1):(maxid+npointsi))
					maxid=maxid+npointsi
					xdisti=newxi-minx
					ydisti=newyi-miny
			
					gridi = data.frame(
						id=idi,
						x=newxi,
						y=newyi,
						xdist=xdisti,
						ydist=ydisti,
						gridnumber=rep(gn,npointsi),
						subgridnumber=rep(length(results)+1,npointsi),
						gridtype=rep("cluster",npointsi))
					
					results[[length(results)+1]]=gridi
					names(results)[length(results)]=paste("grid.cluster.",i,".", cni,".", pni,".", mdi, sep="")
			
				}
	}


# 11 - generating specified clustered points


if (is.null(sp))
	{
	}
	else
	{

		# preparation of working objects, removing positions based on NA coordinates, replacing other NAs and checking, if the argument vectors have the same length


			sp=sp[1:(length(sp)-length(sp)%%4)] # making sp length divisible by 4
			spn=length(sp)/4

			# replacing NAs
			sp[which(is.na(sp))[which(is.na(sp)) %in% c(1:spn*4-1)]]=ceiling(howx*howy/pf)
			sp[which(is.na(sp))[which(is.na(sp)) %in% c(1:spn*4-0)]]=shorter/df


		# creating specified clusters

			for (i in 1:spn) # creating every grid
				{

					xi=sp[i*4-3]
					yi=sp[i*4-2]
					pni=sp[i*4-1]
					mdi=sp[i*4-0]

					# creating x and y coordinates of clustered points
						newxi=rep(xi,each=pni)+sample(c((-mdi*1000):(mdi*1000))/1000,pni,replace=T)
						newyi=rep(yi,each=pni)+sample(c((-mdi*1000):(mdi*1000))/1000,pni,replace=T)

					idi=c((maxid+1):(maxid+pni))
					maxid=maxid+pni
					xdisti=newxi-minx
					ydisti=newyi-miny
			
					gridi = data.frame(
						id=idi,
						x=newxi,
						y=newyi,
						xdist=xdisti,
						ydist=ydisti,
						gridnumber=rep(gn,pni),
						subgridnumber=rep(length(results)+1,pni),
						gridtype=rep("specified",pni))
					
					results[[length(results)+1]]=gridi
					names(results)[length(results)]=paste("grid.specified.",i,".", pni,".", mdi, sep="")
			
				}
	}


# 12 creating set of all points: from basis, borders, corners, every, random sets, clustered sets and specified sets

for (i in 3:length(results))
	{
		grid.all=rbind(grid.all,results[[i]])
	}
results[[1]]=grid.all

# 13 - ending

grids.ending.time=Sys.time()

if (st)
	{
		cat(paste("[ending function; on: ", format(grids.ending.time, "20%y-%m-%d"), ";  at: ", format(grids.ending.time, "%H:%M:%S"),"]",sep=""),"\n")
		cat(paste("[duration: ", duration.time(difftime(grids.ending.time,grids.starting.time,units="secs")),"]",sep=""),"\n")
	}

# 14 - writing csv files

if (write)
	{

		if (st)
			{
				writing.starting.time=Sys.time()
				cat(paste("writing csv files...", sep=""),"\n")
			}

		if(dir.exists(dire)){}else{dir.create(dire)}	

		gnzeros=formatC(gn, width = 5, format = "d", flag = "0")
		pointsnumbers = c()
		for(i in 1:length(results)){pointsnumbers=append(pointsnumbers,dim(results[[i]])[1])}


		# overview in README
		overview = c("
			This file contains main information about the grid and the arguments needed for its re-creation
				the re-creation will lead to the same results except for the random and cluster points - they will be different every time the function is used

			Comments: ", comments,"

			Start:     ", format(grids.starting.time, "20%y-%m-%d  %H:%M:%S"),"
			End:       ", format(grids.ending.time, "20%y-%m-%d  %H:%M:%S"),"
			Duration:  ", duration.time(difftime(grids.ending.time,grids.starting.time,units="secs")),"
			command:   ", "I cannot find how to save command","

			grids:     ", names(results),"
			points:    ", pointsnumbers,"

			arguments and working objects from inside the function:

			xs:       ", deparse(xs),"
			ys:       ", deparse(ys),"
			gn:       ", deparse(gn),"
			stepx:    ", deparse(stepx),"
			stepy:    ", deparse(stepy),"
			howx:     ", deparse(howx),"
			howy:     ", deparse(howy),"
			way:      ", deparse(way),"
			ev:       ", deparse(ev),"
			rp:       ", deparse(rp),"
			cp:       ", deparse(cp),"
			sp:       ", deparse(sp),"
			cf:       ", deparse(cf),"
			pf:       ", deparse(pf),"
			df:       ", deparse(df),"
			st:       ", deparse(st),"
			write:    ", deparse(write),"
			comments: ", deparse(comments),"
			dire:     ", deparse(dire),"

			----------------------------------

			the script for the whole function:


			gridpoints=")			
		sink(paste(dire,"/grid_", gnzeros, "_", formatC(0, width = 3, format = "d", flag = "0"), "_","README", ".txt", sep=""))
			cat(overview)
			print(gridpoints)
			sink()


		# writing the grids
		
		write.csv(grid.all, file=paste(dire,"/grid_", gnzeros, "_", formatC(1, width = 3, format = "d", flag = "0"), "_", "grid_all", ".csv", sep=""),row.names=F)
		write.csv(grid.basis, file=paste(dire,"/grid_", gnzeros, "_", formatC(2, width = 3, format = "d", flag = "0"), "_", "grid_basis", ".csv", sep=""),row.names=F)
		write.csv(grid.borders, file=paste(dire,"/grid_", gnzeros, "_", formatC(3, width = 3, format = "d", flag = "0"), "_", "grid_borders", ".csv", sep=""),row.names=F)
		write.csv(grid.corners, file=paste(dire,"/grid_", gnzeros, "_", formatC(4, width = 3, format = "d", flag = "0"), "_", "grid_corners", ".csv", sep=""),row.names=F)

		for (i in 5:length(results))
			{
				namei=gsub(".","_",names(results)[i],fixed=T)
				write.csv(results[[i]], file=paste(dire,"/grid_", gnzeros, "_", formatC(i, width = 3, format = "d", flag = "0"), "_", namei, ".csv", sep=""),row.names=F)
			}


		if (st)
			{
				writing.ending.time=Sys.time()
				cat(paste("[duration of writing: ", duration.time(difftime(writing.ending.time,writing.starting.time,units="secs")),"]",sep=""),"\n")
			}

	}


# 15 - returning results

results

}

