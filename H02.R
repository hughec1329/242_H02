# STA135 - HW02 - BML traffic model
# 20130125 - Hugh Crockford

# build area and populate

map = function(p,pc,r,c){
	n = p * r * c
	nred = n*pc
	nblue 	= n * (1 - pc)
	dimcar 	= c(r,c,p,n,pc,nred,nblue,0)
	nom=c("rows","column","pc_covered","n_cars","pc_red","n_red","n_blue","velocity")
	names(dimcar) = nom	
	all = data.frame(x=rep(1:r,each = c),y=1:c)
	pos = all[sample(1:(r*c),n),]
	pos$col = c(rep(1,nred),rep(2,nblue))
	cars = list("dim" = dimcar, "pos" = pos)
	class(cars) = "bml"
	return(cars)
}

plot.bml = function(m){
	grid = matrix(0,m$dim["rows"],m$dim["column"])
	grid[ as.matrix(m$pos[m$pos["col"] == 1 ,c("x","y") ])] = 1
	grid[ as.matrix(m$pos[m$pos["col"] == 2 ,c("x","y") ])] = 2
	image(grid,col=c("white","red","blue"))
}

summary.bml = function(x){
	dim = paste("dimensions:",x$dim["rows"],"rows X ",x$dim["column"]," columns")
	n = paste("number of cars:", x$dim["n_cars"])
	p = paste("p = ",x$dim["pc_covered"])
	print(list(dim = dim,n = n,p = p))
}


move = function(x,t){
	copy = x		# make working copy
	u = x$pos	# throw away dims
	new = u
	if(t %% 2){	# odd, move blue up. move up then check for conflicts.
		nmov = nrow(u[u$col ==1,])	# total number of cars that were supposed to move to work out velocity
		newy = u[u$col==2,]["y"]+1
		newy[newy>x$dim["rows"]]=1	# if fallen off edge, put at start
		new[new$col ==2,]["y"]=newy		# update pos?
	}
	# else move red right
	if(!t %% 2){	# odd, move blue up. move up then check for conflicts.
		nmov = nrow(u[u$col ==1,])
		newx = u[u$col==1,]["x"]+1
		newx[newx>x$dim["column"]]=1
		new[new$col ==1,]["x"]=newx		# update pos?
	}
	# conflict working but not if same color next door.
	conflict = duplicated(rbind(x$pos[,1:2],new[,1:2]))[(1+nrow(new)):(2*nrow(new))]
	unewchar = paste(new$x,new$y,sep = "")
	uchar = paste(u$x,u$y,sep = "")
	# conflict = unewchar %in% uchar	# cars getting stuck on edge ?
	copy$pos[!conflict,1:2] = new[!conflict,1:2]
	velocity = 1 - sum(copy$pos != new) / nmov	# depends which color moving, need to check while 
	copy$dim[["velocity"]] = velocity
	return(copy)
}

play = function(m,t) {
	vhs = array(0,dim = c(m$dim[["n_cars"]],3,t))
	vel = numeric(t)
	new = m
	for(i in 1:t){
		new = move(new,i)
		vhs[,,i] = as.matrix(new$pos)
		vel[i] = new$dim[["velocity"]]
	}
	betamax = list(dim = m$dim, vel = vel,vhs = vhs)
	class(betamax) = "vhs"
	return(betamax)
}

plot.vhs = function(m){
	for(i in 1:dim(m$vhs)[[3]]){		# number of time periods
		grid = matrix(0,m$dim["rows"],m$dim["column"])
		grid[ as.matrix(m$vhs[m$vhs[,3,i] == 1 ,1:2,i ])] = 1
		grid[ as.matrix(m$vhs[m$vhs[,3,i] == 2 ,1:2,i ])] = 2
		image(grid,col=c("white","red","blue"))
		Sys.sleep(.02)
	}
}

m = map(.2,.5,100,100)
y = play(m,1000)
plot(y)		# cars getting stuck on edge?
plot(y$vel)


