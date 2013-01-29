# STA135 - HW02 - BML traffic model
# 20130125 - Hugh Crockford

# build area and populate

# this version can have cars in same position!!!

buildMap = function(p,pc=0.5,r=100,c=100){ 	# p - percent covered, pc = percent red.
	if( p>1 | p<0){
		print("invalid p")
	}
	n 	= r * c * p
	nred 	= n * pc
	nblue 	= n * (1 - pc)
	dimcar 	= c(r,c,p,n,pc,nred,nblue)
	nom=c("rows","column","pc_covered","n_cars","pc_red","n_red","n_blue")
	names(dimcar) = nom	
	pos 	= data.frame(x = sample(1:c,n,replace = TRUE), y = sample(1:r,n, replace = TRUE), col = rep(c(1,2),c(nred,nblue)))
	cars = list("dim" = dimcar, "pos" = pos)
	#	class(cars) = Bmap
	return(cars)
}	

m = buildMap(.2,.5,10,10)

# put this inside plot.bml?

grid = matrix(0,m$dim["rows"],m$dim["column"])
grid[ as.matrix(m$pos[m$pos["col"] == 1 ,c("x","y") ])] = 1
grid[ as.matrix(m$pos[m$pos["col"] == 2 ,c("x","y") ])] = 2
image(grid,col=c("white","red","blue"))



setClass("bml",representation(dim="numeric",pos="data.frame"))
class(m) = "bml"

plot(m$pos[[1]],m$pos[[2]], col = c("red","blue")[m$pos[[3]]],pch=15)

plot.bml = function(x){

}

# try to get whole image scaled on size - 
# dev.new and X11.options in plot call.
# pixels/inch?

# develop class and plot.method



# move cars

############################3
# try as matrix
###############################

m = matrix(sample(0:1,10000,replace = TRUE),100,100)
image(m,axes = FALSE)

buildMap = function(p,pc=0.5,r=100,c=100){ 	# p - percent covered, pc = percent red.
	n = r*c 	#total number spaces
	ncars = p*n
	cars = sample(1:n, ncars, replace = TRUE)	# where to put cars
	map = rep(0,n)
	map[cars] = sample(c(1,2),replace = TRUE)
	m = matrix(map,r,c)
	return(m)
}
	

####### alt method to chose where cars go - cannot repeat same car.
map = function(p,pc,r,c){
	n = p * r * c
	nred = n*pc
	nblue 	= n * (1 - pc)
	dimcar 	= c(r,c,p,n,pc,nred,nblue)
	nom=c("rows","column","pc_covered","n_cars","pc_red","n_red","n_blue")
	names(dimcar) = nom	
	all = data.frame(x=rep(1:r,each = c),y=1:c)
	pos = all[sample(1:(r*c),n),]
	pos$col = c(rep(1,nred),rep(2,nblue))
	cars = list("dim" = dimcar, "pos" = pos)
	class(cars) = "bml"
	return(cars)
}


m = map(.2,.5,10,10)

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

plot(m)

move = function(x,t){
	q = x		# make working copy
	u = x$pos	# throw away dims
	new = u
	if(t %% 2){	# odd, move blue up. move up then check for conflicts.
		newy = u[u$col==2,]["y"]+1
		newy[newy>x$dim["rows"]]=1
		new[new$col ==2,]["y"]=newy		# update pos?
	}
	# else move red right
	if(!t %% 2){	# odd, move blue up. move up then check for conflicts.
		newx = u[u$col==2,]["x"]+1
		newx[newx>x$dim["column"]]=1
		new[new$col ==2,]["x"]=newx		# update pos?
	}
	# conflict not working.
	conflict = duplicated(rbind(x$pos[,1:2],new[,1:2]))[1:nrow(x$pos)]
	q$pos[!conflict,1:2] = new[,1:2]
	return(q)
}

