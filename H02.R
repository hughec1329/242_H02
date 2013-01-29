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
	
