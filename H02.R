# STA135 - HW02 - BML traffic model
# 20130125 - Hugh Crockford

# build area and populate

buildMap = function(p,pc=0.5,r=100,c=100){ 	# p - percent covered, pc = percent red.
	if( p>1 | p<0){
		print("invalid p")
	}
	n 	= r * c * p
	nred 	= n * pc
	nblue 	= n * (1 - p)
	dimcar 	= c(r,c,p,n,pc,nred,nblue)
	nom=c("rows","column","pc_covered","n_cars","pc_red","n_red","n_blue")
	names(dimcar) = nom	
	pos 	= data.frame(col = sample(c("red","blue"),n,replace = TRUE),x = sample(1:c,n,replace = TRUE), y = sample(1:r,n, replace = TRUE))
	cars = list("dim" = dimcar, "red" = posR, "blue" = posB)
	#	class(cars) = Bmap
	return(cars)
}	




# develop class and plot.method



# move cars


