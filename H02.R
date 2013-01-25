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
	map  	= data.frame(x = 1:c, y = 1:r)	# build map area - unnececcary.
	map$dimx = c 
	map$dimy = r	# use attributes, or list to store dimension as recquired?
	posR 	= data.frame(x = sample(1:c,nred,replace = TRUE), y = sample(1:r,nred, replace = TRUE))
	posB 	= data.frame(x = sample(1:c,nblue,replace = TRUE), y = sample(1:r,nblue,replace = TRUE))
	cars = list("dim" = dimcar, "red" = posR, "blue" = posB)
	#	class(cars) = Bmap
	return(cars)
}	




# develop class and plot.method



# move cars


