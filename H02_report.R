# STA135 - HW02 - BML traffic model
# 20130125 - Hugh Crockford


library(reshape2)
library(ggplot2)
source("BMLFn.R")

m = map(.2,.5,100,100)
y = play(m,500)
plot(y)	
plot(y$vel)


# velocities of different p
t = 500
p = seq(0.1,.8,.1)
o = sapply(p,function(i) play(map(i,.5,100,100),t))	# returns list of vhs w diff
q = data.frame(1:t,o[2,])
names(q) = c("time", p)
m = melt(q , id.vars = "time")
qplot(data = m , x=time,y=value,color = variable, geom = "smooth") 	# vel drops off at ~ p = 0.4

four = o[,4]
class(four) = "vhs"
plot(four)

five = o[,5]
class(five) = "vhs"
plot(five)

six = o[,6]
class(six) = "vhs"
plot(six)	# locks up.


# investigate this more
p = seq(0.3,.5,.01)
o = sapply(p,function(i) play(map(i,.5,100,100),t))
q = data.frame(1:t,o[2,])
names(q) = c("time", p)
m = melt(q , id.vars = "time")
qplot(data = m , x=time,y=value,color = variable, geom = "smooth")


# velocities of different percent red/blue.
t = 500
p = seq(0,1,.1)
o = sapply(p,function(i) play(map(.3,p,100,100),t))
q = data.frame(1:t,o[2,])
names(q) = c("time", p)
m = melt(q , id.vars = "time")
qplot(data = m , x=time,y=value,color = variable, geom = "smooth")
