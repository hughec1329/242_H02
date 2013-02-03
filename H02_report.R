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
qplot(data = m , x=time,y=value,color = variable, ylab = "velocity", main = "average velocity for different levels of p",geom = "smooth") 	# vel drops off at ~ p = 0.4

four = o[,4]
class(four) = "vhs"
plot(four)

five = o[,5]
class(five) = "vhs"
plot(five)

six = o[,6]
class(six) = "vhs"
plot(six)	# locks up ~ 400


# investigate this more
t = 500
p = seq(0.3,.6,.05)
o = sapply(p,function(i) play(map(i,.5,100,100),t))
q = data.frame(1:t,o[2,])
names(q) = c("time", p)
m = melt(q , id.vars = "time")
qplot(data = m , x=time,y=value,color = variable, geom = "smooth")


# velocities of different percent red/blue.
t = 500
p = seq(0.1,.9,.1)
o = sapply(p,function(i) play(map(.3,i,100,100),t))
q = data.frame(1:t,o[2,])
names(q) = c("time", p)
m = melt(q , id.vars = "time")
qplot(data = m , x=time,y=value,color = variable, geom = "smooth")

one = o[,1]
class(one) = "vhs"
plot(one)


# code testing 

Rprof("testing.out")
t = play(map(0.3,.5,100,100),500)
Rprof(NULL)
head(summaryRprof("testing.out")$by.self)

Rprof("testing2.out")
t = play(map(0.3,.5,100,100),500)
Rprof(NULL)
summaryRprof("testing2.out")
summaryRprof("testing2.out")$by.self


# is it size map or replications (t) that slow it up?

mdim = c(10,50,100,500,1000)
tim.size = sapply(mdim, function(i) system.time({t=play(map(0.3,.5,i,i),50)}))
times = seq(250,1000,250)
time.time = sapply(times, function(i) System.time({t=play(map(0.3,.5,100,100),i)}))
rho = seq(0.1,.9,.2)
tim.rho = sapply(rho, function(i) System.time({t=play(map(i,.5,100,100),500)}))


library(profr)
out = profr(t = play(map(0.3,.5,100,100),500))
