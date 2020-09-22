library(plotrix)


#1.

#Set up the screen with narrow margins and aspect ratio 1:1
pars<-par(mar=c(1,1,1,1)+0.1, pch=20)  #set up narrow margins
plot(NULL, xlim = c(0,25), ylim = c(-1,25), xlab = "", ylab= "", asp = 1, axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left asnd bottom

arrows(0, 0, 5*cos(0*pi/180), 5*sin(0*pi/180), col = "green")
arrows(0, 0, 10*cos(53*pi/180), 10*sin(53*pi/180), col="red")




#2. 
#Set up the screen with narrow margins and aspect ratio 1:1
pars<-par(mar=c(1,1,1,1)+0.1, pch=20)  #set up narrow margins
plot(NULL, xlim = c(0,300), ylim = c(-300,0), xlab = "", ylab= "", asp = 1, axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left asnd bottom

arrows(0, 0, 350*cos(-18*pi/180), 350*sin(-18*pi/180), col = "green")
arrows(0, 0, 220*cos(-14*pi/180), 220*sin(-14*pi/180), col="red")
v1 <- c(220*cos(-14*pi/180), 220*sin(-14*pi/180))
v2 <- c(150*cos(-23*pi/180), 150*sin(-23*pi/180))
v3 <- v1+v2
arrows(v1[1], v1[2], v3[1], v3[2], col="red")


#distance: 
v0 <- c(350*cos(-18*pi/180), 350*sin(-18*pi/180))
v <- v0-v3
v


#Dot product operator
"%.%" <- function(x,y) sum(x*y)

Norm<- function(v) sqrt(v %.% v)

Norm(v)

#19.02954


#3. 

latL <- 52; longL <- 8
latMum <- 19; longMum <- 73
latMoc <-56; longMoc <-38
latC <- 34; longC <-18.5

London <- c(Cos(latL)*Cos(longL),Cos(latL)*Sin(longL),Sin(latL)); London #unit vector for Boston
Mumbai <- c(Cos(latMum)*Cos(longMum),Cos(latMum)*Sin(longMum),Sin(latMum)); Mumbai
Moscow <- c(Cos(latMoc)*Cos(longMoc),Cos(latMoc)*Sin(longMoc),Sin(latMoc)); Moscow
Cape <- c(Cos(latC)*Cos(longC),Cos(latC)*Sin(longC),Sin(latC)); Cape

degrees_lm <- angleBetween(London,Mumbai); degrees_lm #50.40199
#By definition, 90 degrees on the Earth equals 10,000 kilometers
kilometers_lm <- 10000*degrees_lm/90
kilometers_lm   #the trip is 6647 km

degrees_mc <- angleBetween(Moscow, Cape); degrees_mc #13.62378
#By definition, 90 degrees on the Earth equals 10,000 kilometers
kilometers_mc <- 10000*degrees_mc/90
kilometers_mc   #the trip is 2862 km



#3.b
"%x%" <- function(v,w) c(v[2]*w[3]-v[3]*w[2],v[3]*w[1]-v[1]*w[3], v[1]*w[2]-v[2]*w[1])
London %x% Mumbai
Cape %x% Moscow
