#install.packages("RNetCDF")
library (RNetCDF)
inputpath<-"C:/Users/Gohar Shoukat/Downloads/"
inputfile<-paste(inputpath,"ERA5_storm_BC.nc",sep="")
#This a NetCDF file
#1. Open the file:command'open.nc'
REAN_ini<-open.nc(inputfile)

#Have access to a summary:command:print.nc
print.nc(REAN_ini)

#2  Read the variables: command 'var.get.nc'

#1st var:longitude
lonn<-var.get.nc(REAN_ini,"longitude")

#2nd variable:latitude
latt<-var.get.nc(REAN_ini,"latitude")

#3rd Variable:time
ttime<-var.get.nc(REAN_ini,"time")

#hours since 1900-1-1 00:00:00

#We can convert these values into a more familiar time reference
#command: 'utal.nc'
ttim2<-utcal.nc("hours since 1900-1-1 00:00:00",ttime,type='n')

#4th variables:significant wave height
swh<-var.get.nc(REAN_ini,"swh",unpack=T)
dim (swh)

swh[4,1,88]

#5th variables:Mean wave height
mwp<-var.get.nc(REAN_ini,"mwp",unpack=T)
dim (mwp)

mwp[4,1,88]

6th variables:Mean wave direction
mwd<-var.get.nc(REAN_ini,"mwd",unpack=T)
dim (mwd)

mwd[4,1,88]


#7th variable:u10 zonal component of wind speed vector
u10<-var.get.nc(REAN_ini,"u10",unpack=T)

u10[4,1,88]

#th variable:u10 meridional component of wind speed vector
v10<-var.get.nc(REAN_ini,"v10",unpack=T)

v10[4,1,88]

#Create five empty (filled with NA values)
#matrices with 28 rows(gridpoints) and 141 columns
#(lon-lat+139 timesteps) :swh-mwp-mwd-u10-v10
swh_rean<-matrix(NA,28,141)
mwp_rean<-matrix(NA,28,141)
mwd_rean<-matrix(NA,28,141)
u10_rean<-matrix(NA,28,141)
v10_rean<-matrix(NA,28,141)

#POSITIONS OF THE ROWS;using a variable 'counter'
counter<-0
#POSITIONS OF THE COLUMNS IN THE MATRIXES
#7 postions in longitude
for (ii in seq(1,7,1)){
        #4 postions in latitude
        for(jj in seq(1,4,1)){
        counter<-counter+1
        #Longitude goes to the first column
        swh_rean[counter,1]<-lonn[ii]
        mwp_rean[counter,1]<-lonn[ii]
        mwd_rean[counter,1]<-lonn[ii]
        u10_rean[counter,1]<-lonn[ii]
        v10_rean[counter,1]<-lonn[ii]

        #Latitide goes to the second column
        swh_rean[counter,2]<-latt[jj]
        mwp_rean[counter,2]<-latt[jj]
        mwd_rean[counter,2]<-latt[jj]
        u10_rean[counter,2]<-latt[jj]
        v10_rean[counter,2]<-latt[jj]

        #From column 3:141
        swh_rean[counter,3:141]<-swh[ii,jj,1:139]
        mwp_rean[counter,3:141]<-mwp[ii,jj,1:139]
        mwd_rean[counter,3:141]<-mwd[ii,jj,1:139]
        u10_rean[counter,3:141]<-u10[ii,jj,1:139]
        v10_rean[counter,3:141]<-v10[ii,jj,1:139]

        }
        #jj:latitude
}
#ii:longitude

swh_rean
mwp_rean
mwd_rean
u10_rean
v10_rean

pow_rean<-0.489*mwp_rean*(swh_rean**2)
pow_rean[,1:2]<-mwp_rean[,1:2]

pow_rean[1:10,3:4]

library (sp)
library (maptools)
library (rgdal)
library (maps)
library (mapdata)
library (shape)
















#Minimum
min(pow_rean[,3:141],na.rm=T)

#Maximum
max(pow_rean[,3:141],na.rm=T)

#Geographical boundaries
#Longitude is in column#1
#Latitude is in column#2

min(pow_rean[,1],na.rm=T)
max(pow_rean[,1],na.rm=T)

min(pow_rean[,2],na.rm=T)
max(pow_rean[,2],na.rm=T)

mwd_rean2<-(270-mwd_rean)*(pi/180)
mwd_rean2




for (ii in 3:141){
layout(matrix(1:2,ncol=2),width=c(1,4),
height=c(1,1))

#Make a palette

coll<-colorRampPalette(c("green","yellow","orange","blue","brown","violet","red"))(1000)

#Plot the colorscale (left part)
colorlegend(zlim=c(0,410),zval=seq(0,410,50),
col=coll[1:1000],main="WEF Kw/m",
main.cex=0.6,posx=c(0.2,0.35),
posy=c(0.05,0.9))
#We can change the margins of the plot
#par(mar=c(5.1,4.1,5.1,2.1))
#bottom, left, right
par(mar=c(10.1,4.1,10.1,4.1))

#Plot a map on the right part
#Bay of Biscay****
map ("worldHires",xlim=c(-3.3,-0.55),
ylim=c(43,44.475),fill=T,col="beige")

box()
axis(1)
axis(2)


#At each gridpoint we put a symbol with a color
#According to the color scale
rescalecolor<-1+(100/410)*pow_rean[,ii]
#changin*pow_rean[,_] we can change the column
points(pow_rean[,1],pow_rean[,(2)],pch=15,
cex=20,col=coll[rescalecolor])
map ("worldHires",xlim=c(-3.3,-1.3),
ylim=c(43,44.6),fill=T,col="beige",add=T,lforce=T)

box()

#String object for the title
header<-paste("WEF [kW/m] Basque Coast","_",ttim2[ii-2,1],
"_",ttim2[ii-2,2],"_",ttim2[ii-2,3],"_",ttim2[ii-2,4],sep="")
#put a title to the plot
title(main=header,xlab="°E")

#Scale factor for the wind
sfwind<-0.03

arrows(u10_rean[,1],u10_rean[,2],
(u10_rean[,1]+(sfwind*u10_rean[,ii])),
(u10_rean[,2]+(sfwind*v10_rean[,ii])),
col="black",lwd=2,length=0.1,angle=15)

sfwef<-0.001
arrows(u10_rean[,1],u10_rean[,2],
(u10_rean[,1]+(sfwef*pow_rean[,ii]*cos(mwd_rean2[,ii]))),
(u10_rean[,2]+(sfwef*pow_rean[,ii]*sin(mwd_rean2[,ii]))),
col="red",lwd=4,length=0.09,angle=15)

#Location gridpoints
points(u10_rean[,1],u10_rean[,2],
cex=1.2,col="black",pch=21,bg="white",lwd=2)

#put a scale for wind with an arrow of 10m/s
arrows(-3,43.1,(-3+(10*sfwind)),43.1,
col="black",lwd=2,length=0.1,angle=15)
points(-3,43.1,cex=1.2,col="black",pch=21,bg="white",lwd=2)
text(-3,43.05, "10m/s",cex=0.6)


#put a scale for WEF with an arrow of 100 Kw/m
arrows(-2.51,43.1,(-2.51+(100*sfwef)),43.1,
col="red",lwd=1,length=0.09,angle=15)
points(-2.50,43.1,cex=1,col="red",pch=21,bg="white",lwd=2)

text(-2.51,43.05, "100 kW/m",cex=0.6)

#We need a folder to save the plots
#We give a name to the plot WITHOUT EXTENSION
nameplot<-ii
#Save plot in png format:we need to add the extension of the file
#depending on the format of the plot
plotfile<-paste(outputpath,nameplot,".png",sep="")
dev.copy(png,plotfile)
dev.off()
dev.off()
#Save plot in EPS format
#setEPS
#plotfile<-paste(outputpath,nameplot,".eps",sep="")
#dev.copy(postscript,plotfile,horizontal=F)
#dev.off()
}

