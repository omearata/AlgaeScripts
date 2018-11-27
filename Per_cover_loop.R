#USING GGPLOT
######################################
attach(algaetest<-read.table(file.choose(), header=TRUE))
attach(algaetest<-cbind(algaetest, AreaP=ifelse((3.14*(Diameter/2)^2) %% 2 >= 0.1, (3.14*(Diameter/2)^2), 0.1)))
attach(algaetest<-cbind(algaetest, AreaQ=(Xmax-Xmin)*(Ymax-Ymin)))
attach(algaetest<-cbind(algaetest, Count=round((PerCov*(AreaQ/AreaP)), 0)))
head(algaetest)
pts<-data.frame(X=0, Y=0, Spec=0, Diam=0)

for (row in 1:nrow(algaetest)) {
X=runif(Count[row],Xmin[row],Xmax[row])
Y=runif(Count[row], Ymin[row],Ymax[row])
Spec=rep.int(Species[row], Count[row])
Diam=rep.int(area[row], Count[row])
ptstemp<-data.frame(cbind(X, Y))
ptstemp<-cbind(ptstemp, Spec)
ptstemp<-cbind(ptstemp, Diam)
pts<-rbind(ptstemp, pts)
}

library(ggplot2)
ggplot(pts, aes(X,Y, col=Spec, group=factor(Spec)))+
geom_jitter(height=1, width=1, alpha=0.6, aes(size=Diam), 
shape=ifelse(Diam>0.1, 19, 20))+
scale_size_area(max_size = 10) + 
for (row in 1:nrow(shore)){
geom_segment(aes(x = xi[row], y = yi[row], xend = xf[row], yend = yf[row]))
}


#BASIC R CODE
#############################################
attach(algaetest<-read.table(file.choose(), header=TRUE))
attach(algaetest<-cbind(algaetest, AreaP=ifelse((3.14*(Diameter/2)^2) %% 2 >= 0.1, (3.14*(Diameter/2)^2), 0.1)))
attach(algaetest<-cbind(algaetest, AreaQ=(Xmax-Xmin)*(Ymax-Ymin)))
attach(algaetest<-cbind(algaetest, Count=round((PerCov*(AreaQ/AreaP)), 0)))
attach(algaetest<-cbind(algaetest, ColText=c("coral2", 'darkblue', 'blueviolet','forestgreen','firebrick')))

par(xaxs="i", yaxs="i")
plot(c(min(Xmin),max(Xmax)),c(min(Ymin),max(Ymax)), type='n', xlab='distance along shore (m)',ylab='distance from shore (m)',las=1)

for (row in 1:nrow(algaetest)) {
points(runif(Count[row], Xmin[row], Xmax[row]),runif(Count[row], Ymin[row],Ymax[row]), pch=20, 
cex=2.5*AreaP[row], col=color[row])
}

for (row in 1:nrow(algaetest)) {
ggplot(algaetest, aes((runif(Count[row], Xmin[row], Xmax[row]),(runif(Count[row], Ymin[row],Ymax[row]))+geom_point()
}

ggplot(algaetest, aes((runif(Count, Xmin, Xmax)),(runif(Count, Ymin,Ymax)))+geom_point()

pts<-0
for (row in 1:nrow(algaetest)) {
points(runif(Count[row], Xmin[row], Xmax[row]),runif(Count[row], Ymin[row],Ymax[row]), pch=pch[row], 
cex=Diameter[row], col=color[row])
}

pts<-data.frame(cbind(X=runif(10,0,10), Y=runif(10,0,10)))
test<-ggplot(pts, aes(X,Y))+geom_point()
