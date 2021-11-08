X <- read.csv(file="eaux.csv", sep=";", dec=",", header= TRUE, row.names=1)
n<-nrow(X)
p<-ncol(X)
n
p
moy<-apply(X,2,mean)#renvoie le vecteur des moyennes de chaque colonnes
moy
var<-apply(X,2,sd)
var
Y <- sweep(X,2,moy,"-")# Matrice centr¨¦e
apply(Y,2,mean)
sdy<-apply(Y,2,sd)*sqrt((n-1)/n)
sdy
Z <- sweep(Y,2,sdy,"/") #matrice r¨¦duite
apply(Z,2,FUN=sd)*sqrt((n-1)/n)#affiche que des 1 ici
print(Z)
Z2=as.matrix(Z)
mean(Z2)#tends vers 0
sd(Z2)#tends ver 1
#DOnc centree reduite
N<-diag(rep(1/n,n))
cor(X)
R<-t(Z2)%*%Z2/n
R

e=eigen(R)
ev<-e$values
evp<-e$vectors
sum(ev)
Psi=Z2%*%evp
Psi
moy<-apply(Psi,2,mean)
moy
var<-apply(Psi,2,var)*((n-1)/n)
var


part=seq(1,13,1)
for(i in 1:13){
  part[i]=var[i]/sum(var[1:13])
}
part


par(mfrow=c(1,2))
plot(Psi[,1:2])
text(Psi[,1:2],rownames(X))
plot(Psi[,2:3])
text(Psi[,2:3],rownames(X))



r1<-cor(Z2[,],Psi[,1])
r2<-cor(Z2[,],Psi[,2])
r3<-cor(Z2[,],Psi[,3])
par(mfrow=c(1,1))
nomx=paste("Axe 1",round(part[1]),"%")
nomy=paste("Axe 2",round(part[2]),"%")
plot(r1,r2, xlim=c(-1,1), ylim=c(-1,1),asp=1,pch=16,xlab=nomx, ylab=nomy)
abline(h=0,v=0)
symbols(0,0,circles=1, inches=F,add=T)
text(r1,r2,labels=colnames(Z),cex=0.8)
