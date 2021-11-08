library(FactoMineR)
chien <-read.table("chien.txt", sep="\t", header= TRUE, dec=",")
rownames(chien)<- chien$indiv
chien$indiv<-NULL
for(i in 1:7) chien[,i]=as.factor(chien[,i])#chien
#on supprime la variable Utility qui sera illustrative
H <-subset(chien,select=-utili)#ACM avec FactoMineR
res <-MCA(H,ncp=10,graph=FALSE)
plot(res,title="representation simultanee")
#Inertie
vp<- res$eig
vp
#nombre de modalites
m <-sum(unlist(lapply(H,function(x){length(levels(x))})))
m
p <-ncol(H)
p
contrib <- res$var$contrib
#contrib
#contributions relatives
plot(res,select="cos2 10",title="Les 10 chiens les mieux projetes")
res <-MCA(chien,ncp=10,graph=TRUE, quali.sup = 7)
#on a vu tous cela dans le cours
#la modalite du  hcine qui a une unite de la moyenne de coposition pincipale
#la projection de la moyenne de coposition pincipale

res$quali.sup$v.test
#les stats du test pour coparer la moyenne des chiens utili 1 de moyenne 0 
#loi noramle pour stat

nutado <-read.table("nutado.txt", sep="\t", header= TRUE, dec=",")
rownames(nutado)<- nutado$indiv
nutado$indiv<-NULL
length(nutado)
for(i in 1:16) nutado[,i]=as.factor(nutado[,i])
ind=which(apply(is.na(nutado[1:15]),1,sum)==0)
summary(nutado[ind,])
data=nutado[ind,]
head(data)