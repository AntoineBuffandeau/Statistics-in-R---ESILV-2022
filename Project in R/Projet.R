getwd()
setwd(dir= "C:\\Users\\ruben\\Documents\\R\\Repos")
MS=read.csv("Mass Shooting.csv",sep=",", row.name = 1)

#selection et uniformisation des données
MS = MS[c(3,9,10,11,13,16,17,18)]  #selectionne bonnes colonnes
MS = MS[c(155:323),c(1:8)]           #selectionne bonnes lignes
row.names(MS) <- 1:nrow(MS)          #remet compteur à 1


MS$Gender = gsub("Female","F",MS$Gender)
MS$Gender = gsub("Male","M",MS$Gender)

MS$Race = gsub("White American or European American","White",MS$Race)
MS$Race = gsub("white","White",MS$Race)
MS$Race = gsub("White/Some other Race","White",MS$Race)
MS$Race = gsub("Black American or African American","Black",MS$Race)
MS$Race = gsub("black","Black",MS$Race)
MS$Race = gsub("Black/Unknown","Black",MS$Race)
MS$Race = gsub("Native American or Alaska Native","Native",MS$Race)
MS$Race = gsub("Asian American","Asian",MS$Race)
MS$Race = gsub("Asian American/Other","Asian",MS$Race)
MS$Race = gsub("Asian/Other","Asian",MS$Race)
MS$Race = gsub("Some other race","Other",MS$Race)
MS$Race = gsub("Two or more races","Other",MS$Race)

MS$Age = gsub(",.*","",MS$Age)
MS <- MS[c(-7,-26,-37,-84,-112,-115,-136,-167,-5),]    #supprime ligne age nul
row.names(MS) <- 1:nrow(MS)                            #remet compteur à 1
MS$Age = strtoi(MS$Age, base = 0L)  #StringToInt








#boites à moustache :
boxplot(MS$Total.victims~MS$Race)
boxplot(MS$Total.victims~MS$Gender)
boxplot(MS$Total.victims~MS$Mental.Health.Issues)

#graph en bâton :

#condition mentale:
MH=c("Yes","No","Unclear","Unknown")
moyennes=rep(0,4)
k=0
for (i in MH)
{
 k=k+1
 moyennes[k]= mean(MS[MS$Mental.Health.Issues ==i,4])
}
moyennes = matrix(moyennes,nc=4, nr=1, byrow=1)
colnames(moyennes) = MH
barplot(moyennes,beside=T) ; box()

#sexe :
S=c("M","F","M/F")
moyennes=rep(0,3)
k=0
for (i in S)
{
  k=k+1
  moyennes[k]= mean(MS[MS$Gender ==i,4])
}
moyennes = matrix(moyennes,nc=3, nr=1, byrow=1)
colnames(moyennes) = S
barplot(moyennes,beside=T) ; box()

#ethnie :
R=c("White","Black","Asian","Latino","Native","Other")
moyennes=rep(0,6)
k=0
for (i in R)
{
  k=k+1
  moyennes[k]= mean(MS[MS$Race ==i,4])
}
moyennes = matrix(moyennes,nc=6, nr=1, byrow=1)
colnames(moyennes) = R
barplot(moyennes,beside=T) ; box()

#nuage de point :

plot(MS$Age,MS$Total.victims)
b = cov(MS$Age,MS$Total.victims) / var(MS$Age)
a = mean(MS$Total.victims) - b * mean(MS$Age)
abline(a,b, col="red")

Rcarre = (cov(MS$Age,MS$Total.victims) / (sqrt(var(MS$Age)) * sqrt(var(MS$Total.victims)))) ^2
print(Rcarre)
cor.test(MS$Age, MS$Total.victims, method = "pearson")    # = sqrt(Rcarre)
cor.test(MS$Age, MS$Total.victims, method = "spearman")

summar(MS$Total.victims)
summary(MS$Age)

R=c("White","Black","Asian","Latino","Native","Other")
varTotal = var(MS$Race)
varInter = 1/6 * sum


vartot = var(MS$Total.victims)
R=c("White","Black","Asian","Latino","Native","Other")
varinter = rep(0,6)
for(i in range(6)){
  varinter[i] = var(MS[MS$Race == R[i],4])
}
ecarre = sum(varinter)/vartot
print(ecarre)


vartot = var(MS$Total.victims)
S=c("M","F","M/F")
varinter = rep(0,3)
for(i in range(3)){
  varinter[i] = var(MS[MS$Gender == S[i],4])
}
ecarre = sum(varinter)/vartot
print(ecarre)

vartot = var(MS$Total.victims)
MH=c("Yes","No","Unclear","Unknown")
varinter = rep(0,4)
for(i in range(4)){
  varinter[i] = var(MS[MS$Mental.Health.Issues == MH[i],4])
}
ecarre = sum(varinter)/vartot
print(ecarre)
