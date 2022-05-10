###############################
## Illustration loi de Markov #
##  Bernouilli, Binomiale,    #
##  Poisson                   #
###############################

# Partie 0 :Manipulation de vecteurs - Répertoire courant

Test = 10000
is.vector(Test)

Test ="Test"
is.vector(Test)
Test[2] = 10
Test
Test[5] = 3
Test


ls()
rm(list=ls())
ls()

setwd("C:/Users/t.dumont/Documents/DOCUMENTS/PRESENTATIONS/Cours Introduction à R CIRM 09042014/R")


# Partie 1 : Simulation des variables aléatoires à partir de la loi uniforme

# n Bernouilli indépendantes:
#############################

# Par Construction :
Bern =function(n =1,p =0.5)
{
	U = runif(n)						#Tire un n-échantillon de la loi Uniforme sur [0,1]
	X = rep(0,n)						#Cree un vecteur nul de taille n
	X[which(U<p)] = 1					#X[i] = 1 si U[i]<p
	return(X)				
}

# Par fonction R
Bern =function(n =1,p =0.5)
{
	X = rbinom(n,size = 1,prob= p)		#Tire un n-échantillon de la loi B(N,p) avec N=1 (---> Bernouilli)
	return(X)
}

# n Binomiales indépendantes:
#############################

# Par Construction :
Bino = function(n,N,p)
{
	ber = Bern(N,p)						# Appel à la fonction Bern que l'on vient de créer (ber vecteur de taille N contenant des 0 et des 1
	X=sum(ber)							# sommer le le vecteur ber pour obtenir le nombre de succés
	return(X)
}

# Par fonction R
Bino = function(n,N,p)
{	 
	X= rbinom(n,size = N,prob= p)		#Tire un n-échantillon de la loi B(N,p) 
	return(X)
}

# n Geometriques indépendantes:
#############################

# Par fonction R (l'autre est trop pénible)
Geom = function(n,p)
{
	X= rgeom(n,prob= p)
	return(X)
}


# n Poisson indépendantes:
##########################
 
# Par fonction R (l'autre est trop pénible)
Poiss= function(n,lambda)
{
	 X = rpois(n,lambda=lambda)
	 return(X)
}

# Partie 2 : Illustration loi des grands nombres

# simulation

p      = 0.2
N	   = 10
lambda = 1

n      = 100

Xbe    = Bern(n,p)
Xbi    = Bino(n,N,p)
Xge    = Geom(n,p)
Xpo	   = Poiss(n,lambda)

Tbe = tabulate(Xbe+1)/n #La fonction tabulate compte le nombre de "1", de "2", de "3" .... Les "0" ne sont pas comptés d'où l'ajout de 1 à Xbe,Xbi,Xpo
Tbi = tabulate(Xbi+1)/n
Tge = tabulate(Xge)/n
Tpo = tabulate(Xpo+1)/n

Densbe = dbinom(c(0,1),size = 1,prob= p)
Densbi = dbinom(0:(length(Tbi)-1),size = N,prob= p)
Densge = dgeom(1:length(Tge),prob= p)
Denspo = dpois(0:(length(Tpo)-1),lambda=lambda)

x11()	#ouvre un nouvelle fenêtre graphique
op <- par(mfrow = c(2, 2)) # dans cette fenetre 4 graphiques pourront être affichés en tableau 2 lignes 2 colonnes


# Affichage des 4 graphiques :
xmax = 1
ymax = max(Tbe,Densbe)

plot(c(0,xmax),Tbe, ylim= c(0, ymax) ,xlab="K",ylab="P(K)"    , type = "h", col = "red", lwd=5, main="Bernouilli") # Affiche Tbe
points(c(0,1),Densbe,type="h" ,col="blue", lwd=2)  # Superpose Densbe
legend("topright",c("donnees", "densite"), col = c("red","blue"),lty=1,lwd=c(5,2)) # Rajoute la légende

xmax = length(Tbi)-1
ymax = max(Tbi,Densbi)

plot(0:xmax ,Tbi, ylim= c(0, ymax)  ,xlab="K",ylab="P(K)", type = "h", col = "red", lwd=5, main="Binomiale") # Affiche Tbi : un nouveau graphique s'affiche à côté du précédent
points(0:(length(Tbi)-1),Densbi,type="h" ,col="blue", lwd=2)  # Superpose Densbi
legend("topright",c("donnees", "densite"), col = c("red","blue"),lty=1,lwd=c(5,2)) # Rajoute la légende

xmax = length(Tge)
ymax = max(Tge,Densge)
plot(Tge, ylim= c(0,ymax )  ,xlab="K",ylab="P(K)", type = "h", col = "red", lwd=5, main="Geometrique") # Affiche Tge : un nouveau graphique s'affiche en dessous du premier
points(1:xmax,Densge,type="h" ,col="blue", lwd=2) # Superpose Densge
legend("topright",c("donnees", "densite"), col = c("red","blue"),lty=1,lwd=c(5,2)) # Rajoute la légende

xmax = length(Tpo)-1
ymax = max(Tpo,Denspo)
plot(0:xmax,Tpo, ylim= c(0, ymax) ,xlab="K",ylab="P(K)" , type = "h", col = "red", lwd=5, main="Poisson") # Affiche Tpo : un nouveau graphique s'affiche en bas à gauche
points(0:xmax,Denspo,type="h" ,col="blue", lwd=2) # Superpose Denspo
legend("topright",c("donnees", "densite"), col = c("red","blue"),lty=1,lwd=c(5,2)) # Rajoute la légende

# Enregistrement des données et de l'image
# Création d'une matrice n x 4 : chaque colonne correspondant à une simulation
Simus = matrix(0,n,4)
Simus[,1] = Xbe
Simus[,2] = Xbi
Simus[,3] = Xge
Simus[,4] = Xpo

# Sauvegarde de la matrice Simus
 
setwd("C:/Users/t.dumont/Documents/DOCUMENTS/PRESENTATIONS/Cours Introduction à R CIRM 09042014/R/Donnees")

save(Simus, file = "simus.RData") #Sauvegarde l'objet R : "Simus"
write.csv(Simus, file="simus.csv") # Sauvegarde "Simus" sous forme d'un tableau : récupération possible sur open office

rm("Simus") # Supprime l'objet R "Simus" de la session R
Simus		# Rien ne s'affiche
load("simus.RData") # Charge l'objet contenu dans Simus.RData
Simus	# Objet Simus existe 

rm("Simus") # Supprime l'objet R "Simus" de la session R
read.csv("Simus.csv") # charge le fichier .csv (il s'affiche)
Simus # Simus n'existe toujours pas
Simus = read.csv("Simus.csv",header=TRUE) # charge le fichier .csv et le mettre dans la variable Simus
is.matrix(Simus)  # "Simus" n'est pas une matrice (is.matrix(Simus) retourne FALSE)
is.data.frame(Simus) # c'est un objetdeclasse data.frame (is.data.frame(Simus) retourne TRUE)

Simus = as.matrix(Simus) # conversion de l'objet "Simus" en matrice avec la fonction as.matrix 
is.matrix(Simus) # "Simus"  est maintenant une matrice (is.matrix(Simus) retourne TRUE)
dim(Simus) # On a gagné une colonne par rapport à la matrice originale....

# Sauvegarde de l'image 

setwd("C:/Users/t.dumont/Documents/DOCUMENTS/PRESENTATIONS/Cours Introduction à R CIRM 09042014/R/images")

dev.copy(jpeg,filename="GrandNombre.jpg"); 
dev.off ();

graphics.off()

###############################################################
## Approximation de la loi binomiale par la loi de Poisson	 ##
###############################################################

lambda = 100

p = function(N)
{
	return(min(lambda/N,1))
}
 
n=10000

X10   = Bino(n,10,p(10))
X100  = Bino(n,100,p(100))
X1000 = Bino(n,1000,p(1000))
X10000 = Bino(n,10000,p(10000))

T10    = tabulate(X10+1)/n
T100   = tabulate(X100+1)/n
T1000  = tabulate(X1000+1)/n
T10000 = tabulate(X10000+1)/n

Dens10 = dpois(0:(length(T10)-1),lambda=lambda)
Dens100 = dpois(0:(length(T100)-1),lambda=lambda)
Dens1000 = dpois(0:(length(T1000)-1),lambda=lambda)
Dens10000 = dpois(0:(length(T10000)-1),lambda=lambda)


x11()
op <- par(mfrow = c(2, 2))

xmax = length(T10)-1
ymax = max(T10,Dens10)

plot(0:xmax ,T10, ylim= c(0, ymax)  , type = "h", col = "red", lwd=5, main="N=10")
points(0:(length(T10)-1),Dens10,type="h" ,col="blue", lwd=2)
legend("topright",c("donnees", "densite"), col = c("red","blue"),lty=1,lwd=c(5,2))


xmax = length(T100)-1
ymax = max(T100,Dens100)

plot(0:xmax ,T100, ylim= c(0, ymax)  , type = "h", col = "red", lwd=5, main="N=100")
points(0:(length(T100)-1),Dens100,type="h" ,col="blue", lwd=2)
legend("topright",c("donnees", "densite"), col = c("red","blue"),lty=1,lwd=c(5,2))


 
xmax = length(T1000)-1
ymax = max(T1000,Dens1000)

plot(0:xmax ,T1000, ylim= c(0, ymax)  , type = "h", col = "red", lwd=5, main="N=1000")
points(0:(length(T1000)-1),Dens1000,type="h" ,col="blue", lwd=2)
legend("topright",c("donnees", "densite"), col = c("red","blue"),lty=1,lwd=c(5,2))

xmax = length(T10000)-1
ymax = max(T10000,Dens10000)

plot(0:xmax ,T10000, ylim= c(0, ymax)  , type = "h", col = "red", lwd=5, main="N=10000")
points(0:(length(T10000)-1),Dens10000,type="h" ,col="blue", lwd=2)
legend("topright",c("donnees", "densite"), col = c("red","blue"),lty=1,lwd=c(5,2))

setwd("C:/Users/t.dumont/Documents/DOCUMENTS/PRESENTATIONS/Cours Introduction à R CIRM 09042014/R/images")

dev.copy(jpeg,filename="ApproxBinoPois.jpg"); 
dev.off ();

graphics.off()

####################################### 
## Illustration Inégalité de Markov	 ##
## Pour la loi de Poisson		     ##
####################################### 
# P(X>=a)<= E(X)/a

# Objectif tracer sur le même graphe en fonction de "a"
# E(X)/a = lambda/a
# Pn(X>=a)
# P(X>=a)
 
n      = 10000
a      = 3
lambda = 1
X      = rpois(n,lambda=lambda)

F      = ecdf(X)
plot(F)
aMax=40
M      = matrix(0,aMax,4)


for(a in 1:40)
{
	M[a,1] =  a
	M[a,2] = lambda/a
	M[a,3] = 1-F(a-1)
	M[a,4] = 1-ppois(a,lambda=lambda)
		
}

## Utilisation de matplot
x11()
par(mfrow=c(2,1))
matplot(M[,1],M[,2:4])

matplot(M[,1],M[,2:4],type="l") 
legend("topright",c("E(X)/a", "1-Fn","1-F"), col = c("black","red","green"),lty=c(1,2,3))

# utilisation de plot
x11()
par(mfrow=c(2,1))
plot(M[,1],M[,2],type="l",col="black")
points(M[,1],M[,3],type="l",col="red")
lines(M[,1],M[,4], col="green")
legend("topright",c("E(X)/a", "1-Fn","1-F"), col = c("black","red","green") )


plot(M[,1],M[,2],pch = 2,col="black")
points(M[,1],M[,3],pch = 4,col="red")
points(M[,1],M[,4], pch = 5,col="green")
legend("topright",c("E(X)/a", "1-Fn","1-F"),pch=c(2,4,5) ,col = c("black","red","green") )


