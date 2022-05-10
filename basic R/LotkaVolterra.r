#### Version Discrète Lotka-Volterra VS Modèle à temps discrétisé :
# perte de stabilité

# Thèmes et fonctions Abordés :
# - gestion mémoire 
# - changer répertoire courant
# - installer/charge librairies
# - vecteurs
# - fonctions
# - Listes
# - Attributs
# - matrices
# - for(...) if(...)
# - modulo
# - outils graphiques 2D : x11(),plot(),lines(),points(),image(),contour(),graphics.off(),
# - outils graphiques 3D : open3d(),surfaces3d()
# - animations
# - Sauvegarder/charger objets R, fichiers .csv,  figures 2D/3D

# Modèle :
# Xn+1 = Xn + Xn(a-bYn)Dt
# Yn+1 = Yn - Yn(c-dXn)Dt


rm(list=ls())
setwd("C:/Users/t.dumont/Documents/DOCUMENTS/PRESENTATIONS/Cours Introduction à R CIRM 09042014/R")

Recursion = function(X0,Y0,a,b,c,d,Dt)
{
	X1 = X0 + X0*(a-b*Y0)*Dt # Proies
	Y1 = Y0 - Y0*(c-d*X0)*Dt # Predateurs

	return(list(X1=X1,Y1=Y1))
}
 
# Paramètres
{ 
n  = 20000

a  = 1
b  = 1
c  = 1
d  = 1 
Dt = 0.01

X0 = 2
Y0 = 2
}

# Initialisation
X  = X0 # X[1] = X0
Y  = Y0 # Y[1] = Y0

# Récursion
for(i in 1:n)
{
	S= Recursion(X[i],Y[i],a,b,c,d,Dt)
	X[i+1] = S$X1
	Y[i+1] = S$Y1
}

# Changer le répertoire courant
setwd("C:/Users/t.dumont/Documents/DOCUMENTS/PRESENTATIONS/Cours Introduction à R CIRM 09042014/R/images")

x11() # Ouvrir un nouvelle fenêtre grahique
plot(X,Y,type="l",xlab="Proies",ylab="predateurs",main="Trajectoire") # Afficher X (proies) et Y (prédateurs)

# Enregistrer l'image dans le répertoire courant
dev.copy(jpeg,filename="Trajectoire.jpg"); 
dev.off ();

# Fermer toutes les fenêtres graphiques
graphics.off()

# H : intégrale première du mouvement
H = function(P)
{ 
	# P est une matrice. 
	# Chaque ligne de P est une coordonnée (x,y)
	if(is.vector(P))
	{
		P = matrix(P,1,2)
	}
	return(d*P[,1] + b*P[,2] - c*log(P[,1]) - a*log(P[,2]))
	# Retourne un vecteur de taille nrow(P)
}

N     = 1000
xymax = max(c(X,Y))

GridX = (1:N)*xymax/N
GridY =(1:N)*xymax/N

Grille = matrix(0,N*N,2)
Grille[,1]  = rep(GridX,N)
Grille[,2]  = rep(GridY,each=N)

Mvec     = H(Grille)
M     = matrix(Mvec,N,N,byrow=TRUE)

# Différents affichage de la fonction de deux variables H :
# Carte de chaleur
{
x11()
image(GridX,GridY,M)
dev.copy(jpeg,filename="heatMapH.jpg"); 
dev.off ();
}

# Lignes de niveaux
{ 
x11()
contour(GridX,GridY,M,xlim=c(0,max(X)),ylim=c(0,max(Y)))
lines(X,Y,type="l",col="red") 
dev.copy(jpeg,filename="lignesNiveauxH.jpg"); 
dev.off ();
}

# Affichage 3D
{ 
library(rgl) 

open3d()

smallX = GridX[(0:100)*10]
smallY = GridY[(0:100)*10]
smallM = M[(0:100)*10,(0:100)*10]

surface3d(smallX,smallY,smallM, emission = "blue", specular = "white", shininess = 50, alpha = 0.6, front = "line", back = "fill", size = 2) 
axes3d( )
title3d(main = "Integrale Premiere ", xlab = "x", ylab = "y",zlab = "z") 

open3d() 
zlim <- range(smallM)
zlen <- floor((zlim[2] - zlim[1] + 1 )*10)
colorlut <- heat.colors(zlen) # height color lookup table
col <- colorlut[ floor((smallM-zlim[1]+1)*10) ] # assign colors to heights for each point


surface3d(smallX,smallY,smallM, color=col, back="fill")
axes3d( )
title3d(main = "Integrale Premiere ", xlab = "x", ylab = "y",zlab = "z") 
snapshot3d("IntPrem.png", fmt="png") 
}

# Animation : évolution du nombre de proies et de prédateurs
# install.packages("animation")

setwd("C:/Users/t.dumont/Documents/DOCUMENTS/PRESENTATIONS/Cours Introduction à R CIRM 09042014/R/Animations")
library(animation)


LotkaVolteraDiscret=function(ani.pause=F,nMax=n) # Fonction qui affiche la trajectoire tous les n=100 et les lignes de niveaux de la fonction H
{

	for(i in 1:min(n,nMax))
	{
		if(i%%100 ==0){
			contour(GridX ,GridY,M,xlim=c(0,max(X)),ylim=c(0,max(Y)),xlab = "Proies",ylab="predateurs",main ="Lotka Volterra Discret")
			lines(X[1:i],Y[1:i],col="red") 
			points(X[i],Y[i],pch=18,col="blue")
			if (ani.pause) ani.pause()
		}
	}
}
ani.pause = 0.15
x11()
LotkaVolteraDiscret(ani.pause=TRUE, nMax = 1000 ) 
# L'affichage est long à s'effectuer : le rendu est saccadé 

# Utilisation de la librairie "animation"
ani.options(outdir=getwd(),htmlfile="LV.html")
ani.start() 
LotkaVolteraDiscret( ) 
ani.stop()





