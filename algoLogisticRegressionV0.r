###########################
#  Algo centre d'int?r?t  #
###########################

# Etapes de l'algo
# 0- Charger les donn?es. Trier selon la langue employ?e
# 1- Se donner un champ lexical autour du centre d'int?r?t recherch?.
# 2- Chercher tous les messages contenant au moins un mot du champ lexical.
# 3- Valider ? la main quels sont les messages qui rel?vent du centre d'int?r?t. (1:oui,2:non)
# 4- Ecrire un mod?le logit qui associe une pond?ration ? chaque mot du champ lexical et qui renvoie la probabilit? du message d'appartenir au centre d'int?r?t
# 5- Ecrire une fonction de vraisemblance qui calcule la distance entre les pr?visions calcul?es avec le set de pond?ration propos? et la r?alit? donn?e ? l'?tape 3.
# 6- Chercher le minimum de la fonction de vraisemblance avec une boucle MCMC qui cherche le meilleur set de param?tres, avec la moiti? du jeu de donn?es.
# 7- V?rifier la pertinence du set de param?tres obtenu avec la deuxi?me moiti? du jeu de donn?es.

# 0- Donn?es
dataTab = read.csv2("C:/Users/toshiba/Desktop/ExtrJSON/Output/data.csv",header=TRUE,encoding="UTF-8")
dat = dataTab[which(dataTab[,3]=="en"),c(1,2,4)] #On ne garde que les index et les messages
dat2 = dat[!duplicated(dat[,1]),] #Suppression des doublons
n = nrow(dat2)

# 1- Champ lexical
champ_lexical = c("going out","go out","night","beer","alcohol","party","fun","tonight","friends","buddies","good")

# 2- Recherche dans les messages
tabChampLexical = matrix(NA,nrow = n, ncol = length(champ_lexical))

for (i in 1:n)
{
	tabChampLexical[i,] = champ_lexical %in% strsplit(as.character(dat2[i,2])," ")[[1]]	
}
indexMsg=which(rowSums(tabChampLexical)>0)

m=length(indexMsg)

write.csv2(dat2[indexMsg,2],"C:/Users/toshiba/Desktop/ExtrJSON/Output/muscu.csv")

# 3- V?rif manuelle
IsMsgInHobby = read.csv2("/home/remi/Documents//Output/muscu.csv")[,1]

Training = IsMsgInHobby[1:floor(m/2)]
Verif = IsMsgInHobby[ceiling(m/2):m]

# 4- Mod?le LOGIT
probInteret = function(tabCL,param) exp(sum(param*tabCL))/(1+exp(sum(param*tabCL)))

# 5- Fonction de vraisemblance
vraisemblance = function(param)
{
	probs = apply(tabChampLexical[indexMsg,],1,probInteret,param)
	return(sum((probs[1:floor(m/2)]-Training)^2))
}

# 6- Boucle MCMC
	#Point de d?part
	k = length(champ_lexical)
	start_params = rep(1,k)
	tested_params = start_params
	start_vrais = vraisemblance(start_params)
	t_ap = 10000
	for (i in 1:t_ap)
	{
		next_params = rnorm(k,tested_params,(((t_ap-i)/t_ap)^3)*abs(tested_params))
		next_vrais = vraisemblance(next_params)
		accept = (next_vrais < start_vrais)
		if(accept)
		{
			tested_params = next_params
			start_vrais = next_vrais
		}
		if (i%%100 == 0) 	{print(i*100/t_ap);print(next_vrais)}
	}
	last_params = next_params
	
# 7- V?rification
	# On essaie de pr?dire si les phrases de la 2?me moiti? du set de donn?es sont en lien ou non avec le centre d'int?r?t
	# Si la proba pr?dite est <0.5, on consid?re qu'il n'y a pas int?r?t, si elle est >0.5 n consid?re qu'il y a int?r?t.
	# Puis on compte le pourcentage d'erreurs	

probss = round(apply(tabChampLexical[indexMsg,],1,probInteret,last_params),0)
errors = (probss[ceiling(m/2):m]-Verif)^2
msg_verif = dat2[indexMsg,2]
msg_verif = msg_verif[ceiling(m/2):m]
msg_mal_predits = msg_verif[which(errors==1)]
nb_errors = sum(errors)	
sum(errors)/length(errors)
sum(errors[which(Verif==1)])
K=matrix(c(champ_lexical,apply(tabChampLexical,2,sum)),byrow=TRUE,nrow=2)
rownames(K)=c("Word","Occurrences")
K