#Recherche et stockage de tweets à propos d'un centre d'intérêt indépendemment d'un utilisateur#

#Imports
library(httr)
library(twitteR)

write_tweets = function(interest)
{
  tw = searchTwitter(interest,n=3000,lang="fr",resultType="mixed",retryOnRateLimit=200)
  texts = c()
  for(i in 1:length(tw)) texts = c(texts,tw[[i]]$text)
  texts = unique(gsub("\n"," ",texts))
  texts = iconv(texts,from="UTF-8",to="UTF-8",sub="")
  filename = paste0("/home/remi/TrainingData/",paste0(interest,".txt"))
  write(texts,filename)
}

#Daily life
write_tweets("étudiant")
write_tweets("LGBT")
write_tweets("handicap")
write_tweets("famille")
write_tweets("enfants")
write_tweets("caritatif")
write_tweets("association")
#Animaux
write_tweets("chat")
write_tweets("chien")
write_tweets("cheval")

#Lifestyle
write_tweets("végétarien")
write_tweets("manger+bio")
write_tweets("mode")
write_tweets("luxe")
write_tweets("gothique")
write_tweets("moto")
write_tweets("écolo")
#Sport
write_tweets("foot")
write_tweets("basket")
write_tweets("tennis")
write_tweets("natation")
write_tweets("course voiture")
write_tweets("course moto")
write_tweets("baseball")
write_tweets("golf")
write_tweets("ping pong")
write_tweets("e-sport")
write_tweets("musculation")
write_tweets("fitness")
write_tweets("course cheval")
write_tweets("danse classique")
write_tweets("danse jazz")
write_tweets("danse rock")
write_tweets("danse country")
write_tweets("danse zumba")
write_tweets("équitation")
write_tweets("athlétisme")
write_tweets("voile bateau")
write_tweets("handball")
write_tweets("volley")

write_tweets("cinéma OR film OR série")
write_tweets("buzz OR insolite OR biazrre")
write_tweets("mystère OR paranormal")
write_tweets("bière OR vin OR alcool OR cidre OR champagne OR cocktail")
write_tweets("cinéma")
