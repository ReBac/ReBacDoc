#Extraction de tweets#


#Imports
library(httr)
library(twitteR)

#Doc sur la classe user : http://127.0.0.1:24481/library/twitteR/html/user-class.html
#Doc sur la classe status : http://127.0.0.1:24481/library/twitteR/html/status-class.html

process_tweets = function(user,nb_tweets=3200,interest)
{
  userInfo = getUser(user)  #Wrap Up de l'API GET user/show
  userTimeline = userTimeline(user,n=nb_tweets,includeRts=TRUE) #Wrap Up de l'API GET statuses/user_timeline
  
  userTw = data.frame() #Placement de tous les tweets dans une seule dataframe
  for (i in 1:length(userTimeline))
  {
    userTw = rbind(userTw,userTimeline[[i]]$toDataFrame())
  }
  
  #Choix de la langue pour les stop_words. A discuter (tweets "bilingues" courants VS perte de temps si consultations d'une grosse table de stop_words multilingue)
  
  return(preprocess_fr(userTw,interest))

 }

stock_tweets = function(user,filename,nb_tweets=3200)
{
  userInfo = getUser(user)  #Wrap Up de l'API GET user/show
  userTimeline = userTimeline(user,n=nb_tweets,includeRts=TRUE) #Wrap Up de l'API GET statuses/user_timeline
  
  userTw = data.frame() #Placement de tous les tweets dans une seule dataframe
  for (i in 1:length(userTimeline))
  {
    userTw = rbind(userTw,userTimeline[[i]]$toDataFrame())
  }
    write(userTw$text,file=filename)
  return()
  
}
  


stock_interests = function(list_profiles,interest,fileRepertory,typeKey=1)
{
  filenames = paste0(1:10,".txt")
  filenames = paste0(interest,filenames)
  filenames = paste0(fileRepertory,filenames)
  
  
  if(typeKey==1){setup_twitter_oauth(consumer_key=key,consumer_secret=secret,access_token=access_Token,access_secret=access_Token_Secret)
  }else{setup_twitter_oauth(consumer_key=key3,consumer_secret=secret3,access_token=access_Token3,access_secret=access_Token_Secret3)}
  stock_tweets(user=list_profiles[1],filename = filenames[1])
  stock_tweets(user=list_profiles[2],filename = filenames[2])
  stock_tweets(user=list_profiles[3],filename = filenames[3])
  stock_tweets(user=list_profiles[4],filename = filenames[4])
  stock_tweets(user=list_profiles[5],filename = filenames[5])
  
  if(typeKey==1){setup_twitter_oauth(consumer_key=key2,consumer_secret=secret2,access_token=access_Token2,access_secret=access_Token_Secret2)
  }else{setup_twitter_oauth(consumer_key=key4,consumer_secret=secret4,access_token=access_Token4,access_secret=access_Token_Secret4)}
  stock_tweets(user=list_profiles[6],filename = filenames[6])
  stock_tweets(user=list_profiles[7],filename = filenames[7])
  stock_tweets(user=list_profiles[8],filename = filenames[8])
  stock_tweets(user=list_profiles[9],filename = filenames[9])
  stock_tweets(user=list_profiles[10],filename = filenames[10])
}

