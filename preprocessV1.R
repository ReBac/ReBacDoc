#Preprocessing français#

#Retire la ponctuation, les smileys, les liens https
#Extrait les quote @ et les hashtags #
#Enlève les mots d'une liste stop-words
library(stringr)

#Stop words français
stop_words_fr = scan("~/Documents/stopwordsfr.txt",what="character",sep=",",comment.char = "#")
stop_words_fr=gsub(pattern="\t",replacement="",stop_words_fr)
stop_words_fr=stop_words_fr[which(stop_words_fr!="")]

preprocess_fr = function(tweets.data.frame,interest)
{
  n = length(tweets.data.frame$text)
  tweets.data.frame$text <- iconv(tweets.data.frame$text, 'UTF-8', 'UTF-8',"")
  #Retrait ponctuation 
  tweets.data.frame$text = gsub(pattern=".",replacement="",x=tweets.data.frame$text,fixed=TRUE)
  tweets.data.frame$text = gsub(pattern="-",replacement=" ",x=tweets.data.frame$text,fixed=TRUE)
  tweets.data.frame$text = gsub(pattern="+",replacement="",x=tweets.data.frame$text,fixed=TRUE)
  tweets.data.frame$text = gsub(pattern="*",replacement="",x=tweets.data.frame$text,fixed=TRUE)
  tweets.data.frame$text = gsub(pattern="=",replacement="",x=tweets.data.frame$text,fixed=TRUE)
  tweets.data.frame$text = gsub(pattern="!",replacement="",x=tweets.data.frame$text,fixed=TRUE)
  tweets.data.frame$text = gsub(pattern=":",replacement="",x=tweets.data.frame$text,fixed=TRUE)
  tweets.data.frame$text = gsub(pattern=";",replacement="",x=tweets.data.frame$text,fixed=TRUE)
  tweets.data.frame$text = gsub(pattern="?",replacement="",x=tweets.data.frame$text,fixed=TRUE)
  tweets.data.frame$text = gsub(pattern=",",replacement="",x=tweets.data.frame$text,fixed=TRUE)
  tweets.data.frame$text = gsub(pattern="\n",replacement="",x=tweets.data.frame$text)
  tweets.data.frame$text = gsub(pattern="\"",replacement="",x=tweets.data.frame$text)
  tweets.data.frame$text = gsub(pattern="'",replacement=" ",x=tweets.data.frame$text,fixed=TRUE)
  tweets.data.frame$text = gsub(pattern="(",replacement="",x=tweets.data.frame$text,fixed=TRUE)
  tweets.data.frame$text = gsub(pattern=")",replacement="",x=tweets.data.frame$text,fixed=TRUE)
  tweets.data.frame$text = gsub(pattern="/",replacement=" ",x=tweets.data.frame$text,fixed=TRUE)
  tweets.data.frame$text = gsub(pattern="&",replacement="",x=tweets.data.frame$text,fixed=TRUE)
  tweets.data.frame$text = gsub(pattern="…",replacement=" ",x=tweets.data.frame$text,fixed=TRUE)
  tweets.data.frame$text = gsub(pattern="|",replacement=" ",x=tweets.data.frame$text,fixed=TRUE)
  tweets.data.frame$text = gsub(pattern="►",replacement=" ",x=tweets.data.frame$text,fixed=TRUE)
  tweets.data.frame$text = gsub(pattern="’",replacement=" ",x=tweets.data.frame$text,fixed=TRUE)
  tweets.data.frame$text = gsub(pattern="#",replacement=" ",x=tweets.data.frame$text,fixed=TRUE)
  tweets.data.frame$text = gsub(pattern="«",replacement=" ",x=tweets.data.frame$text,fixed=TRUE)
  
  words=list(sentence = list(),interest=rep(interest,n),selfDictionary=c())
  
  for (i in 1:n)
  {
    decByWord = strsplit(tweets.data.frame$text[i],split=" ")[[1]]
    k = length(decByWord)
    
    indexWordsToRemove = c()
    for(word in 1:k)
    {
      
      #Retrait des liens
      if(str_detect(decByWord[word],pattern="http")) {indexWordsToRemove = c(indexWordsToRemove,word)}
      if(str_detect(decByWord[word],pattern="bitly")) {indexWordsToRemove = c(indexWordsToRemove,word)}
      #Retrait et @
      if(str_detect(decByWord[word],pattern="@")) 
      {
        indexWordsToRemove = c(indexWordsToRemove,word)
      } 

      #Retrait des stop words
      if(tolower(decByWord[word]) %in% stop_words_fr) {indexWordsToRemove = c(indexWordsToRemove,word)}
      
      #Retrait des mots de longueur <4
      if(nchar(decByWord[word])<4) {indexWordsToRemove = c(indexWordsToRemove,word)}
    }
    if (length(indexWordsToRemove)>0){
      words$sentence[[i]]=c(tolower(decByWord[-indexWordsToRemove]))
      words$selfDictionary = c(words$selfDictionary,tolower(decByWord[-indexWordsToRemove])) 
    }else {
      words$sentence[[i]]=c(tolower(decByWord))
      words$selfDictionary = c(words$selfDictionary,tolower(decByWord))
    }
  }
  return(words)
}
