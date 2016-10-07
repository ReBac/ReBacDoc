library(tm)

importDirectory = "/home/remi/TrainingData/"
exportDirectory = "/home/remi/SanitizeData/"
fileNames = paste0(importDirectory,list.files(importDirectory))
exportFileNames = paste0(exportDirectory,list.files(importDirectory))

rmv_links = function(x)
{
  y=gsub("(http).*","",x)
  y=gsub("(bit.ly).*","",y)
  y=gsub("(@).*","",y)
  y=gsub("’"," ",y)
  y=gsub("#"," ",y)
  y=gsub("'"," ",y)

  y=gsub("é","e",y) 
  y=gsub("è","e",y) 
  y=gsub("ê","e",y) 
  y=gsub("ë","e",y) 
  y=gsub("ù","u",y)
  y=gsub("ü","u",y) 
  y=gsub("à","a",y)
  y=gsub("ä","a",y)
  y=gsub("â","a",y)
  y=gsub("ô","o",y)
  y=gsub("î","i",y)
  y=gsub("ï","i",y)
  y=gsub("û","u",y)
  y=gsub("ç","c",y)
  y=gsub("œ","oe",y)
  
  y=removePunctuation(y)
  y=gsub("(eda0).*","",y)
  return(y)
}

remove_links <- (function(x) return(paste(lapply(strsplit(x,split=" "),rmv_links)[[1]],collapse=" ")))


for(i in 1:length(fileNames))
{
  text<-readLines(fileNames[i], encoding="UTF-8")
  text = iconv(text,"UTF-8","UTF-8",sub="")
  text = tolower(text)
  for(j in 1:length(text))  text[j] = remove_links(text[j])
  write(text,file=exportFileNames[i])
}
