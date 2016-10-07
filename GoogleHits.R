#Fonction qui renvoie le nombre de résultats estimé d'une requête google 
# La requête doit être pré concaténée
#Exemples : 
#   GoogleHits("+muscu+fitness+Paris")
#   GoogleHits("R%Ssoftware%SProgramming")
#   GoogleHits("My%BLittle%BPuny")
# Le plus restrictif étant
#  GoogleHits("+%BLooking+%BFor+%BSomething")
#Impossible à utiliser en masse, Google nous considère comme un robot.
GoogleHits <- function(reqWords)
  {
    require(RCurl)
    url <- paste("https://www.google.com/search?q=\"",
                 reqWords, "\"", sep = "")
    script <- getURL(url, followlocation = TRUE)

    # start extraction at indicator word position
    posExtractStart <- gregexpr("resultStats", script,
                                fixed = TRUE)[[1]]
    # extract string of 40 chracters length
    stringExtract <- substring(script, first=posExtractStart,
                               last = posExtractStart + 40)
    matchCount <- as.numeric(gsub("[^0-9]", "", stringExtract[2]))
    
    return(matchCount)
    }
