library(rjson)
library(plyr)
library(caTools)



#Obtain the data
a <- dir(recursive = 1)
a <- grep('json', a, value=TRUE)
sca <- grep('sca',dir(recursive = 1),value=TRUE)


ax <- grep("GroundTruthJSONlog.json",a,value=TRUE)
ay <- a[-grep("GroundTruthJSONlog.json",a,value=FALSE)]

y <- matrix()
nbEachJson <- matrix()
for(i in 1:length(ay))
{
  y <- c(y,fromJSON(paste("[",paste(readLines(ay[i]),collapse=","),"]")))
  nbEachJson[i] <- length(y)-1
}
y<-y[-1] ##to remove the first empty line


veremi <- matrix(nrow=length(y),ncol=40)
colnames(veremi) <-
  c(
    "type", "rcvTime","sendTime","sender","messageID","pos1","pos2","pos3","noise1","noise2",
    "noise3","spd1","spd2","spd3","spd_noise1","spd_noise2","spd_noise3","RSSI",
    "receiver","senderTitle","attackerType","seedset","start","attackProb","attackType"
  )


for (i in 1:nrow(veremi))
{
  if(is.null(y[[i]]$sendTime))
  {
    veremi[i,1] <- y[[i]]$type
    veremi[i,2] <- y[[i]]$rcvTime
    veremi[i,6] <- y[[i]]$pos[1]
    veremi[i,7] <- y[[i]]$pos[2]
    veremi[i,8] <- y[[i]]$pos[3]
    veremi[i,9] <- y[[i]]$noise[1]
    veremi[i,10] <- y[[i]]$noise[2]
    veremi[i,11] <- y[[i]]$noise[3]
    veremi[i,12] <- y[[i]]$spd[1]
    veremi[i,13] <- y[[i]]$spd[2]
    veremi[i,14] <- y[[i]]$spd[3]
    veremi[i,15] <- y[[i]]$spd_noise[1]
    veremi[i,16] <- y[[i]]$spd_noise[2]
    veremi[i,17] <- y[[i]]$spd_noise[3]
  }
  else{
    veremi[i,1] <- y[[i]]$type
    veremi[i,2] <- y[[i]]$rcvTime
    veremi[i,3] <- y[[i]]$sendTime
    veremi[i,4] <- y[[i]]$sender
    veremi[i,5] <- y[[i]]$messageID
    veremi[i,6] <- y[[i]]$pos[1]
    veremi[i,7] <- y[[i]]$pos[2]
    veremi[i,8] <- y[[i]]$pos[3]
    veremi[i,9] <- y[[i]]$pos_noise[1]
    veremi[i,10] <- y[[i]]$pos_noise[2]
    veremi[i,11] <- y[[i]]$pos_noise[3]
    veremi[i,12] <- y[[i]]$spd[1]
    veremi[i,13] <- y[[i]]$spd[2]
    veremi[i,14] <- y[[i]]$spd[3]
    veremi[i,15] <- y[[i]]$spd_noise[1]
    veremi[i,16] <- y[[i]]$spd_noise[2]
    veremi[i,17] <- y[[i]]$spd_noise[3]
    veremi[i,18] <- y[[i]]$RSSI
  }
}


for(j in 1:length(nbEachJson))
{
  receiver <- as.numeric(unlist(strsplit(unlist(strsplit(ay[j],"/"))[11],"-"))[2])
  attackerType <- as.numeric(unlist(strsplit(unlist(strsplit(ay[j],"/"))[11],"[A.]"))[2])
  senderTitle <- as.numeric(unlist(strsplit(unlist(strsplit(ay[j],"/"))[11],"-"))[3])
  if(j==1)
  {
    for(i in 1:nbEachJson[j])
    {
      veremi[i,19] <- receiver
      veremi[i,20] <- senderTitle
      veremi[i,21] <- attackerType
    }
  }
  else{
    for(i in (nbEachJson[j-1]+1):nbEachJson[j])
    {
      veremi[i,19] <- receiver
      veremi[i,20] <- senderTitle
      veremi[i,21] <- attackerType
    }
  }
}

for(j in 1:length(ay))
{
  for(k in 1:length(sca))
  {
    if(unlist(strsplit(ay,"/work"))[2*(j-1)+1] == unlist(strsplit(sca,"/work"))[2*(k-1)+1])
    {
      readSca <- read.csv(sca[k])
      seedset <- as.numeric(unlist(strsplit(as.character(readSca[16,1])," "))[3]) #seedset
      startt <- as.numeric(unlist(strsplit(as.character(readSca[17,1])," "))[3]) #start
      attprob <- as.numeric(unlist(strsplit(as.character(readSca[50,1])," "))[3]) #attackerprobability
      atttype <- as.numeric(unlist(strsplit(as.character(readSca[51,1])," "))[3]) #attackertype
      
      if(j==1)
      {
        for(i in 1:nbEachJson[j])
        {
          veremi[i,22] <- seedset
          veremi[i,23] <- startt
          veremi[i,24] <- attprob
          veremi[i,25] <- atttype
        }
      }
      else{
        for(i in (nbEachJson[j-1]+1):nbEachJson[j])
        {
          veremi[i,22] <- seedset
          veremi[i,23] <- startt
          veremi[i,24] <- attprob
          veremi[i,25] <- atttype
        }
      }
    }
  }
}


veremiDF <- as.data.frame(veremi)


bsm <- subset(veremiDF, veremiDF$type==3)

