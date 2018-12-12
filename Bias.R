#For the first step, write code to create bias scores based on the Incongruent minus congruent formula -- one for happy faces and one for disgust (negative) faces. We can then expand the formula to compute other bias scores reported in the paper.
# We first need to take the average/mean RT for ALL incongruent trials of a given emotion (e.g., DIT) across the whole task for a given participant, and then use that mean to subtract the congruent RT (e.g., DCT) on EACH trial. So RT[incongruentMEAN] - RT[congruentTRIAL1 ... TRIAL2 etc.] -
#see the Evans and Britton paper I sent you earlier, page 2. You can use the Trial.Type variable to determine whether it's a congruent or incongruent trial, and then the DCT, DIT, HCT etc. variables for the actual RT. 
#Clear variable list
rm(list=ls())

#load in data
df <- read.csv(file = "C:/Users/jimmy/Documents/UCSD/Senior/TBI/501_502_503.csv", header=TRUE, sep =",")

#Clean up data
df <- subset(df, (Valence.Trial. == 'disgustneu' | Valence.Trial. == 'happyneu'| Valence.Trial. == 'neuneu' ), select=c(Subject, Session, Trial, Target.RT.Trial., Probe.Trial., Target.RESP.Trial., EmoPosition.Trial., ProbePosition.Trial.,  Valence.Trial.))
df<- transform(df, Subject=as.numeric(Subject), Session=as.numeric(Session), Trial=as.numeric(Trial), Target.RESP.Trial.=as.numeric(Target.RESP.Trial.), Target.RT.Trial.=as.numeric(Target.RT.Trial.))

#Change inaccurate data to R friendly NA
for (i in 1:(nrow(df)))
{
  if((df$Probe.Trial.[i]=="E" & df$Target.RESP.Trial.[i] == "2") | (df$Probe.Trial.[i] == "F" & df$Target.RESP.Trial.[i] == "1"))
  {
    df$Target.RT.Trial.[i] <- NA
  }
}

#Create incongruent and congruent trials
attach(df)
df$DCT = ifelse(((EmoPosition.Trial. == "top" & ProbePosition.Trial. == "top") | (EmoPosition.Trial. == "bottom" & ProbePosition.Trial. == "bottom")) & (Valence.Trial. == "disgustneu"), Target.RT.Trial., NA)
df$DIT = ifelse(((EmoPosition.Trial. == "top" & ProbePosition.Trial. == "bottom") | (EmoPosition.Trial. == "bottom" & ProbePosition.Trial. == "top")) & (Valence.Trial. == "disgustneu"), Target.RT.Trial., NA)
df$HCT = ifelse(((EmoPosition.Trial. == "top" & ProbePosition.Trial. == "top") | (EmoPosition.Trial. == "bottom" & ProbePosition.Trial. == "bottom")) & (Valence.Trial. == "happyneu"), Target.RT.Trial., NA)
df$HIT = ifelse(((EmoPosition.Trial. == "top" & ProbePosition.Trial. == "bottom") | (EmoPosition.Trial. == "bottom" & ProbePosition.Trial. == "top")) & (Valence.Trial. == "happyneu"), Target.RT.Trial., NA)
df$NTT = ifelse((ProbePosition.Trial. == "top" & Valence.Trial. == 'neuneu'), Target.RT.Trial., NA)
df$NBT = ifelse((ProbePosition.Trial. == "bottom" & Valence.Trial. == 'neuneu'), Target.RT.Trial., NA)

df$Trial.Type[((EmoPosition.Trial. == "top" & ProbePosition.Trial. == "top") | (EmoPosition.Trial. == "bottom" & ProbePosition.Trial. == "bottom")) & (Valence.Trial. == 'disgustneu')] <- "DCTrial"
df$Trial.Type[((EmoPosition.Trial. == "top" & ProbePosition.Trial. == "bottom") | (EmoPosition.Trial. == "bottom" & ProbePosition.Trial. == "top")) & (Valence.Trial. == 'disgustneu')] <- "DITrial"
df$Trial.Type[((EmoPosition.Trial. == "top" & ProbePosition.Trial. == "top") | (EmoPosition.Trial. == "bottom" & ProbePosition.Trial. == "bottom")) & (Valence.Trial. == 'happyneu')] <- "HCTrial"
df$Trial.Type[((EmoPosition.Trial. == "top" & ProbePosition.Trial. == "bottom") | (EmoPosition.Trial. == "bottom" & ProbePosition.Trial. == "top")) & (Valence.Trial. == 'happyneu')] <- "HITrial"
df$Trial.Type[(ProbePosition.Trial. == "top"  & Valence.Trial. == 'neuneu')] <- "NTTrial"
df$Trial.Type[(ProbePosition.Trial. == "bottom" & Valence.Trial. == 'neuneu')] <- "NBTrial"
detach(df)

df <- df
df$bias.score <- list(c(NA)) #initialize column for bias
subjects = split(df, df$Subject) #Splits data by subjects

for (s in seq_along(subjects))
{
  sub1 <- subjects[[s]]
  len = nrow(sub1)
  avgDIT <- mean(sub1$DIT,na.rm=TRUE)
  avgHIT <- mean(sub1$HIT,na.rm=TRUE)

  #Loop through Subject 501:503 and compute bias scores based on paper
  for (i in 1:len)
  { 
      if(!is.na(sub1$DCT[i]))
      {sub1$bias.score[[i]] <- avgDIT - sub1$DCT[i] 
      
      }
      else if(!is.na(sub1$HCT[i]))
      {sub1$bias.score[[i]] <- avgHIT - sub1$HIT[i]
      }
    } 
   
  
  #removes NAs
  for (i in 1:len)
  {
    if (!is.na(sub1$bias.score[i]))
    {sub1$bias.score[[i]] <- sub1$bias.score[[i]][!is.na(sub1$bias.score[[i]])]}
  }

  #sub1$bias.score <- 

assign(paste('Bias(RT[incon]-RT[cong]: Subject', sep="", s), as.data.frame(sub1))
  
}
