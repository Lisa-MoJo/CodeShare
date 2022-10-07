#################################################################################
# FLF2_Cleanup.R
# Revised from vowel_cleanup2.R
# Script for cleaning up tokens where the F2 was mistakenly recorded as F3 
# Code for IY1, IH1, EY1, EH1, AE1, UW1, UH1 AH1, OW1
# NUMBERS ADJUSTED AFTER TWO TESTS WITH THE TEEN LANGUAGE AND IDENTITY DATASET. 
# THESE NUMBERS (LOWERBOUND, UPPERBOUND, DROP VALUE)
# WERE FOUND TO BE APPROPRIATE FOR VOWELS BEFORE OBSTRUENTS.
# DIFFERENT RANGES ARE NEEDED FOR VOWELS BEFORE L AND NASALS
# OTHER SETTINGS MAY BE MORE APPROPRIATE FOR DIFFERENT DATASETS
#################################################################################


library(tidyverse)

#Read in data. 
V123 <- read.csv("E:\\TokenAnalysis\\Data\\V123.csv")

#Create a log file for recording actions
Vlog <- file("E:\\TokenAnalysis\\Data\\vFix.log", open="a")

#Exclude stopwords, if desired
#stopwords <- read.csv("E:\\TokenAnalysis\\Data\\StopWords.txt")

print("BEGIN")
cat(paste("BEGIN PROCESSING: ", Sys.time()), file = Vlog, sep="\n")

vsub <- V123 %>% #Make a copy of the vowel dataframe where substitutions will be made
    #Filter out place name words from word list, since these are analyzed differently
    #Filer out diphthongs
    #Find words with vowels before obstruents
    filter(!word %in% c("colorado",
                      "hawaii",
                      "nevada",
                      "really",
                      "samoa",
                      "tonga"),
         !vowel %in% c("AY1", "AW1", "OY1", "ER1"),
         !nxtPh %in% c("L", "R", "M", "N", "NG", "sil", "sp"))

#Create a dataframe for logging problems found and actions taken
sublist <- data.frame(speakerid=character(),
                      word=character(),
                      filename=character(),
                      rating=factor(),
                      vowel=factor(),
                      problem=character(),
                      measurepoint=character(),
                      action=character())


##################################################################################
#Correcting IY1 (substitute F3 for F2)

#Set lower bound, upper bound, and drop level based on data observation and tests
F2lowbound <- 1719 
F2upbound <- 3207
dropTest <- 300

#Start counters
count<-0 #For counting tokens of the vowel
count2<-0 #For counting substitutions of F3 for F2
count3 <- 0 #For counting low F2 measurements where a substitution is not made


#Cycle through all tokens in list
for(i in 1:nrow(vsub))
{
  filename <- vsub$filename[[i]]
  word <- vsub$word[[i]]
  
  #Check to see if this is a token of IY1
  if(vsub$vowel[[i]] == "IY1")
  {
    #print(paste0("Row",i," File=",vsub$filename[[i]]))
    count<-count+1
    
    #Check to see if the F2 measurement at 20% is below a stated threshold
    if(vsub$F2_20[[i]] < F2lowbound)
    {
      # print(paste0("Row ",i," OUT OF BOUNDS"))
      # count2<-count2+1
      
      #Check to see if the F3 measurement at 20% is within a possible F2 range
      if(vsub$F3_20[[i]] >=F2lowbound && vsub$F3_20[[i]] < F2upbound )
      {
        vsub$F2_20[[i]] <- vsub$F3_20[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_20[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " IY: used F3_20 measurement for F2_20"))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Low F2",
          measurepoint = "F2_20",
          action = "F3 substituted for F2")
        
        #add new row to log
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
        #add to count of substitutions
        count2<-count2+1
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " IY: Low F2_20. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Low F2",
          measurepoint = "F2_20",
          action = "No substitutions made.")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_30
    #Check to see if the F2 measurement at 30% shows a large drop from the F2 measurement at 20%
    if(vsub$F2_20[[i]] - vsub$F2_30[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 30% is within a possible F2 range
      if(vsub$F3_30[[i]] >=F2lowbound && vsub$F3_30[[i]] < F2upbound)
      {
        vsub$F2_30[[i]] <- vsub$F3_30[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_30[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " IY: used F3_30 measurement for F2_30"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_30",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 30%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_30",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
      
    }
    
    #### Move on to F2_40    
    #Check to see if the F2 measurement at 40% shows a large drop from the F2 measurement at 30%
    if(vsub$F2_30[[i]] - vsub$F2_40[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 40% is within a possible F2 range
      if(vsub$F3_40[[i]] >=F2lowbound && vsub$F3_40[[i]] < F2upbound)
      {
        vsub$F2_40[[i]] <- vsub$F3_40[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_40[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " IY: used F3_40 measurement for F2_40"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_40",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 40%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_40",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_50
    #Check to see if the F2 measurement at 50% shows a large drop from the F2 measurement at 40%
    if(vsub$F2_40[[i]] - vsub$F2_50[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 50% is within a possible F2 range
      if(vsub$F3_50[[i]] >=F2lowbound && vsub$F3_50[[i]] < F2upbound)
      {
        vsub$F2_50[[i]] <- vsub$F3_50[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_50[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " IY: used F3_50 measurement for F2_50"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_50",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 50%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_50",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_60
    #Check to see if the F2 measurement at 60% shows a large drop from the F2 measurement at 50%
    if(vsub$F2_50[[i]] - vsub$F2_60[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 60% is within a possible F2 range
      if(vsub$F3_60[[i]] >=F2lowbound && vsub$F3_60[[i]] < F2upbound)
      {
        vsub$F2_60[[i]] <- vsub$F3_60[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_60[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " IY: used F3_60 measurement for F2_60"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_60",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 60%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_60",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_70
    #Check to see if the F2 measurement at 70% shows a large drop from the F2 measurement at 60%
    if(vsub$F2_60[[i]] - vsub$F2_70[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 70% is within a possible F2 range
      if(vsub$F3_70[[i]] >=F2lowbound && vsub$F3_70[[i]] < F2upbound)
      {
        vsub$F2_70[[i]] <- vsub$F3_70[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_70[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " IY: used F3_70 measurement for F2_70"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_70",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 70%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_70",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    
    ###Move on to F2_80
    #Check to see if the F2 measurement at 80% shows a large drop from the F2 measurement at 70%
    if(vsub$F2_70[[i]] - vsub$F2_80[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 80% is within a possible F2 range
      if(vsub$F3_80[[i]] >=F2lowbound && vsub$F3_80[[i]] < F2upbound)
      {
        vsub$F2_80[[i]] <- vsub$F3_80[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_80[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " IY: used F3_80 measurement for F2_80"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_80",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 80%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_80",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    
  }
}
print(paste0("Total IY1=",count))
cat(paste0("Total IY tokens=",count), file = Vlog, sep="\n")

print(paste0("Total SUBSTITUTED=",count2))
cat(paste0("Total IY SUBSTITUTIONS=",count2), file = Vlog, sep="\n")

print(paste0("Total low F2 without substitution=", count3))
cat(paste0("Total IY low F2 without substitutions=",count3), file = Vlog, sep="\n")

##############################################################################

#Correcting IH1 (substitute F3 for F2)

F2lowbound <- 1310 
F2upbound <- 2666
dropTest <- 300

count<-0 #For counting tokens of the vowel
count2<-0 #For counting substitutions of F3 for F2
count3 <- 0 #For counting low F2 measurements where a substitution is not made


for(i in 1:nrow(vsub))
{
  filename <- vsub$filename[[i]]
  word <- vsub$word[[i]]
  
  #Check to see if this is a token of IH1
  if(vsub$vowel[[i]] == "IH1")
  {
    #print(paste0("Row",i," File=",vsub$filename[[i]]))
    count<-count+1
    
    #Check to see if the F2 measurement at 20% is below a stated threshold
    if(vsub$F2_20[[i]] < F2lowbound)
    {
      # print(paste0("Row ",i," OUT OF BOUNDS"))
      # count2<-count2+1
      
      #Check to see if the F3 measurement at 20% is within a possible F2 range
      if(vsub$F3_20[[i]] >=F2lowbound && vsub$F3_20[[i]] < F2upbound )
      {
        vsub$F2_20[[i]] <- vsub$F3_20[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_20[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " IH: used F3_20 measurement for F2_20"))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Low F2",
          measurepoint = "F2_20",
          action = "F3 substituted for F2")
        
        #add new row to log
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
        #add to count of substitutions
        count2<-count2+1
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " IY: Low F2_20. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Low F2",
          measurepoint = "F2_20",
          action = "No substitutions made.")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_30
    #Check to see if the F2 measurement at 30% shows a large drop from the F2 measurement at 20%
    if(vsub$F2_20[[i]] - vsub$F2_30[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 30% is within a possible F2 range
      if(vsub$F3_30[[i]] >=F2lowbound && vsub$F3_30[[i]] < F2upbound)
      {
        vsub$F2_30[[i]] <- vsub$F3_30[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_30[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " IH: used F3_30 measurement for F2_30"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_30",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 30%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_30",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
      
    }
    
    #### Move on to F2_40    
    #Check to see if the F2 measurement at 40% shows a large drop from the F2 measurement at 30%
    if(vsub$F2_30[[i]] - vsub$F2_40[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 40% is within a possible F2 range
      if(vsub$F3_40[[i]] >=F2lowbound && vsub$F3_40[[i]] < F2upbound)
      {
        vsub$F2_40[[i]] <- vsub$F3_40[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_40[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " IH: used F3_40 measurement for F2_40"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_40",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 40%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_40",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_50
    #Check to see if the F2 measurement at 50% shows a large drop from the F2 measurement at 40%
    if(vsub$F2_40[[i]] - vsub$F2_50[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 50% is within a possible F2 range
      if(vsub$F3_50[[i]] >=F2lowbound && vsub$F3_50[[i]] < F2upbound)
      {
        vsub$F2_50[[i]] <- vsub$F3_50[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_50[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " IH: used F3_50 measurement for F2_50"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_50",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 50%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_50",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_60
    #Check to see if the F2 measurement at 60% shows a large drop from the F2 measurement at 50%
    if(vsub$F2_50[[i]] - vsub$F2_60[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 60% is within a possible F2 range
      if(vsub$F3_60[[i]] >=F2lowbound && vsub$F3_60[[i]] < F2upbound)
      {
        vsub$F2_60[[i]] <- vsub$F3_60[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_60[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " IH: used F3_60 measurement for F2_60"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_60",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 60%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_60",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_70
    #Check to see if the F2 measurement at 70% shows a large drop from the F2 measurement at 60%
    if(vsub$F2_60[[i]] - vsub$F2_70[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 70% is within a possible F2 range
      if(vsub$F3_70[[i]] >=F2lowbound && vsub$F3_70[[i]] < F2upbound)
      {
        vsub$F2_70[[i]] <- vsub$F3_70[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_70[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " IH: used F3_70 measurement for F2_70"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_70",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 70%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_70",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    
    ###Move on to F2_80
    #Check to see if the F2 measurement at 80% shows a large drop from the F2 measurement at 70%
    if(vsub$F2_70[[i]] - vsub$F2_80[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 80% is within a possible F2 range
      if(vsub$F3_80[[i]] >=F2lowbound && vsub$F3_80[[i]] < F2upbound)
      {
        vsub$F2_80[[i]] <- vsub$F3_80[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_80[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " IH: used F3_80 measurement for F2_80"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_80",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 80%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_80",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    
  }
}
print(paste0("Total IH1=",count))
cat(paste0("Total IH tokens=",count), file = Vlog, sep="\n")

print(paste0("Total SUBSTITUTED=",count2))
cat(paste0("Total IH SUBSTITUTIONS=",count2), file = Vlog, sep="\n")

print(paste0("Total low F2 without substitution=", count3))
cat(paste0("Total IH low F2 without substitutions=",count3), file = Vlog, sep="\n")


##############################################################################
# Correcting EY (substitute F3 for F2)


count<-0 #For counting tokens of the vowel
count2<-0 #For counting substitutions of F3 for F2
count3 <- 0 #For counting low F2 measurements where a substitution is not made


F2lowbound <- 1300 
F2upbound <- 2950
dropTest <- 300


for(i in 1:nrow(vsub))
{
  filename <- vsub$filename[[i]]
  word <- vsub$word[[i]]
  
  #Check to see if this is a token of EY1
  if(vsub$vowel[[i]] == "EY1")
  {
    #print(paste0("Row",i," File=",vsub$filename[[i]]))
    count<-count+1
    
    #Check to see if the F2 measurement at 20% is below a stated threshold
    if(vsub$F2_20[[i]] < F2lowbound)
    {
      # print(paste0("Row ",i," OUT OF BOUNDS"))
      # count2<-count2+1
      
      #Check to see if the F3 measurement at 20% is within a possible F2 range
      if(vsub$F3_20[[i]] >=F2lowbound && vsub$F3_20[[i]] < F2upbound )
      {
        vsub$F2_20[[i]] <- vsub$F3_20[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_20[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " EY: used F3_20 measurement for F2_20"))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Low F2",
          measurepoint = "F2_20",
          action = "F3 substituted for F2")
        
        #add new row to log
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
        #add to count of substitutions
        count2<-count2+1
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " EY: Low F2_20. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Low F2",
          measurepoint = "F2_20",
          action = "No substitutions made.")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_30
    #Check to see if the F2 measurement at 30% shows a large drop from the F2 measurement at 20%
    if(vsub$F2_20[[i]] - vsub$F2_30[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 30% is within a possible F2 range
      if(vsub$F3_30[[i]] >=F2lowbound && vsub$F3_30[[i]] < F2upbound)
      {
        vsub$F2_30[[i]] <- vsub$F3_30[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_30[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " EY: used F3_30 measurement for F2_30"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_30",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 30%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_30",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
      
    }
    
    #### Move on to F2_40    
    #Check to see if the F2 measurement at 40% shows a large drop from the F2 measurement at 30%
    if(vsub$F2_30[[i]] - vsub$F2_40[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 40% is within a possible F2 range
      if(vsub$F3_40[[i]] >=F2lowbound && vsub$F3_40[[i]] < F2upbound)
      {
        vsub$F2_40[[i]] <- vsub$F3_40[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_40[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " EY: used F3_40 measurement for F2_40"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_40",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 40%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_40",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_50
    #Check to see if the F2 measurement at 50% shows a large drop from the F2 measurement at 40%
    if(vsub$F2_40[[i]] - vsub$F2_50[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 50% is within a possible F2 range
      if(vsub$F3_50[[i]] >=F2lowbound && vsub$F3_50[[i]] < F2upbound)
      {
        vsub$F2_50[[i]] <- vsub$F3_50[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_50[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " EY: used F3_50 measurement for F2_50"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_50",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 50%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_50",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_60
    #Check to see if the F2 measurement at 60% shows a large drop from the F2 measurement at 50%
    if(vsub$F2_50[[i]] - vsub$F2_60[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 60% is within a possible F2 range
      if(vsub$F3_60[[i]] >=F2lowbound && vsub$F3_60[[i]] < F2upbound)
      {
        vsub$F2_60[[i]] <- vsub$F3_60[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_60[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " EY: used F3_60 measurement for F2_60"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_60",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 60%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_60",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_70
    #Check to see if the F2 measurement at 70% shows a large drop from the F2 measurement at 60%
    if(vsub$F2_60[[i]] - vsub$F2_70[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 70% is within a possible F2 range
      if(vsub$F3_70[[i]] >=F2lowbound && vsub$F3_70[[i]] < F2upbound)
      {
        vsub$F2_70[[i]] <- vsub$F3_70[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_70[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " EY: used F3_70 measurement for F2_70"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_70",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 70%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_70",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    
    ###Move on to F2_80
    #Check to see if the F2 measurement at 80% shows a large drop from the F2 measurement at 70%
    if(vsub$F2_70[[i]] - vsub$F2_80[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 80% is within a possible F2 range
      if(vsub$F3_80[[i]] >=F2lowbound && vsub$F3_80[[i]] < F2upbound)
      {
        vsub$F2_80[[i]] <- vsub$F3_80[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_80[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " EY: used F3_80 measurement for F2_80"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_80",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 80%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_80",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    
  }
}
#print(paste0("Total EY1=",count))
#print(paste0("Total SUBSTITUTED=",count2))
#print(paste0("Total low F2 without substitution=", count3))

print(paste0("Total EY1=",count))
cat(paste0("Total EY tokens=",count), file = Vlog, sep="\n")

print(paste0("Total SUBSTITUTED=",count2))
cat(paste0("Total EY SUBSTITUTIONS=",count2), file = Vlog, sep="\n")

print(paste0("Total low F2 without substitution=", count3))
cat(paste0("Total EY low F2 without substitutions=",count3), file = Vlog, sep="\n")


###########################################################################

#Correcting EH

count<-0 #For counting tokens of the vowel
count2<-0 #For counting substitutions of F3 for F2
count3 <- 0 #For counting low F2 measurements where a substitution is not made


F2lowbound <- 1056 
F2upbound <- 2527
dropTest <- 500


for(i in 1:nrow(vsub))
{
  filename <- vsub$filename[[i]]
  word <- vsub$word[[i]]
  
  #Check to see if this is a token of EH1
  if(vsub$vowel[[i]] == "EH1")
  {
    #print(paste0("Row",i," File=",vsub$filename[[i]]))
    count<-count+1
    
    #Check to see if the F2 measurement at 20% is below a stated threshold
    if(vsub$F2_20[[i]] < F2lowbound)
    {
      # print(paste0("Row ",i," OUT OF BOUNDS"))
      # count2<-count2+1
      
      #Check to see if the F3 measurement at 20% is within a possible F2 range
      if(vsub$F3_20[[i]] >=F2lowbound && vsub$F3_20[[i]] < F2upbound )
      {
        vsub$F2_20[[i]] <- vsub$F3_20[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_20[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " EH: used F3_20 measurement for F2_20"))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Low F2",
          measurepoint = "F2_20",
          action = "F3 substituted for F2")
        
        #add new row to log
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
        #add to count of substitutions
        count2<-count2+1
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " EH: Low F2_20. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Low F2",
          measurepoint = "F2_20",
          action = "No substitutions made.")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_30
    #Check to see if the F2 measurement at 30% shows a large drop from the F2 measurement at 20%
    if(vsub$F2_20[[i]] - vsub$F2_30[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 30% is within a possible F2 range
      if(vsub$F3_30[[i]] >=F2lowbound && vsub$F3_30[[i]] < F2upbound)
      {
        vsub$F2_30[[i]] <- vsub$F3_30[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_30[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " EH: used F3_30 measurement for F2_30"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_30",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 30%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_30",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
      
    }
    
    #### Move on to F2_40    
    #Check to see if the F2 measurement at 40% shows a large drop from the F2 measurement at 30%
    if(vsub$F2_30[[i]] - vsub$F2_40[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 40% is within a possible F2 range
      if(vsub$F3_40[[i]] >=F2lowbound && vsub$F3_40[[i]] < F2upbound)
      {
        vsub$F2_40[[i]] <- vsub$F3_40[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_40[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " EH: used F3_40 measurement for F2_40"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_40",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 40%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_40",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_50
    #Check to see if the F2 measurement at 50% shows a large drop from the F2 measurement at 40%
    if(vsub$F2_40[[i]] - vsub$F2_50[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 50% is within a possible F2 range
      if(vsub$F3_50[[i]] >=F2lowbound && vsub$F3_50[[i]] < F2upbound)
      {
        vsub$F2_50[[i]] <- vsub$F3_50[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_50[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " EH: used F3_50 measurement for F2_50"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_50",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 50%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_50",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_60
    #Check to see if the F2 measurement at 60% shows a large drop from the F2 measurement at 50%
    if(vsub$F2_50[[i]] - vsub$F2_60[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 60% is within a possible F2 range
      if(vsub$F3_60[[i]] >=F2lowbound && vsub$F3_60[[i]] < F2upbound)
      {
        vsub$F2_60[[i]] <- vsub$F3_60[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_60[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " EH: used F3_60 measurement for F2_60"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_60",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 60%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_60",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_70
    #Check to see if the F2 measurement at 70% shows a large drop from the F2 measurement at 60%
    if(vsub$F2_60[[i]] - vsub$F2_70[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 70% is within a possible F2 range
      if(vsub$F3_70[[i]] >=F2lowbound && vsub$F3_70[[i]] < F2upbound)
      {
        vsub$F2_70[[i]] <- vsub$F3_70[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_70[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " EH: used F3_70 measurement for F2_70"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_70",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 70%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_70",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    
    ###Move on to F2_80
    #Check to see if the F2 measurement at 80% shows a large drop from the F2 measurement at 70%
    if(vsub$F2_70[[i]] - vsub$F2_80[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 80% is within a possible F2 range
      if(vsub$F3_80[[i]] >=F2lowbound && vsub$F3_80[[i]] < F2upbound)
      {
        vsub$F2_80[[i]] <- vsub$F3_80[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_80[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " EH: used F3_80 measurement for F2_80"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_80",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 80%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_80",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    
  }
}
#print(paste0("Total EH1=",count))
#print(paste0("Total SUBSTITUTED=",count2))
#print(paste0("Total low F2 without substitution=", count3))


print(paste0("Total EH1=",count))
cat(paste0("Total EH tokens=",count), file = Vlog, sep="\n")

print(paste0("Total SUBSTITUTED=",count2))
cat(paste0("Total EH SUBSTITUTIONS=",count2), file = Vlog, sep="\n")

print(paste0("Total low F2 without substitution=", count3))
cat(paste0("Total EH low F2 without substitutions=",count3), file = Vlog, sep="\n")



####################
#Correct AE

count<-0 #For counting tokens of the vowel
count2<-0 #For counting substitutions of F3 for F2
count3 <- 0 #For counting low F2 measurements where a substitution is not made


F2lowbound <- 1142 
F2upbound <- 2540
dropTest <- 500


for(i in 1:nrow(vsub))
{
  filename <- vsub$filename[[i]]
  word <- vsub$word[[i]]
  
  #Check to see if this is a token of AE1
  if(vsub$vowel[[i]] == "AE1")
  {
    #print(paste0("Row",i," File=",vsub$filename[[i]]))
    count<-count+1
    
    #Check to see if the F2 measurement at 20% is below a stated threshold
    if(vsub$F2_20[[i]] < F2lowbound)
    {
      # print(paste0("Row ",i," OUT OF BOUNDS"))
      # count2<-count2+1
      
      #Check to see if the F3 measurement at 20% is within a possible F2 range
      if(vsub$F3_20[[i]] >=F2lowbound && vsub$F3_20[[i]] < F2upbound )
      {
        vsub$F2_20[[i]] <- vsub$F3_20[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_20[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " AE: used F3_20 measurement for F2_20"))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Low F2",
          measurepoint = "F2_20",
          action = "F3 substituted for F2")
        
        #add new row to log
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
        #add to count of substitutions
        count2<-count2+1
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " AE: Low F2_20. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Low F2",
          measurepoint = "F2_20",
          action = "No substitutions made.")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_30
    #Check to see if the F2 measurement at 30% shows a large drop from the F2 measurement at 20%
    if(vsub$F2_20[[i]] - vsub$F2_30[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 30% is within a possible F2 range
      if(vsub$F3_30[[i]] >=F2lowbound && vsub$F3_30[[i]] < F2upbound)
      {
        vsub$F2_30[[i]] <- vsub$F3_30[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_30[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " AE: used F3_30 measurement for F2_30"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_30",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 30%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_30",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
      
    }
    
    #### Move on to F2_40    
    #Check to see if the F2 measurement at 40% shows a large drop from the F2 measurement at 30%
    if(vsub$F2_30[[i]] - vsub$F2_40[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 40% is within a possible F2 range
      if(vsub$F3_40[[i]] >=F2lowbound && vsub$F3_40[[i]] < F2upbound)
      {
        vsub$F2_40[[i]] <- vsub$F3_40[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_40[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " AE: used F3_40 measurement for F2_40"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_40",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 40%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_40",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_50
    #Check to see if the F2 measurement at 50% shows a large drop from the F2 measurement at 40%
    if(vsub$F2_40[[i]] - vsub$F2_50[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 50% is within a possible F2 range
      if(vsub$F3_50[[i]] >=F2lowbound && vsub$F3_50[[i]] < F2upbound)
      {
        vsub$F2_50[[i]] <- vsub$F3_50[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_50[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " AE: used F3_50 measurement for F2_50"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_50",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 50%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_50",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_60
    #Check to see if the F2 measurement at 60% shows a large drop from the F2 measurement at 50%
    if(vsub$F2_50[[i]] - vsub$F2_60[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 60% is within a possible F2 range
      if(vsub$F3_60[[i]] >=F2lowbound && vsub$F3_60[[i]] < F2upbound)
      {
        vsub$F2_60[[i]] <- vsub$F3_60[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_60[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " AE: used F3_60 measurement for F2_60"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_60",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 60%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_60",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_70
    #Check to see if the F2 measurement at 70% shows a large drop from the F2 measurement at 60%
    if(vsub$F2_60[[i]] - vsub$F2_70[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 70% is within a possible F2 range
      if(vsub$F3_70[[i]] >=F2lowbound && vsub$F3_70[[i]] < F2upbound)
      {
        vsub$F2_70[[i]] <- vsub$F3_70[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_70[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " AE: used F3_70 measurement for F2_70"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_70",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 70%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_70",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    
    ###Move on to F2_80
    #Check to see if the F2 measurement at 80% shows a large drop from the F2 measurement at 70%
    if(vsub$F2_70[[i]] - vsub$F2_80[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 80% is within a possible F2 range
      if(vsub$F3_80[[i]] >=F2lowbound && vsub$F3_80[[i]] < F2upbound)
      {
        vsub$F2_80[[i]] <- vsub$F3_80[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_80[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " AE: used F3_80 measurement for F2_80"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_80",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 80%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_80",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    
  }
}
#print(paste0("Total AE1=",count))
#print(paste0("Total SUBSTITUTED=",count2))
#print(paste0("Total low F2 without substitution=", count3))


print(paste0("Total AE1=",count))
cat(paste0("Total AE tokens=",count), file = Vlog, sep="\n")

print(paste0("Total SUBSTITUTED=",count2))
cat(paste0("Total AE SUBSTITUTIONS=",count2), file = Vlog, sep="\n")

print(paste0("Total low F2 without substitution=", count3))
cat(paste0("Total AE low F2 without substitutions=",count3), file = Vlog, sep="\n")



####################

#Correcting UW1


count<-0 #For counting tokens of the vowel
count2<-0 #For counting substitutions of F3 for F2
count3 <- 0 #For counting low F2 measurements where a substitution is not made


F2lowbound <- 710 
F2upbound <- 2300
dropTest <- 300


for(i in 1:nrow(vsub))
{
  filename <- vsub$filename[[i]]
  word <- vsub$word[[i]]
  
  #Check to see if this is a token of UW1
  if(vsub$vowel[[i]] == "UW1")
  {
    #print(paste0("Row",i," File=",vsub$filename[[i]]))
    count<-count+1
    
    #Check to see if the F2 measurement at 20% is below a stated threshold
    if(vsub$F2_20[[i]] < F2lowbound)
    {
      # print(paste0("Row ",i," OUT OF BOUNDS"))
      # count2<-count2+1
      
      #Check to see if the F3 measurement at 20% is within a possible F2 range
      if(vsub$F3_20[[i]] >=F2lowbound && vsub$F3_20[[i]] < F2upbound )
      {
        vsub$F2_20[[i]] <- vsub$F3_20[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_20[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " UW: used F3_20 measurement for F2_20"))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Low F2",
          measurepoint = "F2_20",
          action = "F3 substituted for F2")
        
        #add new row to log
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
        #add to count of substitutions
        count2<-count2+1
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " UW: Low F2_20. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Low F2",
          measurepoint = "F2_20",
          action = "No substitutions made.")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_30
    #Check to see if the F2 measurement at 30% shows a large drop from the F2 measurement at 20%
    if(vsub$F2_20[[i]] - vsub$F2_30[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 30% is within a possible F2 range
      if(vsub$F3_30[[i]] >=F2lowbound && vsub$F3_30[[i]] < F2upbound)
      {
        vsub$F2_30[[i]] <- vsub$F3_30[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_30[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " UW: used F3_30 measurement for F2_30"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_30",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 30%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_30",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
      
    }
    
    #### Move on to F2_40    
    #Check to see if the F2 measurement at 40% shows a large drop from the F2 measurement at 30%
    if(vsub$F2_30[[i]] - vsub$F2_40[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 40% is within a possible F2 range
      if(vsub$F3_40[[i]] >=F2lowbound && vsub$F3_40[[i]] < F2upbound)
      {
        vsub$F2_40[[i]] <- vsub$F3_40[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_40[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " UW: used F3_40 measurement for F2_40"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_40",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 40%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_40",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_50
    #Check to see if the F2 measurement at 50% shows a large drop from the F2 measurement at 40%
    if(vsub$F2_40[[i]] - vsub$F2_50[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 50% is within a possible F2 range
      if(vsub$F3_50[[i]] >=F2lowbound && vsub$F3_50[[i]] < F2upbound)
      {
        vsub$F2_50[[i]] <- vsub$F3_50[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_50[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " UW: used F3_50 measurement for F2_50"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_50",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 50%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_50",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_60
    #Check to see if the F2 measurement at 60% shows a large drop from the F2 measurement at 50%
    if(vsub$F2_50[[i]] - vsub$F2_60[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 60% is within a possible F2 range
      if(vsub$F3_60[[i]] >=F2lowbound && vsub$F3_60[[i]] < F2upbound)
      {
        vsub$F2_60[[i]] <- vsub$F3_60[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_60[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " UW: used F3_60 measurement for F2_60"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_60",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 60%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_60",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_70
    #Check to see if the F2 measurement at 70% shows a large drop from the F2 measurement at 60%
    if(vsub$F2_60[[i]] - vsub$F2_70[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 70% is within a possible F2 range
      if(vsub$F3_70[[i]] >=F2lowbound && vsub$F3_70[[i]] < F2upbound)
      {
        vsub$F2_70[[i]] <- vsub$F3_70[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_70[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " UW: used F3_70 measurement for F2_70"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_70",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 70%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_70",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    
    ###Move on to F2_80
    #Check to see if the F2 measurement at 80% shows a large drop from the F2 measurement at 70%
    if(vsub$F2_70[[i]] - vsub$F2_80[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 80% is within a possible F2 range
      if(vsub$F3_80[[i]] >=F2lowbound && vsub$F3_80[[i]] < F2upbound)
      {
        vsub$F2_80[[i]] <- vsub$F3_80[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_80[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " UW: used F3_80 measurement for F2_80"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_80",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 80%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_80",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    
  }
}
#print(paste0("Total UW1=",count))
#print(paste0("Total SUBSTITUTED=",count2))
#print(paste0("Total low F2 without substitution=", count3))


print(paste0("Total UW1=",count))
cat(paste0("Total UW tokens=",count), file = Vlog, sep="\n")

print(paste0("Total SUBSTITUTED=",count2))
cat(paste0("Total UW SUBSTITUTIONS=",count2), file = Vlog, sep="\n")

print(paste0("Total low F2 without substitution=", count3))
cat(paste0("Total UW low F2 without substitutions=",count3), file = Vlog, sep="\n")

####################

#Correcting UH1


count<-0 #For counting tokens of the vowel
count2<-0 #For counting substitutions of F3 for F2
count3 <- 0 #For counting low F2 measurements where a substitution is not made


F2lowbound <- 881 
F2upbound <- 2276
dropTest <- 300


for(i in 1:nrow(vsub))
{
  filename <- vsub$filename[[i]]
  word <- vsub$word[[i]]
  
  #Check to see if this is a token of UH1
  if(vsub$vowel[[i]] == "UH1")
  {
    #print(paste0("Row",i," File=",vsub$filename[[i]]))
    count<-count+1
    
    #Check to see if the F2 measurement at 20% is below a stated threshold
    if(vsub$F2_20[[i]] < F2lowbound)
    {
      # print(paste0("Row ",i," OUT OF BOUNDS"))
      # count2<-count2+1
      
      #Check to see if the F3 measurement at 20% is within a possible F2 range
      if(vsub$F3_20[[i]] >=F2lowbound && vsub$F3_20[[i]] < F2upbound )
      {
        vsub$F2_20[[i]] <- vsub$F3_20[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_20[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " UH: used F3_20 measurement for F2_20"))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Low F2",
          measurepoint = "F2_20",
          action = "F3 substituted for F2")
        
        #add new row to log
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
        #add to count of substitutions
        count2<-count2+1
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " UH: Low F2_20. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Low F2",
          measurepoint = "F2_20",
          action = "No substitutions made.")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_30
    #Check to see if the F2 measurement at 30% shows a large drop from the F2 measurement at 20%
    if(vsub$F2_20[[i]] - vsub$F2_30[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 30% is within a possible F2 range
      if(vsub$F3_30[[i]] >=F2lowbound && vsub$F3_30[[i]] < F2upbound)
      {
        vsub$F2_30[[i]] <- vsub$F3_30[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_30[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " UH: used F3_30 measurement for F2_30"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_30",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 30%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_30",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
      
    }
    
    #### Move on to F2_40    
    #Check to see if the F2 measurement at 40% shows a large drop from the F2 measurement at 30%
    if(vsub$F2_30[[i]] - vsub$F2_40[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 40% is within a possible F2 range
      if(vsub$F3_40[[i]] >=F2lowbound && vsub$F3_40[[i]] < F2upbound)
      {
        vsub$F2_40[[i]] <- vsub$F3_40[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_40[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " UH: used F3_40 measurement for F2_40"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_40",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 40%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_40",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_50
    #Check to see if the F2 measurement at 50% shows a large drop from the F2 measurement at 40%
    if(vsub$F2_40[[i]] - vsub$F2_50[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 50% is within a possible F2 range
      if(vsub$F3_50[[i]] >=F2lowbound && vsub$F3_50[[i]] < F2upbound)
      {
        vsub$F2_50[[i]] <- vsub$F3_50[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_50[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " UH: used F3_50 measurement for F2_50"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_50",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 50%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_50",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_60
    #Check to see if the F2 measurement at 60% shows a large drop from the F2 measurement at 50%
    if(vsub$F2_50[[i]] - vsub$F2_60[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 60% is within a possible F2 range
      if(vsub$F3_60[[i]] >=F2lowbound && vsub$F3_60[[i]] < F2upbound)
      {
        vsub$F2_60[[i]] <- vsub$F3_60[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_60[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " UH: used F3_60 measurement for F2_60"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_60",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 60%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_60",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_70
    #Check to see if the F2 measurement at 70% shows a large drop from the F2 measurement at 60%
    if(vsub$F2_60[[i]] - vsub$F2_70[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 70% is within a possible F2 range
      if(vsub$F3_70[[i]] >=F2lowbound && vsub$F3_70[[i]] < F2upbound)
      {
        vsub$F2_70[[i]] <- vsub$F3_70[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_70[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " UH: used F3_70 measurement for F2_70"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_70",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 70%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_70",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    
    ###Move on to F2_80
    #Check to see if the F2 measurement at 80% shows a large drop from the F2 measurement at 70%
    if(vsub$F2_70[[i]] - vsub$F2_80[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 80% is within a possible F2 range
      if(vsub$F3_80[[i]] >=F2lowbound && vsub$F3_80[[i]] < F2upbound)
      {
        vsub$F2_80[[i]] <- vsub$F3_80[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_80[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " UH: used F3_80 measurement for F2_80"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_80",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 80%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_80",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    
  }
}
#print(paste0("Total UH1=",count))
#print(paste0("Total SUBSTITUTED=",count2))
#print(paste0("Total low F2 without substitution=", count3))


print(paste0("Total UH1=",count))
cat(paste0("Total UH tokens=",count), file = Vlog, sep="\n")

print(paste0("Total SUBSTITUTED=",count2))
cat(paste0("Total UH SUBSTITUTIONS=",count2), file = Vlog, sep="\n")

print(paste0("Total low F2 without substitution=", count3))
cat(paste0("Total UH low F2 without substitutions=",count3), file = Vlog, sep="\n")



####################

#Correcting AH1


count<-0 #For counting tokens of the vowel
count2<-0 #For counting substitutions of F3 for F2
count3 <- 0 #For counting low F2 measurements where a substitution is not made


F2lowbound <- 895 
F2upbound <- 2207
dropTest <- 300


for(i in 1:nrow(vsub))
{
  filename <- vsub$filename[[i]]
  word <- vsub$word[[i]]
  
  #Check to see if this is a token of AH1
  if(vsub$vowel[[i]] == "AH1")
  {
    #print(paste0("Row",i," File=",vsub$filename[[i]]))
    count<-count+1
    
    #Check to see if the F2 measurement at 20% is below a stated threshold
    if(vsub$F2_20[[i]] < F2lowbound)
    {
      # print(paste0("Row ",i," OUT OF BOUNDS"))
      # count2<-count2+1
      
      #Check to see if the F3 measurement at 20% is within a possible F2 range
      if(vsub$F3_20[[i]] >=F2lowbound && vsub$F3_20[[i]] < F2upbound )
      {
        vsub$F2_20[[i]] <- vsub$F3_20[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_20[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " AH: used F3_20 measurement for F2_20"))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Low F2",
          measurepoint = "F2_20",
          action = "F3 substituted for F2")
        
        #add new row to log
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
        #add to count of substitutions
        count2<-count2+1
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " AH: Low F2_20. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Low F2",
          measurepoint = "F2_20",
          action = "No substitutions made.")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_30
    #Check to see if the F2 measurement at 30% shows a large drop from the F2 measurement at 20%
    if(vsub$F2_20[[i]] - vsub$F2_30[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 30% is within a possible F2 range
      if(vsub$F3_30[[i]] >=F2lowbound && vsub$F3_30[[i]] < F2upbound)
      {
        vsub$F2_30[[i]] <- vsub$F3_30[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_30[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " AH: used F3_30 measurement for F2_30"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_30",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 30%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_30",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
      
    }
    
    #### Move on to F2_40    
    #Check to see if the F2 measurement at 40% shows a large drop from the F2 measurement at 30%
    if(vsub$F2_30[[i]] - vsub$F2_40[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 40% is within a possible F2 range
      if(vsub$F3_40[[i]] >=F2lowbound && vsub$F3_40[[i]] < F2upbound)
      {
        vsub$F2_40[[i]] <- vsub$F3_40[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_40[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " AH: used F3_40 measurement for F2_40"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_40",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 40%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_40",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_50
    #Check to see if the F2 measurement at 50% shows a large drop from the F2 measurement at 40%
    if(vsub$F2_40[[i]] - vsub$F2_50[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 50% is within a possible F2 range
      if(vsub$F3_50[[i]] >=F2lowbound && vsub$F3_50[[i]] < F2upbound)
      {
        vsub$F2_50[[i]] <- vsub$F3_50[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_50[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " AH: used F3_50 measurement for F2_50"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_50",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 50%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_50",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_60
    #Check to see if the F2 measurement at 60% shows a large drop from the F2 measurement at 50%
    if(vsub$F2_50[[i]] - vsub$F2_60[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 60% is within a possible F2 range
      if(vsub$F3_60[[i]] >=F2lowbound && vsub$F3_60[[i]] < F2upbound)
      {
        vsub$F2_60[[i]] <- vsub$F3_60[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_60[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " AH: used F3_60 measurement for F2_60"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_60",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 60%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_60",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_70
    #Check to see if the F2 measurement at 70% shows a large drop from the F2 measurement at 60%
    if(vsub$F2_60[[i]] - vsub$F2_70[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 70% is within a possible F2 range
      if(vsub$F3_70[[i]] >=F2lowbound && vsub$F3_70[[i]] < F2upbound)
      {
        vsub$F2_70[[i]] <- vsub$F3_70[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_70[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " AH: used F3_70 measurement for F2_70"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_70",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 70%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_70",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    
    ###Move on to F2_80
    #Check to see if the F2 measurement at 80% shows a large drop from the F2 measurement at 70%
    if(vsub$F2_70[[i]] - vsub$F2_80[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 80% is within a possible F2 range
      if(vsub$F3_80[[i]] >=F2lowbound && vsub$F3_80[[i]] < F2upbound)
      {
        vsub$F2_80[[i]] <- vsub$F3_80[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_80[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " AH: used F3_80 measurement for F2_80"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_80",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 80%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_80",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    
  }
}
#print(paste0("Total AH1=",count))
#print(paste0("Total SUBSTITUTED=",count2))
#print(paste0("Total low F2 without substitution=", count3))


print(paste0("Total AH1=",count))
cat(paste0("Total AH tokens=",count), file = Vlog, sep="\n")

print(paste0("Total SUBSTITUTED=",count2))
cat(paste0("Total AH SUBSTITUTIONS=",count2), file = Vlog, sep="\n")

print(paste0("Total low F2 without substitution=", count3))
cat(paste0("Total AH low F2 without substitutions=",count3), file = Vlog, sep="\n")


####################

#Correcting OW1

count<-0 #For counting tokens of the vowel
count2<-0 #For counting substitutions of F3 for F2
count3 <- 0 #For counting low F2 measurements where a substitution is not made


F2lowbound <- 823 
F2upbound <- 2016
dropTest <- 300


for(i in 1:nrow(vsub))
{
  filename <- vsub$filename[[i]]
  word <- vsub$word[[i]]
  
  #Check to see if this is a token of OW1
  if(vsub$vowel[[i]] == "OW1")
  {
    #print(paste0("Row",i," File=",vsub$filename[[i]]))
    count<-count+1
    
    #Check to see if the F2 measurement at 20% is below a stated threshold
    if(vsub$F2_20[[i]] < F2lowbound)
    {
      # print(paste0("Row ",i," OUT OF BOUNDS"))
      # count2<-count2+1
      
      #Check to see if the F3 measurement at 20% is within a possible F2 range
      if(vsub$F3_20[[i]] >=F2lowbound && vsub$F3_20[[i]] < F2upbound )
      {
        vsub$F2_20[[i]] <- vsub$F3_20[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_20[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " OW: used F3_20 measurement for F2_20"))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Low F2",
          measurepoint = "F2_20",
          action = "F3 substituted for F2")
        
        #add new row to log
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
        #add to count of substitutions
        count2<-count2+1
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " OW: Low F2_20. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Low F2",
          measurepoint = "F2_20",
          action = "No substitutions made.")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_30
    #Check to see if the F2 measurement at 30% shows a large drop from the F2 measurement at 20%
    if(vsub$F2_20[[i]] - vsub$F2_30[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 30% is within a possible F2 range
      if(vsub$F3_30[[i]] >=F2lowbound && vsub$F3_30[[i]] < F2upbound)
      {
        vsub$F2_30[[i]] <- vsub$F3_30[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_30[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " OW: used F3_30 measurement for F2_30"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_30",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 30%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_30",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
      
    }
    
    #### Move on to F2_40    
    #Check to see if the F2 measurement at 40% shows a large drop from the F2 measurement at 30%
    if(vsub$F2_30[[i]] - vsub$F2_40[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 40% is within a possible F2 range
      if(vsub$F3_40[[i]] >=F2lowbound && vsub$F3_40[[i]] < F2upbound)
      {
        vsub$F2_40[[i]] <- vsub$F3_40[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_40[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " OW: used F3_40 measurement for F2_40"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_40",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 40%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_40",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_50
    #Check to see if the F2 measurement at 50% shows a large drop from the F2 measurement at 40%
    if(vsub$F2_40[[i]] - vsub$F2_50[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 50% is within a possible F2 range
      if(vsub$F3_50[[i]] >=F2lowbound && vsub$F3_50[[i]] < F2upbound)
      {
        vsub$F2_50[[i]] <- vsub$F3_50[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_50[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " OW: used F3_50 measurement for F2_50"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_50",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 50%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_50",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_60
    #Check to see if the F2 measurement at 60% shows a large drop from the F2 measurement at 50%
    if(vsub$F2_50[[i]] - vsub$F2_60[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 60% is within a possible F2 range
      if(vsub$F3_60[[i]] >=F2lowbound && vsub$F3_60[[i]] < F2upbound)
      {
        vsub$F2_60[[i]] <- vsub$F3_60[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_60[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " OW: used F3_60 measurement for F2_60"))
        count2<-count2
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_60",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 60%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_60",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    ###Move on to F2_70
    #Check to see if the F2 measurement at 70% shows a large drop from the F2 measurement at 60%
    if(vsub$F2_60[[i]] - vsub$F2_70[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 70% is within a possible F2 range
      if(vsub$F3_70[[i]] >=F2lowbound && vsub$F3_70[[i]] < F2upbound)
      {
        vsub$F2_70[[i]] <- vsub$F3_70[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_70[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " OW: used F3_70 measurement for F2_70"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_70",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 70%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_70",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    
    ###Move on to F2_80
    #Check to see if the F2 measurement at 80% shows a large drop from the F2 measurement at 70%
    if(vsub$F2_70[[i]] - vsub$F2_80[[i]] > dropTest)
    {
      #Check to see if the F3 measurement at 80% is within a possible F2 range
      if(vsub$F3_80[[i]] >=F2lowbound && vsub$F3_80[[i]] < F2upbound)
      {
        vsub$F2_80[[i]] <- vsub$F3_80[[i]] #Substitute the F3 measurement for the F2 at this measurement point
        vsub$F3_80[[i]] <- NA #Record the F3 measurement at this point as missing
        print(paste0("Row ",i, ", ", filename, ", ", word, " OW: used F3_80 measurement for F2_80"))
        count2<-count2+1
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_80",
          action = "F3 substituted for F2")
        
        #add new row to a dataframe
        sublist = rbind(sublist, newrow, stringsAsFactors=FALSE)
        
      }
      else
      {
        print(paste0("Row ",i, ", ", filename, ", ", word, " Large drop in F2 at 80%. No substitution made."))
        
        newrow <- list(
          speakerid = vsub$speakerid[[i]],
          word = vsub$word[[i]],
          filename = filename,
          rating = vsub$rating[[i]],
          vowel = vsub$vowel[[i]],
          problem = "Drop in F2",
          measurepoint = "F2_80",
          action = "No substitutions made.")
        
        count3 <- count3+1
      }
    }
    
    
  }
}
#print(paste0("Total OW1=",count))
#print(paste0("Total SUBSTITUTED=",count2))
#print(paste0("Total low F2 without substitution=", count3))


print(paste0("Total OW1=",count))
cat(paste0("Total OW tokens=",count), file = Vlog, sep="\n")

print(paste0("Total SUBSTITUTED=",count2))
cat(paste0("Total OW SUBSTITUTIONS=",count2), file = Vlog, sep="\n")

print(paste0("Total low F2 without substitution=", count3))
cat(paste0("Total OW low F2 without substitutions=",count3), file = Vlog, sep="\n")




####################
cat(paste("END PROCESSING: ", Sys.time()), file = Vlog, sep="\n")
close(Vlog)

write.csv(vsub, file = "E:\\TokenAnalysis\\Data\\vsub.csv", row.names = FALSE)
write.csv(sublist, file = "E:\\TokenAnalysis\\Data\\sublist.csv", row.names = FALSE)




