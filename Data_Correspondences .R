#Read in the 2006 and 2011 census data  and associated correspondence information.
dat1 <- read.csv("2006.csv") #2006 Census Data
dat2 <- read.csv("2011.csv") #2011 Census Data
dat3 <- read.csv("2006to2011.csv") #ABS Correspondences

#Create placeholder variables to store transformed 2006 data with 2011 boundaries.
dat2$pop2006 <- NA
dat2$Score2006 <- NA
#Create loop to assign 2006 values to 2011 boundaries.

for(i in 1:NROW(dat2)){
  
  #Match 2006 values to 2011 boundaries, by calling weighting which corresponds to 2006 SA1.
  idx1 <- which(dat2$code[i] == dat3$code2011)
  if(length(idx1) > 0){
    idx2 <- match(dat3$code2006[idx1], dat1$code)
    
    #Identify if there are any 2006 SA1 boundaries that have no correspondence information.
    if(any(is.na(idx2))){
      idx1 <- idx1[!is.na(idx2)]
      idx2 <- idx2[!is.na(idx2)]
      cat("\Missing\n")
    }
    #assign weights as a ratio between 2006 and 2011
    weights <- dat3$ratio[idx1]
    
    #multiple 2006 population value by 2011 weighting
    dat2$pop2006[i] <- sum(weights * dat1$pop[idx2])
    
    #normalise weight for socio economic index
    weights <- weights / sum(weights)
    
    #multiple SEIFA index by 2011 weighting
    dat2$Score2006[i] <- sum(weights * dat1$Score[idx2])
  }
}