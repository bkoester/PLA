#This takes the student course table and makes a GPA by term by student table.
#NOTES/CAUTION!
#1) Running this on the PLA-MOOC student-course table provided takes ~ 2 min on my laptop.
#2) This uses ONLY the grades in the student-course table to compute GPA.
#3) It therefore assumes the grades in the average are equally-weighted, 
#4) The GPA computed WILL NOT necessarily be consistent with the GPAO! 
#   This is becasue the GPAOs are synthetic, and directed at computing grade anomalies.
######
student.term.GPA <- function(sc)
{
  
  #just keep the fields we need.
  sc       <- sc[,names(sc) %in% c("ANONID","TERM","GRD_PTS_PER_UNIT","GPA","GPAO")]

  #make the new student-term key  
  STKEY    <- paste(sc$ANONID,sc$TERM,sep=".")
  
  #add it on.
  sc       <- data.frame(sc,STKEY)
  
  #Finally, compute a GPA term-by-term, given the data we have
  sc       <- sc[order(sc$ANONID,sc$TERM), ]
  sc$count <- sequence(rle(as.vector(sc$ANONID))$lengths)
  
  nstart <- which(sc$count == 1)
  ntot   <- length(sc$count)
  GPA    <- mat.or.vec(ntot,1)
  
  nid <- length(sc$ANONID[!duplicated(sc$ANONID)])
  
  for (i in 1:nid)
  {
    
    #if (i %% 100 == 0){print(paste(i,' of ',nid,sep=""))}
    #Get the indices of the people in these classes.
    start_ind <- nstart[i]
    if (i < nid){stop_ind  <- nstart[i+1]-1}
    if (i == nid){stop_ind <- ntot}
    ind <- c(start_ind:stop_ind)
    
    #Get the grades for this student and compute GPA term-by-term
    sub    <- sc[ind,]
    sterms <- sub$TERM
    nterms <- length(sterms)
    
    for (j in 1:nterms)
    {
      e1 <- sub$TERM <= sterms[j]
      e2 <- sub$TERM == sterms[j]
      GPA[ind[e2]] <- mean(sub$GRD_PTS_PER_UNIT[e1])
    }
    
  }
  
  sc <- data.frame(sc,GPA)
  sc <- sc[,!names(sc) %in% c("count","GRD_PTS_PER_UNIT")]
  
  #keep only one instance
  sc       <- sc[!duplicated(sc$STKEY),]

  return(sc)
  
}