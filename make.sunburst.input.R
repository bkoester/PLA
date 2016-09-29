######################################################################################
#1) How do students "flow" through a major: which courses do they take and when?
#
#FUNCTION: make.sunburst.input.R
#PURPOSE : For a given major and courselist, create an input table for the D3 sunburst functions. The
#          table written to disk is an input for the D3 functions.
#INPUTS  : sr - student record table
#          sc - student record table
#          SUBJECT - select all courses from this SUBJECT for analysis.
#          MAJ - which major to analyze.
#          NON - if set to TRUE, this will analyze ALL courses NOT in the SUBJECT.
#          DIR - the output directory, CWD by default.
#OUTPUTS : Returns to session/writes sunburst table to specified directory. Also returns the table to 
#          current session (set to 'out' in EXAMPLE below)
#EXAMPLE: out <- make.sunburst.input(sr,sc,SUB='PHYSICS',MAJ='Physics BS')
#Notes: If NON=TRUE, this cuts on courses with enrollments > 100. NON=FALSE: course enrollment must be > 5.
#
#####################################################################################
make.sunburst.input <- function(sr,sc,SUB='PHYSICS',MAJ='Physics BS',DIR='CWD',ORDERED=FALSE,TERM=TRUE,NON=FALSE)
{
  #sr <- read.delim("/Users/bkoester/Box Sync/ART.PIPELINE/student.record.6.Sept.2015.preMOOC.tab")
  #sc <- read.delim("/Users/bkoester/Box Sync/ART.PIPELINE/student.course.6.Sept.2015.preMOOC.tab")
  
  #Need to comment, describe this section still.
  otype <- 'ORDERED'
  if (ORDERED == FALSE){otype <- 'UNORDERED'}
  if (TERM == TRUE){otype <- 'TERM'}
  if (NON == TRUE){otype <- paste(otype,'NON',sep="")}
  oname <- paste(SUB,gsub(" ","",MAJ),otype,'csv',sep=".",collapse="")
  oname <- gsub("&","",oname)
  
  #cut on enrollment
  if (NON == TRUE){ecut <- 100}
  if (NON == FALSE){ecut <- 5}
  
  #Build the course matrix that we will convert to sunburst format.
  out <- build.course.matrix(sr,sc,SUB=SUB,MAJ=MAJ,NON=NON,AGG_TERM=TRUE,ENROLL=ecut)
  nst <- length(out$ANONID)
  out <- out[,!names(out) %in% c("ANONID")]
  
  seq        <- mat.or.vec(nst,1)
  ncrse      <- length(names(out))
  CNAMES     <- names(out) 
  #for each of 12 possible terms for each student, fill this matrix in with the courses taken that term.
  #NONE will be entered by default.
  term.seq   <- mat.or.vec(nst,12)
  term.seq[] <- 'NONE'
  
  for (i in 1:nst)   #looping over the students
  {
    flag <- 0
    len  <- 0
    sub <- out[i,]
    
    if (ORDERED == TRUE){sub <- sort(sub)}
    
    nsub <- as.character(names(sub))
  
    if (SUB == 'DIV')
    {
       lensub <- length(nsub)
       for (k in 1:lensub)
       {
         nsub[k]    <- strsplit(nsub[k],'\\.')[[1]][1]
       }
    }
    
    sub  <- as.numeric(sub)
    ncrse <- length(sub)
    
    for (j in 1:ncrse) #looping over each of their courses
    {
      
      if (!is.na(sub[j]) & flag == 1)
      {
        temp <- paste(temp,nsub[j],sep="-")
        flag <- 1
        len  <- len+1
        if (term.seq[i,sub[j]] == 'NONE')
        {
          term.seq[i,sub[j]] <- nsub[j]
        }
        else
        {
          term.seq[i,sub[j]] <- paste(term.seq[i,sub[j]],nsub[j],sep="/")
        } 
      }
      if (!is.na(sub[j]) & flag == 0)
      {
        temp <- nsub[j]
        flag <- 1
        len <- len+1
        if (term.seq[i,sub[j]] == 'NONE')
        {
          term.seq[i,sub[j]] <- nsub[j]
        }
        else
        {
          term.seq[i,sub[j]] <- paste(term.seq[i,sub[j]],nsub[j],sep="/")
        }
      }
    }
    
    if (len < length(CNAMES)){temp <- paste(temp,'-end',sep="")}
    seq[i] <- temp
  }
  
  #and now paste things together for the term-dependent
  
  if (TERM == TRUE)
  {
    seq <- mat.or.vec(nst,1)
    flag <- 0
    
    for (i in 1:nst)
    {
      for (j in 1:12)
      {
        if (flag == 1)
        {
          temp <- paste(temp,sort(term.seq[i,j]),sep="-")
        }
        if (flag == 0)
        {
          temp <- sort(term.seq[i,j])
          flag <- 1
        }
      }
      
      seq[i] <- temp
      flag <- 0
      
    }
  }
  
  #tabulate and aggregate the different orderings and output!
  col2 <- as.numeric(summary(as.factor(seq),max=length(seq)))
  col1 <- names(summary(as.factor(seq),max=length(seq)))
  seq  <- data.frame(col1,col2)
  #DIR  <- '/Users/bkoester/Google Drive/code/REBUILD/course_patterns/bkoester.github.io/sequences/data/'
  if (DIR != 'CWD')
    {
    outpath <- paste(DIR,oname,sep="")
  }
  else
  {
    outpath <- oname
  }
  write.table(seq,file=outpath,col.names=FALSE,quote=FALSE,sep=",",row.names=FALSE)
  return(seq)
  
}
###############################################
#FUNCTION: build.course.matrix.R
#PURPOSE : Fill in a student course-by-term matrix 
#          --rows: the student
#          --column: courses. Each cells contain the index (1-12) the student took the term.
#INPUTS  : sr - student record table
#          sc - student record table
#          SUBJECT - select all courses from this SUBJECT for analysis.
#          MAJ - which major to analyze.
#          NON - if set to TRUE, this will analyze ALL courses NOT in the SUBJECT.
#OUTPUTS : Returns to session/writes sunburst table to specified directory.
#EXAMPLE: out <- make.sunburst.input(sr,sc,SUB='PHYSICS',MAJ='Physics BS')
#Notes: If NON=TRUE, this cuts on courses with enrollments > 100. NON=FALSE: course enrollment must be > 5.
#####################################################################################
build.course.matrix <- function(sr,sc,SUB='PHYSICS',MAJ='Physics BS',NON=FALSE,ENROLL=0,AGG_TERM=TRUE)
{
  
  e  <- (grepl(MAJ,sr$MAJOR1_DESCR) | 
           grepl(MAJ,sr$MAJOR2_DESCR)) & sr$ADMIT_TERM > 100
  if (NON==TRUE)
  {
    e <- sr$ADMIT_TERM > 100 & !is.na(sr$MAJOR1_DESCR) & 
      !grepl(MAJ,sr$MAJOR1_DESCR) & !grepl(MAJ,sr$MAJOR2_DESCR)
  }
  sr <- sr[which(e),]
  
  sc <- merge(sr,sc,by="ANONID",all.x=TRUE)
  CRSE_ID <- paste(sc$SUBJECT,sc$CATALOG_NBR,sep=".")
  sc <- data.frame(sc,CRSE_ID)
  
  #number the student term
  sc <- number.student.terms(sc)
  e  <- sc$TERM_IND <= 12 & !is.na(sc$DIV)
  sc <- sc[which(e),]
  print('re-numbered terms')
  
  if (SUB != 'ALL' & SUB != 'DIV')
  {
    print(paste('including only ',SUB,' classes', sep=""))
    f <- sc$SUBJECT == SUB  
    sc <- sc[which(f),]
  }
  
  #Keeping this commented-out stuff around until I'm settled on this method of recording division.
  if (SUB == 'DIV')
  {
    
    #e <- sc$DIV == 'E'
    #s <- sc$DIV == 'S'
    #ss <- sc$DIV == 'SS'
    #o <- sc$DIV == 'O'
    #h <- sc$DIV  == 'H'
    
    #sc <- sc[,!names(sc) %in% 'DIV']
    #DIV <- mat.or.vec(length(sc$EMPLID),1)
    #DIV[] <- 'NONE'
    #sc <- data.frame(sc,DIV)
    
    #sc$DIV[which(s)]  <- 'Sci'
    #sc$DIV[which(e)]  <- 'Eng'
    #sc$DIV[which(ss)] <- 'Soc'
    #sc$DIV[which(h)]  <- 'Hu'
    #sc$DIV[which(o)]  <- 'Oth'
    
    sc$SUBJECT <- as.character(sc$DIV)
  }
  
  #only keep the most abundant classes
  if (ENROLL != 0)
  {
    sc <- trim.course.enrollment(sc)
    e  <- sc$ENROLL > ENROLL
    sc <- sc[which(e),]
  }
  #Now, with the course selection settled, create term-specific IDs
  if (AGG_TERM == FALSE)
  {
    sc$CATALOG_NBR <- paste(sc$CATALOG_NBR,sc$TERM_IND,sep=".")
    sc$CRSE_ID     <- paste(sc$CRSE_ID,sc$TERM_IND,sep=".")
  }
  
  e  <- !duplicated(sc$CRSE_ID)
  CRSE_NAMES <- paste(sc$SUBJECT[e],sc$CATALOG_NBR[e],sep="")
  if (SUB == 'DIV'){CRSE_NAMES <- sc$SUBJECT[e]}
  CRSEID     <- sc$CRSE_ID[e]
  t          <- order(CRSE_NAMES)
  CRSE_NAMES <- CRSE_NAMES[t]
  CRSEID     <- CRSEID[t]
  
  NCRSE      <- length(CRSE_NAMES)
  
  data       <- sc
  #And then get courses by term for each student
  nid        <- length(data$ANONID[!duplicated(data$ANONID)])
  data       <- data[order(data$ANONID,data$TERM),] #This sort is crucial. Keeping the SEX makes sure that female is always index 1.
  data$count <- sequence(rle(as.vector(data$ANONID))$lengths)
  
  nid     <- length(data$ANONID[!duplicated(data$ANONID)])
  nstart  <- which(data$count == 1)
  ntot    <- length(data$ANONID)
  ANONID  <- mat.or.vec(nid,1)
  MCRSE   <- mat.or.vec(nid,NCRSE)
  MCRSE[] <- NA
  
  for (i in 1:nid)
  {
    #print(paste(i,' of ',nid,sep=""))
    start_ind <- nstart[i]
    if (i < nid){stop_ind  <- nstart[i+1]-1}
    if (i == nid){stop_ind <- ntot}
    ind <- c(start_ind:stop_ind)
    sub <- data[ind,]
    ANONID[i] <- data$ANONID[start_ind]
    #MCRSE[i,data$TERM_IND[ind]] <- paste(data$SUBJECT[ind],data$CATALOG_NBR[ind],sep="")
    for (j in 1:NCRSE)
    {
      t <- which(sub$CRSE_ID == CRSEID[j])
      if (length(t) > 0){MCRSE[i,j] <- sub$TERM_IND[t[1]]}
    }
    
  }
  
  out <- data.frame(ANONID,MCRSE)
  names(out) <- c('ANONID',CRSE_NAMES)
  
  return(out)
}


#This computes total enrollment by course over all terms for courses in the
#input data set. It allows one to restrict courses considered by enrollment, with ENROLL=TRUE
#in the main function.
trim.course.enrollment <- function(data)
{
  nid        <- length(data$CRSE_ID[!duplicated(data$CRSE_ID)])
  data       <- data[order(data$CRSE_ID),] #This sort is crucial. Keeping the SEX makes sure that female is always index 1.
  data$count <- sequence(rle(as.vector(data$CRSE_ID))$lengths)
  
  nid     <- length(data$CRSE_ID[!duplicated(data$CRSE_ID)])
  nstart  <- which(data$count == 1)
  ntot    <- length(data$CRSE_ID)
  ENROLL  <- mat.or.vec(ntot,1)
  
  for (i in 1:nid)
  {
    start_ind <- nstart[i]
    if (i < nid){stop_ind  <- nstart[i+1]-1}
    if (i == nid){stop_ind <- ntot}
    ind <- c(start_ind:stop_ind)
    ENROLL[ind] <- length(ind)
  }
  
  data <- data.frame(data,ENROLL)
  
}  

#This indexs student terms (1-12). That is, it counts the number 
#of terms enrolled in classes (even summer classes!) and indexes them.
number.student.terms <- function(data)
{
  #for each student, go in and number the terms
  data        <- data[order(data$ANONID,data$TERM),] #This sort is crucial. Keeping the SEX makes sure that female is always index 1.
  data$count  <- sequence(rle(as.vector(data$ANONID))$lengths)
  
  nid      <- length(data$ANONID[!duplicated(data$ANONID)])
  nstart   <- which(data$count  == 1)
  ntot     <- length(data$ANONID)
  TERM_IND <- mat.or.vec(ntot,1)
  
  for (i in 1:nid)
  {
    start_ind <- nstart[i]
    if (i < nid){stop_ind  <- nstart[i+1]-1}
    if (i == nid){stop_ind <- ntot}
    ind <- c(start_ind:stop_ind)
    sub <- data[ind,]
    
    nctot      <- length(ind)
    nt         <- length(sub$TERM[!duplicated(sub$TERM)])
    sub$count2 <- sequence(rle(as.vector(sub$TERM))$lengths)
    nstart2    <- which(sub$count2 == 1)
    
    for (j in 1:nt)
    {
      start_ind2 <- nstart2[j]
      if (j < nt){stop_ind2  <- nstart2[j+1]-1}
      if (j == nt){stop_ind2 <- nctot}
      ind2 <- c(start_ind2:stop_ind2)
      TERM_IND[ind[ind2]] <- j
    }
    
  }
  
  data <- data.frame(data,TERM_IND)
  return(data)
}

# A wrapper to run a bunch of major/course combinations.
# Leaving in the various examples for reference.
run.all.sunburst <- function(sr,sc)
{
  
  #grep('Chem',names(summary(sr$MAJOR1_DESCR)),value=TRUE)
  #STEM combinations
  #MAJ <- c('Astro','Physics','Chemistry',"Ecology & Evolut Biology BS",
  #         "Cellular & Molec Biology BS","Mathematics","Psych","Econ")
  #SUB <- c('ASTRO','PHYSICS','MATH','CHEM','BIOLOGY','MCDB','EEB','PSYCH','ECON')
  
  #the NONs
  MAJ <- c('Physics','Chemistry',"Mathematics","Psychology BS","Economics BS")
  SUB <- c('PHYSICS','CHEM','MATH','PSYCH','ECON')
  #MAJ <- 'Physics'
  #SUB <- 'DIV'
  
  
  nmaj <- length(MAJ)
  nsub <- length(SUB)
  
  for (i in 1:nsub)
  {
    for (j in 1:nmaj)
    {
      if (i == j)
      {
        make.sunburst.input(sr,sc,SUB=SUB[i],MAJ=MAJ[j],TERM=TRUE) #ORDERED=FALSE)#,NON=TRUE)
        #make.sunburst.input(sr,sc,SUB=SUB[i],MAJ=MAJ[j],ORDERED=TRUE)#,NON=TRUE)
        #make.sunburst.input(sr,sc,SUB=SUB[i],MAJ=MAJ[j],TERM=TRUE)#,NON=TRUE)
        #make.sunburst.input(sr,sc,SUB=SUB[i],MAJ=MAJ[j],TERM=TRUE,NON=TRUE)
        #make.sunburst.input(sr,sc,SUB='DIV',MAJ=MAJ[i],TERM=TRUE)
      }
    }
  }
  
}