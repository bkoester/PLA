#####################################################################################
#
#FUNCTION: course.persistence.setup
#PURPOSE : Compute course-to-course persistence for two comparison groups
#INPUTS  : 
#          sc           - full student course table
#          sr           - full student record table
#          SUBJECT1     - course subject 1
#          CATALOG_NBR1 - course catalog number 1
#          SUBJECT2     - course subject 2: the course to which students "persist"
#          CATALOG_NBR2 - course catalog number2 
#          TITLE        - Plot title
#          TYPE         - which kind of comparison to do. this basically tells us which columns to use.
#          GROUP1       - corresponding to TYPE: the name of the group1 in the type field
#          GROUP2       - corresponding to TYPE: the name of the group2 in the TYPE field to exame
#          PDF          - Write plots to PDF. Default is TRUE. Plots go to 'course_portrain.pdf' in CWD.
#OUTPUTS : 
#EXAMPLE: hh <- course.persistence.setup(sr,sc,'PHYSICS','PHYSICS',140,240,TITLE='Physics 140 -- > 240: Gender')
#         hh <- course.persistence.setup(sr,sc,'PHYSICS','PHYSICS',140,240,TYPE='MAJOR1_DEPT',
#                                               GROUP1='Physics Department',GROUP2='Chemistry Department',
#                                               TITLE='Physics 140 -- > 240: MAJOR')
#####################################################################################

course.persistence.setup <- function(sr,sc,SUBJECT1,SUBJECT2,CATALOG_NBR1,CATALOG_NBR2,TITLE='',TYPE='SEX',
                               GROUP1='F',GROUP2='M',PDF=FALSE)
{
  
  e <- (sc$SUBJECT == SUBJECT1 & sc$CATALOG_NBR == CATALOG_NBR1) | 
       (sc$SUBJECT == SUBJECT2 & sc$CATALOG_NBR == CATALOG_NBR2)
  sc <- sc[which(e),]
  
  if (TYPE == 'SEX')
  {
    e    <- sr$SEX == GROUP1 | sr$SEX == GROUP2
    sr   <- sr[which(e),]
    data <- merge(sc,sr,by='ANONID',all.x=TRUE)
    IND1 <- data$SEX == GROUP1
    IND2 <- data$SEX == GROUP2
    
    compute.course.persistence(data,IND1,IND2,CATALOG_NBR1,CATALOG_NBR2,SUBJECT1,SUBJECT2,TITLE)
    legend(0,1,c(GROUP1,GROUP2),text.col=c('black','red'))
    
  }
  
  if (TYPE == 'MAJOR1_DEPT')
  {
    e    <- sr$MAJOR1_DEPT == GROUP1 | sr$MAJOR1_DEPT == GROUP2
    sr   <- sr[which(e),]
    data <- merge(sc,sr,by='ANONID',all.x=TRUE)
    IND1 <- data$MAJOR1_DEPT == GROUP1
    IND2 <- data$MAJOR2_DEPT == GROUP2
    
    compute.course.persistence(data,IND1,IND2,CATALOG_NBR1,CATALOG_NBR2,SUBJECT1,SUBJECT2,TITLE)
    legend(0,1,c(GROUP1,GROUP2),text.col=c('black','red'))
    
  }
  
  
}



#####################################################################################
#
#FUNCTION: compute.course.persistence
#PURPOSE : Compute course-to-course persistence for two comparison groups
#INPUTS  : 
#          data         - full student course table
#          IND1         - indices of input SC table for comparison group 1 (for instance, all females)
#          IND2         - indices of input SC table for comparison group 2 (for instance, all males)
#          SUBJECT1     - course subject 1
#          CATALOG_NBR1 - course catalog number 1
#          SUBJECT2     - course subject 2: the course to which students "persist"
#          CATALOG_NBR2 - course catalog number2 
#          TITLE        - Plot title
#          PDF          - Write plots to PDF. Default is TRUE. Plots go to 'course_portrain.pdf' in CWD.
#OUTPUTS : Plots sent to persistence.pdf in the CWD.
#EXAMPLE: 
#####################################################################################
compute.course.persistence <- function(data,IND1,IND2,CATALOG_NBR1,CATALOG_NBR2,SUBJECT1,SUBJECT2,TITLE,nonGRADE=FALSE)
{
 
  outm <- compute.persistence(data[IND1,],CATALOG_NBR1,CATALOG_NBR2,SUBJECT1,SUBJECT2,nonGRADE=nonGRADE)
  outf <- compute.persistence(data[IND2,],CATALOG_NBR1,CATALOG_NBR2,SUBJECT1,SUBJECT2,nonGRADE=nonGRADE)

  xlab <- paste('Grade in ',SUBJECT1,CATALOG_NBR1,sep="")
  
  plot(outm$grade,outm$gfrac,ylim=c(0,1.0),
     xlab=xlab,pch=19,ylab='Fraction',
     main=TITLE)
  points(outf$grade,outf$gfrac,pch=19,col='red')
  #add error bars
  for (k in 1:length(outf$gfrac))
  {
    arrows(outm$grade[k],outm$gfrac[k]-outm$gse[k],outm$grade[k],outm$gfrac[k]+outm$gse[k],code=0)
  }
  for (k in 1:length(outm$gfrac))
  {
    arrows(outf$grade[k],outf$gfrac[k]-outf$gse[k],outf$grade[k],outf$gfrac[k]+outf$gse[k],code=0,col='red')
  }
  
  #crap <- persistence.matching(data,CATALOG_NBR1,CATALOG_NBR2,SUBJECT1,SUBJECT2)
  #print(sum(crap$gpenm))
  #print(sum(crap$gpenf))
  #scan()
  
}

#########################################
#COMPUTE.PERSISTENCE
#This is the core function.
#Persistence here is defined as the probability a student took course 2, given that
#they took course 1. No explicit time dependence...
compute.persistence <- function(data,CATALOG_NBR1,CATALOG_NBR2,SUBJECT1,SUBJECT2,aggregate=FALSE,nonGRADE=FALSE)
{
  #First,pick out course 1 and handle duplicates
  ee   <- data$SUBJECT == SUBJECT1 & data$CATALOG_NBR == CATALOG_NBR1
  sub1 <- data[which(ee),]
  keep <- remove.duplicates(sub1,keep="LAST",verbose=TRUE)
  sub1 <- sub1[keep,]
  View(sub1)
  #Then,pick out course 2 and handle duplicates
  ee   <- data$SUBJECT == SUBJECT2 & data$CATALOG_NBR == CATALOG_NBR2
  sub2 <- data[which(ee),]
  keep <- remove.duplicates(sub2,keep='NONE',verbose=TRUE)
  sub2 <- sub2[,names(sub2) %in% c("ANONID","TERM")]
  names(sub2) <- c("ANONID","TERM2") #Don't want to confuse two TERM columns!
  View(sub2)
  #Now use the merge function to match these (LEFT JOIN on sub1)

  sub <- merge(sub1,sub2,by='ANONID',all.x=TRUE)
  
  #If this is GPAO or GPEN, then substitute in to hijack GRD_PTS_PER_UNIT. Inefficient, but short.
  if (nonGRADE == TRUE)
  {
    new <- discretized.metric(sub$GPAO-sub$GRD_PTS_PER_UNIT,10)
    sub$GRD_PTS_PER_UNIT <- new
  }
  
  #Now pre-sort for efficiency.
  sub       <- sub[order(sub$GRD_PTS_PER_UNIT), ]
  sub$count <- sequence(rle(as.vector(sub$GRD_PTS_PER_UNIT))$lengths)
  ntot   <- length(sub$GRD_PTS_PER_UNIT)
  nid    <- length(sub$GRD_PTS_PER_UNIT[!duplicated(sub$GRD_PTS_PER_UNIT)])
  nstart <- which(sub$count == 1)
  
  grade  <- mat.or.vec(nid,1)
  grade_se <- NA
  n1     <- mat.or.vec(nid,1)
  n2     <- mat.or.vec(nid,1)
  term1  <- mat.or.vec(nid,1)
  term2  <- mat.or.vec(nid,1)
  
  #Check each grade class for 1) taking CATALOG_NBR1/CATALOG_NBR2 and count. 
  #There is no time-depenendence built-in, but we keep
  #term1 and term2 so that it might be built-in later.
  print('sorting by grade')
  for (i in 1:nid)
  {
    start_ind <- nstart[i]
    if (i < nid){stop_ind  <- nstart[i+1]-1}
    if (i == nid){stop_ind <- ntot}
    ind       <- c(start_ind:stop_ind)
    n1[i] <- length(ind)
    n2[i] <- sum(!is.na(sub$TERM2[ind]))
    grade[i] <- sub$GRD_PTS_PER_UNIT[start_ind]
    term1[i] <- sub$TERM[start_ind]
    term2[i] <- sub$TERM2[start_ind]
  }
  print('finished grade sort')
  gfrac <- n2/n1  #fraction that continue
  gse   <- sqrt(gfrac[i]*(1-gfrac[i]))/sqrt(n1) #asymptotic binomial standard error. 

  #for the aggregate option
  if (aggregate == TRUE)
  {
    gfrac <- sum(n2)/sum(n1)                 #Overall persistence from 1 ---> 2
    gse   <- sqrt(gfrac*(1-gfrac)/sqrt(sum(n1))) #Standard error
    grade <- mean(sub1$GRD_PTS_PER_UNIT)                #Mean grade in course 1
    grade_se <- sd(sub1$GRD_PTS_PER_UNIT)/sqrt(sum(n1)) #Grade standard error in course 1
  }
  
  out <- data.frame(gfrac,gse,grade,grade_se)
  return(out)
}

#####################################
#REMOVE.DUPLICATES
#One of many things to deal with: students that take classes multiple times.
#This function keeps a specific duplicate or removes the ID altogether (default).
#You may keep NONE, the FIRST grade, or the LAST grade among the dupicates
#This assumes you have already selected a subject/catnum and returns indices of the records to KEEP.
remove.duplicates <- function(data,keep='NONE',verbose=FALSE)
{
  
  #Sort grades by ID, then TERM.
 
  data       <- data[order(data$ANONID,data$TERM), ]
  data$count <- sequence(rle(as.vector(data$ANONID))$lengths)
  
  ntot   <- length(data$ANONID)
  
  #Keep the first recorded grade of the duplicates
  if (keep == "FIRST")
  { 
    good <- which(data$count == 1)
    ngood <- length(good)
    if (verbose == TRUE)
    {
      print('keeping the first grade only')
      print(paste('kept ',ngood,' records of ',ntot,sep=""))
    }
  }    
  else
  {
    nid    <- length(data$ANONID[!duplicated(data$ANONID)])
    nstart <- which(data$count == 1)
    
    kdup   <- mat.or.vec(ntot,1)
    kfirst <- mat.or.vec(ntot,1)
    klast  <- mat.or.vec(ntot,1)
    
    for (i in 1:nid)
    {
      start_ind <- nstart[i]
      if (i < nid){stop_ind  <- nstart[i+1]-1}
      if (i == nid){stop_ind <- ntot}
      ind <- c(start_ind:stop_ind)
      kdup[ind] <- length(ind)
      kfirst[ind] <- start_ind
      klast[ind]  <- stop_ind
    }
    
    #Don't keep records for ANY duplicates.
    if (keep == "NONE") 
    {
      good  <- which(klast == kfirst)
      ngood <- length(good)
      if (verbose == TRUE)
      {
        print('discarding duplicate students')
        print(paste('kept ',ngood,' students of ',nid,sep=""))
      }
    }
    
    #Keep only the last grade among duplicates
    if (keep == "LAST") 
    {
      good <- which(data$count == kdup)
      ngood <- length(good)
      if (verbose == TRUE)
      {
        print('keeping the last grade only')
        print(paste('kept ',ngood,' records of ',ntot,sep=""))
      }   
    }
  }
  
  return(good)
  
}

discretized.metric <- function(met,nbins)
{
  minmet <- min(met,na.rm=TRUE)
  maxmet <- max(met,na.rm=TRUE)
  bsize  <- (maxmet-minmet)/nbins
  
  newmet <- mat.or.vec(length(met),1)
  
  for (i in 1:nbins)
  {
    print(minmet+bsize*i)
    print(minmet+bsize*(i-1))
    e <- met < minmet+bsize*i & met > minmet+bsize*(i-1)
    newmet[e] <- minmet+bsize*0.5+bsize*(i-1) 
  }
  
  return(newmet)
  
}