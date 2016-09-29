build.student.network <- function(sc,sr,NETNAME='TEST')
{
  
  #sc <- read.csv("/Users/bkoester/Google Drive/code/REBUILD/MOOC/PLA/student.course.csv")
  #sr <- read.csv("/Users/bkoester/Google Drive/code/REBUILD/MOOC/PLA/student.record.csv")
  
  library(igraph)
  
  e <- !is.na(sr$MAJOR1_DESCR) & sr$ADMIT_TERM == 110 & 
       (sr$MAJOR1_DEPT == 'Economics Department' | sr$MAJOR1_DEPT == 'Psychology Department' |
        sr$MAJOR1_DEPT == 'Mathematics Department' | sr$MAJOR1_DEPT == 'Physics Department' | 
        sr$MAJOR1_DEPT == 'Chemistry Department' | sr$MAJOR1_DEPT == 'Biology Department' |
        sr$MAJOR1_DEPT == 'Ecology & Evolutionary Biology' | sr$MAJOR1_DEPT == 'Molecular, Cellular, and Developmental Biology' |
        sr$MAJOR1_DEPT == 'English Language & Literature Dept' | sr$MAJOR1_DEPT == 'Sociology Department' |
        sr$MAJOR1_DEPT == 'History Department' | sr$MAJOR1_DEPT == 'Political Science Department')

            
  sr <- sr[which(e),]
  sr <- sr[,names(sr) %in% c('ANONID','MAJOR1_DESCR','MAJOR2_DESCR','MAJOR3_DESCR')]
  nst <- length(sr$ANONID)
  print(paste('number of students = ',nst,sep=""))
  
  e <- sc$TERM >= 110
  sc <- sc[which(e),]
  sc <- sc[,names(sc) %in% c('ANONID','SUBJECT','CATALOG_NBR')]
  CRSE_ID <- paste(sc$SUBJECT,sc$CATALOG_NBR,sep="")
  ncrse   <- length(CRSE_ID[!duplicated(CRSE_ID)])
  print(paste('number of courses = ',ncrse,sep=""))
  
  sc      <- data.frame(sc,CRSE_ID)
  
  sc <- trim.course.enrollment(sc)
  e  <- sc$ENROLL < 10000
  sc <- sc[e,]
  hist(sc$ENROLL)
  
  data <- merge(sr,sc,by='ANONID',all.x=TRUE)
  nscsr <- length(data$ANONID)
  print(paste(nscsr,' student-course rows',sep=""))
  
  data <- course.index(data) #index the courses for speed
  
  ncrse <- length(data$CRSE_ID[!duplicated(data$CRSE_ID)]) #only consider courses our students actually took!
  
  data       <- data[order(data$ANONID), ]
  data$count <- sequence(rle(as.vector(data$ANONID))$lengths)
  
  nstart <- which(data$count == 1)
  ntot   <- length(data$count)
  
  #Number of students and IDs
  nid <- length(data$ANONID[!duplicated(data$ANONID)])
  bigmtx  <- mat.or.vec(nid,ncrse)
  ANONID  <- mat.or.vec(nid,1)
  MAJOR1  <- ANONID
  MAJOR1[] <- NA
  MAJOR2  <- MAJOR1
  MAJOR3  <- MAJOR1
  ADMIT   <- MAJOR1
  
  for (i in 1:nid)
  {
    #Get the indices of the people in these classes.
    start_ind <- nstart[i]
    if (i < nid){stop_ind  <- nstart[i+1]-1}
    if (i == nid){stop_ind <- ntot}
    ind <- c(start_ind:stop_ind)
    bigmtx[i,data$COURSE_IND[ind]] <- 1
    ANONID[i] <- data$ANONID[start_ind]
    MAJOR1[i]  <- as.character(data$MAJOR1_DESCR[start_ind])
    MAJOR2[i]  <- as.character(data$MAJOR2_DESCR[start_ind])
    MAJOR3[i]  <- as.character(data$MAJOR3_DESCR[start_ind])
    
  }
  
  out <- data.frame(ANONID,MAJOR1,MAJOR2,MAJOR3,bigmtx)
  jj  <- build.network(out,sc,WRITE_GELPHI=TRUE,NETNAME=NETNAME)
  
  return(jj)
  
}

#use the courses to index a matrix.
course.index <- function(data)
{
  data       <- data[order(data$CRSE_ID), ]
  data$count <- sequence(rle(as.vector(data$CRSE_ID))$lengths)
  
  nstart <- which(data$count == 1)
  ntot   <- length(data$count)
  
  #Number of students and IDs
  nid <- length(data$CRSE_ID[!duplicated(data$CRSE_ID)])
  COURSE_IND <- mat.or.vec(ntot,1)
  
  for (i in 1:nid)
  {
    #Get the indices of the people in these classes.
    start_ind <- nstart[i]
    if (i < nid){stop_ind  <- nstart[i+1]-1}
    if (i == nid){stop_ind <- ntot}
    ind <- c(start_ind:stop_ind)
    COURSE_IND[ind] <- i
  }
  
  out <- data.frame(data,COURSE_IND) 
  
  return(out)
}


#This builds the covariance matrix and the adjacency matrix, 
#and makes several measurements
build.network <- function(inmtx,sc,WRITE_GELPHI=FALSE,NETNAME='TEST')
{
  
  library(igraph)
  adjmtx <- as.matrix(inmtx[,!names(inmtx) %in% c("ANONID","MAJOR1","MAJOR2","MAJOR3")])
  cmtx <- adjmtx %*% t(adjmtx)
  cmtx <- cov2cor(cmtx)
  #compute the communities
  temp <- graph.adjacency(cmtx,weighted=TRUE,mode='upper',diag=FALSE)

  print('computing communities')
  gcomm <- leading.eigenvector.community(temp)
  #gcomm <- edge.betweenness.community(temp)
  
  #create simple node properties
  COMM   <- as.numeric(membership(gcomm))
  MAJOR1    <- inmtx$MAJOR1
  MAJOR2    <- inmtx$MAJOR2
  MAJOR3    <- inmtx$MAJOR3
  
  ANONID <- inmtx$ANONID
  ID     <- c(1:length(ANONID))
  
  #convert the network and measures to someething to input to GELPHI
  if (WRITE_GELPHI == TRUE)
  {

    elist <- data.frame(get.edgelist(temp) , round(E(temp)$weight, 3 ))
    names(elist) <- c('SOURCE','TARGET','WEIGHT')
    d <- elist$WEIGHT > 0.5
  
    elist <- elist[which(d),]
    
    print(length(elist$WEIGHT))
    TYPE <- 'Undirected'
    elist <- cbind(elist,TYPE)
    
    outtab <- data.frame(ID,COMM,MAJOR1,MAJOR2,MAJOR3)
  
    #now write these out for gelphi to visualize
    dir <- '/Users/bkoester/Google Drive/code/REBUILD/course_patterns/gephi/'
    #dir <- '/home/bkoester/communities/data/'
    write.table(outtab,paste(dir,NETNAME,'nodes.csv',sep=""),quote=FALSE,sep=",",row.names=FALSE)
    write.table(elist,paste(dir,NETNAME,'edges.csv',sep=""),quote=FALSE,sep=",",row.names=FALSE)
  }
    
  #create table for output
  #tab <- data.frame(COMM,MAJOR1)
  outtab <- data.frame(ANONID,outtab)
  #print(table(tab))
  #print(prop.table(table(tab),1))
  
  #rematch to courses for course analysis
  #scnew <- merge(tab,sc,by='EMPLID',all.x=TRUE)
  #course.by.class.analysis(scnew,SUB='ALL')
  

  return(temp)
}

#EconBS/BA analysis redux
#sr: student record.
#net: a data framed edgelist. I've been doing these by cohort 
#dir <- '/Users/bkoester/Box Sync/networks/individual_by_school/'
#net <- read.table(paste(dir,'ULSA_TERM1860_ALL.csv',sep=""),sep=",",header=TRUE)

econ.analysis <- function(sr,net)
{
  
  keep <- c("EMPLID","MAJOR1_DESCR","MAJOR2_DESCR","MAJOR3_DESCR",
            "ADMIT_TYPE","SEX","ETHNIC_GROUP_DESCRSHORT","PRNTLVLED")
  e <- (grepl('Econ',sr$MAJOR1_DESCR) | grepl('Econ',sr$MAJOR2_DESCR) | grepl('Physics',sr$MAJOR1_DESCR)) & 
        sr$ADMIT_TERM == 1860 & sr$ADMIT_TYPE == 'Freshman'
  sr <- sr[which(e),names(sr) %in% keep]
  EMPLID <- sr$EMPLID
  
  #Now cleanout the edgelist
  net <- merge(net,EMPLID,by.x='TARGET',by.y=1,all.y=TRUE)
  net <- merge(net,EMPLID,by.x='SOURCE',by.y=1,all.y=TRUE)
  e   <- !is.na(net$SOURCE) & !is.na(net$TARGET)
  net <- net[which(e),]
  
  print('converting to graph object')
  gobj <- graph.data.frame(net,directed=FALSE)
  E(gobj)$weight <- E(gobj)$WEIGHT
  #kk <- shortest_paths(gobj,id,to=V(gobj),"all")#,weights=NULL)
  
  print('computing distances')
  ndist        <- distances(gobj)
  rownames(ndist) <- names(V(gobj))
  colnames(ndist) <- names(V(gobj))
  
  #compute some node-wise network stuff
  
  print('computing communities')
  print(Sys.time())
  COMM_LE <- as.numeric(membership(leading.eigenvector.community(gobj,weights=NA)))
  COMM_LE_WT <- as.numeric(membership(leading.eigenvector.community(gobj,weights=NULL)))
  #COMM_EB <- edge.betweenness.community(gobj)
  print(Sys.time())
  
  BETW       <- as.numeric(betweenness(gobj,weights=NA))
  BETW_WT    <- as.numeric(betweenness(gobj,weights=NULL))
  DEG        <- as.numeric(degree(gobj))
  EMPLID     <- as.numeric(names(V(gobj)))
  
  netstuff <- data.frame(EMPLID,COMM_LE,COMM_LE_WT,BETW,BETW_WT,DEG)
  sr <- merge(sr,netstuff,by='EMPLID',all=TRUE)
  
  #final setup for writing to disk, use in GELPHI
  outdir <- '/Users/bkoester/Google Drive/code/REBUILD/course_patterns/gephi/'
  names(sr)[1] <- "ID"
  TYPE <- 'Undirected'
  net <- cbind(net,TYPE)
  NETNAME <- 'kar_econ_'
  write.table(sr,paste(outdir,NETNAME,'nodes.csv',sep=""),quote=FALSE,sep="\t",row.names=FALSE)
  write.table(net,paste(outdir,NETNAME,'edges.csv',sep=""),quote=FALSE,sep="\t",row.names=FALSE)
  
  #finally, save the edgelist
  #TYPE <- 'Undirected'
  #net <- cbind(net,TYPE)
  
  return(ndist)
}



#Compute distances or communities
#data are the edgelists, which we can make an object before or after.
#be sure ther is a 'weights' attribute for the graph object
#1) pick two groups, measure their average distance from one another, and find the outliers
#2) use edge/betweenness algorithms to select individuals at cut points.
single.edgelist.analysis <- function(id,gobj,sr)
{
  #gobj <- graph.data.frame(data)
  #pmtx <- shortest.paths(gobj)
    
  #kk <- shortest_paths(gobj,id,to=V(gobj),"all")#,weights=NULL)
  kk  <- distances(gobj,id,to=V(gobj),"all")#,weights=NA)
  #rownames(kk) <- names(V(gobj))
  colnames(kk) <- names(V(gobj))
  
  #m <- betweenness(gobj,weights=NA)
  
  #nnb <- length(kk$vpath)
  
  for (i in 1:0)
  {
    #print(length(kk$vpath[i][[1]]))
    #print(kk$vpath[i])
    #if (length(kk$vpath[i][[1]])  > 3)
      #ids <- as.numeric(names(kk$vpath[i][[1]]))
      #out <- merge(sr,ids,by.x='EMPLID',by.y=1,all.y=TRUE)
      #View(out)
      #scan()
    #}
  }
  
  return(kk)
}

#give this a list of IDs, it will return an edgelist of all connections between IDs.
read.kar.weighted <- function(idlist,WRITE_TABLE=FALSE)
{
  
  dir <- '/Users/bkoester/Box Sync/networks/individual_by_school/jan2016/'
  nid <- length(idlist)
  tot <- 0
  
  #First count the number of lines and then define and fill the structure. 
  #I'm pretty sure this is the fastest way to do this.
  
  print('counting, defining edge structure')
  for (i in 1:nid)
  {
    mm <- paste('ULSA_',idlist[i],'.tsv',sep="")
    #if (i %% 100 == 0){print(i)}
    #print(length(readLines(paste(dir,mm,sep=""))))
    if (file.exists(paste(dir,mm,sep="")))
    {
      nlines <- length(readLines(paste(dir,mm,sep="")))
      if (nlines > 1)
      {
        tot <- tot + nlines
      }
    }
  }
  
  SOURCE <- mat.or.vec(tot,1)
  TARGET <- SOURCE
  WEIGHT <- SOURCE
  count <- 1
  
  print('filling edgelist')
  
  for (i in 1:nid)
  {
    mm <- paste('ULSA_',idlist[i],'.tsv',sep="")
    if (i %% 100 == 0){print(i)}
    #print(length(readLines(paste(dir,mm,sep=""))))
    if (file.exists(paste(dir,mm,sep="")))
    {
      nlines <- length(readLines(paste(dir,mm,sep="")))
      if (nlines > 1)
      {
        temp <- read.table(paste(dir,mm,sep=""),header=FALSE,sep="\t")
        SOURCE[count:(count+nlines-1)] <- idlist[i]
        TARGET[count:(count+nlines-1)] <- temp$V1
        WEIGHT[count:(count+nlines-1)] <- temp$V2
        count <- count+nlines
      }
    }
  }
  
  out <- data.frame(SOURCE,TARGET,WEIGHT)
  
  print('cleaning out non-ID targets')
  out <- merge(out,idlist,by.x='TARGET',by.y=1,all.y=TRUE)
  
  if (WRITE_TABLE != FALSE) 
  {
    dirout <- '/Users/bkoester/Box Sync/networks/individual_by_school/'
    write.table(out,paste(dirout,WRITE_TABLE,'.csv',sep=""),sep=",",row.names=FALSE,quote=FALSE)
  }
  return(out)
}

#compute/display statistics of courses taken
course.by.class.analysis <- function(data,SUB='ALL')
{
  
  if (SUB != 'ALL')
  {
    e <- data$SUBJECT == SUB
    data <- data[e,]    
  }
  uComm <- data$COMM[!duplicated(data$COMM)]
  nComm <- length(uComm)
  uMaj <- as.character(data$MAJ[!duplicated(data$MAJ)])
  nMaj <- length(uMaj)
  
  print('****network classifiers****')
  for (i in 1:nComm)
  {
    print(uComm[i])
    e <- data$COMM == uComm[i]
    cname <- paste(data$SUBJECT[e],data$CATALOG_NBR[e],sep="")
    print(summary(as.factor(cname))[1:20])
  }
  print('****major classifiers****')
  for (i in 1:nMaj)
  {
    print(uMaj[i])
    e <- as.character(data$MAJ) == uMaj[i]
    cname <- paste(data$SUBJECT[e],data$CATALOG_NBR[e],sep="")
    print(summary(as.factor(cname))[1:20])
  }
  
}

#this is for cutting on enrollment
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
