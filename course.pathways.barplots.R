#####################################################################################
#1) What courses do students who take this course take before, during, and after?
#2) What do these students ultimately major in?

#FUNCTION: course.pathway
#PURPOSE : To analyze, present basic analysis of the student's courses and his/her major
#INPUTS  : sr - student record table
#          sc - student course table
#          SUBJECT     - course subject
#          CATALOG_NBR - course catalog number
#          TERM_RANGE  - lower and upper limits of terms to be analyzed
#          DEPT        - Which majors to consider for TERM-by-TERM statistics (Physics is default)
#          PDF         - Write plots to PDF. Default is TRUE. Plots go to 'course_pathway_barplots.pdf' in CWD.
#PACKAGES: Treemap
#OUTPUTS : Plots sent to course_pathway_barplots.pdf in the CWD.
# example: course.pathway(sr,sc,"PHYSICS",140,TERM_RANGE=c(132,156), PDF=FALSE)
#sr<-student.record.anon.MOOC.FA.2015.orig
#sc<-student.course.anon.MOOC.FA.2015.orig
#####################################################################################


course.pathway <- function(sr,sc,SUBJECT,CATALOG_NBR,
                            TERM_RANGE=c(152,156),
                            PDF=TRUE)
    
# testing start
 #SUBJECT<-"PHYSICS"
#CATALOG_NBR<-140
#TERM_RANGE=c(142,156)
#PDF <- TRUE
# course.pathway(sr,sc,"PHYSICS",140,TERM_RANGE=c(152,156), PDF=TRUE)

# testing end
    
{
  
  ################################################################
  #1)  What courses do students who take this course take before, during, and after?
  
  
  #Get all unique ANONID of students who took this course in the TERM_RANGE
  
  e <- sc$SUBJECT == SUBJECT & sc$CATALOG_NBR == CATALOG_NBR & sc$TERM >= TERM_RANGE[1] & sc$TERM <= TERM_RANGE[2]

  st <- unique(sc$ANONID[which(e)])
  
  
  #Reduce original sc to only those who took the course under consideration
  sc.m<-sc[sc$ANONID %in% st,c( "ANONID","SUBJECT","CATALOG_NBR","TERM")]

  
  #Add a new column of subject + course number
  sc.m$course <-paste(sc.m$SUBJECT, sc.m$CATALOG_NBR,sep = " ")

  
  list.b<-c() # list of courses taken before
  list.s<-c() # list of courses taken at the same time
  list.a<-c() # list of courses taken after

  
  for(i in st){
      sc.m.a<-subset(sc.m, ANONID==i)
      l<-dim(sc.m.a)[1]
      tm<-sc.m.a$TERM[sc.m.a$SUBJECT == SUBJECT & sc.m.a$CATALOG_NBR == CATALOG_NBR]
      
      for(j in 1:l){
          if (sc.m.a$TERM[j]<tm[1]) 
          {list.b<-c(list.b, sc.m.a$course[j])} 
    
          if (sc.m.a$TERM[j]==tm[1] & (sc.m.a$SUBJECT[j] != SUBJECT & sc.m.a$CATALOG_NBR[j] != CATALOG_NBR)) 
          {list.s<-c(list.s, sc.m.a$course[j])} 
          
          if (sc.m.a$TERM[j]>tm[1]) 
          {list.a<-c(list.a, sc.m.a$course[j])} 
      
      }
  }

sum.b<-summary(as.factor(list.b))
sum.b.o<-sum.b[order(-sum.b)]
if (length(unique(list.b))>9){
sum.b.o.10<-sum.b.o[c(1:10)]} else {sum.b.o.10<-sum.b.o}



sum.s<-summary(as.factor(list.s))
sum.s.o<-sum.s[order(-sum.s)]
if (length(unique(list.s))>9){
    sum.s.o.10<-sum.s.o[c(1:10)]} else {sum.s.o.10<-sum.s.o}


sum.a<-summary(as.factor(list.a))
sum.a.o<-sum.a[order(-sum.a)]
if (length(unique(list.a))>9){
    sum.a.o.10<-sum.a.o[c(1:10)]} else {sum.a.o.10<-sum.a.o}

if (length(unique(list.b))>0){
par(mar=c(5,5,4,2))
barplot(sum.b.o.10,las=1,horiz=TRUE,xlab='Number of Courses',cex.names=0.5,
        main=paste('Top 10 Courses Taken Before: ',SUBJECT, CATALOG_NBR,sep=" "),
        xlim=c(0,sum.b.o.10[[1]]))} else {print ("No Courses Taken Before. Extend TERM range")}


if (length(unique(list.s))>0){
par(mar=c(5,5,4,2))
barplot(sum.s.o.10,las=1,horiz=TRUE,xlab='Number of Courses',cex.names=0.5,
        main=paste('Top 10 Courses Taken At the Same Time: ',SUBJECT, CATALOG_NBR,sep=" "),
        xlim=c(0,sum.s.o.10[[1]]))} else {print ("No Courses Taken At the Same Time. Extend TERM range")}

if (length(unique(list.a))>0){
par(mar=c(5,5,4,2))
barplot(sum.a.o.10,las=1,horiz=TRUE,xlab='Number of Courses',cex.names=0.5,
        main=paste('Top 10 Courses Taken After: ',SUBJECT, CATALOG_NBR,sep=" "),
        xlim=c(0,sum.a.o.10[[1]]))} else {print ("No Courses Taken After. Extend TERM range")}




  ################################################################
  #2) What are their majors (more specifically, from which DEPTs?)
  
  #Get all instances of this course in the TERM_RANGE
  e <- sc$SUBJECT == SUBJECT & sc$CATALOG_NBR == CATALOG_NBR & sc$TERM >= TERM_RANGE[1] & sc$TERM <= TERM_RANGE[2]
  sc.n <- sc[which(e),]
 
  #remove duplicates
  sc.n.a <- sc.n[!duplicated(sc$ANONID),]
  
  #merge with the student record to get characteristics of students
  sub <- merge(sc.n.a,sr,by='ANONID',all.x=TRUE)
  
  # - barplot
  allmajors <- c(as.character(sub$MAJOR1_DEPT),as.character(sub$MAJOR2_DEPT),as.character(sub$MAJOR3_DEPT))
  gg <- summary(as.factor(allmajors))
  gg <- gg[order(-gg)]
if (length(unique(allmajors))>10){
    majtab <- gg[c(2:11)]} else {majtab<-gg[-1]}

length(unique(allmajors))

if (length(unique(allmajors))>1){
  par(mar=c(5,10,4,2))
    barplot(majtab,las=1,horiz=TRUE,xlab='Number of Majors',cex.names=0.5,
          main=paste('Top 10 Majors: ',SUBJECT, CATALOG_NBR,sep=" "),
          xlim=c(0,majtab[[1]]))} else {print( "No Majors. Extend TERM range")}

  
#Writing PDF output file
  if (PDF == TRUE){pdf('course_pathway_barplots.pdf',width=11,height=7)
                   par(mfrow = c(2,2))
                    
                   if (length(unique(list.b))>0){
                       par(mar=c(5,5,4,2))
                       barplot(sum.b.o.10,las=1,horiz=TRUE,xlab='Number of Courses',cex.names=0.5,
                               main=paste('Top 10 Courses Taken Before: ',SUBJECT, CATALOG_NBR,sep=" "),
                               xlim=c(0,sum.b.o.10[[1]]))} 
                   
                   
                   if (length(unique(list.s))>0){
                       par(mar=c(5,5,4,2))
                       barplot(sum.s.o.10,las=1,horiz=TRUE,xlab='Number of Courses',cex.names=0.5,
                               main=paste('Top 10 Courses Taken At the Same Time: ',SUBJECT, CATALOG_NBR,sep=" "),
                               xlim=c(0,sum.s.o.10[[1]]))} 
                   
                   if (length(unique(list.a))>0){
                       par(mar=c(5,5,4,2))
                       barplot(sum.a.o.10,las=1,horiz=TRUE,xlab='Number of Courses',cex.names=0.5,
                               main=paste('Top Courses Taken After: ',SUBJECT, CATALOG_NBR,sep=" "),
                               xlim=c(0,sum.a.o.10[[1]]))} 
                   
                   
                   if (length(unique(allmajors))>1){
                       par(mar=c(5,10,4,2))
                       barplot(majtab,las=1,horiz=TRUE,xlab='Number of Majors',cex.names=0.5,
                               main=paste('Top 10 Majors: ',SUBJECT, CATALOG_NBR,sep=" "),
                               xlim=c(0,majtab[[1]]))} 
                   
                   dev.off()
  }

}
