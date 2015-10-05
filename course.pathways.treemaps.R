#####################################################################################
#1) What courses do students who take this course take before, during, and after?
#2) What do these students ultimately major in?

#FUNCTION: course.pathway
#PURPOSE : To analyze, present basic analysis of the student's courses and his/her major given enrollment in course=(SUBJECT,CATALOG_NBR)
#INPUTS  : sr - student record table
#          sc - student course table
#          SUBJECT     - course subject
#          CATALOG_NBR - course catalog number
#          TERM_RANGE  - lower and upper limits of terms to be analyzed
#          DEPT        - Which majors to consider for TERM-by-TERM statistics (Physics is default)
#          PDF         - Write plots to PDF. Default is TRUE. Plots go to 'course_pathways_treemap.pdf' in CWD.
#PACKAGES: Treemap
#OUTPUTS : Plots sent to course_pathway_treemap.pdf in the CWD.
#EXAMPLE: course.pathway.treemaps(sr,sc,"PHYSICS",140,TERM_RANGE=c(132,156), PDF=FALSE)
#sr<-student.record.anon.MOOC.FA.2015.orig
#sc<-student.course.anon.MOOC.FA.2015.orig
#####################################################################################

course.pathway.treemaps <- function(sr,sc,SUBJECT,CATALOG_NBR,TERM_RANGE=c(100,156),PDF=TRUE)
{
  
  #Do some basic error checking
  if (dim(sr)[1] < 100)
  {
    print('student record too small or non-existent')
    return()
  }
  if (dim(sc)[1] < 100)
  {
    print('student course to small or non-existent')
    return()
  }
  if (TERM_RANGE[1] < 4 )
  {
    print('lower bound on term range must be >= 4')
    return()
  }
  
  library(treemap)
  
  ################################################################
  #1)  What courses do students who take this course take before, during, and after?
  
  
  #Get all unique ANONID of students who took this course in the TERM_RANGE
  e <- sc$SUBJECT == SUBJECT & sc$CATALOG_NBR == CATALOG_NBR & 
    sc$TERM >= TERM_RANGE[1] & sc$TERM <= TERM_RANGE[2]
  if (length(which(e)) == 0)
  {
    print("invalid subject and/or catalog number...or maybe your term range is too narrow")
    return()
  }
  st <- unique(sc$ANONID[which(e)])
  scc <- sc[which(e),c("ANONID","TERM")]
  names(scc) <- c("ANONID","TERM2")
  
  
  #Reduce original sc to only those who took the course under consideration, 
  sc.m<-sc[sc$ANONID %in% st,c( "ANONID","SUBJECT","CATALOG_NBR","GRD_PTS_PER_UNIT","GPAO","DIV","TERM")]
  sc.m <- merge(sc.m,scc,by='ANONID',all.x=TRUE)
  SEQTERM    <- mat.or.vec(length(sc.m$ANONID),1)
  sc.m <- data.frame(sc.m,SEQTERM)
  
  eb   <- which(sc.m$TERM < sc.m$TERM2)
  ea   <- which(sc.m$TERM > sc.m$TERM2)
  es   <- which(sc.m$TERM == sc.m$TERM2)
  lenb <- length(eb)
  lena <- length(ea)
  lens <- length(es)
  
  sc.m$SEQTERM[eb] <- 1
  sc.m$SEQTERM[ea] <- 2
  
  #Add a new column of subject + course number
  sc.m$course <-paste(sc.m$SUBJECT, sc.m$CATALOG_NBR,sep = " ")
  #b           <- sc.m$SUBJECT != SUBJECT & sc.m$CATALOG_NBR != CATALOG_NBR
  b           <- sc.m$course != paste(SUBJECT, CATALOG_NBR,sep = " ")
  sc.m        <- sc.m[which(b),]
  
  list.b <- sc.m[which(sc.m$SEQTERM == 1),]
  list.s <- sc.m[which(sc.m$SEQTERM == 0),]
  list.a <- sc.m[which(sc.m$SEQTERM == 2),]
  
  #Send the structures to the course treemap plotting routine.
  pdf.index<-as.character(1)  
  str.b=paste('Grades in Courses Taken Before',SUBJECT ,CATALOG_NBR, sep=" ") 
  basic.course.treemap(list.b,SUBNAME=str.b,BYDIV=TRUE,PDF,pdf.index)
  
  pdf.index<-as.character(2) 
  str.s=paste('Grades in Courses Taken at the Same Time',SUBJECT ,CATALOG_NBR, sep=" ") 
  basic.course.treemap(list.s,SUBNAME=str.s,BYDIV=TRUE,PDF,pdf.index)
  
  pdf.index<-as.character(3) 
  str.a=paste('Grades in Courses Taken After',SUBJECT ,CATALOG_NBR, sep=" ") 
  basic.course.treemap(list.a,SUBNAME=str.a,BYDIV=TRUE,PDF,pdf.index)
  
  
  
  ################################################################
  #2) What are their majors (more specifically, from which DEPTs?)
  
  pdf.index<-as.character(4) 
  str <- paste("Students Who Took ",SUBJECT,CATALOG_NBR, sep=" ")
  basic.major.treemap(sr,sc,SUBJECT,CATALOG_NBR,SUBNAME=str,
                      TERM_RANGE=TERM_RANGE,MAJOR_NUM=1,PDF,pdf.index)
  
  
}

#####################################################################################
#FUNCTION: basic.course.treemap
#PURPOSE : Create data structure and plot treemap of courses.
#INPUTS  : data        - a list of student-course pairs to be consolidated into a treemap. 
#          SUBNAME     - A title to append to the plot
#          PDF         - Write plots to PDF. Default is TRUE. Plots go to 'course_pathways_treemap.pdf' in CWD.
#          BYDIV       - TRUE by default. Include course division in plot hierarchy. FALSE clusters courses by level (100,200, etc)
#PACKAGES: Treemap
#OUTPUTS : Plots sent to course_pathway_treemap.pdf in the CWD.
#EXAMPLE: basic.course.treemap(data,SUBNAME='Course Treemap',BYDIV=TRUE,PDF=TRUE,pdf.index))
#####################################################################################
basic.course.treemap <- function(data,SUBNAME='Course Treemap',BYDIV=TRUE,PDF=TRUE,pdf.index)
{
  
  library(treemap) 
  
  crse <- paste(data$SUBJECT,data$CATALOG_NBR,sep="")
  division <- mat.or.vec(length(crse),1)
  #View(data)
  if (BYDIV == TRUE)
  {
    e <- data$DIV == 'S'
    division[e]   <- 'NS'
    e <- data$DIV == 'H'
    division[e]   <- 'Hu'
    e <- data$DIV == 'SS'
    division[e]   <- 'SS'
    e <- data$DIV == 'O'
    division[e]   <- 'Oth'
    e <- data$DIV == 'P'
    division[e]   <- 'Pro'
    e <- data$DIV == 'E'
    division[e]   <- 'Eng'
  }
  else
  {
    e <- as.character(data$CATALOG_NBR) >= 100 & as.character(data$CATALOG_NBR) < 200
    division[e] <- '100'
    e <- as.character(data$CATALOG_NBR) >= 200 & as.character(data$CATALOG_NBR) < 300
    division[e] <- '200'
    e <- as.character(data$CATALOG_NBR) >= 300 & as.character(data$CATALOG_NBR) < 400
    division[e] <- '300'
    e <- as.character(data$CATALOG_NBR) >= 400 & as.character(data$CATALOG_NBR) < 500
    division[e] <- '400'
  }
  
  data <- data.frame(data,crse,division)
  data       <- data[order(data$crse), ]
  data$count <- sequence(rle(as.vector(data$crse))$lengths)
  
  ncrse  <- length(data$crse[!duplicated(data$crse)])
  nstart <- which(data$count == 1)
  ntot   <- length(data$crse) 
  dept   <- mat.or.vec(ncrse,1)
  division <- mat.or.vec(ncrse,1)
  freq   <- mat.or.vec(ncrse,1) 
  stem   <- mat.or.vec(ncrse,1)
  grade  <- stem
  crse   <- stem
  subject <- stem
  grade_penalty   <- stem
  
  for (i in 1:ncrse)
  {
    start_ind <- nstart[i]
    if (i < ncrse){stop_ind  <- nstart[i+1]-1}
    if (i == ncrse){stop_ind <- ntot}
    
    ind  <- c(start_ind:stop_ind)
    
    division[i] <- as.character(data$division[start_ind])
    freq[i]     <- length(ind)
    crse[i]     <- paste(as.character(data$crse[start_ind])," (",freq[i],")",sep="")
    subject[i]  <- as.character(data$SUBJECT[start_ind])
    if (freq[i] > 2)
    {
      #stem[i]     <- mean(data$stem[ind],na.rm=TRUE)
      grade_penalty[i]         <- median(data$GRD_PTS_PER_UNIT[ind]-data$GPAO[ind],na.rm=TRUE)
      grade[i] <- median(data$GRD_PTS_PER_UNIT[ind],na.rm=TRUE)
    }
    
    #if (division[i] == 'Human'){division[i] <- paste('Human',neh,sep=" ")}
    #if (division[i] == 'SocSci'){division[i] <- paste('SocSci',nes,sep=" ")}
    #if (division[i] == 'NatSci'){division[i] <- paste('NatSci',nen,sep=" ")}
    #if (division[i] == 'Other'){division[i] <- paste('Other',neo,sep=" ")}
    
  }
  
  out <- data.frame(crse,subject,division,grade,grade_penalty,freq)
  
  e   <- out$freq > 250
  out <- out[order(-out$freq),]
  e   <- out$grade > 2.5 & out$grade < 4.0
  out <- out[which(e),]
  
  if (PDF == FALSE){
    treemap(out,c("division","crse"),vSize='freq',palette='Spectral',
            vColor='grade',type='manual',fontsize.labels=c(30,10),range=c(2.5,4.0),
            title=paste('Student Grades by Course: ',SUBNAME,sep=" "))}
  
  if (PDF == TRUE){pdf(paste('course_pathway_treemap_',pdf.index,".pdf",sep=""),width=11,height=7)
    treemap(out,c("division","crse"),vSize='freq',palette='Spectral',
            vColor='grade',type='manual',fontsize.labels=c(30,10),range=c(2.5,4.0),
            title=paste('Student Grades by Course: ',SUBNAME,sep=" "))
    dev.off()
    
  }
  
  
  #if (PDF == TRUE){dev.off()}
  #return(out)
}

#####################################################################################
#FUNCTION: basic.major.treemap
#PURPOSE : Create data structure and plot treemap of courses.
#INPUTS  : sr          - a student record table
#          sc          - a student course table
#          SUBJECT     - subject of course for which majors treemap will be built          
#          CATALOG_NBR - catalog nbr of course for which majors treemap will be built
#          SUBNAME     - A title to append to the plot
#          TERM_RANGE  - terms to consider
#          MAJOR_NUM   - which of the three major columns to consider. 1 is the default
#          PDF         - Write plots to PDF. Default is TRUE. Plots go to 'course_pathways_treemap.pdf' in CWD.
#          BYDIV       - TRUE by default. Include course division in plot hierarchy. FALSE clusters courses by level (100,200, etc)
#PACKAGES: Treemap
#OUTPUTS : Plots sent to course_pathway_treemap.pdf in the CWD.
#EXAMPLE: basic.major.treemap(data,SUBNAME='Course Treemap',BYDIV=TRUE,PDF=TRUE,pdf.index))
#####################################################################################
basic.major.treemap <- function(sr,sc,SUBJECT,CATALOG_NBR,SUBNAME="ALL",
                                TERM_RANGE=c(4,156),MAJOR_NUM=1,PDF=TRUE,pdf.index)
{
  library(treemap)
  #pick out the course to evaluate and merge with the student record
  e <- sc$CATALOG_NBR == CATALOG_NBR & sc$SUBJECT == SUBJECT & 
    sc$TERM <= TERM_RANGE[2] &  sc$TERM >= TERM_RANGE[1]
  sc <- sc[which(e),]
  data <- merge(sc,sr,by='ANONID',all.x=TRUE)
  
  #Now massage 
  DDIV <- read.dept.division()
  data <- data[,names(data) %in% c("EMPLID","SUBJECT","CATALOG_NBR","GRD_PTS_PER_UNIT","GPAO",
                                   "MAJOR1_DEPT","MAJOR2_DEPT","MAJOR3_DEPT")]
  View(data)
  if (MAJOR_NUM == 1)
  {
    data <- merge(data,DDIV,by.x='MAJOR1_DEPT',by.y='DEPT',all.x=TRUE)
    data <- data[!is.na(data$MAJOR1_DEPT),]
    maj  <- data$MAJOR1_DEPT[!duplicated(data$MAJOR1_DEPT)]
    MAJNAME <- data$MAJOR1_DEPT
  }
  if (MAJOR_NUM == 2)
  {
    data <- merge(data,DDIV,by.x='MAJOR2_DEPT',by.y='DEPT',all.x=TRUE)
    data <- data[!is.na(data$MAJOR2_DEPT),]
    maj  <- data$MAJOR2_DEPT[!duplicated(data$MAJOR2_DEPT)]
    MAJNAME <- data$MAJOR2_DEPT
  }
  if (MAJOR_NUM == 3)
  {
    data <- merge(data,DDIV,by.x='MAJOR3_DEPT',by.y='DEPT',all.x=TRUE)
    data <- data[!is.na(data$MAJOR3_DEPT),]
    maj  <- data$MAJOR3_DEPT[!duplicated(data$MAJOR3_DEPT)]
    MAJNAME <- data$MAJOR3_DEPT
  }
  
  data <- data.frame(data,MAJNAME)
  ncrse    <- length(maj)
  division <- as.character(data$DIV)
  
  e <- is.na(division)
  division[e] <- 'No Degree'
  data <- data.frame(data,division)
  
  data       <- data[order(data$MAJNAME), ]
  data$count <- sequence(rle(as.vector(data$MAJNAME))$lengths)
  
  ncrse  <- length(data$MAJNAME[!duplicated(data$MAJNAME)])
  nstart <- which(data$count == 1)
  ntot   <- length(data$MAJNAME) 
  dept   <- mat.or.vec(ncrse,1)
  division <- mat.or.vec(ncrse,1)
  freq   <- mat.or.vec(ncrse,1) 
  stem   <- mat.or.vec(ncrse,1)
  grade  <- stem
  grade_penalty <- stem
  crse   <- stem
  major <- stem
  
  for (i in 1:ncrse)
  {
    start_ind <- nstart[i]
    if (i < ncrse){stop_ind  <- nstart[i+1]-1}
    if (i == ncrse){stop_ind <- ntot}
    
    ind  <- c(start_ind:stop_ind)
    
    division[i] <- as.character(data$division[start_ind])
    freq[i]     <- length(ind)
    major[i]  <- paste(as.character(data$MAJNAME[start_ind]),"(",freq[i],")",sep="")
    if (freq[i] > 2)
    {
      #stem[i]     <- mean(data$stem[ind],na.rm=TRUE)
      #grade[i]     <- median(data$GRD_PTS_PER_UNIT[ind])
      grade[i]     <- mean(data$GRD_PTS_PER_UNIT[ind])
      grade_penalty[i]      <- median(data$GRD_PTS_PER_UNIT[ind]-data$GPAO[ind],na.rm=TRUE)
    }
    
    #if (division[i] == 'Human'){division[i] <- paste('Human(',neh,')',sep="")}
    #if (division[i] == 'SocSci'){division[i] <- paste('SocSci(',nes,')',sep="")}
    #if (division[i] == 'NatSci'){division[i] <- paste('NatSci(',nen,')',sep="")}
    #if (division[i] == 'Engin'){division[i] <- paste('Engin(',nee,')',sep="")}
    
    
  }
  
  out <- data.frame(major,division,grade,grade_penalty,freq)
  e   <- out$grade > 2.5 & out$grade < 4.0
  out <- out[which(e),]
  
  
  
  if (PDF == FALSE){treemap(out,c("division","major"),vSize='freq',palette='Spectral',overlap.labels=1,
                            vColor='grade',type='manual',fontsize.labels=c(50,10),range=c(2.5,4.0),
                            title=paste(SUBNAME,': Course Grades by Major',sep=" "))}
  
  
  if (PDF == TRUE){pdf(paste('course_pathway_treemap_',pdf.index,".pdf",sep=""),width=11,height=7)
    treemap(out,c("division","major"),vSize='freq',palette='Spectral',overlap.labels=1,
            vColor='grade',type='manual',fontsize.labels=c(50,10),range=c(2.5,4.0),
            title=paste(SUBNAME,': Course Grades by Major',sep=" "))
    dev.off()
    
  }
  
  
  
  
  #out$freq <- out$freq/sum(out$freq)
  #out <- out[order(-out$freq),]
  #print(xtable(out[,names(out) %in% c('major','grade','freq')]),floating=TRUE)
  
  #return(out)
}
####
###

###################
#Supplementary Data:
#We could add these files as extra colums to the student record, but 
#it's small enough that we can just save it here so that we have flexibility to use it
#without having to ship another file.

read.dept.division <- function()
{
  
  DEPT <- c("Electrical Engr & Computer Sci",
            "College of Architecture & Urban Planning",
            "Mathematics Department",
            "School of Business Administration",
            "Sociology Department",
            "Materials Science & Engineering",
            "Mech Eng & Applied Mech Dept",
            "Anthropology Department",
            "Department of Afro-American and African Studies",
            "Women's Studies Department",
            "American Culture Program",
            "School of Information",
            "Chemical Engineering Department",
            "Industrial-Operations Engr Dep",
            "Naval Arch & Marine Engr Dept",
            "Nuclear Engr & Radiological Sci",
            "Engineering Undergraduate Educ",
            "College Of Engineering",
            "Philosophy Department",
            "Physics Department",
            "Economics Department",
            "English Language & Literature Dept",
            "Cell and Developmental Biology",
            "Biological Chemistry Departmen",
            "Statistics Department",
            "Office of International Programs",
            "College of Literature, Science, and the Arts",
            "Biostatistics Department",
            "Epidemiology Department",
            "School Of Public Health",
            "Environmental Health Sciences",
            "College Of Pharmacy",
            "Aerospace Engineering",
            "School of Music, Theatre and Dance",
            "History Department",
            "Sch Of Nat Resources & Environ",
            "Romance Languages Department",
            "Macromolecular Sci & Engr Ctr",
            "Political Science Department",
            "Pathology Department",
            "Program In Manufacturing",
            "Near Eastern Studies Department",
            "Program in the Environment",
            "Astronomy Department",
            "School Of Public Policy",
            "College of L S & A Inteflex Program",
            "Asian Languages And Cultures",
            "Medical School",
            "Chemistry Department",
            "Germanic Languages & Lit Dept",
            "Classical Studies Department",
            "Latin Amer & Carribean Studies",
            "Biomedical Engineering",
            "Psychology Department",
            "Studies In Religion",
            "Slavic Languages & Lit Dept",
            "Center for Russian, E. European & Eurasian Studies",
            "Microbiology And Immunology",
            "Center For Neuroscience",
            "Human Genetics Department",
            "Pharmacology Department",
            "Communication Studies",
            "Program In Computer Science",
            "Civil & Environmental Engr",
            "Department of Linguistics",
            "School Of Social Work",
            "Earth and Environmental Sciences",
            "Comparative Literature Program",
            "History Of Art Department",
            "Screen Arts and Cultures",
            "English Language Institute",
            "College of L S & A Residential College",
            "School Of Art And Design",
            "Program Study of Complex Systems",
            "School Of Dentistry",
            "Sweetland Center for Writing",
            "Department Of Dance",
            "Theatre And Drama",
            "Atm, Oceanic And Space Science",
            "School Of Education",
            "Biology Department",
            "Biophysics Research Division",
            "Health Behavior & Health Education Department",
            "Honors Program",
            "Center for Middle Eastern & North African Studies",
            "Medieval and Early Modern Studies",
            "School Of Nursing",
            "School Of Kinesiology",
            "LS&A First Year Seminars",
            "Health Management And Policy",
            "Physiology Department",
            "Air Force Officer Education Pg",
            "Army Officer Education Program",
            "Navy Officer Education Program",
            "Registrar's Office",
            "Lloyd Hall Scholars Program",
            "Graduate School",
            "Judaic Studies Program",
            "Engineering Study Abroad",
            "Molecular, Cellular, and Developmental Biology",
            "Museum of Art",
            "Law School",
            "Technical Communication",
            "Exhibit Museum",
            "SRC-PSM Graduate Program",
            "Comprehensive Studies Program",
            "Internal Medicine Department",
            "Medical School Administration",
            "Bioinformatics",
            "Ecology & Evolutionary Biology",
            "Cellular and Molecular Biology",
            "Dental Hygiene-Dentistry",
            "U-Move",
            "Applied Physics Program",
            "Organizational Studies",
            "International Institute",
            "Design Science Program",
            "Center For Chinese Studies",
            "Humanities Institute",
            "Cancer Biology Graduate Program",
            "Integrative Systems and Design",
            "Program in Biomedical Sciences",
            "South East Asian Studies",
            "South Asian Studies",
            "Division of Anatomical Science",
            "Pg Medicine & Hlth Prof Edu De",
            "Center For Japanese Studies",
            "Int Med-General Medicine",
            "Neurosurgery",
            "Ophthalmology Department",
            "CoE Robotics Institute",
            "Neurology Department",
            "Physical Medicine & Rehab Dept",
            "Center For West European Studi")
  
  DIV <- c("E",
           "O",
           "S",
           "P",
           "SS",
           "E",
           "E",
           "SS",
           "SS",
           "H",
           "H",
           "S",
           "E",
           "E",
           "E",
           "E",
           "E",
           "E",
           "H",
           "S",
           "SS",
           "H",
           "S",
           "S",
           "S",
           "O",
           "O",
           "S",
           "S",
           "SS",
           "S",
           "P",
           "E",
           "H",
           "H",
           "S",
           "H",
           "E",
           "SS",
           "S",
           "E",
           "H",
           "S",
           "S",
           "SS",
           "O",
           "H",
           "P",
           "S",
           "H",
           "H",
           "H",
           "E",
           "SS",
           "H",
           "H",
           "H",
           "S",
           "S",
           "S",
           "S",
           "SS",
           "S",
           "E",
           "SS",
           "SS",
           "S",
           "H",
           "H",
           "H",
           "H",
           "O",
           "H",
           "S",
           "P",
           "O",
           "H",
           "H",
           "E",
           "SS",
           "S",
           "S",
           "SS",
           "O",
           "H",
           "H",
           "P",
           "S",
           "O",
           "SS",
           "S",
           "O",
           "O",
           "O",
           "O",
           "O",
           "O",
           "H",
           "E",
           "E",
           "H",
           "P",
           "E",
           "H",
           "O",
           "O",
           "S",
           "SS",
           "S",
           "S",
           "S",
           "P",
           "O",
           "S",
           "SS",
           "SS",
           "S",
           "H",
           "H",
           "S",
           "E",
           "S",
           "H",
           "H",
           "S",
           "SS",
           "H",
           "S",
           "S",
           "S",
           "E",
           "S",
           "S",
           "H")
  
  return(data.frame(DEPT,DIV))  
}

