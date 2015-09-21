#####################################################################################
#Take the student course and student record, create grade penalty analysis of a course.
#FUNCTION: grade.penalty
#PURPOSE : Compute grade penalties, gender effects with regression and matching in a course.
#INPUTS  : sr - student record table, trimmed to include only the student group of interest.
#          sc - full student course table
#          SUBJECT     - course subject
#          CATALOG_NBR - course catalog number
#          TERM_RANGE  - lower and upper limits of terms to be analyzed
#          PDF         - Write plots to PDF. Default is TRUE. Plots go to 'course_portrain.pdf' in CWD.
#          REGRESSION  - Run a basic linear regression, return the coefficient on the gender term.
#          MATCHING    - Run a matching analysis to compare grades of matched males and females.
#          GENDER      - Show the grade penalty plot split by gender (default is TRUE)
#OUTPUTS : Plots sent to grade.penalty.pdf in the CWD. test.
#NOTES   : Uses optmatch package (https://cran.r-project.org/web/packages/optmatch/index.html) with MATCHING=TRUE
#EXAMPLE: out <- grade.penalty(sr,sc,'PHYSICS',135,REGRESSION=TRUE,MATCHING=TRUE)
#####################################################################################
grade.penalty <- function(sr,sc,SUBJECT,CATALOG_NBR,TERM_RANGE=c(4,156),PDF=FALSE,REGRESSION=FALSE,MATCHING=FALSE,GENDER=TRUE)
{  
  #SELECT the TERMS
  e    <- sc$SUBJECT == SUBJECT & 
          sc$CATALOG_NBR == CATALOG_NBR & 
          sc$TERM >= TERM_RANGE[1] & sc$TERM <= TERM_RANGE[2]

  sc <- sc[which(e),]
  terms <- sc$TERM[!duplicated(sc$TERM)]
  nterms <- length(terms)
  nst <- sum(e)

  #Merge the course and record tables together.
  data <- merge(sc,sr,by='ANONID',all.x=TRUE)
  title <- paste(SUBJECT,CATALOG_NBR,sep=" ")

  #Select the male and female subsets and compute the Cohen's D for the difference
  #in grade penatly between the two groups
  m    <- data$SEX == 'M'
  gm   <- data[m,]
  f    <- data$SEX == 'F'
  gf   <- data[f,]
  ltitle1 <- 'males: '
  ltitle2 <- 'females: '
  
  gpm_agg <- gm$GRD_PTS_PER_UNIT-gm$GPAO
  gpf_agg <- gf$GRD_PTS_PER_UNIT-gf$GPAO
 
  #Compute and plot the binned grade penalty
  fem.binned <- compute.gpa.binned.grades(gf,15)
  mal.binned <- compute.gpa.binned.grades(gm,15)
  all.binned <- compute.gpa.binned.grades(data,15)
  
  title <- paste(SUBJECT,CATALOG_NBR,"(","N = ",nst,")",sep=" ")
  if (length(terms) == 1){title <- paste(title,'(',data$TERM_DESCRSHORT[1],')',sep=" ")}

  #turn on the PDF device if PDF output is requested.
  if (PDF == TRUE){pdf(paste(SUBJECT,CATALOG_NBR,'.pdf',sep=""),width=11,height=7)}
  
  if (GENDER == TRUE)
  {
    plot.binned.grades(mal.binned,title,col='black')
    plot.binned.grades(fem.binned,title,col='red')
  }
  else 
  {
    plot.binned.grades(all.binned,title,col='black')
  }
  lines(c(0,4),c(0,4)) #The one-to-one line
  
  #Compute aggregate GPs
  dda <- compute.overall.grade.penalty(data)
  ddm <- signif(compute.overall.grade.penalty(gm),3)
  ddf <- signif(compute.overall.grade.penalty(gf),3)

  text(1,3.75,'SIMPLE GRADE PENALTY',pos=4)
  text(1,3.5,paste(ltitle1,ddm$mn,'+/-',ddm$se,sep=" "),pos=4)
  text(1,3.25,paste(ltitle2,ddf$mn,'+/-',ddf$se,sep=" "),col='red',pos=4)

  #These are results which will be set in a data frame and output.
  N <- nst
  N_FEMALES  <- length(which(f))
  N_MALES    <- length(which(m))
  MN_ALL     <- dda$mn
  SE_ALL     <- dda$se
  MN_MALES   <- ddm$mn
  SE_MALES   <- ddm$se
  MN_FEMALES <- ddf$mn
  SE_FEMALES <- ddf$se
  
  out <- data.frame(SUBJECT,CATALOG_NBR,N,MN_ALL,SE_ALL,N_FEMALES,N_MALES,MN_MALES,SE_MALES,MN_FEMALES,SE_FEMALES)
  
  
#Done making the basic plot, now get fancier if requested.
#Do matching, print result to plot, add to output table.
if (MATCHING == TRUE)
{  
  gg <- matching.analysis(data)
  MATCHED_MEAN_MALES   <- mean(gg$gpenm)
  MATCHED_SE_MALES     <- sd(gg$gpenm)/sqrt(length(gg$gpenm))
  MATCHED_MEAN_FEMALES <- mean(gg$gpenf)
  MATCHED_SE_FEMALES   <- sd(gg$gpenf)/sqrt(length(gg$gpenf))
  
  mmn <- signif(mean(gg$gpenm),3)
  mse <- signif(sd(gg$gpenm)/sqrt(length(gg$gpenm)),3)
  fmn <- signif(mean(gg$gpenf),3)
  fse <- signif(sd(gg$gpenf)/sqrt(length(gg$gpenf)),3)
  
  text(1,2.75,'MATCHED MEAN GRADE:',pos=4)
  text(1,2.5,paste(ltitle1,mmn,'+/-',mse,sep=" "),pos=4)
  text(1,2.25,paste(ltitle2,fmn,'+/-',fse,sep=" "),col='red',pos=4)
  
  out <- data.frame(out,MATCHED_MEAN_MALES,MATCHED_SE_MALES,MATCHED_MEAN_FEMALES,MATCHED_SE_FEMALES)
}

#Do the regression and add the coefficient on SEX (and its SE) to the output
if (REGRESSION == TRUE)
{
 uber_reg <- grade.regression(data)
 #print(summary(uber_reg))
 res <- summary(uber_reg)
 SEX_REG    <- res$coefficients[3,1]
 SEX_REG_SE <- res$coefficients[3,2]
 text(1,3.0,paste('SEX_REG_COEFF:',signif(SEX_REG,3),'+/-',signif(SEX_REG_SE,3)),pos=4)
 out <- data.frame(out,SEX_REG,SEX_REG_SE)
}
  
  if (PDF == TRUE){dev.off()}
  return(out)
  
}
#####################################################################################
#Run a simple grade regression,wary of all the standard assumptions
#(i.e. gaussian error distribution, independence of covariates) we have violated in doing so!
#FUNCTION: grade.regression
#PURPOSE : Run a multivariate regression to estimate the gender effect, with grade as the dependent variable, 
#INPUTS  : data  - A merged student-record/student-course data frame
#          cname - the plot title
#          col   - Plot color. Default is black, and by setting it to black,
#                  a new plot is initiated.
#OUTPUTS : A plot (or overplotted points) directed to the current output device.
#####################################################################################
grade.regression <- function(data)
{
  #View(data)
  #reformat things for the regression
  GRD_PTS_PER_UNIT        <- as.numeric(data$GRD_PTS_PER_UNIT)
  SEX                     <- as.factor(data$SEX)
  LAST_ACT_MATH_SCORE     <- as.numeric(data$LAST_ACT_MATH_SCORE)
  LAST_ACT_ENGL_SCORE     <- as.numeric(data$LAST_ACT_ENGL_SCORE)
  GPAO                    <- as.numeric(data$GPAO)
  HSGPA                   <- as.numeric(data$HSGPA)
  
  model <- glm(GRD_PTS_PER_UNIT ~ GPAO+SEX+LAST_ACT_MATH_SCORE+LAST_ACT_ENGL_SCORE+HSGPA)
  #print(summary(model))
  #textplot(signif(summary(model)$coefficients,3))
  #title('Linear Regression, Dependent Variable = Grade')
  #cmtx <- -1.0*cov2cor(vcov(model)) #factor of -1 b/c the correlations are all negative somehow (I spot-checked)
  #heatmap.2(cmtx,Rowv=FALSE,Colv=FALSE,main='Covariate Correlation', 
  #          cexRow=0.75,cexCol=0.75,trace='none',cellnote=signif(cmtx,2),notecol='black')
  return(model)
}

#####################################################################################
#Intiate the grade penalty plot
#FUNCTION: plot.binned grades
#PURPOSE : Plot (or overplot!) grade penalty data.
#INPUTS  : data  - A merged student-record/student-course data frame
#          cname - the plot title
#          col   - Plot color. Default is black, and by setting it to black,
#                  a new plot is initiated.
#OUTPUTS : A plot (or overplotted points) directed to the current output device.
#####################################################################################
plot.binned.grades <- function(data,cname,col='black',xlim=c(1,4),ylim=c(1,4))
{
  if (col == 'black')
  {
    plot(data$meangpa,data$meangd,pch=19,xlab='GPAO',ylab='<Grade>',main=cname,xlim=xlim,ylim=ylim)
    for (i in 1:length(data$meangpa))
    {
      arrows(data$meangpa,data$meangd-data$segd,data$meangpa,data$meangd+data$segd,code=0)
    }
  }
  if (col != 'black')
  {
    points(data$meangpa,data$meangd,pch=19,col=col)
    for (i in 1:length(data$meangpa))
    {
      arrows(data$meangpa,data$meangd-data$segd,data$meangpa,data$meangd+data$segd,code=0,col=col)
    }
  } 
}

#####################################################################################
#Take the student course and student record, create grade penalty analysis of a course.
#FUNCTION: compute.gpa.binned.grades
#PURPOSE : Return binned grade penalty point-estimates for plotting.
#INPUTS  : A merged student-record/student-course data frame
#          
#OUTPUTS : A data frame containing binned grade/gpao statistics for plotting..
#####################################################################################
compute.gpa.binned.grades <- function(data,nbins=10)
{
  min_gpao <- 1#min(data$GPAO)
  max_gpao <- 4#max(data$GPAO)
  binsize  <- (max_gpao-min_gpao)/nbins
  
  meangpa <- mat.or.vec(nbins,1)
  meangd  <- mat.or.vec(nbins,1)
  segd    <- mat.or.vec(nbins,1)
  count   <- 1  
  start_ind <- 0
  stop_ind <- 0
  
  for (i in 1:nbins)
  {
    tmin <- min_gpao+binsize*(i-1)
    tmax <- tmin+binsize
    ind        <- data$GPAO > tmin & data$GPAO < tmax
    
    #Grade penalty mean and SE are computed by bootstrap by default.
    meangpa[i] <- mean(data$GPAO[ind])
    #meangd[i]  <- mean(data$GRD_PTS_PER_UNIT[ind])
    #segd[i]    <- sd(data$GRD_PTS_PER_UNIT[ind])/sqrt(sum(ind))
    temp <- bootstrap.bin.gpao(data$GRD_PTS_PER_UNIT[ind])
    meangd[i]  <- temp$mn
    segd[i]    <- temp$se
    
    
  }
  
  stats <- data.frame(meangpa,meangd,segd)
  return(stats)
}

#####################################################################################
#Match males and females for comparison.
#FUNCTION: matching.analysis
#PURPOSE : This matches males and females for a course based on serveral covariates using
#          the optmatch library.
#INPUTS  : A merged student-record/student-course data frame
#          
#OUTPUTS : A data frame containing binned grade/gpao statistics for plotting..
#####################################################################################  
matching.analysis <- function(data)
{
    library(optmatch)
    print('matching')
    #Clean out problem areas
    e         <- data$SEX != 'U' & 
                 !is.na(data$LAST_ACT_MATH_SCORE) & !is.na(data$LAST_ACT_ENGL_SCORE) &
                 data$HSGPA > 0 
    data      <- data[which(e),]  
    
    #Define/coerce the matching variables.
    SEX       <- mat.or.vec(length(data$ANONID),1)
    e         <- data$SEX == 'F'
    SEX[e]    <- 1
    ACT.MATH  <- as.numeric(data$LAST_ACT_MATH_SCORE)
    ACT.ENGL  <- as.numeric(data$LAST_ACT_ENGL_SCORE)
    GRADE     <- as.numeric(data$GRD_PTS_PER_UNIT)
    GPAO      <- as.numeric(data$GPAO)
    HSGPA     <- as.numeric(data$HSGPA)
    ANONID    <- as.numeric(data$ANONID)
    data      <- data.frame(ANONID,SEX,ACT.MATH,ACT.ENGL,GRADE,GPAO,HSGPA)
    #Supply matching propensity scores
    model <- glm(SEX ~ ACT.MATH+ACT.ENGL+GPAO+HSGPA,family=binomial(),data=data)
    #...and execute the matching with a caliper set at 0.2 to increase speed...
     m1    <- fullmatch(match_on(model,caliper=0.2),data=data)
    
    #Now attach the matching structure to the data
    data <- cbind(data,matches=as.numeric(substr(m1,3,7)))
    out <- data
    
    #And sort once for speed, computing mean grades for the matched groups
    data       <- data[order(data$matches,data$SEX),] #This sort is crucial. Keeping the SEX makes sure that female is always index 1.
    data$count <- sequence(rle(as.vector(data$matches))$lengths)
    
    nid    <- length(data$matches[!duplicated(data$matches)])
    nstart <- which(data$count == 1)
    ntot   <- length(data$matches)
    
    rnames <- row.names(data) #mat.or.vec(nid,1)
    
    #Now compute the grades for males and matched females
    #Note that this matching ONLY considers one-to-one matching, as opposed to averaging
    #all N matches to a single individuals.
    
    gpenf <- mat.or.vec(nid,1)
    gpenm <- gpenf
    
    for (i in 1:nid)
    {
      start_ind <- nstart[i]
      if (i < nid){stop_ind  <- nstart[i+1]-1}
      if (i == nid){stop_ind <- ntot}
      ind <- c(start_ind:stop_ind)
      gpenf[i] <- as.numeric(data$GRADE[stop_ind])
      gpenm[i] <- as.numeric(data$GRADE[start_ind])
    }
    
    return(data.frame(gpenm,gpenf,nid))
  }  
  
#####################################################################################
#Compute un-adjusted, raw grade penalty for input data.
#FUNCTION: compute.overall.grade.penalty
#PURPOSE : Return raw grade penalty stats using either basic stats or boostrapping (default)
#INPUTS  : data: A bin (or full sample) of grade penalty-formatted data.
#        : BOOT: do boostrapping if set to TRUE
#          
#OUTPUTS : A data frame containing output statistics.
#####################################################################################
compute.overall.grade.penalty <- function(data,BOOT=TRUE)
{
  
  nstd <- length(data)         #Number of students
  gpen <- data$GRD_PTS_PER_UNIT-data$GPAO
  
  #Now compute the grade penalty
  if (BOOT == FALSE)
  {
    mn   <- mean(gpen)              #Mean Grade Penalty
    sd   <- sd(gpen)                #Standard deviation
    se   <- sd/sqrt(nstd)           #Naive standard error
    return(data.frame(mn,se))
  }
  
  #Bootstrap by resampling students. 
  #Obviously many ways to do this (e.g. resample terms?), depending on the question.
  #BOOTNUM == 1000 for > 10000 students takes t ~ 1 sec. 
  #Bootstrap SEs will probably be more useful for smaller sample sizes.
  if (BOOT == TRUE)
  {
    out <- bootstrap.bin.gpao(gpen)
    return(out)
  }
  
}

#####################################################################################
#Compute bootstrap mean and SE for 
#FUNCTION: bootstrap.bin.gpao
#PURPOSE : Return boostrap mean, SE on mean, 97.5% CI for input data
#INPUTS  : data: A bin (or full sample) of grade penalty-formatted data.
#        : NBOOT: number of bootstrap resamples to make (defeault = 100)
#          
#OUTPUTS : A data frame containing the mean, SE on mean, and 97.5% CI for the input data..
#####################################################################################
bootstrap.bin.gpao <- function(data,NBOOT=100)
{
  nstd  <- length(data)
  mnvec <- mat.or.vec(NBOOT,1)
  se95low  <-  mat.or.vec(NBOOT,1)
  se95high  <- mat.or.vec(NBOOT,1)
  
  for (i in 1:NBOOT)
  {
    sub <- sample(data,nstd,replace=TRUE)
    mnvec[i] <- mean(sub)
  }
  mn <- mean(mnvec)
  se <- sqrt(var(mnvec))
  se95low  <- mnvec[0.025*NBOOT]
  se95high <- mnvec[0.975*NBOOT]
  return(data.frame(mn,se,se95low,se95high))
}

remove.duplicates <- function(data,keep='NONE',verbose=FALSE)
{
  data       <- data[order(data$ID,data$TERM), ]
  data$count <- sequence(rle(as.vector(data$ANONID))$lengths)
  
  ntot   <- length(data$ANONID)
  
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