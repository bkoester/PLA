#####################################################################################
#1) How does performance in one course affect another?  Are there courses that predict grades 
#in other courses better than GPAO?
#FUNCTION: cor.code
#PURPOSE : Select pairs of courses in a given subject area so that grade in one course 
#is a better predictor of grade in the other course than GPAO given that courses are taken 
#in a particular order.(COURSE_1 is the predicting course, COURSE_2 is the course of interest)
#INPUTS  : sc - student course table
#          SUBJECT     - course subjects
#OUTPUTS : Table sent to cor.grade.GPAO.csv in the CWD. 
# example cor.code(sc, SUBJECT=c("PHYSICS","MATH"), TERM_RANGE=c(122,156))
#####################################################################################



#sc<-student.course.anon.MOOC.FA.2015.orig


cor.code <- function(sc, SUBJECT=c("PHYSICS","MATH"), TERM_RANGE=c(1,156))
{

    library(gdata)
    
  
    e <- sc$GPAO > 0 & !is.na(sc$GPAO) & sc$GPAO <20 &
      sc$SUBJECT%in%SUBJECT& sc$TERM >= TERM_RANGE[1] & sc$TERM <= TERM_RANGE[2]
    
    sc<-sc[e,c( "SUBJECT","ANONID","TERM","CATALOG_NBR", 
                    "GRD_PTS_PER_UNIT","GPAO")]
    dim(sc)

    
    enroll <- droplevels(subset(as.data.frame(table(sc$CATALOG_NBR, sc$SUBJECT)),
                                Freq>200))
  
    
    data.set <- data.frame(SUBJECT=character(),ANONID=numeric(),TERM=numeric(),
                           CATALOG_NBR=numeric(), 
                           GRD_PTS_PER_UNIT=numeric(),GPAO=numeric())
    d<-dim(enroll)[1]
  
    
    for(j in 1:d){
        df.set <- subset(sc, select=c( "SUBJECT","ANONID","TERM","CATALOG_NBR", 
                                         "GRD_PTS_PER_UNIT","GPAO"),
                         SUBJECT == as.character(enroll$Var2[j]) & 
                             CATALOG_NBR == as.numeric(levels(enroll$Var1[j]))[enroll$Var1[j]])
        data.set<-rbind(df.set,data.set)}
    
    
    list.all <- drop.levels(split(enroll$Var1,enroll$Var2))
    
    list.courses<-unique (enroll$Var2)

df_output <- data.frame(SUBJECT.1 = character(), CATALOG_NBR.1=numeric(),
                        SUBJECT.2 = character(), CATALOG_NBR.2=numeric(), ORDER=character(),
                        cor.GPAO=numeric(),  wci.GPAO=numeric(), 
                        cor.grade=numeric(),wci.grade=numeric(),
                        n=numeric(),  diff.norm.cor=numeric())



m=1

for (k in list.courses) {
 
    

for (i in list.all[[m]]) {

group.1 <- subset(data.set,select=c( "SUBJECT","ANONID","TERM","CATALOG_NBR", 
                                         "GRD_PTS_PER_UNIT","GPAO"),
                      SUBJECT == k & CATALOG_NBR == i)  

mm=1


for (kk in list.courses) {
  
    

for (ii in list.all[[mm]]) {
       
       
t <- (kk %in% df_output$SUBJECT.1 & ii %in% df_output$CATALOG_NBR.1)|
    (kk==k & ii==i)
  

if(!t){




group.2 <- subset(data.set,select=c( "SUBJECT","ANONID","TERM","CATALOG_NBR", 
                                 "GRD_PTS_PER_UNIT","GPAO"),
                  SUBJECT == kk & CATALOG_NBR == ii)


group.12 <- merge(group.1,group.2,by='ANONID',all.x=FALSE)

#set 1.2 
group.1.2 <- subset(group.12, TERM.x<TERM.y)
order<-"2 after 1"

n.1.2 <- dim(group.1.2)[1]
if(n.1.2<100){
    cor.GPAO.1.2 <- NA 
    cor.grade.1.2 <-NA
    wci.GPAO.1.2 <- NA 
    wci.grade.1.2 <-NA
    diff.norm.cor.1.2<-NA
} else {
   
    cor.GPAO.1.2 <- cor.test(group.1.2$GRD_PTS_PER_UNIT.y,group.1.2$GPAO.y)$estimate
    ci.GPAO.1.2 <-cor.test(group.1.2$GRD_PTS_PER_UNIT.y,group.1.2$GPAO.y)$conf.int
    wci.GPAO.1.2 <-ci.GPAO.1.2[2]-ci.GPAO.1.2[1]
    cor.grade.1.2 <- cor.test(group.1.2$GRD_PTS_PER_UNIT.y,group.1.2$GRD_PTS_PER_UNIT.x)$estimate
    ci.grade.1.2 <- cor.test(group.1.2$GRD_PTS_PER_UNIT.y,group.1.2$GRD_PTS_PER_UNIT.x)$conf.int
    wci.grade.1.2<-ci.grade.1.2[2]-ci.grade.1.2[1]
    diff.norm.cor.1.2<-abs((-abs(cor.GPAO.1.2)+abs(cor.grade.1.2)))/sqrt(wci.GPAO.1.2*wci.GPAO.1.2+   
                     wci.grade.1.2*wci.grade.1.2)
                         
}
df_result.1.2 <- data.frame(SUBJECT.1 = k, CATALOG_NBR.1=i,
                        SUBJECT.2 = kk, CATALOG_NBR.2=ii,ORDER=order,
                        cor.GPAO=cor.GPAO.1.2, wci.GPAO=wci.GPAO.1.2, 
                        cor.grade=cor.grade.1.2,wci.grade=wci.grade.1.2,
                        n= n.1.2,  diff.norm.cor= diff.norm.cor.1.2)


v<-  abs(cor.grade.1.2)>abs(cor.GPAO.1.2)&  !is.na( diff.norm.cor.1.2)& diff.norm.cor.1.2>0.5


if(v){
    df_output<-rbind(df_output,df_result.1.2)}

    
    
    
#set 2.1    

group.2.1 <- subset(group.12, TERM.x>TERM.y)
order<-"2 before 1"

n.2.1 <- dim(group.2.1)[1]
if(n.2.1<100){
    cor.GPAO.2.1 <- NA 
    cor.grade.2.1 <-NA
    wci.GPAO.2.1 <- NA 
    wci.grade.2.1 <-NA
    diff.norm.cor.2.1<-NA
} else {
    
    cor.GPAO.2.1 <- cor.test(group.2.1$GRD_PTS_PER_UNIT.x,group.2.1$GPAO.x)$estimate
    ci.GPAO.2.1 <-cor.test(group.2.1$GRD_PTS_PER_UNIT.x,group.2.1$GPAO.x)$conf.int
    wci.GPAO.2.1 <-ci.GPAO.2.1[2]-ci.GPAO.2.1[1]
    cor.grade.2.1 <- cor.test(group.2.1$GRD_PTS_PER_UNIT.x,group.2.1$GRD_PTS_PER_UNIT.y)$estimate
    ci.grade.2.1 <- cor.test(group.2.1$GRD_PTS_PER_UNIT.x,group.2.1$GRD_PTS_PER_UNIT.y)$conf.int
    wci.grade.2.1<-ci.grade.2.1[2]-ci.grade.2.1[1]
    diff.norm.cor.2.1<-abs((-abs(cor.GPAO.2.1)+abs(cor.grade.2.1)))/sqrt(wci.GPAO.2.1*wci.GPAO.2.1+   
                                                          wci.grade.2.1*wci.grade.2.1)

}

    
df_result.2.1 <- data.frame(SUBJECT.1 = k, CATALOG_NBR.1=i,
                        SUBJECT.2 = kk, CATALOG_NBR.2=ii,ORDER=order,
                        cor.GPAO=cor.GPAO.2.1, wci.GPAO=wci.GPAO.2.1, 
                        cor.grade=cor.grade.2.1,wci.grade=wci.grade.2.1,
                        n= n.2.1,  diff.norm.cor= diff.norm.cor.2.1)


v<- abs(cor.grade.2.1)>abs(cor.GPAO.2.1)& !is.na( diff.norm.cor.2.1)&diff.norm.cor.2.1>0.5


if(v){
    df_output<-rbind(df_output,df_result.2.1)}



#set 2.2    

group.2.2 <- subset(group.12, TERM.x==TERM.y)
order<-"2 at the same time as 1"

n.2.2 <- dim(group.2.2)[1]
if(n.2.2<100){
    cor.GPAO.2.2 <- NA 
    cor.grade.2.2 <-NA
    wci.GPAO.2.2 <- NA 
    wci.grade.2.2 <-NA
    diff.norm.cor.2.2<-NA
} else {
    
    cor.GPAO.2.2 <- cor.test(group.2.2$GRD_PTS_PER_UNIT.x,group.2.2$GPAO.x)$estimate
    ci.GPAO.2.2 <-cor.test(group.2.2$GRD_PTS_PER_UNIT.x,group.2.2$GPAO.x)$conf.int
    wci.GPAO.2.2 <-ci.GPAO.2.2[2]-ci.GPAO.2.2[1]
    cor.grade.2.2 <- cor.test(group.2.2$GRD_PTS_PER_UNIT.x,group.2.2$GRD_PTS_PER_UNIT.y)$estimate
    ci.grade.2.2 <- cor.test(group.2.2$GRD_PTS_PER_UNIT.x,group.2.2$GRD_PTS_PER_UNIT.y)$conf.int
    wci.grade.2.2<-ci.grade.2.2[2]-ci.grade.2.2[1]
    diff.norm.cor.2.2<-abs((-abs(cor.GPAO.2.2)+abs(cor.grade.2.2)))/sqrt(wci.GPAO.2.2*wci.GPAO.2.2+   
                                                                             wci.grade.2.2*wci.grade.2.2)
    
}


df_result.2.2 <- data.frame(SUBJECT.1 = k, CATALOG_NBR.1=i,
                            SUBJECT.2 = kk, CATALOG_NBR.2=ii,ORDER=order,
                            cor.GPAO=cor.GPAO.2.2, wci.GPAO=wci.GPAO.2.2, 
                            cor.grade=cor.grade.2.2,wci.grade=wci.grade.2.2,
                            n= n.2.2,  diff.norm.cor= diff.norm.cor.2.2)


v<- abs(cor.grade.2.2)>abs(cor.GPAO.2.2)& !is.na( diff.norm.cor.2.2)&diff.norm.cor.2.2>0.5


if(v){
    df_output<-rbind(df_output,df_result.2.2)}


}

}
mm=mm+1

}
#

#
}
m=m+1


}

write.csv(df_output, file = "cor.grade.GPAO.csv")




}

