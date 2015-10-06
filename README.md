# pla-devel
Development code for the PLA project at Michigan, all in R. A master file keeps arranges and organizes analysis of the main questions posed in the course. Code is all desgined to run student record and student course tables provided. Larger pieces of code (e.g. grade.penalty.module.R) are broken into separate files, which we may decide later to paste into the master file, or to keep separate. The final products will be transfered to a separate repository or forked for the live PLA-MOOC. More details are given in the wiki (link on the right side).

master.analysis.R: Intended to be a master/main level wrapper for all relevant questions and modules in the course.

grade.penalty.module.R: Grade penalty analysis. Requires the optmatch package.

course.persistence.module.R: Course to course persistence.

course.pathways.treemaps.R: Given some course, create a treemap of courses taken before, during, and after students take this course.

course.pathways.barplots.R: Given some course, create a barplot of the most commonly taken courses before, during, and after this course.

course.impact.R: Investigate the effect of having taken one course on performance in another course.

student.course.csv: the student course table

student.record.csv: the student record table.


