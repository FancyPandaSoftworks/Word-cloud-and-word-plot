library(pdftools)
text1<- pdf_text("ubd_strategicplan_en.pdf")
text2<-pdf_text("ubd_strategic_plan_utrecht_university_2016-2020.pdf")

#Lower all cases of the texts
text1<- sapply(text1, tolower)
text2<- sapply(text2, tolower)

#Setting up the pdf so we can do things with it for text1
#---------------------------------------------------------------------------------------------------------------------
tfull = unlist(text1)
tokens = unlist(strsplit(tfull,"[\\W]",perl=TRUE))
text1 = tokens[tokens!=""]	
df = data.frame(token=as.factor(text1))

#Getting the frequencies for the words
a <- table(df)
a<- as.data.frame(a)


#Filter the words into another table 
b <-a[!(a$df!="education")|!(a$df!="teaching")|!(a$df!="research")|!(a$df!="staff")|!(a$df!="students"),]
#Plot the table
count <- b$Freq
barplot(count, names.arg = c("education", "teaching", "research","staff", "students" ))

#---------------------------------------------------------------------------------------------------------------------
#Text2
tfull2 = unlist(text2)
tokens2 = unlist(strsplit(tfull2,"[\\W]",perl=TRUE))
text2 = tokens[tokens2!=""]	
df2 = data.frame(token2=as.factor(text2))

#Getting the frequencies for the words
a2 <- table(df2)

a2<- as.data.frame(a2)

#Filter the words into another table 
b2 <-a2[!(a2$df2!="education")|!(a2$df2!="teaching")|!(a2$df2!="research")|!(a2$df2!="staff")|!(a2$df2!="students"),]
#Plot the table
count2 <-b2$Freq
barplot(count2, names.arg = c("education", "teaching", "research","staff", "students" ))
#----------------------------------------------------------------------------------------------------------------------

#Task 1.2
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
stopword <-read.csv("stopwords.csv")
stopword <- c(stopword, stopwords())

#changing df2 to df for the first one
cloud <- Corpus(VectorSource(df2$token))
#cloud <- tm_map(cloud, PlainTextDocument)
cloud <- tm_map(cloud, removePunctuation)
cloud <- tm_map(cloud, removeWords, unlist(stopword))
cloud <- tm_map(cloud, stemDocument)
cloud <- tm_map(cloud, tolower)
dtm <- DocumentTermMatrix(cloud)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
wordcloud(cloud, max.words = 100,scale = c(4,0.1) ,random.order = FALSE, min.freq = 3, colors = brewer.pal(8,"Dark2"))
dev.off()


#-----------------------------------------------------------------------------------------------------------------------
#task 2

library(readr)
library(stringr)
library(htmltab)
library(imager)

courses = htmltab("http://www.cs.uu.nl/education/index.php",which=3)
courses <- courses[courses$`nivo >> nivo >> nivo >> nivo >> nivo >> nivo >> nivo`=="m",]
colnames(courses)[colnames(courses)==names(courses[1])]<- "period"
colnames(courses)[colnames(courses)==names(courses[2])]<- "timeslot"
colnames(courses)[colnames(courses)==names(courses[3])]<- "code"
colnames(courses)[colnames(courses)==names(courses[4])]<- "niveau"
colnames(courses)[colnames(courses)==names(courses[5])]<- "ects"
colnames(courses)[colnames(courses)==names(courses[6])]<- "name"
#remove na
courses <- na.omit(courses)
courses$code <-sort(courses$code)
#Change working directory
courseList <- list.files(pattern = ".htm$")
courseList <- lapply(courseList, htmltab, which=1)
courseList <- ldply(courseList, data.frame)

#Get contents and description and course code only
lists <-courseList[courseList$Website.=="Contents:"|courseList$Website.=="Description:"| courseList$Website.=="Course code:",]
lists <- lists[1:2]
lists <- na.omit(lists)
colnames(lists)[colnames(lists)==names(lists[2])]<- "description"
#description and content merge
for (x in 1:nrow(lists)) {
  if(lists$Website.[x]=="Description:")
  {
    lists$description[x-1]<-paste(lists$description[x-1], lists$description[x])
    
  }
  
}
#Substituting the variable "Contents" into "Course code"
for(x in 1:nrow(lists)){
  y <-1
  if(lists$Website.[x]=="Course code:")
  {
    lists$Website.[x+1]<- lists$description[x]
  }
  y<y+1
}
#Remove description since it is already in the content and the course code that is already subsituted
lists <- lists[!(lists$Website.=="Course code:"),]
lists <- lists[!(lists$Website.=="Description:"),]
#Reorder the rows for my own sake
rownames(lists) <- seq(length= nrow(lists))
#tm map corpus
dfcorpus <- VectorSource(lists$description)
dfcorpus <- Corpus(dfcorpus)
dfcorpus <- tm_map(dfcorpus, removePunctuation)
dfcorpus <- tm_map(dfcorpus, stemDocument)
dfcorpus <- tm_map(dfcorpus, tolower)
tdmm <- TermDocumentMatrix(dfcorpus)
weight <- weightTfIdf(tdmm)
#-------------------------------------------------------------------------------
#testing
#scores <-tm_term_score(weight, terms = c("advanc"))
#scores <- as.data.frame(scores)
#rownames(courses)<- seq(length = nrow(courses))
#-----------------------------------------------------------------------------------------------------------------------

merging <- cbind(lists, scores)
highest = tail(sort(merging$scores), 3)
answer <- matrix(ncol = 3, nrow = nrow(scores))
#-----------------------------------------------------------------------------------------------------------------------
#function to return the top 3 answers
#For input we throw advanc in the function
topAnswer <- function(j)
{
  scores <-tm_term_score(weight, terms = c(j))
  scores <- as.data.frame(scores)
  rownames(courses)<- seq(length = nrow(courses))
  
  for (x in 1:nrow(merging)) {
    for (y in 1:3) {
      {
        if(merging$scores[x]==highest[y])
        {
          for(z in 1:3)
          {
            answer[x,z]<-merging[x,z]
          }
        }
      }
      
    }
    
  }
  #Remove all NAs, and then it will reorder it to 1,2,3
  answer <- na.omit(answer)
  #Get answer
  View(answer)
}

#-----------------------------------------------------------------------------------------------------------------------



