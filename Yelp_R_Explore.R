setwd("C:/Users/iroy/Documents/DL and ML Practical Tutorials - Package/TensorFlow_FILES")

library(jsonlite)
library(tidyr)
library(dplyr)
#Reading review Dataset
df_review<- stream_in(file("review.json"))
str(df_review)

#summary(df_review)
#tail(df_review)
head(sort(table(df_review$business_id),decreasing=TRUE),5)
#Reading Business Dataset
df_business<- stream_in(file("business.json"))
str(df_business)

library(ggplot2)
library(leaflet)
library(ggmap)
library(tmap)
library(sp)
#m<- leaflet() %>% addTiles() %>% addMarkers(data = df_business,lng = ~longitude,lat = ~latitude)
#m

coordinates(df_business_NV)=c('longitude','latitude')
crs=CRS("+proj=longlat") #coordinate reference system with a specific type of projection
proj4string(df_business_NV)=crs

plot(df_business_NV, pch=20,col="steelblue")

#getting top20 businesses by derived metric in NV in a separate df
df_business_NV<- df_business %>% filter(state=='NV')
head(df_business_NV)
df_business_NV$DerivedMetric<- df_business_NV$stars*df_business_NV$review_count
str(df_business_NV)
df_NV_top20<-head(df_business_NV[order(-df_business_NV$DerivedMetric),],20)
head(df_NV_top20)

#Shape File Load
library(sp)
library(rgdal)
#read a shapefile and assign to a spatial object
NV_Clark_shape<-readOGR("./Shapefiles_Yelp_NV/las-vegas-nv-city-limits.shp")
plot(NV_Clark_shape) #just this line will give shapefile with counties of NV
#including scatter points: issues are scaling and color of points
points(df_business_NV, pch=20,col="orange")

#Joins
#Trimming the Review dataset for join
df_review_Trim=df_review[,c(3:9)]
str(df_review_Trim)

df_business_Trim<- df_business[,c("business_id","name","state","review_count","stars")]
str(df_business_Trim)

#Since State was not present in the Review Dataset we could filter for NV businesses & reviews only after joining the data
Join_data<-left_join(df_review_Trim,df_business_Trim,by="business_id")
str(Join_data)
#Operations on this Join dataset (date 2 is the Year part of Date)
Join_data$date2<- as.POSIXct(Join_data$date,format="%Y")
Join_data$date2<- format(Join_data$date2,format="%Y")
Join_data$date2<- as.factor(Join_data$date2)
# rename columns
Join_data<- Join_data %>% rename(date_year=date2,Review_rating=stars.x,Average_Rating=stars.y)
Join_data$DerivedMetric<- Join_data$Average_Rating*Join_data$review_count
#Filtering for NV State
Join_data_NV<- Join_data %>% filter(Join_data$state=='NV')
str(Join_data_NV)
Join_data_NV %>% group_by(Review_rating) %>% summarise(number_of_reviews=n())
#Creating a random 20% chunk size of the dataset since dtm matrices would not get generated for such a large data ~2.32M documents with 580k words
Join_data_NV_chunk<-Join_data_NV %>% group_by(Review_rating) %>% sample_frac(.2)
Join_data_NV_chunk %>% group_by(Review_rating) %>% summarise(number_of_reviews=n()) #shows about 1/5th of the size in each category

#Visualizing for whole dataset
p<- Join_data_NV %>% group_by(date_year,Review_rating) %>% summarise(number_of_reviews=n()) %>% mutate(pct=(number_of_reviews*100/sum(number_of_reviews))) 
p %>% ggplot() + geom_bar(aes(x = date_year, y =number_of_reviews,fill=Review_rating),stat = "identity")+ ggtitle("Distribution of Review Ratings Over Year")
#including labels was too clustered-----> + geom_text(aes(x = date_year, y =number_of_reviews,label=pct),size = 3, position = position_stack(vjust = 0.5))

library(tm)
Join_NV_corpus<- Corpus(VectorSource(Join_data_NV$text)) #this is a list

#inspect a particular review
writeLines(as.character(Join_NV_corpus[[4]]))
inspect(Join_NV_corpus)
#see# available transformations
getTransformations()

#create the toSpace content transformer
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
Join_NV_corpus <- tm_map(Join_NV_corpus, toSpace, "-")
Join_NV_corpus <- tm_map(Join_NV_corpus, toSpace, ":")

#got to remove \n chars 
Join_NV_corpus <- tm_map(Join_NV_corpus, toSpace, "[\n]")
#transforming to lowercase
Join_NV_corpus <- tm_map(Join_NV_corpus,content_transformer(tolower))
#std transformation of stripping numbers as there wouldnt be much value of numbers after tokenization
Join_NV_corpus <- tm_map(Join_NV_corpus, removeNumbers)
#removing stopwords
Join_NV_corpus <- tm_map(Join_NV_corpus, removeWords, stopwords("english"))
#strip whitespace
Join_NV_corpus <- tm_map(Join_NV_corpus, stripWhitespace)
# Remove punctuations
Join_NV_corpus <- tm_map(Join_NV_corpus, removePunctuation)

#Doc Term Matrix (where documents are on row axis)
Join_NV_dtm <- DocumentTermMatrix(Join_NV_corpus)
Join_NV_dtm
inspect(Join_NV_dtm[1:2,1000:1005])
#Creating a matrix of dtm and using column sum will give no of unique terms in each review, hence rowsum will give freq of each term
matrix_dtm <- as.matrix(Join_NV_dtm)
v <- sort(rowSums(matrix_dtm),decreasing=TRUE)

#Term Doc Matrix (where docs are on column axis)
Join_NV_tdm <- TermDocumentMatrix(Join_NV_corpus)
Join_NV_tdm
inspect(Join_NV_tdm[1:2,1000:1005])
#Creating a matrix of tdm and using column sum will give frequency of usage of each term through all reviews
matrix_tdm <- as.matrix(Join_NV_tdm)
matrix_tdm

# Repeating the above steps for chunk dataset as matrices were running into error:

Join_NV_corpus_chunk<- Corpus(VectorSource(Join_data_NV_chunk$text)) #this is a list

#inspect a particular review
writeLines(as.character(Join_NV_corpus_chunk[[4]]))
#inspect(Join_NV_corpus_chunk)
#see# available transformations
getTransformations()

#create the toSpace content transformer
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
Join_NV_corpus_chunk <- tm_map(Join_NV_corpus_chunk, toSpace, "-")
Join_NV_corpus_chunk <- tm_map(Join_NV_corpus_chunk, toSpace, ":")

#got to remove \n chars 
Join_NV_corpus_chunk <- tm_map(Join_NV_corpus_chunk, toSpace, "[\n]")
#transforming to lowercase
Join_NV_corpus_chunk <- tm_map(Join_NV_corpus_chunk,content_transformer(tolower))
#std transformation of stripping numbers as there wouldnt be much value of numbers after tokenization
Join_NV_corpus_chunk <- tm_map(Join_NV_corpus_chunk, removeNumbers)
#removing stopwords
Join_NV_corpus_chunk <- tm_map(Join_NV_corpus_chunk, removeWords, stopwords("english"))
#strip whitespace
Join_NV_corpus_chunk <- tm_map(Join_NV_corpus_chunk, stripWhitespace)
# Remove punctuations
Join_NV_corpus_chunk <- tm_map(Join_NV_corpus_chunk, removePunctuation)

#Doc Term Matrix (where documents are on row axis)
Join_NV_chunk_dtm <- DocumentTermMatrix(Join_NV_corpus_chunk)
Join_NV_chunk_dtm
inspect(Join_NV_chunk_dtm[1:2,1000:1005])
#Creating a matrix of dtm and using column sum will give no of unique terms in each review, hence rowsum will give freq of each term
matrix_dtm <- as.matrix(Join_NV_chunk_dtm)
v <- sort(rowSums(matrix_dtm),decreasing=TRUE)

#Term Doc Matrix (where docs are on column axis)
Join_NV_tdm <- TermDocumentMatrix(Join_NV_corpus)
Join_NV_tdm
inspect(Join_NV_tdm[1:2,1000:1005])
#Creating a matrix of tdm and using column sum will give frequency of usage of each term through all reviews
matrix_tdm <- as.matrix(Join_NV_tdm)
matrix_tdm