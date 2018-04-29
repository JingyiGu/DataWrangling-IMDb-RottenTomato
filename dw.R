library(tidyr)
library(tidyverse)
library(ggplot2)
library(jsonlite)
library(stringr)
library(wordcloud)
library(ggrepel)

######################################
#########     input data
######################################
imdb<-read.csv("tmdb_5000_movies.csv",stringsAsFactors = F)

rotten<-"http://files.grouplens.org/datasets/hetrec2011/hetrec2011-movielens-2k-v2.zip"
file <- tempfile(tmpdir=tempdir(), fileext=".zip")
download.file(rotten,file)
rt<-unzip(file,list = TRUE)$Name[c(1,2,3,4,5,6,7,8,9, 10, 11, 12, 13)]
unzip(file,files=rt,exdir=tempdir(), overwrite=TRUE)
imdb_rotten <-read.delim(file.path(tempdir(), rt)[8], header=TRUE,sep="\t")
imdb_rotten<- imdb_rotten[!duplicated(imdb_rotten$title),]

movie <- merge(imdb, imdb_rotten, by.x = "original_title", by.y = "title")

######################################
#########     data cleaning
######################################
movie <- movie %>% 
  filter(budget != 0) %>% 
  filter(revenue != 0) %>%
  filter(!str_detect(as.character(rtAllCriticsRating),"\\\\N")) %>%
  filter(!is.na(as.numeric(as.character(rtAllCriticsRating)))) %>%
  filter(as.numeric(as.character(rtAllCriticsRating)) != 0) %>%
  select(-c(homepage,id.x,original_title,id.y,imdbID,imdbPictureURL,spanishTitle,rtPictureURL))

movie$rtAllCriticsRating<-as.numeric(as.character(movie$rtAllCriticsRating))

keyword<-movie %>% 
  filter(nchar(keywords)>2) %>%
  unnest(lapply(keywords,fromJSON)) %>%
  select(title,keyword=name)

companies<-movie %>% 
  filter(nchar(production_companies)>2) %>%
  unnest(lapply(production_companies,fromJSON)) %>%
  select(budget,revenue,company=name)

genre<-movie %>% 
  filter(length(genres)>2) %>%
  unnest(lapply(genres,fromJSON)) %>%
  select(title,genre=name,vote_average,rtAllCriticsRating)

country<-movie %>% 
  filter(nchar(production_countries)>4) %>%
  unnest(lapply(production_countries,fromJSON)) %>%
  select(budget,revenue,popularity,country=name,vote_average,rtAllCriticsRating)

write_csv(movie,path = "movie.csv")
write_csv(keyword,path = "keyword.csv")
write_csv(companies,path = "companies.csv")
write_csv(genre,path = "genre.csv")
write_csv(country,path = "country.csv")


############ rating in 2 websites
m<-seq(0,10,by=1)
imdb = as.character(cut(movie$vote_average,m))
rt = as.character(cut(as.numeric(as.character(movie$rtAllCriticsRating)),m))

rate<-matrix(NA,2*dim(movie)[1],2)
rate[1:dim(movie)[1],1]<-imdb
rate[(1+dim(movie)[1]):(2*dim(movie)[1]),1]<-rt
rate[1:dim(movie)[1],2]<-"imdb"
rate[(1+dim(movie)[1]):(2*dim(movie)[1]),2]<-"rotten tomatoes"
colnames(rate)<-c("cut","type")

ggplot(as.data.frame(rate))+
  geom_bar(aes(x=cut,fill=type), position = "dodge") +
  ggtitle("Comparison of rating in rotten tomatoes and IMDb") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_discrete(name = "website") +
  scale_colour_discrete(labels = c("Rotten tomatoes", "IMDb"))

############ number of movies by year
movie_num<-movie %>%
  select(title,year) %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) 
head(movie_num)

ggplot(movie_num, aes(year,count)) +  geom_line() + geom_point() +
  ggtitle("Number of movies by year")+
  xlab("Number of movies") + ylab("Year")  + 
  theme(plot.title = element_text(hjust = 0.5))

############ rating by month
library(lubridate)
month<-data.frame(tibble(m = as.factor(month(ymd(movie$release_date))),
                         rate = movie$vote_average))

month %>% 
  ggplot(aes(m,rate))+
  geom_boxplot(varwidth = T) +
  xlab("month") + ylab("ratings") +
  ggtitle("Average rating by month") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none")

############ genre
genre %>%
  ggplot(aes(genre,vote_average)) +
  geom_boxplot(varwidth = T) +
  xlab("") + ylab("ratings") +
  ggtitle("Comparison of ratings by genres (IMDb)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=90),legend.position="none")

genre %>%
  ggplot(aes(genre,rtAllCriticsRating)) +
  geom_boxplot(varwidth = T) +
  xlab("") + ylab("rating") +
  ggtitle("Comparison of ratings by genres (Rotten Tomatoes)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=90),legend.position="none")

############ company
com_movie<-companies %>%
  group_by(company) %>%
  summarise(count = n(),total = sum(revenue)) %>%
  arrange(desc(count)) %>%
  head(10)
com_movie

com_movie %>%
  ggplot(aes(reorder(company,count),count,fill = company)) +
  geom_bar(stat="identity") +
  xlab("") + ylab("number of movies") +
  ggtitle("Top 10 companies producing most movies") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=90),legend.position="none") +
  geom_text(aes(label=count),vjust=1.5,colour="white")

############ keyword

word<-keyword %>% 
  group_by(keyword) %>% 
  summarise(count=length(keyword)) %>% 
  arrange(desc(count))   
word %>% head(15)
wordcloud(words = word$keyword,freq = word$count,
          random.order=FALSE,max.words = 150,rot.per=0.60, 
          colors=brewer.pal(8, "Dark2"))
############ country
country10<-country %>%
  group_by(country) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)

country %>%
  filter(country %in% country10$country) %>%
  ggplot(aes(country,vote_average)) +
  geom_boxplot(varwidth = T) +
  xlab("") + ylab("") +
  ggtitle("Comparison of ratings by countries (IMDb)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=90),legend.position="none")

country %>%
  filter(country %in% country10$country) %>%
  ggplot(aes(country,rtAllCriticsRating)) +
  geom_boxplot(varwidth = T) +
  xlab("") + ylab("") +
  ggtitle("Comparison of ratings by countries (Rotten Tomatoes)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=90),legend.position="none")


############ popular

popular_movie<-movie %>%
  select(title,popularity) %>%
  arrange(desc(popularity)) %>%
  head(10)
popular_movie

############ profit
profit_movie<-movie %>%
  select(title,revenue,budget) %>%
  mutate(profit = revenue - budget) %>%
  arrange(desc(profit)) %>%
  head(10) 
profit_movie

############ correlation
movie %>%  
  select(budget,popularity) %>%
  distinct() %>%
  ggplot(aes(budget,popularity)) +
  stat_bin_hex(bins=15) +
  scale_fill_distiller(palette="Spectral") +
  stat_smooth(method="lm",color="orchid",size=2) +
  scale_x_continuous(labels=scales::comma) +
  ggtitle("correlation") +
  theme(plot.title=element_text(hjust=0.5)) 


v