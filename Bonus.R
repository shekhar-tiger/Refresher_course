## Only 2 Question of Bonus

#Q1

library("dplyr")
library("tidyr")
library("ggplot2")

'    Using IMDB data
#Q1
df <-read.csv("imdb_new.csv")

df <- df[-which(df$year == "video.episode"),] ####### REMOVING NOISE

df <- select(df,2,6,8:9,17:44)

df <- gather(df,Genre,boolean,5:32)
df <- filter(df[!is.na(df$boolean),],boolean == 1)

avg_rating <- df %>%
      group_by(Genre,year) %>%
      summarise(avg = mean(imdbRating, na.rm = TRUE))


min_rating <- df %>%
  group_by(Genre,year) %>%
  summarise(avg = min(imdbRating, na.rm = TRUE))


max_rating <- df %>%
  group_by(Genre,year) %>%
  summarise(avg = max(imdbRating, na.rm = TRUE))

total_run_time_mins <- df %>%
  group_by(Genre,year) %>%
  summarise(avg = sum(duration, na.rm = TRUE))
'

#Q1  Using movie_metadeta

df <-read.csv("C:/Users/shekhar.sharma/Desktop/4th feb practice/movie_metadata.csv")
df <- unique(df)
df <- df[!is.na(df$title_year),]

df <- select(df,title_year,duration,movie_title,genres,imdb_score)

avg_rating <- df %>%
  group_by(genres,title_year) %>%
  summarise(avg_rating = mean(imdb_score, na.rm = TRUE))


min_rating <- df %>%
  group_by(genres,title_year) %>%
  summarise(min_rating = min(imdb_score, na.rm = TRUE))


max_rating <- df %>%
  group_by(genres,title_year) %>%
  summarise(max_rating = max(imdb_score, na.rm = TRUE))

total_run_time_mins <- df %>%
  group_by(genres,title_year) %>%
  summarise(total_run_time_mins = sum(duration, na.rm = TRUE))

df <- merge(merge(merge(avg_rating,min_rating),max_rating),total_run_time_mins)


#Q2
df <-read.csv("C:/Users/shekhar.sharma/Desktop/4th feb practice/imdb_new.csv")

df <- select(df,3,6,9)
df$title_length <- lapply(df$title, function(x) nchar(as.character(x)))
df$title_length <- as.numeric(df$title_length)

df <- df[!is.na(df$imdbRating),] ####### REMOVING NOISE

trend <- df %>%
  group_by(year) %>%
  summarise(avg = mean(title_length, na.rm = TRUE))

trend_plot <- ggplot(trend, aes(x=year, y=avg)) + geom_point()
trend_plot

#############      Bucket Rating  
df$rating_bracket <- lapply(df$imdbRating, function(x) as.integer(x))
df$rating_bracket <- as.factor(as.integer(df$rating_bracket))

trend_rating <- df %>%
  group_by(rating_bracket) %>%
  summarise(avg = mean(title_length, na.rm = TRUE))

#    Ploting
trend_rating_plot <- ggplot(trend_rating, aes(x=rating_bracket, y=avg)) + geom_point()
trend_rating_plot


for (i in c(0.0,0.25,0.5,0.75,1)){
    paste("num_video",i,"_perc", sep = "_")  <-  df %>% 
                                    dplyr::group_by(year) %>% 
                                    dplyr::summarize(quants = quantile(title_length, probs = i))
}

percentile_0 <-  df %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(quants = quantile(title_length)[0][0])

percentile_0.25 <-  df %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(quants = quantile(title_length, probs = 0.25))

percentile_0.5 <-  df %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(quants = quantile(title_length, probs = 0.5))

percentile_0.75 <-  df %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(quants = quantile(title_length, probs = 0.75))

percentile_1 <-  df %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(quants = quantile(title_length, probs = 1))

a <- cbind(percentile_0,percentile_0.25,percentile_0.5,percentile_0.75,percentile_1)
a <- a[,-c(3,5,7,9)]


