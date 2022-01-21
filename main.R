# REQUIRED PACKAGES
library(stringr)
library(rvest) 
library(tidyverse) 
library(jsonlite) 
library(tidytext)
library(lubridate) 
library(wordcloud)
library(httr)
library(ggplot2)
library(wordcloud2)
library(RCurl)
library(curl)
library(pbapply)
library(ggthemes)
library(plotly)
library(kableExtra)

# SET THE NUMBER OF REQUESTS YOU WANT TO MAKE TO THE YOUTUBE API
# DAILY QUOTA FOR THE YOUTUBE API IS 10000
# IF YOUR NUMBER OF REQUESTS EXCEEDS 10000 YOU CAN JUST RERUN THE SCRIPT THE NEXT DAY
# TILL YOU GET ALL YOUR DATA ANALYSED, THE SCRIPT SAVES ALLREADY LOADED METADATA

requestNumber <- 9000

# PUT URL HERE "C:\\Users\\ ..."
SearchHistoryUrl <- ""
watchHistoryUrl <- ""

# IF YOU WANT TO RERUN THE SCRIPT MULTIPLE TIMES YOU CAN CHOOSE TO NOT REANALYZE THE TAKEOUT DATA EVERY TIME
# SAVES YOU SOME TIME
analyseTakeout <- FALSE

# ESTABLISH API KEY AND CONNECTION
youtubeAPIKey <- "PUT_YOUR_API_KEYHERE"
connectionURL <- 'https://www.googleapis.com/youtube/v3/videos'

if(analyseTakeout) 
{
# READ SEARCH HISTORY
youtubeSearchHistory <- read_html(SearchHistoryUrl)

# SCRAPING SEARCH HISTORY
youtubeSearch <- youtubeSearchHistory %>%
  html_nodes(".header-cell + .content-cell > a") %>%
  html_text()

# SCRAPING TIMESTAMP
youtubeSearchContent <- youtubeSearchHistory %>%
  html_nodes(".header-cell + .content-cell")
youtubeSearchTimeStr <- str_match(youtubeSearchContent, "<br>(.*?)</div>")[,2]
youtubeSearchTime <- dmy_hms(youtubeSearchTimeStr)


# CREATING DATA FRAME SEARCH + TIMESTAMP
youtubeSearchDataFrame <- data.frame(search = youtubeSearch, 
                                     time = youtubeSearchTime,
                                     stringsAsFactors = FALSE)

# READ WATCH HISTORY 
watchHistory <- read_html(watchHistoryUrl)

watchedVideoContent <-  watchHistory %>%
  html_nodes(".header-cell + .content-cell")

# POSSIBLE TIME CHARACTERS
watchVideoTimes <- str_match(watchedVideoContent, 
                             "</a><br>(.*?)</div>")[,2]

# POSSIBLE ID VALUES 
watchedVideoIDs <- str_match(watchedVideoContent, 
                             "watch\\?v=([a-zA-Z0-9-_]*)")[,2]

# VIDEO TITLE
watchedVideoTitles <- str_match(watchedVideoContent, 
                                "watch\\?v=[a-zA-Z0-9-_]*\">(.*?)</a>")[,2]

# DATA FRAME WATCH HISTORY
watchedVideosDataFrame <- data.frame(id = watchedVideoIDs, 
                                     scrapedTitle = watchedVideoTitles, 
                                     scrapedTime = watchVideoTimes, 
                                     stringsAsFactors = FALSE)

watchedVideosDataFrame$time <- dmy_hms(watchedVideosDataFrame$scrapedTime)

# LOCALY SAVE ANALYSED DATA
save(youtubeSearchDataFrame, file = "C:\\youtubeSearchDataFrame.RData")
save(watchedVideosDataFrame, file = "C:\\watchedVideosDataFrame.RData")
}
{
load("C:\\youtubeSearchDataFrame.RData")
load("C:\\watchedVideosDataFrame.RData")
}

# CREATE REQUEST AND REMOVE DUPLICATES
createRequest  <- function(id){
  paste0(connectionURL,
         "?key=",youtubeAPIKey,
         "&id=",id,
         "&fields=","items(id,snippet(channelId,title,description,categoryId))",
         "&part=","snippet")
}
uniqueWatchedVideoIDs <- unique(watchedVideosDataFrame$id)


# GET LOCALY SAVED META
allreadyLoadedMeta <- read.csv("C:\\allreadyLoadedMeta.csv")
allreadyLoadedMeta <- distinct(allreadyLoadedMeta)

# ONLY CREATES A REQUEST FOR VIDEOIDS WHICH ARENT ALLREADY LOADED
uniqueWatchedVideoIDs <- setdiff(uniqueWatchedVideoIDs, allreadyLoadedMeta$id)
requests <- pblapply(uniqueWatchedVideoIDs, createRequest )


# PARSE OUT RESPONSE
getMetadataDataFrame <- function(response){
  rawchar <- rawToChar(response$content)
  parsedData <- fromJSON(rawchar)
  data.frame <- cbind(id = parsedData$items$id, parsedData$items$snippet)
  return(data.frame)
}

videoMetadataDataFrame <- data.frame(id = c(), 
                                       channelId = c(), 
                                       title = c(), 
                                       description = c(), 
                                       categoryId = c()
                                       )

# SUCCESS
addToMetadataDataFrame <- function(response){
  .GlobalEnv$videoMetadataDataFrame <- rbind(.GlobalEnv$videoMetadataDataFrame,getMetadataDataFrame(response))
}

# FAIL
failFunction <- function(request){
  print("fail")
}

# GRAB REQUEST RESPONSE FROM MEMORY
fetchMetadataFromMemory <- function(request){
  return(getMetadataDataFrame(curl_fetch_memory(request)))
}

system.time(out <- multi_run(pool = pool)) 
saveRDS(videoMetadataDataFrame, file = "videoMetadataDataframeAsync1.rds")

length(requests)

listMetadata <- pblapply(requests[1:requestNumber], fetchMetadataFromMemory)
allreadyLoadedMeta <- rbind(allreadyLoadedMeta, bind_rows(listMetadata))

#SAVE METADATA TO DISK
write.csv(allreadyLoadedMeta, file="C:\\allreadyLoadedMeta.csv", row.names = FALSE)
listMetadata <- allreadyLoadedMeta

# COMBINE LIST INTO A DATA FRAME
videoMetadataDataFrame <- bind_rows(listMetadata)
saveRDS(videoMetadataDataFrame, file = "videoMetadataDataFrame_memory.rds")

# CATEGORY ID REQUEST
categoryListURL <- "https://www.googleapis.com/youtube/v3/videoCategories"

categoryResponse <- GET(url = categoryListURL,
                         query = list(
                           key = youtubeAPIKey,
                           regionCode = "de",
                           part = "snippet"
                         ))
parsedCategoryResponse <- content(categoryResponse, "parsed")


categoryDataFrame <- data.frame(categoryId=c(), category=c())
for(item in parsedCategoryResponse$items){
  categoryDataFrame <<-rbind(categoryDataFrame, 
                             data.frame(categoryId = item$id, category=item$snippet$title))
}

categoryDataFrame
videoMetadata <- merge(x = videoMetadataDataFrame, y = categoryDataFrame, by = "categoryId")

# COMBINE WITH WATCH HISTORY
watchedVideos <- merge(watchedVideosDataFrame , videoMetadata, by="id")
write.csv(bind_rows(watchedVideos), file="C:\\watchedVideos.csv", row.names = FALSE)

# VISUALIZE VIDEO CATEGORIES WATCHED 
watchedVideos%>% 
  group_by(category) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

watchedVideos %>% 
  ggplot(aes(x = time, fill = category)) + 
  labs(x= "Year", y= "Count") + 
  ggtitle("How much have your genre tastes changed over time?", "Most played categories")+
  geom_area(bins= 40,stat = "bin",position = "fill") + 
  theme_economist_white()+
  scale_y_continuous(labels = scales::percent)

ggplotly()

# VISUALIZE CLOCK WATCHES PER HOUR
clockPlot <- function (x, col = heat.colors(n), ...) {
  if( min(x)<0 ) x <- x - min(x)
  if( max(x)>1 ) x <- x/max(x)
  n <- length(x)
  if(is.null(names(x))) names(x) <- 0:(n-1)
  m <- 1.05
  plot(0, type = 'n', xlim = c(-m,m), ylim = c(-m,m), 
       axes = F, xlab = '', ylab = '', ...)
  fig <- pi/2 - 2*pi/200*0:200
  polygon( cos(fig), sin(fig) )
  f2 <- .02
  fig <- pi/2 - 2*pi/n*0:n
  segments( (1+f2)*cos(fig), (1+f2)*sin(fig), (1-f2)*cos(fig), (1-f2)*sin(fig) )
  segments( cos(fig), sin(fig),0, 0, col = 'light grey', lty = 3)
  f1 <- -2*pi/n*(0:50)/50
  for (i in 1:n) {
    fig <- pi/2 - 2*pi/n*(i-1)
    b <- pi/2 - 2*pi/n*i
    polygon( c(0, x[i]*cos(fig+f1), 0), c(0, x[i]*sin(fig+f1), 0), col=col[i] )
    f2 <- .1
    text((1+f2)*cos(fig), (1+f2)*sin(fig), names(x)[i])
  }
}

clockDataFrame <- watchedVideos %>% 
  mutate(hour = hour(time)) %>% 
  group_by(hour) %>% 
  summarise(count = n()) %>% 
  arrange(hour) 

clockPlot(clockDataFrame$count, main = "What hours do you spend the most time watching YouTube?") 

# TABLE MOST RE-WATCHED VIDEOS
w1 <- watchedVideos %>%
  mutate(year = year(time)) %>% 
  group_by(year, title) %>% 
  summarise(count = n()) %>% 
  arrange(year, desc(count)) %>% 
  top_n(5) 

mostReWatched <- knitr::kable(x = head(arrange(w1, desc(count)) %>%
                                      select(year, title, ,count), 10),
                                      col.names = c('Year', 'Video Title', 'Count'))
kable_styling(mostReWatched, "striped", position = "left", font_size = 12)

# WORDCLOUD MOST SEARCHED WORDS
myWords <- youtubeSearchDataFrame %>%
  unnest_tokens(word, search) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) 

myWordcloud <- myWords %>%
  group_by(word) %>%
  summarize(count = sum(n)) %>%
  anti_join(stop_words)

wordcloud(words = myWordcloud$word, freq = myWordcloud$count, min.freq = 10, 
          max.words = 200, random.order =FALSE, rot.per =.35,
          colors=brewer.pal(9, "Set1"))

# WORDCLOUD MOST FREQUENT WORDS IN VIDEO DESCRIPTIONS
descriptionsWordcloud <- watchedVideos %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(! word %in% c("www.instagram.com", "gmail.com", "www.twitter.com", "youtu.be", "como", "instagram", "instagram.com", "tÃº", "watch", "aquÃ­", "pero", "su", "http", "al","se","si","goo.gl","smarturl.it","facebook","video","mÃ¡s", "twitter", "te","lo","este","tu", "para", "por", "con", "es", "del", "las", "una", "mi", "de", "en", "la", "el", "los", "https", "bit.ly" , "Ã¢", "www.youtube.com")) %>%
  filter(n > 250)

wordcloud2(descriptionsWordcloud)
