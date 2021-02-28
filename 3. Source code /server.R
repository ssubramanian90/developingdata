## server.R

# load libraries
library(dplyr)
library(ggplot2)
library(recommenderlab)
library(DT)
library(data.table)
library(reshape2)

#load the data
myurl = "https://liangfgithub.github.io/MovieData/"

#ratings = read.csv(paste0(myurl, 'ratings.dat'), sep = ':',colClasses = c('integer', 'NULL'), header = FALSE)
ratings = read.csv('ratings.dat',                    sep = ':',                 colClasses = c('integer', 'NULL'),                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')


movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")


users = read.csv(paste0(myurl, 'users.dat?raw=true'),
                 sep = ':', header = FALSE)
users = users[, -c(2,4,6,8)] # skip columns
colnames(users) = c('UserID', 'Gender', 'Age', 'Occupation', 'Zip-code')

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))  

genre_list=c("Action", "Adventure", "Animation", 
             "Children's", "Comedy", "Crime",
             "Documentary", "Drama", "Fantasy",
             "Film-Noir", "Horror", "Musical", 
             "Mystery", "Romance", "Sci-Fi", 
             "Thriller", "War", "Western")

#output$selected_genre_index <- renderPrint({which(genre_list %in% input$select)})

genres = as.data.frame(movies$Genres, stringsAsFactors=FALSE)
tmp = as.data.frame(tstrsplit(genres[,1], '[|]',
                              type.convert=TRUE),
                    stringsAsFactors=FALSE)
m = length(genre_list)
genre_matrix = matrix(0, nrow(movies), length(genre_list))
for(i in 1:nrow(tmp)){
  genre_matrix[i,genre_list %in% tmp[i,]]=1
}
colnames(genre_matrix) = genre_list

movies_by_ratings = ratings %>% 
  group_by(MovieID) %>% 
  dplyr::summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
  inner_join(movies, by = 'MovieID')

# define functions
get_user_ratings <- function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
  
  ##

}





shinyServer(function(input, output, session) {
  
  # show the movies to be rated
  output$ratingsByRating <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], 
                                                      style = "max-height:150")),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", 
                     ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), 
                                 label = "", dataStop = 5)))) 
      })))
    })
    
  })
  
  
  
  # Calculate recommendations by user when the sbumbutton is clicked
  df1 <- eventReactive(input$btn1, {
    withBusyIndicatorServer("btn1", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        runjs(jsCode)
        
        # get the user's rating data
        value_list <- reactiveValuesToList(input)
        user_ratings <- get_user_ratings(value_list)
        
        
        #Use the ubcf  agorithm  model
        rec_UBCF<-readRDS("model/rec_UBCF.rds")
        movieIds = colnames(getModel(rec_UBCF)$data)
        movieCnt = length(movieIds)
        colIndex = unlist(which(movieIds==paste0('m', user_ratings$MovieID)))
        rmat = sparseMatrix(rep(1, length(colIndex)), colIndex, x = user_ratings$Rating, dims = c(1, movieCnt))
        rownames(rmat) = c('u200000')
        colnames(rmat) = movieIds
        rmat<-as(rmat, "realRatingMatrix")
        rec = predict(rec_UBCF, rmat, n = 50, type = 'topNList')
        res =as(rec, 'list')
        
        # sort, organize, and return the results
        user_results <- sort(res[[1]], decreasing = TRUE)
        user_results <-user_results[!is.na(movies$MovieID[as.numeric(sub('m', '', user_results))])]
        user_results <-user_results[1:20]
        user_predicted_ids <- as.numeric(sub('m', '', user_results))
        filtered_movies<- movies[0,]
        for (i in user_predicted_ids){ 
             filtered_movies[nrow(filtered_movies) + 1,] = movies[movies$MovieID %in% i,]
        }
        
        recom_results <- data.table(Rank = 1:20, 
                                    MovieID = filtered_movies$MovieID, 
                                    Title = filtered_movies$Title, 
                                    Predicted_rating =  user_results,
                                    image_url=filtered_movies$image_url)
        
    }) # still busy
    
  }) # clicked on button
  

  # Calculate recommendations by genre when the sbumbutton is clicked
  df2 <- eventReactive(input$btn2, {
    withBusyIndicatorServer("btn2", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      #jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      jsCode<- "document.getElementById('select').value;"
      runjs(jsCode)
      
      # get the user's genre
      genre <- reactiveValuesToList(input)$select
      filtered_movies=movies[which(genre_matrix[,genre] %in% 1),]$MovieID
      filtered_ratings=movies_by_ratings[movies_by_ratings$MovieID %in% filtered_movies,]
      result_movies=head(filtered_ratings[order(filtered_ratings$ave_ratings, decreasing = TRUE),],20)
      
      recom_results <- data.table(Rank = 1:20, 
                                  MovieID = result_movies$MovieID, 
                                  Title = result_movies$Title, 
                                  Predicted_rating =  result_movies$ave_ratings,
                                  image_url=result_movies$image_url)

      
    }) # still busy
    
  }) # clicked on button
  
  # display the recommendations for collaborative Filtering
  output$resultsByRating <- renderUI({
    num_rows <- 4
    num_movies <- 5
    recom_result <- df1()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, 
            title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", a(img(src = recom_result[(i - 1) * num_movies + j,]$image_url, height = 150))),
            div(style="text-align:center; font-size: 100%", strong(recom_result[(i - 1) * num_movies + j,]$Title))
            
          
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
  output$resultsByGenre <- renderUI({
    num_rows <- 4
    num_movies <- 5
    recom_result <- df2()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, 
            title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", a(img(src = recom_result[(i - 1) * num_movies + j,]$image_url, height = 150))),
            div(style="text-align:center; font-size: 100%", strong(recom_result[(i - 1) * num_movies + j,]$Title))
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
}) # server function
