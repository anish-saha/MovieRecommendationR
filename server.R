get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
ratings$Timestamp = NULL
small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

#Create genre_matrix
genres = as.data.frame(movies$Genres, stringsAsFactors=FALSE)
tmp = as.data.frame(tstrsplit(genres[,1], '[|]',
                              type.convert=TRUE),
                    stringsAsFactors=FALSE)
genre_list = c("Action", "Adventure", "Animation", 
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical", 
               "Mystery", "Romance", "Sci-Fi", 
               "Thriller", "War", "Western")
m = length(genre_list)
genre_matrix = matrix(0, nrow(movies), length(genre_list))
for(i in 1:nrow(tmp)){
  genre_matrix[i,genre_list %in% tmp[i,]]=1
}
colnames(genre_matrix) = genre_list
remove("tmp", "genres")



# Train UBFC
i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
Rmat = new('realRatingMatrix', data = Rmat)
rec_UBCF = Recommender(Rmat, method = 'UBCF',
                       parameter = list(normalize = 'Z-score', 
                                        method = 'Cosine', 
                                        nn = 5))
movieIDs = colnames(Rmat)
n.item = ncol(Rmat)
shinyServer(function(input, output, session) {
  
  # show the top-movies to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    top = total_ratings()
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = top$image_url[(i - 1) * num_movies + j], height = 150)),
                 div(style = "text-align:center", strong(top$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", top$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      topn = UserbasedRecommendation(user_ratings)$new
      user_results = (1:10)/10
      user_predicted_ids = 1:10
      user_predicted_ids = topn
      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = movies$MovieID[user_predicted_ids], 
                                  Title = movies$Title[user_predicted_ids], 
                                  Predicted_rating =  user_results)
      
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  })# renderUI function
  
  #display recommendations based on genre
  output$result <- renderUI({
    
    num_rows <- 4
    num_movies <- 5
    genre = input$genre
    #read movie ids from selected genre file and load the images and titles
    #filename = paste(genre,".txt")
    #myData = read.delim(filename, header = FALSE)
    #MovieID = myData$V1
    MovieID = average_ratings(genre)
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE,
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
  })

  # Userbased recommendation
  UserbasedRecommendation = function(userRating)
  {
    
    new.ratings = rep(NA, n.item)
    for(x in 1:nrow(userRating))
    {
      x = userRating[x]
      x$MovieID = paste("m",x$MovieID,collapse = NULL,sep = "")
      new.ratings[which(movieIDs == x$MovieID)] = x$Rating
    }
    new.user = matrix(new.ratings, 
                      nrow=1, ncol=n.item,
                      dimnames = list(
                        user=paste('new'),
                        item=movieIDs
                      ))
    new.Rmat = as(new.user, 'realRatingMatrix')
    recom2 = predict(rec_UBCF, new.Rmat, type = 'topN')
    return(recom2@items)
  }
  
  ## get the top movies
  total_ratings = function()
  {
    tmp = ratings %>% group_by(MovieID) %>% summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%inner_join(movies, by = 'MovieID')
    genre_movie_rating1 = tmp[order(tmp$ratings_per_movie,decreasing = TRUE),]
    top = head(genre_movie_rating1,120)
    return(top)
  }
  
  #get top ratings based on genre
  average_ratings = function(genre)
  {
    tmp = ratings %>% 
      group_by(MovieID) %>% 
      summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
      inner_join(movies, by = 'MovieID')
    index = which(genre_matrix[,genre]==1)
    moviesIds = movies[index,]$MovieID
    tmp = tmp[tmp$MovieID %in% moviesIds,]
    index = which(movies$MovieID %in% tmp$MovieID)
    tmp = cbind(index,tmp)
    genre_movie_rating1 = tmp[order(tmp$ave_ratings,decreasing = TRUE),]
    top = head(genre_movie_rating1,20)
    return(top$index)
  }
  
}) # server function