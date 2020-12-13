get_user_ratings = function(value_list) {
    dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                      function(x) ifelse(length(x) > 1, x[[2]], NA)),
                     Rating = unlist(as.character(value_list)))
    dat = dat[!is.null(Rating) & !is.na(MovieID)]
    dat[Rating == " ", Rating := 0]
    dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
    dat = dat[Rating > 0]
}

predict_top_movies = function(input_ratings) {
    input = as.matrix(rating_matrix)
    colnames(input) = paste0("i",movies$MovieID)
    for (i in 1:nrow(input_ratings)) {
        item = paste0("i", input_ratings$MovieID[i])
        input[1,item] = input_ratings$Rating[i]
    }
    recom = predict(modelubcf, as(input, "realRatingMatrix"), n=10)
    recom3 = bestN(recom, n = 10)
    top10 = as(recom3, "list")[[1]]
    top10 = as.numeric(str_sub(top10, 2, -1))
    return(top10)
}

myurl = "https://raw.githubusercontent.com/gcbacel/Movies/main/"
avg_rating = read.csv(paste0(myurl, "Data/avg_rating.csv"))
movies = readLines(paste0(myurl, "Data/movies.dat"))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$item = paste0("i",movies$MovieID)
movies = movies %>% inner_join(avg_rating, by = "MovieID")
movies$Title = iconv(movies$Title, "latin1", "UTF-8")
movies$Year = as.numeric(unlist(
    lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))

small_image_url = "https://github.com/gcbacel/Movies/blob/main/Images/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(myurl, "Images/", x, '.jpg?raw=true'))

rating_matrix = data.table(matrix(rep(NA,nrow(movies)), nrow = 1))
colnames(rating_matrix) =paste0("i", avg_rating$MovieID)
modelubcf = readRDS(gzcon(url(paste0(myurl, "Data/modelubcf.rds"))))

shinyServer(function(input, output, session) {
    
    # show the movies to be rated
    output$ratings = renderUI({
        num_rows = 20
        num_movies = 6 # movies per row
        rated_movies = movies %>% filter (n>500)
        sampleidx = sample(x = nrow(rated_movies), size = num_rows * num_movies, replace = FALSE)
        samplemovies = rated_movies[sampleidx,]
        lapply(1:num_rows, function(i) {
            list(fluidRow(lapply(1:num_movies, function(j) {
                list(box(width = 2,
                         div(style = "text-align:center", img(src = samplemovies$image_url[(i - 1) * num_movies + j], height = 150)),
                         div(style = "text-align:center", strong(samplemovies$Title[(i - 1) * num_movies + j])),
                         div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", samplemovies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
            })))
        })
    })
    
    # Calculate recommendations when the button is clicked
    df = eventReactive(input$btn, {
        withBusyIndicatorServer("btn", { # showing the busy indicator
            # hide the rating container
            useShinyjs()
            jsCode = "document.querySelector('[data-widget=collapse]').click();"
            runjs(jsCode)

            # get the user's rating data
            value_list = reactiveValuesToList(input)
            user_ratings = get_user_ratings(value_list)

            # create a standard list with highest average ratings in case no rating is created
            if (nrow(user_ratings)>0) {
                user_predicted_ids = predict_top_movies(user_ratings)
                pred = data.table(MoveID = user_predicted_ids)
                colnames(pred) = "MovieID"
                recom_result = pred %>% inner_join(movies)
            } else {
                recom_result = (movies %>% arrange(-avg_rating) %>%
                                    filter(Year>=1980))[1:10,]
            }
            
        }) # still busy
        
    }) # clicked on button
    
    
    # display the recommendations
    output$results = renderUI({
        num_rows = 2
        num_movies = 5
        recom_result = df()
        if (!exists("recom_result")) num_rows = 0
        lapply(1:num_rows, function(i) {
            list(fluidRow(lapply(1:num_movies, function(j) {
                box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
                    div(style = "text-align:center", 
                        a(img(src = recom_result$image_url[(i - 1) * num_movies + j], height = 150))
                    ),
                    div(style="text-align:center; font-size: 100%", 
                        strong(recom_result$Title[(i - 1) * num_movies + j])
                    )
                    
                )        
            }))) # columns
        }) # rows
    }) # renderUI function
    
    
    # display the recommendations
    output$genresuggestions = renderUI({
        num_rows = 2
        num_movies = 5
        genre = input$genreinput
        genre_result = (movies  %>% 
                        filter(Year>=1970, grepl(genre, Genres)) %>%
                        arrange(-avg_rating))[1:10,]
        lapply(1:num_rows, function(i) {
            list(fluidRow(lapply(1:num_movies, function(j) {
                box(width = 2, status = "success", solidHeader = TRUE,
                    title = paste0("Avg. Rate: ", round(genre_result$avg_rating[(i - 1) * num_movies + j],2)),
                    div(style = "text-align:center", 
                        a(img(src = genre_result$image_url[(i - 1) * num_movies + j], height = 150))
                    ),
                    div(style="text-align:center; font-size: 100%", 
                        strong(genre_result$Title[(i - 1) * num_movies + j])
                    )
                    
                )        
            }))) # columns
        }) # rows
        
    }) # renderUI function
    
    
}) # server function
