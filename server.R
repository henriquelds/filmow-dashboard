#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(recommenderlab)
library(Matrix)

# Define server logic required to draw a histogram
server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    output$table <- DT::renderDataTable({
        query <- sqlInterpolate(ANSI(), "SELECT title, year, runtime from movies WHERE year BETWEEN ?min and ?max LIMIT 100;",
                                min = input$slidervisu[1], max = input$slidervisu[2])
        outp <- dbGetQuery(pool, query)
        ret <- DT::datatable(outp)
        return(ret)
    })
    
    output$plotdensrun <- renderPlot({
        query <- sqlInterpolate(ANSI(), "SELECT movs.runtime as runtime
                FROM public.movie_to_directors as dirs
                INNER JOIN public.movies as movs ON movs.movie_tag = dirs.movie_tag AND movs.year BETWEEN ?min and ?max
                INNER join public.ratings as rats ON movs.movie_tag = rats.movie_tag
                where dirs.director = ?dirname ORDER BY movs.year ASC LIMIT 1000;",
                dirname = input$selectizedir, min = input$slideryear[1], max = input$slideryear[2])
        outp <- dbGetQuery(pool, query)
        p <- ggplot(outp, aes(x=runtime)) + 
            geom_histogram(aes(y=..density..), colour="black", fill="white")+
            geom_density(alpha=.2, fill="#FF6666") + labs(x = "Runtime (min)", y = "Density")
        return(p)
    })
    
    output$plotdir <- renderPlot({
        query <- sqlInterpolate(ANSI(), "SELECT rats.rating as rating
                FROM public.movie_to_directors as dirs
                INNER JOIN public.movies as movs ON movs.movie_tag = dirs.movie_tag AND movs.year BETWEEN ?min and ?max
                INNER join public.ratings as rats ON movs.movie_tag = rats.movie_tag
                where dirs.director = ?dirname ORDER BY movs.year ASC LIMIT 1000;",
                dirname = input$selectizedir, min = input$slideryear[1], max = input$slideryear[2])
        outp <- dbGetQuery(pool, query)
        p <- ggplot(outp, aes(x=rating)) + 
            geom_histogram(aes(y=..density..), colour="black", fill="white")+
            geom_density(alpha=.2, fill="#FF6666") + labs(x = "Rating", y = "Density")
        return(p)
    })
    
    userdata <- eventReactive(input$runButton,{
        query <- sqlInterpolate(ANSI(), "SELECT rats.rating as rating
                FROM public.users as users
                INNER join public.ratings as rats ON users.id = rats.user_id
                where users.username = ?username LIMIT 1000;",
                username = input$textUser)
        outp <- dbGetQuery(pool, query)
        return(outp)
    })
    
    userdatayear <- eventReactive(input$runButton,{
        query <- sqlInterpolate(ANSI(), "SELECT rats.rating as rating
                FROM public.users as users
                INNER join public.ratings as rats ON users.id = rats.user_id
                INNER JOIN public.movies as movs ON movs.movie_tag = rats.movie_tag AND movs.year BETWEEN ?min and ?max
                where users.username = ?username LIMIT 1000;",
                username = input$textUser, min = input$slideryearpd[1], max = input$slideryearpd[2])
        outp <- dbGetQuery(pool, query)
        return(outp)
    })
    
    output$plotdensuser <- renderPlot({
        p <- ggplot(userdata(), aes(x=rating)) + 
            geom_histogram(aes(y=..density..), colour="black", fill="white")+
            geom_density(alpha=.2, fill="#FF6666") + labs(x = "Rating", y = "Density")
        return(p)   
    })
    
    output$plotdensuseryear <- renderPlot({
        p <- ggplot(userdatayear(), aes(x=rating)) + 
            geom_histogram(aes(y=..density..), colour="black", fill="white")+
            geom_density(alpha=.2, fill="#FF6666") + labs(x = "Rating", y = "Density")
        return(p)   
    })
    
    recdata <- eventReactive(input$recButton,{
        query <- sqlInterpolate(ANSI(), "SELECT rats.user_id, movs.title, rats.rating as rating
                FROM public.ratings as rats
                INNER JOIN public.movies as movs ON movs.movie_tag = rats.movie_tag
                LIMIT ?lim;", lim=input$numRats)
        outp <- dbGetQuery(pool, query)

        query2 <- sqlInterpolate(ANSI(), "SELECT rats.user_id, movs.title, rats.rating as rating
                FROM public.ratings as rats
                INNER JOIN public.movies as movs ON movs.movie_tag = rats.movie_tag
                INNER JOIN public.users as users ON rats.user_id = users.id AND users.username = ?username
                LIMIT 2000;", username = input$textUserRec)
        outp2 <- dbGetQuery(pool, query2)
        pivotid <- outp2$user_id[1]
        train <- rbind(outp,outp2)
        refid <- unique(train$user_id)
        pivotindex <- which(refid == pivotid)
        train$user_id <- as.factor(train$user_id)
        train$title <- as.factor(train$title)
        train$rating <- as.numeric(train$rating)
        train <- as(train, "realRatingMatrix")
        model2 <- Recommender(data = train, method="IBCF", parameter=list(k=5, method="Cosine"))
        b <- as(predict(model2,pivotindex,data=train, n=input$numRecs), "list")
        return(b[[1]])
    })
    
    output$rectable <- DT::renderDataTable({
        l <- recdata()
        df <- data.frame(matrix(unlist(l), nrow=length(l), byrow=T))
        names(df)[1] <- "Title"
        ret <- DT::datatable(df)
        return(ret)
    })
    
    
}
