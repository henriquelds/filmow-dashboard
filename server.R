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
library(plyr)
library(dplyr)
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
    
    output$histcountries <- renderPlot({
        query <- sqlInterpolate(ANSI(), "SELECT country as country 
                FROM movie_to_countries;")
        outp <- dbGetQuery(pool, query)
        outp$country <- factor(outp$country)
        levels(outp$country) <- sub("Reino Unido da Grã-Bretanha e Irlanda do Norte", "Reino Unido", levels(outp$country))
        levels(outp$country) <- sub("Estados Unidos da América", "Estados Unidos", levels(outp$country))
        w <- Merge.factors(outp$country, .01)
        outp$newcountry <- w
        b <- outp %>% group_by(newcountry) %>% summarise(count=n())
        b <- max(b$count)
        nl <- names(sort(table(outp$newcountry),decreasing=FALSE))
        outp <- within(outp,newcountry <- factor(newcountry,levels=nl))
        p <- ggplot(outp, aes(newcountry, fill=newcountry))
        p <- p + geom_bar(width=1, colour="white")
        p <- p + geom_text(aes(y=..count.., label=..count..),
                           stat="count", color="black",
                           hjust=0.0, size=6)
        p <- p + labs(y = "Filmes")
        p <- p + theme(legend.position="none", axis.text=element_text(colour="black",size=14,face="bold"), 
                       axis.title.x = element_text(colour="black",size=14,face="bold"),
                       axis.title.y = element_blank())
        p <- p + coord_flip(ylim = c(0, b+(b*0.1))) 
        return(p)
    })
    
    output$histgenres <- renderPlot({
        query <- sqlInterpolate(ANSI(), "SELECT genres.genre as genre 
                FROM movie_to_genres
                INNER JOIN genres ON movie_to_genres.genre_id = genres.id;")
        outp <- dbGetQuery(pool, query)
        x <- factor(outp$genre)
        w <- Merge.factors(x, .01)
        outp$newgenre <- w
        b <- outp %>% group_by(newgenre) %>% summarise(count=n())
        b <- max(b$count)
        nl <- names(sort(table(outp$newgenre),decreasing=FALSE))
        outp <- within(outp,newgenre <- factor(newgenre,levels=nl))
        p <- ggplot(outp, aes(newgenre, fill=newgenre))
        p <- p + geom_bar(width=1, colour="white")
        p <- p + geom_text(aes(y=..count.., label=..count..),
                           stat="count", color="black",
                           hjust=0.0, size=4)
        p <- p + labs(y = "Filmes")
        p <- p + theme(legend.position="none", axis.text=element_text(colour="black",size=14,face="bold"), 
                       axis.title.x = element_text(colour="black",size=14,face="bold"),
                       axis.title.y = element_blank())
        p <- p + coord_flip(ylim = c(0, b+(b*0.1)))
        return(p)
    })
    
    output$histruntime <- renderPlot({
        query <- sqlInterpolate(ANSI(), "SELECT movies.runtime as runtime 
                FROM movies;")
        outp <- dbGetQuery(pool, query)
        outp <- outp %>% mutate(runtime=cut(runtime,breaks=c(seq(0,300,20),10000))) # %>%
        outp$runtime <- factor(outp$runtime)
        b <- outp %>% group_by(runtime) %>% summarise(count=n())
        b <- max(b$count)
        p <- ggplot(outp, aes(runtime, fill=runtime))
        p <- p + geom_bar(width=1, colour="white")
        p <- p + geom_text(aes(y=..count.., label=..count.., angle=90),
                           stat="count", color="black",
                           hjust=0, size=5)
        p <- p + labs(x = "Duração (min)")
        p <- p + theme(legend.position="none", axis.text.y = element_text(colour="black",size=14,face="bold"), 
                       axis.text.x = element_text(colour="black",size=14,face="bold", angle=90, vjust = 0.8), 
                       axis.title.x = element_text(colour="black",size=14,face="bold"),
                       axis.title.y = element_blank())
        p <- p + ylim(0,b+(b*0.12))
        return(p)
    })
    
    output$histyear <- renderPlot({
        query <- sqlInterpolate(ANSI(), "SELECT movies.year as year 
                FROM movies;")
        outp <- dbGetQuery(pool, query)
        outp <- outp %>% mutate(year=cut(year,dig.lab = 4, breaks=c(seq(1900,2020,10),2100))) # %>%
        
        outp$year <- factor(outp$year)
        b <- outp %>% group_by(year) %>% summarise(count=n())
        b <- max(b$count)
        p <- ggplot(outp, aes(year, fill=year))
        p <- p + geom_bar(width=1, colour="white")
        p <- p + geom_text(aes(y=..count.., label=..count.., angle=90),
                           stat="count", color="black",
                           hjust=0, size=5)
        p <- p + labs(x = "Ano")
        p <- p + theme(legend.position="none", axis.text.y = element_text(colour="black",size=14,face="bold"), 
                       axis.text.x = element_text(colour="black",size=14,face="bold", angle=90, vjust = 0.8), 
                       axis.title.x = element_text(colour="black",size=14,face="bold"),
                       axis.title.y = element_blank())
        p <- p + ylim(0,b+(b*0.12))
        return(p)
    })
    
    output$histcities <- renderPlot({
        query <- sqlInterpolate(ANSI(), "SELECT users.city as city 
                FROM users;")
        outp <- dbGetQuery(pool, query)
        outp <- na.omit(outp)
        outp$city <- factor(outp$city)
        outp$city <- Merge.factors(outp$city, .01)
        b <- outp %>% group_by(city) %>% summarise(count=n())
        b <- max(b$count)
        nl <- names(sort(table(outp$city),decreasing=FALSE))
        outp <- within(outp,city <- factor(city,levels=nl))
        p <- ggplot(outp, aes(city, fill=city))
        p <- p + geom_bar(width=1, colour="white")
        p <- p + geom_text(aes(y=..count.., label=..count..),
                           stat="count", color="black",
                           hjust=0.0, size=6)
        p <- p + labs(y = "Usuários")
        p <- p + theme(legend.position="none", axis.text=element_text(colour="black",size=14,face="bold"), 
                       axis.title.x = element_text(colour="black",size=14,face="bold"),
                       axis.title.y = element_blank())
        p <- p + coord_flip(ylim = c(0, b+(b*0.1))) 
        return(p)
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
