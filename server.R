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
    
}
