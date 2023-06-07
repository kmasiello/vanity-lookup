
library(shiny)

# Define UI for application that draws a histogram

library(connectapi)
library(pins)
library(dplyr)
library(gt)

client <- connect()
board <- board_connect()

content_w_vanities <- board |> pin_read("katie.masiello/content_w_vanities")

vlu <- function(pattern) {
      dplyr::filter(content_w_vanities, stringr::str_detect(url, pattern)) |> 
        select(url, name, title, owner_username, guid, access_type) |> 
        mutate(url = paste0("<a href=",url,">",url,"</a>")) |> 
        rename(custom_url = url) |> 
        mutate(dashboard_url = paste0(
            "<a href=",connectapi::dashboard_url_chr(client$server, guid),">",
            connectapi::dashboard_url_chr(client$server, guid),"</a>")
               ) |> 
        mutate(custom_url = purrr::map(custom_url, ~ gt::html(as.character(.x)))) |> 
        mutate(dashboard_url = purrr::map(dashboard_url, ~ gt::html(as.character(.x)))) |> 
        gt() |> 
        cols_align(
            align = "left",
            columns = c(custom_url, dashboard_url)
        )

     
    
}


ui <- fluidPage(

    titlePanel("Custom URL reverse lookup"),
    
    fluidPage(
        textInput("pattern", "Input all or part of custom URL"),
  
        
        gt_output("results")
    )


)

server <- function(input, output) {
    

    
    output$results <- 
        render_gt({
            vlu(input$pattern)
        })


    
}

# Run the application 
shinyApp(ui = ui, server = server)
