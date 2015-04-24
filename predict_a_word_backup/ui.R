library(shiny)

shinyUI(fluidPage(theme = "bootstrap.css",
  titlePanel("Predict-a-word"),
  br(),
  sidebarLayout(
    sidebarPanel(
      h4("Type a sentence, minus the last word.", align = "center", style = "font-family: 'sans serif'; color:cornflowerblue"),
      br(),
      radioButtons("corpus", "This is for a",
                  c("blog." = "blogs", 
                    "news story." = "news",
                    "tweet." = "tweets")),
      textInput("stub", ""),
      actionButton("submission", "Get next word!")),
    mainPanel(
      tabsetPanel(
        tabPanel('Prediction!',
       br(),
       br(),
       p(''),
       img(src = "fortune-cookie-575751_1280.png", height = 130, width = 150, align = "center")),
       h3(textOutput("prediction"),
       br()),
       tabPanel('Background',
       h4(em("Change text type to get a new word and refresh to start over.", align = "center")),
       br(),
       h5('This application uses a model trained on text corpora drawn from blogs, news and tweets to predict a word based on context.'),
       br(),
       p(""),
       p("* The app was built as part of the Capstone project in the ",
         a("Coursera Data Science certificate.", 
           href = "https://www.coursera.org/specialization/jhudatascience/1")),
       p("* The project was undertaken in cooperation with ",
         a("Swiftkey.", 
           href = "http://swiftkey.com/en/")))
       
    )))))