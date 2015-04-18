require(shiny)

shinyUI(fluidPage(
  titlePanel("What's the word?"),
  tags$pre("Text Prediction Application", align = "left", style = "font-family: 'georgia'; font-si16pt"),
  br(),
  sidebarLayout(
    sidebarPanel(
      h4(em("Type a sentence, leaving out the last word, and let me do the rest!")),
      br(),
      radioButtons("corpus", "I'm looking for a word for my",
                  c("blog" = "blogs", 
                    "news story" = "news",
                    "tweet" = "tweets")),
      br()),
      textInput("stub", "What do you have so far?", value = "")
      actionButton("userinput", label = "Submit"), 
    mainPanel(
      tabsetPanel(
        tabPanel('Prediction:',
          br(),
          br(),
          h4('Here's the next word in your sentence:'),
          textOutput("prediction"),
          br(),
          br(),
          br(),
          br(),
          br(),
          p(''),
      img(src = "word-cloud-680706_640.png", height = 100, width = 100)
        ),
        tabPanel('Background:',
          br(),
          h5('This application uses a model trained on text corpora drawn from blogs, news and tweets to predict a word based on context.'),
          br(),
          br(),
          p(""),
          p("The app was built as part of the Capstone project in the ",
            a("Coursera Data Science certificate.", 
              href = "https://www.coursera.org/specialization/jhudatascience/1")),
          p("The project was undertaken in cooperation with ",
            a("Swiftkey.", 
              href = "http://swiftkey.com/en/"))
        )
)))))
