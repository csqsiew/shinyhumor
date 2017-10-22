# A cleaned up version of the 'humorous phrases' shiny app.
# 10/5/17 
# drop_upload does not work locally, however 

library(shiny)

# user interface
ui <- fluidPage(

  # title
  h3('Humorous Phrases?'), 
  # instructions
  h5("Click on the buttons below to indicate if the phrase below is 'humorous' or 'humorless'..."),
  h6(em("(A new phrase will appear!)")),
  # show a pair of words
  h2(textOutput(outputId = 'newpair')),
  br(),
  br(),
  # input from user via action buttons 
  actionButton(inputId = 'answer1', label = 'Humorous', width = '100px'),
  actionButton(inputId = 'answer2', label = 'Humorless', width = '100px'),
  br(),
  br(),
  br(),
  # additional info 
  h6("Remember: Your responses are anonymous and you are free to leave the website at any time."),
  h6("If you have any questions about this project, 
     please don't hesitate to contact Tomas Engelthaler (T.Engelthaler@warwick.ac.uk).")
  )

# global commands to set up data collection
library(rdrop2) 

token <- readRDS("droptoken.rds") # droptoken.rds file must be uploaded to the server in the same directory as app.R 
drop_acc(dtoken = token)          # authenticates CS's dropbox account 
dbfolder <- "shinyhumor"  

# global commands to set up randomization
data <- read.csv('humor_ortho.csv') # humor_ortho.csv file must be uploaded to the server in the same directory as app.R
words <- as.vector(data$x)

#function to generate a random string of letters and numbers for unique filenames 
MHmakeRandomString <- function(n=1, length=12)
{
  randomString <- c(1:n) # initialize vector
  for (i in 1:n)
  {
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                    length, replace=TRUE),
                             collapse="")
  }
  return(randomString)
}

# backend processes (which runs once per user/session)
server <- function(input, output, session) {
  
  #make a reactive object
  word.pair <- reactive( {
    input$answer1 # if either action button is pressed, re-sample word pairs 
    input$answer2
    sample(words, 2, replace=F)
  } )
  
  observeEvent(input$answer1, {                  # if button 1 is pressed...
    save.data <<- append(save.data, '-Humorous') # save the response 'humorous'
    save.data <<- append(save.data, word.pair()) # save the next word pair generated
  })
  
  observeEvent(input$answer2, {                   # if button 2 is pressed...
    save.data <<- append(save.data, '-Humorless') # save the response 'humorless'
    save.data <<- append(save.data, word.pair())  # save the next word pair generated
  })
  
  # render word.pairs as output to the ui
  output$newpair <- renderText({ 
    word.pair()
  })
  
  # set of commands to be run in isolation
  isolate( { 
    save.data <- vector()                                    # initialize empty vector
    save.data <- append(save.data, as.character(Sys.time())) # save date and time
    save.data <- append(save.data, word.pair())              # save the first word.pair
  } )
  
  # this code will be run after the client has disconnected
  # saves the data as a .csv file to CS's dropbox folder 'shinyhumor'
  session$onSessionEnded(function() {
    dbfolder <- "shinyhumor"                                                 # folder in dropbox to save data files to 
    file_path <- file.path(tempdir(), paste0(MHmakeRandomString(), '.csv') ) # create temp file with random generator
    write.csv(save.data, file_path, row.names=F)                             # save data to a temp file
    drop_upload(file_path, dbfolder, dtoken = token)                         # upload temp file to dropbox, dtoken = token for authentication 
  } )
  
}

shinyApp(ui = ui, server = server)
