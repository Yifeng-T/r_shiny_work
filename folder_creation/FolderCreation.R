library(shiny)
library(shinyFiles)
library(shinyalert)
library(shinyWidgets)

ui <- fluidPage( # Application title
  mainPanel(
    headerPanel("Neuroscience Work Folders Creation Tool"),
    img(src="neuro.png", height = 300, width = 400),
    br(),
    br(),
    br(),
    radioButtons(
      "options",
      "choose the way you want change",
      choiceNames=c("qa and prd","Only qa", "Only prd"),
      choiceValues = c(1,2,3),
      selected = "qa and prd"
    ),
    br(),
    br(),
    shinyDirButton("dir", "Chose the Working Directory", "Upload"),
    verbatimTextOutput("dir", placeholder = TRUE),
    actionBttn(
      inputId = "bttn",
      label = "APPLY!",
      style = "pill",
      color = "danger"
    )
  ))

server <- function(input, output) {
  bttn_click <- 0
  bttn_text <- NULL
  shinyDirChoose(
    input,
    'dir',
    roots = c(home = '~'),
    filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
  )
  
  dir <- reactive(input$dir)
 
  output$dir <- renderText({  # use renderText instead of renderPrint
    parseDirPath(c(home = '~'), dir())
  })
  observe({
    #print(input$options=="1")
    if(!is.na(parseDirPath(c(home = '~'), dir())[1]) && input$bttn > bttn_click){
      path = parseDirPath(c(home = '~'), dir())[1]
      if(file.exists(paste(path, "/data", sep = ""))){
        shinyalert("Warning: the selected folder [data] has already exist", type = "error")
      }
      
      else{
        dir.create (paste(path, "/data", sep = "")) #include full file path
        dir.create (paste(path, "/data/analysis", sep = ""))
        dir.create (paste(path, "/data/analysis/shared", sep = ""))
        dir.create (paste(path, "/data/analysis/shared/custom", sep = ""))
        dir.create (paste(path, "/data/observed", sep = ""))
        dir.create (paste(path, "/data/observed/shared", sep = ""))
        dir.create (paste(path, "/data/raw", sep = ""))
        dir.create (paste(path, "/data/raw/dm", sep = ""))
        dir.create (paste(path, "/data/raw/dm/blinded", sep = ""))
        dir.create (paste(path, "/data/raw/dm/edc", sep = ""))
        dir.create (paste(path, "/data/raw/dm/tpo", sep = ""))
        dir.create (paste(path, "/data/raw/dm/unblinded", sep = ""))
        dir.create (paste(path, "/data/raw/shared", sep = ""))
        shinyalert("folder(s)[data] create successed", type = "success")
      #print(path)
      }
      if(file.exists(paste(path, "/documentation", sep = ""))){
        shinyalert("Warning: the selected folder [documentation] has already exist", type = "error")
      }
      
      else{
        dir.create (paste(path, "/documentation", sep = "")) #include full file path
        dir.create (paste(path, "/documentation/meta", sep = ""))
        dir.create (paste(path, "/documentation/meta/analysis", sep = ""))
        dir.create (paste(path, "/documentation/meta/analysis/observed", sep = ""))
        dir.create (paste(path, "/documentation/meta/analysis/observed/tfl", sep = ""))
        dir.create (paste(path, "/documentation/specs", sep = ""))
        dir.create (paste(path, "/documentation/specs/analysis", sep = ""))
        dir.create (paste(path, "/documentation/specs/analysis/observed", sep = ""))
        dir.create (paste(path, "/documentation/specs/analysis/observed/tfl", sep = ""))
        shinyalert("folder(s)[documentation] create successed", type = "success")
      }
      
      if(file.exists(paste(path, "/logs", sep = ""))){
        shinyalert("Warning: the selected folder [logs] has already exist", type = "error")
      }
      
      else{
        dir.create (paste(path, "/logs", sep = ""))
        dir.create (paste(path, "/logs/analysis", sep = ""))
        dir.create (paste(path, "/logs/analysis/primary", sep = ""))
        dir.create (paste(path, "/logs/analysis/validation", sep = ""))
        
        dir.create (paste(path, "/logs", sep = ""))
        dir.create (paste(path, "/logs/non_sas", sep = ""))
        dir.create (paste(path, "/logs/non_sas/primary", sep = ""))
        dir.create (paste(path, "/logs/non_sas/validation", sep = ""))
        
        dir.create (paste(path, "/logs", sep = ""))
        dir.create (paste(path, "/logs/observed", sep = ""))
        dir.create (paste(path, "/logs/observed/primary", sep = ""))
        dir.create (paste(path, "/logs/observed/validation", sep = ""))
        
        dir.create (paste(path, "/logs", sep = ""))
        dir.create (paste(path, "/logs/tfl", sep = ""))
        dir.create (paste(path, "/logs/tfl/primary", sep = ""))
        dir.create (paste(path, "/logs/tfl/validation", sep = ""))
        shinyalert("folder(s)[logs] create successed", type = "success")
        #print(path)
      }
      
      
      if(file.exists(paste(path, "/output", sep = ""))){
        shinyalert("Warning: the selected folder [output] has already exist", type = "error")
      }
      
      else{
        dir.create (paste(path, "/output", sep = ""))
        dir.create (paste(path, "/output/shared", sep = ""))
        dir.create (paste(path, "/output/shared/tfl", sep = ""))
        dir.create (paste(path, "/output/shared/tfldata", sep = ""))
        shinyalert("folder(s)[output] create successed", type = "success")
        #print(path)
      }
      
      
      if(file.exists(paste(path, "/programs", sep = ""))){
        shinyalert("Warning: the selected folder [programs] has already exist", type = "error")
      }
      
      else{
        dir.create (paste(path, "/programs", sep = ""))
        dir.create (paste(path, "/programs/analysis", sep = ""))
        dir.create (paste(path, "/programs/analysis/primary", sep = ""))
        dir.create (paste(path, "/programs/analysis/validation", sep = ""))
        dir.create (paste(path, "/programs/macros", sep = ""))
        dir.create (paste(path, "/programs/non_sas", sep = ""))
        dir.create (paste(path, "/programs/non_sas/primary", sep = ""))
        dir.create (paste(path, "/programs/non_sas/validation", sep = ""))
        dir.create (paste(path, "/programs/observed", sep = ""))
        dir.create (paste(path, "/programs/observed/primary", sep = ""))
        dir.create (paste(path, "/programs/observed/validation", sep = ""))
        dir.create (paste(path, "/programs/tfl", sep = ""))
        dir.create (paste(path, "/programs/tfl/primary", sep = ""))
        dir.create (paste(path, "/programs/tfl/validation", sep = ""))
        
        shinyalert("folder(s)[programs] create successed", type = "success")
        #print(path)
      }
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)


