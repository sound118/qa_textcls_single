library(fastrtext)
library(jiebaR)
library(stringr)
library(readxl)
library(readr)
library(shiny)

Sys.setlocale("LC_ALL","chinese")

ui <-fluidPage(
      titlePanel(title = div("ABO Feedback Text Classification", img(src="R&D Logo.PNG", height=80, width=240, style = "float:right; padding-right:25px"))),
      
      sidebarLayout(
        
        sidebarPanel(
          textAreaInput("input_text", "Enter text here...", height ="400px"),
          actionButton("do", "Submit"),
          helpText("When you click the button above, you should see",
                   "the output on the right update to reflect the defect code you",
                   "entered at the top"),
          
          br(),
          
          h6("Powered By:"),
          tags$img(src="AWS_R.png", height=80, width=180),
          tags$img(src="logo.png", height=80, width=280)
          
        ),
        # mainPanel(verbatimTextOutput("value"))
        mainPanel( uiOutput("tb") )
      )
)

server <- function(input, output, session) {
  df <- eventReactive(input$do, {
    # tdf <- data.frame(input_text = input$input_text, stringsAsFactors = FALSE)
    # tdf$input_text
    c(input$input_text)
  })
  
  # source("C:/Users/CNU074VP/Documents/ABO Feedback Text Classification/global.r")
  model <- load_model("/home/jasonyang/txtcls_model.bin")
  
  output$value <- renderText({
    engine <- worker()
    text1 <- paste(str_extract_all(df(), "[\u4e00-\u9fa5]")[[1]], collapse = "")
    text2 <- paste(segment(text1, engine), collapse = " ")
    # paste0(text2)
    pred_label <- predict(model, text2)
    paste0("The most likely predicted defect code to your input text is ", substr(rownames(data.frame(pred_label)), 10,12), " with probability ", round(data.frame(pred_label)[1,1], digits = 4), ".")
  })
  
  output$caption <- renderText({paste("This APP is developed by Jason Yang. It is traind by deep learning algorithm called fastRText. For any technical issue, please contact jason.jian.yang@amway.com, thank you!")})
  
  
  output$tb <- renderUI({
    if(is.null(df()))
      h5("Powered By", tags$img(src='AWS_R.png', heigth=100, width=200))
    else
      tabsetPanel(tabPanel("Predicted Result", verbatimTextOutput("value")), tabPanel("About the APP", textOutput("caption")))
    
    
  })
}

shinyApp(ui = ui, server = server)



