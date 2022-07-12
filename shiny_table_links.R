

library(shiny)
library(ggplot2)  # for the diamonds dataset

ui <- fluidPage(
  title = "Examples of DataTables",
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "diamonds"',
        checkboxGroupInput("show_vars", "Columns in diamonds to show:",
                           names(diamonds), selected = names(diamonds))
      ),
      conditionalPanel(
        'input.dataset === "mtcars"',
        helpText("Click the column header to sort a column.")
      ),
      conditionalPanel(
        'input.dataset === "iris"',
        helpText("Display 5 records by default.")
      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("diamonds", DT::dataTableOutput("mytable1")),
        tabPanel("mtcars", DT::dataTableOutput("mytable2")),
        tabPanel("iris", DT::dataTableOutput("mytable3"))
      )
    )
  )
)

server <- function(input, output) {
  
  # choose columns to display
  diamonds2 = diamonds[sample(nrow(diamonds), 1000), ]
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(diamonds2[, input$show_vars, drop = FALSE])
  })
  
  # sorted columns are colored now because CSS are attached to them
  output$mytable2 <- DT::renderDataTable({
    DT::datatable( link = c(mtcars[,1]),
                   cay = c(mtcars[,2]),
                   dadad = c(mtcars[,3]))
  })
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$mytable3 <- DT::renderDataTable({
    DT::datatable(iris, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
}

shinyApp(ui, server)





library(shiny)
library(DT)
library(data.table)

# add links in table
server <- function(input, output, session) {
  
  df_table <- reactive({
    data.table(
      links = c(paste0("<a href='https://www.jingege.wang'>", mtcars[,5], "</a>")),
      Site = c(mtcars[,3]))
    
  })
  
  output$dt_table <- renderDataTable(
    df_table(), escape = FALSE, options = list(pageLength = 5))
  
}

ui <- fluidPage(
  navbarPage('TEST', 
             tabPanel("Table",
                      fluidPage(
                        fluidRow(dataTableOutput("dt_table")))),
             
  )
)

# Run the application 
shinyApp(ui, server)
















