library(tidyverse)
library(dplyr)
library(Biostrings)
library(Peptides)  
library(shiny)
library(shinydashboard)
library(waiter)


header <- dashboardHeader(
  title = "Protein properties"
)

sidebar <- dashboardSidebar(
  fileInput("file1", "Choose FASTA File",
            multiple = TRUE,
            accept = c("text/fa",
                       "text/comma-separated-values,text/plain",
                       ".fa")),
  useWaiter(), # include dependencies
  actionButton("show", "Update"),
  # Horizontal line ----
  tags$hr(),
  selectInput("dataset", "Choose a dataset:",
              choices = c("properties")),
  downloadButton("downloadData", "Download")
)




body <- dashboardBody(
  title = "Examples of DataTables",
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "properties"',
        checkboxGroupInput("show_vars", "Columns in properties to show:",
                           c("names","sequence","length","molecular_weight",
                             "instability","hydrophobicity","aliphatic","pI","charge",
                             "sequence_N20","pI_N20","charge_N20",
                             "sequence_C20","pI_C20","charge_C20",
                             "sequence_N30","pI_N30","charge_N30",
                             "sequence_C30","pI_C30","charge_C30",
                             "sequence_N50","pI_N50","charge_N50",
                             "sequence_C50","pI_C50","charge_C50"), 
                           selected = c("names","sequence","length","molecular_weight",
                                        "instability","hydrophobicity","aliphatic","pI","charge",
                                        "sequence_N20","pI_N20","charge_N20",
                                        "sequence_C20","pI_C20","charge_C20",
                                        "sequence_N30","pI_N30","charge_N30",
                                        "sequence_C30","pI_C30","charge_C30",
                                        "sequence_N50","pI_N50","charge_N50",
                                        "sequence_C50","pI_C50","charge_C50")
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("properties", DT::dataTableOutput("mytable1"))
      )
    )
  )
)


ui <- dashboardPage(header, sidebar,body)

take_fa <- function(dat){
  inFile <- dat
  fa <- readAAStringSet(inFile$datapath)
  
  table = data.frame(fa) %>%
    rownames_to_column("name") %>%
    mutate("length" = Peptides::lengthpep(seq = fa)) %>% 
    mutate("molecular_weight" = mw(seq = fa)) %>%
    mutate("instability" = instaIndex(seq = fa)) %>%
    mutate("hydrophobicity" = hydrophobicity(seq = fa)) %>%
    mutate("aliphatic" = aIndex(seq = fa)) %>%     
    mutate("pI" = pI(seq = fa)) %>% 
    mutate("charge" = charge(seq = fa)) %>%
    as_tibble()
  
  table = as.data.frame(table)
  colnames(table) = c("names","sequence","length","molecular_weight",
                      "instability","hydrophobicity","aliphatic",
                      "pI","charge")
  
  x = data.frame(fa)
  for (i in 1:nrow(x)) {
    x[i,2] = substr(x[i,1],1,20)
    x[i,3] = substr(x[i,1],nchar(x[i,1]) - 20,nchar(x[i,1]))
    x[i,4] = substr(x[i,1],1,30)
    x[i,5] = substr(x[i,1],nchar(x[i,1]) - 30,nchar(x[i,1]))
    x[i,6] = substr(x[i,1],1,50)
    x[i,7] = substr(x[i,1],nchar(x[i,1]) - 50,nchar(x[i,1]))
  }
  x = x[,-1]
  colnames(x) = c("N_20","C_20","N_30","C_30","N_50","C_50")
  
  y = x %>%
    rownames_to_column("name") %>%
    mutate("pI_N20" = pI(seq = x[,1])) %>% 
    mutate("pI_C20" = pI(seq = x[,2])) %>% 
    mutate("pI_N30" = pI(seq = x[,3])) %>% 
    mutate("pI_C30" = pI(seq = x[,4])) %>% 
    mutate("pI_N50" = pI(seq = x[,5])) %>% 
    mutate("pI_C50" = pI(seq = x[,6])) %>% 
    mutate("charge_N20" = charge(seq = x[,1])) %>% 
    mutate("charge_C20" = charge(seq = x[,2])) %>%
    mutate("charge_N30" = charge(seq = x[,3])) %>% 
    mutate("charge_C30" = charge(seq = x[,4])) %>%
    mutate("charge_N50" = charge(seq = x[,5])) %>% 
    mutate("charge_C50" = charge(seq = x[,6])) %>%
    as_tibble()
  
  y = as.data.frame(y)
  
  colnames(y) = c("names",
                  "sequence_N20","sequence_C20","sequence_N30","sequence_C30",
                  "sequence_N50","sequence_C50",
                  "pI_N20","pI_C20","pI_N30","pI_C30","pI_N50","pI_C50",
                  "charge_N20","charge_C20","charge_N30","charge_C30","charge_N50","charge_C50")
  
  dataTable = data.frame(cbind(table,
                               y$sequence_N20,y$pI_N20,y$charge_N20,
                               y$sequence_C20,y$pI_C20,y$charge_C20,
                               y$sequence_N30,y$pI_N30,y$charge_N30,
                               y$sequence_C30,y$pI_C30,y$charge_C30,
                               y$sequence_N50,y$pI_N50,y$charge_N50,
                               y$sequence_C50,y$pI_C50,y$charge_C50
  ))
  
  
  colnames(dataTable) = c("names","sequence","length","molecular_weight",
                          "instability","hydrophobicity","aliphatic","pI","charge",
                          "sequence_N20","pI_N20","charge_N20",
                          "sequence_C20","pI_C20","charge_C20",
                          "sequence_N30","pI_N30","charge_N30",
                          "sequence_C30","pI_C30","charge_C30",
                          "sequence_N50","pI_N50","charge_N50",
                          "sequence_C50","pI_C50","charge_C50")
  return(dataTable)
}


server <- function(input, output) {
  
  observeEvent(input$show, {
    
    waiter_show( # show the waiter
      html = spin_fading_circles() # use a spinner
    )
    
    dataTable = take_fa(input$file1)
    
    
    output$mytable1 <- DT::renderDataTable({
      DT::datatable(dataTable[, input$show_vars, drop = FALSE],
                    extensions = 'Buttons',
                    options = list(                                                     
                      fixedColumns = TRUE,
                      autoWidth = TRUE,
                      ordering = TRUE,
                      dom = 'Bliftsp',
                      buttons = c('copy', 'csv', 'excel')
                    ),)
    })
    
    #output$dataset <- dataTable
    
    waiter_hide() # hide the waiter
    
    # put downloadevent into loading event
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$dataset, ".csv", sep = "")
      },
      content = function(file) {
        
        write.csv(dataTable, file, row.names = FALSE)
      }
    )
  })
}

shinyApp(ui,server)
