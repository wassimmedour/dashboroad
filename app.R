library(shiny)
library(DT)

# Define UI
ui <- shinyUI(fluidPage(
    
    dateRangeInput("dates", label = h3("Date range")),
   
    hr(),
    fluidRow(column(4, verbatimTextOutput("value"))),
   htmlOutput("sample_table")
)
)

# Define server logic
server <- shinyServer(function(input, output) {
    output$value <- renderPrint({ input$dates })
    df_products_upload <- reactive({
       
        df <- read.csv("Baseventes.csv", header = TRUE,sep =";")
        df<-mutate(df,DATE_BL=lubridate::dmy(DATE_BL))
        df<-filter(df,DATE_BL >= input$dates[1] & DATE_BL <= input$dates[2])
        links<-data.frame(source=df$PRODUIT,target=df$REGION,value=df$QTE_TM)
        nodes<-data.table::data.table(name=c(as.character(links$source),as.character(links$target))%>%unique())
        nodes
        links$IDsource <- match(links$source, nodes$name)-1 
        links$IDtarget <- match(links$target, nodes$name)-1
        fig <- plotly::plot_ly(
            type = "sankey",
            orientation = "h",
            
            node = list(
                label = nodes%>%as.list()%>%unlist(use.names = FALSE),
                color = c("orange", "#3A9BDC", "blue", "blue", "blue", "blue", "blue", "blue"),
                pad = 15,
                thickness = 20,
                line = list(
                    color = "black",
                    width = 0.5
                )
            ),
            
            link = list(
                source = links$IDsource,
                target = links$IDtarget,
                value =  links$value,
                color= "#D3D3D3"
            )
        )
        fig<- fig %>% plotly::layout(
            title = "Basic Sankey Diagram",
            font = list(
                size = 10
            )
        )
        
        
        return(fig)
        
        
    })
    
        
        
  
    output$sample_table<- renderUI({
        df <- df_products_upload()
        return( df)
    })
    
}
)

# Run the application 
shinyApp(ui = ui, server = server)