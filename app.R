library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(sf)


ui <- dashboardPage(
    
    #header, 
    dashboardHeader(
        title = "Reproducible Plots"
    ),
    
    #sidebar,
    dashboardSidebar(
        
        selectInput(
            inputId = "plotType", 
            label = "Plot Type:", 
            choices = list("Choropleth Map"), ),
        
        
        uiOutput(
            outputId = "plotTypeUI"), #additional UI based on plotType 
        
        radioButtons(
            inputId = "plotMethod",
            label = "Plotting Method:",
            choices = list("Ordinary","Facet","Interactive"),
            selected = "Ordinary"),
        
        
        conditionalPanel( # Ordinary 
            
            condition = "input.plotMethod == 'Ordinary'",
            
            checkboxInput(
                inputId = "islabeled",
                label = "Label")),
        
        conditionalPanel(
            
            condition = "input.plotMethod == 'Facet'",
            
            numericInput(
                inputId = "numVariable",
                label = "Additional Variables:",
                value = 1,min=1,max=2)),
        
        fileInput("csv", "Upload CSV File:"),
        
        actionButton("generatePlot", "Generate Plot")

      
    ),
    
    #body
    dashboardBody(

        fixedRow(
            
            box(title ="Sample Plot",
                collapsible = T, 
                width=12, 
                status="primary", 
                solidHeader =T,
                
                plotOutput("plotSample",width = 800,height = 800))),
        
        fluidRow(
            
            box(title = "Sample Dataset", 
                width = 6, 
                collapsible = T,
                status = "primary", 
                solidHeader = T,
                
                DTOutput(
                    outputId = "datasetSample"),
                
                downloadButton(
                    outputId = "downloadDatasetSample",
                    label = "Download CSV")),
            
            box(title = "Uploaded Dataset", 
                id="box",
                width = 6, 
                collapsible = T,
                status = "warning", 
                solidHeader = T,
                
                DTOutput(
                    outputId = "datasetUploaded")
            )),
        
        fluidRow(
            
            box(title = "Uploaded Plot",
                width=12, 
                status = "warning",
                solidHeader = T,
                
                textOutput("test"))
        )
    )
)


server <- function(input, output) {
    
    plotTypeCurrent <- reactive({ #tracking on the tab used
        input$plotType})

    output$plotTypeUI <- renderUI({
        if(plotTypeCurrent()=="Choropleth Map"){
            tagList(
                selectInput(
                    inputId = "region",
                    label = "Region:",
                    choices = list.files("raw_data/Choropleth Map")),
                
                selectInput(
                    inputId = "division",
                    label = "Division:",
                    choices = list.files(paste0("raw_data/Choropleth Map/",input$region))))}})
    
    datasetSampleCurrent <- reactive({ #tracking on the sample dataset
        
        if(plotTypeCurrent()=="Choropleth Map"){
            foo <- st_read(paste0("raw_data/Choropleth Map/",
                           input$region,"/",
                           input$division,
                           "/data.shp"))
            return(data.frame(division = foo$division,
                              value= rnorm(nrow(foo))))
        }})

    
    plotSampleCurrent <- reactive({ #tracking on the original plot
        
        if(plotTypeCurrent()=="Choropleth Map"){
            
            foo <- st_read(paste0("raw_data/Choropleth Map/",
                                  input$region,"/",
                                  input$division,
                                  "/data.shp"))
            
            foo <- merge(foo, datasetSampleCurrent(),by="division")
            return(ggplot(foo)+
                       geom_sf(aes(fill=value)))
        }
        
        })
    
    datasetUploadedCurrent <- reactive({}) 
    
    
    ## Download original
    output$downloadDatasetOriginal <- downloadHandler(
        filename = function(){
            str_c(plotTypeCurrent(),".csv")
        }, content = function(file){
            write.csv(datasetOriginalCurrent(),file)
        }
    )
    
    output$plotSample <- renderPlot({
        plotSampleCurrent()})
    
    output$datasetSample <- renderDT(server = F,{
        datasetSampleCurrent()
    })
    
    output$datasetUploaded <- renderDT({
        if(is.null(input$csv)){
            return(NULL)}
        datatable(fread(input$csv$datapath), 
                  editable = TRUE
        )})
    
    output$test <- renderText({
        str(plotSampleCurrent())
    })
    
}

shinyApp(ui = ui, server = server)