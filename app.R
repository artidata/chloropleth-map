library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(sf)
library(RColorBrewer)

ui <- dashboardPage(
  dashboardHeader(
    title = "Reproducible Plots",
    titleWidth = 240),
  
  dashboardSidebar(
    width = 240,
    
    selectInput(
      inputId = "typePlot",
      label = "Type of Plot:",
      choices = list("Choropleth Map")),
    
    conditionalPanel(
      condition = "input.typePlot == 'Choropleth Map'",
      
      selectInput(
        inputId = "region",
        label = "Region:",
        choices = list.files("raw_data/Choropleth Map")),
      
      selectInput(
        inputId = "division",
        label = "Division:",
        choices = list.files(paste0("raw_data/Choropleth Map/singapore")))), #hard coding
    
    selectInput(
      inputId = "themePlot",
      label = "Theme:",
      choices = list("Standard", "Choropleth - Minimal"),
      selected = "Choropleth - Minimal"),
    
    selectInput(
      inputId = "methodPlot",
      label = "Method:",
      choices = list("Standard", "Facet", "Interactive", "Animation"),
      selected = "Standard"),
    
    conditionalPanel(
      condition = "input.methodPlot == 'Ordinary'",
      
      checkboxInput(
        inputId = "islabeled",
        label = "Label")),
    
    conditionalPanel(
      condition = "input.methodPlot == 'Facet'",
      
      numericInput(
        inputId = "numVariable",
        label = "Additional Category Level:",
        value = 1,
        min = 1,
        max = 2)),
    
    numericInput(
      inputId = "heightPlot",
      label = "Height (px):",
      value = 600,
      width = "50%"),
    
    numericInput(
      inputId = "widthPlot",
      label = "Width (px):",
      value = 800,
      width = "50%"),
    
    numericInput(
      inputId = "sizeFontPlot",
      label = "Font Size (px):",
      value = 12,
      width = "50%"),
    
    fileInput("csv", "Upload CSV File:"),
    
    radioButtons(
      inputId = "datasetPlot",
      label = "Dataset:",
      choices = list("Sample", "Upload"),
      selected = "Sample"),
    
    actionButton(
      inputId = "executePlot",
      label = "RE-PLOT")),

  
  dashboardBody(
    
    fluidRow(
      
      uiOutput(outputId  = "boxPlot")),
                
    fluidRow(
      
      box(
        title = "Sample Dataset",
        width = 6,
        collapsible = T,
        status = "primary",
        solidHeader = T,
        
        DTOutput(outputId = "datasetSample"),
        
        downloadButton(outputId = "downloadDatasetSample",
                       label = "Download CSV")),
      
      box(
        title = "Uploaded Dataset",
        id = "box",
        width = 6,
        collapsible = T,
        status = "primary",
        solidHeader = T,
        
        DTOutput(outputId = "datasetUpload")))))
                  

server <- function(input, output, session) {
  
  #observe
  observeEvent(input$region, {
    #choropleth map related
    updateSelectInput(session,
                      inputId = "division",
                      choices = list.files(paste0(
                        "raw_data/Choropleth Map/", input$region)))})
  
  #reactives
  typePlotCurrent <- reactive({
    input$typePlot
  })
  
  datasetSampleCurrent <-reactive({
    if (typePlotCurrent() == "Choropleth Map") {
      foo <- st_read(
        paste0(
          "raw_data/Choropleth Map/",
          input$region,
          "/",
          input$division,
          "/data.shp"))}
  
      return(data.frame(division = foo$division,
                        value = rnorm(nrow(foo))))})
      
  
  datasetUploadCurrent <- reactive({
    if (is.null(input$csv$datapath)) {
      return(NULL)}
    else{read.csv(input$csv$datapath,header = T)}})
  
  plotCurrent <- reactive({
    input$executePlot
    isolate({
      
      foo <- NULL
      
      if (typePlotCurrent() == "Choropleth Map") {
        foo <- st_read(
          paste0(
            "raw_data/Choropleth Map/",
            input$region,
            "/",
            input$division,
            "/data.shp"))}
      
      ifelse(input$datasetPlot=="Sample",
             foo <- merge(foo,datasetSampleCurrent(),
                          by=colnames(foo)[1]),
             foo <- merge(foo,datasetUploadCurrent(),
                          by=colnames(foo)[1]))
      
      replot <- ggplot(data = foo)
      
      nameCol <- colnames(foo)[2]
      
      print(nameCol)
      valueCol <- foo[[nameCol]]
      extremeCol <- max(abs(min(valueCol)),abs(max(valueCol)))
      
      if(typePlotCurrent() == "Choropleth Map"){
        replot <- replot + geom_sf(aes_string(fill = nameCol))
        if(is.numeric(valueCol)){
        if (max(valueCol) > 0 & min(valueCol) < 0) {
          replot <- replot +
            scale_fill_gradientn(
              colors = c(rev(brewer.pal(4, "Reds")),
                         brewer.pal(4, "Greens")),
              limits = c(-extremeCol, extremeCol)
            )
        } else if (min(valueCol) >= 0) {
          replot <- replot +
            scale_fill_gradientn(colors = c(brewer.pal(9, "Greens")),
                                 limits = c(0, extremeCol))
        } else if (max(valueCol) <= 0) {
          replot <- replot +
            scale_fill_gradientn(colors = rev(c(brewer.pal(9, "Reds"))),
                                 limits = c(-extremeCol, 0))
        }
          replot <- replot+guides(fill = guide_colorbar(barheight = input$heightPlot/50,
                                                        ticks.colour = "black",
                                                        frame.colour= "black"))
        }
        
        }
      
      
      if(input$themePlot=="Standard"){
        replot <- replot + theme_light()+theme(
          panel.border = element_rect(colour = "black"),
          legend.position = "right",
          strip.background = element_rect(fill = "white",color = "black"),
          strip.text = element_text(color = "black"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )
      }else if(input$themePlot=="Choropleth - Minimal"){
        replot <- replot +
          coord_sf(datum=NA)+ #bug
          theme_minimal()+
          theme(plot.background = element_rect(fill="#deebf7",
                                               color = "#deebf7"))
      }
      
      replot <- replot+theme(
        text = element_text(size = input$sizeFontPlot)
      )
      
      return(replot)})})
  
  output$datasetSample <- renderDT(server = F, {
    datasetSampleCurrent()})
  
  output$datasetUpload <- renderDT({

    datatable(datasetUploadCurrent())})
  
  ## Download original
  output$downloadDatasetSample <- downloadHandler(
    
    filename = function() {
      paste0(typePlotCurrent(), ".csv")},
    
    content = function(file) {
      write.csv(datasetSampleCurrent(), file,row.names = F)}) #annoying write.csv
  
  output$boxPlot <- renderUI({
    
    input$executePlot
    
    isolate({
    
      output$plot <- renderPlot({
        plotCurrent()})
      
      tagList(box(
        title = "Plot",
        solidHeader = T,
        collapsible = T,
        width = 12,
        status = "primary",
        column(
          align = "center",
          width = 12,
          
          plotOutput(
            outputId  = "plot",
            width = input$widthPlot,
            height = input$heightPlot))))})})}

shinyApp(ui = ui, server = server)