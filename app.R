library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(sf)
library(RColorBrewer)

ui <- dashboardPage(
  dashboardHeader(
    title = "Choropleth Map",
    titleWidth = 240),
  
  dashboardSidebar(
    width = 240,
    
    selectInput(
      inputId = "region",
      label = "Region:",
      choices = list.files("raw_data/"),
      selected = "world"),
    
    selectInput(
      inputId = "division",
      label = "Division:",
      choices = list.files(paste0("raw_data/world"))), #hard coding

    selectInput(
      inputId = "themePlot",
      label = "Theme:",
      choices = list("Grid", "Minimal"),
      selected = "Grid"),
    
    selectInput(
      inputId = "methodPlot",
      label = "Method:",
      choices = list("Standard", "Facet", "Interactive", "Animation"),
      selected = "Standard"),
    
    conditionalPanel(
      condition = "input.methodPlot == 'Standard'",
      
      checkboxInput(
        inputId = "islabeled",
        label = "Label")),
    
    conditionalPanel(
      condition = "input.methodPlot == 'Facet'",
      
      numericInput(
        inputId = "numCategory",
        label = "Additional Category Level:",
        value = 1,
        min = 1,
        max = 2)),
    
    numericInput(
      inputId = "heightPlot",
      label = "Height (px):",
      value = 400,
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
  observeEvent(input$region,{
    updateSelectInput(session,
                      inputId = "division",
                      choices = list.files(paste0(
                        "raw_data/", input$region)))})

  datasetSampleCurrent <- reactive({
    df <- read.csv(paste0(
      "raw_data/",input$region,"/",input$division,"/division.csv"),
      header = T)
    
    if(input$methodPlot == "Facet"){
      ifelse(input$numCategory==1,
             df <- expand.grid(df[,1],category =  LETTERS[1:sample(2:26,1)]),
             df <- expand.grid(df[,1],category1 = LETTERS[1:sample(2:6,1)],
                               category2 = letters[1:sample(2:6,1)]))}
    
    return(cbind(df,value = rnorm(nrow(df))))})
  
  datasetUploadCurrent <- reactive({
    if(is.null(input$csv$datapath)) {
      return(NULL)}
    else{read.csv(input$csv$datapath,header = T)}})
  
  plotCurrent <- reactive({
    input$executePlot
    isolate({
      sf <- st_read(paste0(
        "raw_data/",input$region,"/",input$division,
            "/data.shp"))
      
      ifelse(input$datasetPlot=="Sample",
             sf <- merge(sf,datasetSampleCurrent(),
                         by.x=colnames(sf)[1],
                         by.y=colnames(datasetSampleCurrent())[1]),
             sf <- merge(sf,datasetUploadCurrent(),
                         by.x=colnames(sf)[1],
                         by.y=colnames(datasetOutputCurrent())[1]))
      
      replot <- ggplot(data = sf)
      
      nameCol <- colnames(sf)[ncol(sf)-1] #get values
      valueCol <- sf[[nameCol]]
      extremeValue <- max(abs(min(valueCol)),abs(max(valueCol)))
      
      replot <- replot + geom_sf(aes_string(fill = nameCol))
      
      if(is.numeric(valueCol)){
        if(max(valueCol) > 0 & min(valueCol) < 0) {
          replot <- replot +
            scale_fill_gradientn(
              colors = c(rev(brewer.pal(4, "Reds")),
                         brewer.pal(4, "Greens")),
              limits = c(-extremeValue, extremeValue))} 
        
        else if(min(valueCol) >= 0){
          replot <- replot +
            scale_fill_gradientn(colors = c(brewer.pal(9, "Greens")),
                                 limits = c(0, extremeValue))} 
        else if (max(valueCol) <= 0){
          replot <- replot +
            scale_fill_gradientn(colors = rev(c(brewer.pal(9, "Reds"))),
                                 limits = c(-extremeValue, 0))}
          replot <- replot+guides(fill = guide_colorbar(barheight = input$heightPlot/50,
                                                        ticks.colour = "black",
                                                        frame.colour= "black"))}
      
      if(input$methodPlot=="Facet"){
        ifelse(input$numCategory == 1,
               replot <- replot + facet_wrap(as.formula(paste0("~",colnames(sf)[2]))),
               replot <- replot + facet_grid(as.formula(paste0(colnames(sf)[2],"~",colnames(sf)[3]))))}
      
      if(input$themePlot=="Grid"){
        replot <- replot + theme_light()+theme(
          panel.border = element_rect(colour = "black"),
          legend.position = "right",
          strip.background = element_rect(fill = "white",color = "black"),
          strip.text = element_text(color = "black"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())}
      
      else if(input$themePlot=="Minimal"){
        replot <- replot +
          coord_sf(datum=NA)+ #bug
          theme_minimal()+
          theme(plot.background = element_rect(fill="#deebf7",
                                               color = "#deebf7"))}
      
      replot <- replot+theme(
        text = element_text(size = input$sizeFontPlot))
      
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
          height = input$heightPlot))))})}
      

shinyApp(ui = ui, server = server)