library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(sf)
library(RColorBrewer)
library(shinycssloaders)

ui <- dashboardPage(
  dashboardHeader(
    title = "Choropleth Map",
    titleWidth = 240,
    dropdownMenu(type = "messages",
                 messageItem(
                   from = "artidata",
                   message = "Click here to learn more about the app!",
                   href = "http://blog.artidata.io/posts/2018-10-29-choropleth-map-app/"))),
  
  dashboardSidebar(
    width = 240,
    
    fluidRow(
      column(
        width = 11, #seems more central, although 12 is the default
        align = "center",
        actionButton(
          inputId = "execute",
          color = "green",
          label = "RE-PLOT",
          style = "color: #fff; background-color: #00a65a; border-color: #00a65a;"),
        
        tags$head(tags$style(HTML("#execute:hover{background-color:#008d4c !important;}")))
      )),
    
    selectInput(
      inputId = "region",
      label = "Country/Region",
      choices = list.files("database/"),
      selected = "World"),
    
    selectInput(
      inputId = "division",
      label = "Division",
      choices = list.files("database/World")), #hard coding

    selectInput(
      inputId = "theme",
      label = "Theme",
      choices = list("Minimal"),
      selected = "Minimal"),
    
    
    selectInput(
      inputId = "colorPositive",
      label = "( + ) Color",
      choices = list("Blues","BuGn","BuPu","GnBu",
                     "Greens","Greys","Oranges","OrRd",
                     "PuBu","PuBuGn","PuRd","Purples",
                     "RdPu","Reds","YlGn","YlGnBu","YlOrBr","YlOrRd"),
      selected = "Greens",
      width = "50%"),
    
    selectInput(
      inputId = "colorNegative",
      label = "( - ) Color",
      choices = list("Blues","BuGn","BuPu","GnBu",
                     "Greens","Greys","Oranges","OrRd",
                     "PuBu","PuBuGn","PuRd","Purples",
                     "RdPu","Reds","YlGn","YlGnBu","YlOrBr","YlOrRd"),
      selected = "Reds",
      width = "50%"),
  
    selectInput(
      inputId = "method",
      label = "Method",
      choices = list("Standard", 
                     "Facet"),
      selected = "Standard"),
    
    conditionalPanel(
      condition = "input.method == 'Facet'" ,
      
      radioButtons(
        inputId = "numCategory",
        label = "Additional Category Level",
        choices = list(1,2),
        selected = 1,
        inline = T)),
    
    numericInput(
      inputId = "sizeFont",
      label = "Font Size (px)",
      value = 12,
      width = "50%"),
    
    fileInput("csv", "Upload CSV File"),
    
    radioButtons(
      inputId = "dataset",
      label = "Dataset",
      choices = list("Sample", "Upload"),
      inline = T,
      selected = "Sample"),
    
    
    checkboxInput(
      inputId = "manualSize",
      label = "Manual Size",
      value =  F),
    
    conditionalPanel(
      condition = "input.manualSize == true",
      
      numericInput(
        inputId = "height",
        label = "Height (px)",
        value = 400,
        width = "50%"),
      
      numericInput(
        inputId = "width",
        label = "Width (px)",
        value = 800,
        width = "50%")
    )),


  dashboardBody(
    # some javascript
    tags$head(tags$script('
                                var widthBox = 0;
                                $(document).on("shiny:connected", function(e) {
                                    widthBox = document.getElementById("box").offsetWidth-52;
                                    Shiny.onInputChange("widthBox", widthBox);
                                });
                                $(window).resize(function(e) {
                                    newWidthBox =  document.getElementById("box").offsetWidth-52;
                                    if(widthBox != newWidthBox){
                                      Shiny.onInputChange("widthBox", newWidthBox);
                                    }
                                });
                            ')),
    
    fluidRow(
      
      uiOutput(outputId  = "box")),
                
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
  
  observeEvent(input$region,{
    updateSelectInput(session,
                      inputId = "division",
                      choices = list.files(paste0(
                        "database/", input$region)))})
  
  dirCurrent <- reactive({
    paste0("database/",input$region,"/",input$division)
  }) %>% debounce(200) #debouce
  
  ratioCurrent <- reactive({
    input$execute
    isolate({
      as.numeric(readLines(paste0(dirCurrent(),"/ratio.txt")))
    })
  })
 
  datasetSampleCurrent <- reactive({
  
    df <- read.csv(paste0(dirCurrent(),"/division.csv"),header = T) 
    
    if(input$method == "Facet"){
      ifelse(input$numCategory==1,
             df <- expand.grid(category =  LETTERS[1:sample(2:26,1)],
                               division = df[,1]),
             df <- expand.grid("category_1" = LETTERS[1:sample(2:6,1)],
                               "category_2" = letters[1:sample(2:6,1)],
                               division = df[,1]))}
    
    return(cbind(df,value = rnorm(nrow(df))))})
  
  datasetUploadCurrent <- reactive({
    if(is.null(input$csv$datapath)) {
      return(NULL)}
    else{read.csv(input$csv$datapath,header = T)}})
  
  plotCurrent <- reactive({
    
    input$execute
    isolate({
      sf <- st_read(paste0(dirCurrent(),"/feature.gpkg"),quiet=T)
      
      ifelse(input$dataset=="Sample",
             sf <- merge(sf,datasetSampleCurrent(),
                         by = "division"),
             sf <- merge(sf,datasetUploadCurrent(),
                         by = "division"))
      
      replot <- ggplot(data = sf)
      
      nameCol <- colnames(sf)[ncol(sf)-1] #get values
      valueCol <- sf[[nameCol]]
      extremeValue <- max(abs(min(valueCol)),abs(max(valueCol)))
      
      replot <- replot + geom_sf(aes_string(fill = nameCol))
      
      if(is.numeric(valueCol)){
        
        if(input$theme == "Minimal"){
          
          pal1 <- c(rev(brewer.pal(4, input$colorNegative)),brewer.pal(4, input$colorPositive)) #pal diverge
          pal2 <- c(brewer.pal(9, input$colorPositive)) #pal positive 
          pal3 <- c(brewer.pal(9, input$colorNegative)) #pal negative
          
          pal <- NULL 
          lim <- NULL
          
          if(max(valueCol) > 0 & min(valueCol) < 0){
            pal <- pal1
            lim <- c(-extremeValue, extremeValue)
          }else if(max(valueCol)<=0){
            pal <- rev(pal3)
            lim <- c(-extremeValue, 0)
          }else if(min(valueCol)>=0){
            pal <- pal2
            lim <- c(0,extremeValue)}
          
          replot <- replot + 
            scale_fill_gradientn(name=NULL,
                                 colors = pal, 
                                 limits = lim, 
                                 na.value = "grey50")
          
          replot <- replot +
            coord_sf(datum=NA)+ #bug
            theme_void()}}
      
        
        replot <- replot+guides(fill = guide_colorbar(barheight = sizePlot()$height/50,
                                                      ticks.colour = "black",
                                                      frame.colour= "black"))
      

      if(input$method=="Facet"){
        ifelse(input$numCategory == 1,
               replot <- replot + facet_wrap(as.formula("~ category")),
               replot <- replot + facet_grid(as.formula("category_1 ~ category_2")))}
      replot <- replot+theme(
        text = element_text(size = input$sizeFont))
      
      return(replot)})})
  
  output$datasetSample <- renderDT(server = F, {
    
    datasetSampleCurrent()})
  
  output$datasetUpload <- renderDT({
    
    datatable(datasetUploadCurrent())})
  
  output$downloadDatasetSample <- downloadHandler(
    
    filename = function() {
      paste0(input$region,".csv")},
    
    content = function(file) {
      write.csv(datasetSampleCurrent(),file,row.names = F)}) #annoying write.csv
  
  
  sizePlot <- reactive({
    if(input$manualSize){
      list(width = input$width, height= input$height)  
    } else{
      list(width = input$widthBox, height = input$widthBox/ratioCurrent())
    }
  })
  sizePlot <- sizePlot %>% debounce(500)
  
  output$box <- renderUI({
    validate(need(sizePlot,message = "LOADING"))
    output$plot <- renderPlot({
      plotCurrent()})

    tagList(box(
      title = "Plot",
      solidHeader = T,
      collapsible = T,
      width = 12,
      status = "primary",
      withSpinner(
        plotOutput(
          outputId  = "plot",
          width = sizePlot()$width,
          height = sizePlot()$height),
        color = getOption("spinner.color", default = "#3c8dbc"))))})}
      
shinyApp(ui = ui, server = server)