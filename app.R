# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# VolcaNoseR - A Shiny app for for nosing around in volcano plots
# Created by Joachim Goedhart (@joachimgoedhart), first version 2019
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Joachim Goedhart (C) 2019
# electronic mail address: j #dot# goedhart #at# uva #dot# nl
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Allow files up to 10 Mb
options(shiny.maxRequestSize=10*1024^2)

#Load necessary packages

library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)
library(ggrepel)
library(shinycssloaders)
library(readxl)
library(DT)
library(RCurl)

source("themes.R")

#df_example <- read.csv("Data-Vulcano-plot.csv", na.strings = "")
df_example_cdc42 <- read.csv("elife-45916-Cdc42QL_data.csv", na.strings = "")
df_example_diffgenes_HFHC <- read.csv("Becares-diffgenes_HFHC.csv", na.strings = "")

# Create a reactive object here that we can share between all the sessions.
vals <- reactiveValues(count=0)



# Define UI
ui <- fluidPage(
   
   # Application title
   titlePanel("VolcaNoseR - Exploring volcano plots"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(width=3,
          conditionalPanel(
            condition = "input.tabs=='Plot'",

            
            # h3("Select Geom(etry)"),
            #   selectInput("geom", label = NULL, choices = list("geom_point"="geom_point", "-"="-")),
            # conditionalPanel(condition = "input.geom=='geom_line'",  
            # selectInput('grouping', label="Group", choices = list("-"="-"), selected = "-")
            # ),
            h4("Aesthetics"),

            sliderInput("pointSize", "Size of the datapoints", 0, 10, 4),  
            
            sliderInput("alphaInput", "Visibility of the data", 0, 1, 0.8),  

            # hr(),
            h4("Selection & Annotation of hits"),
            sliderInput("fc_cutoff", "Fold Change threshold:", -5, 5, step=0.1, value = c(-1.5,1.5)),
            sliderInput("p_cutoff", "Significance threshold:", 0, 5, step=0.1, value = 2),
            selectInput("direction", label="Use thresholds to annotate:", choices = list("All (ignores thresholds)"="all", "Changed (and significant)"="significant","Increased (and significant)"="increased", "Decreased (and significant)"="decreased"), selected ="significant"),
            
            selectInput("criterion", label="Criterion for ranking hits:", choices = list("Manhattan distance"="manh", "Euclidean distance"="euclid","Fold change"="fc","Significance"="sig"), selected ="manh"),
            

            numericInput("top_x", "Number of top hits (0 to hide):", value = 10),
            
            # textInput("user_gene_list2", "Selected hits (names separated by commas, e.g. DOK6,TBX5)", value = ""), 
            
            selectizeInput(inputId = 'user_gene_list',
                           label = "User selected hits:",
                           choices = "-",
                          selected = "-",
                           multiple = TRUE, # allow for multiple inputs
                            options = list(create = TRUE)), # if TRUE, allows newly created inputs
           
            checkboxInput(inputId = "show_table",
                          label = "Show table with hits",
                          value = FALSE),
            checkboxInput(inputId = "hide_labels",
                          label = "Hide labels in the plot",
                          value = FALSE),

            radioButtons("adjustcolors", "Color (Unchanged,Increased,Decreased)", choices = 
                           list(
                             "Grey, Red, Blue" = 1,
                             "Grey, Blue, Green" = 3,
                             "Grey, Cyan, Purple" = 4,
                             "User defined"=5),
                         selected =  1),
            conditionalPanel(condition = "input.adjustcolors == 5",
                             textInput("user_color_list", "List of names or hexadecimal codes", value = "turquoise2,#FF2222,lawngreen")),
            checkboxInput(inputId = "dark", label = "Dark Theme", value = FALSE),
            
            # h5("",
            #    a("Click here for more info on color names",
            #      href = "http://www.endmemo.com/program/R/color.php", target="_blank"))

            h4("Transformation & Scaling"),
            
            

            
            # conditionalPanel(condition = "input.transform==true",
            #                  
            #                  radioButtons(
            #                    "transform_x", "Transform x-axis data:",
            #                    choices =
            #                      list("No" = "No",
            #                           "log2" = "log2",
            #                           "log10" = "log10",
            #                           "-log10" = "minus_log10"),
            #                    selected = "No"),
            #                  radioButtons(
            #                    "transform_y", "Transform y-axis data:",
            #                    choices =
            #                      list("No" = "No",
            #                           "log2" = "log2",
            #                           "log10" = "log10",
            #                           "-log10" = "minus_log10"),
            #                    selected = "No")
            #                  
            #                  
            # ),
            checkboxInput(inputId = "rotate_plot",
                          label = "Rotate plot 90 degrees",
                          value = FALSE),
            checkboxInput(inputId = "add_grid",
                          label = "Add gridlines",
                          value = FALSE),
            checkboxInput(inputId = "change_scale",
                          label = "Change scale",
                          value = FALSE),
            conditionalPanel(
              condition = "input.change_scale == true",
              
              
              textInput("range_x", "Range x-axis (min,max)", value = "")
              
            ),
            

            conditionalPanel(
              condition = "input.change_scale == true",
              textInput("range_y", "Range y-axis (min,max)", value = ""),            
              checkboxInput(inputId = "scale_log_10", label = "Log10 scale on y-axis", value = FALSE)
              
            ),
            numericInput("plot_height", "Plot height (# pixels): ", value = 600),
            numericInput("plot_width", "Plot width (# pixels):", value = 800),


            h4("Labels"),
  
            checkboxInput(inputId = "add_title",
                          label = "Add title",
                          value = FALSE),
            conditionalPanel(
              condition = "input.add_title == true",
              textInput("title", "Title:", value = "")
            ),
            
            checkboxInput(inputId = "label_axes",
                          label = "Change axis labels",
                          value = FALSE),
            conditionalPanel(
              condition = "input.label_axes == true",
              textInput("lab_x", "X-axis:", value = ""),
              textInput("lab_y", "Y-axis:", value = "")
              
            ),
            
            checkboxInput(inputId = "adj_fnt_sz",
                          label = "Change font size",
                          value = FALSE),
            conditionalPanel(
              condition = "input.adj_fnt_sz == true",
              numericInput("fnt_sz_title", "Plot title:", value = 24),
              numericInput("fnt_sz_labs", "Axis titles:", value = 24),
              numericInput("fnt_sz_ax", "Axis labels:", value = 18),
              numericInput("fnt_sz_cand", "Labels of hits:", value = 6)
              
            ),

              checkboxInput(inputId = "add_legend",
                            label = "Add legend",
                            value = FALSE),
            
            # 
            # conditionalPanel(
            #   condition = "input.add_legend == true",
            #   textInput("legend_title", "Legend title:", value = "")
            # ),
            
            
              NULL),
          
          
          conditionalPanel(
            condition = "input.tabs=='iPlot'",h4("iPlot")
          ),
              conditionalPanel(
                  condition = "input.tabs=='Data'",
              h4("Data upload"),
              
              radioButtons(
                "data_input", "",
                choices = 
                  list("Example data 1" = 1,
                       "Example data 2" = 2,
                       "Upload file (CSV, text, excel)" = 3,
                       "URL (CSV files only)" = 5
                  )
                ,
                selected =  1),
              
              conditionalPanel(
                condition = "input.data_input=='1'",p('Data of differentially expressed genes in livers of mice on a high-fat-high-cholesterol diet - from figure 4C of Becares et al (2019): https://doi.org/10.1016/j.celrep.2018.12.094'),hr()),              
              conditionalPanel(
                condition = "input.data_input=='2'",p('Data of potential interactors of Cdc42 - from Figure 9A of Gillingham et al (2019): https://doi.org/10.7554/eLife.45916.001'),hr()),
              
              
              conditionalPanel(
                condition = "input.data_input=='3'",
        
                fileInput("upload", NULL, multiple = FALSE, accept = c(".xlsx", ".xls", ".txt", ".csv")),
                # selectInput("file_type", "Type of file:",
                #             list(".csv or .txt" = "text",
                #                  ".xls or .xlsx" = "excel"
                #             ),
                #             selected = "text"),

                #   radioButtons(
                #     "upload_delim", "Delimiter",
                #     choices =
                #       list("Comma" = ",",
                #            "Tab" = "\t",
                #            "Semicolon" = ";",
                #            "Space" = " ")),
                #     selected = ","),         
                
                selectInput("upload_delim", label = "Select Delimiter (for text file):", choices =list("Comma" = ",",
                                                                                 "Tab" = "\t",
                                                                                 "Semicolon" = ";",
                                                                                 "Space" = " ")),
                

                  selectInput("sheet", label = "Select sheet (for excel workbook):", choices = " ")


                
                
                # actionButton("submit_datafile_button", "Submit datafile")

                ),
              ### csv via URL as input      
              conditionalPanel(
                condition = "input.data_input=='5'",
                #         textInput("URL", "URL", value = "https://zenodo.org/record/2545922/files/FRET-efficiency_mTq2.csv"), 
                 textInput("URL", "URL", value = ""), 
                NULL
              ),
              h4('Select X & Y variables'),
              
              selectInput("x_var", label = "X-axis; Effect (fold change)", choices = "-"),
              selectInput("y_var", label = "Y-axis; Significance (p-value)", choices = "-"),
              selectInput("g_var", label = "Select column with names", choices = "-"),
              

              # h4("Transformation"),
              # checkboxInput(inputId = "transformation",
              #               label = "Data transformation",
              #               value = FALSE),
              # conditionalPanel(condition = "input.transformation==true",
              #                  selectInput("transform_var_x", label = "Transform:", choices = "-"),
              #                  
              #                  radioButtons(
              #                    "transform_x", NULL,
              #                    choices =
              #                      list("log2" = "log2",
              #                           "log10" = "log10",
              #                           "-log10" = "minus_log10"),
              #                    selected = "log2"),
              # 
              #                    
              # 
              #                  
              #                  selectInput("transform_var_y", label = "Transform:", choices = "-"),
              #                  
              #                  radioButtons(
              #                    "transform_y", NULL,
              #                    choices =
              #                      list("log2" = "log2",
              #                           "log10" = "log10",
              #                           "-log10" = "minus_log10"),
              #                    selected = "minus_log10"),
              #                  
              #                  NULL
              #                  
              # 
              # ),
              # conditionalPanel(
              #   condition = "input.transformation==true", (downloadButton("downloadTransformedData", "Download transformed data (csv)"))),
              # 
              # hr(),
              hr(),

              NULL
              ),
          conditionalPanel(
            condition = "input.tabs=='About'",
            
            #Session counter: https://gist.github.com/trestletech/9926129
            h4("About"),  "There are currently", 
            verbatimTextOutput("count"),
            "session(s) connected to this app.",
            hr(),
            h4("Find our other dataViz apps at:"),a("https://huygens.science.uva.nl/", href = "https://huygens.science.uva.nl/")
          )
                   
                   
      ),   #Close sidebarPanel

      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(id="tabs",
                    tabPanel("Data", h4("Data as provided"),dataTableOutput("uploaded")),
                    tabPanel("Plot",h3("Volcano Plot"
                                       ),
                             downloadButton("downloadPlotPDF", "Download pdf-file"),
                             downloadButton("downloadPlotSVG", "Download svg-file"),
                             downloadButton("downloadPlotPNG", "Download png-file"),
                             
                             actionButton("settings_copy", icon = icon("clone"),
                                          label = "Clone current setting"),

                             plotOutput("coolplot",
                                        height = 'auto',
                                        # click = "clicked",
                                        hover = hoverOpts("plot_hover", delay = 10, delayType = "debounce")),uiOutput("hover_info"),
                            
                            conditionalPanel(
                              condition = "input.show_table == true",
                              h3("Top hits (based on distance from origin)"), withSpinner(dataTableOutput('toptableDT')))
                            
                            
                            
                              ),
                    # tabPanel("iPlot", h4("iPlot"), plotlyOutput("out_plotly")),

                    tabPanel("About", includeHTML("about.html"))
                    )
        
      )   #Close mainPanel
      

   ) #Close sidebarLayout
) #Close fluidPage

server <- function(input, output, session) {
  
  # Session variable - initialize defaults
  genelist.selected <- ""
  x_var.selected <- "log2_FoldChange"
  y_var.selected <- "minus_log10_pvalue"
  g_var.selected <- "Gene"
  sheet.selected <- " "
  
  # transform_var_x.selected <- "-"
  # transform_var_y.selected <- "-"  

  isolate(vals$count <- vals$count + 1)
  ###### DATA INPUT ###################
  
df_upload <- reactive({
    
    if (input$data_input == 1) {
      x_var.selected <<- "log2_FoldChange"
      y_var.selected <<- "minus_log10_pvalue"
      g_var.selected <<- "Gene"
      genelist.selected <<- ""
      # updateCheckboxInput(session, "transformation", value = FALSE)
      # transform_var_x.selected <<- "-"
      # transform_var_y.selected <<- "-"

      data <- df_example_diffgenes_HFHC
    } else if (input$data_input == 2) {
      x_var.selected <<- "log2_FoldChange"
      y_var.selected <<- "minus_log10_pvalue"
      g_var.selected <<- "Gene"
      genelist.selected <<- "HSPA6"
      # updateCheckboxInput(session, "transformation", value = FALSE)
      # transform_var_x.selected <<- "-"
      # transform_var_y.selected <<- "-"

      data <- df_example_cdc42
    } else if (input$data_input == 3) {
      genelist.selected <<- ""
      # updateCheckboxInput(session, "transformation", value = FALSE)
      # transform_var_x.selected <<- "-"
      # transform_var_y.selected <<- "-"
      file_in <- input$upload
      # Avoid error message while file is not uploaded yet
      if (is.null(input$upload)) {
        return(data.frame(x = "Click 'Browse...' to select a datafile or drop file onto 'Browse' button"))
      # } else if (input$submit_datafile_button == 0) {
      #   return(data.frame(x = "Press 'submit datafile' button"))
      } else {
        
        #Isolate extenstion and convert to lowercase
        filename_split <- strsplit(file_in$datapath, '[.]')[[1]]
        fileext <- tolower(filename_split[length(filename_split)])
        
        # observe({print(fileext)})
        
        # isolate({
           # data <- read.csv(file=file_in$datapath, sep = input$upload_delim, na.strings=c("",".","NA", "NaN", "#N/A", "#VALUE!"))
          
          if (fileext == "txt" || fileext=="csv") {
            
            data <- read.csv(file=file_in$datapath, sep = input$upload_delim, na.strings=c("",".","NA", "NaN", "#N/A", "#VALUE!"))
            updateSelectInput(session, "sheet", choices = " ", selected = " ")
          } else if (fileext=="xls" || fileext=="xlsx") {
            names <- excel_sheets(path = input$upload$datapath)
            # updateSelectInput(session, "sheet_names", choices = names)
            sheet.selected <<- input$sheet 
            updateSelectInput(session, "sheet", choices = names, selected = sheet.selected)

            if (input$sheet %in% names)
            {
              n <- which(names==input$sheet)
              # sheet.selected <<- input$sheet 
            } else {
              n <- 1
              #Ensures update and selection of first sheet upon loading the data
              updateSelectInput(session, "sheet", choices = names)
            }
            
            # names <- excel_sheets(path = input$upload$datapath)
            # updateSelectInput(session, "sheet_names", choices = names)
            data <- read_excel(file_in$datapath, sheet = n , na = c("",".","NA", "NaN", "#N/A", "#VALUE!"))
          } 
          
        # })
      }
      
    } else if (input$data_input == 5) {
      # genelist.selected <<- ""
      # updateCheckboxInput(session, "transformation", value = FALSE)
      # transform_var_x.selected <<- "-"
      # transform_var_y.selected <<- "-"
      
      #Read data from a URL
      #This requires RCurl
      if(input$URL == "") {
        return(data.frame(x = "Enter a full HTML address, for example: https://zenodo.org/record/3713174/files/CSV_1-GFP-CSB-WT_vs_GFP-NLS.csv"))
      } else if (url.exists(input$URL) == FALSE) {
        return(data.frame(x = paste("Not a valid URL: ",input$URL)))
      } else {data <- read.csv(input$URL)}
    }
  
  
  #Replace space and dot of header names by underscore
  data <- data %>% select_all(~gsub("\\s+|\\.", "_", .))
    return(data)
  })
  
  
  #### DISPLAY UPLOADED DATA (as provided) ##################
  
output$uploaded <- renderDataTable(
    
    #    observe({ print(input$tidyInput) })
    df_upload(),
    # df_transformed(),
    rownames = FALSE,
    options = list(pageLength = 20, autoWidth = FALSE,
                   lengthMenu = c(20, 100, 1000, 10000)),
    editable = FALSE
    # ,selection = 'none'
  )
  
  
  


  ############## Export Normalized data in tidy format ###########
  
  # output$downloadTransformedData <- downloadHandler(
  #   
  #   filename = function() {
  #     paste("VolcaNoseR_transformed", ".csv", sep = "")
  #   },
  #   content = function(file) {
  #       write.csv(df_transformed(), file, row.names = FALSE)
  #   }
  # )
  

  ##### Get Variables from the input ##############
  
observe({
  
  #Retrieve the currently selected geom and use as default, even when y_var changes


  
  # if (input$transformation != TRUE) {
    df <- df_upload()
  # } else if (input$transformation == TRUE) {
  #   transform_var_x.selected <<- input$transform_var_x
  #   transform_var_y.selected <<- input$transform_var_y
  #  df <- df_transformed() 
  # }
  
  
    var_names  <- names(df)

    # Get the names of columns that are factors.
    nms_fact <- names(Filter(function(x) is.factor(x) || is.integer(x) || is.logical(x) || is.character(x), df))
    nms_var <- names(Filter(function(x) is.integer(x) || is.numeric(x) || is.double(x), df))
    nms_fact <- c("-",nms_fact)


    # Pre-selection works well when example 1 or 2 is selected, but may interfere with URL-loading 
    # updateSelectInput(session, "x_var", choices = nms_var, selected = "log2_FoldChange")
    # updateSelectInput(session, "y_var", choices = nms_var, selected = "minus_log10_pvalue")
    
    
    updateSelectInput(session, "x_var", choices = nms_var, selected = x_var.selected)
    updateSelectInput(session, "y_var", choices = nms_var, selected = y_var.selected)
    
    # updateSelectInput(session, "map_size", choices = mapping_list_all)
   updateSelectInput(session, "g_var", choices = nms_fact, selected = g_var.selected)
   
   updateSelectizeInput(session, "user_gene_list", selected = genelist.selected)
   
   # updateSelectInput(session, "transform_var_x", choices = c("-",nms_var), selected = transform_var_x.selected)
   # updateSelectInput(session, "transform_var_y", choices = c("-",nms_var), selected = transform_var_y.selected)   
   

  })
  
  
########### GET INPUT VARIABLEs FROM HTML ##############

observe({
  query <- parseQueryString(session$clientData$url_search)
  

  ############ ?url ################
  
  if (!is.null(query[['url']])) {
    # updateRadioButtons(session, "data_input", selected = 5)  
    updateTextInput(session, "URL", value= query[['url']])
    observe(print((query[['url']])))
    # updateTabsetPanel(session, "tabs", selected = "Plot")
  }
  
  ############ ?data ################
  
  if (!is.null(query[['data']])) {
    presets_data <- query[['data']]
    presets_data <- unlist(strsplit(presets_data,";"))
    observe(print((presets_data)))
    
    updateRadioButtons(session, "data_input", selected = presets_data[1])    
    # updateCheckboxInput(session, "tidyInput", value = presets_data[2])
    
    # updateSelectInput(session, "x_var", selected = presets_data[3])
    # updateSelectInput(session, "y_var", selected = presets_data[4])    
    # updateSelectInput(session, "g_var", selected = presets_data[5])
    
    x_var.selected <<- presets_data[3]
    y_var.selected <<- presets_data[4]
    g_var.selected <<- presets_data[5]
    
    if (presets_data[1] == "1" || presets_data[1] == "2") {
      updateTabsetPanel(session, "tabs", selected = "Plot")
    }
  }
  
  
  
  ############ ?vis ################
  
  if (!is.null(query[['vis']])) {
    
    presets_vis <- query[['vis']]
    presets_vis <- unlist(strsplit(presets_vis,";"))
    observe(print((presets_vis)))
    
    updateSliderInput(session, "pointSize", value = presets_vis[1])
    updateSliderInput(session, "alphaInput", value = presets_vis[2])
    updateSliderInput(session, "fc_cutoff", value = unlist(strsplit(presets_vis[3],",")))
    updateSliderInput(session, "p_cutoff", value = presets_vis[4])
    updateSelectInput(session, "direction", selected = presets_vis[5])
    updateSelectInput(session, "criterion", selected = presets_vis[6])
    
  }
  
  
  ############ ?can ################
  
  if (!is.null(query[['can']])) {
    
    presets_can <- query[['can']]
    presets_can <- unlist(strsplit(presets_can,";"))
    
    updateNumericInput(session, "top_x", value = presets_can[1])
    updateCheckboxInput(session, "show_table", value = presets_can[2])
    updateCheckboxInput(session, "hide_labels", value= presets_can[3])
    # updateCheckboxInput(session, "user_selected", value= presets_can[4])
    # updateTextInput(session, "user_gene_list", value= presets_can[4])
    
    genelist.selected <<- unlist(strsplit(presets_can[4],","))
    # observe({print(genelist.selected)})

  }
  

  ############ ?layout ################
  
  if (!is.null(query[['layout']])) {
    
    presets_layout <- query[['layout']]
    presets_layout <- unlist(strsplit(presets_layout,";"))
    # observe(print((presets_layout)))
    
    updateCheckboxInput(session, "rotate_plot", value = presets_layout[1])
    updateCheckboxInput(session, "add_grid", value = (presets_layout[2]))
    
    updateCheckboxInput(session, "change_scale", value = presets_layout[3])
    updateTextInput(session, "range_x", value= presets_layout[4])
    updateTextInput(session, "range_y", value= presets_layout[5])
    # updateCheckboxInput(session, "transform", value = presets_layout[6])
    # updateRadioButtons(session, "transform_x", selected = presets_layout[7])
    # updateRadioButtons(session, "transform_y", selected = presets_layout[8])    
    #    updateCheckboxInput(session, "add_description", value = presets_layout[9])
    if ((presets_layout[6])=='X') {
      updateNumericInput(session, "plot_height", value= presets_layout[7])
      updateNumericInput(session, "plot_width", value= presets_layout[8])
    }
    #  updateTabsetPanel(session, "tabs", selected = "Plot")
  }
  
  ############ ?color ################
  
  if (!is.null(query[['color']])) {

    presets_color <- query[['color']]
    presets_color <- unlist(strsplit(presets_color,";"))

    updateSelectInput(session, "adjustcolors", selected = presets_color[1])
    updateTextInput(session, "user_color_list", value= presets_color[2])
  }
  
  ############ ?label ################
  
  if (!is.null(query[['label']])) {
    
    presets_label <- query[['label']]
    presets_label <- unlist(strsplit(presets_label,";"))

    updateCheckboxInput(session, "add_title", value = presets_label[1])
    updateTextInput(session, "title", value= presets_label[2])
    
    updateCheckboxInput(session, "label_axes", value = presets_label[3])
    updateTextInput(session, "lab_x", value= presets_label[4])
    updateTextInput(session, "lab_y", value= presets_label[5])
    
    updateCheckboxInput(session, "adj_fnt_sz", value = presets_label[6])
    updateNumericInput(session, "fnt_sz_title", value= presets_label[7])
    updateNumericInput(session, "fnt_sz_labs", value= presets_label[8])
    
    updateNumericInput(session, "fnt_sz_ax", value= presets_label[9])
    updateNumericInput(session, "fnt_sz_cand", value= presets_label[10])
    updateCheckboxInput(session, "add_legend", value = presets_label[11])    
    # updateTextInput(session, "legend_title", value= presets_label[12])


  }
  

  
  
  ############ ?url ################
  
  if (!is.null(query[['url']])) {
    updateRadioButtons(session, "data_input", selected = 5)  
    updateTextInput(session, "URL", value= query[['url']])
    # observe(print((query[['url']])))
    updateTabsetPanel(session, "tabs", selected = "Plot")
  }
  
  
})

########### RENDER URL ##############

output$HTMLpreset <- renderText({
  url()
})

######### GENERATE URL with the settings #########

url <- reactive({
  
  base_URL <- paste(sep = "", session$clientData$url_protocol, "//",session$clientData$url_hostname, ":",session$clientData$url_port, session$clientData$url_pathname)
  
  data <- c(input$data_input, "", input$x_var, input$y_var, input$g_var)
  
  # vis <- c(input$pointSize, input$alphaInput, input$fc_cutoff, input$p_cutoff, input$direction, input$criterion)
  #Convert upper/lower boundary to comma seperated values
  fc <- input$fc_cutoff
  fc <- paste(fc, collapse=",")

    vis <- c(input$pointSize, input$alphaInput, fc, input$p_cutoff, input$direction, input$criterion)
  
  #Convert the list of genes to a comma-seperated string  
  a <- input$user_gene_list
  a <- paste(a, collapse=",")
  # observe({print(a)})
  
  #as.character is necessary; if omitted TRUE is converted to 0 and FALSE to 1 which is undesired
  can <- c(input$top_x, as.character(input$show_table), input$hide_labels, a)

  layout <- c(input$rotate_plot, input$add_grid, input$change_scale, input$range_x, input$range_y, "X", input$plot_height, input$plot_width)
  
  #Hide the standard list of colors if it is'nt used
  if (input$adjustcolors != "5") {
    color <- c(input$adjustcolors, "none")
  } else if (input$adjustcolors == "5") {
    color <- c(input$adjustcolors, input$user_color_list)
  }
  
  ############ COLOR ##########

  label <- c(input$add_title, input$title, input$label_axes, input$lab_x, input$lab_y, input$adj_fnt_sz, input$fnt_sz_title, input$fnt_sz_labs, input$fnt_sz_ax, input$fnt_sz_cand, input$add_legend, input$legend_title, input$hide_labels_y)

  #replace FALSE by "" and convert to string with ; as seperator
  data <- sub("FALSE", "", data)
  data <- paste(data, collapse=";")
  data <- paste0("data=", data) 
  
  vis <- sub("FALSE", "", vis)
  vis <- paste(vis, collapse=";")
  vis <- paste0("vis=", vis)
  
  
  can <- sub("FALSE", "", can)
  can <- paste(can, collapse=";")
  can <- paste0("can=", can)
  
  # 
  layout <- sub("FALSE", "", layout)
  layout <- paste(layout, collapse=";")
  layout <- paste0("layout=", layout)
  # 
  color <- sub("FALSE", "", color)
  color <- paste(color, collapse=";")
  color <- paste0("color=", color)
  
  label <- sub("FALSE", "", label)
  label <- paste(label, collapse=";")
  label <- paste0("label=", label) 
  
  # stim <- sub("FALSE", "", stim)
  # stim <- paste(stim, collapse=";")
  # stim <- paste0("stim=", stim) 
  
  if (input$data_input == "5") {url <- paste("url=",input$URL,sep="")} else {url <- NULL}
  
  # parameters <- paste(data, vis,layout,color,label,stim,url, sep="&")
  
  parameters <- paste(data,vis,can,layout,color,label,url, sep="&")

  
    preset_URL <- paste(base_URL, parameters, sep="?")
  
  observe(print(parameters))
  # observe(print(preset_URL))  
  return(preset_URL)
})


############# Pop-up that displays the URL to 'clone' the current settings ################

observeEvent(input$settings_copy , {
  showModal(urlModal(url=url(), title = "Use the URL to launch VolcaNoseR with the current setting"))
})

# observeEvent(input$legend_copy , {
#   showModal(urlModal(url=Fig_legend(), title = "Legend text"))
# })






  
  ################ Select top hits #########
  df_top  <- reactive({
        df <- df_filtered()

        
        if (input$direction =="increased") {
          df <- df %>% filter(Change=="Increased")
          
        } else if (input$direction =="decreased") {
          df <- df %>% filter(Change=="Decreased")
          
        } else if (input$direction =="significant") {
          df <- df %>% filter(Change!="Unchanged")
          
        }
         
        
        if (input$criterion == "manh") {    
        df <- df %>% mutate(`Manhattan distance` = abs(`Significance`)+abs(`Fold change (log2)`)) %>% arrange(desc(`Manhattan distance`))
        df_out <- df %>% top_n(input$top_x,`Manhattan distance`) %>% select(Name, Change, `Fold change (log2)`,`Significance`,`Manhattan distance`)
        } else if (input$criterion == "euclid") {
          df <- df %>% mutate(`Euclidean distance` = sqrt((`Significance`)^2+(`Fold change (log2)`)^2)) %>% arrange(desc(`Euclidean distance`))
          df_out <- df %>% top_n(input$top_x,`Euclidean distance`) %>% select(Name, Change, `Fold change (log2)`,`Significance`,`Euclidean distance`)
        } else if (input$criterion == "fc") {
          df <- df %>% arrange(desc(abs(`Fold change (log2)`)))
          df_out <- df %>% top_n(input$top_x,abs(`Fold change (log2)`)) %>% select(Name, Change, `Fold change (log2)`,`Significance`)
        } else if (input$criterion == "sig") {
          df <- df %>% arrange(desc(`Significance`))
          df_out <- df %>% top_n(input$top_x,`Significance`) %>% select(Name, Change, `Fold change (log2)`,`Significance`)
        }
        
        #Add user selected hits, but remove them when already present
        df_out <- bind_rows(df_out,df_user()) %>% distinct(Name, .keep_all = TRUE)
        # }
        
        # observe({print(df_out)})
        return(df_out)
  })

  ################ List of user-selected hits #########
  df_user <- reactive({
    

    df <- as.data.frame(df_filtered())
    
    #select based on text input
    usr_selection <- input$user_gene_list
    df_selected_by_name <- df %>% filter(Name %in% usr_selection)    

    
    # clicked <- nearPoints(df, input$clicked)
    # observe({print(clicked)})
    
    
    #Select rows from DT
    table_selection <- input$uploaded_rows_selected
    # observe({print(table_selection)})
    if (length(table_selection)>=1){
      df_selected_by_tab <- df %>% slice(table_selection)
      df_selected_by_name <- df_selected_by_name %>% bind_rows(df_selected_by_tab)
    }

    return(df_selected_by_name)


  })
  
  ################ LOG TRANSFORM DATA #########
# df_transformed <- reactive({     
#     
#     df <- df_upload()
#     
#     x_transform <- input$transform_var_x
#     y_transform <- input$transform_var_y
#     
# 
#     
#     if (input$transform_x =="log2" && x_transform !="-") {
#       log_name <- paste0(input$transform_x,"_",input$transform_var_x)
#       df <- df %>% mutate(!!log_name := log2(df[, x_transform]))
#     } else if (input$transform_x =="log10" && x_transform !="-") {
#       log_name <- paste0(input$transform_x,"_",input$transform_var_x)
#       df <- df %>% mutate(!!log_name := log10(df[, x_transform]))
#     } else if (input$transform_x =="minus_log10" && x_transform !="-") {
#       log_name <- paste0("-Log10_",input$transform_var_x)
#       df <- df %>% mutate(!!log_name := -log10(df[, x_transform]))
#     }
#     
#     if (input$transform_y =="log2" && y_transform !="-") {
#       log_name <- paste0(input$transform_y,"_",input$transform_var_y)
#       df <- df %>% mutate(!!log_name := log2(df[, y_transform]))
#     } else if (input$transform_y =="log10" && y_transform !="-") {
#       log_name <- paste0(input$transform_y,"_",input$transform_var_y)
#       df <- df %>% mutate(!!log_name := log10(df[, y_transform]))
#     } else if (input$transform_y =="minus_log10" && y_transform !="-") {
#       log_name <- paste0("-Log10_",input$transform_var_y)
#       df <- df %>% mutate(!!log_name := -log10(df[, y_transform]))
#     }
#     
#     
#     return(df)
#     
#     
#   })
  
  
  
  ################ SELECT COLUMNS AND ANNOTATE CHANGES #########
df_filtered <- reactive({     
    
      df <- df_upload()

    x_choice <- input$x_var
    y_choice <- input$y_var
    g_choice <- input$g_var
    

    if (g_choice == "-") {
      koos <- df %>% select(`Fold change (log2)` = !!x_choice , `Significance` = !!y_choice)
      koos$Name <- " "
    } else if (g_choice != "-") {
      koos <- df %>% select(`Fold change (log2)` = !!x_choice , `Significance` = !!y_choice, Name = input$g_var)
      #Remove  names after semicolon for hits with multiple names, seperated by semicolons, e.g.: POLR2J3;POLR2J;POLR2J2
      koos <- koos %>% mutate(Name = gsub(';.*','',Name))
      
    }
    
    #Update the gene list for user selection
    updateSelectizeInput(session, "user_gene_list", choices = koos$Name, selected = genelist.selected)

      
    foldchange_min=input$fc_cutoff[1]
    foldchange_max=input$fc_cutoff[2]

    pvalue_tr=input$p_cutoff

##    koos <- koos %>%mutate(Change = ifelse((foldchange >= foldchange_tr && pvalue >= pvalue_tr ),"Increased", ifelse(foldchange<=-foldchange_tr , "Decreased", "Unchanged")))

    if (input$direction =="decreased") {
      koos <- koos %>%mutate(
        Change = case_when(
          `Fold change (log2)` < foldchange_min & `Significance` > pvalue_tr ~ "Decreased",
          TRUE ~ "Unchanged")
      )
    } else if (input$direction =="increased") {
      koos <- koos %>%mutate(
        Change = case_when(
          `Fold change (log2)` > foldchange_max & `Significance` > pvalue_tr ~ "Increased",
          TRUE ~ "Unchanged")
      )
    } else {
    koos <- koos %>%mutate(
           Change = case_when(
             `Fold change (log2)` > foldchange_max & `Significance` > pvalue_tr ~ "Increased",
             `Fold change (log2)` < foldchange_min & `Significance` > pvalue_tr ~ "Decreased",
             TRUE ~ "Unchanged")
          )
    }
    
    return(koos)
    #Replace space and dot of header names by underscore
    # df <- df %>%  
    #   select_all(~gsub("\\s+|\\.", "_", .))
    
})

############## Render the data summary as a dataTable ###########

output$toptableDT <- renderDataTable(
    
    df_top(),
    extensions = c('Buttons'),
    rownames = FALSE,
    options = list(dom = 'Blfrtip', buttons = c('copy', 'csv','excel', 'pdf'), autoWidth = FALSE, lengthMenu = c(20, 50, 100)),
    editable = FALSE,selection = 'none'
  )
  
plot_data <- reactive({
  
  if (input$dark) {line_color="white"} else {line_color="gray20"}
    
    ############## Adjust X-scaling if necessary ##########
    
    #Adjust scale if range for x (min,max) is specified
    if (input$range_x != "" &&  input$change_scale == TRUE) {
      rng_x <- as.numeric(strsplit(input$range_x,",")[[1]])
      observe({ print(rng_x) })
    } else if (input$range_x == "" ||  input$change_scale == FALSE) {
      
      rng_x <- c(NULL,NULL)
    }
    
    ############## Adjust Y-scaling if necessary ##########
    
    #Adjust scale if range for y (min,max) is specified
    if (input$range_y != "" &&  input$change_scale == TRUE) {
      rng_y <- as.numeric(strsplit(input$range_y,",")[[1]])
    } else if (input$range_y == "" ||  input$change_scale == FALSE) {
      
      rng_y <- c(NULL,NULL)
    }
    
    df <- as.data.frame(df_filtered())
    #Convert 'Change' to a factor to keep this order, necessary for getting the colors right
    df$Change <- factor(df$Change, levels=c("Unchanged","Increased","Decreased"))
    
    ########## Determine color use #############
    if (input$adjustcolors == 1 && input$dark) {
      newColors <- c("#505050", "#FF3333", "#0092CC")
    } else if (input$adjustcolors == 1 && input$dark == FALSE) {
      newColors <- c("grey", "red", "blue")
    }
    
    if (input$adjustcolors == 3 && input$dark) {
      newColors <- c( "#505050", "deepskyblue", "green")
    } else if (input$adjustcolors == 3 && input$dark == FALSE) {
      newColors <- c("Grey80", "darkblue", "darkgreen")
    }
  
    
    if (input$adjustcolors == 4 && input$dark) {
      newColors <- c("#505050", "#BB86FC", "#03DAC5") }
    else if (input$adjustcolors == 4 && input$dark == FALSE)
    { newColors <- c("grey", "#9932CC","#turquoise4")
    # } else if (input$adjustcolors == 6) {
    #   newColors <- Okabe_Ito
    }
    
    if (input$adjustcolors == 5) {
      newColors <- gsub("\\s","", strsplit(input$user_color_list,",")[[1]])
      
      #If unsufficient colors available, repeat
      if(length(newColors) < 3) {
        newColors<-rep(newColors,times=(round(3/length(newColors)))+1)
      }
      
      
    }
    
    # Remove the color for category 'increased' when absent
    if (("Increased" %in% df$Change) == FALSE) {
      newColors <- newColors[c(1,3)]
      
    }
    
    
    p <-  ggplot(data = df) +
      aes(x=`Fold change (log2)`) +
      aes(y=`Significance`) +
      geom_point(alpha = input$alphaInput, size = input$pointSize, shape = 16) +
      
      # This needs to go here (before annotations)
      theme_light(base_size = 16) +
      aes(color=Change) + 
      scale_color_manual(values=newColors) +
      
      NULL
    
    if (input$dark) {p <- p+ theme_light_dark_bg(base_size = 16)}
    
    
    #Indicate cut-offs with dashed lines
    if (input$direction !="decreased")  p <- p + geom_vline(xintercept = input$fc_cutoff[2], linetype="dashed", color=line_color)
    if (input$direction !="increased")  p <- p + geom_vline(xintercept = input$fc_cutoff[1], linetype="dashed", color=line_color)
    
    p <- p + geom_hline(yintercept = input$p_cutoff, linetype="dashed", color=line_color) 
    
    # if log-scale checked specified
    if (input$scale_log_10)
      p <- p + scale_y_log10() 
    
    #remove gridlines (if selected)
    if (input$add_grid == FALSE) {  
      p <- p+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
    }
    
    ########## User defined labeling     
    if (input$hide_labels == FALSE) {
      p <-  p + geom_point(data=df_top(), aes(x=`Fold change (log2)`,y=`Significance`), shape=1,color=line_color, size=(input$pointSize))+
        geom_text_repel(
          data = df_top(),
          aes(label = Name),
          size = input$fnt_sz_cand,
          color=line_color,
          nudge_x = 0.2,
          nudge_y=0.2,
          box.padding = unit(0.9, "lines"),
          point.padding = unit(.3+input$pointSize*0.1, "lines"),show.legend=F
          )
      
    }
    # 
    # ########## Top hits labeling 
    # if (input$hide_labels == FALSE) {
    #   p <- p + geom_text_repel(
    #     data = df_top(),
    #     aes(label = Name),
    #     size = input$fnt_sz_cand,
    #     nudge_x = 0.2,
    #     nudge_y=-0.2,
    #     # check_overlap = TRUE,
    #     box.padding = unit(0.35, "lines"),
    #     point.padding = unit(0.3+input$pointSize*0.1, "lines"),
    #     show.legend=F
    #   )
    #   
    # }
    p <- p + coord_cartesian(xlim=c(rng_x[1],rng_x[2]),ylim=c(rng_y[1],rng_y[2]))
    #### If selected, rotate plot 90 degrees CW ####
    if (input$rotate_plot == TRUE) { p <- p + coord_flip(xlim=c(rng_x[1],rng_x[2]),ylim=c(rng_y[1],rng_y[2]))}
    
    ########## Do some formatting of the lay-out ##########
    
    
    
    # if title specified
    if (input$add_title == TRUE) {
      #Add line break to generate some space
      title <- paste(input$title, "\n",sep="")
      p <- p + labs(title = title)
    } else if (input$sheet !=" ") {
      title <- paste(input$sheet, "\n",sep="")
      # observe({print('yay')})
      p <- p + labs(title = title)
    }
    
    # # if labels specified
    if (input$label_axes)
    {p <- p + labs(x = input$lab_x, y = input$lab_y)}
    else {
      p <- p + labs(x=bquote('Fold Change ('*Log[2]*')'), y=bquote('Significance ('*-Log[10]*')'))
    }
    
    # # if font size is adjusted
    if (input$adj_fnt_sz) {
      p <- p + theme(axis.text = element_text(size=input$fnt_sz_ax))
      p <- p + theme(axis.title = element_text(size=input$fnt_sz_labs))
      p <- p + theme(plot.title = element_text(size=input$fnt_sz_title))
    }
    
    #remove legend (if selected)
    if (input$add_legend == FALSE) {  
      p <- p + theme(legend.position="none")
    }
    
    p
    
  })

  
    ##### Render the plot ############

  ##### Set width and height of the plot area
  width <- reactive ({ input$plot_width })
  height <- reactive ({ input$plot_height }) 
  
output$coolplot <- renderPlot(width = width, height = height,{
  
  if (input$dark) {line_color="white"} else {line_color="gray20"}
  
  df <- as.data.frame(df_filtered())
  #Convert 'Change' to a factor to keep this order, necessary for getting the colors right
  df$Change <- factor(df$Change, levels=c("Unchanged","Increased","Decreased"))


  ########## Determine color use #############
  if (input$adjustcolors == 1 && input$dark) {
    newColors <- c("#505050", "#FF3333", "#0092CC")
  } else if (input$adjustcolors == 1 && input$dark == FALSE) {
    newColors <- c("grey", "red", "blue")
  }
  
  if (input$adjustcolors == 3 && input$dark) {
    newColors <- c( "#505050", "deepskyblue", "green")
  } else if (input$adjustcolors == 3 && input$dark == FALSE) {
    newColors <- c("Grey80", "darkblue", "darkgreen")
  }
  
  
  if (input$adjustcolors == 4 && input$dark) {
    newColors <- c("#505050", "#BB86FC", "#03DAC5") }
  else if (input$adjustcolors == 4 && input$dark == FALSE)
  { newColors <- c("grey", "#9932CC","turquoise4")
  # } else if (input$adjustcolors == 6) {
  #   newColors <- Okabe_Ito
  }
  
  if (input$adjustcolors == 5) {
    newColors <- gsub("\\s","", strsplit(input$user_color_list,",")[[1]])
    
    #If unsufficient colors available, repeat
    if(length(newColors) < 3) {
      newColors<-rep(newColors,times=(round(3/length(newColors)))+1)
    }
    
    
  }
  
  # Remove the color for category 'increased' when absent
  if (("Increased" %in% df$Change) == FALSE) {
    newColors <- newColors[c(1,3)]
    
  }
  
  
  ############## Adjust X-scaling if necessary ##########
  
  #Adjust scale if range for x (min,max) is specified
  if (input$range_x != "" &&  input$change_scale == TRUE) {
    rng_x <- as.numeric(strsplit(input$range_x,",")[[1]])
    observe({ print(rng_x) })
  } else if (input$range_x == "" ||  input$change_scale == FALSE) {
    
    rng_x <- c(NULL,NULL)
  }
  
    
    ############## Adjust Y-scaling if necessary ##########
    
    #Adjust scale if range for y (min,max) is specified
    if (input$range_y != "" &&  input$change_scale == TRUE) {
      rng_y <- as.numeric(strsplit(input$range_y,",")[[1]])
    } else if (input$range_y == "" ||  input$change_scale == FALSE) {
      
      rng_y <- c(NULL,NULL)
    }
  

    
    p <-  ggplot(data = df) +
      aes(x=`Fold change (log2)`) +
      aes(y=`Significance`) +
      geom_point(alpha = input$alphaInput, size = input$pointSize, shape = 16) +
      
 
      
      # This needs to go here (before annotations)
      theme_light(base_size = 16) +
      aes(color=Change) + 
      scale_color_manual(values=newColors) +
    
      NULL
    
    if (input$dark) {p <- p+ theme_light_dark_bg(base_size = 16)}
    
    
    #Indicate cut-offs with dashed lines
    if (input$direction !="decreased")  p <- p + geom_vline(xintercept = input$fc_cutoff[2], linetype="dashed", color=line_color)
    if (input$direction !="increased")  p <- p + geom_vline(xintercept = input$fc_cutoff[1], linetype="dashed", color=line_color)
    
    p <- p + geom_hline(yintercept = input$p_cutoff, linetype="dashed", color=line_color) 
    
    # if log-scale checked specified
    if (input$scale_log_10)
      p <- p + scale_y_log10() 
    
    #remove gridlines (if selected)
    if (input$add_grid == FALSE) {  
      p <- p+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
    }

    ########## User defined labeling     
    if (input$hide_labels == FALSE) {
      p <-  p + geom_point(data=df_top(), aes(x=`Fold change (log2)`,y=`Significance`), shape=1,color=line_color, size=(input$pointSize))+
        geom_text_repel(
          data = df_top(),
          aes(label = Name),
          size = input$fnt_sz_cand,
          color=line_color,
          nudge_x = 0.2,
          nudge_y=0.2,
          box.padding = unit(0.9, "lines"),
          point.padding = unit(.3+input$pointSize*0.1, "lines"),show.legend=F
        )
      
    }
    # 
    # ########## Top hits labeling 
    # if (input$hide_labels == FALSE) {
    #   p <- p + geom_text_repel(
    #     data = df_top(),
    #     aes(label = Name),
    #     size = input$fnt_sz_cand,
    #     nudge_x = 0.2,
    #     nudge_y=-0.2,
    #     # check_overlap = TRUE,
    #     box.padding = unit(0.35, "lines"),
    #     point.padding = unit(0.3+input$pointSize*0.1, "lines"),
    #     show.legend=F
    #   )
    #   
    # }
    p <- p + coord_cartesian(xlim=c(rng_x[1],rng_x[2]),ylim=c(rng_y[1],rng_y[2]))
    #### If selected, rotate plot 90 degrees CW ####
    if (input$rotate_plot == TRUE) { p <- p + coord_flip(xlim=c(rng_x[1],rng_x[2]),ylim=c(rng_y[1],rng_y[2]))}
    ########## Do some formatting of the lay-out ##########
    
    
    
    # if title specified
    if (input$add_title == TRUE) {
      #Add line break to generate some space
      title <- paste(input$title, "\n",sep="")
      p <- p + labs(title = title)
    } else if (input$sheet !=" ") {
      title <- paste(input$sheet, "\n",sep="")
      # observe({print('yay')})
      p <- p + labs(title = title)
    }
    
    # # if labels specified
    if (input$label_axes)
      {p <- p + labs(x = input$lab_x, y = input$lab_y)}
    else {
      p <- p + labs(x=bquote('Fold Change ('*Log[2]*')'), y=bquote('Significance ('*-Log[10]*')'))
    }
    
    # # if font size is adjusted
    if (input$adj_fnt_sz) {
      p <- p + theme(axis.text = element_text(size=input$fnt_sz_ax))
      p <- p + theme(axis.title = element_text(size=input$fnt_sz_labs))
      p <- p + theme(plot.title = element_text(size=input$fnt_sz_title))
    }
    
    #remove legend (if selected)
    if (input$add_legend == FALSE) {  
      p <- p + theme(legend.position="none")
    }

    p
  })
  
###### From: https://gitlab.com/snippets/16220 ########
output$hover_info <- renderUI({
  df <- as.data.frame(df_filtered())
  
  hover <- input$plot_hover
  point <- nearPoints(df, hover, threshold = 10, maxpoints = 1, addDist = FALSE)
  if (nrow(point) == 0) return(NULL)
  
  # calculate point position INSIDE the image as percent of total dimensions
  # from left (horizontal) and from top (vertical)
  left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
  
  # calculate distance from left and bottom side of the picture in pixels
  left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
  top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
  
  # create style property fot tooltip
  # background color is set so tooltip is a bit transparent
  # z-index is set so we are sure are tooltip will be on top
  style <- paste0("position:absolute;
                  padding: 5px;
                  z-index:100; background-color: rgba(200, 200, 245, 0.65); ",
                  "left:", left_px + 20, "px; top:", top_px + 32, "px;")
  
  # actual tooltip created as wellPanel
  wellPanel(
    style = style,
    p(HTML(paste0("<b> Name: </b>", point$Name, "<br/>",
                  "<b> Fold change: </b>", round(point[1],2), "<br/>",
                  "<b> Significance: </b>", round(point[2],2), "<br/>",
                  # "<b> Number: </b>", rownames(point), "<br/>",
                  # top_px,
                  NULL
    )
    ))
  )
})

  
  ######### DEFINE DOWNLOAD BUTTONS FOR ORDINARY PLOT ###########
  
output$downloadPlotPDF <- downloadHandler(
            filename <- function() {
                        paste("VolcaNoseR_", Sys.time(), ".pdf", sep = "")
            },
            content <- function(file) {
                        pdf(file, width = input$plot_width/72, height = input$plot_height/72)
                        plot(plot_data())
                        
                        dev.off()
            },
            contentType = "application/pdf" # MIME type of the image
)


output$downloadPlotSVG <- downloadHandler(
            filename <- function() {
                        paste("VolcaNoseR_", Sys.Date(), ".svg", sep = "")
            },
            content <- function(file) {
                        ggplot2::ggsave(file=file,plot=plot_data())
            },
            contentType = "application/svg" # MIME type of the image
)


output$downloadPlotPNG <- downloadHandler(
    filename <- function() {
      paste("VolcaNoseR_", Sys.time(), ".png", sep = "")
    },
    content <- function(file) {
      png(file, width = input$plot_width*4, height = input$plot_height*4, res=300)
      plot(plot_data())

      dev.off()
    },
    contentType = "application/png" # MIME type of the image
  )  
  

########### Update count #########
# Reactively update the client.
output$count <- renderText({
  vals$count
})

# When a session ends, decrement the counter.
session$onSessionEnded(function(){
  isolate(vals$count <- vals$count - 1)
})

######## The End; close server ########################
} #Close server


# Run the application 
shinyApp(ui = ui, server = server)

