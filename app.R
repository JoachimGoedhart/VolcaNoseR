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

# To implement:
# Color selection
# Add PlotLy
# Select genes from table

library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)
library(ggrepel)
library(shinycssloaders)
library(DT)
library(RCurl)

df_example <- read.csv("Data-Vulcano-plot.csv", na.strings = "")
df_example_cdc42 <- read.csv("elife-45916-Cdc42QL_data.csv", na.strings = "")


# Create a reactive object here that we can share between all the sessions.
vals <- reactiveValues(count=0)

# Define UI for application that draws a histogram
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
            sliderInput("fc_cutoff", "Fold Change threshold:", 0, 5, step=0.1, value = 1.5),
            sliderInput("p_cutoff", "Significance threshold:", 0, 5, step=0.1, value = 2),
            selectInput("direction", label="Use threshold to highlight:", choices = list("All"="all", "Only Significant"="significant","Only Increased"="increased", "Only Decreased"="decreased"), selected ="all"),
            
            
            h4("Annotation of candidates"),
            numericInput("top_x", "Number of top candidates:", value = 10),
            
            
            checkboxInput(inputId = "show_table",
                          label = "Show top candidates in table",
                          value = FALSE),
            checkboxInput(inputId = "show_labels",
                          label = "Label top candidates in plot",
                          value = FALSE),
            checkboxInput(inputId = "user_selected",
                          label = "User defined candidates",
                          value = FALSE),
            
            conditionalPanel(condition = "input.user_selected == true",
                             textInput("user_gene_list", "Add labels for (case sensitive)", value = "DYSF,LAMC3"), 
                             

                          NULL   
            ),
            

            h4("Transformation & Scaling"),
            
            
            checkboxInput(inputId = "transform",
                          label = "Transform the data",
                          value = FALSE),
            
            conditionalPanel(condition = "input.transform==true",
                             
                             radioButtons(
                               "transform_x", "Transform x-axis data:",
                               choices =
                                 list("No" = "No",
                                      "log2" = "log2",
                                      "log10" = "log10",
                                      "-log10" = "minus_log10"),
                               selected = "No"),
                             radioButtons(
                               "transform_y", "Transform y-axis data:",
                               choices =
                                 list("No" = "No",
                                      "log2" = "log2",
                                      "log10" = "log10",
                                      "-log10" = "minus_log10"),
                               selected = "No")
                             
                             
            ),
            
            checkboxInput(inputId = "change_scale",
                          label = "Change scale",
                          value = FALSE),
            conditionalPanel(
              condition = "input.change_scale == true",
              
              
              textInput("range_x", "Range x-axis (min,max)", value = "")
              
            ),
            

            conditionalPanel(
              condition = "input.change_scale == true",
              textInput("range_y", "Range y-axis (min,max)", value = "")
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
              numericInput("fnt_sz_cand", "Labels of candidates:", value = 6)
              
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
                       "Upload TXT or CSV file" = 3,
                       "URL (csv files only)" = 5
                  )
                ,
                selected =  1),
              
              conditionalPanel(
                condition = "input.data_input=='1'",p('This data was used in a blog to demonstrate how to generate a volcano plot: https://www.gettinggeneticsdone.com/2014/05/r-volcano-plots-to-visualize-rnaseq-microarray.html'),hr()),              
              conditionalPanel(
                condition = "input.data_input=='2'",p('Data of potential interactors of Cdc42. Data from Figure 9A of this paper: https://elifesciences.org/articles/45916'),hr()),
              
              
              conditionalPanel(
                condition = "input.data_input=='3'",
        
                fileInput("upload", NULL, multiple = FALSE),

                  radioButtons(
                    "upload_delim", "Delimiter",
                    choices =
                      list("Comma" = ",",
                           "Tab" = "\t",
                           "Semicolon" = ";",
                           "Space" = " "),
                    selected = ","),          actionButton("submit_datafile_button",
                                                           "Submit datafile")

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
              

              # downloadButton("downloadData", "Download (transformed) data (csv)"),
              hr(),

              NULL
              ),
          conditionalPanel(
            condition = "input.tabs=='About'",
            
            #Session counter: https://gist.github.com/trestletech/9926129
            h4("About"),  "There are currently", 
            verbatimTextOutput("count"),
            "session(s) connected to this app."  
          )
                   
                   
      ),   #Close sidebarPanel

      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(id="tabs",
                    tabPanel("Data", h4("Data as provided"),dataTableOutput("data_uploaded")),
                    tabPanel("Plot",h3("Volcano Plot"
                                       ),
                             downloadButton("downloadPlotPDF", "Download pdf-file"),
                             #                          downloadButton("downloadPlotSVG", "Download svg-file"),
                             downloadButton("downloadPlotPNG", "Download png-file"),
                             
                             actionButton("settings_copy", icon = icon("clone"),
                                          label = "Clone current setting"),

                             plotOutput("coolplot",
                                        height = 'auto',
                                        hover = hoverOpts("plot_hover", delay = 10, delayType = "debounce")),uiOutput("hover_info"),
                             
                             conditionalPanel(
                               condition = "input.show_table == true",
                            h3("Top candidates (based on Manhattan distance from origin)")),
                             withSpinner(tableOutput('toptable'))

                              ),
                    # tabPanel("iPlot", h4("iPlot"), plotlyOutput("out_plotly")),

                    tabPanel("About", includeHTML("about.html"))
                    )
        
      )   #Close mainPanel
      

   ) #Close sidebarLayout
) #Close fluidPage

server <- function(input, output, session) {
  
  # Session variable - initialize defaults
  x_var.selected <- "log2_FoldChange"
  y_var.selected <- "minus_log10_pvalue"
  g_var.selected <- "Gene"
  

  isolate(vals$count <- vals$count + 1)
  ###### DATA INPUT ###################
  
df_upload <- reactive({
    
    if (input$data_input == 1) {
      x_var.selected <<- "log2_FoldChange"
      y_var.selected <<- "minus_log10_pvalue"
      gene.selected <<- "Gene"
      data <- df_example
    } else if (input$data_input == 2) {
      x_var.selected <<- "log2_FoldChange"
      y_var.selected <<- "minus_log10_pvalue"
      gene.selected <<- "Gene"
      data <- df_example_cdc42
    } else if (input$data_input == 3) {
      file_in <- input$upload
      # Avoid error message while file is not uploaded yet
      if (is.null(input$upload)) {
        return(data.frame(x = "Select your datafile"))
      } else if (input$submit_datafile_button == 0) {
        return(data.frame(x = "Press 'submit datafile' button"))
      } else {
        isolate({
           data <- read.csv(file=file_in$datapath, sep = input$upload_delim)
        })
      }
      
    } else if (input$data_input == 5) {
      
      #Read data from a URL
      #This requires RCurl
      if(input$URL == "") {
        return(data.frame(x = "Enter a full HTML address, for example: https://raw.githubusercontent.com/JoachimGoedhart/VolcaNoseR/master/elife-45916-Cdc42QL_data.csv"))
      } else if (url.exists(input$URL) == FALSE) {
        return(data.frame(x = paste("Not a valid URL: ",input$URL)))
      } else {data <- read.csv(input$URL)}
    }
  
  
  #Replace space and dot of header names by underscore
  data <- data %>% select_all(~gsub("\\s+|\\.", "_", .))
    return(data)
  })
  
  
  #### DISPLAY UPLOADED DATA (as provided) ##################
  
output$data_uploaded <- renderDataTable(
    
    #    observe({ print(input$tidyInput) })
    df_upload(),
    rownames = FALSE,
    options = list(pageLength = 20, autoWidth = FALSE,
                   lengthMenu = c(20, 100, 1000, 10000)),
    editable = FALSE,selection = 'none'
  )
  



  ##### Get Variables from the input ##############
  
observe({
    df <- df_upload()
    var_names  <- names(df)

    # Get the names of columns that are factors. These can be used for coloring the data with discrete colors
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
    updateSliderInput(session, "fc_cutoff", value = presets_vis[3])
    updateSliderInput(session, "p_cutoff", value = presets_vis[4])
    updateSelectInput(session, "direction", selected = presets_vis[5])

    
  }
  
  
  ############ ?can ################
  
  if (!is.null(query[['can']])) {
    
    presets_can <- query[['can']]
    presets_can <- unlist(strsplit(presets_can,";"))
    observe(print((presets_can)))
    
    updateNumericInput(session, "top_x", value = presets_can[1])
    updateCheckboxInput(session, "show_table", value = presets_can[2])
    updateCheckboxInput(session, "show_labels", value= presets_can[3])
    updateCheckboxInput(session, "user_selected", value= presets_can[4])
    updateTextInput(session, "user_gene_list", value= presets_can[5])
  }
  
  
  
  ############ ?layout ################
  
  if (!is.null(query[['layout']])) {
    
    presets_layout <- query[['layout']]
    presets_layout <- unlist(strsplit(presets_layout,";"))
    # observe(print((presets_layout)))
    
    # updateCheckboxInput(session, "no_grid", value = (presets_layout[2]))
    
    updateCheckboxInput(session, "change_scale", value = presets_layout[3])
    updateTextInput(session, "range_x", value= presets_layout[4])
    updateTextInput(session, "range_y", value= presets_layout[5])
    updateCheckboxInput(session, "transform", value = presets_layout[6])
    updateRadioButtons(session, "transform_x", selected = presets_layout[7])
    updateRadioButtons(session, "transform_y", selected = presets_layout[8])    
    #    updateCheckboxInput(session, "add_description", value = presets_layout[9])
    if (length(presets_layout)>10) {
      updateNumericInput(session, "plot_height", value= presets_layout[10])
      updateNumericInput(session, "plot_width", value= presets_layout[11])
    }
    #  updateTabsetPanel(session, "tabs", selected = "Plot")
  }
  
  ############ ?color ################
  
  # if (!is.null(query[['color']])) {
  #   
  #   presets_color <- query[['color']]
  #   presets_color <- unlist(strsplit(presets_color,";"))
  #   
  #   updateSelectInput(session, "colour_list", selected = presets_color[1])
  #   updateTextInput(session, "user_color_list", value= presets_color[2])
  # }
  
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
    # updateCheckboxInput(session, "show_labels_y", value = presets_label[13])
    
    #    updateCheckboxInput(session, "add_description", value = presets_label[9])
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
  
  vis <- c(input$pointSize, input$alphaInput, input$fc_cutoff, input$p_cutoff, input$direction)
  
  #as.character is necessary; if omitted TRUE is converted to 0 and FALSE to 1 which is undesired
  can <- c(input$top_x, as.character(input$show_table), input$show_labels, input$user_selected)
  
  if (input$user_selected==TRUE) {can <- c(can,input$user_gene_list)}
  
  layout <- c("", "", input$change_scale, input$range_x, input$range_y, input$transform, input$transform_x,
               input$transform_y, "X", input$plot_height, input$plot_width)
  

  label <- c(input$add_title, input$title, input$label_axes, input$lab_x, input$lab_y, input$adj_fnt_sz, input$fnt_sz_title, input$fnt_sz_labs, input$fnt_sz_ax, input$fnt_sz_cand, input$add_legend, input$legend_title, input$show_labels_y)

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
  # color <- sub("FALSE", "", color)
  # color <- paste(color, collapse=";")
  # color <- paste0("color=", color) 
  
  label <- sub("FALSE", "", label)
  label <- paste(label, collapse=";")
  label <- paste0("label=", label) 
  
  # stim <- sub("FALSE", "", stim)
  # stim <- paste(stim, collapse=";")
  # stim <- paste0("stim=", stim) 
  
  if (input$data_input == "5") {url <- paste("url=",input$URL,sep="")} else {url <- NULL}
  
  # parameters <- paste(data, vis,layout,color,label,stim,url, sep="&")
  
  parameters <- paste(data,vis,can,layout,label,url, sep="&")

  
    preset_URL <- paste(base_URL, parameters, sep="?")
  
  # observe(print(parameters))
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






  
  ################ Select top candidates #########
  df_top  <- reactive({
        df <- df_filtered()

        
        if (input$direction =="increased") {
          df <- df %>% filter(Change=="Increased")
          
        } else if (input$direction =="decreased") {
          df <- df %>% filter(Change=="Decreased")
          
        } else if (input$direction =="significant") {
          df <- df %>% filter(Change!="Unchanged")
          
        }
                
        df <- df %>% mutate(`Manhattan distance` = abs(`Significance`)+abs(`Fold change`)) %>% arrange(desc(`Manhattan distance`))
        

        
        df_out <- df %>% top_n(input$top_x,`Manhattan distance`) %>% select(Name, Change, `Fold change`,`Significance`,`Manhattan distance`)
        
        # observe({print(df_out)})
        return(df_out)
  })

  ################ List of user-selected candidates #########
  df_user <- reactive({
    
    usr_selection <- strsplit(input$user_gene_list,",")[[1]]
    
    df <- as.data.frame(df_filtered())

    df <- df %>% filter(Name %in% usr_selection)

  })
  
  ################ SELECT COLUMNS AND ANNOTATE CHANGES #########
df_filtered <- reactive({     
    
      df <- df_upload()

    x_choice <- input$x_var
    y_choice <- input$y_var
    g_choice <- input$g_var
    

    if (g_choice == "-") {
      koos <- df %>% select(`Fold change` = !!x_choice , `Significance` = !!y_choice)
      koos$Name <- " "
    } else if (g_choice != "-") {
      koos <- df %>% select(`Fold change` = !!x_choice , `Significance` = !!y_choice, Name = input$g_var)
      #Remove  names after semicolon for candidates with multiple names, seperated by semicolons, e.g.: POLR2J3;POLR2J;POLR2J2
      koos <- koos %>% mutate(Name = gsub(';.*','',Name))
      
    }
    

    
    if (input$transform_x =="log2") {
      koos <- koos %>% mutate(`Fold change` = log2(`Fold change`))
    } else if (input$transform_x =="log10") {
      koos <- koos %>% mutate(`Fold change` = log10(`Fold change`))
    } else if (input$transform_x =="minus_log10") {
      koos <- koos %>% mutate(`Fold change` = -log10(`Fold change`))
    }
    
    
    if (input$transform_y =="log2") {
      koos <- koos %>% mutate(`Fold change` = log2(`Fold change`))
    } else if (input$transform_y =="log10") {
      koos <- koos %>% mutate(`Fold change` = log10(`Fold change`))
    } else if (input$transform_y =="minus_log10") {
      koos <- koos %>% mutate(`Fold change` = -log10(`Fold change`))
    }
      
    foldchange_tr=input$fc_cutoff
    pvalue_tr=input$p_cutoff

##    koos <- koos %>%mutate(Change = ifelse((foldchange >= foldchange_tr && pvalue >= pvalue_tr ),"Increased", ifelse(foldchange<=-foldchange_tr , "Decreased", "Unchanged")))

    if (input$direction =="decreased") {
      koos <- koos %>%mutate(
        Change = case_when(
          `Fold change` < -foldchange_tr & `Significance` > pvalue_tr ~ "Decreased",
          TRUE ~ "Unchanged")
      )
    } else if (input$direction =="increased") {
      koos <- koos %>%mutate(
        Change = case_when(
          `Fold change` > foldchange_tr & `Significance` > pvalue_tr ~ "Increased",
          TRUE ~ "Unchanged")
      )
    } else {
    koos <- koos %>%mutate(
           Change = case_when(
             `Fold change` > foldchange_tr & `Significance` > pvalue_tr ~ "Increased",
             `Fold change` < -foldchange_tr & `Significance` > pvalue_tr ~ "Decreased",
             TRUE ~ "Unchanged")
          )
    }
    
    return(koos)
    #Replace space and dot of header names by underscore
    # df <- df %>%  
    #   select_all(~gsub("\\s+|\\.", "_", .))
    
})

############## Render the data summary as a table ###########
  
output$toptable <- renderTable({
    
    if (input$show_table == F) return(NULL)
    df <- as.data.frame(df_top())
    
  })

  
plot_data <- reactive({
    
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
    
    p <-  ggplot(data = df) +
      aes(x=`Fold change`) +
      aes(y=`Significance`) +
      geom_point(alpha = input$alphaInput, size = input$pointSize) +
      
      #Indicate cut-offs with dashed lines
      geom_vline(xintercept = input$fc_cutoff, linetype="dashed") +
      geom_vline(xintercept = -input$fc_cutoff, linetype="dashed") +
      geom_hline(yintercept = input$p_cutoff, linetype="dashed") +  
      
      # This needs to go here (before annotations)
      theme_light(base_size = 16) +
      aes(color=Change) + 
      scale_color_manual(values=c("grey", "red", "blue")) +
      
      #remove gridlines (if selected
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      
      NULL
    
    ########## User defined labeling     
    if (input$user_selected == TRUE) {
      p <-  p + geom_point(data=df_user(), aes(x=`Fold change`,y=`Significance`), shape=1,color="black", size=(input$pointSize+1))+
        geom_text_repel(
          data = df_user(),
          aes(label = Name),
          size = input$fnt_sz_cand,
          color="black",
          nudge_x = 0.2,
          nudge_y=0.2,
          box.padding = unit(0.9, "lines"),
          point.padding = unit(.3+input$pointSize*0.1, "lines"),show.legend=F
          )
      
    }
    
    ########## Top candidates labeling 
    if (input$show_labels == TRUE) {
      p <- p + geom_text_repel(
        data = df_top(),
        aes(label = Name),
        size = input$fnt_sz_cand,
        nudge_x = 0.2,
        nudge_y=-0.2,
        # check_overlap = TRUE,
        box.padding = unit(0.35, "lines"),
        point.padding = unit(0.3+input$pointSize*0.1, "lines"),
        show.legend=F
      )
      
    }
    p <- p + coord_cartesian(xlim=c(rng_x[1],rng_x[2]),ylim=c(rng_y[1],rng_y[2]))
    
    ########## Do some formatting of the lay-out ##########
    
    
    
    # if title specified
    if (input$add_title == TRUE) {
      #Add line break to generate some space
      title <- paste(input$title, "\n",sep="")
      p <- p + labs(title = title)
    }
    
    # # if labels specified
    if (input$label_axes)
      p <- p + labs(x = input$lab_x, y = input$lab_y)
    
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
    
    p <-  ggplot(data = df) +
      aes(x=`Fold change`) +
      aes(y=`Significance`) +
      geom_point(alpha = input$alphaInput, size = input$pointSize) +
      
 
      
      # This needs to go here (before annotations)
      theme_light(base_size = 16) +
      aes(color=Change) + 
        scale_color_manual(values=c("grey", "red", "blue")) +
    
      #remove gridlines (if selected
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      
      NULL
    
    #Indicate cut-offs with dashed lines
    if (input$direction !="decreased")  p <- p + geom_vline(xintercept = input$fc_cutoff, linetype="dashed")
    if (input$direction !="increased")  p <- p + geom_vline(xintercept = -input$fc_cutoff, linetype="dashed")
    
    p <- p + geom_hline(yintercept = input$p_cutoff, linetype="dashed") 
    

 ########## User defined labeling     
    if (input$user_selected == TRUE) {
      p <-  p + geom_point(data=df_user(), aes(x=`Fold change`,y=`Significance`), shape=1,color="black", size=(input$pointSize+1))+
        geom_text_repel(
          data = df_user(),
          aes(label = Name),
          size = input$fnt_sz_cand,
          color="black",
          nudge_x = 0.2,
          nudge_y=0.2,
          box.padding = unit(0.9, "lines"),
          point.padding = unit(.3+input$pointSize*0.1, "lines"),
          show.legend=F)
      
    }

  ########## Top candidates labeling 
      if (input$show_labels == TRUE) {
        p <- p + geom_text_repel(
            data = df_top(),
            aes(label = Name),
            size = input$fnt_sz_cand,
            nudge_x = 0.2,
            nudge_y=-0.2,
            # check_overlap = TRUE,
            box.padding = unit(0.35, "lines"),
            point.padding = unit(0.3+input$pointSize*0.1, "lines"),
            show.legend=F
          )
        
      }
    p <- p + coord_cartesian(xlim=c(rng_x[1],rng_x[2]),ylim=c(rng_y[1],rng_y[2]))
    ########## Do some formatting of the lay-out ##########
    
    
    
    # if title specified
    if (input$add_title == TRUE) {
      #Add line break to generate some space
      title <- paste(input$title, "\n",sep="")
      p <- p + labs(title = title)
    }
    
    # # if labels specified
    if (input$label_axes)
      p <- p + labs(x = input$lab_x, y = input$lab_y)
    
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

