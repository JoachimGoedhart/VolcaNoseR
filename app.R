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
            h4('Select X & Y variables'),

              selectInput("x_var", label = "X-axis; typically log fold change", choices = "-"),
              selectInput("y_var", label = "Y-axis; typically -log p-value", choices = "-"),
              selectInput("g_var", label = "Select column with names", choices = "-"),
            
            # h3("Select Geom(etry)"),
            #   selectInput("geom", label = NULL, choices = list("geom_point"="geom_point", "-"="-")),
            # conditionalPanel(condition = "input.geom=='geom_line'",  
            # selectInput('grouping', label="Group", choices = list("-"="-"), selected = "-")
            # ),
            h4("Modify the plot"),

            sliderInput("pointSize", "Size of the datapoints", 0, 10, 4),  
            
            
            sliderInput("alphaInput", "Visibility of the data", 0, 1, 0.8),  
            
            

            # hr(),
            sliderInput("fc_cutoff", "Fold Change threshold:", 0, 5, step=0.1, value = 1.5),
            sliderInput("p_cutoff", "Significance threshold:", 0, 5, step=0.1, value = 2),
            
            h4("Annotation of candidates"),
            numericInput("top_x", "Number of top candidates:", value = "10"),
            selectInput("direction", label=NULL, choices = list("All"="all", "Only Significant"="significant","Only Increased"="increased", "Only Decreased"="decreased"), selected ="all"),
            
            
            checkboxInput(inputId = "show_table",
                          label = "Show top candidates in table",
                          value = TRUE),
            checkboxInput(inputId = "show_labels",
                          label = "Label top candidates in plot",
                          value = TRUE),
            checkboxInput(inputId = "user_selected",
                          label = "User defined candidates",
                          value = FALSE),
            
            
            conditionalPanel(condition = "input.user_selected == true",
                             textInput("user_gene_list", "Add labels for (case sensitive)", value = "DYSF,LAMC3"), 
                             

                          NULL   
            ),
            

            h4("Scaling"),
            
            
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
                       "Upload TXT or CSV file" = 3
                  )
                ,
                selected =  1),
              
              conditionalPanel(
                condition = "input.data_input=='1'",h4('Data source'),p('This data was used in a blog to demonstrate how to generate a volcano plot: https://www.gettinggeneticsdone.com/2014/05/r-volcano-plots-to-visualize-rnaseq-microarray.html')),              
              conditionalPanel(
                condition = "input.data_input=='2'",h4('Data source'),p('Data of potential interactors of Cdc42. Data from Figure 9A of this paper: https://elifesciences.org/articles/45916')),
              
              
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
                    selected = ",")

                ),
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

  isolate(vals$count <- vals$count + 1)
  ###### DATA INPUT ###################
  
df_upload <- reactive({
    
    if (input$data_input == 1) {
      data <- df_example
    } else if (input$data_input == 2) {
      data <- df_example_cdc42
    } else if (input$data_input == 3) {
      file_in <- input$upload
      # Avoid error message while file is not uploaded yet
      if (is.null(input$upload)) {
        return(data.frame(x = "Select your datafile"))
      } else  {
        isolate({

            # data <- read_delim(file_in$datapath,
            #                    delim = input$upload_delim,
            #                    col_names = TRUE)
          
          data <- read.csv(file=file_in$datapath,
                             sep = input$upload_delim)
          # observe({print(input$upload$name)})
           
        })
      }
    }
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
  
  
  geom.selected <- "-"
  #Retrieve the currently selected geom and use as default, even when y_var changes
  observe({
    geom.selected <<- input$geom
  })
  


  ##### Get Variables from the input ##############
  
  observe({
    df <- df_upload()
    var_names  <- names(df)
    varx_list <- c("-", var_names)

    # Get the names of columns that are factors. These can be used for coloring the data with discrete colors
    nms_fact <- names(Filter(function(x) is.factor(x) || is.integer(x) ||
                               is.logical(x) ||
                               is.character(x),
                             df))
    nms_var <- names(Filter(function(x) is.integer(x) ||
                              is.numeric(x) ||
                              is.double(x),
                            df))

    vary_list <- c("-",nms_var)
    mapping_list_num <- c("No",nms_var)
    mapping_list_fact <- c("-",nms_fact)
    mapping_list_all <- c("No",var_names)
    facet_list_factors <- c(".",nms_fact)
    
    updateSelectInput(session, "x_var", choices = varx_list, selected = "log2_FoldChange")
    updateSelectInput(session, "y_var", choices = vary_list, selected = "minus_log10_pvalue")
    # updateSelectInput(session, "map_size", choices = mapping_list_all)
   updateSelectInput(session, "g_var", choices = mapping_list_fact, selected = "Gene")
    # updateSelectInput(session, "map_color", choices = mapping_list_all)


  })
  
  
  ################ Select top candidates #########
  df_top  <- reactive({
        df <- df_filtered()
        
        df <- df %>% mutate(`Manhattan distance` = abs(`Significance`)+abs(`Fold change`)) %>% arrange(desc(`Manhattan distance`))
        
        if (input$direction =="increased") {
          df <- df %>% filter(Change=="Increased")
          
        } else if (input$direction =="decreased") {
          df <- df %>% filter(Change=="Decreased")
          
        } else if (input$direction =="significant") {
          df <- df %>% filter(Change!="Unchanged")
          
        }
        
        df_out <- df %>% top_n(input$top_x,`Manhattan distance`) %>% select(Name, Change, `Fold change`,`Significance`,`Manhattan distance`)
        
        observe({print(df_out)})
        return(df_out)
  })

  
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
      
    }
    
    foldchange_tr=input$fc_cutoff
    pvalue_tr=input$p_cutoff

##    koos <- koos %>%mutate(Change = ifelse((foldchange >= foldchange_tr && pvalue >= pvalue_tr ),"Increased", ifelse(foldchange<=-foldchange_tr , "Decreased", "Unchanged")))

    
    koos <- koos %>%mutate(
           Change = case_when(
             `Fold change` > foldchange_tr & `Significance` > pvalue_tr ~ "Increased",
             `Fold change` < -foldchange_tr & `Significance` > pvalue_tr ~ "Decreased",
             TRUE ~ "Unchanged")
    )

    
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
          size = 6,
          color="black",
          nudge_x = 0.2,
          nudge_y=0.2,
          box.padding = unit(0.9, "lines"),
          point.padding = unit(.3+input$pointSize*0.1, "lines"),show_guide=F
          )
      
    }
    
    ########## Top candidates labeling 
    if (input$show_labels == TRUE) {
      p <- p + geom_text_repel(
        data = df_top(),
        aes(label = Name),
        size = 6,
        nudge_x = 0.2,
        nudge_y=-0.2,
        check_overlap = TRUE,
        box.padding = unit(0.35, "lines"),
        point.padding = unit(0.3+input$pointSize*0.1, "lines"),
        show_guide=F
      )
      
    }
    p <- p + coord_cartesian(xlim=c(rng_x[1],rng_x[2]),ylim=c(rng_y[1],rng_y[2]))
    
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
          size = 6,
          color="black",
          nudge_x = 0.2,
          nudge_y=0.2,
          box.padding = unit(0.9, "lines"),
          point.padding = unit(.3+input$pointSize*0.1, "lines"),
          show_guide=F)
      
    }

  ########## Top candidates labeling 
      if (input$show_labels == TRUE) {
        p <- p + geom_text_repel(
            data = df_top(),
            aes(label = Name),
            size = 6,
            nudge_x = 0.2,
            nudge_y=-0.2,
            check_overlap = TRUE,
            box.padding = unit(0.35, "lines"),
            point.padding = unit(0.3+input$pointSize*0.1, "lines"),
            show_guide=F
          )
        
      }
    p <- p + coord_cartesian(xlim=c(rng_x[1],rng_x[2]),ylim=c(rng_y[1],rng_y[2]))

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

