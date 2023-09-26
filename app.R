library(shiny)
library(ggplot2)
library(dplyr)
library(gt)
library(gtsummary)


########################ui##################################
#############################################################

ui <- navbarPage("UiO Nettskjema visualization tool", 
                 
                 collapsible = TRUE, #TRUE to automatically collapse the navigation elements into a menu when the width of the browser is 
                 #less than 940 pixels (useful for viewing on smaller touchscreen device)
                 #inverse = TRUE, #TRUE to use a dark background and light text for the navigation bar
                 #theme = shinytheme("united"),
                 
                 ####### own tab panel for file seleciton options ########
                 tabPanel(
                   "File selection options",
                   sidebarPanel(
                     ### put back IF I figure out how to PROPERLY merge two nettskjema (issue is join_var)
                     selectInput("num_files", label = "Number of nettskjemas to upload (and combine)", choices = c(1,2)),
                     
                     #Selector for file upload
                     fileInput('nettskjema_1', 'Choose nettskjema (CSV file)',
                               accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                     
                     ### this is fine, but do NOT know how to get list of variables in
                     ### input$join_1_var, when that depends on first file being uploaded...
                     conditionalPanel("input.num_files==2",
                                      fileInput('nettskjema_2', 'Choose nettskjema 2 (Note: must have at least one 
                                 common variable with nettskjema 1)',
                                                accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                                      selectInput("join_1_var", "Specify variable to join by", choices=""),
                                      selectInput("join_1_type", "How to join with nettskjema 1?",
                                                  choices=c("","full", "left", "right", "inner"))
                     ),
                     # alternative is to do fileInput(multiple=TRUE)
                     #ONLY show options when FINAL file is uploaded (see server)
                     conditionalPanel("output.fileUploaded",
                                      selectInput("files_loaded", "All file options finished?", choices=c("No", "Yes"))
                     )
                   )
                 ),
                 
                 
                 ####### own tabpanel for plot options and output##############
                 tabPanel(
                   "Plot options",
                   
                   #conditionalPanel("output.fileUploaded", #previously when ONLY one nettskjema possible 
                   #ONLY show options when file is uploaded (see server)
                   conditionalPanel(
                     #"output.fileUploaded", # so no empty sidebar is shown
                     "output.fileUploaded & input.files_loaded == 'Yes'", # so no empty sidebar is shown
                     sidebarPanel(
                       
                       #maybe radiobutton better?
                       selectInput("show_var_struct", label = "Show summary of variable types?", choices = c("No", "Yes")),
                       
                       selectInput("number_vars", label = "Number of variables to plot", choices = c(1, 2)),
                       
                       selectInput("select_var_1", label = "Select Variable1 ", choices = ""),
                       
                       conditionalPanel(
                         condition = "input.select_var_1 !== ''",
                         selectInput("type_of_var_1", label = "What should the variable be treated as?", 
                                     choices = c("", "categorical/factor", "continuous"))
                       ),
                       
                       conditionalPanel(
                         condition = "input.number_vars >= 2",
                         selectInput("select_var_2", label = "Select Variable 2", choices = ""),
                         
                         conditionalPanel(
                           condition = "input.select_var_2 !== ''",
                           selectInput("type_of_var_2", label = "What should the variable be treated as?", 
                                       choices = c("", "categorical/factor", "continuous"))
                         )
                       ),
                       
                       
                       ############
                       # if I can make it so selectInput is "plot_type" for both (i.e. not separate thingies),
                       # then I can merge observe event later.. but it didn't really work
                       conditionalPanel(
                         condition = "input.number_vars == 1 & input.select_var_1 !== '' & input.type_of_var_1 !== ''", 
                         selectInput("plot_type_1_var", label = "Choose type of plot", choices = "")
                       ),
                       
                       
                       conditionalPanel(
                         condition = "input.number_vars == 2 & input.select_var_1 !== '' & 
                                      input.type_of_var_1 !== '' & input.select_var_2 !== '' &
                                      input.type_of_var_2 !==''", 
                         selectInput("plot_type_2_vars", label = "Choose type of plot", choices = "")
                       ),
                       
                       
                       
                       conditionalPanel(
                         #condition = "input.type_of_var_1 !== ''", 
                         condition = "input.number_vars == 1 & input.plot_type_1_var !== '' || 
                                      input.number_vars == 2 & input.plot_type_2_vars !== ''", 
                         selectInput("plot_grouping", label = "Variable to plot by", choices = ""),
                         selectInput("createPlot", label = "Create plot!", choices = c("No", "Yes"))
                       ),
                       
                       
                       
                       conditionalPanel(
                         "input.createPlot == 'Yes'", #easier than actionButton for conditional
                         
                         sliderInput(inputId="plot_width",
                                     label="Plot width",
                                     #min=3, max=20, step=0.25, value=3), #ggsave
                                     min=0, max=1500, value=750), 
                         
                         sliderInput(inputId="plot_height",
                                     label="Plot height",
                                     #min=3, max=20, step=0.25, value=3), ggsave
                                     min=0, max=1000, value=500), 
                         
                         textInput("plot_name", "Input a name for the plot", value = "plot"), #value is initial text
                         selectInput("plot_format", label = "Choose plot format", choices = c("png", "pdf", "jpeg", "tiff")), 
                         downloadButton("downloadPlot", "Download Plot")
                       ) #end of conditional (plot output stuff)
                       
                       
                     ) #end of sidebar
                     
                   ), #end of conditional for sidebar
                   
                   mainPanel(
                     verbatimTextOutput("dataStruct"),
                     #plotOutput("result_plot")# height = "600px", width = "900px")
                     uiOutput("result_plot.ui")# if dynamic height?
                   )
                   
                   
                 ), #end of "plot options" tabpanel
                 
                 
                 
                 #####Tab panel for "table options"
                 tabPanel(
                   "Table options",

                   #conditionalPanel("output.fileUploaded", #previously when ONLY one nettskjema possible
                   #ONLY show options when file is uploaded (see server)
                   conditionalPanel(
                     #"output.fileUploaded", # so no empty sidebar is shown
                     "output.fileUploaded & input.files_loaded == 'Yes'", # so no empty sidebar is shown
                     sidebarPanel(
                       selectInput("table_variables", label = "Which variables do you want to include?", 
                                   multiple= TRUE, choices = ""),
                       
                       #put in conditional later
                       selectInput("table_grouping", label = "Variable to group by", choices = ""),
                       helpText("note: table grouping variable must be included in inclusion"),
                       selectInput("createTable", label = "Create table!", choices = c("No", "Yes")),
                       
                       conditionalPanel(
                         "input.createTable == 'Yes'",
                         textInput("table_name", "Input a name for the table", value = "table"), #value is initial text
                         #other formats png, pdf, etc. need package 'webshot'
                         selectInput("table_format", label = "Choose plot format", choices = "html"), 
                         downloadButton("downloadTable", "Download Table")
                       )
                       
                       
                     )
                 ), #end of conditionalPanel with sidebar
                 
                 mainPanel(
                   #tableOutput('result_table')
                   gt::gt_output('result_table') #GT
                 )
                 
                 
              )
                 
                 
                 
                 
                 
                 
                 
                 
) #end of UI






#############################################################





##########Server#############################################
#############################################################
server <- function(input, output, session) {
  
  #This function is repsonsible for loading in the selected file
  file_1 <- reactive({
    
    infile_1 <- input$nettskjema_1
    if (is.null(infile_1)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    
    file_1 <- read.csv(infile_1$datapath) #datapath gets loaded automatically by shiny (based on location you click?)
    file_1
    
  })
  
  
  file_2 <- reactive({
    
    req(input$num_files)
    
    if (input$num_files == 2){
      infile_2 <- input$nettskjema_2
      if (is.null(infile_2)) {
        # User has not uploaded a file yet
        return(NULL)
      }
    }
    
    file_2 <- read.csv(infile_2$datapath) #datapath gets loaded automatically by shiny (based on location you click?)
    file_2
  })
  
  ##update join var
  observe({
    
    updateSelectInput(session,"join_1_var",  ##I think observe, or updateSelectInput needs session...
                      label = "Select join variable",
                      #choices = names(filedata()),
                      choices = c("-automatic-", names(file_1())),
                      selected = "")
  })
  
  
  #the "FINAL" data
  filedata <- reactive({
    
    #if (is.null(file_2)) {
    if (input$num_files == 1) {
      
      filedata <- file_1()
      
    } else if (input$num_files == 2) { # 2 files loaded
      
      if (!is.null(file_2)){
        
        req(input$join_1_var)
        req(input$join_1_type)
        
        if (input$join_1_var == "-automatic-"){
          var_1_join <- NULL
        } else {
          var_1_join <- input$join_1_var
        }
        
        if (input$join_1_type == "full"){
          filedata <- full_join(file_1(), file_2(), by = var_1_join)
        } else if (input$join_1_type == "left"){
          filedata <- left_join(file_1(), file_2(), by = var_1_join)
        } else if (input$join_1_type == "right"){
          filedata <- right_join(file_1(), file_2(), by = var_1_join)
        } else if (input$join_1_type == "inner"){
          filedata <- inner_join(file_1(), file_2(), by = var_1_join)
        }
        
      }
      
      
    }
    
    filedata
    
  })
  
  
  
  ### to make options conditional on file upload ###
  
  output$fileUploaded <- reactive({
    return(!is.null(filedata()))
  })
  
  # I don't understand fully, but without this, then NONE of
  # the options show when file is uploaded (so it is NECESSARY)
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  ########
  
  
  #### data structure ###
  output$dataStruct <- renderPrint({
    
    if (is.null(filedata()) == FALSE){
      if (input$show_var_struct == "Yes"){
        str(filedata())
      }
    }
    
    
  })
  #####
  
  
  ####### update select_var ########
  
  observe({
    
    updateSelectInput(session,"select_var_1",  ##I think observe, or updateSelectInput needs session...
                      label = "Select Variable 1",
                      #choices = names(filedata()),
                      choices = c("", names(filedata())),
                      selected = "")
  })
  
  observe({
    
    updateSelectInput(session,"select_var_2",  ##I think observe, or updateSelectInput needs session...
                      label = "Select Variable 2",
                      #choices = names(filedata()))#,
                      choices = c("", names(filedata())),
                      selected = "")
  })
  
  ###################
  
  
  
  ### update plot options ###
  
  plot_choices_1_var <- reactive({
    
    #req(input$number_vars)
    req(input$type_of_var_1)
    
    if(input$type_of_var_1 == "continuous"){
      plot_choices <- c("histogram", "boxplot")
    } else if (input$type_of_var_1 == "categorical/factor"){
      plot_choices <- "barplot"
    }
    
    
  })
  
  observe({
    
    updateSelectInput(session,"plot_type_1_var",
                      label = "Choose type of plot",
                      choices = c("", plot_choices_1_var()))
  })
  
  
  #### grouping variables ####
  grouping_choices <- reactive({
    
    grouping_choices <- names(filedata())
    
  })
  
  observe({
    
    updateSelectInput(session,"plot_grouping",
                      label = "Choose grouping variable",
                      choices = c("-NOTHING-", grouping_choices()))
  })
  
  
  
  
  plot_choices_2_vars <- reactive({
    
    #req(input$number_vars)
    req(input$type_of_var_1)
    req(input$type_of_var_2)
    
    if(input$type_of_var_1 == "continuous" & input$type_of_var_2 == "continuous"){
      plot_choices <- "scatterplot"
    } else if ( (input$type_of_var_1 == "continuous" & input$type_of_var_2 == "categorical/factor") |
                (input$type_of_var_2 == "continuous" & input$type_of_var_1 == "categorical/factor")){
      plot_choices <- "boxplot"
    } else if (input$type_of_var_1 == "categorical/factor" & input$type_of_var_2 == "categorical/factor"){
      plot_choices <- "barplot"
    }
    
    
  })
  
  observe({
    
    updateSelectInput(session,"plot_type_2_vars",
                      label = "Choose type of plot",
                      choices = c("", plot_choices_2_vars()))
  })
  
  
  
  
  ######## single variable visualization #########
  plotObject <- reactive({ #if also saving result in png
    #observeEvent(input$createPlot, { #if just plotting in shiny
    
    #output$result_plot <- renderPlot({ #if just plotting in shiny
    
    
    req(input$number_vars)
    req(input$select_var_1)
    req(input$type_of_var_1)
    req(input$plot_grouping) #necessary, if no do_grouping
    req(input$createPlot) #not technically necessary
    
    if (input$createPlot == "Yes"){ #new #not technically necessary 
      
      plot_data <- filedata()
      #plot_var_1 <- plot_data[, input$select_var_1]
      names(plot_data)[names(plot_data) == input$select_var_1] <- "plot_var_1" #ggplot2 preferred
      
      
      # if (input$do_grouping == "Yes"){
      if (input$plot_grouping != "-NOTHING-"){
        # req(input$plot_grouping)
        names(plot_data)[names(plot_data) == input$plot_grouping] <- "grouping_var"
        plot_data$grouping_var <- as.factor(plot_data$grouping_var)
      }
      
      
      
      ##### single variable plot ######
      if (input$number_vars == 1){
        
        req(input$plot_type_1_var)
        
        #if (is.integer(plot_data$plot_var_1) | is.numeric(plot_data$plot_var_1)){
        if (input$type_of_var_1 == "continuous"){ 
          #so no issues if something like year should NOT be treated as continuous
          
          
          if (input$plot_type_1_var == "histogram"){
            
            if (input$plot_grouping == "-NOTHING-"){ 
              
              plot <-
                ggplot(plot_data, aes(x=plot_var_1))+
                geom_histogram(fill="steel blue")+
                labs(x=input$select_var_1)
              
            } else {
              
              plot <-
                ggplot(plot_data, aes(x=plot_var_1, group=grouping_var, fill=grouping_var))+
                geom_histogram(position="dodge")+ #identity is default (dodge is better?)
                labs(x=input$select_var_1,
                     fill=input$plot_grouping)
              
            }
            
            
            
          } else if (input$plot_type_1_var == "boxplot"){
            
            no_x_labels <- theme(axis.title.x=element_blank(),
                                 axis.text.x=element_blank(),
                                 axis.ticks.x=element_blank())
            
            # if (input$do_grouping != "Yes"){ 
            if (input$plot_grouping == "-NOTHING-"){ 
              
              plot <-
                ggplot(plot_data, aes(y=plot_var_1))+
                geom_boxplot(fill="steel blue")+
                no_x_labels+
                labs(y=input$select_var_1)
              
            } else { #some grouping
              
              plot <-
                ggplot(plot_data, aes(y=plot_var_1, group=grouping_var, fill=grouping_var))+
                geom_boxplot()+
                no_x_labels+
                labs(y=input$select_var_1,
                     fill=input$plot_grouping)
              
            }
            
          }
          
          
          #} else if (is.factor(plot_data$plot_var_1)){
        } else if (input$type_of_var_1 == "categorical/factor"){
          
          if (input$plot_grouping == "-NOTHING-"){
            
            plot <-
              ggplot(plot_data, aes(x=as.factor(plot_var_1)))+
              geom_bar(fill="steel blue")+
              labs(x=input$select_var_1)
            
          }  else {
            
            plot <-
              ggplot(plot_data, aes(x=as.factor(plot_var_1),
                                    group=grouping_var, fill=grouping_var))+
              geom_bar(position="dodge")+ #dodge for now
              labs(x=input$select_var_1,
                   fill=input$plot_grouping)
            
          }
          
        } 
        
        
        #### two variable plot ######
        
      } else if (input$number_vars == 2){
        
        req(input$select_var_2)
        req(input$type_of_var_2)
        req(input$plot_type_2_vars)
        
        
        #plot_var_2 <- plot_data[, input$select_var_2]
        names(plot_data)[names(plot_data) == input$select_var_2] <- "plot_var_2" #ggplot2 preferred
        
        
        ## scatterplot (two continuous variables)#######
        if ( (input$type_of_var_1 == "continuous" & input$type_of_var_2 == "continuous")){
          
          if (input$plot_grouping == "-NOTHING-"){
            
            plot <-
              ggplot(plot_data, aes(x=plot_var_1, y=plot_var_2))+
              geom_point()+
              geom_smooth()+
              labs(x=input$select_var_1,
                   y=input$select_var_2)
            
          } else {
            
            plot <-
              ggplot(plot_data, aes(x=plot_var_1, y=plot_var_2, 
                                    color=grouping_var, shape=grouping_var, 
                                    fill=grouping_var))+
              geom_point()+
              geom_smooth()+
              labs(x=input$select_var_1,
                   y=input$select_var_2,
                   color=input$plot_grouping,
                   shape=input$plot_grouping,
                   fill=input$plot_grouping)
            
          }
          
          
          
          #### boxplot (1 continuous, 1 "factor")  
        } else if ( (input$type_of_var_1 == "continuous" & input$type_of_var_2 == "categorical/factor") |
                    (input$type_of_var_1 == "categorical/factor" & input$type_of_var_2 == "continuous")) {
          
          if(input$type_of_var_1 == "continuous"){
            names(plot_data)[names(plot_data) == "plot_var_1"] <- "var_cont"
            var_cat_name <- input$select_var_1
            names(plot_data)[names(plot_data) == "plot_var_2"] <- "var_cat"
            var_cont_name <- input$select_var_2
          } else { #reverse
            names(plot_data)[names(plot_data) == "plot_var_2"] <- "var_cont"
            var_cat_name <- input$select_var_2
            names(plot_data)[names(plot_data) == "plot_var_1"] <- "var_cat"
            var_cont_name <- input$select_var_1
          }
          
          if (input$plot_grouping == "-NOTHING-"){
            
            plot <-
              ggplot(plot_data, aes(x=as.factor(var_cat), y=var_cont))+
              geom_boxplot(fill="steel blue")+
              labs(x=var_cat_name,
                   y=var_cont_name)
            
          } else {
            
            plot <-
              ggplot(plot_data, aes(x=as.factor(var_cat), y=var_cont,
                                    fill=grouping_var))+
              geom_boxplot()+
              labs(x=var_cat_name,
                   y=var_cont_name,
                   fill=input$plot_grouping)
            
          }
          
          
          
          ### 2 categorical variables (barplot) ###  
        } else if (input$type_of_var_1 == "categorical/factor" & input$type_of_var_2 == "categorical/factor"){
          
          #plot_1_var (and plot_1_var) not foundnot found (??)
          
          
          if (input$plot_grouping == "-NOTHING-"){
            
            ### looks nice, but not "consistent" with grouping scheme
            ### of other plots
            # plot <-
            #   ggplot(plot_data, aes(x=as.factor(plot_var_1), fill=as.factor(plot_var_2)))+
            #   geom_bar(position="dodge")+
            #   labs(x=input$select_var_1,
            #        fill=input$select_var_2)
            
            plot <-
              ggplot(plot_data, aes(x=as.factor(plot_var_1)))+
              geom_bar(position="dodge", fill="steelblue")+
              facet_wrap(~plot_var_2)+
              labs(x=input$select_var_1,
                   subtitle=paste0("grids are based on variable 2: ", input$select_var_2))
            
          } else {
            
            plot <-
              ggplot(plot_data, aes(x=as.factor(plot_var_1), fill=as.factor(grouping_var)))+
              geom_bar(position="dodge")+
              facet_wrap(~plot_var_2)+
              labs(x=input$select_var_1,
                   fill=input$plot_grouping,
                   subtitle=paste0("grids are based on variable: ", input$select_var_2))
            
          }
          
          
        }
        
        
        
        
        
      }
      
      #plot #not necessary if creating save thingy?
      
      
      #}) #if just plotting in shiny
      
      
    }
    
  })
  
  
  # show plot
  output$result_plot <- renderPlot({
    
    if (is.null(plotObject())){ #not necessary, but can keep
      return(NULL)
    } else {
      print(plotObject())
    }
    
    
  })
  
  
  #if dynamic size
  output$result_plot.ui <- renderUI({
    
    if(input$createPlot == "Yes"){
      
      #cannot directly send "plotObject" here. plotObject must be parsed through renderPlot first
      plotOutput("result_plot", width = paste0(input$plot_width, "px"), height = paste0(input$plot_height, "px"))
      
    } else {
      return(NULL)
    }
    
  })
  
  
  ### download plot ####
  
  # gets saved to downloads folder
  # NOTE! Must be run in browser (NOT IN Rstudio window thingy)
  output$downloadPlot <- downloadHandler(
    
    filename = function() { ## can add name here
      
      paste0(input$plot_name, ".", input$plot_format)
      
    }, 
    
    content = function(file) {
      
      px_to_cm <- function(px_size){
        
        cm_size <- px_size * 0.026458333
        
        return(cm_size)
      }
      
      ggsave(file, plot = plotObject(), device = input$plot_format,
             width = px_to_cm(input$plot_width), height = px_to_cm(input$plot_height),
             units = "cm")
      
      
    }
  )
  
  
  ############ Table details ###########
  
  ### update table variables###
  
  #this crashes everything after loading data (why?)
  observe({
    
    updateSelectInput(session,"table_variables",  ##I think observe, or updateSelectInput needs session...
                      label = "Which variables do you want to include?",
                      choices = c(names(filedata()))
                      )
  })
  
  ### update table grouping options###
  # table_group_choices <- reactive({
  #   
  #   req(input$table_variables)
  # 
  #   #table_group_choices <- names(filedata()[,input$table_variables])
  # 
  # })
  
  observe({
    
    updateSelectInput(session,"table_grouping",  ##I think observe, or updateSelectInput needs session...
                      label = "Variable to group by",
                      choices = c("-NOTHING-", input$table_variables),
                      selected = "-NOTHING-")
  })
  
  ##########################
  
  ######## Table output ######
  
  make_table <- reactive({
  #output$result_table <- gt::render_gt({
  #output$result_table <- renderTable({
    
    req(input$files_loaded)
    req(input$table_variables)
    req(input$table_grouping) #necessary, if no do_grouping
    
    if (input$files_loaded == "Yes"){
      
      table_data <- filedata()
      
      if (input$table_grouping == "-NOTHING-"){ #no grouping
        
        ##base R (test)
        # result_table <- summary(table_data) #this works, so input IS correct...??
        
        ##gsummary
        result_table <- gtsummary::tbl_summary(table_data[, input$table_variables]) %>%
          gtsummary::as_gt() #this is not necessary when running outside of shiny (hm..)
        
        
      
      } else { #some grouping var
        
        result_table <- gtsummary::tbl_summary(table_data[, input$table_variables], 
                                               by = input$table_grouping) %>%
          gtsummary::as_gt()
        
      }
      
      result_table #move down later
      
    }
    
    
  })
  
  
  output$result_table <- gt::render_gt({
    
    if (input$createTable == "Yes"){
      make_table()
    }
    
    
  })
  
  
  output$downloadTable <- downloadHandler(
    
    filename = function() { ## can add name here
      
      paste0(input$table_name, ".", input$table_format)
      
    }, 
    
    content = function(file) {
      
      gt::gtsave(file, data = make_table())

      
    }
  )
  
  ############################# Finished table output ############################


  
} # close the shinyServer
###################################################################################################

shinyApp(ui, server)
