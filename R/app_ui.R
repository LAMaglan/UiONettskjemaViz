#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

    navbarPage("UiO Nettskjema visualization tool",

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
               #########################
    )

  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "UiONettskjemaViz"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
