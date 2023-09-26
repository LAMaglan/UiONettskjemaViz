#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

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
          filedata <- dplyr::full_join(file_1(), file_2(), by = var_1_join)
        } else if (input$join_1_type == "left"){
          filedata <- dplyr::left_join(file_1(), file_2(), by = var_1_join)
        } else if (input$join_1_type == "right"){
          filedata <- dplyr::right_join(file_1(), file_2(), by = var_1_join)
        } else if (input$join_1_type == "inner"){
          filedata <- dplyr::inner_join(file_1(), file_2(), by = var_1_join)
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
                ggplot2::ggplot(plot_data, aes(x=plot_var_1))+
                  ggplot2::geom_histogram(fill="steel blue")+
                  ggplot2::labs(x=input$select_var_1)

            } else {

              plot <-
                ggplot2::ggplot(plot_data, aes(x=plot_var_1, group=grouping_var, fill=grouping_var))+
                  ggplot2::geom_histogram(position="dodge")+ #identity is default (dodge is better?)
                  ggplot2::labs(x=input$select_var_1,
                                fill=input$plot_grouping)

            }



          } else if (input$plot_type_1_var == "boxplot"){

            no_x_labels <- ggplot2::theme(axis.title.x=element_blank(),
                                          axis.text.x=element_blank(),
                                          axis.ticks.x=element_blank())

            # if (input$do_grouping != "Yes"){
            if (input$plot_grouping == "-NOTHING-"){

              plot <-
                ggplot2::ggplot(plot_data, aes(y=plot_var_1))+
                  ggplot2::geom_boxplot(fill="steel blue")+
                  no_x_labels+
                  ggplot2::labs(y=input$select_var_1)

            } else { #some grouping

              plot <-
                ggplot2::ggplot(plot_data, aes(y=plot_var_1, group=grouping_var, fill=grouping_var))+
                  ggplot2::geom_boxplot()+
                  no_x_labels+
                  ggplot2::labs(y=input$select_var_1,
                                fill=input$plot_grouping)

            }

          }


          #} else if (is.factor(plot_data$plot_var_1)){
        } else if (input$type_of_var_1 == "categorical/factor"){

          if (input$plot_grouping == "-NOTHING-"){

            plot <-
              ggplot2::ggplot(plot_data, aes(x=as.factor(plot_var_1)))+
                ggplot2::geom_bar(fill="steel blue")+
                ggplot2::labs(x=input$select_var_1)

          }  else {

            plot <-
              ggplot2::ggplot(plot_data, aes(x=as.factor(plot_var_1),
                                             group=grouping_var, fill=grouping_var))+
                ggplot2::geom_bar(position="dodge")+ #dodge for now
                ggplot2::labs(x=input$select_var_1,
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
              ggplot2::ggplot(plot_data, aes(x=plot_var_1, y=plot_var_2))+
                ggplot2::geom_point()+
                ggplot2::geom_smooth()+
                ggplot2::labs(x=input$select_var_1,
                              y=input$select_var_2)

          } else {

            plot <-
              ggplot2::ggplot(plot_data, aes(x=plot_var_1, y=plot_var_2,
                                             color=grouping_var, shape=grouping_var,
                                             fill=grouping_var))+
                ggplot2::geom_point()+
                ggplot2::geom_smooth()+
                ggplot2::labs(x=input$select_var_1,
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
              ggplot2::ggplot(plot_data, aes(x=as.factor(var_cat), y=var_cont))+
                ggplot2::geom_boxplot(fill="steel blue")+
                ggplot2::labs(x=var_cat_name,
                              y=var_cont_name)

          } else {

            plot <-
              ggplot2::ggplot(plot_data, aes(x=as.factor(var_cat), y=var_cont,
                                             fill=grouping_var))+
                ggplot2::geom_boxplot()+
                ggplot2::labs(x=var_cat_name,
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
              ggplot2::ggplot(plot_data, aes(x=as.factor(plot_var_1)))+
                ggplot2::geom_bar(position="dodge", fill="steelblue")+
                ggplot2::facet_wrap(~plot_var_2)+
                ggplot2::labs(x=input$select_var_1,
                              subtitle=paste0("grids are based on variable 2: ", input$select_var_2))

          } else {

            plot <-
              ggplot2::ggplot(plot_data, aes(x=as.factor(plot_var_1), fill=as.factor(grouping_var)))+
                ggplot2::geom_bar(position="dodge")+
                ggplot2::facet_wrap(~plot_var_2)+
                ggplot2::labs(x=input$select_var_1,
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

      ggplot2::ggsave(file, plot = plotObject(), device = input$plot_format,
                      width = px_to_cm(input$plot_width), height = px_to_cm(input$plot_height),
                      units = "cm")


    }
  )

}
