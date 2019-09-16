
library(shiny)
library(shinydashboard)
library(readxl)
library(ggplot2)
library(dplyr)
library(plotly)
library(treemap)
library(grid)


types_of_courses = readxl::read_excel("data.xlsx")
training_courses = readxl::read_excel("data.xlsx", sheet = "Training_Courses")

slack = readxl::read_excel("data.xlsx", sheet = "Slack")
col_choice = names(slack)

policy = readxl::read_excel("data.xlsx", sheet = "GSS_Guidance_Views")
policy$time_on_page <- as.character(policy$time_on_page)

consultancy = readxl::read_excel("data.xlsx", sheet = "Consultancy")
gsshelp = readxl::read_excel("data.xlsx", sheet = "GSSHelp_Emails")

awards = readxl::read_excel("data.xlsx", sheet = "Sharing_Awards")
engagement_events = readxl::read_excel("data.xlsx", sheet = "Engagement_Attendence")

gss_website <- readxl::read_excel("data.xlsx", sheet = "GSS_website")

gss_pageviews <- readxl::read_excel("data.xlsx", sheet = "GSS_pageviews")
colnames(gss_pageviews) <- c("Day_Index", "Page_Views") 
gss_pageviews <-  head(gss_pageviews, -1)

ui <- dashboardPage(skin = "blue",
  
  dashboardHeader(title = "BPI Dashboard", 
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "GPT",
                                 message = "Harminsoation Meeting this 12th"
                               ),
                               messageItem(
                                 from = "Marina",
                                 message = "How do I register?",
                                 icon = icon("question")
                               ),
                               messageItem(
                                 from = "RAP",
                                 message = "RAP meeting is next week, register now!",
                                 icon = icon("life-ring")
                               )
                  ),
  
  dropdownMenu(type = "notifications",
               notificationItem(
                 text = "New Slack tab",
                 icon("users")
               ),
               notificationItem(
                 text = "SHEL project delieverd on time and with great sucess!",
                 icon("lightbulb"),
                 status = "success"
               )
  ) ,
  
  dropdownMenu(type = "tasks", badgeStatus = "success",
                    taskItem(value = 90, color = "green",
                             "BPI Dashboard"
                    ),
                    taskItem(value = 17, color = "aqua",
                             "Project X"
                    ),
                    taskItem(value = 75, color = "yellow",
                             "Server deployment for SHEL"
                    ),
                    taskItem(value = 66, color = "red",
                             "Overall project"
                    )
  )), # end of daskboard header
  
  
  dashboardSidebar(sidebarMenu(
    menuItem("Building Capability", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Slack", tabName = "Slack", icon = icon("comment-dots")),
    menuItem("Consultancy", tabName = "consultancy", icon = icon("hands-helping")),
    menuItem("Sharing", tabName = "sharing", icon = icon("hands-helping")),
    menuItem("GSS", tabName = "GSS", icon =  icon("chalkboard-teacher")),
    menuItem(             radioButtons(inputId= "days", label = "", 
                                choices = list("Full" = 1, 
                                               "Weekdays" = 2), 
                                selected = 1))
    ) # End of sidebarMenu
  ), # End of dashboardSidebar
  
  dashboardBody( tags$head(tags$link(rel="stylesheet", type="text/css", href="style.css")),
    tabItems(
      tabItem(tabName = "dashboard",
              
                fluidRow(
                  box(width = 6, 
                      title = "Training",
                      solidHeader = TRUE,
                      status = "primary",
                    plotlyOutput("distPlot")
                    ), # End of box
                 
                  box(width = 6,
                      title = "Number Trained Across GSS",
                      solidHeader = TRUE,
                      status = "primary",
                      plotOutput("courses_treemap"))
                  ), # end first fluidrow
              fluidRow(
                box(width = 6,
                  title = "Controls",
                  selectInput("team_selection", "Select team to filter data", choices = c("Please select a team",c(unique(training_courses$topic_area))))
                ) # End of box
                
              ) # end second fluid row
                  
      ), # End of Building Capability,
      
      tabItem(tabName = "Slack",

              fluidRow(
                box(title = "Slack",
                    solidHeader = TRUE,
                    status = "primary",
                  plotlyOutput("slack")
                ),
                
                box(status = "danger",
                  h3("Choose between displaying the total membership and the messages posted on Slack."),
                  radioButtons("input1", "Choose", choices = col_choice[-1])
                ),
                
                box(status = "info", "There has been lots of engagement with the strategies as tracked by views and time spent.", br(),  
                    tableOutput("policy_table") )
              )
            ), # End of Slack
      tabItem(tabName = "consultancy",
              h2("Consultancy"),
              fluidRow(
                box(title = "GSSHelp Email Requests",
                    solidHeader = TRUE,
                    status = "primary",
                    plotlyOutput("ggshelp_plot", height = 300)
                  ),# End of GSSHelp Email Request Plot box
                box(status = "danger",
                    title = "Graph Controls",
                    sliderInput("input2", "Choose data to display:", min = 1, max = nrow(gsshelp), value = c(1,nrow(gsshelp)), step = 1)
                ), # End of graph controls box
                box(title = "Ongoing Consultancy",
                    status = "info",
                    solidHeader = TRUE,
                  tableOutput("consultancy_table")
                ) # End of consultancy table box
              ) # end of fluidRow
              
      ), # End of cosultancy tab
      
      tabItem(
        tabName = "sharing",
              h2("Sharing"),
              fluidRow(
              box(
                  title = "GSS Awards",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  tags$head(tags$style(HTML("
                                #final_text {
                                            text-align: center;
                                            }
                                            div.box-header {
                                            text-align: center;
                                            }
                                            "))),
                  
                  box(
                    title = "Nominations",
                    status = "info",
                    solidHeader = TRUE,
                    width = 4,
                    align = "center",
                    h1(textOutput("nominations"))
                    ), # End of Nominations box
                  
                  box(
                    title = "Shortlisted",
                    status = "info",
                    solidHeader = TRUE,
                    width = 4,
                    align = "center",
                    h1(textOutput("shortlist"))
                  ), # End of Shortlisted box
                  
                  box(
                    #tag$style(HTML("background-color:#FFD700")),
                    align = "center",
                    title = "Winners",
                    status = "info",
                    solidHeader = TRUE,
                    width = 4,
                    color = "#FFD700",
                    h1(textOutput("awarded"))
                  ) # End of Winners box
                  
                )# End of box for GSS Awards

              ), # end of fluid row
        fluidRow(
             box(width = 12,
               title = "BPI Engagement Events",
               status = "info",
               solidHeader = TRUE,
               align = "center",
               tableOutput("engagement_events")
             )
        )
      ), # End of sharing tab

      tabItem(tabName = "GSS", 
              fluidRow(
              box(width = 12,
                title = "GSS website Views",
                  solidHeader = TRUE,
                  status = "primary",
                plotlyOutput("gss_views"))
      ), # end fluidrow
      fluidRow(
        box( width = 12,
          dataTableOutput("gss_website"))
      ) # end fluidrow
      ) # end gss        
              
    ) # End of tabItems
      
  )# End of dashboardBody

  
) # end of dashboardPage


server <- function(input, output) { 
  
  output$distPlot <- renderPlotly({
    
    if(input$team_selection == "Please select a team"){
      #df <- types_of_courses %>%
      #  group_by(team) %>%
      #  summarise(num = sum(num))
      
      df <- training_courses %>%
        group_by(topic_area) %>%
        summarise(num = n())

      ggplot(data = df, aes(x = topic_area, y = num, fill = topic_area))+ geom_col() +  theme_bw() + xlab("Course Topic Area") + ylab("") + 
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position="none")
      
    } else {
      df <- training_courses %>%
        filter(topic_area == input$team_selection)
      
      ggplot(df, aes(x = event_name, y = number_trained, fill = event_name )) + geom_col() +  theme_bw() + xlab("Course Title") + ylab("") + 
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position="none")
    }
    
  })
  
  
  
  output$courses_treemap <- renderPlot({
    
    df <- training_courses %>%
      group_by(where_delivered) %>%
      summarise(num = sum(number_trained))

      par(mar=c(0,0,0,0), xaxs='i', yaxs='i') 
      plot(c(0,1), c(0,1),axes=F, col="white")
      
      temp=df
      .tm <<- treemap(temp, 
                      index="where_delivered", 
                      vSize="num", 
                      vColor="num",
                      type="value",
                      title = "",
                      palette="Blues",
                      border.col ="white",
                      position.legend="right",
                      fontsize.labels = 16,
                      title.legend="")
  })
  
 
  output$slack <- renderPlotly({
    
    ggplot(data = slack, aes_string(x = "name", y = input$input1, fill = "name"))+ geom_col() +  theme_bw() + xlab("Slack Channels") + ylab("") + 
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position="none")
    
  })
  

  output$policy_table <- renderTable (digits = 0, {
    names(policy) <- c("Policy", "Views", "Time_on_Page")
    policy
    })
  

  output$ggshelp_plot <- renderPlotly({
    
    df <- gsshelp[order(gsshelp$requests, decreasing = TRUE),]
    df <- df[input$input2[1]:input$input2[2],]
    
    ggplot(data = df, aes(x = reorder(groups, requests, desc), y = requests, fill = groups))+ geom_col() +  theme_bw() + xlab("Email Category") + ylab("") + 
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position="none")
    #return(p)

  })
  
  output$consultancy_table <- renderTable ({
    names(consultancy) <- c("Department", "Consultancy Type")
    consultancy
  }) # End of consultancy table render function
  
  output$nominations <- renderText({
    nominations <- awards %>%
      filter(type == "nominations")
    return(as.character(nominations[["count"]]))
  }) # End of nominations text render function
  
  output$shortlist <- renderText({
    shortlisted <- awards %>%
      filter(type == "shortlisted")
    return(as.character(shortlisted[["count"]]))
  }) # End of shortlist text render function
  
  output$awarded <- renderText({
    awarded <- awards %>%
      filter(type == "awarded")
    return(as.character(awarded[["count"]]))
  }) # End of awarded text render function
  

  output$engagement_events <- renderTable({
    
    df <- engagement_events[order(engagement_events$count, decreasing = TRUE),]
    df$count <- NULL
    return(df)
  })

  output$gss_views <- renderPlotly({
    
    if(input$days == 1) {
      gss_pageviews = gss_pageviews
    } else {
      gss_pageviews = gss_pageviews[which(weekdays(as.Date(gss_pageviews$Day_Index, format = "%m/%d/%Y"))
                                 %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday')), ]
    }
    
    ggplot(gss_pageviews, aes(x = Day_Index, y= Page_Views)) + geom_line() + geom_point() + theme_bw() + xlab("GSS Views") + ylab("") + 
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position="none")
    
  })
  
  output$gss_website <- renderDataTable({
        gss_website
   
  })
  

} # end of server


shinyApp(ui, server)