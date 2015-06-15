#!/usr/bin/env Rscript


library(magrittr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(data.table)
library(shiny)
library(argparser)


p = arg.parser("A R application for examining the effect of assignments on the pass rate of the course") %>%
  add.argument("--input", help="input file")  %>%
  add.argument("--max", default=20, help="maximum mark") %>%
  add.argument("--poor", default=4, help="highest mark that is a poor effort") %>%
  add.argument("--pass", default=8, help="lowest mark that is a pass") 

args = parse.args(p, commandArgs())

if(is.na(args$input) | args$help){
  print(p)
  quit("no")
}

results = tryCatch(fread(args$input), 
                   warning = function(x) {print(x) ; return(NULL);} , 
                   error = function(x) {print(x);  return(NULL);} )

# If the input file doesn't exist die
if(is.null(results)){
  quit("no")
}

#Remove any unusual grades
pass = c("A+","A","A-","B+","B","B-","C+","C","C-")
fail = c("D","E")
results %<>% filter(Final %in% c(pass,fail))


#Melt the assignments, and make any NAs 0 (for consistency between courses and
#data sets)
results %<>% melt(measure.var=paste0("A",1:10),value.name="Mark",variable.name="Assignment")
results$Mark[is.na(results$Mark)] = 0


#We want to know, for each assignment how many students actually did them. 
#We look at three categories. Assignments are marked  
#
#   * zeros: students who got exactly zero. This is either that they did not
#     submit, were late, or did appauling.
#   * poor: these are students who got <= poor
#   * failed: these are all the students who failed the assessment
#   * passed: these are all the students who passes the assessment
#   * total: The total number of students in the course



generalSidebar = sidebarPanel(h3("Settings and summary"),
      sliderInput("poorVal", "Poor cut off", min=0, max=args$max, value=args$poor, step=1),                      
      sliderInput("passVal", "Pass mark", min=0, max=args$max, value=args$pass, step=1),
      p("Summary data"),
      dataTableOutput("attemptSummary")      
                              )

shinyApp(ui =         navbarPage("Assignment Effect Explorer", 
                   id="navigation",
                   tabPanel("Basic Data",
                       sidebarLayout(generalSidebar,
                                     mainPanel(
                            h2("Student attempts at assignments"), 
                            p("The following plot shows the number of students that did each assignment to each level of completion. The levels are as follows:"),
                            HTML("<ul><li>zeros: 0 mark</li><li>poor: <= the poor grade</li><li>failed: < the pass grade </li><li>completed: acheived the pass grade or better</li></ul>"),
                            plotOutput("nAttempts"),
                            p("We can also look at the number of students who attempted each assignment"),
                            plotOutput("nAssign"),
                            "Overall ",
                            textOutput("percentAttempts", inline=TRUE),
                            "% of students submitted all of their assignments."
                              )
                                     )),
                   tabPanel("Total Attempt Effect"
                           ),
                   tabPanel("Individual Assignment Effect"
                           )
                   ),
server = function(input,output,session){

        poor <- reactive({input$poorVal})
        pass <- reactive({input$passVal})

assignmentCompletion <- reactive( {results %>%
  group_by(Assignment) %>%
  summarise(zeros = sum(Mark == 0),
            poor  = sum(Mark <= poor()),
            failed = sum(Mark < pass()),
            completed = sum(Mark >= pass()),
            total = n())
                   })



           output$nAttempts <- renderPlot({
              assignmentCompletion() %>%
                melt(id.vars=c("Assignment","total"),
                     variable.name="Result",value.name="NumStudents") %>%
                ggplot(aes(Assignment,NumStudents,colour=Result,group=Result,shape=Result)) + geom_point() + 
                geom_line() + ylab("Number of Students")
           })

          output$nAssign <- renderPlot({
            results %>% 
              group_by(ID) %>% 
              summarise(Skipped = sum(Mark == 0))%>%
              ggplot(aes(x=factor(Skipped))) + geom_bar() + xlab("Number of assignments skipped") + ylab("Number of Students")
          })

          output$percentAttempts <- renderText({
            results %>% 
              group_by(ID) %>% 
              summarise(Skipped = sum(Mark == 0))%>%
              group_by(Skipped) %>%
              summarise(students = n()) %>% 
              mutate(percent = 100 * students / sum(students)) %>%
              filter(Skipped == 0) %>% 
              extract2("percent") %>%
              round(1)
          })

          output$attemptSummary <- renderDataTable({

                 assignmentCompletion() %>% select(-total)

          })
            
            })


#
#```{r fig.cap="Passing the course based on number of missed assignments"}
#results %>% 
#  group_by(ID) %>% 
#  summarise(Skipped = sum(Mark == 0),
#            passed = unique(Final %in% pass))%>%
#  ggplot(aes(x=factor(Skipped),fill=passed)) + geom_bar(position="dodge") + xlab("Number of assignments skipped")
#```
#
#
#We can then have a look at how the number of skipped assignments affects your 
#probability of passing.
#
#```{r fig.cap="Probability of passing based on assignment attempts"}
#personalCompletion = results %>% 
#  group_by(ID) %>% 
#  summarise(skipped = sum(Mark == 0),
#            poor  = sum(Mark <= 4),
#            failed = sum(Mark < 8),
#            completed = sum(Mark >= 8),
#            passedCourse = unique(Final %in% pass)) 
#
#personalCompletion %>% 
#  group_by(skipped) %>% 
#  summarise( pPass = sum(passedCourse) / length(passedCourse),students=n()) %>%
#  ggplot(aes(skipped,pPass,label=students)) + geom_point() + geom_line() + 
#        scale_x_continuous(breaks=0:10) + geom_text(aes(y=pPass+0.05))
#  
#
#personalCompletion %>% 
#  group_by(poor) %>% 
#  summarise( pPass = sum(passedCourse) / length(passedCourse),students=n()) %>%
#  ggplot(aes(poor,pPass,label=students)) + geom_point() + geom_line() + 
#        scale_x_continuous(breaks=0:10) + geom_text(aes(y=pPass+0.05))
#
#
#personalCompletion %>% 
#  group_by(failed) %>% 
#  summarise( pPass = sum(passedCourse) / length(passedCourse),students=n()) %>%
#  ggplot(aes(failed,pPass,label=students)) + geom_point() + geom_line() + 
#        scale_x_continuous(breaks=0:10) + geom_text(aes(y=pPass+0.05))
#
#
#personalCompletion %>% 
#  group_by(completed) %>% 
#  summarise( pPass = sum(passedCourse) / length(passedCourse),students=n()) %>%
#  ggplot(aes(completed,pPass,label=students)) + geom_point() + geom_line() +
#         scale_x_continuous(breaks=0:10) + geom_text(aes(y=pPass+0.05))
#
#```
#
#This suggests that the number of assignments missed or done poorly is an indication that
#the student is at risk of failure. 
#
#
#Lets have a look at the effect of specific assignments.
#
#```{r fig.cap="Probability of passing the course given skipping a specific assignment"}
#results %>% 
#  group_by(Assignment) %>%
#  summarise(skipped = sum(Mark == 0),
#            pPass   = sum(Mark == 0 & Final %in% pass)/ sum(Mark == 0)
#           ) %>%
#  ggplot(aes(Assignment,pPass,group=factor(1),label=skipped)) + geom_point() + geom_line() + geom_text(aes(y=pPass+0.01))
#
#```
#
#Skipping any specific assignment does seem to matter. How about which is your
#fisrt assignment to skip.
#
#```{r fig.cap="Success based on first assignment skipped"}
#results %>% 
#  group_by(ID) %>%
#  summarise( firstSkipped = gsub("A","",Assignment[min(which(Mark == 0))]) %>%
#            as.integer,
#             passed = unique(Final %in% pass)) %>%
#  group_by(firstSkipped) %>%
#  summarise( pPass = sum(passed) / length(passed),
#             students = n()) %>%
#  filter(!is.na(firstSkipped)) %>%
#  ggplot(aes(firstSkipped, pPass,colour=students,label=students)) + geom_point(size=10) + geom_line(colour="black") + scale_x_continuous(breaks=0:10) + geom_text(aes(y=pPass+0.05),colour="black")
#```
#
#This is about skipping assignments, lets try look at failing instead. 
#
#
#```{r fig.cap="Probability of passing the course given failing a specific assignment"}
#results %>% 
#  group_by(Assignment) %>%
#  summarise(skipped = sum(Mark <8),
#            pPass   = sum(Mark < 8 & Final %in% pass)/ sum(Mark <8)
#            ) %>%
#  ggplot(aes(Assignment,pPass,group=factor(1),label=skipped)) + geom_point() + geom_line()  +
#  xlab("Failing assignment X") + geom_text(aes(y=pPass+0.01))
#```
#
#```{r fig.cap="Success based on first assignment failed"}
#results %>% 
#  group_by(ID) %>%
#  summarise( firstSkipped = gsub("A","",Assignment[min(which(Mark < 8))]) %>%
#            as.integer,
#             passed = unique(Final %in% pass)) %>%
#  group_by(firstSkipped) %>%
#  summarise( pPass = sum(passed) / length(passed),
#             students = n()) %>%
#  filter(!is.na(firstSkipped)) %>%
#  ggplot(aes(firstSkipped, pPass,colour=students,label=students)) + geom_point(size=10) + geom_line(colour="black") + scale_x_continuous(breaks=0:10) + geom_text(aes(y=pPass+0.05),colour="black") + xlab("First assignment failed")
#```
#
