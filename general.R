#!/usr/bin/env Rscript

#    Assignment exploring tool
#    Copyright (C) 2015  Roman Klapaukh
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.


library(magrittr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(data.table)
library(shiny)
library(argparser)


p = arg.parser("A R application for examining the effect of assignments on the pass rate of the course") %>%
  add.argument("--aregex", default="^[aA][0-9]*$", help="Regex to match assignment columns") %>%
  add.argument("--input",                          help="input file")  %>%
  add.argument("--max",    default=20,             help="maximum mark") %>%
  add.argument("--pass",   default=8, help="lowest mark that is a pass") %>%
  add.argument("--poor",   default=4, help="highest mark that is a poor effort") 

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
passGrade = c("A+","A","A-","B+","B","B-","C+","C","C-")
failGrade = c("D","E")
results %<>% filter(Final %in% c(passGrade,failGrade))


#Melt the assignments, and make any NAs 0 (for consistency between courses and
#data sets)
assignmentColumnNames = results %>% colnames %>% grep(args$aregex,.,value=T)
results %<>% melt(measure.var=assignmentColumnNames,value.name="Mark",variable.name="Assignment")
results$Mark[is.na(results$Mark)] = 0


if(any(results$Mark > args$max)) {
  cat(sprintf("Found marks greater than the expected max (%d) -- automatically adjusting max.\n", max(results$Mark)))
  cat("Please restart the system with the correct value.\n")
  args$max = max(results$Mark)
}

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
      sliderInput("poorVal", "Poor cut off", min=1, max=args$max, value=args$poor, step=1),                      
      sliderInput("passVal", "Pass mark", min=1, max=args$max, value=args$pass, step=1),
      p("Summary data"),
      dataTableOutput("attemptSummary")
                              )

shinyApp(ui =        fluidPage(includeCSS("style.css"),
                               titlePanel("Assignment Monitoring"),sidebarLayout(
  generalSidebar, mainPanel(
  tabsetPanel(
                   tabPanel("Basic Data",
                            h2("Student attempts at assignments"), 
                            p("The following plot shows the number of students that did each assignment to each level of completion. The levels are as follows:"),
                            HTML("<ul><li>zeros: 0 mark</li><li>poor: <= the poor grade</li><li>failed: < the pass grade </li><li>completed: acheived the pass grade or better</li></ul>"),
                            plotOutput("nAttempts"),
                            p("We can also look at the number of students who attempted each assignment"),
                            plotOutput("nAssign"),
                            "Overall ",
                            textOutput("percentAttempts", inline=TRUE),
                            "% of students submitted all of their assignments."
                              ),
                   tabPanel("Total Attempt Effect",
                        h2("Group assignment effects"),
                        p("Your chance of passing based on number of assignment submitted"),
                        plotOutput("missedAss")
                           ),
                   tabPanel("Individual Assignment Effect",
                            h2("Specific assignment effects"),
                            p("Your chance of passing based on your performance at a specific assignment"),
                            plotOutput("specificMissChance"),
                            p("In contrast we can look at it based on the first assignment skipped"),
                            plotOutput("firstMissChance")
                           )
                   )))),
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

          output$missedAss <- renderPlot({
results %>% 
  group_by(ID) %>%
   summarise(Skipped = sum(Mark == 0),
            poor  = sum(Mark <= poor()),
            failed = sum(Mark < pass()),
            completed = sum(Mark >= pass()),
            passed = unique(Final %in% passGrade))%>%
   melt(id.vars=c("ID","passed"),value.name="num",variable.name="attempt") %>% 
   group_by(attempt,num) %>%
   summarise(total = n(),
             nPassed = sum(passed == T),
             nFailed = sum(passed == F)) %>%
   rowwise() %>%
   mutate(
           bestimate   = binom.test(nPassed, total,conf.level=0.8) %>% extract2("estimate"),
           bconfLower  = binom.test(nPassed, total,conf.level=0.8) %>% extract2("conf.int") %>% head(1),
           bconfUpper  = binom.test(nPassed, total,conf.level=0.8) %>% extract2("conf.int") %>% as.numeric %>% last) %>%
  ggplot(aes(x=num,y=bestimate,colour=attempt,fill=attempt,ymin=bconfLower,ymax=bconfUpper,label=total)) + 
  geom_point() + geom_line() + geom_ribbon(alpha=0.5) + geom_text(aes(y=bestimate - 0.1),color="black") +
  scale_x_continuous(breaks=0:10) + xlab("Number of Assignments") +
  ylab("Probability of passing") + facet_wrap(~attempt)
          })
           

                output$specificMissChance <- renderPlot({
results %>% 
 rowwise() %>%
 mutate(passed = Final %in% passGrade) %>% 
 ungroup %>%
  group_by(Assignment, passed) %>%
   summarise(Skipped = sum(Mark == 0),
            poor  = sum(Mark <= poor()),
            failed = sum(Mark < pass()),
            completed = sum(Mark >= pass())
            ) %>% 
   melt(id.vars=c("Assignment","passed"),value.name="num",variable.name="attempt") %>% 
   group_by(Assignment,attempt) %>% 
   summarise( total = sum(num),
              nPassed = sum(num[passed==TRUE]),
              nFailed = sum(num[passed==F])
              ) %>%
   ungroup %>%
   mutate( pPass = nPassed / total) %>%
   ggplot(aes(Assignment,pPass,colour=attempt,group=attempt,label=total)) + 
   geom_point() + geom_line() + geom_text(aes(y=pPass-0.1),colour="black") + 
   facet_wrap(~attempt) + xlab("Attempt at specific assignment") + 
   ylab("Probability of passing the course")
                })



          firstMissData <- reactive({
          results %>% 
                group_by(ID) %>%
                  summarise(
                            passed = head(Final %in% passGrade,1),
                            skipped = ifelse(any(Mark == 0), Assignment[min(which(Mark == 0))], as.integer(NA)),
                            poor    = ifelse(any(Mark <= poor()), Assignment[min(which(Mark <= poor() ))], as.integer(NA)),
                            failed  = ifelse(any(Mark < pass()), Assignment[min(which(Mark <  pass() ))], as.integer(NA)),
                            completed  = ifelse(any(Mark >= pass()), Assignment[min(which(Mark >=  pass() ))], as.integer(NA))
                            ) %>% 
                melt(id.vars=c("ID","passed"), value.name="assignment", variable.name="attempt") %>%
                filter(!is.na(assignment)) %>%
                group_by(attempt,assignment) %>%
                summarise(
                          total = n(),
                          nPassed = sum(passed==T)
                          ) %>%
                mutate(pPass = nPassed / total,
                       assignmentNum = gsub("[^0-9]*","",assignment) %>% as.integer)  

          })

        output$firstMissChance <- renderPlot({
                ggplot(firstMissData(), 
                       aes(x=factor(assignment),y=pPass,colour=attempt,label=total,group=attempt)) + 
                        geom_point() + geom_line() + 
                        geom_text(aes(y=pPass-0.1),colour="black") + 
                        facet_wrap(~attempt) + xlab("First assignment to be ...") +
                        ylab("Probability of passing") 
                        
                })

            })
