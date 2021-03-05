#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(veccompare)
library(shinythemes)
library(DT)


button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;
/* Change the text size to 15 pixels. */
font-size: 15px;
}"

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  
  navbarPage("veccompare package", theme = shinytheme("lumen"),
                 tabPanel("2-way comparison", fluid = TRUE, icon = icon(""),
                          tags$style(button_color_css),
                                     sidebarLayout(
                                       sidebarPanel(
                                         
                                         titlePanel("Choose number of vectors to compare"),
                                         sliderInput("vec",
                                                     "Number of vectors:",
                                                     min = 2,
                                                     max = 6,
                                                     value = 3)
                                       ),# end sidebarPanel
                                       
                                       mainPanel(
                                         HTML(
                                           paste(
                                             h4("Here you can see the graphical and tabular output of the 2-way comparison. Input is a list wiht 6 different vectors. With the slider on the left you can choose how many vectors to compare. The plot matrix is showing the decimal percentage overlap"), h3("Graphical output")
                                           )
                                         ),
                                         plotOutput("vecPlot"),
                                         HTML(
                                           paste(
                                             h3("Tabular output")
                                           )
                                         ),
                                         tableOutput("vec2waytable"))
                                     )# end sidebarLayout
                                     ),# end tabPabel
             tabPanel("Network Graph", fluid = TRUE, icon = icon(""),
                      tags$style(button_color_css),
                      sidebarLayout(
                        sidebarPanel(
                          
                          titlePanel("Choose number of vectors to compare"),
                          sliderInput("vec3",
                                      "Number of vectors:",
                                      min = 2,
                                      max = 6,
                                      value = 3)
                        ),# end sidebarPanel
                        
                        mainPanel(HTML(
                          paste(
                            h4("It is possible to create a network graph of all two ways comparisons between vectors. Higher overlap is represented with darker lines. Maps are sized based on their relative numbers of items.")
                          )
                        ),
                        plotOutput("netgraph"))
                      )# end sidebarLayout
             ),# end tabPabel
             tabPanel("Venn-Diagram", fluid = TRUE, icon = icon(""),
                      tags$style(button_color_css),
                      sidebarLayout(
                        sidebarPanel(
                          
                          titlePanel("Choose number of vectors to compare"),
                          sliderInput("vec2",
                                      "Number of vectors:",
                                      min = 2,
                                      max = 5,
                                      value = 3)
                        ),# end sidebarPanel
                        
                        mainPanel(HTML(
                          paste(
                            h4("Here you can see the graphical output as a venn-diagram. Due to graphical limitations it is only possible to plot a 5-way comparison. The result list of the compare.vectors() function contains details on the individual overlaps of the vectors.")
                          )
                        ),
                        plotOutput("vennDiagram"))
                      )# end sidebarLayout
             )# end tabPabel
                 )# end navbarPage
)# end fluidPage
   
   

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # 2 way comparison plot---------------------------------------------------------------------#
   output$vecPlot <- renderPlot({
     vectors_to_use <- veccompare::example.vectors.list
     vectors_to_use <- vectors_to_use[1:input$vec]
     veccompare::summarize.two.way.comparisons.percentage.overlap(
       vectors_to_use,
       output_type = "matrix_plot"
     )
   })
   
   # table 2way comparison
   reactive_vec2waytable <- reactive({
     vectors_to_use <- veccompare::example.vectors.list
     vectors_to_use <- vectors_to_use[1:input$vec]
     
       veccompare::summarize.two.way.comparisons.percentage.overlap(
         vectors_to_use,
         output_type = "table",
         melt_table = TRUE
       )
     
     
   })
   output$vec2waytable <- renderTable({
     reactive_vec2waytable()
   })
   #---net graph ------------------------------------------------------------------------------#
   
   # netgraph plot---------------------------------------------------------------------#
   output$netgraph <- renderPlot({
     vectors_to_use <- veccompare::example.vectors.list
     vectors_to_use <- vectors_to_use[1:input$vec3]
     
     veccompare::summarize.two.way.comparisons.percentage.overlap(
       vectors_to_use,
       output_type = "network_graph",
       network_graph_minimum = .2
     )
   })
   # venn diagram -----------------------------------------------------------------------------#
   
   venn_reactive <- reactive({
     vectors_to_use <- veccompare::example.vectors.list
     vectors_to_use <- vectors_to_use[1:input$vec2]
     
     veccompare::compare.vectors.and.return.text.analysis.of.overlap(
       vectors_to_use,
       degrees_of_comparison_to_include = 2:length(vectors_to_use),
       cat_immediately = TRUE,
       draw_venn_diagrams = TRUE,
       viewport_npc_width_height_for_images = 0.65, # If venn diagrams are getting cut off, this number can be lowered (for example, to 0.7)
       base_heading_level_to_use = 1
     )
   })
   
   output$vennDiagram <- renderPlot({
     venn_reactive()
     })
  #-----------------------------------------------------------------------------------------------# 
   # Table output venn diagram
  # output$vennTable <- DT::renderDataTable({
  #   DT::datatable(over_norm_meth_customer(),
  #                 options = list(scrollX=TRUE, scrollY=300, searching = FALSE, paging=FALSE)
  #   )
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)

