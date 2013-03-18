library(shiny)


shinyUI(pageWithSidebar(
  # Application title
  headerPanel("What do p-values look like?"),

  # Sidebar with controls to select the variable to plot against mpg
  sidebarPanel(
    
    #max n must be <1000
    sliderInput("n", "Sample Size (n):", 
                min=0, max=500, value=100,step=1),
    
    numericInput("logp", "p-value=10^", -2,min=-10,max=-.2,step=.2),
    
    checkboxInput("nulldist", "Generate from an actual null", FALSE),
        
    checkboxInput("bestFit", "Best Fit Line", FALSE),
    checkboxInput("lowess", "Lowess", FALSE),
    numericInput("XEseed", "Change seed for X & e", 312,min=0,max=100000,step=1)
    
    
  ),


 mainPanel(
    h3(textOutput("pval")),
    h3(textOutput("test")),
    plotOutput("outplot"),
    h3(textOutput("formula")),
    h3(textOutput("genParams")),
    h3(textOutput("fitParams")),
    tableOutput('detailTable')
  )
))