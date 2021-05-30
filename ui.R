library("ggplot2")
library(gridExtra)
library(data.table)
library(xlsx) 
library(DT)
library(shinyWidgets)
library(shinyBS)
library(shinythemes)
library(shinyjs)
 ui <- fluidPage(
#   hidden(
#     div(
#       id = "pp"
#       #p("")
#     )
#   ),
  theme = shinytheme("united"),
  useShinyjs(),
  
#tags$head(HTML("<title>PSUMAnT: PolySubstance Use Mining for Associations and Transitions</title>")),
 # navbarPage(#h1(HTML("  PSUMAnT: PolySubstance Use Mining for Associations and Transitions ")),
             #  title=h3(HTML("  &nbsp;&nbsp;&nbsp &nbsp;&nbsp PSUMAnT: PolySubstance Use Mining for Associations and Transitions")), # , <br/> Cite this as: <I/>Identifying Patterns of Association and Transition in the Use of Addictive Substances over Five Decades in the United States</I>, S. Ray, M. Aruru, and S. Pyne, medRxiv 2021.03.11.21253386; doi: https://doi.org/10.1101/2021.03.11.21253386")),
               # h4(HTML("Cite as: <I/>Identifying Patterns of Association and Transition in the Use of Addictive Substances over Five Decades in the United States.</I>
#Sumanta Ray, Meghana Aruru and Saumyadipta Pyne, medRxiv 2021.03.11.21253386; doi: https://doi.org/10.1101/2021.03.11.21253386")),
#img(src="PS_logo.gif", style="float:left; padding-right:25px"),
#titlePanel("PSUMAnT: PolySubstance Use Mining for Associations and Transitions",img(src = "C:/Users/HP/Downloads/www/PS_logo.png", height = 50, width = 100)),
titlePanel(title=div(img(src='ps_logo1.png',style="margin-top: -14px; padding-right:10px;padding-bottom:10px", height = 60)), h3(HTML("PSUMAnT: PolySubstance Use Mining for Associations and Transitions"))),

#title = div(img(src='ps_logo1.png',style="margin-middle: -14px; padding-right:80px;padding-bottom:1px", height = 60)),#,h3(HTML("  &nbsp;&nbsp;&nbsp &nbsp;&nbsp PSUMAnT: PolySubstance Use Mining for Associations and Transitions"))), 
#h3(HTML("  &nbsp;&nbsp;&nbsp &nbsp;&nbsp PSUMAnT: PolySubstance Use Mining for Associations and Transitions")),

sidebarLayout(
  
  # Sidebar panel for inputs ----
  sidebarPanel(            
selectInput(inputId = "choose",
            label = "Search space",
            choices = c("Frequent drugsets (fast)", "All possible drugsets (slow)" )),

actionButton("do3", "Load drugsets"),


),
mainPanel( h4(HTML('<b/>A (frequent) drugset is an (frequent) itemset based on polysubstance use data from NSDUH. <br/>First, the user must load a collection of drugsets to define the search space.</br></b>')),
  h4(HTML('Option 1 (fast): Use <I/>frequent drugset </I>to search within the master table ')),
           h4(HTML('Option 2 (slow): Use <I/>all possible drugsets </I> to search within the full search space')),
           h6(HTML("<br/> </br> Cite as: <I/>Identifying Patterns of Association and Transition in the Use of Addictive Substances over Five Decades in the United States.</I>
S. Ray, M. Aruru and S.Pyne, medRxiv 2021.03.11.21253386; doi: https://doi.org/10.1101/2021.03.11.21253386 <br/> </br>")),
           
           
)
),

# tags$style(type = 'text/css', 
#            '.navbar { background-color: steelblue;}',
#            '.navbar-default .navbar-brand{color: white;}',
#            '.tab-panel{ background-color: red; color: white}',
#            '.nav navbar-nav li.active:hover a, .nav navbar-nav li.active a {
#                         background-color: green ;
#                         border-color: green;
#                         }'
#            
# ))


 

tabsetPanel(
 tabPanel(
    "Association pattern matching",
  
  # App title ----
  #titlePanel("PSUMAnT"),
  
  # Sidebar layout with a input and output definitions ----
  div(
    id = "form",
    
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      h3(HTML("Association pattern matching")),
      # Input: Selector for choosing dataset ----
       
      selectInput(inputId = "meth",
                  label = "Format",
              choices = c("fixed bitstring (length 10 bits)","regular expression", "approximate regular expression")),
    
       textInput("text", h4("Input pattern (of above format)"), 
                            value = ""),
      
       #   actionBttn("reset", "RESET", style="simple", size="sm", color = "warning"),
      actionButton("do1", "Run"),
  actionButton("resetAll", "Reset parameters",style="simple", size="sm", color = "warning"),
  downloadButton("downloadData1", "Save", size="xs")#, style="color: #fff; background-color: steelblue; border-color: Black;")
    ),      
#     
    
    mainPanel(
     # plotOutput("plot"),
      #tableOutput("view")
    #  plotOutput("plot1"),
      
      # bsModal("modalExample", "Your plot", "go", size = "large",plotOutput("plot1"),downloadButton('downloadPlot', 'Download')),
      
      fluidRow(
        htmlOutput('nmatch'),
        htmlOutput('msg1'),
        htmlOutput('msg2'),
     #    column(2, DT::dataTableOutput('x1')),
      #  column(1, DT::dataTableOutput('x2')),
     #   column(1, DT::dataTableOutput('x3'))
     ),
      textOutput('normalError'),
      textOutput('safeError'),
      textOutput('load'),
     # actionBttn("reset", "RESET", style="simple", size="sm", color = "warning"),
      verbatimTextOutput(outputId = "text"),
      
     tabsetPanel(
       tabPanel("Results", column(3, DT::dataTableOutput('x1')), column(1,DT::dataTableOutput('x2')), column(1,DT::dataTableOutput('x3'))),
       tabPanel("Plot",  plotOutput(outputId = "plot"))
     ),
  )
  ),
),
),

tabPanel("Transition rule mining",

sidebarLayout(
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    h3(HTML("Transition rule mining")),
    
    selectInput(inputId = "per",
                label = "Time period",
                choices = c("1965-1974","1970-1979","1975-1984","1980-1989","1985-1994","1990-1999","1995-2004","2000-2009","2005-2013")),
    
  
  
  
  selectInput(inputId = "drg",
              label = "Substance",
              choices = c("CIGA (CIGARETTE)", "ALCO (ALCOHOL)", "MARI (MARIJUANA)", "COCA (COCAINE)", "CRAC (CRACK COCAINE)", "HERO (HEROIN)", "HALU (HALLUCINOGENS)", "LSD (LSD)",  "PCP (PCP)",  "ECST (ECSTASY)", "INHA (INHALENTS)", "PAIN (PAIN RELIEVERS)", "TRNQ (TRANQUILIZERS)", "STIM (STIMULANT)", "METH (METHAMPHETAMINE)", "SEDA (SEDATIVES)")),
  
  
  selectInput(inputId = "dir",
              label = "Fixed substance",
              choices = c("Right", "Left")),
  
  actionButton("do2", "Run"),
  
  actionButton("resetAll1", "Reset parameters",style="simple", size="sm", color = "warning"),
  #  actionButton("resetName", "Reset name"),
  
  downloadButton("downloadData", "Save" ,size="sm")# style="color: #fff; background-color: steelblue; border-color: Black;")
  
  ),
  mainPanel(
    
    tabsetPanel(
      tabPanel("Results", DT::dataTableOutput('x4')), #downloadButton("downloadData", "Download", style="color: #fff; background-color: green; border-color: Black;")),
      tabPanel("Plot1",  plotOutput(outputId = "plot1")),#,downloadButton("download", "Download", style="color: #fff; background-color: green; border-color: Black;")),
      tabPanel("Plot2", plotOutput(outputId = "plot2"))#,downloadButton("download", "Download", style="color: #fff; background-color: green; border-color: Black;"))
    
   
   
            )
  )
)
),

 tags$style(HTML(".navbar-header { width:100% }
                    .navbar-brand { width: 100%; text-align: left }"),
           #  type = 'text/css', 
            '.nav-tabs {font-size: 10px,background-color: white}',
            '.navbar { background-color: white;}',
            '.navbar-default .navbar-brand{color: white;}',
           '.tab-panel{ background-color: black; color: white}')
 )
)
#)

