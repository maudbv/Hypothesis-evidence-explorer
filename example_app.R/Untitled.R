library(shiny)
library(shinyWidgets)
library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(title = "Simple tabs"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              h5("Click the upper left hand corner of the picture to switch tabs"),
              tags$map(name="fitMap",
                       tags$area(shape ="rect", coords="10,10,200,300", alt="floors", 
                                 onclick="var message = {id: \"tab\", data: \"widgets\", 
                           nonce: Math.random()}; Shiny.onInputChange('tab', message)"), 
                       tags$img(src = 'https://i.stack.imgur.com/U1SsV.jpg', 
                                alt = 'System Indicators', usemap = '#fitMap') 
              )   
      ),
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$tab, {
    updateTabItems(session, "tabs", input$tab$data)
  })
}

shinyApp(ui, server)
