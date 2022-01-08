# Define UI for application that draws a histogram

header <- dashboardHeader(
  title = "Surface Temperature"
)
sidebar <- dashboardSidebar(
      sidebarMenu(
        menuItem(text = "Overview", tabName = "overview", icon = icon("globe")),
        menuItem(text = "Temperature Map", tabName = "map", icon = icon("map")),
        menuItem(text = "Data", tabName = "data", icon = icon("book")),
        menuItem(text = "Info", tabName = "info", icon = icon("user"))
      )
    )
body <- dashboardBody(
      tabItems(
        tabItem(tabName = "overview",
                fluidPage(
                  h2(tags$b("Earth Surface Temperature")),
                  br(),
                  div(style = "text-align:justify;font-size:20px;", 
                      p("Climate change is real! But some still say's that climate change is only a myth. For those who think the climate change is only a myth, please follow these data visualization. It will make you think again.", 
                    "This data visualization is based on data of earth surface temperature from 1700 to 2013 compiled by the Berkeley Earth, which is affiliated with Lawrence Berkeley National Laboratory."),
                    br()
                  )
                ),
                fluidPage(
                  box(width = 8, 
                      solidHeader = T,
                      plotlyOutput("q_Plot")),
                  valueBox(value = temp_country2$Temp %>% 
                           max(), "Highest Temperature (°C)",
                           icon = icon("fire"),
                           color = "red"),
                  valueBox("Kuwait",
                           "Country with the Highest Surface Temperature",
                           icon = icon("city"),
                           color = "purple"),
                  valueBox("Interesting Fact",
                           "The visualization data on the left side is a data from 1900 to 2013. It can be seen, that a significant increase in earth surface temperature occurred around 1975 to 2013. Is it related to industrialization which increased in those years until now which cause a global warming?",
                           icon = icon("question"),
                           color = "blue"),
                ),
                fluidPage(
                  tabBox(width = 8,
                         title = tags$b("Temperature per Year"),
                         id = "tabset1",
                         side = "right",
                         tabPanel(tags$b("Country with Surface Temperature more than 30 °C"),
                                  plotlyOutput("bar_plot")),
                         tabPanel(tags$b("Country with Surface Temperature less than 30 °C"),
                                  plotlyOutput("bar_plot2"))
                         ),
                  box(width = 4,
                      selectInput(inputId = "year_id", 
                                  label = "Year",
                                  choices = unique(temp_country_$Year), 
                                  selected = 2013)),
                  box(width = 4,
                      h2(tags$b("Let's Act!")),
                      br(),
                      div(style = "text-align:justify;font-size:20px;",
                        p("Industrialization are important things, but also must be sustainable.", 
                          "Let's start supporting a sustainable environment to reduce global warming and climate crisis.",
                          "It's not too late, let's act!"),
                      ),
                      br())
                   ),
               ),
        tabItem(tabName = "map",
          fluidPage(
            box(width = 12,
                solidHeader = T,
                h3(tags$b("Earth Surface Temperature of Each Country")),
                leafletOutput("leaflet", height = 520))
          ),
          fluidPage(
            tabBox(width = 8,
                   title = tags$b("Surface Temperature per Country"),
                   id = "tabset2",
                   side = "right",
                   tabPanel(tags$b("Surface Temperature by Year"), 
                            plotlyOutput("point_plot")
                   ),
                   tabPanel(tags$b("Surface Temperature by Month"), 
                            plotlyOutput("point2_plot")
                   )
                ),
            box(width = 4,
                selectInput(inputId = "country_id", 
                            label = "Country",
                            choices = unique(temp_hist$Country), 
                            selected = "Kuwait")),
            box(width = 4,
                h2(tags$b("Description")),
                br(),
                div(style = "text-align:justify;font-size:17px;",
                    p("On the top, we can see through the map which country has the highest average temperature. Then, if you click on the map, it can be seen that each country has a different average maximum and minimum temperature."),
                    p("On the left, we can see an increasing of surface temperatur every year by each country. We can also see the highest temperature per month.")
                ),
                br()),
          )
        ),
        tabItem(tabName = "data",
                h2(tags$b("Earth Surface Temperature Data by Country")),
                DT::dataTableOutput("table")),
        tabItem(tabName = "info",
                box(width = 4,
                    h2(tags$b("Irdiansyah")),
                    br(),
                    h3(style = "text-align:justify;font-size:23px;", "Data Newbie | Tech Enthusiast"),
                    div(style = "text-align:justify;font-size:20px;", 
                    p("Thank you for your willingness to see this simple work."),
                    p("More about me? Please visit", a(href = "https://www.linkedin.com/in/irdiansyah/","my LinkedIn")),
                    br()
                    )
                  )
                )
        )
)

 

# Combining Dashboard Part
dashboardPage(
  header = header,
  body = body,
  sidebar = sidebar,
  skin = "red"
)