# final-projects-ellenandhyein
final-projects-ellenpthao1901 created by GitHub Classroom

library(shiny)
library(shinydashboard)
library(ggplot2)
source("data wraggling.R")
library(leaflet)
library(maps)
library(plotly)
library(ggpubr)
library(data.table)
source("data wraggling 1.R")



df_ <- merge(x= df, y=coordinates_df, by.x= "Country.or.region", by.y= "Country")

ui <- fluidPage(

    titlePanel("Final Project"),
    #img(src="myimage.png",height="50%", width="50%",align="right"),

navbarPage("World Happiness",
           tabPanel("Introduction",
                    verbatimTextOutput("intro")
           ),
           tabPanel("Factors Contribute to World Happiness Score",
                    sidebarLayout(
                      sidebarPanel(
                        h2("Control Panel"),
                        selectInput(
                          inputId = "year",
                          label = "Select a Year",
                          choices=unique(df_1$Year),selected=unique(df_1$Year)[1]
                        )
                      ),
                      mainPanel(
                        plotOutput(outputId = "scatterplot")
                      )
                    )
           ),
               tabPanel("World Happiness and Suicide Rate in 2018",
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput(inputId = "slider",
                                        label = "World Happiness Score",
                                        min = 2, max = 8, value = c(5, 7),
                                        step = 0.1)
                          ),
                          mainPanel(
                            plotOutput(outputId = "worldmap")
                          )
                        )
               
               ),
           tabPanel("Suicide Rate and World Happiness in 2020",
                    sidebarLayout(
                      sidebarPanel(
                        sliderInput(
                          inputId = "happiness_scale",
                          label = "Happiness Scale",
                          min = 0.5,
                          max = 8,
                          step = 0.5,
                          value = 8
                        )
                      ),
                      mainPanel(
                        h3("Suicide Rate Bubble Map"),
                        leafletOutput("map")
                      )
                    )
           ),
           tabPanel("Conclusion",
                    verbatimTextOutput("conclude")
           )
)
)



server <- function(input, output) {
  output$intro <- renderText({
"Happiness is one of the most important factors of life that fulfill people’s satisfaction, tranquility, and deep sleep. 
Happiness level is calculated by GDP per capita, social support, health life expectancy, freedom to make life choices, generosity etc. 
Comparing the factors of world happiness level before and after COVID-19, it will help us understand which factor affected the most. 
On top of that, we will investigate relation between the suicide rate and the trends in happiness levels. 
We are using a variety of ways to understand the happiness level better. 
Our goal is to figure out which element has changed, especially during tough times like the COVID-19 pandemic. 

In this project, we will use the 2018 and 2020 World Happiness Index report and suicide rate report as a correlating element. 
Which factor impacted the most for Happiness level before and after COVID-19? 
How suicide rate correspond to Happiness level in 2018? 
How suicide rate correspond to Happiness level in 2020?"
  })

  filtered_df <- reactive({
    df_filtered <- df %>%
      filter(Ladder.score >= input$slider[1] & Ladder.score <= input$slider[2])
    return(df_filtered)
  })
  
  
  calculate_radius <- function(scale) {
    if (scale == 0) {
      return(5)  # Minimum radius value when Happiness Scale is 0
    } else {
      return(100 / scale)
    }
  }
  
  output$barplot <- renderPlot({
    world_coordinates <- map_data("world")
    data("world.cities")
    newdf<-merge(filtered_df(),world.cities,by.x="Country.or.region",by.y="country.etc")  
    ggplot() + geom_map(
      data = world_coordinates, map = world_coordinates,
      aes(long, lat, map_id = region)
    ) + geom_point(
      data = newdf,
      aes(long, lat, color = Suicide.rate,
          size=pop),
      alpha = 1
    ) 
  })
  
  
  output$worldmap <- renderPlot({
    world_coordinates <- map_data("world")
    data("world.cities")
    newdf<-merge(filtered_df(),world.cities,by.x="Country.or.region",by.y="country.etc")  
    ggplot() + geom_map(
      data = world_coordinates, map = world_coordinates,
      aes(long, lat, map_id = region)
    ) + geom_point(
      data = newdf,
      aes(long, lat, color = Suicide.rate,
          size=pop),
      alpha = 1
    ) 
  })
  
  
  output$scatterplot <- renderPlot ({
    filt_df_1<-filter(df_1, Year%in%input$year)
    print(head(df_1))
    print(input$year)
    ggpubr::ggscatter(data=df_1,x="Happiness.score",y="Healthy.life.expectancy",color="blue",add = "reg.line")
    df_long <- melt(setDT(filt_df_1), id.vars = c("Country","Year","Happiness.score"), variable.name = "PARAM",value.name = "VALUE")
    ggpubr::ggscatter(data=df_long,x="Happiness.score",y="VALUE",facet.by = "PARAM",color="blue",add = "reg.line", cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                      cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n"), scales="free_y")
  })
  
  filtered_data_1 <- reactive({
    df_ %>%
      dplyr::filter(Score >= input$happiness_scale) %>%
      select(Country, Suicide.rate, Score, Latitude, Longitude)
  })

  
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(map, lng = 26, lat = 38, zoom = 1) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addCircleMarkers(
        data = filtered_data_1(),
        lat = ~Latitude,
        lng = ~Longitude,
        radius = ~9/input$happiness_scale,  # Adjust radius based on happiness scale
        color = "red",
        fillOpacity = 0.8,
        popup = paste("Country: ", filtered_data_1()$Country, "<br>",
                      "Suicide Rate: ", filtered_data_1()$Suicide.rate, "<br>",
                      "Happiness Score: ", filtered_data_1()$Score)
      )
  })
  output$conclude <- renderText({
"The most impacted factor to the happiness level before and after COVID-19 was both GDP per capita. 
Among health life expectancy, GDP per capita, social support, freedom to make life choices, and generosity, GDP per capita had the strongest relationship with happiness. 
In 2018, before COVID-19, correlation coefficient (R) of GDP per capita was 0.8 as the strongest relationship and generosity was 0.14 as the weakest relationship. 
In 2020, after COVID-19, GDP per capita also recorded the strongest relationship as 0.78, and the lowest impact factor was also generosity as 0.069. 
On top of that, since every R in 2020 decreased, we can infer that overall happiness level after COVID-19 decreased. 
Therefore, by observing the relationship between happiness level and each factor, we can conclude that happiness level depends on how people spend their money regardless of the pandemic, even though R decreased a little bit in 2020. 

In the world map, we can observe that when the happiness level from the year 2018 is higher, the color of the world map becomes darker. 
While world happiness score is around 2 and 3.2, the color of the world map gets very lighter which means the suicide rate in high. 
On the other hand, when the happiness level is around 5.6 and 6.8 we can see the darker blue on the map. 
As a result, we can assume that if a country gets a high rate of happiness level, probably that the suicide rate is low. 

In the suicide rate bubble map, we can visualize that the bubbles get bigger when the 2020 happiness scale gets smaller inversely. 
The bubble map displaying countries with happiness scores to the selected scale, and the size of the bubbles corresponds to the adjusted happiness scale, offering a visual representation of suicide rate. 
The first small bubbles appeared at 7.5 from North and Western Europe. At scale 6, bubbles became bigger and spread into different countries. 
Finally at 0.5, the bubbles across the country are so large that it appeared to be a huge lump. In conclusion, same as the world map, we can assume that the higher happiness level countries are likey to have lower suicide rate. 

In this project, we could examine that GDP per capita affected the most between five factors of happiness level before and after COVID-19. 
Moreover, interactive tool allowed us to observe patterns and potential correlations between happiness and suicide rates across different countries in 2018 and 2020 that the higher happiness level of countries have lower suicide rate."
})
    }


# Run the application 
shinyApp(ui = ui, server = server)

