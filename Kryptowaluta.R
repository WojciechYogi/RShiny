#### Shiny ####
# library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)
library(SciViews)
baza = read_xlsx("C:/Users/Yogi/Desktop/Wizualizacje/Kryptowaluty.xlsx")
baza$date = as.Date(baza$date)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    h2("Analiza kryptowalut - Wojciech Krause", align = "center"),
    hr(),
    fluidRow(column(width = 6,
                    plotOutput(outputId = "wykresceny")),
             column(width = 6,
                    plotOutput(outputId = "wykresstopy"))),
    hr(),
    wellPanel(  # dla szarego tła
        fluidRow(
            column(width = 2,
                   checkboxGroupInput(inputId = "WyborKrypt", 
                                      label = h3("Wybórr Kryptowaluty"),
                                      choices = list("ETH" = "ETH",
                                                     "XMR" = "XMR",
                                                     "LTC" = "LTC"),
                                      selected = "ETH")),
            
            column(width = 2,
                   checkboxGroupInput(inputId = "WyborStopy", 
                                      label = h3("Wybór stóp zwrotu"),
                                      choices = list("rETH" = "rETH",
                                                     "rXMR" = "rXMR",
                                                     "rLTC" = "rLTC"),
                                                     selected = "rETH")),
            column(width = 3,
                   
                   
                   selectInput(inputId = "WyborSzereg", 
                               label = h3("Wybór ceny bądź logarytmu ceny"),
                               choices = list("Cena" = "Cena", 
                                              "Ceny zlogarytmowane" = "LNCena"))),
            column(width = 5,
                   h4("Statystyki Szeregu Czasowego Cen"),
                   tableOutput(outputId = "tabela"))
            
            
        )
    )
  )

server <- function(input, output) {
  kolory <- c("ETH" = "red", "XMR" = "orange","LTC" = "blue")  
  
  output$wykresceny <- renderPlot({
    baza.ceny = baza %>% select(date, ETH, XMR, LTC)
    if(input$WyborSzereg != "Cena")
    {
        baza.ceny$ETH = log(baza.ceny$ETH)
        baza.ceny$XMR = log(baza.ceny$XMR)
        baza.ceny$LTC = log(baza.ceny$LTC)
    }
    
    
                        
    plot.baza = ggplot(data = baza.ceny, aes(x = date)) +
        
        xlab("Rok") +
        ylab("Cena kryptowaluty") + 
        theme_bw() + 
        theme(axis.text = element_text(size = 16),
              axis.title = element_text(size = 18),
              legend.text = element_text(size = 18))
    
    for (i in input$WyborKrypt) {
      plot.baza <- plot.baza + 
        geom_line(aes_string(y = i), color = kolory[i], size = 1)
    }
    
    print(plot.baza)
    
    })
    output$wykresstopy <- renderPlot({
      
        kolory2 <- c("rETH" = "red", "rXMR" = "orange","rLTC" = "blue") 
        
        baza.stopy = baza %>% select(date, rETH, rXMR, rLTC)
        
        plot.baza.stopa = ggplot(data = baza.stopy, aes(x = date)) +
            xlab("Rok") +
            ylab("Stopa Zwrotu") + 
            theme_bw() + 
            theme(axis.text = element_text(size = 16),
                  axis.title = element_text(size = 18),
                  legend.text = element_text(size = 18))
        
        for (j in input$WyborStopy) 
        {
          plot.baza.stopa <- plot.baza.stopa + 
            geom_line(aes_string(y = j), color = kolory2[j], size = 1)
        } 
        print(plot.baza.stopa)
    })
    output$tabela <- renderTable({
        baza.tabela = baza %>% select(ETH, XMR, LTC)
        if(input$WyborSzereg != "Cena")
        {
            baza.tabela$ETH = log(baza.tabela$ETH)
            baza.tabela$XMR = log(baza.tabela$XMR)
            baza.tabela$LTC = log(baza.tabela$LTC)
        }
        baza.tabela.LTC = baza.tabela %>% summarise("Max "= max(baza.tabela$LTC),
                                                    "Min "= min(baza.tabela$LTC),
                                                    "Mediana" = median(baza.tabela$LTC),
                                                    "Ĺšrednia" = mean(baza.tabela$LTC))
        baza.tabela.XMR = baza.tabela %>% summarise("Max "= max(baza.tabela$XMR),
                                                    "Min "= min(baza.tabela$XMR),
                                                    "Mediana" = median(baza.tabela$XMR),
                                                    "Ĺšrednia" = mean(baza.tabela$XMR))
        baza.tabela.ETH = baza.tabela %>% summarise("Max "= max(baza.tabela$ETH),
                                                    "Min "= min(baza.tabela$ETH),
                                                    "Mediana" = median(baza.tabela$ETH),
                                                    "Ĺšrednia" = mean(baza.tabela$ETH))
        macierz.tabela = matrix(data = c(baza.tabela.ETH, baza.tabela.LTC, baza.tabela.XMR), ncol = 3)
        colnames(macierz.tabela) = c("LTC","XMR","ETH")
        rownames(macierz.tabela) = c("Max", "Min", "Mediana","Ĺšrednia")
        macierz.tabela = as.data.frame(macierz.tabela)
        macierz.tabela = t(macierz.tabela)
        Kryptowaluta = c("LTC","XMR","ETH")
        macierz.tabela = cbind(macierz.tabela, Kryptowaluta)
        macierz.tabela = as.data.frame(macierz.tabela)
        macierz.tabela = macierz.tabela %>% dplyr::select(Kryptowaluta, Max, Min, Mediana,)
        print(macierz.tabela)
    })
    
}
# Run the application 
shinyApp(ui = ui, server = server)
