library(ggplot2)
library(dplyr)
library(gridExtra)
library(shiny)
library(shinydashboard)
library(rlist)
library(devtools)
library(ggbiplot)
library(tidyverse)
library(cluster) 
library(factoextra)
### Przygotowanie danych
dane = USArrests
West = c("Washington", "Oregon","Idaho","Montana","Wyoming","Colorado","New Mexico",
         "Arizona", "Utah","Nevada", "California", "Hawaii", "Alaska")
Midwest = c("North Dakota","Minnesota","Wisconsin","Iowa","Illinois", "Indiana","Ohio",
            "Missouri","Kansas","Nebraska", "Michigan", "South Dakota" )
NorthEast = c("Maine", "New York", "New Hampshire","Vermont","Massachusetts",
              "Pennsylvania", "New Jersey","Connecticut","Rhode Island")
South = c("Texas","Oklahoma","Arkansas","Lousiana","Mississippi", "Alabama",
          "Tennessee", "Kentucky", "Virginia", "Florida","Georgia","South Carolina",
          "North Carolina", "Louisiana", "West Virginia", "Maryland","Delaware")
wektor = c()
for(i in 1:50)
{
wktr = ifelse(any(rownames(dane)[i]==(West)), "West",
              ifelse(any(rownames(dane)[i]==(Midwest)), "Mid - West",
                     ifelse(any(rownames(dane)[i]==(NorthEast)), "North - East",
                            ifelse(any(rownames(dane)[i]==(South)), "South", "BĹ‚Ä…d"))))
wektor = c(wektor, wktr)
}
dane = cbind(dane, wektor)
colnames(dane)[5] = "RejonUSA"
dane$RejonUSA = factor(dane$RejonUSA)
par(mfrow = c(1,2))
plot(x = dane$RejonUSA, 
     y = dane$Murder,
     main = "Wykres pudełkowy - Współczynnik Zabójstw w poszczególnych rejonach USA",
     xlab = "Rejon USA",
     ylab = "WspĂłĹ‚czynnik ZabĂłjstw", col = "orange")

plot(x= dane$RejonUSA,
     y = dane$Rape,
     main = "Wykres pudełkowy - Współczynnik gwałtów w poszczególnych rejonach USA",
     xlab = "Rejon USA",
     ylab = "Współczynnik gwałtów",
     col = "deepskyblue1")
# windows(width = 12, height = 10)
par(mfrow = c(1,1))
dane.barplot = dane

dane.barplot = dane.barplot %>% dplyr::group_by(RejonUSA) %>%
  dplyr::summarise('Murder' = mean(Murder),'Assault' =  mean(Assault),"UrbanPop" = mean(UrbanPop),"Rape" =  mean(Rape))

nazwy = as.character(dane.barplot$RejonUSA)
dane.barplot =dane.barplot[,-1]
rownames(dane.barplot) = nazwy
dane.barplot = as.matrix(dane.barplot)

barplot(height = dane.barplot, beside = T, legend=T, col = c("coral4", "darkgoldenrod1",
                                                             "darkgreen", "firebrick1"),
        args.legend = list(x = "topright", horiz = T, inset = -0.1, title = "Rejony USA",
                           box.lty = 0),
        ylim = c(0,250))

#### Częsć ggplot2

ggplot(data = dane, aes(x = Murder, y = Assault, color = RejonUSA)) + 
  geom_point(size = 1.9) +
  geom_smooth(method = "loess", se = F, size =1.3) + 
  theme_bw() + 
  theme(axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))


# chisq.test(c(dane.barplot[,1], dane.barplot[,2]))
# chisq.test(c(dane.barplot[,1], dane.barplot[,4]))
# chisq.test(c(dane.barplot[,2], dane.barplot[,4]))

Histogramy = function(dane = dane){
w1 = ggplot(data=dane, aes(dane[[1]])) + 
  geom_histogram(aes(y =..density..), 
                 breaks=seq(min(dane[[1]]), max(dane[[1]]), by = 0.5), 
                 col="red", 
                 fill="green", 
                 alpha=.2) + 
  geom_density(col=2) + 
  labs(title=paste("Histogram ",colnames(dane)[1]), x=colnames(dane)[1], y="CzÄ™stotliwoĹ›Ä‡") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        title = element_text(size = 16))
w2 = ggplot(data=dane, aes(dane[[2]])) + 
  geom_histogram(aes(y =..density..), 
                 breaks=seq(min(dane[[2]]), max(dane[[2]]), by = 15), 
                 col="red", 
                 fill="green", 
                 alpha=.2) + 
  geom_density(col=2) + 
  labs(title=paste("Histogram ",colnames(dane)[2]), x=colnames(dane)[2], y="CzÄ™stotliwoĹ›Ä‡") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        title = element_text(size = 16))

w3 = ggplot(data=dane, aes(dane[[3]])) + 
  geom_histogram(aes(y =..density..), 
                 breaks=seq(min(dane[[3]]), max(dane[[3]]), by = 5), 
                 col="red", 
                 fill="green", 
                 alpha=.2) + 
  geom_density(col=2) + 
  labs(title=paste("Histogram ",colnames(dane)[3]), x=colnames(dane)[3], y="Częstotliwość") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        title = element_text(size = 16))
w4 = ggplot(data=dane, aes(dane[[4]])) + 
  geom_histogram(aes(y =..density..), 
                 breaks=seq(min(dane[[4]]), max(dane[[4]]), by = 2), 
                 col="red", 
                 fill="green", 
                 alpha=.2) + 
  geom_density(col=2) + 
  labs(title=paste("Histogram ",colnames(dane)[4]), x=colnames(dane)[4], y="Częstotliwość") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        title = element_text(size = 16))
return(grid.arrange(w1, w2, w3, w4, ncol = 2))
} ## Metoda by skrĂłciÄ‡
Histogramy(dane)

### Analiza PCA + Wizualizacja

USA.PCA = prcomp(dane[,c(1,2,3,4)], center = T, scale. = T)
str(USA.PCA)
USA.PCA$sdev ### Na podstawie tego stwierdzam ĹĽe sÄ… 2 gĹ‚owne skĹ‚adowe sdev >= 1

ggbiplot(USA.PCA,choices = c(1,2), labels = rownames(dane), groups = dane$RejonUSA, ellipse = T, labels.size = 4)  +
  theme_bw() + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.position = "bottom")
## PoniĹĽsze tylko z tego wzglÄ™du ĹĽeby zobaczyÄ‡ jaka jest rĂłĹĽnica :)
# ggbiplot(USA.PCA,choices = c(1,4), labels = rownames(dane), groups = dane$RejonUSA, ellipse = T, labels.size = 4)  +
#   theme_bw() + 
#   theme(axis.title = element_text(size = 16),
#         axis.text = element_text(size = 15),
#         legend.title = element_text(size = 13),
#         legend.text = element_text(size = 12),
#         legend.position = "bottom")
# ggbiplot(USA.PCA,choices = c(2,4), labels = rownames(dane), groups = dane$RejonUSA, ellipse = T, labels.size = 4)  +
#   theme_bw() + 
#   theme(axis.title = element_text(size = 16),
#         axis.text = element_text(size = 15),
#         legend.title = element_text(size = 13),
#         legend.text = element_text(size = 12),
#         legend.position = "bottom")


### Clustrowanie
Dystans = dist(dane[,c(1,2,3,4)], method = "euclidean")
Hclust = hclust(Dystans, method = "complete")
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
op = par(bg = "#EFEFEF")
A2Rplot(Hclust, k = 3, boxes = FALSE, col.up = "gray50", col.down = c("tomato", 
                                                                  "orange", "blue"))

kfit.fviz = kmeans(Dystans, 10, nstart = 25)
fviz_cluster(kfit.fviz, data = Dystans, labelsize = 14,  main = "",
             ggtheme = theme_minimal(),ellipse = T)  + 
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 16),legend.text =  element_text(size = 16),
        legend.position = "bottom")


