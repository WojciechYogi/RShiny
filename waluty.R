library(rjson)
library(RCurl)
url <- getURL("http://api.nbp.pl/api/exchangerates/tables/A//?format=json")
result = fromJSON(url)
datas = as.data.frame(result)
datas = datas[,-c(1:3)]
Waluta = seq(1, 105, 3)
Waluta = datas[Waluta]
Skrot = seq(2, 105, 3)
Skrot =datas[Skrot]
Przelicznik = seq(3, 105, 3)
Przelicznik = datas[Przelicznik]

data.frames.ER = matrix(ncol = 3, nrow = 35)
data.frames.ER[,1] = as.matrix(Waluta)
data.frames.ER[,2] = as.matrix(Skrot)
data.frames.ER[,3] = as.matrix(Przelicznik)
colnames(data.frames.ER) = c("Waluta", "Skrót","Przelicznik")
np = c(2,8, 10, 11, 12, 13, 34)
Data.Frame.Shiny = as.data.frame(data.frames.ER[np,])

url2 = getURL("http://api.nbp.pl/api/exchangerates/tables/A/2019-06-01/2019-08-30//?format=json")
result2 = fromJSON(url2)
datas2 = as.data.frame(result2)
tablica.A = datas2[,seq(from = 1, to = length(datas2), by = 108)]
usun = datas2[,seq(from = 2, to = length(datas2), by = 108)]
daty = datas2[,seq(from = 3, to = length(datas2), by = 108)]

datas.listowna = datas2
datas.listowna = datas.listowna[,-c(seq(from = 1, to = length(datas2), by = 108), seq(from = 0, to = length(datas2), by = 108))]
library(rlist)
lista.waluty = list()
x= list()
dl = length(daty)
for(i in 1:dl)
{
  x = list()
  for(j in 1:105)
  {
  y = datas.listowna[,i*j]
  x = list.append(x, y)
  }
  lista.waluty = list.append(lista.waluty, x)
}
tablice = list()
Waluta = seq(1, 105, 3)
Skrot = seq(2, 105, 3)
Przelicznik = seq(3, 105, 3)

for(i in 1:dl)
{
  df.t = lista.waluty[[i]]
  Waluta = seq(1, 105, 3)
  Waluta = df.t[Waluta]
  Skrot = seq(2, 105, 3)
  Skrot =df.t[Skrot]
  Przelicznik = seq(3, 105, 3)
  Przelicznik = df.t[Przelicznik]
  
  data.frames.ER = matrix(ncol = 3, nrow = 35)
  data.frames.ER[,1] = as.matrix(Waluta)
  data.frames.ER[,2] = as.matrix(Skrot)
  data.frames.ER[,3] = as.matrix(Przelicznik)
  colnames(data.frames.ER) = c("Waluta", "Skrót","Przelicznik")
  
  df.ER = data.frames.ER
  df.ER = as.data.frame(df.ER)
  df.ER2 = df.ER %>%
    filter(Skrót %in% c("USD", "EUR","CHF","GBP","UAH","JPY","CNY"))
  df.ER2$Przelicznik = as.numeric(as.character(df.ER2$Przelicznik))
  
  tablice = list.append()
}

Waluta = seq(1, 105, 3)
Waluta = datas[Waluta]
Skrot = seq(2, 105, 3)
Skrot =datas[Skrot]
Przelicznik = seq(3, 105, 3)
Przelicznik = datas[Przelicznik]

data.frames.ER = matrix(ncol = 3, nrow = 35)
data.frames.ER[,1] = as.matrix(Waluta)
data.frames.ER[,2] = as.matrix(Skrot)
data.frames.ER[,3] = as.matrix(Przelicznik)
colnames(data.frames.ER) = c("Waluta", "Skrót","Przelicznik")
np = c(2,8, 10, 11, 12, 13, 34)
Data.Frame.Shiny = as.data.frame(data.frames.ER[np,])
