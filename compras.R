setwd("/home/gustavo/Projeto de Dados/Análise_R/compras")
getwd()

library(dplyr)
library(tidyverse)
library(ggplot2)
library(esquisse) 
library(hrbrthemes)
library(stringr)

compras <- read.csv("compras.csv", sep = ";")

View(compras)

## Total de Compras 

total_compras <- sum(compras$Total_Gasto)

total_compras <- paste("R$", format(total_compras, decimal.mark = ",", big.mark = ".", nsmall = 2))

view(total_compras)

## Sexo por Total de Compras 

data <- data.frame(
  
  Sexo <- compras$Sexo, 
  Total <- compras$Total_Gasto
)

ggplot(data, aes(x=Total, y=Sexo, fill=Sexo)) + 
  labs(title = "Total de Compras por Sexo") +
  geom_bar(stat = "identity") + coord_flip()

## Total de Compras por Ocupação e Sexo 

esquisser(compras)

library(ggplot2)

ggplot(compras) +
 aes(x = Sexo, weight = Total_Gasto) +
 geom_bar(fill = "#CB9900") +
 labs(title = "Total de Compras por Sexo e Ocupação", 
 subtitle = "Comparação por Sexo e Profissão") +
 hrbrthemes::theme_ipsum_tw() +
 facet_wrap(vars(Ocupação))

## Total de Gasto das Assinaturas por Sexo e Ocupação

esquisser(compras)

library(ggplot2)

ggplot(compras) +
 aes(x = Total_Gasto, y = Nome, size = Ocupação) +
 geom_point(shape = "circle small", 
 colour = "#B90000") +
 labs(x = "Total de Compras", y = "Ocupação", title = "Total de Compras por Nome e Ocupação", 
 subtitle = "Comparação de Gasto por Profissão e Nome") +
 hrbrthemes::theme_modern_rc() +
 theme(plot.title = element_text(face = "bold", 
 hjust = 0.5), plot.subtitle = element_text(face = "bold", hjust = 0.5))

## Total de Compras por Nome 

esquisser(compras)

library(ggplot2)

ggplot(compras) +
 aes(x = Nome, weight = Total_Gasto) +
 geom_bar(position = "dodge", fill = "#000000") +
 labs(x = "Nome", y = "Total", title = "Total de Compras por Nome", subtitle = "Compras Geral", caption = "Valores") +
 theme_dark() +
 theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.subtitle = element_text(face = "bold", 
 hjust = 0.5), plot.caption = element_text(face = "bold"), axis.title.y = element_text(face = "bold"), 
 axis.title.x = element_text(face = "bold"))

## Total de Assinaturas por Nome 

data = data.frame(
  
  Total <- compras$Total_Gasto, 
  Nome <- compras$Nome
)

ggplot(data, aes(x=Nome, y=Total)) + 
  labs(title = "Total Gasto por Nome de Assinantes") +
  geom_bar(stat = "identity") + 
  coord_flip()

## Total de Compras por Assinaturas

compras$Assinatura <- as.Date(compras$Assinatura)

## Gráfico 

compras %>%
  tail(10) %>% 
  ggplot(aes(x=Assinatura, y=Total_Gasto)) +
  geom_line(color="grey") + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  theme_ipsum() +
  ggtitle("Total Gasto por Assinaturas")
