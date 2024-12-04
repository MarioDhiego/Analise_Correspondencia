
#------------------------------------------------------------------------------#

library(readr)
library(readxl)
library(magrittr)
library(ca)
library(xtable)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(plotly)
#------------------------------------------------------------------------------#



#------------------------------------------------------------------------------#
data(smoke)

Fumante_CA <- CA(smoke, graph = FALSE)
summary(Fumante_CA)

par(cex = 0.7,
    mar = c(2.5,3,2,0.8)+0.1)

colnames(smoke) <- c("Nenhum", "Pouco", "Moderado", "Forte")
rownames(smoke) <- c("GS", "GJ", "FS", "FJ", "SC")

#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
# Grafico Simples
plot(Fumante_CA)
#------------------------------------------------------------------------------#



#------------------------------------------------------------------------------#
# Grafico Personalizado


# Variância Explicada Dimensões
var_exp <- round(as.numeric(Fumante_CA$eig[1:2, 2]), 1)


# Grafico de Correspondência
grafico <- factoextra::fviz_ca_biplot(
  Fumante_CA, 
  title = "Análise de Correspondência Simples", # Título
  labelsize = 5,                                # Tamanho do Texto
  pointsize = 2,                                # Tamanho dos Pontos
  arrowsize = 2,                                # Tamanho das Setas
  mean.point = TRUE,                            # Ponto médio das observações.
  graph = TRUE,                                 # Como Gráfico será Desenhado
  repel = TRUE,                                 # Evita sobreposição de rótulos.
  label = "all",                                # Legenda para os eixos
  shape.row = 19,                               # Formato dos Pontos para Linhas
  shape.col = 17,                               # Formato dos Pontos para Colunas
  alpha.row = "contrib",                        # Transparência dos pontos
  map = "symmetric",                            # Tipo de mapeamento usado.
  axes.linetype = "dotted",                     # Tipo de linha para os eixos
  ggtheme = theme_gray()) +
  ggplot2::labs(x = paste0("Dimensão I (", var_exp[1], "% Variância Explicada)"),
                y = paste0("Dimensão II (", var_exp[2], "% Variância Explicada)")) +
  ggplot2::theme(plot.title = element_text(hjust = 0.5))  # centraliza o Título

  # Exibir Gráfico
  grafico

#------------------------------------------------------------------------------#











