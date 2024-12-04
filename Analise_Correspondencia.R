
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
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
# Grafico Simples
plot(Fumante_CA)
#------------------------------------------------------------------------------#



#------------------------------------------------------------------------------#
# Grafico Personalizado


# Variância Explicada Dimensões
var_exp <- round(Fumante_CA$eig[1:2, 2], 1)


# Grafico de Correspondência
grafico <- factoextra::fviz_ca_biplot(Fumante_CA, 
                           title = "Análise de Correspondência Simples",
                           labelsize = 5,
                           pointsize = 2,
                           arrowsize = 2,
                           mean.point = TRUE,
                           graph = TRUE,
                           repel = TRUE,
                           label = "all",
                           shape.row = 19,
                           shape.col = 17,
                           alpha.row = "contrib",
                           map = "symmetric",
                           axes.linetype = "dotted", # "dashed", "solid"
                           ggtheme = theme_gray()) +
  ggplot2::labs(x = paste0("Dimensão I (", var_exp[1], "% Variância Explicada)"),
                y = paste0("Dimensão II (", var_exp[2], "% Variância Explicada)"))+
  ggplot2::theme(plot.title = element_text(hjust = 0.5))

  # Exibir Gráfico
  grafico

#------------------------------------------------------------------------------#











