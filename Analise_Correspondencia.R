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
# Criando os vetores para cada coluna

# Tabela 03
EFPeu <- c(18,5,13,8,4,8)
EPMoutro <- c(27,29,15,14,15,10)
ManComp <- c(7,1,2,4,1,3)
AtrCausa <- c(4,12,12,18,12,18)
NaoClass <- c(12,5,6,6,20,10)
NsabeNI <- c(2,1,3,3,4,3)


# Tabela 04
Alegria <- c(7,9,46,34)
Tristeza <- c(30,29,23,14)
Raiva <- c(35,36,18,8)
Medo <- c(31,24,22,19)
Orgulho <- c(46,26,7,17)
Vergonha <- c(36,30,17,13)

# Tabela 5
ExpressãoFacial <- c(11,8,4,1,2,3)
ManiFCorporal <- c(7,9,11,7,3,15)
ManiFOral <- c(2,5,6,12,5,6)
Contexto <- c(25,24,19,26,23,17)
NãoClass <- c(7,5,12,5,13,8)
NsabeNI <- c(1,2,1,2,7,4)


# Tabela 6
ExpressãoFacial <- c(14,10,9,4,1,3)
ManiFCorporal <- c(7,11,8,5,1,16)
ManiFOral <- c(3,5,6,11,1,7)
Contexto <- c(24,18,16,19,17,15)
NãoClass <- c(9,6,11,5,8,9)
NsabeNI <- c(1,1,1,4,7,3)

# Tabela 13
Pessoal <- c(15,6,5,9,16,5)
Conexão <- c(10,12,6,12,7,15)
Genérico <- c(13,10,23,7,1,4)
NãoClass <- c(12,21,14,12,20,23)
NsabeNI <- c(3,5,6,6,10,7)

# Tabela 14
Pessoal <- c(29,1,3,8,10,4)
Conexão <- c(20,10,7,9,4,9)
Genérico <- c(25,8,16,5,1,3)
NãoClass <- c(23,17,6,13,19,18)
NsabeNI <- c(6,5,6,6,8,5)


# Tabela 7
Mensão_Mãe <- c(32,25,16,5,4,4,3,2,1,1,1)
Mensão_Pai <- c(15,13,7,1,1,1,2,2,1,1,1)


# Tabela 9
Mensão_Mãe <- c(27,24,14,11,6,5,4,3)
Mensão_Pai <- c(12,9,8,3,3,4,1,3)



#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Criando os nomes das linhas


# Tabela 08
nomes_linhas <- c("Estou","Sorrir1","Feliz","Conversar","Abraçar",
                  "NãoRespondeu","Escola","Brincar")

# Tabela 07
nomes_linhas <- c("Brincar","Sorrir","Feliz","Conversar","Abraçar",
                  "NãoRespondeu","Escola","Liberdade","Chorar","Dançar","Passear")
# Tabela 05
nomes_linhas <- c("Alegria","Tristeza","Raiva","Medo","Orgulho","Vergonha")

# Tabela 04
nomes_linhas <- c("NemPouco_Mãe","NemPouco_Pai","Muito_Mãe","Muito_Pai")

# Tabela 03
nomes_linhas <- c("Alegria", "Tristeza", "Raiva", "Medo", "Orgulho", "Vergonha")
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Criando o data frame com os dados

#Tabela 07
dados_completo <- data.frame(Mensão_Mãe, Mensão_Pai)

# tabela 13
dados_completo <- data.frame(Pessoal,Conexão,Genérico,NãoClass,NsabeNI)

# Tabela 05
dados_completo <- data.frame(ExpressãoFacial, ManiFCorporal, ManiFOral, Contexto, NãoClass, NsabeNI)
 
# Tabela 04
dados_completo <- data.frame(Alegria,Tristeza,Raiva,Medo,Orgulho,Vergonha)

# Tabela 03
dados_completo <- data.frame(EFPeu, EPMoutro, ManComp, AtrCausa, NaoClass, NsabeNI)
#------------------------------------------------------------------------------#


# Definindo os nomes das linhas para o data frame
rownames(dados_completo) <- nomes_linhas

# Exibindo o objeto com todos os dados
print(dados_completo)

Fumante_CA <- CA(dados_completo, graph = FALSE)
summary(Fumante_CA)
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# ANÁLISE DE RESÍDUOS

# Obtendo os valores observados
observados <- dados_completo

# Obtendo a tabela de contingência total (marginais)
marginais <- Fumante_CA$call$X

# Calculando os valores esperados (matriz de probabilidades)
soma_total <- sum(marginais)
linhas_somas <- rowSums(marginais)
colunas_somas <- colSums(marginais)
esperados <- outer(linhas_somas, colunas_somas) / soma_total

# Calculando os resíduos
residuos <- (marginais - esperados)/sqrt(esperados)

# Exibindo os resíduos
print(residuos)


#------------------------------------------------------------------------------#
# Grafico Simples
plot(Fumante_CA)
#------------------------------------------------------------------------------#



# Função para calcular a estatística Beta

chi_quadrado <-  58.48136
Linha <- 6
Coluna <- 5

calcular_beta <- function(chi_quadrado, Linha, Coluna) {
  
  beta <- (chi_quadrado-(Linha-1)*(Coluna-1))/sqrt((Linha-1)*(Coluna-1))
  
  # Retornar o valor de Beta
  return(beta)
}


# Calcular Beta
beta_resultado <- calcular_beta(chi_quadrado, Linha, Coluna)

# Exibir o resultado
cat("Critério Beta é:", beta_resultado, "\n")







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











