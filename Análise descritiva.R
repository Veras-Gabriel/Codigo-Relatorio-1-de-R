Banco <- read.csv("amostra_190106794.csv", header = T)
help(ggplot)
library(ggplot2)
install.packages("stringr")
library(stringr)
library(dplyr)

#3 Variáveis categóricas 
#a)Sexo: 
Banco$SEXO <- str_replace_all(Banco$SEXO, c('A' = 'Masculino', 'B' = "Feminino"))
qplot(SEXO, data = Banco, fill = SEXO) + 
  labs(x = "Sexo", title = "Figura 1: Número de estudantes por gênero") 

table(Banco$SEXO)

#b)computador:
Banco$COMPUTADOR <- 
  str_replace_all(Banco$COMPUTADOR, c("A" = 'Não', "B"= "1", "C" = "2", "D" = "3", "E" = "4+", " " = "Sem resposta"))

qplot(COMPUTADOR, data = Banco, fill = COMPUTADOR) + 
  labs(x = "Possui computadore(es)?", title = "Figura 2: Número de alunos em relação ao acesso a computadores")

table(Banco$COMPUTADOR)

#c) Raça;
Banco$RACA_COR <- 
  str_replace_all(Banco$RACA_COR, c("A" = "branca",  "B"= "preta", 'C' = 'parda', 'D' = "amarela", 
                                                   'E' = 'Indígena', 'F' = "Não_declarado", " " = "Sem resposta"))
qplot(RACA_COR, data = Banco, fill = RACA_COR) + 
  labs(x = "Declaração de Raça", title = "Figura 3: Número de estudantes em relação à auto-declaração de sua cor") 
table(Banco$RACA_COR)

#d) Mora com o pai
Banco$MORA_PAI <- 
  str_replace_all(Banco$MORA_PAI, c('A' = 'Sim', 'B' = 'Não', 'C' = 'Com outro homem responsável'))

qplot(MORA_PAI, data = Banco) + 
  labs(x = "Mora com o pai?", title = "Figura 4: Número de estudantes que moram ou não com o pai")

table(Banco$MORA_PAI)

#e) Mora com a mãe:
Banco$MORA_MÃfE <- 
  str_replace_all(Banco$MORA_MÃfE, c('A' = 'Sim', 'B' = 'Não', 'C' = 'Com outra mulher responsável'))

qplot(MORA_MÃfE, data = Banco) + 
  labs(x = "Mora com a mãe?",title = "Figura 5: Número de estudantes que moram ou não com a mãe")

table(Banco$MORA_MÃfE)

#4- Variáveis Quantitativas:
#a)
IntervaloMT <- c('100-120', '120-140', '140-160', '160-180', '180-200',
               '200-220', '220-240', '240-260', '260-280', '280-300', '300-320', '320-340', '340-360')

##
Tabela_distribuicaoLP <- data.frame(IntervaloLP)%>%
  mutate(Frequência_Absoluta = hist(Banco$NOTA_LP, plot=FALSE)$counts)%>%
  mutate(Frequência_Relativa = hist(Banco$NOTA_LP, plot=FALSE)$density)

  
##
Tabela_distribuicaoMT <- data.frame(IntervaloMT)%>%
  mutate(Frequência_Absoluta = hist(Banco$NOTA_MT, plot=T)$counts)%>%
  mutate(Frequência_Relativa = hist(Banco$NOTA_MT, plot=T)$density)

#b) 
hist(Banco$NOTA_LP, breaks = 10,
     xlab = "Notas",
     ylab = "Frequência", 
     main = "Figura 6: Histograma de frequência de notas de Língua Portuguesa",
     xlim = c(90, 350),
     freq = F)

##
hist(Banco$NOTA_MT, breaks = 10,
     xlab = "Notas",
     ylab = "Freqência", 
     main = "Figura 7: Histograma de frequência de notas de Matemática",
     xlim = c(100, 350),
     freq = F)

#c)
# Posição LP
Q1 <- quantile(Banco$NOTA_LP, probs = 0.25)
Q2 <- quantile(Banco$NOTA_LP, probs = 0.50)
Q3 <- quantile(Banco$NOTA_LP, probs = 0.75)

#####

# Posição  mat
Q1mat <- quantile(Banco$NOTA_MT, probs = 0.25)
Q2mat <- quantile(Banco$NOTA_MT, probs = 0.50)
Q3mat <- quantile(Banco$NOTA_MT, probs = 0.75)

####################################

g1<-function(x){  ## coeficiente de assimetria
  n<-length(x)
  s<-sd(x)
  m<-mean(x)
  n/((n-1)*(n-2))*sum((x-m)^3)/s^3
}

g2<-function(x){  ## coeficiente de curtose
  n<-length(x)
  s<-sd(x)
  m<-mean(x)
  (n*(n+1)/((n-1)*(n-2)*(n-3)))*sum((x-m)^4)/s^4-3*(n-1)^2/((n-2)*(n-3))
}

cat("Assimetria Portugês=",g1(Banco$NOTA_LP),"Curtose Portugês=",g2(Banco$NOTA_LP),"\n")
cat("Assimetria Matemática=" ,g1(Banco$NOTA_MT),"Curtose Matemática=",g2(Banco$NOTA_MT),"\n")

summary(Banco$NOTA_MT)

#d

boxplot(Banco$NOTA_LP, 
        main = "Figura 8: Boxplot de notas da matéria Língua Portuguesa",
        ylab = "Notas Portugês")

boxplot(Banco$NOTA_MT, 
        main = "Figura 9: Boxplot de notas da matéria Matemática",
        ylab = "Notas Matemática")
  

