Banco <- read.csv("amostra_190106794.csv", header = T)
help(ggplot)
library(ggplot2)
install.packages("stringr")
library(stringr)
library(dplyr)

#3 Vari�veis categ�ricas 
#a)Sexo: 
Banco$SEXO <- str_replace_all(Banco$SEXO, c('A' = 'Masculino', 'B' = "Feminino"))
qplot(SEXO, data = Banco, fill = SEXO) + 
  labs(x = "Sexo", title = "Figura 1: N�mero de estudantes por g�nero") 

table(Banco$SEXO)

#b)computador:
Banco$COMPUTADOR <- 
  str_replace_all(Banco$COMPUTADOR, c("A" = 'N�o', "B"= "1", "C" = "2", "D" = "3", "E" = "4+", " " = "Sem resposta"))

qplot(COMPUTADOR, data = Banco, fill = COMPUTADOR) + 
  labs(x = "Possui computadore(es)?", title = "Figura 2: N�mero de alunos em rela��o ao acesso a computadores")

table(Banco$COMPUTADOR)

#c) Ra�a;
Banco$RACA_COR <- 
  str_replace_all(Banco$RACA_COR, c("A" = "branca",  "B"= "preta", 'C' = 'parda', 'D' = "amarela", 
                                                   'E' = 'Ind�gena', 'F' = "N�o_declarado", " " = "Sem resposta"))
qplot(RACA_COR, data = Banco, fill = RACA_COR) + 
  labs(x = "Declara��o de Ra�a", title = "Figura 3: N�mero de estudantes em rela��o � auto-declara��o de sua cor") 
table(Banco$RACA_COR)

#d) Mora com o pai
Banco$MORA_PAI <- 
  str_replace_all(Banco$MORA_PAI, c('A' = 'Sim', 'B' = 'N�o', 'C' = 'Com outro homem respons�vel'))

qplot(MORA_PAI, data = Banco) + 
  labs(x = "Mora com o pai?", title = "Figura 4: N�mero de estudantes que moram ou n�o com o pai")

table(Banco$MORA_PAI)

#e) Mora com a m�e:
Banco$MORA_M�fE <- 
  str_replace_all(Banco$MORA_M�fE, c('A' = 'Sim', 'B' = 'N�o', 'C' = 'Com outra mulher respons�vel'))

qplot(MORA_M�fE, data = Banco) + 
  labs(x = "Mora com a m�e?",title = "Figura 5: N�mero de estudantes que moram ou n�o com a m�e")

table(Banco$MORA_M�fE)

#4- Vari�veis Quantitativas:
#a)
IntervaloMT <- c('100-120', '120-140', '140-160', '160-180', '180-200',
               '200-220', '220-240', '240-260', '260-280', '280-300', '300-320', '320-340', '340-360')

##
Tabela_distribuicaoLP <- data.frame(IntervaloLP)%>%
  mutate(Frequ�ncia_Absoluta = hist(Banco$NOTA_LP, plot=FALSE)$counts)%>%
  mutate(Frequ�ncia_Relativa = hist(Banco$NOTA_LP, plot=FALSE)$density)

  
##
Tabela_distribuicaoMT <- data.frame(IntervaloMT)%>%
  mutate(Frequ�ncia_Absoluta = hist(Banco$NOTA_MT, plot=T)$counts)%>%
  mutate(Frequ�ncia_Relativa = hist(Banco$NOTA_MT, plot=T)$density)

#b) 
hist(Banco$NOTA_LP, breaks = 10,
     xlab = "Notas",
     ylab = "Frequ�ncia", 
     main = "Figura 6: Histograma de frequ�ncia de notas de L�ngua Portuguesa",
     xlim = c(90, 350),
     freq = F)

##
hist(Banco$NOTA_MT, breaks = 10,
     xlab = "Notas",
     ylab = "Freq�ncia", 
     main = "Figura 7: Histograma de frequ�ncia de notas de Matem�tica",
     xlim = c(100, 350),
     freq = F)

#c)
# Posi��o LP
Q1 <- quantile(Banco$NOTA_LP, probs = 0.25)
Q2 <- quantile(Banco$NOTA_LP, probs = 0.50)
Q3 <- quantile(Banco$NOTA_LP, probs = 0.75)

#####

# Posi��o  mat
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

cat("Assimetria Portug�s=",g1(Banco$NOTA_LP),"Curtose Portug�s=",g2(Banco$NOTA_LP),"\n")
cat("Assimetria Matem�tica=" ,g1(Banco$NOTA_MT),"Curtose Matem�tica=",g2(Banco$NOTA_MT),"\n")

summary(Banco$NOTA_MT)

#d

boxplot(Banco$NOTA_LP, 
        main = "Figura 8: Boxplot de notas da mat�ria L�ngua Portuguesa",
        ylab = "Notas Portug�s")

boxplot(Banco$NOTA_MT, 
        main = "Figura 9: Boxplot de notas da mat�ria Matem�tica",
        ylab = "Notas Matem�tica")
  

