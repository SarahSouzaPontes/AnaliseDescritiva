#ENDEREÇO DO BANCO
getwd()

#PACOTES INSTALADO PARA ANÁLISE
library(dplyr)
library(psych)

#BANCO DE DAODOS EM ANÁLISE 17/10/2021
 
any(is.na(banco))

##BANCO TRATADO SEM MISSING!!!!!!!!!!!! UHUL!!!!!   IDADE           SEXO                PESO       
colSums(is.na(banco))

#LINHAS ANALISADAS
nrow(banco)

describe(banco)



###variável dicotômica DOMINANCIA
#############VARIÁVEL DICOTÔMICA DOM - TABELA 2 X 2 NÚMEROS ABSOLUTOS
table(banco$DOM)

#ANÁLISE DUAS VARIÁVEIS DICOTÔMICAS
##############SEXO vs DOMINÂNCIA
table(banco$SEXO, banco$DOM)



#DADOS PESSOAIS DA AMOSTRA
#IDADE
describe(banco$IDADE)
hist(banco$IDADE)
plot(banco$IDADE)
boxplot(banco$IDADE)

#ALTURA
describe(banco$ALTURA)
hist(banco$ALTURA)
plot(banco$ALTURA)
boxplot(banco$ALTURA)


#TESTE IPAQ - COMPOSTO POR 8 QUESTÕES 1 (A/B), 2(A/B), 3 (A/B), 4(A/B)
#################Estatística descritiva IPAQ / NÍVEL DE ATIVIDADE FÍSICA
#IPAQ1A
describe(banco$IPAQ1A)
hist(banco$IPAQ1A)
plot(banco$IPAQ1A)
boxplot(banco$IPAQ1A)


#IPAQ1B
describe(banco$IPAQ1B)
hist(banco$IPAQ1B)
plot(banco$IPAQ1B)
boxplot(banco$IPAQ1B)

#IPAQ2A
describe(banco$IPAQ2A)
hist(banco$IPAQ2A)
plot(banco$IPAQ2A)
boxplot(banco$IPAQ2A)


#IPAQ2B
describe(banco$IPAQ2B)
hist(banco$IPAQ2B)
plot(banco$IPAQ2B)
boxplot(banco$IPAQ2B)

#IPAQ3A
describe(banco$IPAQ3A)
hist(banco$IPAQ3A)
plot(banco$IPAQ3A)
boxplot(banco$IPAQ3A)


#IPAQ3B
describe(banco$IPAQ3B)
hist(banco$IPAQ3B)
plot(banco$IPAQ3B)
boxplot(banco$IPAQ3B)


#IPAQ4A
describe(banco$IPAQ4A)
hist(banco$IPAQ4A)
plot(banco$IPAQ4A)
boxplot(banco$IPAQ4A)

#IPAQ4B
describe (banco$IPAQ4B)
hist(banco$IPAQ4B)
plot(banco$IPAQ4B)
boxplot(banco$IPAQ4B)


#COMPOSTO POR VARIÁVEIS DO ISOCINÉTICO PT, TT - 60, 180 E 240 GRAUS, SENDO QUE EM 180 POSSUI AVG POWER
#PRECISEI CODIGICAR AS VARIÁVEIS PARA NÃO DÁ ERRO NA ANÁLISE COM AS LETRAS DO ALFABETO COM A DESCRIÇÃO AO LADO, SEGUE A APRESENTAÇÃO NO LAUDO.
######## DADOS DO ISOCINÉTICO#####

#A - PT EXTENSÃO 60 GRAUS DIREITA
describe(banco$A)
hist(banco$A)
plot(banco$A)
boxplot(banco$A)

#B -  PT EXTENSÃO 60 GRAUS ESQUERDA
describe(banco$B)
hist(banco$B)
plot(banco$B)
boxplot(banco$B)

#C - DÉFICIT PT EXTENSÃO 60 GRAUS
describe(banco$C)
hist(banco$C)
plot(banco$C)
boxplot(banco$C)

#D - PT FLEXÃO 60 GRAUS DIREITA 
describe(banco$D)
hist(banco$D)
plot(banco$D)
boxplot(banco$D)

#E -  PT FLEXÃO 60 GRAUS ESQUERDA
describe(banco$E)
hist(banco$E)
plot(banco$E)
boxplot(banco$E)

#F - DÉFICIT PT FLEXÃO 60 GRAUS
describe(banco$F)
hist(banco$F)
plot(banco$F)
boxplot(banco$F)

#G - TT EXTENSÃO 60 GRAUS DIREITA 
describe(banco$G)
hist(banco$G)
plot(banco$G)
boxplot(banco$G)

#H -  TT EXTENSÃO 60 GRAUS ESQUERDA
describe(banco$H)
hist(banco$H)
plot(banco$H)
boxplot(banco$H)

#I - DEFICIT TT EXTENSÃO 60 GRAUS 
describe(banco$I)
hist(banco$I)
plot(banco$I)
boxplot(banco$I)


#J - TT FLEXÃO 60 GRAUS DIREITA 
describe(banco$J)
hist(banco$J)
plot(banco$G)
boxplot(banco$J)


#L - TT FLEXÃO 60 GRAUS ESQUERDA
describe(banco$L)
hist(banco$L)
plot(banco$L)
boxplot(banco$L)

 
#M - DEFICT TT FLEXÃO 60 GRAUS 
describe(banco$M)
hist(banco$M)
plot(banco$M)
boxplot(banco$M)

#N - PT EXTENSÃO 180 GRAUS DIREITA 
describe(banco$N)
hist(banco$N)
plot(banco$N)
boxplot(banco$N)

#O - - PT EXTENSÃO 180 GRAUS ESQUERDA
describe(banco$O)
hist(banco$O)
plot(banco$O)
boxplot(banco$O)


#P - DEFICIT PT EXTENSÃO 180 GRAUS 
describe(banco$P)
hist(banco$P)
plot(banco$P)
boxplot(banco$P)


#Q - PT FLEXÃO 180 GRAUS DIREITA 
describe(banco$Q)
hist(banco$Q)
plot(banco$Q)
boxplot(banco$Q)

#R - - PT FLEXÃO 180 GRAUS ESQUERDA
describe(banco$R)
hist(banco$R)
plot(banco$R)
boxplot(banco$R)


#S - DEFICIT PT FLEXÃO 180 GRAUS 
describe(banco$S)
hist(banco$S)
plot(banco$S)
boxplot(banco$S)


#T - TT EXTENSÃO 180 GRAUS DIREITA 
describe(banco$T)
hist(banco$T)
plot(banco$T)
boxplot(banco$T)


#U - TT EXTENSÃO 180 GRAUS ESQUERDA 
describe(banco$U)
hist(banco$U)
plot(banco$U)
boxplot(banco$U)


#V - DEFICIT TT EXTENSÃO 180 GRAUS 
describe(banco$V)
hist(banco$V)
plot(banco$V)
boxplot(banco$V)

#X - TT FLEXÃO 180 GRAUS DIREITA 
describe(banco$X)
hist(banco$X)
plot(banco$X)
boxplot(banco$X)

#Z  - TT FLEXÃO 180 GRAUS ESQUERDA
describe(banco$Z)
hist(banco$Z)
plot(banco$Z)
boxplot(banco$Z)


#AA - DEFICIT FLEXÃO 180 
describe(banco$AA)
hist(banco$AA)
plot(banco$AA)
boxplot(banco$AA)

#BB - AVG POWER EXTENSÃO DIREITA
describe(banco$BB)
hist(banco$BB)
plot(banco$BB)
boxplot(banco$BB)


#CC - AVG POWER EXTENSÃO ESQUERDA
describe(banco$CC)
hist(banco$CC)
plot(banco$CC)
boxplot(banco$CC)



#DD - DEFICIT AVG POWER EXTENSÃO 
describe(banco$DD)
hist(banco$DD)
plot(banco$DD)
boxplot(banco$DD)

#EE AVG POWER FLEXÃO DIREITA
describe(banco$EE)
hist(banco$EE)
plot(banco$EE)
boxplot(banco$EE)


#FF - AVG POWER FLEXÃO ESQUERDA
describe(banco$FF)
hist(banco$FF)
plot(banco$FF)
boxplot(banco$FF)



#GG - DEFICIT AVG POWER EXTENSÃO 
describe(banco$GG)
hist(banco$GG)
plot(banco$GG)
boxplot(banco$GG)


#HH - PT EXTENSÃO 240 DIREITA 
describe(banco$HH)
hist(banco$HH)
plot(banco$HH)
boxplot(banco$HH)


#II - PT EXTENSÃO 240 ESQUERDA 
describe(banco$II)
hist(banco$II)
plot(banco$II)
boxplot(banco$II)


#JJ- DEFICIT - PT EXTENSÃO  240 DIREITA 
describe(banco$JJ)
hist(banco$JJ)
plot(banco$JJ)
boxplot(banco$JJ)



#LL -  PT FLEXÃOO 240 DIREITA 
describe(banco$LL)
hist(banco$LL)
plot(banco$LL)
boxplot(banco$LL)

#MM - PT FLEXÃO 240 DIREITA 
describe(banco$MM)
hist(banco$MM)
plot(banco$MM)
boxplot(banco$MM)


#NN - DEFICIT  PT EXTENSÃO 240 
describe(banco$NN)
hist(banco$NN)
plot(banco$NN)
boxplot(banco$NN)


#OO - TT EXTENSÃO  240 DIREITA 
describe(banco$OO)
hist(banco$OO)
plot(banco$OO)
boxplot(banco$OO)

#PP -TT EXTENSÃO 240 ESQUERDA
describe(banco$PP)
hist(banco$PP)
plot(banco$PP)
boxplot(banco$PP)

#QQ - DEFICIT TT EXTENSÃO  240 
describe(banco$QQ)
hist(banco$QQ)
plot(banco$QQ)
boxplot(banco$QQ)

#RR - TT FLEXÃO 240 DIREITA
describe(banco$RR)
hist(banco$RR)
plot(banco$RR)
boxplot(banco$RR)


#SS - TT FLEXÃO  240 ESQUERDA
describe(banco$SS)
hist(banco$SS)
plot(banco$SS)
boxplot(banco$SS)


#TT - DEFICIT TT FLEXÃO  240 
describe(banco$TT)
hist(banco$TT)
plot(banco$SS)
boxplot(banco$TT)

summary(banco)






