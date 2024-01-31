#BIBLIOTECAS
library("openxlsx")
library("MatchIt")
library("Rcpp")
library("readxl")
library("tidyverse")
library("boot")
library("dplyr")

#LE BASE VINDA DA QUERY 8
base <-
  read_excel(
    "C:/Users/02.000623/Desktop/2022/Campanhas/Uber Digio Final//analise bootstrap uber digio TESTE.xlsx"
  )

a<-base %>%  count(safra) #GERA CONTAGEM DE SAFRAS PARA O LOOP

#GERA VETORES VAZIOS ONDE A INFORMAÇÃO SERÁ INSERIDA 
lim_inf <- c()
lim_sup <- c()
fat_incremental <- c()
safra <-c()
mes<- c()
df <- data.frame(safra,mes,lim_inf,fat_incremental, lim_sup)

#PARA CADA SAFRA
for (x in a$safra) {

    safra_analise <- x
    
    base_2 <- base %>% select(-numerocartao_sha2) %>% filter(safra==safra_analise) #FILTRO DE SAFRA
    
    #FUNCAO QUE ESTIMA FATURAMENTO INCREMENTAL DE M0 A M5
    foo <- function(data, indices) 
           {
           dt <- data[indices, ]
           alvo <-
             dt %>%  filter(flag_promo == 1) %>% select(-safra,-flag_promo)
           controle <-
             dt %>% filter(flag_promo == 0) %>% select(-safra,-flag_promo)
           c(
              sum(alvo$fat_m0) - sum(controle$fat_m0) * sum(alvo$fat_m1) / sum(controle$fat_m1)            #M0
             ,sum(alvo$fat_m_mais_1) - sum(controle$fat_m_mais_1) * sum(alvo$fat_m1)/sum(controle$fat_m1)  #M1
             ,sum(alvo$fat_m_mais_2) - sum(controle$fat_m_mais_2) * sum(alvo$fat_m1)/sum(controle$fat_m1)  #M2
             ,sum(alvo$fat_m_mais_3) - sum(controle$fat_m_mais_3) * sum(alvo$fat_m1)/sum(controle$fat_m1)  #M3
             ,sum(alvo$fat_m_mais_4) - sum(controle$fat_m_mais_4)*sum(alvo$fat_m1)/sum(controle$fat_m1)    #M4
             ,sum(alvo$fat_m_mais_5) - sum(controle$fat_m_mais_5)*sum(alvo$fat_m1)/sum(controle$fat_m1)    #M5
           )
           }
    #GERA BOOTSTRAPS
    myBootstrap <- boot(base_2, foo, R = 5000) #A partir de 1000 reamostragens é ok
    
    #CRIA A BASE DE FAT INCREMENTAL, LIMITE INF E LIMITE SUPERIOR 
    lim_inf <- c()
    lim_sup <- c()
    fat_incremental <- c()
    safra <-c()
    mes<- c()
      #INSERE DADOS NA BASE
      for(i in 1:6)  
      {
      x<-boot.ci(myBootstrap,
      index = i,
      type = c('perc', 'norm')) 
      lim_inf <- c(lim_inf, x[["normal"]][2]) 
      lim_sup <- c(lim_sup, x[["normal"]][3]) 
      fat_incremental <- c(fat_incremental, x[["t0"]]) 
      safra<- c(safra, safra_analise) 
      mes<- c(mes, i-1)
      }
    #CONCATENA BASE LOOP
    df_2 <- data.frame(safra,mes,lim_inf,fat_incremental, lim_sup)
    
    df <- rbind(df, df_2)           } #FIM LOOP

#JOGA BASE PRO EXCEL -> COLAR NA ABA "dados IC"
write.xlsx(df, 'C:/Users/02.000623/Desktop/2022/Campanhas/Uber Digio Final/teste boot write.xlsx') 
