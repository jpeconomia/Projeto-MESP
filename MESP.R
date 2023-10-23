
# Pegando os pacotes

library(tidyverse)
library(dplyr)
library(readxl)

# Importando os dados

Tabelas_Projeto_MESP_2022_09_02 <- read_excel("MESP/Tabelas - Projeto MESP 2022.09.02.xlsx", 
                                              sheet = "Dados Brutos - Comex")
View(Tabelas_Projeto_MESP_2022_09_02)

# Renomeando

Tab <- select(Tabelas_Projeto_MESP_2022_09_02, "Código SH2":"Ano")


Tab <- rename(Tab, FOB = `Valor FOB (US$)`)
Tab <- rename(Tab, BLC = `Bloco Econômico Destino`)
Tab <- rename(Tab, kg = `Quilograma Líquido`)
Tab <- rename(Tab, PAIS = `País Destino`)
Tab <- rename(Tab, QE = `Quantidade Estatística`)
Tab <- rename(Tab, COD = `Código - Classificação Tecnológica`)
Tab <- rename(Tab, DESC = `Descrição - Classificação Tecnológica`)
Tab <- rename(Tab, NCM = `Código NCM`)
Tab <- rename(Tab, SH2 = `Código SH2`)
# Quais são as descrições

View(Tab %>% group_by(COD) %>% summarize(descricao_y = unique(DESC)) %>% ungroup())

# Filtrando as transações que estão repetidas por causa do nome do bloco

Tab <- filter(Tab, !(BLC %in% c("Mercado Comum do Sul - Mercosul","União Europeia - UE","Comunidade Andina das Nações - CAN","Associação de Nações do Sudeste Asiático - ASEAN")))

# Renomeando as transações da Ásia

Tab$BLC[Tab$BLC == "Ásia (Exclusive Oriente Médio)"] <- "Ásia"

# Lista das regiões

BLC <- as.data.frame(unique(c(Tab$BLC)))

# Gerando alguns atalhos (lembrando que summarise pode estar associado a mais de um pacote)

Tab1 <- Tab %>% group_by(BLC) %>% summarise(s = sum(FOB))

Tab2 <- Tab %>% group_by(BLC, Ano) %>% summarise(s = sum(FOB))

# Eportações totais de Goiás para cada ano

TabEX <- Tab %>% group_by(Ano) %>% summarise(s1 = sum(FOB))

# Gráficos de linha

ggplot(data = TabEX)+
  geom_line(mapping = aes(x = Ano, y = s1/1000000000), colour = "red")+
  labs(title = "Exportações de Goiás, 1997-2021", x = "Ano", y = "Bilhões de R$")

ggplot(data = filter(Tab2, BLC == "América do Norte")) + 
  geom_line(mapping = aes(x = Ano, y = s/1000000)) +
  labs(title = "Exportações para Ámérica do Norte 1997-2021", x = "Ano", y = "Milhões de R$")

ggplot(data = filter(Tab2, BLC == "América do Sul")) + 
  geom_line(mapping = aes(x = Ano, y = s/1000000)) +
  labs(title = "Exportações para Ámérica do Sul 1997-2021", x = "Ano", y = "Milhões de R$")

ggplot(data = filter(Tab2, BLC %in% c("Europa","Ásia"))) + 
  geom_smooth(mapping = aes(x = Ano, y = s/1000000, fill = BLC)) +
  geom_line(mapping = aes(x = Ano, y = s/1000000, group = BLC), colour = "red")+
  labs(title = "Exportações para Europa e Ásia 1997-2021", x = "Ano", y = "Milhões de R$")

ggplot(data = filter(Tab2, BLC == "África")) + 
  geom_line(mapping = aes(x = Ano, y = s/1000000)) +
  labs(title = "Exportações para África 1997-2021", x = "Ano", y = "Milhões de R$")

ggplot(data = filter(Tab2, BLC == "Ásia")) + 
  geom_line(mapping = aes(x = Ano, y = s/1000000)) +
  labs(title = "Exportações para Ásia 1997-2021", x = "Ano", y = "Milhões de R$")

ggplot(data = filter(Tab2, BLC == "Oriente Médio")) + 
  geom_line(mapping = aes(x = Ano, y = s/1000000)) +
  labs(title = "Exportações para Oriente Médio 1997-2021", x = "Ano", y = "Milhões de R$")

ggplot(data = filter(Tab2, BLC == "Oceania")) + 
  geom_line(mapping = aes(x = Ano, y = s/1000000)) +
  labs(title = "Exportações para Oceania 1997-2021", x = "Ano", y = "Milhões de R$")

# Gráficos de Barra

#Para todo o período

ggplot(data = Tab1) + 
  geom_bar(mapping = aes(x = BLC, y = s/1000000000),fill = "lightblue", stat = "identity") + 
  labs(title = "Exportações por Região 1997-2021", x = "Região", y = "Bilhões de R$") +
  theme(axis.text.x = element_text(angle = -20, hjust = 0.35))


#Para qualquer ano específico

ggplot(data = filter(Tab2, Ano == 2021)) + 
  geom_bar(mapping = aes(x = BLC, y = s/1000000000), stat = "identity") + 
  labs(title = "Exportações por Região 2021", x = "Região", y = "bi de R$") +
  theme(axis.text.x = element_text(angle = -20, hjust = 0.35))

#Média dos últimos anos 

Tab3 <- filter(Tab2, Ano %in% c(2015:2021))  %>% group_by(BLC) %>% summarise(m = mean(s))

ggplot(data = Tab3) +
  geom_bar(mapping = aes(x = BLC, y = m/1000000),fill = "lightblue", stat = "identity") +
  labs(title = "Média de Exportações 2015 a 2021", x = "Região", y = "Milhões R$") + 
  theme(axis.text.x = element_text(angle = -20, hjust = 0.35))

# Exportações para a Europa para qualquer ano

Tab4 <- filter(Tab, BLC == "Europa", Ano %in% c(1997:2021)) %>% group_by(COD, Ano) %>% summarise(s = sum(FOB))

Tab4$DESC <- recode(Tab4$COD, "PP" = "Produtos Primários", 
                    "RB" = "Manufaturas Baseadas em Recursos Primários", 
                    "HT" = "Manufaturas de Alta Tecnologia", 
                    "LT" = "Manufaturas de Baixa Tecnologia", 
                    "MT" = "Manufaturas de Média Tecnologia",
                    "Other" = "Outros")

ggplot(data = filter(Tab4, Ano == 2017))+
  geom_bar(mapping = aes(x = COD, y = s/1000000000, fill = DESC), stat = "identity") +
  labs(title = "Exportações para Europa por categoria em 2017", x = "Categorias", y = "Valor bi R$")
  
# Exportações para a Europa por categoria para os anos

ggplot(data = Tab4) +
  geom_line(mapping = aes(x = Ano, y = s/1000000), colour = "red") +
  facet_wrap(~DESC, nrow = 2)+
  labs(title = "Exportações para a Europa, 1997-2021", y = "Milhões de R$")

# Exportações para Ásia para qualquer ano

Tab5 <- filter(Tab, BLC == "Ásia", Ano %in% c(1997:2021)) %>% group_by(COD, Ano) %>% summarise(s = sum(FOB))

Tab5$DESC <- recode(Tab5$COD, "PP" = "Produtos Primários", 
                    "RB" = "Manufaturas Baseadas em Recursos Primários", 
                    "HT" = "Manufaturas de Alta Tecnologia", 
                    "LT" = "Manufaturas de Baixa Tecnologia", 
                    "MT" = "Manufaturas de Média Tecnologia",
                    "Other" = "Outros")

ggplot(data = filter(Tab5, Ano == 2021))+
  geom_bar(mapping = aes(x = COD, y = s/1000000000, fill = DESC), stat = "identity") +
  labs(title = "Exportações para Ásia por categoria em 2021", x = "Categorias", y = "Valor bi R$")

# Exportações para Ásia ao longo dos anos 

ggplot(data = Tab5) +
  geom_line(mapping = aes(x = Ano, y = s/1000000)) +
  facet_wrap(~DESC, nrow = 2)+
  labs(title = "Exportações para a Ásia, 1997-2021", y = "Milhões de R$")

# Exportações para a China

# Filtrando só para a China para algum conjunto de anos

TabCH <- filter(Tab, PAIS == "China", Ano %in% c(1997:2021))

# Exportações totais por ano

TabCH1 <- TabCH %>% group_by(Ano) %>% summarise(s = sum(FOB))

# Evolução das exportações para a China

ggplot(TabCH1) +
  geom_line(mapping = aes(x = Ano, y = s/1000000)) +
  labs(title = "Exportações para a China, 1997 - 2021 ", y = "Milhões de R$")

# Exportações para a China por produto ao longo dos anos

TabCH2 <- TabCH %>% group_by(Ano, COD, DESC) %>% summarise(s = sum(FOB))

ggplot(TabCH2) +
  geom_line(mapping = aes(x = Ano, y = s/1000000), colour = "red") +
  facet_wrap(~DESC, nrow = 2)+
  labs(title = "Exportações para a China, 1997-2021", x = "Ano", y = "Valor")

# Para quais países da Europa Goiás mais exporta

TabEU <- filter(Tab, BLC == "Europa") %>% group_by(Ano, COD, DESC, PAIS) %>% summarise(s = sum(FOB))
TabEU1 <- filter(Tab, BLC == "Europa") %>% group_by(Ano, PAIS) %>% summarise(s = sum(FOB))

# Países para os quais a exportação foi maior que valor em milhões em um ano, no agregado

# Gráfico de barra para o ano qualquer

ggplot(filter(TabEU1, Ano == 2021, s > 50000000))+
  geom_bar(mapping = aes(x = PAIS, y = s/1000000, group = PAIS), fill = "lightblue", stat = "identity")+
  labs(title = "Exportações de Goiás para a Europa maiores que 50 milhões em 2021",
       x = "País", y = "Milhões de R$")+
  theme(axis.text.x = element_text(angle = -20, hjust = 0.35))

# Evolução para algum país qualquer da Europa

ggplot(filter(TabEU1, PAIS == "França"))+
  geom_line(mapping = aes(x = Ano, y = s/1000000))+
  labs(title = "Exportação em milhões para país e período (França)", x = "Ano", y = "Milhões R$")


# Quanto a China representa nas exportações 

TabCH1$Participação <- TabCH1$s/TabEX$s1


ggplot(TabCH1)+
  geom_line(mapping = aes(x = Ano, y = Participação), colour = "red")+
  labs(title = "Participação da China nas exportações 1997-2021", y = "Percentual")

# Proporções por região

Tab2$soma <- c(1:200)

for(i in unique(c(Tab2$BLC))){

  Tab2$soma[Tab2$BLC == i] <- TabEX$s1
  
    
}

Tab2$shares <- Tab2$s/Tab2$soma

ggplot(filter(Tab2, BLC %in% c("Europa","Ásia", "América do Sul", "América do Norte")))+
  geom_line(mapping = aes(x = Ano, y = shares, colour = BLC))+
  labs(title = "Participação nas exportações totais, 1997-2021", y = "Percentuais")
  
#Shares dos tipos de produtos  

Tab6 <- Tab %>% group_by(Ano, COD) %>% summarise(s = sum(FOB))

Tab6$somas <- c(1:150)

# Colocando o total das exportações na base que tá separada por tipos

for(i in unique(c(Tab6$COD))){
  
  
    Tab6$somas[Tab6$COD == i] <- TabEX$s1  
    
}
  

Tab6$Tipo <- recode(Tab6$COD, "PP" = "Produtos Primários", 
                    "RB" = "Manufaturas Baseadas em Recursos Primários", 
                    "HT" = "Manufaturas de Alta Tecnologia", 
                    "LT" = "Manufaturas de Baixa Tecnologia", 
                    "MT" = "Manufaturas de Média Tecnologia",
                    "Other" = "Outros")


Tab6$shares <- Tab6$s/Tab6$somas



ggplot(Tab6)+
  geom_line(mapping = aes(x = Ano, y = shares, colour = Tipo))+
  labs(title = "Participação nas exportações por tipo, 1997-2021", x = "Ano", y = "Percentual")

#Olhando o quanto foi exportado para alguns países da Europa

Tab7 <- filter(Tab, PAIS %in% c("Reino Unido", "França","Espanha", "Alemanha", "Rússia", "Países Baixos (Holanda)", "Itália","Bélgica","Dinamarca"))

Tab7 <- Tab7 %>% group_by(PAIS, Ano) %>% summarise(s = sum(FOB))

ggplot(Tab7)+
  geom_line(mapping = aes(x = Ano, y = s/1000000), colour = "red")+
  labs(title = "Exportações para alguns países da Europa", y = "valor em milhões de R$")+
  facet_wrap(~PAIS, nrow = 3)

# Fazendo os índices de concentração: 

# Por classificação: 

Tab6$s2 <- Tab6$shares^2

HHIclass <- as.data.frame(c(1997:2021))
HHIclass <- rename(HHIclass, HHI = `c(1997:2021)`)
HHIclass$Ano <- c(1997:2021)

for(i in c(1997:2021)){
  
  HHIclass$HHI[HHIclass$Ano == i] <- sum(filter(Tab6, Ano == i)$s2) 
  
}

ggplot(HHIclass)+
  geom_line(mapping = aes(x = Ano, y = HHI), colour = "red")+
  labs(title = "Índice HHI para as classes de produto, 1997-2021")

# Para as regiões 

HHIreg <- as.data.frame(c(1997:2021))
HHIreg <- rename(HHIreg, HHI = `c(1997:2021)`)
HHIreg$Ano <- c(1997:2021)

for(i in c(1997:2021)){
  
  HHIreg$HHI[HHIreg$HHI == i] <- sum(filter(Tab2, Ano == i)$shares^2)
  
}

ggplot(HHIreg)+
  geom_line(mapping = aes(x = Ano, y = HHI), colour = "red")+
  labs(title = "Índice HHI para regiões econômicas, 1997-2021")

# Para os países presentes na amostra 

TabPais <- Tab %>% group_by(Ano, PAIS) %>% summarise(s = sum(FOB))

TabPais$EX <- c(1:3314)

for(i in c(1997:2021)){
  
  TabPais$EX[TabPais$Ano == i] <- TabEX$s1[TabEX$Ano == i]
  
}

TabPais$s2 <- (TabPais$s/TabPais$EX)^2

TabPais1 <- as.data.frame(c(1997:2021))
TabPais1 <- rename(TabPais1, Ano = `c(1997:2021)`)
TabPais1$HHI <- c(1997:2021)

for(i in c(1997:2021)){
  
  TabPais1$HHI[TabPais1$Ano == i] <- sum(TabPais$s2[TabPais$Ano == i])
  
}

ggplot(TabPais1)+
  geom_line(mapping = aes(x = Ano, y = HHI), colour = "red")+
  labs(title = "Índice HHI para os países, 1997-2021")


# Índice por produto do NCM

TabProd1 <-  Tab %>% group_by(Ano, NCM) %>% summarise(s = sum(FOB)) 

TabProd1$EX <- c(1:18352)

for(i in c(1997:2021)){
  
TabProd1$EX[TabProd1$Ano == i] <- TabEX$s1[TabEX$Ano == i]  
  
}

TabProd1$s2 <- (TabProd1$s/TabProd1$EX)^2



HHIprod <- as.data.frame(c(1997:2021))
HHIprod <- rename(HHIprod, Ano = `c(1997:2021)`)

HHIprod$HHI <- c(1997:2021)

for(i in c(1997:2021)){
  
  HHIprod$HHI[HHIprod$Ano == i] <- sum(TabProd1$s2[TabProd1$Ano == i])
  
}

# Classificação por produto fica ruim 

ggplot(HHIprod)+
  geom_line(mapping = aes(x = Ano, y = HHI), colour = "red")+
  labs(title = "Índice HHI para produtos pela classificação NCM")

# Comparando produtos com classificação 

HHI <- HHIclass
HHI$prod <- HHIprod$HHI

HHI <- rename(HHI, Classificação = HHI, Produto = prod)


ggplot(HHI)+
  geom_line(mapping = aes(x = Ano, y = Classificação), colour = "red")+
  geom_line(mapping = aes(x = Ano, y = Produto), colour = "blue")+
  labs(y = "HHI")


### pelo tipo do SH2 

TabSH <- Tab %>% group_by(Ano, SH2) %>% summarise(s = sum(FOB))

TabSH$EX <- 1

for(i in c(1997:2021)){

TabSH$EX[TabSH$Ano == i] <- TabEX$s1[TabEX$Ano == i]
  
}
  
TabSH$s2 <- (TabSH$s/TabSH$EX)^2  

HHIsh <- TabSH %>% group_by(Ano) %>% summarise(HHI = sum(s2))

ggplot(HHIsh)+
  geom_line(mapping = aes(x = Ano, y = HHI), colour = "red")+
  labs(title = "Índice HHI para a classificação SH2")
