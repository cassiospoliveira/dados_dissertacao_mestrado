# Alteracoes para incorporar votos de ministros no banco bd_adinorma_coalizao (20/10/2018)
rm(list = ls())
install.packages("tidyverse")
install.packages("data.table")
library(dplyr)
library(data.table)
library(readr)
library(tidyr)
library(stringi)
library(stringr)

bancos <- list.files()[grep(".csv", list.files())]
bancos
adinorma <- fread(bancos[1])
bdjeferson<- fread(bancos[2])
codjeferson<- fread(bancos[3])
bd_presind<- fread(bancos[4])

bd_presind <- bd_presind %>%
  filter(presInd != "") %>%
  mutate(prespt = recode(presInd, "LULA I" = "PT", "LULA II" = "PT", "DILMA I" = "PT", "DILMA II" = "PT", "FHC" = "PSDB", "FHC I" = "PSDB", "FHC II" = "PSDB", .default = "OUTROS")) %>%
  select(ministroCaps, codMinistro, presInd, prespt)

bdjeferson <- bdjeferson %>%
  select(V1, V48:V81) %>% # Selecionando votos de ministros em decisões finais
  rename(adi = V1)

bd_long <- bdjeferson %>%
  gather(var_id, voto_venc, V48:V81) %>%
  left_join(codjeferson) %>%
  left_join(bd_presind) %>%
  select(-var, -var_id) %>%
  mutate(adi = as.numeric(str_remove_all(adi, "Adin "))) %>%
  right_join(adinorma) %>%
  filter(voto_venc != "") %>% # Removendo células sem votação de ministros
  mutate(cod_resultado = as.logical(ifelse(resultado == "Procedente" | resultado == "Procedente em parte", 1,0))) %>%
  mutate(votomin = ifelse(voto_venc == "Vencedor", cod_resultado, !cod_resultado))
  
#table(bd_long$resultado, bd_long$cod_resultado) # Conferindo a codificação do resultado

# Ver quantas vezes cada ministro votou
View(bd_long %>%
  group_by(ministroCaps) %>%
  summarise(votos = n()) %>%
  arrange(votos))

bd_long <-bd_long %>%
  #mutate(prespt = recode(presInd, "LULA I" = "PT", "LULA II" = "PT", "DILMA I" = "PT", "DILMA II" = "PT", "FHC" = "PSDB", "FHC I" = "PSDB", "FHC II" = "PSDB", .default = "OUTROS")) %>%
  mutate(governo = str_to_upper(governo)) %>%
  mutate(iniPT = recode(governo, "LULA" = "PT", "DILMA" = "PT", "LULA I" = "PT", "LULA II" = "PT", "DILMA I" = "PT", "DILMA II" = "PT", .default = "OUTROS")) %>%
  mutate(coalizao = as.logical(coalizao)) %>%
  mutate(favPT = ifelse(iniPT == "PT" & votomin != coalizao, 1, 0)) %>%
  mutate(favPT = ifelse(iniPT != "PT" & votomin == coalizao, 1, favPT)) %>% #votos favoráveis
  mutate(favPT = ifelse(iniPT == "PT" & votomin == coalizao, -1, favPT)) %>%
  mutate(favPT = ifelse(iniPT != "PT" & votomin != coalizao, -1, favPT))#votos desfavoráveis

# Análise

# Nível 1 - Dados Agregados por Julgamento de Norma Federal
# n = 70




#com presidente de indicação
#todas as votações
all_comPresid <-bd_long %>%
       group_by(ministroCaps) %>%
       summarise(favoravel = sum(favPT)/n(), votos = n()) %>%
       left_join(bd_presind) %>%
       select(-codMinistro) %>%
       arrange(favoravel)

write.csv2(all_comPresid, "tabela1_todos_favorabilidade.csv", row.names = FALSE)


#só acima de 24 votações
comPresid <- bd_long %>%
       group_by(ministroCaps) %>%
       summarise(favoravel = sum(favPT)/n(), votos = n()) %>%
       filter(votos>24) %>%
       left_join(bd_presind) %>%
       select(-codMinistro) %>%
       arrange(favoravel)
View(comPresid)

write.csv2(comPresid, "tabela2_acimade24votos_favorabilidade.csv", row.names = FALSE)

#agrupadas por ministros indicados pelo PT, PSDB e Outros
porPresid <- comPresid %>%
  group_by(prespt) %>%
  summarise(favoravel = mean(favoravel), votos = sum(votos))  %>%
  arrange(favoravel)
View(porPresid) 

write.csv2(porPresid, "tabela3_acimade24votos_favorabilidadeporpresid.csv", row.names = FALSE)

table(bd_long$favPT)
table(bd_long$prespt)
table(bd_long$iniPT)



mutate(favpres = ifelse(,1,-1)

View(bd_long)


glimpse(adi)  

levels(as.factor(bd_long$votomin))
View(table(adinorma$coalizao, adinorma$proponente1))

table(bd_long$voto_venc, bd_long$cod_resultado, bd_long$votomin)

glimpse(adinorma)
glimpse(bd_long)
glimpse(bd_presind)