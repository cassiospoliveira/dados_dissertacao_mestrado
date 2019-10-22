# Alteracoes para incorporar votos de ministros no banco bd_adinorma_coalizao (20/10/2018)
rm(list = ls())
install.packages("tidyverse")
install.packages("data.table")
install.packages("RColorBrewer")
install.packages("colorRamps")
install.packages("colorspace")
library(dplyr)
library(data.table)
library(readr)
library(tidyr)
library(stringi)
library(stringr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(colorRamps)
library(colorspace)

bancos <- list.files()[grep(".csv", list.files())]
bancos
adinorma <- fread(bancos[1])
bdjeferson<- fread(bancos[2])
codjeferson<- fread(bancos[3])
bd_presind<- fread(bancos[6])


# Análise

# Nível 1 - Dados Agregados por normas
# n = 68

# Frequência das Normas

glimpse(adinorma)
freqnorma <- table(adinorma$tiponorma_ext)

write.csv2(freqnorma, "tabela_freq_tipos_normas.csv", row.names = FALSE)

#Sucesso por requerente
por_req <- adinorma %>%
  mutate(autor1 = as.character(autor1)) %>%
  mutate(requerente = recode(autor1,
                       "105" = "Mesa de Assembléia Legislativa do Estado",
                       "106" = "Governador",
                       "107" = "Procurador-Geral da República",
                       "108" = "Conselho Federal da OAB",
                       "109" = "Partido Político",
                       "110" = "Confederação Sindical",
                       "111" = "Associação/Entidade de Classe de Âmbito Nacional")) %>%
  group_by(requerente) %>%
  summarise(n = n(),
            sucesso = sum(resultado == "Procedente" | resultado == "Procedente em parte")/n(),
            proc_em_parte = sum(resultado == "Procedente em parte")/n(),
            md_taprov = median(dif_aprov)
            #, md_tdec = median(tdecisao)
            )

View(por_req)

write.csv2(por_req, "resultados_por_requerente.csv", row.names = FALSE)


# Sucesso por tipo de norma impugnada
por_norma <- adinorma %>%
  #mutate(tiponorma_ext = as.character(autor1)) %>%
  #mutate(requerente = recode(autor1,
   #                          "105" = "Mesa de Assembléia Legislativa do Estado",
    #                         "106" = "Governador",
     #                        "107" = "Procurador-Geral da República",
      #                       "108" = "Conselho Federal da OAB",
       #                      "109" = "Partido Político",
        #                     "110" = "Confederação Sindical",
         #                    "111" = "Associação/Entidade de Classe de Âmbito Nacional")) %>%
  group_by(tiponorma_ext) %>%
  summarise(n = n(),
            sucesso = sum(resultado == "Procedente" | resultado == "Procedente em parte")/n(),
            proc_em_parte = sum(resultado == "Procedente em parte")/n(),
            md_taprov = median(dif_aprov)
            #, md_tdec = median(tdecisao)
            ) %>%
  filter(n>5)

View(por_norma)

write.csv2(por_norma, "resultados_por_tipo_de_norma.csv", row.names = FALSE)

# Boxplots
# Filtrando por normas apenas propostas pela coalizão
adinorma_coalizao <- adinorma %>%
  filter(coalizao == 1)

# Todas as normas
ggplot() + geom_boxplot(data = adinorma, outlier.shape = NA) + aes(x = resultado, y = dif_aprov) +
  labs(x = "Resultado", y ="Tempo de aprovação (em dias)") +
  coord_cartesian(ylim = quantile(adinorma$dif_aprov, c(0, 0.93)))

setwd("graficos")
ggsave("boxplot_todas_as_normas.png")

# Normas de iniciativa da coalizão
ggplot() + geom_boxplot(data = adinorma_coalizao, outlier.shape = NA) + aes(x = resultado, y = dif_aprov) +
  labs(x = "Resultado", y ="Tempo de aprovação (em dias)") +
  coord_cartesian(ylim = quantile(adinorma$dif_aprov, c(0, 0.93)))

ggsave("boxplot_normas_da_coalizao.png")

# Gráficos de barras de mandatos presidenciais e anos de julgamentos
# Apenas para normas de iniciativa de coalizões de governo

adinorma_coalizao <- adinorma_coalizao %>%
  mutate(governo = recode(governo, "FHC" = "FHC I", "Lula" = "Lula I", "Dilma" = "Dilma I"),
         ano = as.integer(substr(decisao, nchar(decisao)-3, nchar(decisao))))
adinorma_coalizao$governo <- factor(adinorma_coalizao$governo,
                                    levels = c("Sarney",
                                             "Collor",
                                             "Itamar",
                                             "FHC I",
                                             "FHC II",
                                             "Lula I",
                                             "Lula II",
                                             "Dilma I"))

# Normas de Iniciativa de Coalizões de Governo em ADIs por Mandato Presidencial

# Configurando a ordem das categorias
adinorma_coalizao <- adinorma_coalizao %>%
  mutate(resultado = factor(resultado, levels = c("Procedente", "Procedente em parte", "Improcedente")))

gg1 <- ggplot(adinorma_coalizao, aes(x=governo, fill = resultado))+
  geom_bar(stat="count", position=position_dodge2(width = 1, preserve = "single")) +
  labs(x="Mandatos presidenciais", y="Normas julgadas",
       #title = "Normas de Iniciativa de Coalizões de Governo em ADIs (1988-2014)",
       fill = "Resultado") + scale_y_continuous(breaks = pretty_breaks()) +
  theme_bw()
gg1 + scale_fill_hue(c = 60, l = 62, h=c(490, 0))

ggsave("normas_coalizao_por_mandato.png")

# Normas de Iniciativa de Coalizões de Governo em ADIs por Ano

gg2 <- ggplot(adinorma_coalizao, aes(x=ano, fill = resultado)) +
  geom_bar(stat="count") +
  labs(x="Ano", y="Normas julgadas",
       #title = "Normas de Iniciativa de Coalizões de Governo em ADIs (1988-2014)",
       fill = "Resultado") + scale_x_continuous(breaks = pretty_breaks()) +
  theme_bw()
gg2 + scale_fill_hue(c = 60, l = 62, h=c(490, 0))

ggsave("normas_coalizao_por_ano.png")
   

# Nível 2 - Dados Agregados por votos de ministros em adis impugnando normas
# n = 680
# Criação de variáveis

# Criando governo de aprovação das normas
adinorma <- adinorma %>%
  mutate(anoaprov = as.numeric(str_sub(dataaprov, 7, 10))) %>%
  mutate(govaprov = cut(anoaprov, c(1988, 1990, 1992, 1994, 1998, 2002, 2006, 2010, Inf),
                        labels = c("Sarney", "Collor", "Itamar", "FHC I", "FHC II", "Lula I", "Lula II", "Dilma I")))

# Criando variável prespt no banco de indicações presidenciais
# Variável divide presidentes de indicação entre petistas, psdbistas e outros.
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


# Criando % de votos favoráveis ao PT por governo de PROPOSIÇÃO da norma
bd_long <-bd_long %>%
  mutate(prespt = recode(presInd, "LULA I" = "PT", "LULA II" = "PT", "DILMA I" = "PT", "DILMA II" = "PT", "FHC" = "PSDB", "FHC I" = "PSDB", "FHC II" = "PSDB", .default = "OUTROS")) %>%
  mutate(governo = str_to_upper(governo)) %>%
  mutate(iniPT = recode(as.character(governo), "LULA" = "PT", "DILMA" = "PT", "LULA I" = "PT", "LULA II" = "PT", "DILMA I" = "PT", "DILMA II" = "PT", .default = "OUTROS")) %>%
  mutate(coalizao = as.logical(coalizao)) %>%
  mutate(favPT = ifelse(iniPT == "PT" & votomin != coalizao, 1, 0)) %>%
  mutate(favPT = ifelse(iniPT != "PT" & votomin == coalizao, 1, favPT)) #%>% #votos favoráveis
  #mutate(favPT = ifelse(iniPT == "PT" & votomin == coalizao, -1, favPT)) %>%
  #mutate(favPT = ifelse(iniPT != "PT" & votomin != coalizao, -1, favPT))#votos desfavoráveis

table(bd_long$favPT)

# Criando % de votos favoráveis ao PT por governo de APROVAÇÃO da norma
# Assumimos que normas aprovadas durante um governo seguem o interesse de
# coalizões desse governo
bd_long <-bd_long %>%
  #mutate(prespt = recode(presInd, "LULA I" = "PT", "LULA II" = "PT", "DILMA I" = "PT", "DILMA II" = "PT", "FHC" = "PSDB", "FHC I" = "PSDB", "FHC II" = "PSDB", .default = "OUTROS")) %>%
  mutate(govaprov = str_to_upper(govaprov)) %>%
  mutate(aprovPT = recode(govaprov, "LULA" = "PT", "DILMA" = "PT", "LULA I" = "PT", "LULA II" = "PT", "DILMA I" = "PT", "DILMA II" = "PT", .default = "OUTROS")) %>%
  mutate(favPT2 = ifelse(aprovPT == "PT", !votomin, votomin))
  #mutate(favPT2 = ifelse(favPT2 == 1, as.numeric(favPT2), -1))


#---Análises - Segundo Nível
  
# % de Favoráveis ao PT por governo de aprovação da norma
# Apenas ministros com mais de 24 votos
favorab_aprov <-bd_long %>%
  group_by(ministroCaps) %>%
  summarise(favoravelaprov = sum(favPT2)/n(), votos = n()) %>%
  filter(votos>24) %>%
  left_join(bd_presind) %>%
  select(-codMinistro) %>%
  arrange(favoravelaprov)

View(favorab_aprov)

setwd('..')
write.csv2(favorab_aprov, "tbl_favorabilidade_aprovacao.csv", row.names = FALSE)

# % de Favoráveis ao PT por governo de proposição da norma
# Apenas ministros com mais de 24 votos
favorab_prop <- bd_long %>%
  group_by(ministroCaps) %>%
  summarise(favoravel = sum(favPT)/n(), votos = n()) %>%
  filter(votos>24) %>%
  left_join(bd_presind) %>%
  select(-codMinistro) %>%
  arrange(favoravel)

View(favorab_prop)

write.csv2(favorab_prop, "tbl_favorabilidade_proposicao.csv", row.names = FALSE)

#agrupadas por ministros indicados pelo PT, PSDB e Outros
favorab_prob_por_partido <- favorab_prop %>%
  group_by(prespt) %>%
  summarise(favoravel = mean(favoravel), votos = sum(votos))  %>%
  arrange(favoravel)

View(favorab_prob_por_partido)

write.csv2(favorab_prob_por_partido, "tbl_favorabilidade_proposicao_partido.csv", row.names = FALSE)

# Mudanças efetuadas no excel

favorab_partido_teste <- read.csv2("tbl_favorabilidade_proposicao_partido.csv")
favorab_partido_teste <- favorab_partido_teste %>%
  t.test()


favorab_prob_por_partido <- favorab_prob_por_partido %>%
  mutate(favoravel = )

# Decisões favoráveis ao presidente de indicação
acordo_bd_long <-bd_long %>%
  rowwise() %>%
  mutate(presInd = recode(presInd, "LULA I" = "LULA", "LULA II" = "LULA", "DILMA I" = "DILMA", "FHC I" = "FHC", "FHC II" = "FHC")) %>%
  mutate(governo = str_to_upper(governo)) %>%
  mutate(governo = recode(governo, "LULA I" = "LULA", "LULA II" = "LULA", "DILMA I" = "DILMA", "FHC I" = "FHC", "FHC II" = "FHC")) %>%
  mutate(coalizao = as.logical(coalizao)) %>%
  mutate(favPresInd = ifelse(governo == presInd & votomin != coalizao, 1, NA)) %>%
  mutate(favPresInd = ifelse(governo == presInd & votomin == coalizao, 0, favPresInd)) %>%
  filter(!is.na(favPresInd))

# Tabela de votos favoráveis ao PresInd
# Todos os ministros
favorab_presind <- acordo_bd_long %>%
  group_by(ministroCaps) %>%
  summarise(votos = n(), favoravel = sum(favPresInd)/votos) %>%
  left_join(bd_presind) %>%
  select(-codMinistro) %>%
  arrange(favoravel)

write.csv2(favorab_presind, "tbl_favorabilidade_presidente_indicacao.csv", row.names = FALSE)
