##Carga de dados sobre índice GINI

library(tidyverse)
library("basedosdados")
library(cluster)

# Defina o seu projeto no Google Cloud
set_billing_id("nice-diorama-306223")
# Para carregar o dado direto no R
query <- "SELECT * FROM `basedosdados.mundo_onu_adh.municipio`"
idh_municipios <- read_sql(query, page_size=1000)


glimpse(idh_municipios)

library(viridis)

idh_municipios%>%
  select(ano, idhm, idhm_e, idhm_l, idhm_r) %>%
  mutate(ano = as.factor(ano)) %>%
  pivot_longer(cols= 2:5,names_to = "idh", values_to = "valor") %>%
  ggplot() +
  geom_boxplot(aes(x=ano, y=valor,fill= ano)) +
  scale_fill_viridis(discrete = TRUE)+
  facet_wrap(idh~.)


###Clusters de 2010

idh_filtro<-
  idh_municipios%>%
  filter(ano == 2010) %>%
  select(idhm_e, idhm_l, idhm_r)


sil_info<-
  map_dbl(3:6, function(k){
    print(k)
    model_idh<- cluster::pam(idh_filtro,k=k)
    model_idh$silinfo$avg.width
  })


set.seed(1972)
model_idh_2010<- cluster::pam(idh_filtro,k=3)

idh_cluster_2010<- idh_filtro

idh_cluster_2010$cluster <- model_idh_2010$clustering

idh_cluster_2010 %>%
  mutate(cluster = as.factor(cluster)) %>%
  select(cluster,  idhm_e, idhm_l, idhm_r) %>%
  pivot_longer(cols= 2:4,names_to = "idh", values_to = "valor") %>%
  ggplot() +
  geom_boxplot(aes(x=idh, y=valor)) +
  scale_fill_viridis(discrete = TRUE)+
  facet_wrap(cluster~.)



#########Clusters de 2000

idh_filtro<-
  idh_municipios%>%
  filter(ano == 2000) %>%
  select(idhm_e, idhm_l, idhm_r)


sil_info<-
  map_dbl(3:6, function(k){
    print(k)
    model_idh<- cluster::pam(idh_filtro,k=k)
    model_idh$silinfo$avg.width
  })


set.seed(1972)
model_idh_2000<- cluster::pam(idh_filtro,k=3)

idh_cluster_2000<- idh_filtro

idh_cluster_2000$cluster <- model_idh_2000$clustering

idh_cluster_2000 %>%
  mutate(cluster = as.factor(cluster)) %>%
  select(cluster,  idhm_e, idhm_l, idhm_r) %>%
  pivot_longer(cols= 2:4,names_to = "idh", values_to = "valor") %>%
  ggplot() +
  geom_boxplot(aes(x=idh, y=valor)) +
  scale_fill_viridis(discrete = TRUE)+
  facet_wrap(cluster~.)


#########Clusters de 1991

idh_filtro<-
  idh_municipios%>%
  filter(ano == 1991) %>%
  select(idhm_e, idhm_l, idhm_r)


sil_info<-
  map_dbl(3:6, function(k){
    print(k)
    model_idh<- cluster::pam(idh_filtro,k=k)
    model_idh$silinfo$avg.width
  })


set.seed(1972)
model_idh_1991<- cluster::pam(idh_filtro,k=3)

idh_cluster_1991<- idh_filtro

idh_cluster_1991$cluster <- model_idh_1991$clustering

idh_cluster_1991 %>%
  mutate(cluster = as.factor(cluster)) %>%
  select(cluster,  idhm_e, idhm_l, idhm_r) %>%
  pivot_longer(cols= 2:4,names_to = "idh", values_to = "valor") %>%
  ggplot() +
  geom_boxplot(aes(x=idh, y=valor)) +
  scale_fill_viridis(discrete = TRUE)+
  facet_wrap(cluster~.)



idh_filtro_sintese<-
  idh_municipios%>%
  filter(ano == 1991) %>%
  select(idhm,idhm_e, idhm_l, idhm_r)


sil_info<-
  map_dbl(3:6, function(k){
    print(k)
    model_idh<- cluster::pam(idh_filtro_sintese[1],k=k)
    model_idh$silinfo$avg.width
  })


set.seed(1972)
model_idh_sintese<- cluster::pam(idh_filtro_sintese[1],k=3)

idh_cluster_sintese<- idh_filtro_sintese

idh_cluster_sintese$cluster <- model_idh_sintese$clustering

idh_cluster_sintese %>%
  mutate(cluster = as.factor(cluster)) %>%
  select(cluster,  idhm_e, idhm_l, idhm_r ) %>%
  pivot_longer(cols= 2:4,names_to = "idh", values_to = "valor") %>%
  ggplot() +
  geom_boxplot(aes(x=cluster, y=valor)) +
  scale_fill_viridis(discrete = TRUE)+
  facet_wrap(idh~.)

idh_cluster_1991 %>%
  mutate(cluster = factor(cluster, levels = c(2,1,3))) %>%
  select(cluster,  idhm_e, idhm_l, idhm_r) %>%
  pivot_longer(cols= 2:4,names_to = "idh", values_to = "valor") %>%
  ggplot() +
  geom_boxplot(aes(x=cluster, y=valor)) +
  scale_fill_viridis(discrete = TRUE)+
  facet_wrap(idh~.)



####################Primeiras e segundas diferenças



#lab com plm
library(plm)

E <- pdata.frame(EmplUK, index=c("firm","year"), drop.index=TRUE, row.names=TRUE)


head(diff(E$emp, 0:2))

head(attr(E, "index"))




idh_panel<- pdata.frame(idh_municipios, index = c("id_municipio", "ano"), drop.index=TRUE, row.names=TRUE)

head(diff(idh_panel[,3], 0:2, shift = "row"))

head(attr(idh_panel, "index"))


indice<- as_tibble( attr(idh_panel, "index"))

diff_values<- as_tibble(diff(idh_panel[,3], 1:2, shift = "row"))



data("Grunfeld", package="plm")

grun.fe <- plm(inv~value+capital , data = Grunfeld, model = "within")

summary(grun.fe)

idh_municipios %>%
  pivot_wider()

idh_municipios%>%
  group_by(id_municipio) %>%
  mutate(diff_expectativa_vida =  expectativa_vida - lag(expectativa_vida) )

###### Primeiras e e segundas diferenças relativas usando dplyr


diff_idh<-
  idh_municipios%>%
  group_by(id_municipio) %>%
  summarise( across(expectativa_vida:idhm, function(x){ (x -dplyr::lag(x))/dplyr::lag(x)},.names = "perc_{.col}" )) %>%
  ungroup()

idh_lag<- rep(c(0,1,2),times= NROW(diff_idh)/3)

diff_idh$idh_lag<- idh_lag



base_cluster_idh1<-
diff_idh %>%
  select(id_municipio,perc_idhm) %>%
  filter(idh_lag==1)

set.seed(1972)
sil_info<-
  map_dbl(2:6, function(k){
    print(k)
    model_idh<- cluster::pam(base_cluster_idh1[2],k=k)
    model_idh$silinfo$avg.width
  })

quantile(base_cluster_idh1[2])

mediana<-median(base_cluster_idh1$perc_idhm)

base_cluster_idh1 %>%
  mutate(grupo = ifelse(perc_idhm<mediana,"1","2")) %>%
  ggplot(aes(x=grupo, y= perc_idhm))+
  geom_boxplot()


set.seed(1972)
model_idh_diff1<- cluster::pam(base_cluster_idh1[2],k=3)
base_cluster_idh1$grupo <-model_idh_diff1$clustering


base_cluster_idh1 %>%
  mutate( grupo = as.character(grupo),
         grupo = factor(grupo, levels=c("2","1","3"), labels = c("Baixa","Média","Alta"))) %>%
  group_by(grupo) %>%
  summarise(max(perc_idhm))

base_cluster_idh1<-
base_cluster_idh1 %>%
  mutate( grupo = as.character(grupo),
          grupo = factor(grupo, levels=c("2","1","3"), labels = c("Baixa","Média","Alta")))

##########Gráfico dos clusters de variação de idh


p<-
base_cluster_idh1 %>%
  ggplot(aes(x=grupo, y= perc_idhm*100))+
  geom_jitter(aes(fill= grupo),pch=21, color="white", alpha= 0.5)+
  scale_fill_viridis(discrete = TRUE)+
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black"),
    legend.position = "bottom",
    axis.title.x = element_blank()
  ) +
  labs(
    y= "Variação percentual do IDHM",
    fill= "Classe de variação"
  )

png("clusters_1991_2000.png", width=3200, height =2000, res= 400)

print(p)

dev.off()



base_cluster_idh1 %>%
  group_by(grupo) %>%
  summarise(n(), min(perc_idhm),max(perc_idhm))

dt_model$finalModel$variable.importance


########Preparação para ML
base_cluster_idh1_ml <-
  diff_idh %>%
  filter(idh_lag == 1) %>%
  select(-c(idh_lag,perc_indice_gini, perc_indice_theil,perc_indice_treil_trabalho)) %>%
  bind_cols(
    base_cluster_idh1 %>%
      select(grupo)
  ) %>%
  select(-perc_idhm)


base_cluster_idh1_ml <-base_cluster_idh1_ml[, sapply(base_cluster_idh1_ml, Negate(anyNA)), drop = FALSE]





library(caret)


control_dt <- trainControl(method="cv")

set.seed(1972)
dt_model <- train(grupo~., data=base_cluster_idh1_ml, method="rpart",  trControl=control_dt)

library(rattle)
fancyRpartPlot(dt_model$finalModel)

save(list=ls(), file="modelo_idh.Rdata")



municipios<-
  (diff_idh%>%
  filter(idh_lag ==1) %>%
  select(id_municipio))$id_municipio

base_cluster_idh1_ml$id_municipio<- municipios

##Base dos mapas

library(geobr)

estados<- geobr::read_state(year=2010)
municipios<- geobr::read_municipal_seat(year = 2010)
brasil <- geobr::read_country()
municipios_area<- geobr::read_municipality()

#mapa idh

df_mapa<-
  municipios %>%
  mutate(id_municipio = as.character(code_muni) ) %>%
  inner_join(
    base_cluster_idh1_ml %>%
      inner_join(
        idh_municipios %>%
          dplyr::filter(ano == 1991 | ano == 2000 ) %>%
          mutate(ano = factor(ano)) %>%
          select(ano,
                 id_municipio,
                 idhm),
        by= "id_municipio"
      )
  )


df_mapa_area<-
  municipios_area %>%
  mutate(id_municipio = as.character(code_muni) ) %>%
  inner_join(
    base_cluster_idh1_ml %>%
      inner_join(
        idh_municipios %>%
          dplyr::filter(ano == 1991 | ano == 2000 ) %>%
          mutate(ano = factor(ano)) %>%
          select(ano,
                 id_municipio,
                 idhm),
        by= "id_municipio"
      )
  )




library(colorspace)

p<-
df_mapa %>%
  ggplot() +
  geom_sf(aes(color= idhm), size=0.1, alpha=1) +
  geom_sf(data = brasil, fill=NA) +
  scale_color_continuous_divergingx (palette = "RdYlBu",mid=0.5)+
  labs( color="IDHM", size=8) +
  theme_light()  +
  theme(
    axis.text = element_blank(),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "black")
  ) +
  facet_wrap(ano+grupo~.)


df_mapa_area %>%
  ggplot() +
  geom_sf(aes(fill= idhm),color = NA, alpha= 1) +
  geom_sf(data = brasil, fill=NA) +
  scale_fill_continuous_divergingx (palette = "RdYlBu",mid=0.5)+
  labs( fill="IDHM", size=8) +
  theme_light()  +
  theme(
    axis.text = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "black")
  ) +
  facet_wrap(ano+grupo~.)

png("mapa_idh.png", width=3200, height =2300, res= 400)

print(p)

dev.off()


################ Gráficos selecionados

#Gráfico da dispersão do índice de frequência escolar com idhm com wrap ano

p<-
base_cluster_idh1_ml %>%
  inner_join(
    idh_municipios %>%
      dplyr::filter(ano == 1991 | ano== 2000) %>%
      mutate(ano = factor(ano)) %>%
      select(ano,
             id_municipio,
             indice_frequencia_escolar,
             idhm),
    by= "id_municipio"
  ) %>%
  ggplot() +
  geom_jitter(aes(x= indice_frequencia_escolar*100, y= idhm, fill= grupo),pch=21, color="white", alpha= 0.5) +
  facet_wrap(ano~.)+
  scale_fill_viridis(discrete = TRUE) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black"),
    legend.position = "bottom",
    strip.background = element_rect(fill = "black")

  ) +
  labs(
    x= "Frequência escolar em (%) de população jovem",
    y= "IDHM",
    fill= "Classe de variação"
  )


png("frequencia_pop_jovem_1.png", width=3200, height =2000, res= 400)

print(p)

dev.off()



#Gráfico da dispersão da do índice de frequência escolar com idhm com wrap ano+grupo
# Observar esse texto da UOL
#https://noticias.uol.com.br/ultimas-noticias/agencia-estado/2013/07/29/melhora-do-idhm-educacao-e-puxado-por-fluxo-escolar.htm

p<-
base_cluster_idh1_ml %>%
  inner_join(
    idh_municipios %>%
      dplyr::filter(ano == 1991 | ano== 2000) %>%
      mutate(ano = factor(ano)) %>%
      select(ano,
             id_municipio,
             indice_frequencia_escolar,
             idhm),
    by= "id_municipio"
  ) %>%
  ggplot() +
  geom_jitter(aes(x= indice_frequencia_escolar*100, y= idhm, fill= grupo),pch=21, color="white", alpha= 0.5) +
  facet_wrap(ano+grupo~.)+
  scale_fill_viridis(discrete = TRUE) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black"),
    legend.position = "bottom",
    strip.background = element_rect(fill = "black")
  ) +
  labs(
    x= "Frequência escolar em (%) de população jovem",
    y= "IDHM",
    fill= "Classe de variação"
  )




png("frequencia_pop_jovem_2.png", width=3200, height =2300, res= 400)

print(p)

dev.off()

#Mapa da dispersão da do índice de frequência escolar com idhm com wrap ano+grupo


mid_point = median(df_mapa$indice_frequencia_escolar)

df_mapa %>%
  ggplot() +
  geom_sf(aes(color= indice_frequencia_escolar*100), size=0.1, alpha=0.5) +
  geom_sf(data = brasil, fill=NA) +
  scale_color_continuous_divergingx (palette = "RdYlBu",mid=mid_point*100)+
  labs( color="Frequência escolar", size=8) +
  theme_light()  +
  theme(
    axis.text = element_blank(),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "black")
  ) +
  facet_wrap(ano+grupo~.)



#Gráfico da dispersão da do índice de frequência escolar 5 e 6 anos com idhm wrap ano

base_cluster_idh1_ml %>%
  inner_join(
    idh_municipios %>%
      dplyr::filter(ano == 1991 | ano== 2000) %>%
      mutate(ano = factor(ano)) %>%
      select(ano,
             id_municipio,
             taxa_freq_5_6,
             idhm),
    by= "id_municipio"
  ) %>%
  #mutate(super_grupo= "super grupo") %>%
  ggplot() +
  geom_jitter(aes(x= taxa_freq_5_6*100,  y= idhm, color= grupo, fill= grupo),pch=21, color="white", alpha= 0.5) +
  facet_wrap(ano~.)+
  scale_fill_viridis(discrete = TRUE) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black"),
    legend.position = "bottom",
    strip.background = element_rect(fill = "black")
  ) +
  labs(
    x= "Percentual da população de 5 a 6 anos de idade frequentando a escola",
    y= "IDHM",
    fill= "Classe de variação"
  )


#Gráfico da dispersão da do índice de frequência escolar 5 e 6 anos com idhm wrap ano + grupo

p<-
base_cluster_idh1_ml %>%
  inner_join(
    idh_municipios %>%
      dplyr::filter(ano == 1991 | ano== 2000) %>%
      mutate(ano = factor(ano)) %>%
      select(ano,
             id_municipio,
             taxa_freq_5_6,
             idhm),
    by= "id_municipio"
  ) %>%
  #mutate(super_grupo= "super grupo") %>%
  ggplot() +
  geom_jitter(aes(x= taxa_freq_5_6,  y= idhm, color= grupo, fill= grupo),pch=21, color="white", alpha= 0.5) +
  facet_wrap(ano+grupo~.)+
  scale_fill_viridis(discrete = TRUE) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black"),
    legend.position = "bottom",
    strip.background = element_rect(fill = "black")
  ) +
  labs(
    x= "Percentual da população de 5 a 6 anos de idade frequentando a escola",
    y= "IDHM",
    fill= "Classe de variação"
  )


png("frequencia_5_6.png", width=3200, height =2300, res= 400)

print(p)

dev.off()


#Gráfico da dispersão da do índice de fundamental com 18 anos ou mais com idhm wrap com ano

base_cluster_idh1_ml %>%
  inner_join(
    idh_municipios %>%
      dplyr::filter(ano == 1991 | ano== 2000) %>%
      mutate(ano = factor(ano)) %>%
      select(ano,
             id_municipio,
             taxa_fundamental_18_mais,
             idhm),
    by= "id_municipio"
  ) %>%
  #mutate(super_grupo= "super grupo") %>%
  ggplot() +
  geom_jitter(aes(x= taxa_fundamental_18_mais,  y= idhm, color= grupo, fill= grupo),pch=21, color="white", alpha= 0.5) +
  facet_wrap(ano~.)+
  scale_fill_viridis(discrete = TRUE) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black"),
    legend.position = "bottom"
  ) +
  labs(
    x= "Percentual da população de 18 a 24 anos com fundamental completo",
    y= "IDHM",
    fill= "Classe de variação"
  )


#Gráfico da dispersão da do índice de fundamental com 18 anos ou mais com idhm wrap com ano + grupo

base_cluster_idh1_ml %>%
  inner_join(
    idh_municipios %>%
      dplyr::filter(ano == 1991 | ano== 2000) %>%
      mutate(ano = factor(ano)) %>%
      select(ano,
             id_municipio,
             taxa_fundamental_18_mais,
             idhm),
    by= "id_municipio"
  ) %>%
  #mutate(super_grupo= "super grupo") %>%
  ggplot() +
  geom_jitter(aes(x= taxa_fundamental_18_mais,  y= idhm, color= grupo, fill= grupo),pch=21, color="white", alpha= 0.5) +
  facet_wrap(ano+grupo~.)+
  scale_fill_viridis(discrete = TRUE) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black"),
    legend.position = "bottom"
  ) +
  labs(
    x= "Percentual da população de 18 a 24 anos com fundamental completo",
    y= "IDHM",
    fill= "Classe de variação"
  )






######################## Gráficos de apoio

library(viridis)

#Gráfico da dispersão da variação percentual do índice de taxa_freq_1_5  com idhm

base_cluster_idh1_ml %>%
  inner_join(
    idh_municipios %>%
      dplyr::filter(ano == 1991 | ano== 2000) %>%
      mutate(ano = factor(ano)) %>%
      select(ano,
             id_municipio,
             indice_frequencia_escolar,
             idhm),
    by= "id_municipio"
  ) %>%
  ggplot() +
  geom_jitter(aes(x= perc_taxa_freq_5_6, y= idhm, color= grupo, fill= grupo),pch=21, color="white", alpha= 0.5) +
  facet_grid(ano~.)+
  scale_fill_viridis(discrete = TRUE) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black"),
    legend.position = "bottom"
  ) +
  labs(
    x= "Variação percentual da população de 5 a 6 anos de idade frequentando escola entre 1991 e 2000",
    y= "IDHM",
    fill= "Classe de variação"
  )


#Gráfico da dispersão da variação percentual do índice de taxa_perc_18_mais  com idhm

base_cluster_idh1_ml %>%
  inner_join(
    idh_municipios %>%
      dplyr::filter(ano == 1991 | ano== 2000) %>%
      mutate(ano = factor(ano)) %>%
      select(ano,
             id_municipio,
             indice_frequencia_escolar,
             idhm),
    by= "id_municipio"
  ) %>%
  ggplot() +
  geom_jitter(aes(x= perc_taxa_fundamental_18_mais, y= idhm, color= grupo, fill= grupo),pch=21, color="white", alpha= 0.5) +
  facet_grid(ano~.)+
  scale_fill_viridis(discrete = TRUE) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black"),
    legend.position = "bottom"
  ) +
  labs(
    x= "Variação percentual da população de 18 anos ou mais com ensino fundamental",
    y= "IDHM",
    fill= "Classe de variação"
  )

#Gráfico da dispersão da variação percentual do índice de frequência escola com idhm


dt_text_1<- data.frame(perc_indice_frequencia_escolar= 0, indice_frequencia_escolar= 0.9, ano = 1991)
dt_text_2<- data.frame(perc_indice_frequencia_escolar= 1.20, indice_frequencia_escolar= 0.75, ano = 1991)
dt_text_3<- data.frame(perc_indice_frequencia_escolar= 2.40, indice_frequencia_escolar= 0.6, ano = 1991)

library(viridis)
base_cluster_idh1_ml %>%
  inner_join(
    idh_municipios %>%
      dplyr::filter(ano == 1991 | ano== 2000) %>%
      mutate(ano = factor(ano)) %>%
      select(ano,
             id_municipio,
             indice_frequencia_escolar),
    by= "id_municipio"
  ) %>%
  ggplot(aes(x= perc_indice_frequencia_escolar*100, y= indice_frequencia_escolar)) +
  geom_rect(aes(xmin=0,xmax=120,ymin=0, ymax=1), fill="#A9A9A9")+
  geom_rect(aes(xmin=120,xmax=240,ymin=0, ymax=1), fill="#696969")+
  geom_rect(aes(xmin=240,xmax=3617,ymin=0, ymax=1), fill="#404040")+
  geom_jitter(aes( fill= grupo),pch=21, color="white", alpha= 0.5 ) +
  scale_fill_viridis(discrete = TRUE) +
  facet_wrap(ano+grupo~.) +
  theme_light() +
  geom_text(data = dt_text_1,label = "82% das alterações baixas de IDHM",
            color= "white",hjust   = -0.01,)+
  geom_text(data = dt_text_2,label = "75% das alterações médias de IDHM",
            color= "white",hjust   = -0.01,)+
  geom_text(data = dt_text_3,label = "85% das alterações altas de IDHM",
            color= "white",hjust   = -0.01,)+
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black"),
    legend.position = "bottom"
  ) +
  labs(
    x= "Variação percenutal em frequência escolar da população jovem entre 1991 e 2000",
    y= "IDHM",
    fill= "Classe de variação de IDHM"
  )
