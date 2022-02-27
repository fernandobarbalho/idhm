base_cluster_idh2<-
  diff_idh %>%
  select(id_municipio,perc_idhm) %>%
  filter(idh_lag==2)

set.seed(1972)
sil_info_idh2<-
  map_dbl(2:6, function(k){
    print(k)
    model_idh<- cluster::pam(base_cluster_idh2[2],k=k)
    model_idh$silinfo$avg.width
  })


set.seed(1972)
model_idh_diff2<- cluster::pam(base_cluster_idh2[2],k=3)
base_cluster_idh2$grupo <-model_idh_diff2$clustering


base_cluster_idh2<-
  base_cluster_idh2 %>%
  mutate( grupo = as.character(grupo),
          grupo = factor(grupo, levels=c("3","1","2"), labels = c("Baixa","Média","Alta")))



base_cluster_idh2_ml <-
  diff_idh %>%
  filter(idh_lag == 2) %>%
  select(-c(idh_lag,perc_indice_gini, perc_indice_theil,perc_indice_treil_trabalho)) %>%
  bind_cols(
    base_cluster_idh2 %>%
      select(grupo)
  ) %>%
  select(-perc_idhm)


base_cluster_idh2_ml <-base_cluster_idh2_ml[, sapply(base_cluster_idh2_ml, Negate(anyNA)), drop = FALSE]


library(caret)


control_dt <- trainControl(method="cv")

set.seed(1972)
dt_model_idh2 <- train(grupo~., data=base_cluster_idh2_ml, method="rpart",  trControl=control_dt)

library(rattle)
fancyRpartPlot(dt_model_idh2$finalModel)

save(list=ls(), file="modelo_idh.Rdata")



#mapa idh

df_mapa<-
  municipios %>%
  mutate(id_municipio = as.character(code_muni) ) %>%
  inner_join(
    base_cluster_idh2_ml %>%
      inner_join(
        idh_municipios %>%
          dplyr::filter(ano == 2000 | ano == 2010 ) %>%
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



png("mapa_idh_lag2.png", width=3200, height =2300, res= 400)

print(p)

dev.off()


df_mapa<-
  municipios %>%
  mutate(id_municipio = as.character(code_muni) ) %>%
  inner_join(
    base_cluster_idh1_ml %>%
      inner_join(
        idh_municipios %>%
          dplyr::filter(ano == 1991 | ano == 2010 ) %>%
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
  facet_wrap(ano~.)



png("mapa_idh_1991-2010.png", width=3200, height =2300, res= 400)

print(p)

dev.off()


################ Gráficos selecionados

#Gráfico da dispersão do índice de frequência escolar com idhm com wrap ano

max_point<- max(idh_municipios$indice_frequencia_escolar)
p<-
  base_cluster_idh1_m2 %>%
  inner_join(
    idh_municipios %>%
      dplyr::filter(ano == 2000 | ano== 2010) %>%
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
  ylim(0)+
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
  base_cluster_idh2_ml %>%
  inner_join(
    idh_municipios %>%
      dplyr::filter(ano == 2000 | ano== 2010) %>%
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
  ylim(0,1)+
  labs(
    x= "Frequência escolar em (%) de população jovem",
    y= "IDHM",
    fill= "Classe de variação"
  )




png("frequencia_pop_jovem_2_lag2.png", width=3200, height =2300, res= 400)

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

base_cluster_idh2_ml %>%
  inner_join(
    idh_municipios %>%
      dplyr::filter(ano == 2000 | ano== 2010) %>%
      mutate(ano = factor(ano)) %>%
      select(ano,
             id_municipio,
             taxa_fundamental_11_13,
             idhm),
    by= "id_municipio"
  ) %>%
  #mutate(super_grupo= "super grupo") %>%
  ggplot() +
  geom_jitter(aes(x= taxa_fundamental_11_13*100,  y= idhm, color= grupo, fill= grupo),pch=21, color="white", alpha= 0.5) +
  facet_wrap(ano~.)+
  scale_fill_viridis(discrete = TRUE) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black"),
    legend.position = "bottom",
    strip.background = element_rect(fill = "black")
  ) +
  ylim(0,1)+
  labs(
    x= "Percentual da população de 11 a 13 anos de idade nos anos finais do fundamental",
    y= "IDHM",
    fill= "Classe de variação"
  )


#Gráfico da dispersão da do índice de frequência escolar 5 e 6 anos com idhm wrap ano + grupo

p<-
  base_cluster_idh2_ml %>%
  inner_join(
    idh_municipios %>%
      dplyr::filter(ano == 2000 | ano== 2010) %>%
      mutate(ano = factor(ano)) %>%
      select(ano,
             id_municipio,
             taxa_fundamental_11_13,
             idhm),
    by= "id_municipio"
  ) %>%
  #mutate(super_grupo= "super grupo") %>%
  ggplot() +
  geom_jitter(aes(x= taxa_fundamental_11_13,  y= idhm, color= grupo, fill= grupo),pch=21, color="white", alpha= 0.5) +
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
    x= "Percentual da população de 11 a 13 anos de idade nos anos finais do fundamental",
    y= "IDHM",
    fill= "Classe de variação"
  )


png("taxa_11_13.png", width=3200, height =2300, res= 400)

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



