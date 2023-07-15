art_df <- read.csv("/Users/Zila/opt/anaconda3/lib/python3.9/site-packages/py_tat_morphan/articles_clean.csv")
str(art_df)

categories <- unique(art_df$category)
categories
art_df$category[art_df$category == "Бала"] <- "Дети"
art_df$category[art_df$category == "Актуаль тема"] <- "Актуальная тема"
art_df$category[art_df$category == "Яңалыклар"] <- "Новости"
art_df$category[art_df$category == "Белмәсәң бел"] <- "Полезные советы"
art_df$category[art_df$category == "Матур булыйк"] <- "Красота"
art_df$category[art_df$category == "Күңелеңә җыйма"] <- "Не переживай (психология)"
art_df$category[art_df$category == "Тормыш кыйммәтләре"] <- "Жизненные ценности"
art_df$category[art_df$category == "Дин"] <- "Религия"
art_df$category[art_df$category == "Сәхнә һәм язмыш"] <- "Сцена и жизнь"
art_df$category[art_df$category == "Арабыздан беребез"] <- "Один из нас"
art_df$category[art_df$category == "Бакча"] <- "Огород"
art_df$category[art_df$category == "Сәламәтлек"] <- "Здоровье"
art_df$category[art_df$category == "Татар гаиләсе"] <- "Семья"
art_df$category[art_df$category == "«Йолдызлы» яңалыклар"] <- "Звёздные новости"
art_df$category[art_df$category == "«Көтә белгәннәр полкы»"] <- "\"Они умели ждать (ВОВ)\""

library(scales)
library(tidyverse)

model <- lm(views ~ likes, data = art_df)
model

ggplot(data = art_df, aes(x = likes, y = views))+
  geom_point(alpha = 0.5, aes(size=views), color='#ad1c47')+
  geom_abline(slope = model$coefficients[2], intercept = model$coefficients[1], color='#0A7BA3')+
  theme_classic()+
  labs(title = "Зависимость количества просмотров статьи от количества отметок \"мне нравится\"") +
  xlab("Количество отметок \"мне нравится\"") +
  ylab("Количество просмотров (тыс.)")+
  scale_y_continuous(breaks = seq(from = 0, to = 35000, by = 5000), labels = unit_format(unit='', scale = 1e-3))+
  scale_x_continuous(breaks = seq(from = 0, to = 350, by = 50))+
  guides(size=F)+
  theme(plot.title = element_text(face = "bold", size=20))

summary(model)

  
ggplot(data = art_df) +
  geom_bar(aes(y = forcats::fct_infreq(category), fill = category))+
  theme_classic()+
  labs(title = "Распределение статей по категориям") +
  xlab("Количество статей") +
  ylab("Тематические категории") +
  guides(fill = FALSE)+
  theme(text = element_text(size = 20))+
  scale_x_continuous(breaks = seq(from = 0, to = 500, by = 50)) +
  theme(plot.title = element_text(face = "bold"))
