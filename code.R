# Assignment 3
# Giovanni Machado
# Prof Ricardo Dahis

# bibliotecas
library(rvest)
library(janitor)
library(lubridate)
library(xml2)
library(tidyverse)

# Diretórios
figura = "C:/Users/Giovanni Machado/OneDrive/Desktop/Pessoal/Mestrado/Verão/CIENCIA DE DADOS/Assignment 3"

# Tema para os gráficos
tema = theme(panel.background = element_rect(fill = "white"),
             panel.grid = element_line(color = "grey95"),
             legend.key = element_blank(),
             legend.position = "bottom",
             legend.text = element_text(size = 9),
             legend.title = element_blank())


# Construindo os urls 
df = data.frame(
  marca = c("Xiaomi", "Apple", "Samsung", "Positivo", "Motorola"),
  url = c("https://www.amazon.com.br/s?i=electronics&bbn=16243890011&rh=n%3A16243890011%2Cp_72%3A17833786011%2Cp_89%3AXiaomi&dc&pd_rd_r=a7f887e9-146f-40a2-87eb-152fd6ed0e18&pd_rd_w=Qiuoq&pd_rd_wg=QCbDq&pf_rd_p=716b5142-7b40-4cb8-94a9-c2d932cb1153&pf_rd_r=VJ0RBMH8RR6ETJA9T23P&qid=1645195077&rnid=18120432011&ref=sr_nr_p_89_1",
          "https://www.amazon.com.br/s?i=electronics&bbn=16243890011&rh=n%3A16243890011%2Cp_72%3A17833786011%2Cp_89%3AApple&dc&pd_rd_r=a7f887e9-146f-40a2-87eb-152fd6ed0e18&pd_rd_w=Qiuoq&pd_rd_wg=QCbDq&pf_rd_p=716b5142-7b40-4cb8-94a9-c2d932cb1153&pf_rd_r=VJ0RBMH8RR6ETJA9T23P&qid=1645199257&rnid=18120432011&ref=sr_nr_p_89_5",
          "https://www.amazon.com.br/s?i=electronics&bbn=16243890011&rh=n%3A16243890011%2Cp_72%3A17833786011%2Cp_89%3ASAMSUNG&dc&pd_rd_r=a7f887e9-146f-40a2-87eb-152fd6ed0e18&pd_rd_w=Qiuoq&pd_rd_wg=QCbDq&pf_rd_p=716b5142-7b40-4cb8-94a9-c2d932cb1153&pf_rd_r=VJ0RBMH8RR6ETJA9T23P&qid=1645199285&rnid=18120432011&ref=sr_nr_p_89_2",
          "https://www.amazon.com.br/s?i=electronics&bbn=16243890011&rh=n%3A16243890011%2Cp_72%3A17833786011%2Cp_89%3APositivo&dc&pd_rd_r=a7f887e9-146f-40a2-87eb-152fd6ed0e18&pd_rd_w=Qiuoq&pd_rd_wg=QCbDq&pf_rd_p=716b5142-7b40-4cb8-94a9-c2d932cb1153&pf_rd_r=VJ0RBMH8RR6ETJA9T23P&qid=1645228689&rnid=18120432011&ref=sr_nr_p_89_3",
          "https://www.amazon.com.br/s?i=electronics&bbn=16243890011&rh=n%3A16243890011%2Cp_72%3A17833786011%2Cp_89%3AMotorola&dc&pd_rd_r=a7f887e9-146f-40a2-87eb-152fd6ed0e18&pd_rd_w=Qiuoq&pd_rd_wg=QCbDq&pf_rd_p=716b5142-7b40-4cb8-94a9-c2d932cb1153&pf_rd_r=VJ0RBMH8RR6ETJA9T23P&qid=1645228724&rnid=18120432011&ref=sr_nr_p_89_4")
)

# Extração
dados = data.frame()

for (i in 1:nrow(df)){
amazon = read_html(df$url[i])

telefones = 
  amazon %>% 
  html_elements('.a-price-whole') %>%
  html_text()

j = as.data.frame(t(matrix(telefones,nrow = 1)))
dados = rbind(dados,j %>% mutate(marca = df[i,1]))
}

# Tratamento
dados = 
  dados %>%
  mutate(preco = as.numeric(gsub(",|\\.","", V1))) %>% 
  select(-V1)

# gráfico - excluindo apple
marcas = 
  dados %>%
  filter(marca != "Apple")

marcas = marcas %>% mutate(x = 1:nrow(marcas))
  

ggplot(marcas, aes(x = x, y = preco))+
  geom_point(aes(color = marca)) +
  labs(
    title = "Dispersão do preço dos telefones por marca",
    y = "Preço",
    x = NULL,
    subtitle = "Dados extraídos do site da Amazon em 18/02/2022"
  ) + tema


ggsave("Dispersao-marcas.pdf", path = figura, plot = last_plot())

# gráfico - médias
medias = 
  dados %>% 
  group_by(marca) %>% 
  summarise(media = mean(preco, na.rm = T))

ggplot(medias, aes(x = marca, y = media))+
  geom_col(aes(fill = marca)) +
  labs(
    title = "Média do preço dos telefones por marca",
    y = "Preço",
    x = NULL,
    subtitle = "Dados extraídos do site da Amazon em 18/02/2022"
  ) + tema + theme(legend.position = "none")

ggsave("Media-marcas.pdf", path = figura, plot = last_plot())

