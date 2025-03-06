#Gráficos do case do Paraná Banco

#Bbiliotecas
library(tidyverse)
library(geobr)
library(scales)
library(tidyr)
library(ggthemes) 

#Base 1: Distribuição por estados
df_1<-read_delim("C:/Users/zaian/OneDrive/OTHER/DIVERSOS/CASE_PB/dados_estado.csv", 
                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
#Visualização
df_1 |> view()

#Base Brasil
brasil <- read_state(year = 2020)
dados_brasil <- brasil |> left_join(df_1, by = c("abbrev_state"="estado"))

#Gráfico
plot_1<- ggplot() + 
  geom_sf(data=dados_brasil,  aes(fill=percentual),color = "black")+
  scale_fill_gradient(low = "white", high = "dodgerblue4",
                      name="Percentual") +
  labs(title="Distribuição dos clientes por estado") + xlab("") +  ylab("") +
  theme_minimal()+
  geom_sf_label(data=dados_brasil, aes(label = abbrev_state),label.padding = unit(0.5, "mm"),size = 2) +
  theme(text = element_text(size=8), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 9, hjust = 0),
        plot.subtitle = element_text(face = "italic", size = 8, hjust = 0),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom")

plot_1


#Base 2: Distribuição por remuneração
df_2<-read_delim("C:/Users/zaian/OneDrive/OTHER/DIVERSOS/CASE_PB/dados_publico.csv", 
                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

#Visualização
df_2 |> view()

df_2$publico_alvo <- str_wrap(df_2$publico_alvo, width = 20)

#Quebrando os rótulos longos em várias linhas
df_2$publico_alvo <- str_wrap(df_2$publico_alvo, width = 20) # Ajuste o width conforme necessário

#Gráfico: Barras verticais
ggplot(data=df_2, aes(x=reorder(publico_alvo, percentual), y=percentual)) +
  geom_bar(stat="identity", fill="steelblue", width = 0.65) +
  geom_text(
    aes(label = ifelse(percentual >= 5, paste(percentual, "%"), "")), # Mostra rótulo apenas se percentual >= 5
    hjust = 1.1, color = "white", size = 3.1
  ) +
  coord_flip() + # Inverte o gráfico para barras verticais
  theme_stata() +
  labs(title="Distribuição dos clientes pelo tipo de remuneração",
       y = " ", x = "Tipo de remuneração") + 
  theme(text = element_text(size=9), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 9, hjust = 0), 
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.y = element_text(size = 8, angle = 0, hjust = 1), # Rótulos "deitados" (90 graus)
        axis.text.x = element_blank()) + # Remove os rótulos do eixo x
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) # Ajusta o espaçamento no eixo y


 