#script que será usado para análise exploratória dos dados

setwd("~/Data_Science/FCD_DSA/BigDataRAzure/Cap20/Projeto1")
library(data.table)
df  <- fread("data_work.csv", header = T, dec=",")

str(df)

#convertendo o click time em data
df$click_time <- as.POSIXct(df$click_time,format='%Y-%m-%d %H:%M:%S')

#sumarizando os dados
summary(df)
#no intervalo entre cliques podemos observar a mediana distante da média (mediana = 3, média = 91.92)
#o que significa uma grande quantidade de acessos com um pequeno intervalo entre cliques

#inserindo a hora no dataset
df$hour <- hour(df$click_time)

mean(df$is_attributed)
#% de downloads da amostra = 0.2505%, semalhante ao % de downloads da população = 0.2470721%

#verificando o % de downloads por número de acesso
library(dplyr)
t <- df %>%
    select(Acessos, is_attributed) %>%
    group_by(Acessos) %>%
    summarise(perc = mean(is_attributed)*100)%>%

library(ggplot2)
ggplot(t,aes(x=Acessos,y=perc)) + geom_line()
#temos um range de acessos muito grande, porém fica claro que a linha se aproxima de zero conforme 
#a qtde de acessos aumenta, vamos limitar o eixo x a 1000 acessos para melhor análise

ggplot(t,aes(x=Acessos,y=perc)) + geom_line() + scale_x_continuous(limits=c(1, 1000), breaks = seq(0,1000,by=100))
#ainda não legível, mas a tendência se confirma, limitaremos a 100 acessos

ggplot(t,aes(x=Acessos,y=perc)) + geom_line() + scale_x_continuous(limits=c(1, 100), breaks = seq(0,100,by=10))
#podemos observar uma queda consideravel no percentual de downloads quando o número de acessos por IP é maior que 10

#comparando os acessos X intervalo
#pois teoricamente bots tendem a ter um alto número de acessos em um pequeno intervalo de tempo
ggplot(df[!is.na(df$intervalo),c("Acessos","intervalo")],aes(x=Acessos, y=intervalo)) + geom_point() + scale_x_continuous(limits=c(1, 1000), breaks = seq(0,1000,by=100))

#ajustando os eixos para uma melhor visualização
ggplot(df[!is.na(df$intervalo),c("Acessos","intervalo")],aes(x=Acessos, y=intervalo)) + geom_point() + scale_x_continuous(limits=c(1, 500), breaks = seq(0,500,by=100)) + scale_y_continuous(limits=c(0, 50000))
#o gráfico mostra maior dispersão no intervalo onde a qtde de acessos é baixa, e maior concentração onde a qtde de acessos é alta

#criando uma tabela de intervalo médio por acesso para preencher os NA's
t <- df %>%
  filter(!is.na(intervalo)) %>%
  select(Acessos, intervalo) %>%
  group_by(Acessos) %>%
  summarise(media = mean(intervalo))

#intervalo médio para IP's com apenas 1 acesso = intervalo médio de 2 acessos 
#Pois quem teve apenas um acesso não clicou anteriormente, portanto não temos base de comparação
t <- rbind(t,c(1,t[t$Acessos==2,]$media))
t <- as.data.frame(t)
rownames(t) <- t$Acessos
t$Acessos <- NULL

df[is.na(df$intervalo), "intervalo"] = t[as.character(df[is.na(df$intervalo), ]$Acessos),]
any(is.na(df$intervalo))
#nenhum NA na coluna intervalo

  
#visualizando a média de downloads por intervalo, app, device, os, channel e hora, linha vermelha igual a média geral
for (i in c("intervalo", "app", "device", "os", "channel", "hour"))
{
  p <- df %>%
    select(one_of(i),is_attributed) %>%
    group_by_at(vars(one_of(i))) %>%
    summarise(media = mean(is_attributed)*100) %>%
    ggplot(aes_string(x=i,y="media")) + geom_line() + geom_hline(yintercept=0.2505, color="red") 
   print(p)
}

#podemos observar alguns apps, devices, os, channels com diferenças bastantes significativas da média
#com relação as horas do dia, as diferenças são sutis, já os intervalos devemos olhar com mais detalhe

#dividindo os intervalos em 5 categorias
df$catIntervalo <- cut(df$intervalo,5)
#sumarizando as categorias
summary(df$catIntervalo)
#podemos observar que a grande maioria (99.97% do total) ficou no primeiro intervalo

df$catIntervalo <- cut(df$intervalo,10)
summary(df$catIntervalo)
#mesmo dividindo a base em 10 intervalos continuamos com uma categorização desbalanceada
#por isso olharemos para os percentis do intervalo, dividindo em 5
df$catIntervalo <- rep(0,nrow(df))
quantile(df$intervalo,c(0.2,0.4,0.6,0.8))
df[df$intervalo==0,"catIntervalo"] <- 1
df[df$intervalo>0 & df$intervalo<=1,"catIntervalo"] <- 2
df[df$intervalo>1 & df$intervalo<=6,"catIntervalo"] <- 3
df[df$intervalo>6 & df$intervalo<=28,"catIntervalo"] <- 4
df[df$intervalo>28,"catIntervalo"] <- 5

#verificando a quantidade de registros por categoria
table(df$catIntervalo)
#agora conseguimos ver uma divisão mais balanceada
#vamos analisar os percentuais de downloads por categoria, linha vermelha igual a média geral
df %>%
  select(catIntervalo,is_attributed) %>%
  group_by(catIntervalo) %>%
  summarise(media = mean(is_attributed)*100) %>%
  ggplot(aes(x=catIntervalo,y=media)) + geom_line() + geom_hline(yintercept=0.2505, color="red") 

#podemos ver claramente que o percentual de dowloads por acesso aumenta conforme o intervalo entre cliques aumenta
#vamos analisar as categorias de intervalo por quatidade absoluta de downloads

df %>%
  select(catIntervalo,is_attributed) %>%
  group_by(catIntervalo) %>%
  summarise(total = sum(is_attributed)) %>%
  ggplot(aes(x=catIntervalo,y=total)) + geom_line()
#o comportamento do gráfico anterior se repete, a quantidade de dowloads aumenta conforme o intervalo entre cliques aumenta

#categorizando a quantidade de acessos por IP, para isso, será utilizado o dataset de frequencias por IP
#a partir dos dados de acesso por IP será criado a frequencia relativa de acessos por IP, pois frequencia absoluta apenas 
#aumenta ao longo do tempo, podendo alterar os patamares dependo da periodo analisado

#ler arquivo de acessos por App
ip <- read.csv("freq_IP.csv", header = T, sep=";")

#criar frequencia relativa de acesso por App
ip$frq_rel_IP <- ip$Acessos/sum(ip$Acessos)
#removendo a frequencia absoluta, pois essa informação já está na base
ip$Acessos <- NULL

#inserindo a informação no dataset de trabalho
df <- left_join(df,ip,by="ip")

rm(ip)

#dividindo a quantidade de acessos por IP em categorias
df$catAcessos <- cut(df$frq_rel_IP,5)
#sumarizando as categorias
summary(df$catAcessos)
#nesse caso também temos um desbalanceamento utilizando a função cut, portanto dividiremos em quartis
df$catAcessos <- rep(0,nrow(df))
quantile(df$frq_rel_IP,c(0.2,0.4,0.6,0.8))
df[df$frq_rel_IP<=0.00001087051,"catAcessos"] <- 1
df[df$frq_rel_IP>0.00001087051 & df$frq_rel_IP<=0.00002287134,"catAcessos"] <- 2
df[df$frq_rel_IP>0.00002287134 & df$frq_rel_IP<=0.00005326551,"catAcessos"] <- 3
df[df$frq_rel_IP>0.00005326551 & df$frq_rel_IP<=0.0001106304,"catAcessos"] <- 4
df[df$frq_rel_IP>0.0001106304,"catAcessos"] <- 5

#verificando a quantidade de registros por categoria
table(df$catAcessos)
#categorias de acessos estão melhores distribuidas do que quando feita pela função cut

#analisando o percentual de downloads por categoria, linha vermelha igual a média geral
df %>%
  select(catAcessos,is_attributed) %>%
  group_by(catAcessos) %>%
  summarise(media = mean(is_attributed)*100) %>%
  ggplot(aes(x=catAcessos,y=media)) + geom_line() + geom_hline(yintercept=0.2505, color="red") 
#conforme o frequência de acessos por IP aumenta o percentual diminui, podemos ver uma queda consideravel no percentual a partir da categoria 2

#analisando as categorias por quantidade absoluta
df %>%
  select(catAcessos,is_attributed) %>%
  group_by(catAcessos) %>%
  summarise(total = sum(is_attributed)) %>%
  ggplot(aes(x=catAcessos,y=total)) + geom_line()
#o comportamento do gráfico anterior se repete, a quantidade de dowloads diminui conforme a frequência de acessos por IP aumenta


#Para melhor exploração dos dados de app, device, os e channel voltaramos a base completo para criação de atributos, pois não temos 
#atributos sufientes para a exploração, e aleatóriadade da base de trabalho pode descaracterizar as estatisticas desses atributos

#criaremos um novo script (script 03) onde criaremos datasets contendo estatisticas de acesso por app, device, os e channel
#após a criação desses datasets voltaremos a esse script para continuar a análise 

#voltando a análise exploratória
#ler arquivo de acessos por App
app <- read.csv("freq_app.csv", header = T, sep=";", dec=",")

#criar frequencia relativa de acesso por App
#frequencia absoluta apenas aumenta ao longo do tempo, podendo alterar os patamares dependo da periodo analisado
app$isNAttr = NULL
app$frq_rel_App <- app$Acessos/sum(app$Acessos)
names(app)[4] <- "Acessos_App"

#inserindo a informação no dataset
df <- left_join(df,app,by="app")
#removendo o dataset de acessos por App, faremos as analise no dataframe de trabalho
rm(app)

#criando um dataset para analise dos dados
r <- df %>%
  select(app, Acessos_App, is_attributed) %>%
  group_by(app) %>%
  summarise(acessos=first(Acessos_App), downloads=sum(is_attributed), total_base= n()) %>%
  mutate(perc = downloads/total_base)

#verificando a média de acesso por App
mean(r$acessos)
#media de acessos por App = 576007

#visualizando os dados por percentual de acessos por app de forma decrescente pelo percentual de donwloads por acesso
View(r[order(r$perc,decreasing=T),])
#conforme vimos anteriormente, alguns Apps possuem alto percentual de downloads por Acesso
#porém a grande maioria desses apps possuí um baixo número de acessos, bem abaixo da média

#vamos dividir a frequencia de acessos em categorias, 
df$catAcesApp <- cut(df$frq_rel_App,5)
summary(df$catAcesApp)

#colocar categorias no dataframe
r <- df %>%
 select(app,catAcesApp) %>%
 group_by(app)  %>%
 summarise(cat = first(catAcesApp))  %>%
 right_join(r,by="app")

#analise de qtde de downloads por categoria
ggplot(r, aes(x=as.integer(cat),y=downloads)) + geom_point() + geom_hline(yintercept=0.2505, color="red")
 
#analis de perencetual de donwloads por categoria
ggplot(r, aes(x=as.integer(cat),y=perc)) + geom_point()
#categoria de acessos por App 1 (até 3,67% dos acessos) possui maior efetividade nos downloads
rm(r)

#ler arquivo de acessos por device
device <- read.csv("freq_device.csv", header = T, sep=";", dec=",")

#criar frequencia relativa de acesso por device
#frequencia absoluta apenas aumenta ao longo do tempo, podendo alterar os patamares dependo da periodo analisado
device$isNAttr = NULL
device$frq_rel_device <- device$Acessos/sum(device$Acessos)
names(device)[4] <- "Acessos_device"

#inserindo a informação no dataset
df <- left_join(df,device,by="device")
#removendo o dataset de acessos por device, faremos as analise no dataframe de trabalho
rm(device)

#criando um dataset para analise dos dados
r <- df %>%
  select(device, Acessos_device, is_attributed) %>%
  group_by(device) %>%
  summarise(acessos=first(Acessos_device), downloads=sum(is_attributed), total_base= n()) %>%
  mutate(perc = downloads/total_base)

#verificando a média de acesso por device
mean(r$acessos)
#media de acessos por device = 308125.3
sd(r$acessos)
#desvio padrão de 7124149
max(r$acessos)
#o device com o maior número de acessos possui 174.330.052 acessos o que equivale a mais de 94% da base


#dividindo a frequencia de acessos em categorias, 
df$catAcesDevice <- cut(df$frq_rel_device,5)
summary(df$catAcesDevice)
#os dados foram alocados em apenas duas categorias, em função da aalta disperção dos acessos por device
#nesse caso podemos criar apenas duas categorias, a categoria do device 1 (onde 94% dos dados serão alocados), e 2 para os demais devices
df$catAcesDevice <- rep(0,nrow(df))
df[df$device==1,"catAcesDevice"] <- 1
df[df$device!=1,"catAcesDevice"] <- 2

#verificando as  medias de downloads por categoria de device
mean(df[df$catAcesDevice==1,]$is_attributed)*100
sum(df[df$catAcesDevice==1,]$is_attributed)
#categoria 1 média de 0.18% dos acessos se convertem em downloads, totalizando 3.351 downloads na base de trabalho

mean(df[df$catAcesDevice==2,]$is_attributed)*100
sum(df[df$catAcesDevice==2,]$is_attributed)
#categoria 1 média de 1.45% dos acessos se convertem em downloads, totalizando 1.659 downloads na base de trabalho

#analisando a base de dados de trabalho, apenas 5.71% dos acessos estão na categoria de device 2, porém 33% dos downloads estão nessa categoria
rm(r)

#ler arquivo de acessos por OS
OS <- read.csv("freq_os.csv", header = T, sep=";", dec=",")

#criar frequencia relativa de acesso por OS
#frequencia absoluta apenas aumenta ao longo do tempo, podendo alterar os patamares dependo da periodo analisado
OS$isNAttr = NULL
OS$frq_rel_OS <- OS$Acessos/sum(OS$Acessos)
names(OS)[4] <- "Acessos_OS"

#inserindo a informação no dataset
df <- left_join(df,OS,by="os")
#removendo o dataset de acessos por OS, faremos as analise no dataframe de trabalho
rm(OS)

#criando um dataset para analise dos dados
r <- df %>%
  select(os, Acessos_OS, is_attributed) %>%
  group_by(os) %>%
  summarise(acessos=first(Acessos_OS), downloads=sum(is_attributed), total_base= n()) %>%
  mutate(perc = downloads/total_base)


#verificando a média de acesso por OS
mean(r$acessos)
#media de acessos por OS = 818141.6
sd(r$acessos)
#desvio padrão de 4128370
max(r$acessos)
#o OS com o maior número de acessos possui 44.181.914 acessos, quase 24% da base

#dividindo a frequencia de acessos em categorias, 
df$catAcesOS <- cut(df$frq_rel_OS,5)
summary(df$catAcesOS)
#os dados foram alocados em apenas três categorias

quantile(df$frq_rel_OS,c(0.2,0.4,0.6,0.8))
df$catAcesOS <- rep(0,nrow(df))

df[df$frq_rel_OS<=0.01571597,"catAcesOS"] <- 1
df[df$frq_rel_OS>0.01571597 & df$frq_rel_OS<=0.02854627,"catAcesOS"] <- 2
df[df$frq_rel_OS>0.02854627 & df$frq_rel_OS<=0.21515398,"catAcesOS"] <- 3
df[df$frq_rel_OS>0.21515398 & df$frq_rel_OS<=0.23894529,"catAcesOS"] <- 4
df[df$frq_rel_OS>0.23894529,"catAcesOS"] <- 5

#verificando a distribuição dos dados
table(df$catAcesOS)
#distribuição ficou mais uniforme

#analisando o percentual de downloads por categoria, linha vermelha igual a média geral
df %>%
  select(catAcesOS,is_attributed) %>%
  group_by(catAcesOS) %>%
  summarise(media = mean(is_attributed)*100) %>%
  ggplot(aes(x=catAcesOS,y=media)) + geom_line() + geom_hline(yintercept=0.2505, color="red") 

#conversão de downloads da categoria 1 estão bem acima da média, categorias de 2 a 5 estabilizadas, sendo que a partir da categora 3 existe uma leve subida

#analisando as categorias por quantidade absoluta
df %>%
  select(catAcesOS,is_attributed) %>%
  group_by(catAcesOS) %>%
  summarise(total = sum(is_attributed)) %>%
  ggplot(aes(x=catAcesOS,y=total)) + geom_line()

#o desenho do gráfico anterior se repete, porém a subida está mais acentuada depois da categoria 3

rm(r)

#ler arquivo de acessos por channel
channel <- read.csv("freq_channel.csv", header = T, sep=";", dec=",")

#criar frequencia relativa de acesso por channel
#frequencia absoluta apenas aumenta ao longo do tempo, podendo alterar os patamares dependo da periodo analisado
channel$isNAttr = NULL
channel$frq_rel_channel <- channel$Acessos/sum(channel$Acessos)
names(channel)[4] <- "Acessos_channel"

#inserindo a informação no dataset
df <- left_join(df,channel,by="channel")
#removendo o dataset de acessos por channel, faremos as analise no dataframe de trabalho
rm(channel)

#criando um dataset para analise dos dados
r <- df %>%
  select(channel, Acessos_channel, is_attributed) %>%
  group_by(channel) %>%
  summarise(acessos=first(Acessos_channel), downloads=sum(is_attributed), total_base= n()) %>%
  mutate(perc = downloads/total_base)

#verificando a média de acesso por channel
mean(r$acessos)
#media de acessos por channel = 308125.3
sd(r$acessos)
#desvio padrão de 1912341
max(r$acessos)
#o channel com o maior número de acessos possui 15.065.927 acessos o que equivale a mais de 8% da base

#dividindo a frequencia de acessos em categorias, 
df$catAcesChannel <- cut(df$frq_rel_channel,5)
summary(df$catAcesChannel)
#existe um certo desbalanceamento da distribuicao, a base será dividida em percentis
quantile(df$frq_rel_channel,c(0.2,0.4,0.6,0.8))
df$catAcesChannel <- rep(0,nrow(df))

df[df$frq_rel_channel<=0.006936225,"catAcesChannel"] <- 1
df[df$frq_rel_channel>0.006936225 & df$frq_rel_channel<=0.013998516,"catAcesChannel"] <- 2
df[df$frq_rel_channel>0.013998516 & df$frq_rel_channel<=0.023575383,"catAcesChannel"] <- 3
df[df$frq_rel_channel>0.023575383 & df$frq_rel_channel<=0.038876089,"catAcesChannel"] <- 4
df[df$frq_rel_channel>0.038876089,"catAcesChannel"] <- 5

#verificando a distribuição dos dados
table(df$catAcesChannel)
#distribuição ficou mais uniforme

#analisando o percentual de downloads por categoria, linha vermelha igual a média geral
df %>%
  select(catAcesChannel,is_attributed) %>%
  group_by(catAcesChannel) %>%
  summarise(media = mean(is_attributed)*100) %>%
  ggplot(aes(x=catAcesChannel,y=media)) + geom_line() + geom_hline(yintercept=0.2505, color="red") 

#conversão de downloads da categoria 1 está bem acima da média, categoria 2 está acima das demais, categorias de 3 a 5 estáveis

#analisando as categorias por quantidade absoluta
df %>%
  select(catAcesChannel,is_attributed) %>%
  group_by(catAcesChannel) %>%
  summarise(total = sum(is_attributed)) %>%
  ggplot(aes(x=catAcesChannel,y=total)) + geom_line()

#o desenho do gráfico anterior se repete

rm(r)

#agora com os dados devidamente categorizados, vamos a aplicação do modelo de machine learning
#salvando a base de trabalho que será utilizada no script 04
write.csv2(df,"data_work.csv", row.names=FALSE)