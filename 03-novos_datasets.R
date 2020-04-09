#esse script será usado para gerar alguns atributos novos na base de trabalho, identificados durante o processo de EDA

#recuperar o dataframe criado usando o ff para criação dos dataframes contendo os dados sumarizados
setwd("~/Data_Science/FCD_DSA/BigDataRAzure/Cap20/Projeto1")
library(ffbase)

#lendo o dataframe completo para a área de trabalho do R
load.ffdf("ffdb")


#tabelas de frequencia app, device, os e channel, apos serão criados arquivos csv com 
#a sumarização de cada atributo e essas informações serão acrescentadas no dataframe de trabalho
app <-  table.ff(df$app,df$is_attributed)


d <- data.frame(app=rownames(app),isNAttr=unname(app[,1]),downloads=unname(app[,2]))
d$conv = d$downloads / (d$downloads + d$isNAttr)
d$Acessos <- (d$downloads + d$isNAttr)

write.csv2(d,"freq_app.csv", row.names=FALSE)
rm(app,d)

device <-  table.ff(df$device,df$is_attributed)

d <- data.frame(device=rownames(device),isNAttr=unname(device[,1]),downloads=unname(device[,2]))
d$conv = d$downloads / (d$downloads + d$isNAttr)
d$Acessos <- (d$downloads + d$isNAttr)

write.csv2(d,"freq_device.csv", row.names=FALSE)
rm(device,d)

os <-  table.ff(df$os,df$is_attributed)

d <- data.frame(os=rownames(os),isNAttr=unname(os[,1]),downloads=unname(os[,2]))
d$conv = d$downloads / (d$downloads + d$isNAttr)
d$Acessos <- (d$downloads + d$isNAttr)

write.csv2(d,"freq_os.csv", row.names=FALSE)
rm(os,d)

channel <-  table.ff(df$channel,df$is_attributed)

d <- data.frame(channel=rownames(channel),isNAttr=unname(channel[,1]),downloads=unname(channel[,2]))
d$conv = d$downloads / (d$downloads + d$isNAttr)
d$Acessos <- (d$downloads + d$isNAttr)

write.csv2(d,"freq_channel.csv", row.names=FALSE)
rm(channel,d)

