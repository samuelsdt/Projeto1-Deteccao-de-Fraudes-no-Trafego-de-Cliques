#Script usado para colocar novos atributos na base (quantidade de acessos por IP e intervalo entre cliques), 
#esses atributos foram calculados usando a base completa, pois caso fossem calculados sobre uma amostra 
#a aletoriedade de amostra descaracterizaria esses atributos

#Em fun��o do tamanho do arquivo de treino, a leitura foi feita usando o pacote ff, foram colocados os novos 
#atributos e ap�s isso gerado uma base de 2.000.000 de registros para trabalho (em torno de 1% do arquivo completo)
#Por quest�es de desempenho tamb�m alguns dataframes de sumariza��o foram criados separadamente, podendo esses serem  
#inseridos ao arquivo que ser� utilizado para trabalho quando necess�rio

setwd("~/Data_Science/FCD_DSA/BigDataRAzure/Cap20/Projeto1")
library(ffbase)
df<- read.csv2.ffdf(file = "train.csv",VERBOSE = TRUE,sep=",",header=T,colClasses = c("integer","integer","integer","integer","integer","POSIXct","factor","integer"))

#contagem de acessos por ip inseridos em um novo dataframe
t <-  table.ff(df$ip)
t2 <- as.ffdf(data.frame(ip=names(t),Acessos=t))
t2$Acessos.Var1<-NULL
colnames(t2)[2] <- "Acessos"

#criando um DataFrame com as informa��es de Acesso por IP caso seja necess�rio para uso posterior
write.csv2(t2,"freq_IP.csv", row.names=FALSE)

#informa��o de acessos incluida no dataframe principal
df <- merge.ffdf(df,t2,by="ip")

#limpeza de vari�veis para otimiza��o de mem�ria
rm(t2,t)

mean(df$is_attributed)
#% de downloads = 0.2470721%

#ordena��o do dataframe por IP s�guido pelo momento de acesso
#cliques ordernados de forma cronol�gica por ip
df <- df[fforder(df$ip,df$click_time),]

#vetor contendo as linhas para gerar a amostra
tmn <- 2000000
smp <- sample(c(2:nrow(df)),tmn)

#dataframe contendo a amostra
df1 <- df[smp,]
#dataframe contendo as linhas anteriores da amostra 
#ser� utilizado para calcular o intervalo entre cliques quando o IP for igual
df2 <- df[smp-1,c("ip","click_time")]

#salvando o dataframe caso haja necessidade futura de utiliz�-lo novamente
save.ffdf(df)
rm(df,smp,tmn)

#c�lculo do intervalo entre cliques por IP, caso seja o primeiro clique do IP ficar� como NA
lstTimes <- unlist(lapply(c(1:nrow(df1)), function(x) ifelse(df1$ip[x]!=df2$ip[x],NA,as.integer(difftime(df1$click_time[x],df2$click_time[x],units = "secs")))))
df1$intervalo <- lstTimes

rm(df2,lstTimes)

#salvando o dataframe de trabalho
write.csv2(df1,"data_work.csv", row.names=FALSE)

