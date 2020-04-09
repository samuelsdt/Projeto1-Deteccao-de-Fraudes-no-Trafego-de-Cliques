#esse script será usado para treinar os modelos de machine learning, por se tratar de uma base desbalanceada,
#o critério de medição será a AUC (area under the curve), 
#será considerado satisfatório o algoritmo que apresentar AUC acima de 0.90 na base de testes que será gerada para validar o modelo

setwd("~/Data_Science/FCD_DSA/BigDataRAzure/Cap20/Projeto1")
library(data.table)
df  <- fread("data_work.csv", header = T, dec=",")

head(df)

#eliminando atributos desnecessários
df$click_time <- NULL
df$attributed_time <- NULL

#elminando atributos categoricos nominais com muitas categorias, esses atributos estarão representados
#a partir de estatisticas como a frequencia relativa de acessos, a representatividade dos downloads 
#e a taxa de conversão de download por clique
df$ip <- NULL
df$app <- NULL
df$device <- NULL
df$os <- NULL
df$channel <- NULL

#eliminando os atributos que estarão representados pela frequencia relativa
df$Acessos <- NULL
df$Acessos_App <- NULL
df$Acessos_OS <- NULL
df$Acessos_device <- NULL
df$Acessos_channel <- NULL

normaliza <- function (col)
{
  #unname(unlist(df[,"hour"]))
  mx = max(df[, col,with=F])
  mn = min(df[, col,with=F])
  
  df[,(col) := (unlist(lapply(df[,col,with=F],function(x){(x - mn) / (mx - mn)})))]
}

nmrVec <- c("hour","frq_rel_App","intervalo","frq_rel_OS","frq_rel_IP","frq_rel_channel","frq_rel_device","downloads_app","downloads_device","downloads_OS","downloads_channel")
for (i in nmrVec)
{
  normaliza(i)
}

geraFator <- function(col)
{
  df[,(col) := factor(unname(unlist(df[,col,with=F])))]
}

catVec <- c("catAcesApp","catAcesChannel","catIntervalo","catAcessos","catAcesOS","catAcesDevice","is_attributed")
for (i in catVec)
{
  geraFator(i)
}


#limpando os objetos usados
rm(catVec,geraFator,i,intVec,normaliza)

#dividindo a base em treino e teste (70% treino, 30% teste)
amostra <- sample(2,nrow(df),replace=T, prob=c(0.7,0.3))
treino <- df[amostra==1,]
teste <- df[amostra==2,]

#base está desbalanceada, portanto será aplicando SMOTE nos dados de treino
#com relação aos dados de teste serão mantidos os originais, pois assim poderemos testar 
#a eficacia do modelo com relação ao cenário real

library(DMwR)
treino <- SMOTE(is_attributed ~ ., treino, perc.over = 16000, k = 5, perc.under = 150)

table(treino$is_attributed)

library(randomForest)

#para a criação do primeiro modelo serão deixados os atributos de frequencia relativa (normalizados) e as categorias
#criadas a partir desses atributos, a partir disso serão avaliados os atributos com maior relevância para a construção do modelo

#testando modelo de randomForest com todas as variáveis
rf = randomForest(is_attributed ~ .,data=treino, ntree=100,importance=T)

library(ROCR)
library(caret)

#avaliando a performance do modelo
previsao = predict(rf,teste, type = 'prob')
pred <- prediction(previsao[,2],teste$is_attributed)
perf <- performance(pred, "auc")

#avaliando a importancia das variáveis
varImp(rf)
varImpPlot(rf)

#os atributos de frequencia mostraram-se mais efetivos para a construção do modelo em comparação com a divisão dos valores por categoria
#criando modelo otimizado
rf2 <- randomForest(is_attributed ~ conversao_app + hour + downloads_app + frq_rel_IP + intervalo + conversao_channel + frq_rel_OS + downloads_OS + conversao_OS + downloads_channel + frq_rel_App, data=treino, data=treino, ntree=100)

#avaliando a performance
prevrf <- predict(rf2,teste, type = 'prob')
predrf <- prediction(prevrf[,2],teste$is_attributed)
perfrf <- performance(predrf, "auc") 
#perfomance de 0.9251712

#testtando outros algoritmos
library(e1071)

#naive bayes
nvb = naiveBayes(is_attributed ~ conversao_app + hour + downloads_app + frq_rel_IP + intervalo + conversao_channel + frq_rel_OS + downloads_OS + conversao_OS + downloads_channel + frq_rel_App, data=treino)


#avaliando a performance
prevnvb <- predict(nvb,teste)
prednvb <- prediction(as.integer(prevnvb),as.integer(teste$is_attributed))
perfnvb <- performance(prednvb, "auc")
#perfomance de 0.8984436

#svm
#devido a demora de convergência do SVM, o algoritmo será treinado com uma amostra da base de treino contendo 100.000 registros
amostra = sample(c(1:nrow(treino)),size=100000)
trn_svm = treino[amostra]
svm_ml = svm(is_attributed ~ conversao_app + hour + downloads_app + frq_rel_IP + intervalo + conversao_channel + frq_rel_OS + downloads_OS + conversao_OS + downloads_channel + frq_rel_App, data=trn_svm, type = 'C-classification', kernel = 'linear')

#avaliando a performance
prevsvm <- predict(svm_ml,teste)
predsvm <- prediction(as.integer(prevsvm),as.integer(teste$is_attributed))
perfsvm <- performance(predsvm, "auc")
#perfomance de 0.9130691

library(class)
#knn
knn_model <- knn(train = treino[,c("conversao_app","hour","downloads_app","frq_rel_IP","intervalo","conversao_channel","frq_rel_OS","downloads_OS","conversao_OS","downloads_channel","frq_rel_App")], test = teste[,c("conversao_app","hour","downloads_app","frq_rel_IP","intervalo","conversao_channel","frq_rel_OS","downloads_OS","conversao_OS","downloads_channel","frq_rel_App")],cl = treino$is_attributed, k = 11)

#avaliando a performance
predknn <- prediction(as.integer(knn_model),as.integer(teste$is_attributed))
perfknn <- performance(predknn, "auc")
#perfomance de 0.8497461

#Regressao Logistica
rl <- glm(is_attributed ~ conversao_app + hour + downloads_app + frq_rel_IP + intervalo + conversao_channel + frq_rel_OS + downloads_OS + conversao_OS + downloads_channel + frq_rel_App, family=binomial(link="logit"), data=treino)

prevrl = predict(rl,newdata=teste,type='response')
prevrl = ifelse(prevrl > 0.5,1,0)

#avaliando a performance
predrl <- prediction(prevrl,(as.integer(teste$is_attributed)-1))
perfrl <- performance(predrl, "auc")
#perfomance de 0.9113481

#eXtreme Gradient Boosting 
library(xgboost)
catVec <- c("catAcesApp","catAcesChannel","catIntervalo","catAcessos","catAcesOS","catAcesDevice","is_attributed")
for (i in catVec)
{
  treino[,(i) := as.integer(unname(unlist(treino[,i,with=F])))]
  teste[,(i) := as.integer(unname(unlist(teste[,i,with=F])))]
}


mtx_train = as.matrix(treino[,-c("is_attributed")])
lbl_train = (as.integer(treino$is_attributed)-1)
mtx_test = as.matrix(teste[,-c("is_attributed")])


xgb = xgboost(data = mtx_train[,c("conversao_app","hour","downloads_app","frq_rel_IP","intervalo","conversao_channel","frq_rel_OS","downloads_OS","conversao_OS","downloads_channel","frq_rel_App")], label = lbl_train, nthread = 2, nrounds = 25, objective = "binary:logistic", max.depth=2)

prevxgb <- predict(xgb,mtx_test[,c("conversao_app","hour","downloads_app","frq_rel_IP","intervalo","conversao_channel","frq_rel_OS","downloads_OS","conversao_OS","downloads_channel","frq_rel_App")])
prevxgb <- round(prevxgb)
predxgb <- prediction(prevxgb,(as.integer(teste$is_attributed)-1))
perfxgb <- performance(predxgb, "auc")
#perfomance de 0.8703483

#rede neural
library(h2o)

h2o.init()

treino <- as.h2o(treino)
teste <- as.h2o(teste)

#criando um grid com algumas opções de neuronios escondidos
deep_param <- list(hidden=list(c(12,12,12),c(16,16,16),c(24,24,24),c(32,32,32),c(48,48,48),c(64,64,64)))
deep_grid <-h2o.grid("deeplearning", grid_id="deep_grid", x = c("conversao_app","hour","downloads_app","frq_rel_IP","intervalo","conversao_channel","frq_rel_OS","downloads_OS","conversao_OS","downloads_channel","frq_rel_App"),  y = "is_attributed",  training_frame = treino,  validation_frame = teste,  distribution = "AUTO",  activation = "RectifierWithDropout",  sparse = TRUE, epochs = 20, stopping_metric="AUC", variable_importances=F, score_training_samples = 100000, hyper_params = deep_param)
deep_grid_perf <- h2o.getGrid(grid_id = "deep_grid", sort_by = "auc", decreasing = TRUE)

mdl_dl = h2o.getModel(deep_grid_perf@model_ids[[1]])
h2o.auc(h2o.performance(mdl_dl,teste))
#perfomance de 0.953089 (modelo com hidden=c(24,24,24))

#Gradient Boosting
gbm = h2o.gbm(x = c("conversao_app","hour","downloads_app","frq_rel_IP","intervalo","conversao_channel","frq_rel_OS","downloads_OS","conversao_OS","downloads_channel","frq_rel_App"),  y = "is_attributed",  training_frame = treino,  validation_frame = teste, ntrees = 500, stopping_metric ="AUC")


h2o.auc(h2o.performance(gbm,teste))
#perfomance de 0.9551788

#os modelos acima foram testados usando diversas variações nos parâmetros, porém foram deixados no script os 
#parâmetros que apresentaram melhor performance em cada algoritmo


#entre todos os algoritmos, o que apresentou melhor performance foi o Gradient Boosting

