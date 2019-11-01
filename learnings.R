#bin
cut(carat, 5, include.lowest=TRUE)


#separar os atributos nominais em diferentes colunas. Se colocar ., a função
#vai escolher todos os atributos nominais. Se quiser espeficificar qual quer
#transformar, só escrever o nome no atributo na fórmula. Nesse caso estamos separando
#em diferente colunas, porque só queremos olhar para quatro dos 13 tipos de produtos
newDataFrame <- dummyVars(" ~ .", data = existing_products)
cleanData <- data.frame(predict(newDataFrame, newdata = existing_products))