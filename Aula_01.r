library(readxl)
questionario <- read_excel("/home/tainara.santos03/Downloads/database_example_xlsx.xlsx")
View(questionario)
questionario
summary(questionario)
questionario$`Região de Procedência`<-factor(questionario$`Região de Procedência`)
names(questionario)
length(questionario)

round(table(questionario$`Região de Procedência`)/length(questionario$`Região de Procedência`)*100,1)

par(mfrow=c(1,1))

Sim =c(68.5, 20.4, 7.4, 0, 1.9, 0, 1.9, 0)

par(las=1) # nomes dos eixos perpendicular
par(mar=c(5,22,1,1)+0.1) # para aumentar a margem a esquerda

x <- barplot(Sim, xlab = "% estudantes",
             names.arg = c(row.names = c("São José dos Campos ",
                                         "Vale do Paraíba (com exceção de São José dos Campos)",
                                         "Outros municípios do estado da Região Sudeste",
                                         "Região Sul",
                                         "Região Norte",
                                         "Região Nordeste",
                                         "Região Centro Oeste")), horiz = TRUE)
text(Sim-0.5*Sim, x, labels=round(Sim), col="black", cex=1.0) # grafico de colunas

table(questionario$`Número de calçado`)

round (table(questionario$`Qual o seu curso?`)/length(questionario$`Qual o seu curso?`))

cols <- c("red","blue")
pielabels<- paste(round(table(questionario$Sexo)/length(questionario$Sexo)*100), "%", sep="")
pie(round((table(questionario$Sexo)/length(questionario$Sexo)*100),2),labels=pielabels, cex=1.2, col=cols)
legend("topright", c("Feminino","Masculino"), cex = 0.8,
       fill = c("red","blue")) # grafico de pizzas

barplot(table(questionario$`Qual a sua carga horária semanal no trabalho?`)) # cria o gráfico de barras
barplot(table(questionario$`Qual a sua carga horária semanal no trabalho?`), xlab = "refrigerante", ylab = "nº entrevistados") # cria o gráfico de barras e coloca título nos eixos
barplot(table(questionario$`Qual a sua carga horária semanal no trabalho?`), xlab = "refrigerante", ylab = "nº entrevistados", ylim=c(0,20)) # define o mínimo e máximo do eixo
barplot(table(questionario$`Qual a sua carga horária semanal no trabalho?`), xlab = "refrigerante", ylab = "nº entrevistados", cex.lab=0.7, cex.names=0.7, cex.axis=0.7, col="lightgreen", ylim=c(0,20)) # deixa o gráfico na cor verde
par(mar=c(5,4,4,2)+0.1) # para retornar a margem default

require (fdth)
tab=fdt(questionario$Peso..em.quilogramas., start=47,h=15,end=30) # ver os dados ordenados para definir os valores de start, h e end
plot(tab, type='rfph', xlab="tempo de espera (min)",ylab="% pacientes") # histograma

tab=fdt(questionario$Peso..em.quilogramas., start=47,h=15,end=140) # ver os dados ordenados para definir os valores de start, h e end
plot(tab, type='rfph', xlab="peso (kg)",ylab="% entrevistados")

par(new=TRUE)
plot(tab,type='rfpp', xlab="peso (kg)",ylab="% entrevistados", col = "black")

boxplot(questionario$Peso..em.quilogramas., ylab = "peso (kg)")
points(mean(questionario$Peso..em.quilogramas.), pch=3) # para inserir a média no gráfico

#gráfico de caixas

tab=fdt(questionario$Peso..em.quilogramas., start=47,h=15,end=140) # ver os dados ordenados para definir os valores de start, h e end
plot(tab, type='rfph', xlab="peso (kg)",ylab="% entrevistados")

par(new=TRUE)
plot(tab,type='rfpp', xlab="peso (kg)",ylab="% entrevistados", col = "black")

boxplot(questionario$Peso..em.quilogramas., ylab = "peso (kg)")
points(mean(questionario$Peso..em.quilogramas.), pch=3) # para inserir a média no gráfico

tab=fdt(questionario$Peso..em.quilogramas., start=47,h=15,end=1
        par(mfrow=c(1,3))
