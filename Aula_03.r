require(fdth)
library(readxl
questionario <- read_excel("/home/tainara.santos03/Downloads/respostas.xlsx")
view(questionario)
questionario
summary(questionario)
questionario$`Região de Procedência`<-factor(questionario$`Região de Procedência`)
names(questionario)
length (questionario)

round(table(respostas$`Região de Procedência`)/length(respostas$`Região de Procedência`)*100,1)

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
text(Sim-0.5*Sim, x, labels=round(Sim), col="black", cex=1.0)
