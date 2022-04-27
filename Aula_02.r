require(fdth)
library(readxl)
        questionario <- read_excel("/home/pierre.guedes21/Downloads/respostas.xlsx")
        view(questionario)
        questionario
        summary(questionario)
        questionario$`Região de Procedência`<-factor(questionario$`Região de Procedência`)
        names(questionario)
        length (questionario)
       
        round(table(questionario$`Região de Procedência`)/length(respostas$`Região de Procedência`)*100,1)
       
        par(mfrow=c(1,1))
       
        Sim =c(68.5, 20.4, 7.4, 0, 1.9, 0, 1.9, 0)
       
        par(las=1) # nomes dos eixos perpendicular
        par(mar=c(5,22,1,1)+0.1) # para aumentar a margem a esquerda

        barplot((sort(table(respostas$Sexo), decreasing=T))/length(respostas$Sexo)*100, xlab = "Refrigerante", ylab = "% entrevistados", col=c("red","blue"), ylim=c(0,40))
        legend("topright", c("Feminino","Masculino"), cex = 0.8,
               fill = c("blue","red"))
       
        cols <- c("grey","yellow","blue","red","green")
        pielabels<- paste(round(table(respostas$Sexo)/length(respostas$Sexo)*100), "%", sep="")
        pie(round((table(respostas$Sexo)/length(respostas$Sexo)*100),2),labels=pielabels, cex=1.2, col=cols)
        legend("topright", c("Coke Classic","Diet Coke","Dr. Pepper","Pepsi","Sprite"), cex = 0.8,
               fill = c("grey","yellow","blue","red","green"))
        cols <- c("red","blue")
        tab=fdt(var_quant, start=2,h=3,end=23) # ver os dados ordenados para definir os valores de start, h e end
        plot(tab, type='fh', xlab="tempo de espera (min)",ylab="nº pacientes")
        par(new=TRUE)  
        plot(tab,type='fp', xlab="tempo de espera (min)",ylab="nº pacientes", col = "black")  
        pielabels<- paste(round(table(questionario$Sexo)/length(questionario$Sexo)*100), "%", sep="")
        pie(round((table(questionarios$Sexo)/length(questionario$Sexo)*100),2),labels=pielabels, cex=1.2, col=cols)
        legend("topright", c("Feminino","Masculino"), cex = 0.8,
               fill = c("red","blue"))
        tab=fdt(respostas$Peso..em.quilogramas., start=47,h=15,end=140) # ver os dados ordenados para definir os valores de start, h e end
        plot(tab, type='rfph', xlab="peso (kg)",ylab="% entrevistados")
       
        par(new=TRUE)
        plot(tab,type='rfpp', xlab="peso (kg)",ylab="% entrevistados", col = "black")
       
        boxplot(respostas$Peso..em.quilogramas., ylab = "peso (kg)")
        points(mean(respostas$Peso..em.quilogramas.), pch=3) # para inserir a média no gráfico
       
        tab=fdt(var_quant, start=2,h=3,end=23) # ver os dados ordenados para definir os valores de start, h e end
       
        par(mfrow=c(1,3))
       
        boxplot(var_quant, ylab = "tempo de espera (min)", cex.axis=1.6, cex.lab=1.6) #diagrama de caixas
        points(mean(var_quant), pch=3)
       
        plot(tab, type='rfph', xlab="tempo de espera (min)",ylab="% pacientes", cex.axis=1.6, cex.lab=1.6)
        par(new=TRUE)
        plot(tab,type='rfpp', xlab="tempo de espera (min)",ylab="% pacientes", col = "black", cex.axis=1.6, cex.lab=1.6) # histograma e poligono de frequencia
       
        plot(tab,type='cfpp', xlab="tempo de espera (min)",ylab="% acumulados", ylim=c(0,100), col = "black",cex.axis=1.6, cex.lab=1.6 ) #ogiva de Galton (% acumulados)
