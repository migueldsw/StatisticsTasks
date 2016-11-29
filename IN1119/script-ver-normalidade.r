#########################
#Projeto Estatística - IN1119
#Scrip para verificação da normalidade dos dados
#########################

#import libs
library(nortest)

#import data
T1<-data.frame(read.csv("TURMA1.csv"))
T2<-data.frame(read.csv("TURMA2.csv"))
T3<-data.frame(read.csv("TURMA3.csv"))
T4<-data.frame(read.csv("TURMA4.csv"))
T5<-data.frame(read.csv("TURMA5.csv"))

#to omit NA values
#T1<-na.omit(T1) 
#T2<-na.omit(T2) 
#T3<-na.omit(T3) 
#T4<-na.omit(T4) 
#T5<-na.omit(T5) 

#função que realiza análise exploratória dos dados e todos os testes de verificação de normalidade
VERNORM<-function(M,name){
	MNA<-M
	M<-na.omit(M) 
	##SAIDA PARA ARQUIVOS
	# direct output to a file 
	sink(paste(name,"_VERIFICACAO_NORMALIDADE.txt",sep=""), append=FALSE, split=FALSE)
	pdf(paste(name,"_VERIFICACAO_NORMALIDADE_PLOTS.pdf",sep=""))

	cat("***************************************\n")
	cat(paste("VERIFICAÇÂO DE NORMALIDADE PARA ",name,"\n"))
	#boxplot all
	boxplot(M,main = paste("Boxplot ",name,sep=""), xlab = "Matérias" ,ylab = "Notas");
	
	for (i in 1:13){
		cat("---------------------------------------------------------------------\n")
		cat(paste("Matéria",i," (m",i,"):\n",sep=""))
		mna<-MNA[,i];
		m<-M[,i];
		
		#quantidade de notas na matéria
		cat("Quantidade de alunos = ", length(mna),"\n")
		cat("Quantidade de notas indefinidas (NA's) = ", sum(is.na(mna)),"\n")
		cat("Quantidade de notas = ", length(mna)-sum(is.na(mna)),"\n")
		#summary
		cat("Min. | 1ºQua. | Mediana | Média | 3ºQua. | Max. | NA's \n")
		cat(summary(mna),sep="   |  ")
		
		#plot histograma m
		hist(m, main=paste("Histograma Matéria",i," (m",i,")",sep=""),xlab = "Nota" ,ylab = "frequência");
	
		#verificar normalidade dos dados 
		#Normal Q-Q Plot
		qqnorm(m, main=paste("Matéria",i," (m",i,") - Normal Q-Q Plot",sep=""), xlab = "Quantis Teoricos N(0,1)" , ylab = "Nota"); qqline(m,lty=2,col=2);
	
		#TESTES DE NORMALIDADE
		# Estimativas dos parâmetros
		xb <- mean(m) # mu
		sx <- sd(m) # sigma
		cat("\nMédia amostral =", xb, "\nDesvio padrão amostral =", sx,"\n")

		# Testes
		t1 <- ks.test(m, "pnorm", xb, sx) # KS
		t2 <- lillie.test(m) # Lilliefors
		t3 <- cvm.test(m) # Cramér-von Mises
		t4 <- shapiro.test(m) # Shapiro-Wilk
		t5 <- sf.test(m) # Shapiro-Francia
		t6 <- ad.test(m) # Anderson-Darling

		# Tabela de resultados
		testes <- c(t1$method, t2$method, t3$method, t4$method, t5$method,t6$method)
		estt <- as.numeric(c(t1$statistic, t2$statistic, t3$statistic,t4$statistic, t5$statistic, t6$statistic))
		valorp <- c(t1$p.value, t2$p.value, t3$p.value, t4$p.value, t5$p.value,t6$p.value)
		resultados <- cbind(estt, valorp)
		rownames(resultados) <- testes
		colnames(resultados) <- c("Estatística", "p-value")
		cat("\nResultados dos testes de normalidade:\n")
		print(resultados, digits = 4)
	}
	cat("\nxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
	
	# return output to the terminal 
	sink()
	# close plot to file
	dev.off()
}
#####fim da função

VERNORM(T1,"TURMA_1")
VERNORM(T2,"TURMA_2")
VERNORM(T3,"TURMA_3")
VERNORM(T4,"TURMA_4")
VERNORM(T5,"TURMA_5")

#END