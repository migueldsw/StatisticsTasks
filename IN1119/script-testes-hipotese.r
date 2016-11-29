#########################
#Projeto Estatística - IN1119
#Scrip para testes de hipótese - testa se as turmas são equivalentes, ou se há diferença significante entre estas
#########################

#import data
T1<-na.omit(data.frame(read.csv("TURMA1.csv")))
T2<-na.omit(data.frame(read.csv("TURMA2.csv")))
T3<-na.omit(data.frame(read.csv("TURMA3.csv")))
T4<-na.omit(data.frame(read.csv("TURMA4.csv")))
T5<-na.omit(data.frame(read.csv("TURMA5.csv")))

##SAIDA PARA ARQUIVOS
# direct output to a file 
sink(paste("COMPARACAO_TURMAS.txt",sep=""), append=FALSE, split=FALSE)

getData <- function(M,f){
	out <- c()
	for (m in M){
		out<-c(out,f(m))
	}
	return(out)
}
mT1 <- getData(T1,mean)
mT2 <- getData(T2,mean)
mT3 <- getData(T3,mean)
mT4 <- getData(T4,mean)
mT5 <- getData(T5,mean)
vT1 <- getData(T1,var)
vT2 <- getData(T2,var)
vT3 <- getData(T3,var)
vT4 <- getData(T4,var)
vT5 <- getData(T5,var)

#dados com a média das notas finais
MDATA <- data.frame(mT1,mT2,mT3,mT4,mT5)
#dados com a variância das notas finais
VDATA <- data.frame(vT1,vT2,vT3,vT4,vT5)
nameList <- c("TURMA_1","TURMA_2","TURMA_3","TURMA_4","TURMA_5")

#testa se há diferença significante entre as turmas
TC<-function(M,nameList){
	#tests: wilcox.test, kruskal.test
	i = 0
	j = 0 
	cat(paste("Testes par a par para as turmas :\n")) 
	cat(paste("                   p-values: Wilcoxon test e Kruskal-Wallis test\n"))
	for (m in M){
		i = i+1
		for (n in M){
			j = j+1
			if (i < j){
				cat(paste(nameList[i], " e ", nameList[j], ":	"))
				t1 <- wilcox.test(m,n , paired = FALSE)
				t2 <- kruskal.test(data.frame(m,n))
				cat(paste(t1$p.value,"	",t2$p.value,"\n"))
			}
		}
		j = 0
	}
	
	cat(paste("Testes para todas as turmas:\n"))
	t1 <- kruskal.test(M)
	cat(paste("  Kruskal-Wallis test\n"))
	cat(paste("p-value =  ",t1$p.value,"\n\n"))
	
}

#Testes da média das notas finais
cat(paste("Testes da média das notas finais:\n"))
TC(MDATA,nameList)

#Testes da variância das notas finais
cat(paste("\nTestes da variância das notas finais:\n"))
TC(VDATA,nameList)

# return output to the terminal 
sink()
#END