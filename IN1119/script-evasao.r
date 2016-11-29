#########################
#Projeto Estatística - IN1119
#Scrip para verificação da evasão -> notas indefinidas
#########################

#import data
T1<-data.frame(read.csv("TURMA1.csv"))
T2<-data.frame(read.csv("TURMA2.csv"))
T3<-data.frame(read.csv("TURMA3.csv"))
T4<-data.frame(read.csv("TURMA4.csv"))
T5<-data.frame(read.csv("TURMA5.csv"))

##SAIDA PARA ARQUIVOS
# direct output to a file 
sink(paste("VERIFICACAO_EVASAO.txt",sep=""), append=FALSE, split=FALSE)
pdf(paste("VERIFICACAO_EVASAO_PLOTS.pdf",sep=""))

#apresenta a porcentagem de Evasão nas matérias de uma turma
evad<-function(M, name){	
	cat(paste("VERIFICAÇÂO DA EVASAO PARA ",name,"\n"))
	qtdna<-c()
	for (m in M){
		qtdTotal<-length(m)
		qtdna<-c(qtdna,(sum(is.na(m))/qtdTotal)*100)
	}
	cat("Taxa de Evasão \n","Min. | 1ºQua. | Mediana | Média | 3ºQua. | Max. | \n")
	cat(summary(qtdna),sep="  |  ","\n\n")
	plot(qtdna,main=paste("Evasão - ",name), xlab = "Disciplina" , ylab = "Evasão (%)")
	lines(qtdna)
	
	return(qtdna)
}

cat("***ESTUDO DE 	EVASÃO***\nEvasão (percentual) por turmas:\n")

#dados com as proporções 
et1 <- evad(T1, "TURMA_1")
et2 <- evad(T2, "TURMA_2")
et3 <- evad(T3, "TURMA_3")
et4 <- evad(T4, "TURMA_4")
et5 <- evad(T5, "TURMA_5")

#####################
##TESTES DE PROPORÇÃO 
#cria os vetores com as quantidades de dados (NA e total) nas disciplinas
qtdTotalVector<-function(M){
	out <- c()
	for (m in M){
		qtdTotal<-length(m)
		out <- c(out, qtdTotal)
	}
	return (out)
}
qtdNAVector<-function(M){
	out <- c()
	for (m in M){
		qtdNA<-sum(is.na(m))
		out <- c(out, qtdNA)
	}
	return (out)
}

#vetores com as quantidades de dados (NA e total)
nat1<-qtdNAVector(T1)
ttt1<-qtdTotalVector(T1)
nat2<-qtdNAVector(T2)
ttt2<-qtdTotalVector(T2)
nat3<-qtdNAVector(T3)
ttt3<-qtdTotalVector(T3)
nat4<-qtdNAVector(T4)
ttt4<-qtdTotalVector(T4)
nat5<-qtdNAVector(T5)
ttt5<-qtdTotalVector(T5)

#vetores com os valores maximos de NA e TOTAL, para avaliar a proporção de evasão
vna<-c(max(nat1),max(nat2),max(nat3),max(nat4),max(nat5))
vtot<-c(max(ttt1),max(ttt2),max(ttt3),max(ttt4),max(ttt5))

cat("\nTESTES DE PROPORÇÃO (testando contra um p=0.1906):\n")

#Testes de proporção, teste contra o valor do CENSO EAD 2013: 
#evasão de 19,06% em cursos regulamentados totalmente a distância.
#testes unilateral "greater" : Hipótese alternativa: p_real é maior que p 
t1<-prop.test(max(nat1),max(ttt1),p=0.1906,alternative = "greater")
t2<-prop.test(max(nat2),max(ttt2),p=0.1906,alternative = "greater")
t3<-prop.test(max(nat3),max(ttt3),p=0.1906,alternative = "greater")
ta<-prop.test(max(nat4),max(ttt4),p=0.1906,alternative = "greater")
tb<-prop.test(max(nat5),max(ttt5),p=0.1906,alternative = "greater")
#teste de proporção para mais de uma amostra (bilateral) 
tt<-prop.test(vna,vtot,p=rep(c(0.1906),5))

cat(paste("Testes por turma (unilateral 'maior'):\n"))
cat(paste("         p-value\n"))
cat(paste("TURMA 1: ",t1$p.value,"\n"))
cat(paste("TURMA 2: ",t2$p.value,"\n"))
cat(paste("TURMA 3: ",t3$p.value,"\n"))
cat(paste("TURMA 4: ",ta$p.value,"\n"))
cat(paste("TURMA 5: ",tb$p.value,"\n"))

cat(paste("Testes para todas as turmas (bilateral - mais de 2 amostras):\n"))
cat(paste("p-value = : ",tt$p.value,"\n"))

barplot(evadcursos, main="Evasão (%) por Turmas",xlab="turma",ylab="Evasão (%)", horiz=FALSE, names.arg=c("TURMA 1", "TURMA 2", "TURMA 3", "TURMA A", "TURMA B"), cex.names=0.8)

# return output to the terminal 
sink()
# close plot to file
dev.off()
#END