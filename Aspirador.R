source("Estado.R")

## Classe e métodos para o problema do mundo do Aspirador de Pó
Aspirador <- function(desc = NULL, pai = NULL){
  
  e <- environment()
  
 ## assign("pos", pos, envir = e)
  assign("desc", desc, envir = e)
  assign("pai", pai, envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  
  class(e) <- c("Aspirador", "Estado")
  
  return(e)
}

## Sobrecarregando o operador "==" para comparação entre estados
Ops.Aspirador = function(obj1,obj2){
  if(.Generic == "=="){
    return(all(obj1$desc == obj2$desc))
  }
}

## Sobrecarga da função genérica "print" do R
print.Aspirador <- function(obj) {
  cat("(A B1 B2 B3 B4): (", obj$desc, ")\n") ## A = Posição do Aspirador, Bn = Quadrantes
  cat("G(n): ", obj$g, "\n")
  cat("H(n): ", obj$h, "\n")
  cat("F(n): ", obj$f, "\n")
}

## Sobrecarga da função genérica "heuristica", definida por Estado.R
heuristica.Aspirador <- function(atual){
  
  if(is.null(atual$desc))
    return(Inf)
  ## h(obj) = b1+b2+b3+b4
  return(sum(atual$desc) - as.numeric(atual$desc[1]))
}

geraFilhos.Aspirador <- function(obj) {
  
  filhos <- list()
  
  filhosDesc <- list()
  
  desc <- obj$desc
  
  bAtual <- as.numeric(desc[1])
  b1 <- as.numeric(desc[2]) ## quadrante 1
  b2 <- as.numeric(desc[3]) ## quadrante 2
  b3 <- as.numeric(desc[4]) ## quadrante 3
  b4 <- as.numeric(desc[5]) ## quadrante 4
  
  ## gera filhos usando todos os operadores 
  ##  ___  ___
  ## | 3 || 4 |
  ## |___||___|
  ## | 1 || 2 |
  ## |___||___|
  
  ## Ordem das operações:
  ## Limpar, Ir para Direita, Ir para Baixo, Ir para Esquerda, Ir para Cima
  
  if(bAtual == 1){
    ## A B1 B2 B3 B4
    filhosAsp<- list(c(bAtual, 0, b2, b3, b4), ## limpou
                     c(2, b1, b2, b3, b4), ## foi pra direita
                     c(bAtual, b1, b2, b3, b4), ## não pode ir pra baixo
                     c(bAtual, b1, b2, b3, b4), ## não pode ir pra esquerda
                     c(3, b1, b2, b3, b4) ## foi pra cima
    )
  }
  if(bAtual == 2){
    ## A B1 B2 B3 B4
    filhosAsp<- list(c(bAtual, b1, 0, b3, b4), ## limpou
                     c(bAtual, b1, b2, b3, b4), ## não pode ir pra direita
                     c(bAtual, b1, b2, b3, b4), ## não pode ir pra baixo
                     c(1, b1, b2, b3, b4), ## foi pra esquerda
                     c(4, b1, b2, b3, b4) ## foi pra cima
                     
    )
    
  }
  if(bAtual == 3){
    ## A B1 B2 B3 B4
    filhosAsp<- list(c(bAtual, b1, b2, 0, b4), ## limpou
                     c(4, b1, b2, b3, b4), ## foi pra direita
                     c(1, b1, b2, b3, b4), ## foi pra baixo
                     c(bAtual, b1, b2, b3, b4), ## não pode ir pra esquerda
                     c(bAtual, b1, b2, b3, b4) ## não pode ir pra cima
                     
    )
    
  }
  if(bAtual == 4){
    ## A B1 B2 B3 B4
    filhosAsp<- list(c(bAtual, b1, b2, b3, 0), ## limpou
                     c(bAtual, b1, b2, b3, b4), ## não pode ir pra direita
                     c(2, b1, b2, b3, b4), ## foi pra baixo
                     c(3, b1, b2, b3, b4), ## foi pra esquerda
                     c(bAtual, b1, b2, b3, b4) ## não pode ir pra cima
                     
    )
    
  }
  
  ## Custos:
  ## Limpar = 2
  ## Ir para baixo ou para cima = 3
  ## Ir para direita ou para esquerda = 1
  custo <- c(2, 1, 3, 1, 3)
  
  i<-1
  filhosDesc <- filhosAsp
  
  ## gera os objetos Aspirador para os filhos
  for(filhoDesc in filhosDesc){
    filho <- Aspirador(desc = filhoDesc, pai = obj)
    filho$h <- heuristica(filho)
    filho$g <- obj$g + custo[i]
    i<-i+1
    filhos <- c(filhos, list(filho))
  }
  
  return(filhos)
}

