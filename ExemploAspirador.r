debugSource("Aspirador.R")
debugSource("buscaDesinformada.R")
debugSource("buscaInformada.R")

## A B1 B2 B3 B4
inicial <- Aspirador(desc = c(1, 1, 0, 1, 1)) ##  quadrantes 1, 3 e 4 sujos e o aspirador começando no quadrante 1

objetivo <- Aspirador()
objetivo$desc <- c(1, 0, 0, 0, 0) ## objetivo (exemplo) acabar no quadrante 1

cat("====\tBusca em Largura\t====\n")
print(unlist(buscaEmLargura(inicial, objetivo)))

cat("====\tBusca em Profundidade\t====\n")
print(buscaEmProfundidade(inicial, objetivo))

cat("====\tBusca de Custo Uniforme\t====\n")
print(buscaCustoUniforme(inicial, objetivo))

cat("====\tBusca Gulosa\t====\n")
print(buscaBestFirst(inicial, objetivo, "Gulosa"))

cat("====\tBusca A*\t====\n")
print(buscaBestFirst(inicial, objetivo, "AEstrela"))

