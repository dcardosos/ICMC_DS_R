# Lista 2
## Q1 
tibble::tibble(
  codigo = c(234, 111, 562, 452, 829, 198, 335, 723, 661),
  descricao = c("detergente", "macarrão", "açucar", "molho de tomate",
                "desinfetante", "sabão em pó", "farinha", "arroz", "esponja"),
  tipo = c("limpeza", "alimento", "alimento", "alimento", "limpeza", "limpeza",
           "alimento", "alimento", "limpeza"),
  preco_unitario = c(2.3, 3.4, 3.19, 0.99, 3.3, 6.49, 2.99, 11.82, 4.40),
  quantiadade_estoque = c(100, 78, 40, 33, 19, 28, 85, 60, 50)) 

## Q2
n_divisible_by <- function(n0 = 1, n = 959, by_n = 3){
  
  tibble::tibble(
    numbers = n0:n,
    divisible_by = numbers %% by_n == 0) |>
  dplyr::filter(divisible_by) |>
  dplyr::summarise(quantidade = dplyr::n())
}

n_divisible_by()


## Q3
logaritmo <- function(){
  
  tibble::tibble(
    vetor = 1:64,
    log = log(x = vetor, base = 15)) |>
    purrr::pluck('log') |>
    matrix(nrow = 8, ncol = 8, byrow = TRUE)
  
}

logaritmo()

## Q4
soma <- function(n){
  
  cat("A soma com a função `sum` é de:", sum(1:n), "\n")
  cat("A soma com a fórmula dada é de:", (n*(n+1)) / 2, "\n")
  cat("A diferença é de:", sum(1:n) - (n*(n+1)) / 2)
}

soma(n = as.integer(
  readline("Escolha um número maior que zero:")))

  
## Q6
media_ponderada <- function(v1, v2){
  
  tibble::tibble(
    vetor = v1,
    peso = v2,
    ponderado = vetor * peso) |>
    dplyr::summarise(media_pond = sum(ponderado) / sum(peso))
  
}

media_ponderada(c(11, 9, 8, 7, 11, 6, 7, 9),
                c(1, 2, 1, 2, 1, 2, 1, 2))
  
## Q7
celsius_to_farenheit <- function(graus_C){
  
  (9/5) * graus_C + 32
  
}

tibble::tibble(
  celsius = c(0,25, 50, 75, 100),
  farenheit = celsius_to_farenheit(celsius))

## Q8
fatorial <- function(n){
  
  1:n |> purrr::reduce(`*`)

}

purrr::map(1:10, fatorial)

## Q9
is_even <- function(x){
  
  x %% 2 == 0

}

tibble::tibble(x = 1:10, é_par = is_even(x))

## Q10
