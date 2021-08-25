library(ggplot2)
library(patchwork)
library(magrittr)
# 1 Visualização 

## Histograma
tibble::tibble(
  x = c(21,22,23,4,5,6,77,8,9,10,31,32,
        33,34,35,36,37,18,49,50,100)) %>% 
  ggplot(aes(x)) +
  geom_histogram(binwidth = 9, fill = '#7F7FFF', color = 'black') +
  theme_minimal() +
  labs(x = 'X', y = 'Frequência', title = 'Histograma qualquer')

## Frequencia
tibble::tibble(
  x = c(21,22,23,4,5,6,77,8,9,10,31,32,
        33,34,35,36,37,18,49,50,100)) %>% 
  ggplot(aes(x)) +
  geom_histogram(stat = 'density', fill = '#7F7FFF') +
  theme_minimal() +
  labs(x = 'X', y = 'f(x)', title = 'Histograma qualquer')

tibble::tibble(
  x = c(21,22,23,4,5,6,77,8,9,10,31,32,
        33,34,35,36,37,18,49,50,100)) %>% 
  ggplot(aes(x)) +
  geom_density(color = '#7F7FFF', fill = '#7F7FFF', alpha = 0.5) +
  theme_minimal() +
  labs(x = 'X', y = 'f(x)', title = 'Gráfico de densidade')


### Valores nominais
tibble::tibble(
  x = c("Bom", "Ruim", "Ótimo", "Regular", "Regular", "Ótimo", 
        "Ótimo","Bom", "Ótimo", "Bom", "Ótimo")) %>% 
  ggplot(aes(x)) +
  geom_bar(fill = '#7F7FFF') +
  theme_minimal() +
  labs(x = 'Valores', 
       y = 'Frequência', 
       title = 'Gráfico de barras - variáveis nominais')

### Representação sem ordem
tibble::tibble(
  x = c("Bom", "Ruim", "Ótimo", "Regular", "Regular", "Ótimo", 
        "Ótimo","Bom", "Ótimo", "Bom", "Ótimo")) %>%
  dplyr::group_by(x) %>% 
  dplyr::summarise(frequencia = dplyr::n()) %>%
  dplyr::mutate(total = sum(frequencia),
                pctg = frequencia / total * 100,
                pctg_escrita = paste0(round(pctg, 2), "%")) %>% 
  ggplot(aes(x = '', y = frequencia, fill = x)) +
  geom_bar(stat = 'identity') +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(y = c(9, 5.5, 2, 0.5), label = pctg_escrita), 
            color = "white", 
            size = 4, nudge_x = 0.2) +
  guides(fill = guide_legend(title = "Rótulo")) +
  labs(title = 'Gráfico de pizza - variáveis nominais',
       subtitle = 'Forma de demonstrar os dados sem inferir uma ordem')
  
## Scatter plot
tibble::tibble(
  x = seq(-1, 1, length.out = 100),
  erro = stats::runif(100, -1, 1),
  sigma = 0.5,
  y = 0.8 * x + erro * sigma) %>%
  ggplot(aes(x, y)) +
  geom_point(color = '#7F7FFF') +
  theme_minimal() +
  labs(x = 'X', y = 'Y', title = 'Scatter plot de uma relação linear') +
  geom_smooth(method = 'lm', color = 'dark blue')


# 2 - Medidas de posição
## Moda
getmode <- function(x) {
  uniqv <- unique(x)
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  
  glue::glue("A moda é {ux[tab == max(tab)]}")
  
}

getmode(c(1,2,3,1,2,3,4,1,3,6,4,1))
getmode(c(1,1,2,2,3))

tibble::tibble(x = c(1,1,2,2,3)) %>% 
  ggplot(aes(x)) +
  geom_bar(fill = '#7F7FFF', width = 0.8) +
  theme_minimal() +
  labs(x = 'Valores', y = 'Frequência', title = 'Moda')

## Média e mediana

tibble::tibble(
  mu = 0,
  sigma = 10,
  y = stats::rnorm(500, mu, sigma)) -> da 

centro_mean <- mean(da$y)
centro_median <- median(da$y)

da %>% 
  ggplot(aes(y)) +
  geom_histogram(fill = '#7F7FFF') +
  geom_vline(xintercept = centro_mean, size = 1) +
  geom_vline(xintercept = centro_median, size = 1, linetype = 'dashed') +
  theme_minimal() +

da %>% 
  ggplot(aes(y)) +
  geom_density(fill = '#7F7FFF', color = '#7F7FFF') +
  geom_vline(xintercept = centro_mean, size = 1) +
  geom_vline(xintercept = centro_median, size = 1, linetype = 'dashed') +
  theme_minimal()  
  
  
#### distribuição polinomial
tibble::tibble(
  lbda = 100,
  beta = 1.0/lbda,
  y = stats::rexp(500, beta)) -> da_exp

exp_centro_mean <- mean(da_exp$y)
exp_centro_median <- median(da_exp$y)

da_exp %>% 
  ggplot(aes(y)) +
  geom_histogram(fill = '#7F7FFF') +
  geom_vline(xintercept = exp_centro_mean, size = 1) +
  geom_vline(xintercept = exp_centro_median, size = 1, linetype = 'dashed') +
  theme_minimal() +

da_exp %>% 
  ggplot(aes(y)) +
  geom_density(fill = '#7F7FFF', color = '#7F7FFF') +
  geom_vline(xintercept = exp_centro_mean, size = 1) +
  geom_vline(xintercept = exp_centro_median, size = 1, linetype = 'dashed') +
  theme_minimal() 
  
## Quantis
readr::read_csv('dados/iris.csv', show_col_types = FALSE) %>% 
  ggplot(aes(x = species, y = petal_length, fill = species)) +
  geom_boxplot() +
  scale_fill_manual(values = c("green", "#E69F00", "#56B4E9")) +
  theme_minimal(base_size = 15) +
  guides(fill = guide_legend(title = element_blank())) +
  labs(x = 'Espécie', y = ' Comprimento da pétala', 
       title = 'Boxplot do comprimento da pétala por espécie')


## Medidas de dispersão

### Variância
variancia <- function(x){
  
  tibble::tibble(
    xi = x,
    n = length(xi),
    mu = mean(xi),
    s = (xi - mu)^2,
    soma = purrr::reduce(s, `+`),
    variancia = soma / (n - 1)) %>% 
    purrr::pluck('variancia') %>% 
    unique()  
}

variancia(c(0,0,1,1,18)) == var(c(0,0,1,1,18))
variancia(c(4,4,4,4,4)) == var(c(4,4,4,4,4))

### IQR
stats::IQR(c(0,0,1,1,18))
stats::IQR(c(4,4,4,4,4))

### Amplitude
max(c(0,0,1,1,18)) - min(c(0,0,1,1,18))
max(c(4,4,4,4,4)) - min(c(4,4,4,4,4))

### Resumo descritivo
readr::read_csv('dados/iris.csv') %>% 
  summary()

# 4. Correlação - Pearson

fabrica_pearson <- function(sigma){
  
  function(x, erro){
    
    tibble::tibble(id = 1:length(x),
                   resultado = -0.8 * x + erro * sigma)
    
  } 
}

tibble::tibble(
  sigma = seq(0, 2.2, by = 0.2)) %>% 
  dplyr::mutate(
    data = list(
      tibble::tibble(
        x = seq(-1, 1, length.out = 100),
        id = 1:length(x),
        erro = stats::runif(100, -1, 1))),
    
    equacao = purrr::map(.x = sigma, 
                         .f= ~ fabrica_pearson(.x)),
    
    aplicacao = purrr::map2(.x = data,
                            .y = equacao,
                            .f = ~ .y(.x$x, .x$erro)),
    
    join = purrr::map2(.x = data,
                       .y = aplicacao,
                       .f = ~ dplyr::inner_join(.x, .y, by = 'id'))) %>% 
  dplyr::select(join) %>%
  dplyr::mutate(
    pearson = purrr::map_dbl(.x = join,
                         .f = 
                           ~ stats::cor(.x$x, .x$resultado)),
    
    graficos = purrr::map2(.x = join,
                           .y = pearson,
                          .f = 
                            ~ .x %>% 
                            ggplot(aes(x, resultado)) +
                            geom_point(color = '#C44E52') +
                            theme_minimal()),
    
    add_title = purrr::map2(.x = pearson, 
                            .y = graficos,
                            .f = 
                              ~ .y +
                              labs(x = '',
                                   y = '',
                                   title = glue::glue('corr = {round(.x, 2)}')))) %>% 
  
  purrr::pluck('add_title') %>% 
  purrr::reduce(`+`)
