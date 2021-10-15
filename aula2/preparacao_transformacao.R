# Preparação e Transformação dos dados
library(magrittr)
library(patchwork)
library(tidymodels)

iris_error <- readr::read_csv('dados/iris-with-errors.csv',
                              na = c('NaN')) # podia colocar o "?" também

## Detalhes dos dados
dplyr::glimpse(iris_error)
dim(iris_error)

## Remover duplicadas
iris_error %>% 
  dplyr::distinct()

### unique
iris_error %>% 
  unique()

### remover baseadas em uma única coluna - sim, não faz sentido, só mostrando
iris_error %>% 
  dplyr::distinct(species, .keep_all = TRUE)

### remover baseada em várias colunas
iris_error %>% 
  dplyr::distinct(species, sepal_length, petal_length, .keep_all = TRUE)

## Remover NaN
iris_error %>%
  tidyr::drop_na()

### Replace "?" por NA
iris_error %>% 
  dplyr::na_if('?') 

## Together - drop rows | drop columns
iris_error %>%
  dplyr::distinct() %>% 
  dplyr::na_if('?') %>% 
  tidyr::drop_na()
  
iris_error %>%
  dplyr::distinct() %>% 
  dplyr::na_if('?') %>% 
  dplyr::select(where(~ !any(is.na(.))))

## Substituir NA por valores
iris_error <- readr::read_csv('dados/iris-with-errors.csv',
                              na = c('NaN', '?'))

(iris <- iris_error %>%
  naniar::impute_mean_if(.predicate = is.double))

iris_error %>% 
  naniar::impute_median_if(.predicate = is.double)

## Normalização e padronização
recipe(species ~ ., data = iris)  %>%      # . == sepal_length + sepal_width + petal_length + petal_width 
  step_normalize(all_numeric_predictors()) # z-score
  
## step_normalize: média 0, desvio padrão 1 
## step_center: média 0 (apenas)
## minmax:
step_minmax <- function(x) {
  
  (x - min(x)) / (max(x) - min(x))
  
  } 
  
iris %>% 
  dplyr::mutate( dplyr::across(is.double, ~ step_minmax(.x))) 


## Binarização dos dados - zero ou um, de acordo com um limiar
step_binarizer <- function(x, threshold = 0.0) {
  
  cond <- x > threshold
  
  x[cond] <- 1
  x[!cond] <- 0
  
  x
}

iris %>% 
  dplyr::mutate(dplyr::across(is.double, ~ step_minmax(.x)),
                dplyr::across(is.double, ~ step_binarizer(.x, 0.5)))

## Dummies
iris %>% 
  dplyr::mutate(species = factor(species,
                                 levels = c('duplicada', 'setosa'),
                                 labels = c(1, 0)))

## One Hot Encoding
tibble::tibble(
  A = c('a', 'b', 'a', 'c', 'a', 'b')) %>%
  recipe() %>% 
  step_dummy(A, one_hot = TRUE) %>% 
  prep() %>% 
  bake(new_data = NULL)

## Dados correlacionados

housing <- readr::read_csv('dados/BostonHousing.csv')

housing %>% 
  cor() %>% 
  heatmap(scale = 'column', Colv = NA, Rowv = NA)

housing %>% 
  cor() %>% 
  corrplot::corrplot(method = 'square',
                    tl.col = 'black')

housing %>% 
  cor() %>% 
  heatmaply::heatmaply_cor(k_col = 2,
                           k_row = 2)

### remover dados altamente correlacionados
housing %>% 
  recipe(medv ~ .) %>% 
  step_corr(all_numeric_predictors(), threshold = .75)


## Dados desbalanceados
vehicle <- readr::read_csv('dados/Vehicle.csv')

### numero de elemento em cada classe
vehicle %>% 
  dplyr::group_by(Class) %>% 
  dplyr::summarise(n_elementos = dplyr::n()) %>% 
  ggplot(aes(Class, n_elementos)) +
  geom_bar(stat = 'identity', fill = '#57071f') +
  geom_text(aes(label = n_elementos), 
            vjust = 1.5, size = 12,
            colour = "white") +
  theme_minimal()

### Down Sample
recipe(Class ~ ., data = vehicle) %>% 
  themis::step_downsample(Class) %>% 
  prep() %>% 
  bake(new_data = NULL) %>% 
  dplyr::group_by(Class) %>% 
  dplyr::summarise(n_elementos = dplyr::n()) %>% 
  ggplot(aes(Class, n_elementos)) +
  geom_bar(stat = 'identity', fill = '#57071f') +
  geom_text(aes(label = n_elementos), 
            vjust = 1.5, size = 12,
            colour = "white") +
  theme_minimal()

### Up Sample  
recipe(Class ~ ., data = vehicle) %>% 
  themis::step_upsample(Class) %>% 
  prep() %>% 
  bake(new_data = NULL) %>% 
  dplyr::group_by(Class) %>% 
  dplyr::summarise(n_elementos = dplyr::n()) %>% 
  ggplot(aes(Class, n_elementos)) +
  geom_bar(stat = 'identity', fill = '#57071f') +
  geom_text(aes(label = n_elementos), 
            vjust = 1.5, size = 12,
            colour = "white") +
  theme_minimal()


## Outliers
iris_normal <- readr::read_csv('dados/iris.csv')
iris_normal %>% 
  ggplot(aes(species, petal_length, fill = species)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    x = 'Espécie',
    y  = 'Comprimento da pétala')

### Cuidar de outliers
### step_spatialsign(), step_BoxCox() and step_YeoJohnson()



## Exercicios de fixação -------------------------------------------------

# Leia os dados com erro novamente, faça a limpeza e remova as duas últimas colunas.
iris_error %>% 
  tidyr::drop_na() %>% 
  dplyr::distinct() %>% 
  dplyr::select(-species, -petal_width)

# insira a mediana de cada atributo onde for encontrada NaN.
iris_error %>% 
  recipe(species ~ .) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  prep() %>% 
  bake(new_data = NULL)

# com Iris, mostre a distribuição de probabilidades de cada uma das variáveis 
# após a normalização e padronização.
prep_bake <- function(receita){ 
  receita %>% 
    recipes::prep() %>% 
    recipes::bake(new_data = NULL)
  }

## original
iris_normal %>% 
  tidyr::pivot_longer(sepal_length:petal_width, 
                      names_to = 'parte', 
                      values_to = 'medida') %>% 
  ggplot(aes(medida, color = parte, fill = parte)) +
  geom_density() +
  facet_wrap(~ parte) +
  theme_minimal() +
  labs(x = "", 
       title = "Distribuição de probabilidades das variáveis",
       subtitle = "Antes da normalização ou padronização") 

## normalização linear 
iris_normal %>% 
  dplyr::mutate( dplyr::across(is.double, ~ step_minmax(.x))) %>% 
  tidyr::pivot_longer(sepal_length:petal_width, 
                      names_to = 'parte', 
                      values_to = 'medida') %>% 
  ggplot(aes(medida, color = parte, fill = parte)) +
  geom_density() +
  facet_wrap(~ parte) +
  theme_minimal() +
  labs(x = "",
       title = "Distribuição de probabilidades das variáveis",
       subtitle = "Após normalização linear") 

## normalização z-score
iris_normal %>% 
  recipe(species ~ .) %>% 
  step_normalize(all_numeric_predictors()) %>%
  prep_bake() %>% 
  tidyr::pivot_longer(sepal_length:petal_width, 
                      names_to = 'parte', 
                      values_to = 'medida') %>% 
  ggplot(aes(medida, color = parte, fill = parte)) +
  geom_density() +
  facet_wrap(~ parte) +
  theme_minimal() +
  labs(x = "",
       title = "Distribuição de probabilidades das variáveis",
       subtitle = "Após normalização com z-score")

# Monte um boxplot para cada variável dos dados da Iris.
iris_normal %>% 
  tidyr::pivot_longer(sepal_length:petal_width, 
                      names_to = 'parte', 
                      values_to = 'medida') %>% 
  ggplot(aes(medida, color = parte)) +
  geom_boxplot() +
  facet_wrap(~parte) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = '', title = 'Boxplot das medidas das variáveis')

# Encontre os outliers nos dados das bases Iris e BostonHouse.
housing %>% 
  tidyr::pivot_longer(crim:medv, 
                      names_to = 'variavel', 
                      values_to = 'valores') %>% 
  ggplot(aes(valores)) +
  geom_boxplot() +
  facet_wrap(~variavel) +
  theme_minimal()


## remove outliers
housing %>% 
  tidyr::pivot_longer(crim:medv, 
                      names_to = 'variavel', 
                      values_to = 'valores') %>% 
  dplyr::group_by(variavel) %>% 
  dplyr::summarise(out = list(boxplot.stats(valores)$out %>% as_tibble())) %>% 
  dplyr::mutate('in' = purrr::map(.x = out,
                                  .f = ~ .x %$% ))


boxplot.stats(housing$b)$out %>% as_tibble() %>% list()
