# Projeto de Probabilidade e Estatistica sobre Tratamento de Dados 
# Rafael  Iamashita Becsei 24.123.018-4

# Biblioteca de Leitura
library(readr)
# Biblioteca de Manipulação de Dados
library(dplyr)
# Biblioteca para Formatação de Dados
library(tidyr)
# Biblioteca de Criação de Dados
library(ggplot2)
# Biblioteca de Testes de Distribuição
library(dgof)

# Leitura dos dados da planilha
dados <- read_csv("/home/rafbecsei/CodeSpace/concrete_data.csv")

# Remoção de linhas vazias
dados <- drop_na(dados)

# Remoção de valores duplicados
dados <- distinct(dados)

# Target(X) e Attribute(Y)
X <- select(dados, -'concrete_compressive_strength')
Y <- dados$'concrete_compressive_strength'

#--------------------------------------------------------------------------

# a) Média, Variância, Desvio Padrão e Mediana de X e Y

MVDM_X <- data.frame(
	Media = colMeans(X)
	, Variancia = apply(X, 2, var)
	, Desvio = apply(X, 2, sd)
	, Mediana = apply(X, 2, median)
)

MVDM_Y <- data.frame(
	Media = mean(Y)
	, Variancia = var(Y)
	, Desvio = sd(Y)
	, Mediana= median(Y)
)

#--------------------------------------------------------------------------

# b) Histograma de X e Y

# cement	
ggplot(data.frame(cement = X$cement), aes(x = cement)) + 
	geom_histogram(color = "white", fill = "blue", binwidth = 5) + 
	labs(title = "Histograma de X (cement)", x = "Quantidade", y = "Frequencia")

# blast_furnace_slag
ggplot(data.frame(blast_furnace_slag = X$blast_furnace_slag), aes(x = blast_furnace_slag)) + 
	geom_histogram(color = "white", fill = "blue", binwidth = 5) + 
	labs(title = "Histograma de X (blast_furnace_slag)", x = "Quantidade", y = "Frequencia")

# fly_ash
ggplot(data.frame(fly_ash = X$fly_ash), aes(x = fly_ash)) + 
	geom_histogram(color = "white", fill = "blue", binwidth = 5) + 
	labs(title = "Histograma de X (fly_ash)", x = "Quantidade", y = "Frequencia")

# water
ggplot(data.frame(water = X$water), aes(x = water)) + 
	geom_histogram(color = "white", fill = "blue", binwidth = 5) + 
	labs(title = "Histograma de X (water)", x = "Quantidade", y = "Frequencia")

# superplasticizer
ggplot(data.frame(superplasticizer = X$superplasticizer), aes(x = superplasticizer)) + 
	geom_histogram(color = "white", fill = "blue", binwidth = 5) + 
	labs(title = "Histograma de X (superplasticizer)", x = "Quantidade", y = "Frequencia")

# fine_aggregate
ggplot(data.frame(fine_aggregate = X$fine_aggregate), aes(x = fine_aggregate)) + 
	geom_histogram(color = "white", fill = "blue", binwidth = 5) + 
	labs(title = "Histograma de X (fine_aggregate)", x = "Quantidade", y = "Frequencia")

# age
ggplot(data.frame(age = X$age), aes(x = age)) + 
	geom_histogram(color = "white", fill = "blue", binwidth = 5) + 
	labs(title = "Histograma de X (age)", x = "Quantidade", y = "Frequencia")

# Y
ggplot(data.frame(Y), aes(x = Y)) + 
	geom_histogram(color = "white", fill = "red", binwidth = 5) +  
	labs(title = "Histograma de Y (Resistencia a Compressao)", x = "Resistência", y = "Frequência")

#--------------------------------------------------------------------------

# c) Boxplot de X e Y

# cement
ggplot(data.frame(cement = X$cement), aes(y = cement)) +
	stat_boxplot(geom ="errorbar", width = 0.25) + 
	geom_boxplot(fill = "lightgreen") + 
	labs(title = "Boxplot de X (cement)", y = "Quantidade") 


# blast_furnace_slag
ggplot(data.frame(blast_furnace_slag = X$blast_furnace_slag), aes(y = blast_furnace_slag)) +
	stat_boxplot(geom ="errorbar", width = 0.25) + 
	geom_boxplot(fill = "lightgreen") + 
	labs(title = "Boxplot de X (blast_furnace_slag)", y = "Quantidade") 

# fly_ash
ggplot(data.frame(fly_ash = X$fly_ash), aes(y = fly_ash)) +
	stat_boxplot(geom ="errorbar", width = 0.25) + 
	geom_boxplot(fill = "lightgreen") + 
	labs(title = "Boxplot de X (fly_ash)", y = "Quantidade") 

# water
ggplot(data.frame(water = X$water), aes(y = water)) +
	stat_boxplot(geom ="errorbar", width = 0.25) + 
	geom_boxplot(fill = "lightgreen") + 
	labs(title = "Boxplot de X (water)", y = "Quantidade") 

# superplasticizer
ggplot(data.frame(superplasticizer = X$superplasticizer), aes(y = superplasticizer)) +
	stat_boxplot(geom ="errorbar", width = 0.25) + 
	geom_boxplot(fill = "lightgreen") + 
	labs(title = "Boxplot de X (superplasticizer)", y = "Quantidade") 

# fine_aggregate
ggplot(data.frame(fine_aggregate = X$fine_aggregate), aes(y = fine_aggregate)) +
	stat_boxplot(geom ="errorbar", width = 0.25) + 
	geom_boxplot(fill = "lightgreen") + 
	labs(title = "Boxplot de X (fine_aggregate)", y = "Quantidade") 

# age
ggplot(data.frame(age = X$age), aes(y = age)) +
	stat_boxplot(geom ="errorbar", width = 0.25) + 
	geom_boxplot(fill = "lightgreen") + 
	labs(title = "Boxplot de X (age)", y = "Quantidade") 

# Y
ggplot(data.frame(Y), aes(y = Y)) + 
	stat_boxplot(geom = "errorbar", width = 0.25) +
	geom_boxplot(fill = "red") +  
	labs(title = "Boxplot de Y (Resistencia a Compressao)", y = "Resistência")

#--------------------------------------------------------------------------

# d) Coenficiente de Correlação de X e Y

correlacao_cement <- cor(X$cement, Y)
correlacao_blast_furnace_slag <- cor(X$blast_furnace_slag, Y)
correlacao_fly_ash <- cor(X$fly_ash, Y)
correlacao_water <- cor(X$water, Y)
correlacao_superplasticizer <- cor(X$superplasticizer, Y)
correlacao_fine_aggregate <- cor(X$fine_aggregate, Y)
correlacao_age <- cor(X$age, Y)

correlacao_frame <- data.frame(
	Variaveis = c("cement", "blast_furnace_slag", "fly_ash", "water", 
	"superplasticizer", "fine_aggregate", "age")
	, Correlacao_Geral = c(correlacao_cement, correlacao_blast_furnace_slag
	, correlacao_fly_ash, correlacao_water, correlacao_superplasticizer
	, correlacao_fine_aggregate, correlacao_age))

#--------------------------------------------------------------------------

# e) Teste de Normalidade de X e Y

shapiro_cement <- shapiro.test(X$cement)
shapiro_blast_furnace_slag <- shapiro.test(X$blast_furnace_slag)
shapiro_fly_ash <- shapiro.test(X$fly_ash)
shapiro_water <- shapiro.test(X$water)
shapiro_superplasticizer <- shapiro.test(X$superplasticizer)
shapiro_fine_aggregate <- shapiro.test(X$fine_aggregate)
shapiro_age <- shapiro.test(X$age)

testeshapiro_frame <- data.frame(
	Variaveis = c("cement", "blast_furnace_slag", "fly_ash", "water"
	, "superplasticizer", "fine_aggregate", "age")
	, shapiro_frame = c(shapiro_cement$p.value, shapiro_blast_furnace_slag$p.value
	, shapiro_fly_ash$p.value, shapiro_water$p.value, shapiro_superplasticizer$p.value
	, shapiro_fine_aggregate$p.value, shapiro_age$p.value))

shapiro_Y <- shapiro.test(Y)

testeshapiroY_frame <- data.frame(Variavel = "Y", Valor = shapiro_Y$p.value)

ks_cement <- ks.test(X$cement, "pnorm", mean(X$cement), sd(X$cement), exact = TRUE)
ks_blast_furnace_slag <- ks.test(X$blast_furnace_slag, "pnorm", mean(X$blast_furnace_slag)
	, sd(X$blast_furnace_slag), exact = TRUE)
ks_fly_ash <- ks.test(X$fly_ash, "pnorm", mean(X$fly_ash), sd(X$fly_ash), exact = TRUE)
ks_water <- ks.test(X$water, "pnorm", mean(X$water), sd(X$water), exact = TRUE)
ks_superplasticizer <- ks.test(X$superplasticizer, "pnorm", mean(X$superplasticizer)
	, sd(X$superplasticizer), exact = TRUE)
ks_fine_aggregate <- ks.test(X$fine_aggregate, "pnorm", mean(X$fine_aggregate)
	, sd(X$fine_aggregate), exact = TRUE)
ks_age <- ks.test(X$age, "pnorm", mean(X$age), sd(X$age), exact = TRUE)

testeks_frame <- data.frame(
	Variaveis = c("cement", "blast_furnace_slag", "fly_ash", "water"
	, "superplasticizer", "fine_aggregate", "age")
	, ks_frame = c(ks_cement$p.value, ks_blast_furnace_slag$p.value, ks_fly_ash$p.value
       , ks_water$p.value, ks_superplasticizer$p.value, ks_fine_aggregate$p.value, ks_age$p.value))


ks_Y <- ks.test(Y, "pnorm", mean(Y), sd(Y), exact = TRUE)

testeks_Y <- data.frame(Variavel = "Y", Valor = ks_Y$p.value)

#--------------------------------------------------------------------------

# f) Gráfico de Densidade e Histograma para X e Y

# cement
ggplot(data.frame(cement = X$cement), aes(x = cement, y = after_stat(density))) +
	geom_histogram(color = "White", fill = "blue", binwidth = 5
	, boundary = 0, alpha = 0.5) +
        geom_density(color = "black", linewidth = 1) +
        labs(title = "Histograma e Densidade de X (cement)", x = "Quantidade"
        , y = "Densidade")

# blast_furnace_slag
ggplot(data.frame(blast_furnace_slag = X$blast_furnace_slag), aes(x = blast_furnace_slag
	, y = after_stat(density))) +
	geom_histogram(color = "White", fill = "green", binwidth = 5, boundary = 0, alpha = 0.5) +
        geom_density(color = "black", linewidth = 1) +
        labs(title = "Histograma e Densidade de X (blast_furnace_slag)", x = "Quantidade"
        , y = "Densidade")

# fly_ash
ggplot(data.frame(fly_ash = X$fly_ash), aes(x = fly_ash, y = after_stat(density))) +
	geom_histogram(color = "White", fill = "blue", binwidth = 5, boundary = 0, alpha = 0.5) +
        geom_density(color = "black", linewidth = 1) +
        labs(title = "Histograma e Densidade de X (fly_ash)", x = "Quantidade", y = "Densidade")

# water
ggplot(data.frame(water = X$water), aes(x = water, y = after_stat(density))) +
	geom_histogram(color = "White", fill = "green", binwidth = 5, boundary = 0, alpha = 0.5) +
        geom_density(color = "black", linewidth = 1) +
        labs(title = "Histograma e Densidade de X (water)", x = "Quantidade", y = "Densidade")

# superplasticizer
ggplot(data.frame(superplasticizer = X$superplasticizer), aes(x = superplasticizer, y = after_stat(density))) +
        geom_histogram(color = "White", fill = "blue", binwidth = 5, boundary = 0, alpha = 0.5) +
	geom_density(color = "black", linewidth = 1) +
        labs(title = "Histograma e Densidade de X (superplasticizer)", x = "Quantidade", y = "Densidade")

# fine_aggregate
ggplot(data.frame(fine_aggregate = X$fine_aggregate), aes(x = fine_aggregate, y = after_stat(density))) +
	geom_histogram(color = "White", fill = "green", binwidth = 5, boundary = 0, alpha = 0.5) +
        geom_density(color = "black", linewidth = 1) +
        labs(title = "Histograma e Densidade de X (fine_aggregate)", x = "Quantidade", y = "Densidade")

# age
ggplot(data.frame(age = X$age), aes(x = age, y = after_stat(density))) +
	geom_histogram(color = "White", fill = "blue", binwidth = 5, boundary = 0, alpha = 0.5) +
        geom_density(color = "black", linewidth = 1) +
        labs(title = "Histograma e Densidade de X (age)", x = "Quantidade", y = "Densidade")

# Y
ggplot(data.frame(Y), aes(x = Y, y = after_stat(density))) +
	geom_histogram(color = "White", fill = "red", binwidth = 5, boundary = 0, alpha = 0.5) +
        geom_density(color = "darkred", linewidth = 1) +
        labs(title = "Histograma e Densidade de Y (Resistencia a Compressao)", x = "Resistencia", y = "Densidade")

#--------------------------------------------------------------------------

# a) Média, variância, desvio padrão e mediana para x e y

#print(MVDM_X)
#print(MVDM_Y)

# d) O coeficiente de correlação de x e y

#print(correlacao_frame)

# e) Fazer o teste de normalidade para  y e x

#print(testeshapiroY_frame)
#print(testeshapiro_frame)

#print(testeks_Y)
#print(testeks_frame)

# Print X e Y

#print(X)
#print(Y)



































