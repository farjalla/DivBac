library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyr)


## nao esquecer de colocar a header=TRUE caso haja nomes das variaveis
divfun = read.table("c:/Users/Farjala/Documents/R/DivBac/DivFun.txt", header=TRUE)
divfun

## uma boa dar uma olhada geral para ver se tem algo estranho na tabela
summary(divfun)

## primeiro plot. O uso do ggplot foi sugerido pelo Andrew
# mais informacoes podem ser encontradas no site www.ggplot2.org
# primeiro entra a fonte dos dados, depois a funcao.
# no caso foi escolhida "aesthetic" *aes* que da boa liberdade de escolha dos eixos, cores erc.
# entra o $onde$ estao os dados, a variavel resposta, seguida da variavel explicatoria
# como temos varios tratamentos na variavel resposta, colocamos uma cor *colour* em funcao dos tratamentos *Carbon*
# veja que tem que aparecer o as.factor para o tratamento carbono seje reconheido como categorico
# geom_point e necessario para saber $como$ os dados serao plotados, nete caso, como pontos
# geon_line seria uma alternativa - dados plotados como linhas
# method - e uma breve estatistica - nocaso para medir a area de erro de cada tratamento
# faced_wrap(~Carbon) faz com que sejam construidos varios graficos cada um com um dos 31 tipos de carbono
ggplot(divfun, aes(y = Abs, x = Time, colour = as.factor(Scenario)))+ geom_point()+ stat_smooth(method = "loess")+facet_wrap(~Carbon)

##segundo plot
# e selhante ao anetrior mas neste caso quero mostrar o comportamento de cada carbono nos tratamentos
# logo, a cor e relacionada ao carbono e o fator de agregamento em paines aos tratamentos
# ha um detalhe no geon_point *position=jittered* - faz com que os pontos nao fiquem sobrepostos 
# veja que o metodo foi omitido pois o jittered espalha os pontos
ggplot(divfun, aes(y = Abs, x = Time, colour = as.factor(Carbon))) + geom_point(position = "jitter") + facet_wrap(~Scenario)

## avancando um pouco mais
# vc que agora estou usando a funcao %>% que faz com que o comando dado em uma linha anterior siga para linha seguinte
# parace que vc esta escrevendo em ingles (muito mais pratico)
# este comando e dado pelo pacote magrittr

# o objetivo aqui e ver qual o tempo de incubacao temos a maior variacao de consumo dos substratos entre os cenarios
# imaginamos que os tempos iniciais seriam descartaveis pois as bacterias nao teriam crescido ainda
# estou usando o pacote *dplyr* para arrumacao e visualizacao dos dados que deve ser chamado antes de prosseguir.
# o group_by agrupa a variavel resposta (linhas) em funcao de variaveis preditoras especificas (colunas)
# IMPORTANTE, o group_by faz o agrupamento na ORDEM especificada
# no caso, o que interessa e o tipo de carbono e o tempo e o tratamento, nesta ordem (fica claro o pq no *summarise* abaixo). 
# Bromelia e replicas sao replicas (o efeito do bloco $bromeliad$ pode ser avaliado no futuro)
# o *arrange* aqui foi feito apenas para facilitar a visualizacao da ordem do agrupamento
# com os grupos formados e hora de tirar a media e a varianca em cada grupo (mesmo Tratamento, do mesmo carbono, no mesmo tempo)
# estes calculos sao feitos pelo *summarize* - *mean* e *var*
# IMPORTANTE - o summarise, ao ser chamado, aumenta no nivel de agregamento do group_by, removendo a ultima variavel agregadora
# por exemplo, no nosso caso, o 1o summarise fez a media desconsiderando os tratamentos, enquanto que o segundo retirou o tempo!
# isto e feito para ganhar tempo na analise (para contornar isto, poderiamos ter group_by as replicas ou blocos tambem)
# depois sao plotados a var da Abs em funcao do tempo,  para cada tipo de carbono, usando o metodo descrito anteriormente
divfun %>%  
  group_by(Carbon,Time,Scenario) %>% 
  arrange(Carbon,Time,Scenario) %>%
  summarise(mAbs = mean(Abs)) %>%  
  summarise(va = var(mAbs)) %>%  
  ggplot(aes(y = va, x = Time)) + geom_line() + facet_wrap(~ Carbon)

## colocando todos os carbonos no mesmo grafico para facilitar a visualizacao
divfun %>%
  group_by(Carbon,Time,Scenario) %>%
  arrange(Carbon, Time, Scenario) %>%
  summarise(mAbs = mean(Abs)) %>%
  summarise(va = var(mAbs)) %>%
  ggplot(aes(y = va, x = Time,group = Carbon, colour = Carbon)) + geom_point()
# fica claro que para a grande maioria das fontes de carbono, apenas os tempos de 96 e 120 h sao importantes. 

## vamos agora trabalhar apenas com os tempos de 96 e 120
# com o tempo de 96. 
# em qual tratamento ha a maior variacao nas absorbancias (em relacao ao tempo inicial)?
# achei este calculo confuso e desnecessario por causa da grande quantidade de pontos negativos
# mas mantive para exemplificar algumas funcoes usadas

# a funcao mutate modifica o planilha, adionando colunas ou mudando as legendas. 
# no primeiro caso mudou a leganda para T0, T24, T48 etc.
# em outros adicionou uma nova linha ja com a formula para os valores desta nova linha
# ex. em mutate(Abs_change = T96 - T0)

# a funcao filter seleciona apenas parte da planilha que sera trabalhada
# exemplo, filter(Time %in% c("T0","T96")) so ficarao os dados referentes aos tempo de 0 e 96h

# a funcao spread muda a caracteristica da planilha de wide para long (a funcao gather faz o contrario).
# ex. spread(Time,Abs), os tempos T0 e T96 irao virar colunas, cada qual com suas abs em funcao dos tratamentos, carbono etc.

# o resto ja e conhecido menos o stat_summary(fun.data = "mean_cl_boot", colour = "green")
# a stat_summary calcula estatisticas em um plot. Especificamente pedimos mean e os *c*onfidence *l*imits de uma reamostragem *boot*strap
# a media com o erro calculados aparecerao em verde no plot

divfun %>%
  mutate(Time = paste0("T",Time)) %>% 
  filter(Time %in% c("T0","T96")) %>% #
  spread(Time,Abs) %>%
  mutate(Abs_change = T96 - T0) %>%
  group_by(Scenario,Block,Bromeliad) %>%
  mutate(relative_change = Abs_change / sum(Abs_change)) %>%
  ggplot(aes(x = Scenario, y = relative_change)) +
  geom_point(position = "jitter") + 
  stat_summary(fun.data = "mean_cl_boot", colour = "green") + 
  facet_wrap( ~ Carbon)

## na figura abaixo removi o calculo que dividia a abs pelo somatorio das Abs (qu estava me incomodando)
# e tambem nao plotei por cada tipo de carbono. Optei por fazaer uma media das abs de cada carbono por bromelia (logo na figura, cada ponto e uma bromelia)
# ha tambem algumas modificacoes no tamanho da figura e na cor dos pontos
divfun %>%
  mutate(Time = paste0("T",Time)) %>%
  filter(Time %in% c("T0","T96")) %>%
  spread(Time,Abs) %>%
  mutate(Abs_change = T96 - T0) %>%
  group_by(Scenario,Block,Bromeliad) %>%
  summarise(Mean_abs = mean(Abs_change)) %>%
  ggplot(aes(x = Scenario, y = Mean_abs)) +
  geom_point(size = 5, alpha = 0.8, position = position_jitter(width = .05)) +
  scale_y_continuous(limits = c(-0.5, 1.0)) + #retirei os outliers da figura e mudei a escala para otimizar o espa?o 
  theme_bw() + theme(panel.grid.major.y = element_blank(), 
                     panel.grid.minor.y = element_blank(), 
                     panel.grid.major.x = element_blank()) + # retirei as linhas de grade e peinel de fundo ci
  theme(axis.title = element_text(size = rel(1.5))) + 
  theme(axis.text = element_text(size = rel(1.0))) + #mudei o tamanho das escalas e legendas
  stat_summary(fun.data = "mean_cl_boot", colour = "blue")

# fica claro que ha uma tendencia de perda na leitura da Abs do tratamento 1 (chuva normal) para o 4  (chuva bem reduzida)

## comecando a testar alguma coisa nos dados
## vou comecar com uma lme pois tenho claramente fatores fixos (Carbon, Scenario) e Blocos (Bromeliad, Blocks)
library(nlme)
divfun %>%
  mutate(Time = paste0("T",Time)) %>%
  mutate(Bromeliad=paste0("B",Bromeliad)) %>%
  mutate(Block=paste0("K",Block)) %>%
  mutate(Scenario=paste0("S",Scenario)) %>%
  mutate(Carbon=paste0("C", Carbon)) %>%
  filter(Time %in% c("T96")) 

### da maneira como fiz acima tive dificuldade de criar uma nova tabela referente aos dados filtrados de 96h
  # neste caso, refiz o script formalizando uma nova tabela (divfun96) sobra a qual fiz a lme
  
divfun96 <- mutate(divfun, Time = paste0("T",Time), Bromeliad=paste0("B",Bromeliad), Block=paste0("K",Block), Scenario=paste0("S",Scenario), Carbon=paste0("C", Carbon))
divfun96 <- filter(divfun96, Time %in% c("T96"))

library(nlme)
z<-lme(Abs ~ Scenario, random = ~ 1 | Bromeliad/Carbon, data = divfun96)
summary(z)  # para ver os principais resultados
anova(z)  # para ver as diferen?as entres os cen?rios


