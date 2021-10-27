######################## Limpeza dos dados #######################
library(readxl)
library(tidyverse)
library(lubridate)
library(stringr)
library(abjutils) # remover acentos
library(janitor)
library(stringi)

# Disciplinas (historico) -------------------------------------------------

sigaa <- read_excel('SIGAA.xlsx')
sigra <- read_excel('SIGRA.xlsx')

# Diferenças SIGAA e SIGRA:
## SIGAA variável Horário em código, SIGRA variaveis `Hora início` e `Hora Fim`
## SIGAA variável Frequência (0 a 100), SIGRA variável Faltas (0 a 1)
## SIGAA não tem `Cód. Disciplina`

str(sigaa)
str(sigra)

summary(sigaa)
summary(sigra)

sigaa_limpo <- sigaa %>%
  transmute(matricula = as.integer(Matrícula),
            nome = rm_accent(tolower(Nome)),
            curso = as.factor(tolower(Curso)),
            disciplina = rm_accent(tolower(Disciplina)),
            ano = as.integer(str_sub(Semestre, 1, 4)),
            semestre = as.factor(str_sub(Semestre, -1, -1)),
            periodo = factor(paste(ano, semestre, sep = "/"), ordered = TRUE),
            professor = rm_accent(tolower(Professor)),
            turma = as.factor(Turma),
            mencao = factor(Menção, ordered = TRUE, levels = c("SR", "II", "MI", "TR", "TJ", "DP", "CC", "MM", "MS", "SS")),
            faltas = (100 - as.numeric(Frequência))/100,
            resultado = fct_collapse(mencao,
                                     "Aprovação" = c("SS", "MS", "MM", "CC", "DP"),
                                     "Reprovação" = c("MI", "II", "SR"),
                                     "Trancamento" = c("TR", "TJ")
            )
  ) %>% arrange(matricula)

head(sigaa_limpo)
str(sigaa_limpo)

sigra_limpo <- sigra %>%
  transmute(matricula = as.integer(Matrícula),
            nome = rm_accent(tolower(Nome)),
            curso = as.factor(tolower(Curso)),
            disciplina = rm_accent(tolower(Disciplina)),
            ano = as.integer(str_sub(`Semestre em que cursou a disciplina`, 1, 4)),
            semestre = as.factor(str_sub(`Semestre em que cursou a disciplina`, -1, -1)),
            periodo = factor(paste(ano, semestre, sep = "/"), ordered = TRUE),
            professor = rm_accent(tolower(Professor)),
            turma = as.factor(Turma),
            mencao = factor(Menção, ordered = TRUE, levels = c("SR", "II", "MI", "TR", "TJ", "DP", "CC", "MM", "MS", "SS")),
            faltas = as.numeric(Faltas),
            resultado = fct_collapse(mencao,
                                     "Aprovação" = c("SS", "MS", "MM", "CC", "DP"),
                                     "Reprovação" = c("MI", "II", "SR"),
                                     "Trancamento" = c("TR", "TJ")
            )
  ) %>% arrange(matricula)

head(sigra_limpo)
str(sigra_limpo)

historico_completo <- rbind(sigaa_limpo, sigra_limpo) %>% 
  arrange(matricula)

# função de https://gist.github.com/jjesusfilho/454192db8356eb9c486a02698338221a
ToTitleCasePT <- function(string) {
  string %>% 
    stringr::str_to_title() %>% 
    stringr::str_replace_all( # using `c("regex" = "replacement")` syntax
      c(
        # articles 
        "(.)\\bA(s)?\\b" = "\\1a\\2",
        "(.)\\bO(s)?\\b" = "\\1o\\2",
        "(.)\\bU((m(a(s)?)?)|ns)\\b" = "\\1u\\2",
        # oblique pronouns 
        "(.)\\bL(he(s)?)\\b" = "\\1l\\2", 
        "(.)\\bM((e(u(s)?)?)|(i(m|(nha(s)?))))\\b" = "\\1m\\2", 
        "(.)\\bN(os(s[ao](s)?)?)\\b" = "\\1n\\2", 
        "(.)\\bS((e(u(s)?)?)|(ua(s)?))\\b" = "\\1s\\2", 
        "(.)\\bT((e(u(s)?)?)|i|(ua(s)?))\\b" = "\\1t\\2", 
        "(.)\\bV(os(s[ao](s)?)?)\\b" = "\\1v\\2",
        # prepositions 
        "(.)\\bA((o)(s)?|nte|té|pós)\\b" = "\\1a\\2",
        "(.)\\bÀ(s)?\\b" = "\\1à\\2",
        "(.)\\bC(om|ontra)\\b" = "\\1c\\2",
        "(.)\\bD(((a|o)(s)?)|(e(sde)?))\\b" = "\\1d\\2",
        "(.)\\bE(m|ntre)\\b" =  "\\1e\\2",
        "(.)\\bN((a|o)(s)?)\\b" = "\\1n\\2",
        "(.)\\bP(ara|(e((l(a|o)(s)?)|rante))|or)\\b" = "\\1p\\2",
        "(.)\\bS(em|(ob(re)?))\\b" = "\\1s\\2",
        "(.)\\bT(rás)\\b" = "\\1t\\2",
        # conjunctions 
        "(.)\\bC(on(forme|quanto|tudo))\\b" = "\\1c\\2",
        "(.)\\bD(urante)\\b" = "\\1D\\2",
        "(.)\\bE((mbora|n(quanto|t(ão|retanto))|xceto)?)\\b" = "\\1e\\2",
        "(.)\\bL(ogo)\\b" = "\\1l\\2",
        "(.)\\bM(as)\\b" = "\\1m\\2",
        "(.)\\bN(em)\\b" = "\\1n\\2",
        "(.)\\bO(u|ra)\\b" = "\\1o\\2",
        "(.)\\bP(o(is|r(ém|qu(e|anto)|tanto)))\\b" = "\\1p\\2",
        "(.)\\bQ(u(an[dt]o|e))\\b" = "\\1q\\2",
        "(.)\\bS(e(não)?)\\b" = "\\1s\\2",
        "(.)\\bT(odavia)\\b" = "\\1t\\2"
      )
    )
}

completo_limpo <- historico_completo %>% 
  mutate(nome = ToTitleCasePT(nome),
         professor = ToTitleCasePT(professor),
         disciplina = as.factor(ToTitleCasePT(disciplina)),
         curso = as.factor(ToTitleCasePT(curso)))

str(completo_limpo)
summary(completo_limpo)

table(completo_limpo$disciplina) # De Series Temporais e Das Series Temporais

servico <- c("Bioestatistica", "Estatistica Aplicada", "Probabilidade e Estatistica", "Probabilidade e Estatistica 2")
matematica <- c("Calculo 1", "Calculo 2", "Calculo 3", "Calculo de Probabilidade 1", "Calculo de Probabilidade 2",
                "Calculo Numerico", "Introducao a Algebra Linear")

completo_limpo <- completo_limpo %>% 
  mutate(disciplina = str_replace(disciplina, "das Series Temporais", "de Series Temporais"),
         disciplina = fct_collapse(disciplina, "Estatística Exploratoria" = c("Estatistica Exploratoria 1", "Estatistica Exploratoria")),
         tipo = as.factor(ifelse(disciplina %in% servico, "Serviço", ifelse(disciplina %in% matematica, "Matemática","Bacharelado"))))

# Colocar acento

completo_limpo <- completo_limpo %>%
  mutate(disciplina = str_replace_all(disciplina, c("tistica" = "tística", "Analise" = "Análise", "coes" = "ções", 
                                                    "cao" = "ção", "Series" = "Séries", "encia" = "ência",
                                                    "tistico" = "tístico", "tagio" = "tágio", "Historia" = "História", 
                                                    "Met" = "Mét", "casticos" = "cásticos", "Item" = "Ítem",
                                                    "Topicos" = "Tópicos", "sao" = "são", "Exploratoria" = "Exploratória",
                                                    "Tecnica" = "Técnica", "Introdução a " = "Introdução à ",
                                                    "Laboratorio" = "Laboratório", "Calculo" = "Cálculo", "Numerico" = "Numérico", "Algebra" = "Álgebra")),
         professor = str_replace_all(professor, c("Marcio" = "Márcio", "Brandao" = "Brandão", "Andre" = "André", "Maranhao" = "Maranhão",
                                                  "Aloisio" = "Aloísio", "Claudia" = "Cláudia", "Antonio" = "Antônio", "Magalhaes" = "Magalhães",
                                                  "Barbara" = "Bárbara", "Demerson" = "Démerson", "Correa" = "Corrêa", "Junior" = "Júnior",
                                                  "Espirito" = "Espírito", "Helio" = "Hélio", "Araujo" = "Araújo", "Joao" = "João",
                                                  "Jose" = "José", "Cesar" = "César", "Lucio" = "Lúcio", "Luis" = "Luís",
                                                  "Vinicius" = "Vinícius", "Leao" = "Leão", "Marilia" = "Marília", "Natalia" = "Natália",
                                                  "Otavio" = "Otávio", "Simoes" = "Simões", "Patricia" = "Patrícia", "Rene" = "Renê",
                                                  "Rogerio" = "Rogério", "Valeria" = "Valéria", "Cancado" = "Cançado")))
  
str(completo_limpo)
table(completo_limpo$disciplina)
table(completo_limpo$professor)
table(completo_limpo$curso) # Aparentemente existe curso de computação (sem ser ciencia da computacao)
# Comunicacao (sem ser social), só com 10 obs, parece estar errado
# Cursos com apenas 1 obs: Teatro, Saúde Animal. Filtrar talvez? 

### Duplicadas

completo_limpo %>% count(nome, disciplina, ano, periodo) %>% filter(n > 1) %>% nrow # 18 mil casos
completo_limpo %>% get_dupes(nome, disciplina, ano, periodo)

completo_limpo %>% count(nome, disciplina, ano, periodo, horario) %>% filter(n > 1) %>% nrow # 14 mil
completo_limpo %>% get_dupes(nome, disciplina, ano, periodo, horario)

completo_limpo %>% count(nome, curso, disciplina, ano, periodo) %>% filter(n > 1) %>% nrow # 12 mil

completo_limpo %>% get_dupes(nome, disciplina, ano, periodo, horario)
completo_limpo %>% count(nome, curso, disciplina, ano, periodo, horario) %>% filter(n > 1) %>% nrow # 6 mil
completo_limpo %>% count(nome, curso, disciplina, professor, ano, periodo, horario) %>% filter(n > 1) %>% nrow # 2 mil
completo_limpo %>% count(nome, curso, disciplina, professor, ano, periodo, horario) %>% filter(n > 1) %>% nrow # 2 mil

# Aparentemente tem muitas observações que repetem pela quantidade de créditos (tem valores repetidos até 10 vezes)
# Substituir nome por matricula reduz os casos repetidos, provavelmente porque a pessoa trocou de curso/matricula

completo_limpo <- completo_limpo %>% distinct(nome, disciplina, ano, periodo, turma, mencao, .keep_all = TRUE)
# Removi considierando matrícula mais recente 
# Importante: tinha deixado antes a matricula como string, aí na hora de remover duplicadas, não removeu a certa "1100102410" < "9913246"

# Menções Diferentes
completo_limpo %>% arrange(desc(mencao)) %>% get_dupes(nome, matricula, disciplina, curso, ano, periodo, horario)

completo_limpo <- completo_limpo %>% 
  arrange(desc(mencao)) %>% 
  distinct(nome, matricula, disciplina, curso, ano, periodo, #horario, 
           .keep_all = TRUE)

# Horarios Diferentes
# levels(historico_limpo$horario)
# 
# historico_limpo <- historico_limpo %>% 
#   mutate(horario = fct_collapse(horario,
#                                 "08:00-09:50" = c("08:00-09:50", "08:00-10:00"),
#                                 "14:00-15:50" = c("14:00-15:50", "14:00-16:00"),
#                                 "16:00-17:50" = c("16:00-17:50", "16:00-18:00"),
#                                 "19:00-20:50" = c("19:00-20:50", "19:00-20:40"),
#                                 "20:50-22:30" = c("20:50-22:30", "20:50-22:40"),
#                                 "14:00-17:40" = c("14:00-17:40", "14:00-16:45", "14:00-17:00"))) # ultimos dois só tem 1 obs

completo_limpo %>% get_dupes(nome, disciplina, ano, periodo) # 36
completo_limpo %>% get_dupes(nome, disciplina, ano, periodo, matricula) # 0

completo_limpo <- completo_limpo %>% 
  arrange(matricula) %>% 
  distinct(nome, disciplina, ano, periodo, .keep_all = TRUE)

# Filtra matérias da Matemática
completo_est <- completo_limpo %>% filter(!(tipo == "Matemática" & curso != "Estatística") & !is.na(mencao) & periodo != "2021/1")

#saveRDS(completo_est, "completo_est.rds")
#write_Csv(completo_est, "completo_est.csv")  