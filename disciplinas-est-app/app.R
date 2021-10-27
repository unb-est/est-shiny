# Pacotes -----------------------------------------------------------------
library(tidyverse)
library(readxl)
library(reshape2)
library(ggpubr)
library(tm)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(dashboardthemes)
library(scales)
library(shinycssloaders)
library(stringr)
library(caesar)
library(abjutils)

# Dados -------------------------------------------------------------------

#load("historico.RData")

historico <- read_csv("completo_est_cripto.csv") %>% 
  mutate(mencao = factor(mencao, ordered = TRUE, levels = c("SR", "II", "MI", "TR", "TJ", "DP", "CC", "MM", "MS", "SS")),
         resultado = factor(resultado, ordered = TRUE, levels = c("Reprovação", "Trancamento", "Aprovação")))

formandos <- read_csv("formandos.csv")

professores <- tribble(
  ~professor, ~formacao,
  "Alan Ricardo da Silva", "Doutor",
  "Ana Maria Nogales Vasconcelos", "Doutor",
  "André Luiz Fernandes Cançado", "Doutor",
  "Antônio Eduardo Gomes", "Doutor",
  "Bernardo Borba de Andrade", "Doutor",
  "Bernardo Nogueira Schlemper", "Mestre",
  "Cibele Queiroz da Silva", "Doutor",
  "Cira Etheowalda Guevara Otiniano", "Doutor",
  "Claudete Ruas", "Mestre",
  "Démerson André Polli", "Doutor",
  "Donald Matthew Pianto", "Doutor",
  "Eduardo Freitas da Silva", "Doutor",
  "Eduardo Monteiro de Castro Gomes", "Doutor",
  "Eduardo Yoshio Nakano", "Doutor",
  "George Freitas Von Borries", "Doutor",
  "Geraldo da Silva e Souza", "Doutor",
  "Gladston Luiz da Silva", "Doutor",
  "Guilherme Souza Rodrigues", "Doutor",
  "Gustavo Leonel Gilardoni Avalle", "Doutor", 
  "Helton Saulo Bezerra dos Santos", "Doutor",
  "Israel de Freitas Madureira", "Mestre",
  "Jhames Matos Sampaio", "Doutor",
  "Joanlise Marco de Leon Andrade", "Doutor",
  "José Angelo Belloni", "Doutor",
  "José Augusto Fiorucci", "Doutor",
  "Juliana Betini Fachini Gomes", "Doutor",
  "Leandro Tavares Correia", "Doutor",
  "Lucas Moreira", "Doutor",
  "Luís Gustavo do Amaral Vinha", "Doutor",
  "Maria Teresa Leão Costa", "Mestre",
  "Peter Zornig", "Doutor",
  "Raul Yukihiro Matsushita", "Doutor",
  "Roberto Vila Gabriel", "Doutor",
  "Thais Carvalho Valadares Rodrigues", "Doutor"
)

professores_ativos <- caesar(rm_accent(str_to_title(professores$professor)), shift=6)

bacharelado <- historico %>% filter(tipo == "Bacharelado")
servico <- historico %>% filter(tipo == "Serviço")
matematica <- historico %>% filter(tipo == "Matemática")

periodo <- levels(historico$periodo)

conditional <- function(condition, success) {
  if (condition) success else TRUE
}

# UI - Barra Lateral ------------------------------------------------------

sidebar <- dashboardSidebar(
    sidebarMenu(id="est",
        menuItem("Estatística", tabName = "geral", icon = icon("university")),
        conditionalPanel(
          condition = "input.est == 'geral'",
          selectInput("period", "Período:", 
                      choices = c(as.character(sort(unique(historico$periodo))), "Todos"),
                      selected = "Todos")
        ),
        menuItem("Disciplinas do Bacharelado", tabName = "bacharelado", icon = icon("chart-bar")),
        conditionalPanel(
          condition = "input.est == 'bacharelado'",
          switchInput(inputId = "porcent_bach", value = T, 
                      label = "% | N", offLabel = "N", onLabel = "%", labelWidth = 40)
        ),
        menuItem("Disciplinas de Serviço", tabName = "serviço", icon = icon("handshake")),
        conditionalPanel(
          condition = "input.est == 'serviço'",
          switchInput(inputId = "porcent_serv", value = T, 
                      label = "% | N", offLabel = "N", onLabel = "%", labelWidth = 40)
        ),
        menuItem("Disciplinas da Matemática", tabName = "matematica", icon = icon("divide")),
        conditionalPanel(
          condition = "input.est == 'matematica'",
          switchInput(inputId = "porcent_mat", value = T, 
                      label = "% | N", offLabel = "N", onLabel = "%", labelWidth = 40)
        ),
        menuItem("Professores", tabName = "prof", icon = icon("address-card")),
        conditionalPanel(
          condition = "input.est == 'prof'",
          switchInput(inputId = "porcent_prof", value = T, 
                      label = "% | N", offLabel = "N", onLabel = "%", labelWidth = 40)
        ),
        br(),
        div(img(src='unb_logo.png', height = '80px'), style="text-align: center;")
    )
    
)

header <- dashboardHeader(title = "Disciplinas da EST")

body <- dashboardBody(

  ### Tema
  # shinyDashboardThemes(
  #   theme = "blue_gradient"
  # ),
# UI - Página 1 Geral -----------------------------------------------------

tabItems(
  tabItem(tabName = "geral",
          fluidRow(
            column(width = 3,
                   infoBoxOutput("alunos", width = 12),
                   infoBoxOutput("disciplinas", width = 12),
                   infoBoxOutput("professores_ativ", width = 12),
                   infoBoxOutput("aprovacoes_geral", width = 12),
                   infoBoxOutput("reprovacoes_geral", width = 12)                     
            ),
            box(width = 3,
                title = "Formação dos Professores da Est",
                plotlyOutput("professores_doutores")),
            box(width = 3, 
                title = "Tipo de Disciplina",
                plotlyOutput("disciplinas_tipo"))),
          fluidRow(
            box(width = 9,
                title = "Quantidade de Formandos",
                plotlyOutput("formandos_prop")  
            )
          )            
  ),
        
# UI - Página 2 Bacharelado -----------------------------------------------

        tabItem(tabName = "bacharelado",
                fluidRow(
                  column(width = 3,
                    box(title="Parâmetros", status = "primary", solidHeader = T,width = NULL,
                        selectInput('bach_disc', 'Filtre pela(s) disciplina(s):', 
                                    choices = sort(unique(bacharelado$disciplina)),
                                    selected = "None",
                                    multiple = TRUE
                        ),
                        sliderTextInput("bach_periodo", "Filtre pelo(s) período(s):", 
                                        choices = sort(unique(bacharelado$periodo)),
                                        selected = c(as.character(min(bacharelado$periodo)), as.character(max(bacharelado$periodo)))
                        ),
                        selectInput('bach_professor', 'Filtre pelo(s) professor(es):', 
                                    choices = sort(professores_ativos),
                                    selected = "None",
                                    multiple = TRUE
                        ),
                        # selectInput("bach_horario", "Filtre pelo(s) horário(s):", 
                        #             choices = sort(unique(bacharelado$horario)),
                        #             multiple = TRUE
                        # ),
                        selectInput("bach_turma", "Filtre pela(s) turma(s):", 
                                    choices = sort(as.character(unique(bacharelado$turma))),
                                    multiple = TRUE
                        ),
                        checkboxInput("bach_sr", "Incluir SR", 
                                      value = TRUE
                        ),
                        checkboxInput("bach_tr", "Incluir trancamentos", 
                                      value = TRUE
                        ),
                        actionBttn(
                          inputId = "bach_limpar",
                          label = "Limpar",
                          style = "stretch", 
                          color = "primary"
                        )
                    ),
                    
                    infoBoxOutput("bach_aprovacoes", width = 12),
                    infoBoxOutput("bach_reprovacoes", width = 12)
                    ),
                  tabBox(
                    title = "Disciplinas",
                    id = "tabset0", width = 9,
                    tabPanel("Menções", withSpinner(plotlyOutput("bach_mencoes", height = "800px"))),
                    tabPanel("Reprovações", withSpinner(plotlyOutput("bach_aprov", height = "800px")))
                    #withSpinner(plotlyOutput("mapa", height = "600px")), hide.ui = FALSE)
                  ),
                  
                  tabBox(
                    title = "Histórico",
                    id = "tabset1", width = 12,
                    tabPanel("Menções", withSpinner(plotlyOutput("bach_mencoes_hist"))),
                    tabPanel("Reprovações", withSpinner(plotlyOutput("bach_aprov_hist")))
                  )
                )
        ),

# UI - Página 3 Serviço ---------------------------------------------------

tabItem(tabName = "serviço",
        fluidRow(
          column(width = 3,
                 box(title="Parâmetros",status = "primary",solidHeader = T,width = NULL,
                     selectInput('serv_disc', 'Filtre pela(s) disciplina(s):', 
                                 choices = sort(unique(servico$disciplina)),
                                 selected = "None",
                                 multiple = TRUE
                     ),
                     sliderTextInput("serv_periodo", "Filtre pelo(s) período(s):", 
                                     choices = sort(unique(servico$periodo)),
                                     selected = c(as.character(min(servico$periodo)), as.character(max(servico$periodo)))
                     ),
                     selectInput('curso', 'Filtre pelo(s) curso(s):', 
                                 choices = sort(unique(servico$curso)),
                                 selected = "None", 
                                 multiple = TRUE
                     ),
                     selectInput('serv_professor', 'Filtre pelo(s) professor(es):', 
                                 choices = sort(professores_ativos),
                                 selected = "None",
                                 multiple = TRUE
                     ),
                     # selectInput("serv_horario", "Filtre pelo(s) horário(s):", 
                     #             choices = sort(unique(servico$horario)),
                     #             multiple = TRUE
                     # ),
                     selectInput("serv_turma", "Filtre pela(s) turma(s):", 
                                 choices = sort(as.character(unique(servico$turma))),
                                 multiple = TRUE
                     ),
                     checkboxInput("serv_sr", "Incluir SR", 
                                   value = TRUE
                     ),
                     checkboxInput("serv_tr", "Incluir trancamentos", 
                                   value = TRUE
                     ),
                     actionBttn(
                       inputId = "serv_limpar",
                       label = "Limpar",
                       style = "stretch", 
                       color = "primary"
                     ),
                 ),
                 infoBoxOutput("serv_aprovacoes", width = 12),
                 infoBoxOutput("serv_reprovacoes", width = 12)
          ),
          tabBox(
            title = "Disciplinas",
            id = "tabset2", width = 9,
            tabPanel("Menções", withSpinner(plotlyOutput("serv_mencoes", height = "665px"))),
            tabPanel("Reprovações", withSpinner(plotlyOutput("serv_aprov", height = "665px")))
          ),
          tabBox(
            title = "Histórico",
            id = "tabset3", width = 12,
            tabPanel("Menções", withSpinner(plotlyOutput("serv_mencoes_hist"))),
            tabPanel("Reprovações", withSpinner(plotlyOutput("serv_aprov_hist")))
            )
          )             
        ),


# UI - Página 4 Matemática -----------------------------------------------

tabItem(tabName = "matematica",
        fluidRow(
          column(width = 3,
                 box(title="Parâmetros", status = "primary", solidHeader = T,width = NULL,
                     selectInput('mat_disc', 'Filtre pela(s) disciplina(s):', 
                                 choices = sort(unique(matematica$disciplina)),
                                 selected = "None",
                                 multiple = TRUE
                     ),
                     sliderTextInput("mat_periodo", "Filtre pelo(s) período(s):", 
                                     choices = sort(unique(matematica$periodo)),
                                     selected = c(as.character(min(matematica$periodo)), as.character(max(matematica$periodo)))
                     ),
                     selectInput('mat_professor', 'Filtre pelo(s) professor(es):', 
                                 choices = sort(professores_ativos),
                                 selected = "None",
                                 multiple = TRUE
                     ),
                     # selectInput("mat_horario", "Filtre pelo(s) horário(s):", 
                     #             choices = sort(unique(matematica$horario)),
                     #             multiple = TRUE
                     # ),
                     selectInput("mat_turma", "Filtre pela(s) turma(s):", 
                                 choices = sort(as.character(unique(matematica$turma))),
                                 multiple = TRUE
                     ),
                     checkboxInput("mat_sr", "Incluir SR", 
                                   value = TRUE
                     ),
                     checkboxInput("mat_tr", "Incluir trancamentos", 
                                   value = TRUE
                     ),
                     actionBttn(
                       inputId = "mat_limpar",
                       label = "Limpar",
                       style = "stretch", 
                       color = "primary"
                     )
                 ),
                 
                 infoBoxOutput("mat_aprovacoes", width = 12),
                 infoBoxOutput("mat_reprovacoes", width = 12)
          ),
          tabBox(
            title = "Disciplinas",
            id = "tabset0", width = 9,
            tabPanel("Menções", withSpinner(plotlyOutput("mat_mencoes", height = "800px"))),
            tabPanel("Reprovações", withSpinner(plotlyOutput("mat_aprov", height = "800px")))
            #withSpinner(plotlyOutput("mapa", height = "600px")), hide.ui = FALSE)
          ),
          
          tabBox(
            title = "Histórico",
            id = "tabset1", width = 12,
            tabPanel("Menções", withSpinner(plotlyOutput("mat_mencoes_hist"))),
            tabPanel("Reprovações", withSpinner(plotlyOutput("mat_aprov_hist")))
          )
        )
),

# UI - Página 5 Professores -----------------------------------------------

tabItem(tabName = "prof",
        fluidRow(
          column(width = 3,
                 box(title="Parâmetros",status = "primary",solidHeader = T,width = NULL,
                     selectInput('prof_disc', 'Filtre pela(s) disciplina(s):', 
                                 choices = sort(unique(historico$disciplina)),
                                 selected = "None",
                                 multiple = TRUE
                     ),
                     sliderTextInput("prof_periodo", "Filtre pelo(s) período(s):", 
                                     choices = sort(unique(historico$periodo)),
                                     selected = c(as.character(min(historico$periodo)), as.character(max(historico$periodo)))
                     ),
                     selectInput('prof_professor', 'Filtre pelo(s) professor(es):', 
                                 choices = sort(professores_ativos),
                                 selected = "None",
                                 multiple = TRUE
                     ),
                     # selectInput("prof_horario", "Filtre pelo(s) horário(s):", 
                     #             choices = sort(unique(historico$horario)),
                     #             multiple = TRUE
                     # ),
                     selectInput("prof_turma", "Filtre pela(s) turma(s):", 
                                 choices = sort(as.character(unique(historico$turma))),
                                 multiple = TRUE
                     ),
                     checkboxInput("prof_ativos", "Filtrar professores ativos", 
                                 value = TRUE
                     ),
                     checkboxInput("prof_sr", "Incluir SR", 
                                   value = TRUE
                     ),
                     checkboxInput("prof_tr", "Incluir trancamentos", 
                                   value = TRUE
                     ),
                     actionBttn(
                       inputId = "prof_limpar",
                       label = "Limpar",
                       style = "stretch", 
                       color = "primary"
                     )
                 ),
                 infoBoxOutput("prof_alunos_por_professor_geral", width = 12)
          ),
          tabBox(
            title = "Professores",
            id = "tabset4", width = 9,
            tabPanel("Menções",
                     withSpinner(plotlyOutput("prof_mencoes", height = "1000px"))),
            tabPanel("Reprovações",
                     withSpinner(plotlyOutput("prof_aprov", height = "1000px"))),
            tabPanel("Alunos por Professor",
                     withSpinner(plotlyOutput("prof_alunos_por_prof", height = "1000px")))
            ),
          tabBox(
            title = "Histórico",
            id = "prof_hist", width = 12,
            tabPanel("Alunos por Professor", withSpinner(plotlyOutput("prof_alunos_por_professor_hist")))
            )
          )
        )
    )
)

ui <- dashboardPage(sidebar = sidebar, header = header, body = body)

server <- function(input, output, session) {

# Server - Página 1 Geral -------------------------------------------------

  opcao_hist<- reactive({
    if(input$period=="Todos"){
      opcao_hist<- historico
    } else{
      opcao_hist<- historico %>% filter(periodo==input$period)
    }
    
    opcao_hist
    
  })
  
  output$alunos <- renderInfoBox({
    infoBox(
      "Alunos matriculados", nrow(opcao_hist() %>% filter(curso=="Estatística") %>%
                                    summarise(unique(nome, ))) ,
      color = "purple", fill = TRUE,icon=icon("user-friends")
    )
  })
  
  output$disciplinas <- renderInfoBox({
    infoBox(
      "Disciplinas ofertadas", nrow(unique(opcao_hist() %>% select(disciplina,tipo))),
      color = "navy", fill = TRUE, icon=icon("book-reader")
    )
  })
  
  prof_ativ<-reactive({
    prof_ativ<-opcao_hist() %>% summarise(professor=unique(professor))
    prof_ativ
  })
  
  output$professores_ativ <- renderInfoBox({
    infoBox(
      "Professores", nrow(prof_ativ()) ,
      color = "light-blue", fill = TRUE, icon=icon("chalkboard-teacher")
    )
  })
  
  taxa_aprovacao <- reactive({
    (opcao_hist() %>% 
       count(resultado) %>% 
       summarise(prop = n/sum(n), resultado = resultado) %>% 
       filter(resultado == "Aprovação"))$prop      
  })
  
  taxa_reprovacao <- reactive({
    (opcao_hist() %>%
       count(resultado) %>% 
       summarise(prop = n/sum(n), resultado = resultado) %>% 
       filter(resultado == "Reprovação"))$prop      
  })  
  
  
  output$aprovacoes_geral <- renderInfoBox({
    infoBox(
      "Aprovação média", label_percent(accuracy = 0.1, decimal.mark = ",")(taxa_aprovacao()) ,
      color = "aqua", fill = TRUE,icon=icon("check")
    )
  })
  
  output$reprovacoes_geral <- renderInfoBox({
    infoBox(
      "Reprovação média", label_percent(accuracy = 0.1, decimal.mark = ",")(taxa_reprovacao()) ,
      color = "red", fill = TRUE,icon=icon("times")
    )
  })
  
  #TODO: simplificar calculos dos professores doutores e gráfico
  # prof_dout <- reactive({
  #   
  #   if(input$period == "Todos"){
  #     Sem_ano_dout<-as.numeric(str_sub(factor(periodo[length(periodo)]), end = 4))
  #   } else{
  #     Sem_ano_dout<-as.numeric(str_sub(factor(input$period), end = 4))
  #   }
  #   
  #   prof_dout<-merge(prof_ativ(),
  #                    professores_dout,by.x="professor",by.y="Professores",all.x=T) %>%
  #     mutate(Ano_dout=if_else(Ano_dout<=Sem_ano_dout,1,Ano_dout),
  #            Ano_dout=if_else(Ano_dout>Sem_ano_dout,0,Ano_dout),
  #            Ano_dout=replace_na(Ano_dout, 0),
  #            Ano_dout=as.character(Ano_dout),
  #            Ano_dout=replace(Ano_dout, Ano_dout=="1", "Doutor"),
  #            Ano_dout=replace(Ano_dout, Ano_dout=="0", "Mestre")) %>% count(Ano_dout)
  #   
  #   prof_dout
  # })
  
  # output$professores_doutores<-renderPlotly({
  #   prof_dout()%>%
  #     plot_ly(labels = ~factor(Ano_dout), values = ~n,
  #             sort = FALSE, marker = list(colors = c("#D0F5FF",
  #                                                    "#133e79")))%>%
  #     add_pie(hole = 0.6) %>% layout(showlegend = T,
  #                                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
  #                                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  #   
  # })

  
  output$disciplinas_tipo <- renderPlotly({
    unique(opcao_hist() %>% select(disciplina,tipo)) %>% count(tipo) %>%
      plot_ly(labels = ~factor(tipo), values = ~n,
              sort = FALSE, marker = list(colors = c("#008940","#20B2AA", "#C5F1E9")))%>%
      add_pie(hole = 0.6) %>% layout(showlegend = T, 
                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })  
    
  output$professores_doutores <- renderPlotly({
    professores %>% count(formacao) %>% 
      plot_ly(labels = ~factor(formacao), values = ~n,
              sort = FALSE, marker = list(colors = c("#133e79", "#D0F5FF"))) %>%
      add_pie(hole = 0.6) %>% layout(showlegend = T,
                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  #TODO: Usar dados confiáveis de formandos (e precisa mesmo desse gráfico na 1a pagina?)
  
  # prop_formandos<- reactive({
  #   
  #   if(input$period=="Todos"){
  #     ano_form <- tail(periodo,1)
  #   }else{
  #     ano_form <- input$period
  #   }
  #   
  #   prop_formandos <- formandos %>% 
  #     group_by(periodo) %>% summarise(n = n()) %>% 
  #     summarise(n=n, periodo=periodo) %>%
  #     filter(periodo <= ano_form)
  #   prop_formandos
  # })
  
  output$formandos_prop <-renderPlotly({
    p <- formandos %>%
      ggplot(aes(x = Semestre, y = Quantitativo, group = 1, text = paste0('Período: ', Semestre, '\n', 'Quantidade: ', Quantitativo))) + 
      geom_line(color="#133e79") + 
      labs(x="Período", y="Quantidade") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
  })


# Server - Página 2 Bacharelado -------------------------------------------

    bach_filtrado <- reactive({
      bacharelado %>%
        filter(
          conditional(!is.null(input$bach_disc), disciplina %in% input$bach_disc),
          conditional(TRUE, periodo >= input$bach_periodo[1] & periodo <= input$bach_periodo[2]),
          conditional(!is.null(input$bach_professor), professor %in% input$bach_professor),
          # conditional(!is.null(input$bach_horario), horario %in% input$bach_horario),
          conditional(!is.null(input$bach_turma), turma %in% input$bach_turma)
        )
    })
    
    bach_taxa_aprovacao <- reactive({
      (bach_filtrado() %>% 
        count(resultado) %>% 
        summarise(prop = n/sum(n), resultado = resultado) %>% 
        filter(resultado == "Aprovação"))$prop
    })
    
    bach_taxa_reprovacao <- reactive({
      (bach_filtrado() %>% 
         count(resultado) %>% 
         summarise(prop = n/sum(n), resultado = resultado) %>% 
         filter(resultado == "Reprovação"))$prop
    })
    
    output$bach_aprovacoes <- renderInfoBox({
        infoBox(
            "Aprovação",  label_percent(accuracy = 0.1, decimal.mark = ",")(bach_taxa_aprovacao()),
            color = "aqua", fill = TRUE,icon=icon("check")
        )
    })
    
    output$bach_reprovacoes <- renderInfoBox({
        infoBox(
            "Reprovação", label_percent(accuracy = 0.1, decimal.mark = ",")(bach_taxa_reprovacao()),
            color = "red", fill = TRUE,icon=icon("times")
        )
    })
    
    bach_disciplinas <- reactive(filter(bacharelado, disciplina %in% input$bach_disc))
    
    observe({
      if (!is.null(input$bach_disc)){
      updateSelectInput(session, "bach_professor", choices = sort(unique(bach_disciplinas()$professor)))
      } else {
      updateSelectInput(session, "bach_professor", choices = sort(professores_ativos))  
      }
    })
    
    # bach_professores <- reactive(filter(bacharelado, professor %in% input$bach_professor))
    # 
    # observe({
    #   if (!is.null(input$bach_professor)){
    #     updateSelectInput(session, "bach_disc", choices = sort(unique(bach_professores()$disciplina)))
    #   } else {
    #     updateSelectInput(session, "bach_disc", choices = sort(unique(bacharelado$disciplina)))  
    #   }
    # }) Erro que limpa as disciplinas depois que seleciona o professor
    
    observeEvent(input$bach_limpar, {
      updateSelectInput(session, "bach_disc", selected = "None")
      updateSliderTextInput(session, "bach_periodo", selected = c(as.character(min(bacharelado$periodo)), as.character(max(bacharelado$periodo))))
      updateSelectInput(session, "bach_professor", selected = "None")
      # updateSelectInput(session, "bach_horario", selected = "None")
      updateSelectInput(session, "bach_turma", selected = "None")
    })
    
    output$bach_mencoes <- renderPlotly({
      
      if(input$porcent_bach == TRUE){
        
        if(input$bach_sr == TRUE & input$bach_tr == TRUE){
          
          bach_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP")) %>%
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR")),
                   disciplina = fct_reorder(disciplina, mencao, function(.x) mean(.x %in% c("SR", "II", "MI", "TR")))) %>% 
            count(disciplina, mencao) %>% 
            group_by(disciplina) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~disciplina, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', mencao,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$bach_sr == FALSE & input$bach_tr == TRUE){
          
          bach_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "SR")) %>%
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR")),
                   disciplina = fct_reorder(disciplina, mencao, function(.x) mean(.x %in% c("II", "MI", "TR")))) %>% 
            count(disciplina, mencao) %>% 
            group_by(disciplina) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~disciplina, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', mencao,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$bach_sr == TRUE & input$bach_tr == FALSE){
          
          bach_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>%
            mutate(disciplina = fct_reorder(disciplina, mencao, function(.x) mean(.x %in% c("SR", "II", "MI")))) %>% 
            count(disciplina, mencao) %>% 
            group_by(disciplina) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~disciplina, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', mencao,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))   
          
        } else{
          
          bach_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "SR", "TJ", "TR")) %>%
            mutate(disciplina = fct_reorder(disciplina, mencao, function(.x) mean(.x %in% c("II", "MI")))) %>% 
            count(disciplina, mencao) %>% 
            group_by(disciplina) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~disciplina, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', mencao,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))          
          }
        
      } else{
       
        if(input$bach_sr == TRUE & input$bach_tr == TRUE){
          
          bach_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP")) %>%
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR")),
                   disciplina = fct_reorder(disciplina, mencao, function(.x) sum(.x %in% c("SR", "II", "MI", "TR")))) %>% 
            count(disciplina, mencao) %>% 
            plot_ly(type = "bar", x = ~n, y = ~disciplina, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', mencao,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
        
        } else if(input$bach_sr == FALSE & input$bach_tr == TRUE){
        
          bach_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "SR")) %>%
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR")),
                   disciplina = fct_reorder(disciplina, mencao, function(.x) sum(.x %in% c("II", "MI", "TR")))) %>% 
            count(disciplina, mencao) %>% 
            plot_ly(type = "bar", x = ~n, y = ~disciplina, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', mencao,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$bach_sr == TRUE & input$bach_tr == FALSE){
          
          bach_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>%
            mutate(disciplina = fct_reorder(disciplina, mencao, function(.x) sum(.x %in% c("SR", "II", "MI")))) %>% 
            count(disciplina, mencao) %>% 
            plot_ly(type = "bar", x = ~n, y = ~disciplina, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', mencao,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))  
          
        } else{
          
          bach_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "TJ", "TR", "SR")) %>%
            mutate(disciplina = fct_reorder(disciplina, mencao, function(.x) sum(.x %in% c("II", "MI")))) %>% 
            count(disciplina, mencao) %>% 
            plot_ly(type = "bar", x = ~n, y = ~disciplina, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', mencao,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))  
          
        }
          
      }
    })
    
    output$bach_aprov <- renderPlotly({
      
      if(input$porcent_bach == TRUE){
        
        if(input$bach_sr == TRUE & input$bach_tr == TRUE){
          
          bach_filtrado() %>% 
            mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) mean(.x %in% c("Reprovação", "Trancamento")))) %>% 
            count(disciplina, resultado) %>% 
            group_by(disciplina) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~disciplina, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', resultado,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$bach_sr == FALSE & input$bach_tr == TRUE){  
        
          bach_filtrado() %>% 
            filter(!mencao %in% c("SR")) %>% 
            mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) mean(.x %in% c("Reprovação", "Trancamento")))) %>% 
            count(disciplina, resultado) %>% 
            group_by(disciplina) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~disciplina, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', resultado,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$bach_sr == TRUE & input$bach_tr == FALSE){
        
          bach_filtrado() %>% 
            filter(!mencao %in% c("TJ", "TR")) %>% 
            mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) mean(.x %in% c("Reprovação")))) %>% 
            count(disciplina, resultado) %>% 
            group_by(disciplina) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~disciplina, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', resultado,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else{
          
          bach_filtrado() %>% 
            filter(!mencao %in% c("TJ", "TR", "SR")) %>% 
            mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) mean(.x %in% c("Reprovação")))) %>% 
            count(disciplina, resultado) %>% 
            group_by(disciplina) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~disciplina, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', resultado,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
        }
        
      } else{
        
        if(input$bach_sr == TRUE & input$bach_tr == TRUE){
          
          bach_filtrado() %>% 
            mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) sum(.x %in% c("Reprovação", "Trancamento")))) %>% 
            count(disciplina, resultado) %>% 
            plot_ly(type = "bar", x = ~n, y = ~disciplina, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', resultado,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$bach_sr == FALSE & input$bach_tr == TRUE){  
          
          bach_filtrado() %>% 
            filter(!mencao %in% c("SR")) %>% 
            mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) sum(.x %in% c("Reprovação", "Trancamento")))) %>% 
            count(disciplina, resultado) %>% 
            plot_ly(type = "bar", x = ~n, y = ~disciplina, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', resultado,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$bach_sr == TRUE & input$bach_tr == FALSE){
          
          bach_filtrado() %>% 
            filter(!mencao %in% c("TJ", "TR")) %>% 
            mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) sum(.x %in% c("Reprovação")))) %>% 
            count(disciplina, resultado) %>% 
            plot_ly(type = "bar", x = ~n, y = ~disciplina, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', resultado,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else{
          
          bach_filtrado() %>% 
            filter(!mencao %in% c("TJ", "TR", "SR")) %>% 
            mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) sum(.x %in% c("Reprovação")))) %>% 
            count(disciplina, resultado) %>% 
            plot_ly(type = "bar", x = ~n, y = ~disciplina, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', resultado,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
        }
        
      }
    })
    
    output$bach_mencoes_hist <-renderPlotly({
      
      if(input$porcent_bach == TRUE){
        
        if(input$bach_sr == TRUE & input$bach_tr == TRUE){
          
          p <- bach_filtrado() %>% 
            filter(mencao %in% c("SR", "II", "MI", "TJ", "TR", "MM", "MS", "SS")) %>% 
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR"))) %>% 
            droplevels() %>% 
            count(periodo, mencao) %>% 
            group_by(periodo) %>% mutate(prop = n/sum(n)) %>% 
            complete(mencao, fill = list(n = 0, prop = 0)) %>% 
            ggplot(aes(x = periodo, y = prop, group = mencao, color = mencao)) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nMenção: ', mencao,
                                        '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Porcentagem de Alunos") +
            scale_colour_manual(name="", values = c("#b2172b", "#ef8a62", "#fddbc7", "grey70", "#d1e5f0", "#66a9cf", "#2266ac")) + 
            scale_y_continuous(labels = scales::label_percent()) +
            theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
      
        } else if(input$bach_sr == FALSE & input$bach_tr == TRUE){  
  
          p <- bach_filtrado() %>% 
            filter(mencao %in% c("II", "MI", "TJ", "TR", "MM", "MS", "SS")) %>% 
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR"))) %>% 
            droplevels() %>% 
            count(periodo, mencao) %>% 
            group_by(periodo) %>% mutate(prop = n/sum(n)) %>% 
            complete(mencao, fill = list(n = 0, prop = 0)) %>% 
            ggplot(aes(x = periodo, y = prop, group = mencao, color = mencao)) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nMenção: ', mencao,
                                        '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Porcentagem de Alunos") +
            scale_colour_manual(name="", values = c("#ef8a62", "#fddbc7", "grey70", "#d1e5f0", "#66a9cf", "#2266ac")) + 
            scale_y_continuous(labels = scales::label_percent()) +
            theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
          
        } else if(input$bach_sr == TRUE & input$bach_tr == FALSE){

          p <- bach_filtrado() %>% 
            filter(mencao %in% c("SR", "II", "MI", "MM", "MS", "SS")) %>% 
            droplevels() %>% 
            count(periodo, mencao) %>% 
            group_by(periodo) %>% mutate(prop = n/sum(n)) %>% 
            complete(mencao, fill = list(n = 0, prop = 0)) %>% 
            ggplot(aes(x = periodo, y = prop, group = mencao, color = mencao)) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nMenção: ', mencao,
                                        '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Porcentagem de Alunos") +
            scale_colour_manual(name="", values = c("#b2172b", "#ef8a62", "#fddbc7", "#d1e5f0", "#66a9cf", "#2266ac")) + 
            scale_y_continuous(labels = scales::label_percent()) +
            theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
          
        } else{
          
          p <- bach_filtrado() %>% 
            filter(mencao %in% c("II", "MI", "MM", "MS", "SS")) %>% 
            droplevels() %>% 
            count(periodo, mencao) %>% 
            group_by(periodo) %>% mutate(prop = n/sum(n)) %>% 
            complete(mencao, fill = list(n = 0, prop = 0)) %>% 
            ggplot(aes(x = periodo, y = prop, group = mencao, color = mencao)) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nMenção: ', mencao,
                                        '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Porcentagem de Alunos") +
            scale_colour_manual(name="", values = c("#ef8a62", "#fddbc7", "#d1e5f0", "#66a9cf", "#2266ac")) + 
            scale_y_continuous(labels = scales::label_percent()) +
            theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
        }
          
      } else{
        
        if(input$bach_sr == TRUE & input$bach_tr == TRUE){
          
          p <- bach_filtrado() %>% 
            filter(mencao %in% c("SR", "II", "MI", "TJ", "TR", "MM", "MS", "SS")) %>% 
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR"))) %>% 
            droplevels() %>% 
            count(periodo, mencao) %>% 
            complete(mencao, fill = list(n = 0, prop = 0)) %>% 
            ggplot(aes(x = periodo, y = n, group = mencao, color = mencao)) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nMenção: ', mencao,
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Quantidade de Alunos") +
            scale_colour_manual(name="", values = c("#b2172b", "#ef8a62", "#fddbc7", "grey70", "#d1e5f0", "#66a9cf", "#2266ac")) + 
            theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))        
        
      } else if(input$bach_sr == FALSE & input$bach_tr == TRUE){  
        
        p <- bach_filtrado() %>% 
          filter(mencao %in% c("II", "MI", "TJ", "TR", "MM", "MS", "SS")) %>% 
          mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR"))) %>% 
          droplevels() %>% 
          count(periodo, mencao) %>% 
          complete(mencao, fill = list(n = 0, prop = 0)) %>% 
          ggplot(aes(x = periodo, y = n, group = mencao, color = mencao)) + 
          geom_line(aes(text = paste0('Período: ', periodo, 
                                      '\nMenção: ', mencao,
                                      '\nQuantidade: ', n))) + 
          labs(x="Período", y="Quantidade de Alunos") +
          scale_colour_manual(name="", values = c("#ef8a62", "#fddbc7", "grey70", "#d1e5f0", "#66a9cf", "#2266ac")) + 
          theme_minimal() + 
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))        

    } else if(input$bach_sr == TRUE & input$bach_tr == FALSE){

      p <- bach_filtrado() %>% 
        filter(mencao %in% c("SR", "II", "MI", "MM", "MS", "SS")) %>% 
        droplevels() %>% 
        count(periodo, mencao) %>% 
        complete(mencao, fill = list(n = 0, prop = 0)) %>% 
        ggplot(aes(x = periodo, y = n, group = mencao, color = mencao)) + 
        geom_line(aes(text = paste0('Período: ', periodo, 
                                    '\nMenção: ', mencao,
                                    '\nQuantidade: ', n))) + 
        labs(x="Período", y="Quantidade de Alunos") +
        scale_colour_manual(name="", values = c("#b2172b", "#ef8a62", "#fddbc7", "#d1e5f0", "#66a9cf", "#2266ac")) + 
        theme_minimal() + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))        

    } else{
      
      p <- bach_filtrado() %>% 
        filter(mencao %in% c("II", "MI", "MM", "MS", "SS")) %>% 
        droplevels() %>% 
        count(periodo, mencao) %>% 
        complete(mencao, fill = list(n = 0, prop = 0)) %>% 
        ggplot(aes(x = periodo, y = n, group = mencao, color = mencao)) + 
        geom_line(aes(text = paste0('Período: ', periodo, 
                                    '\nMenção: ', mencao,
                                    '\nQuantidade: ', n))) + 
        labs(x="Período", y="Quantidade de Alunos") +
        scale_colour_manual(name="", values = c("#ef8a62", "#fddbc7", "#d1e5f0", "#66a9cf", "#2266ac")) + 
        theme_minimal() + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))     
    }
        }
      })
    
    output$bach_aprov_hist <-renderPlotly({
      
      if(input$porcent_bach == TRUE){
        
        if(input$bach_sr == TRUE & input$bach_tr == TRUE){
          
          p <- bach_filtrado() %>% 
            group_by(periodo, resultado) %>% summarise(n = n()) %>% 
            mutate(prop = n/sum(n)) %>% 
            complete(resultado, fill = list(prop = 0)) %>% 
            ggplot(aes(x = periodo, y = prop, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")))) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nResultado: ', resultado,
                                        '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop)))) + 
            labs(x="Período", y="Porcentagem de Alunos") +
            scale_colour_manual(name="", values = c("#2266ac", "#b2172b", "grey70")) + 
            scale_y_continuous(labels = scales::label_percent()) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
        
        } else if(input$bach_sr == FALSE & input$bach_tr == TRUE){  
          
          p <- bach_filtrado() %>% 
            filter(!mencao %in% c("SR")) %>% 
            group_by(periodo, resultado) %>% summarise(n = n()) %>% 
            mutate(prop = n/sum(n)) %>% 
            complete(resultado, fill = list(prop = 0)) %>% 
            ggplot(aes(x = periodo, y = prop, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")))) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nResultado: ', resultado,
                                        '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop)))) + 
            labs(x="Período", y="Porcentagem de Alunos") +
            scale_colour_manual(name="", values = c("#2266ac", "#b2172b", "grey70")) + 
            scale_y_continuous(labels = scales::label_percent()) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
          
        } else if(input$bach_sr == TRUE & input$bach_tr == FALSE){
          
          p <- bach_filtrado() %>% 
            filter(!mencao %in% c("TR", "TJ")) %>% 
            droplevels() %>% 
            group_by(periodo, resultado) %>% summarise(n = n()) %>% 
            mutate(prop = n/sum(n)) %>% 
            complete(resultado, fill = list(prop = 0)) %>% 
            ggplot(aes(x = periodo, y = prop, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação")))) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nResultado: ', resultado,
                                        '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop)))) + 
            labs(x="Período", y="Porcentagem de Alunos") +
            scale_colour_manual(name="", values = c("#2266ac", "#b2172b")) + 
            scale_y_continuous(labels = scales::label_percent()) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
          
        } else{
          
          p <- bach_filtrado() %>% 
            filter(!mencao %in% c("SR", "TR", "TJ")) %>% 
            droplevels() %>% 
            group_by(periodo, resultado) %>% summarise(n = n()) %>% 
            mutate(prop = n/sum(n)) %>% 
            complete(resultado, fill = list(prop = 0)) %>% 
            ggplot(aes(x = periodo, y = prop, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação")))) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nResultado: ', resultado,
                                        '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop)))) + 
            labs(x="Período", y="Porcentagem de Alunos") +
            scale_colour_manual(name="", values = c("#2266ac", "#b2172b")) + 
            scale_y_continuous(labels = scales::label_percent()) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
        }
        
        } else{
          
          if(input$bach_sr == TRUE & input$bach_tr == TRUE){
            
            p <- bach_filtrado() %>% 
              group_by(periodo, resultado) %>% 
              mutate(n = n()) %>% 
              complete(resultado, fill = list(n = 0)) %>% 
              ggplot(aes(x = periodo, y = n, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")))) + 
              geom_line(aes(text = paste0('Período: ', periodo, 
                                          '\nResultado: ', resultado,
                                          '\nQuantidade: ', n))) + 
              labs(x="Período", y="Quantidade de Alunos") +
              scale_colour_manual(name="", values = c("#2266ac", "#b2172b", "grey70")) + 
              theme_minimal() +
              theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
            ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))        
          
            } else if(input$bach_sr == FALSE & input$bach_tr == TRUE){  
            
              p <- bach_filtrado() %>% 
                filter(!mencao %in% c("SR")) %>% 
                group_by(periodo, resultado) %>% 
                mutate(n = n()) %>% 
                complete(resultado, fill = list(n = 0)) %>% 
                ggplot(aes(x = periodo, y = n, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")))) + 
                geom_line(aes(text = paste0('Período: ', periodo, 
                                            '\nResultado: ', resultado,
                                            '\nQuantidade: ', n))) + 
                labs(x="Período", y="Quantidade de Alunos") +
                scale_colour_manual(name="", values = c("#2266ac", "#b2172b", "grey70")) + 
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 90, hjust = 1))
              
              ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))        
             
            } else if(input$bach_sr == TRUE & input$bach_tr == FALSE){
 
              p <- bach_filtrado() %>% 
                filter(!mencao %in% c("TR", "TJ")) %>%
                droplevels() %>% 
                group_by(periodo, resultado) %>% 
                mutate(n = n()) %>% 
                complete(resultado, fill = list(n = 0)) %>% 
                ggplot(aes(x = periodo, y = n, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")))) + 
                geom_line(aes(text = paste0('Período: ', periodo, 
                                            '\nResultado: ', resultado,
                                            '\nQuantidade: ', n))) + 
                labs(x="Período", y="Quantidade de Alunos") +
                scale_colour_manual(name="", values = c("#2266ac", "#b2172b", "grey70")) + 
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 90, hjust = 1))
              
              ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))    
              
            } else{
              
              p <- bach_filtrado() %>% 
                filter(!mencao %in% c("SR", "TR", "TJ")) %>%
                droplevels() %>% 
                group_by(periodo, resultado) %>% 
                mutate(n = n()) %>% 
                complete(resultado, fill = list(n = 0)) %>% 
                ggplot(aes(x = periodo, y = n, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")))) + 
                geom_line(aes(text = paste0('Período: ', periodo, 
                                            '\nResultado: ', resultado,
                                            '\nQuantidade: ', n))) + 
                labs(x="Período", y="Quantidade de Alunos") +
                scale_colour_manual(name="", values = c("#2266ac", "#b2172b", "grey70")) + 
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 90, hjust = 1))
              
              ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))    
            }
          }
      })
    

# Server - Página 3 Serviço -----------------------------------------------

    output$aprovacoes <- renderInfoBox({
      infoBox(
        "Aprovação média", "230 (30%)" ,
        color = "aqua", fill = TRUE,icon=icon("check")
      )
    })
    
    serv_filtrado <- reactive({
      servico %>%
        filter(
          conditional(!is.null(input$serv_disc), disciplina %in% input$serv_disc),
          conditional(TRUE, periodo >= input$serv_periodo[1] & periodo <= input$serv_periodo[2]),
          conditional(!is.null(input$curso), curso %in% input$curso),
          conditional(!is.null(input$serv_professor), professor %in% input$serv_professor),
          # conditional(!is.null(input$serv_horario), horario %in% input$serv_horario),
          conditional(!is.null(input$serv_turma), turma %in% input$serv_turma)
        )
    })
    
    serv_taxa_aprovacao <- reactive({
      (serv_filtrado() %>% 
         count(resultado) %>% 
         summarise(prop = n/sum(n), resultado = resultado) %>% 
         filter(resultado == "Aprovação"))$prop
    })
    
    serv_taxa_reprovacao <- reactive({
      (serv_filtrado() %>% 
         count(resultado) %>% 
         summarise(prop = n/sum(n), resultado = resultado) %>% 
         filter(resultado == "Reprovação"))$prop
    })
    
    output$serv_aprovacoes <- renderInfoBox({
      infoBox(
        "Aprovação média",  label_percent(accuracy = 0.1, decimal.mark = ",")(serv_taxa_aprovacao()),
        color = "aqua", fill = TRUE,icon=icon("check")
      )
    })
    
    output$serv_reprovacoes <- renderInfoBox({
      infoBox(
        "Reprovação média", label_percent(accuracy = 0.1, decimal.mark = ",")(serv_taxa_reprovacao()),
        color = "red", fill = TRUE,icon=icon("times")
      )
    })
    
    serv_disciplinas <- reactive(filter(servico, disciplina %in% input$serv_disc))
    
    observe({
      if (!is.null(input$serv_disc)){
        updateSelectInput(session, "serv_professor", choices = sort(unique(serv_disciplinas()$professor)))
      } else {
        updateSelectInput(session, "serv_professor", choices = sort(professores_ativos))  
      }
    })
    
    # serv_professores <- reactive(filter(servico, professor %in% input$serv_professor))
    # 
    # observe({
    #   if (!is.null(input$serv_professor)){
    #     updateSelectInput(session, "serv_disc", choices = sort(unique(serv_professores()$disciplina)))
    #   } else {
    #     updateSelectInput(session, "serv_disc", choices = sort(unique(servico$disciplina)))  
    #   }
    # }) Erro que limpa as disciplinas depois que seleciona o professor
    
    observeEvent(input$serv_limpar, {
      updateSelectInput(session, "serv_disc", selected = "None")
      updateSliderTextInput(session, "serv_periodo", selected = c(as.character(min(servico$periodo)), as.character(max(servico$periodo))))
      updateSelectInput(session, "curso", selected = "None")
      updateSelectInput(session, "serv_professor", selected = "None")
      # updateSelectInput(session, "serv_horario", selected = "None")
      updateSelectInput(session, "serv_turma", selected = "None")
    })
    
    output$serv_mencoes <- renderPlotly({
      
      if(input$porcent_serv == TRUE){
        
        if(input$serv_sr == TRUE & input$serv_tr == TRUE){
          
          serv_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP")) %>%
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR")),
                   disciplina = fct_reorder(disciplina, mencao, function(.x) mean(.x %in% c("SR", "II", "MI", "TR")))) %>% 
            count(disciplina, mencao) %>% 
            group_by(disciplina) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~disciplina, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', mencao,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$serv_sr == FALSE & input$serv_tr == TRUE){
          
          serv_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "SR")) %>%
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR")),
                   disciplina = fct_reorder(disciplina, mencao, function(.x) mean(.x %in% c("II", "MI", "TR")))) %>% 
            count(disciplina, mencao) %>% 
            group_by(disciplina) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~disciplina, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', mencao,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$serv_sr == TRUE & input$serv_tr == FALSE){
          
          serv_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>%
            mutate(disciplina = fct_reorder(disciplina, mencao, function(.x) mean(.x %in% c("SR", "II", "MI")))) %>% 
            count(disciplina, mencao) %>% 
            group_by(disciplina) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~disciplina, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', mencao,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))   
          
        } else{
          
          serv_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "SR", "TJ", "TR")) %>%
            mutate(disciplina = fct_reorder(disciplina, mencao, function(.x) mean(.x %in% c("II", "MI")))) %>% 
            count(disciplina, mencao) %>% 
            group_by(disciplina) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~disciplina, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', mencao,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))          
        }
        
      } else{
        
        if(input$serv_sr == TRUE & input$serv_tr == TRUE){
          
          serv_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP")) %>%
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR")),
                   disciplina = fct_reorder(disciplina, mencao, function(.x) sum(.x %in% c("SR", "II", "MI", "TR")))) %>% 
            count(disciplina, mencao) %>% 
            plot_ly(type = "bar", x = ~n, y = ~disciplina, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', mencao,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$serv_sr == FALSE & input$serv_tr == TRUE){
          
          serv_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "SR")) %>%
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR")),
                   disciplina = fct_reorder(disciplina, mencao, function(.x) sum(.x %in% c("II", "MI", "TR")))) %>% 
            count(disciplina, mencao) %>% 
            plot_ly(type = "bar", x = ~n, y = ~disciplina, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', mencao,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$serv_sr == TRUE & input$serv_tr == FALSE){
          
          serv_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>%
            mutate(disciplina = fct_reorder(disciplina, mencao, function(.x) sum(.x %in% c("SR", "II", "MI")))) %>% 
            count(disciplina, mencao) %>% 
            plot_ly(type = "bar", x = ~n, y = ~disciplina, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', mencao,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))  
          
        } else{
          
          serv_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "TJ", "TR", "SR")) %>%
            mutate(disciplina = fct_reorder(disciplina, mencao, function(.x) sum(.x %in% c("II", "MI")))) %>% 
            count(disciplina, mencao) %>% 
            plot_ly(type = "bar", x = ~n, y = ~disciplina, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', mencao,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))  
          
        }
        
      }
    })
    
    output$serv_aprov <- renderPlotly({
      
      if(input$porcent_serv == TRUE){
        
        if(input$serv_sr == TRUE & input$serv_tr == TRUE){
          
          serv_filtrado() %>% 
            mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) mean(.x %in% c("Reprovação", "Trancamento")))) %>% 
            count(disciplina, resultado) %>% 
            group_by(disciplina) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~disciplina, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', resultado,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$serv_sr == FALSE & input$serv_tr == TRUE){  
          
          serv_filtrado() %>% 
            filter(!mencao %in% c("SR")) %>% 
            mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) mean(.x %in% c("Reprovação", "Trancamento")))) %>% 
            count(disciplina, resultado) %>% 
            group_by(disciplina) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~disciplina, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', resultado,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$serv_sr == TRUE & input$serv_tr == FALSE){
          
          serv_filtrado() %>% 
            filter(!mencao %in% c("TJ", "TR")) %>% 
            mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) mean(.x %in% c("Reprovação")))) %>% 
            count(disciplina, resultado) %>% 
            group_by(disciplina) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~disciplina, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', resultado,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else{
          
          serv_filtrado() %>% 
            filter(!mencao %in% c("TJ", "TR", "SR")) %>% 
            mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) mean(.x %in% c("Reprovação")))) %>% 
            count(disciplina, resultado) %>% 
            group_by(disciplina) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~disciplina, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', resultado,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
        }
        
      } else{
        
        if(input$serv_sr == TRUE & input$serv_tr == TRUE){
          
          serv_filtrado() %>% 
            mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) sum(.x %in% c("Reprovação", "Trancamento")))) %>% 
            count(disciplina, resultado) %>% 
            plot_ly(type = "bar", x = ~n, y = ~disciplina, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', resultado,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$serv_sr == FALSE & input$serv_tr == TRUE){  
          
          serv_filtrado() %>% 
            filter(!mencao %in% c("SR")) %>% 
            mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) sum(.x %in% c("Reprovação", "Trancamento")))) %>% 
            count(disciplina, resultado) %>% 
            plot_ly(type = "bar", x = ~n, y = ~disciplina, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', resultado,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$serv_sr == TRUE & input$serv_tr == FALSE){
          
          serv_filtrado() %>% 
            filter(!mencao %in% c("TJ", "TR")) %>% 
            mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) sum(.x %in% c("Reprovação")))) %>% 
            count(disciplina, resultado) %>% 
            plot_ly(type = "bar", x = ~n, y = ~disciplina, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', resultado,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else{
          
          serv_filtrado() %>% 
            filter(!mencao %in% c("TJ", "TR", "SR")) %>% 
            mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) sum(.x %in% c("Reprovação")))) %>% 
            count(disciplina, resultado) %>% 
            plot_ly(type = "bar", x = ~n, y = ~disciplina, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', resultado,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
        }
        
      }
    })
    
    output$serv_mencoes_hist <-renderPlotly({
      
      if(input$porcent_serv == TRUE){
        
        if(input$serv_sr == TRUE & input$serv_tr == TRUE){
          
          p <- serv_filtrado() %>% 
            filter(mencao %in% c("SR", "II", "MI", "TJ", "TR", "MM", "MS", "SS")) %>% 
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR"))) %>% 
            droplevels() %>% 
            count(periodo, mencao) %>% 
            group_by(periodo) %>% mutate(prop = n/sum(n)) %>% 
            complete(mencao, fill = list(n = 0, prop = 0)) %>% 
            ggplot(aes(x = periodo, y = prop, group = mencao, color = mencao)) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nMenção: ', mencao,
                                        '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Porcentagem de Alunos") +
            scale_colour_manual(name="", values = c("#b2172b", "#ef8a62", "#fddbc7", "grey70", "#d1e5f0", "#66a9cf", "#2266ac")) + 
            scale_y_continuous(labels = scales::label_percent()) +
            theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
          
        } else if(input$serv_sr == FALSE & input$serv_tr == TRUE){  
          
          p <- serv_filtrado() %>% 
            filter(mencao %in% c("II", "MI", "TJ", "TR", "MM", "MS", "SS")) %>% 
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR"))) %>% 
            droplevels() %>% 
            count(periodo, mencao) %>% 
            group_by(periodo) %>% mutate(prop = n/sum(n)) %>% 
            complete(mencao, fill = list(n = 0, prop = 0)) %>% 
            ggplot(aes(x = periodo, y = prop, group = mencao, color = mencao)) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nMenção: ', mencao,
                                        '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Porcentagem de Alunos") +
            scale_colour_manual(name="", values = c("#ef8a62", "#fddbc7", "grey70", "#d1e5f0", "#66a9cf", "#2266ac")) + 
            scale_y_continuous(labels = scales::label_percent()) +
            theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
          
        } else if(input$serv_sr == TRUE & input$serv_tr == FALSE){
          
          p <- serv_filtrado() %>% 
            filter(mencao %in% c("SR", "II", "MI", "MM", "MS", "SS")) %>% 
            droplevels() %>% 
            count(periodo, mencao) %>% 
            group_by(periodo) %>% mutate(prop = n/sum(n)) %>% 
            complete(mencao, fill = list(n = 0, prop = 0)) %>% 
            ggplot(aes(x = periodo, y = prop, group = mencao, color = mencao)) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nMenção: ', mencao,
                                        '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Porcentagem de Alunos") +
            scale_colour_manual(name="", values = c("#b2172b", "#ef8a62", "#fddbc7", "#d1e5f0", "#66a9cf", "#2266ac")) + 
            scale_y_continuous(labels = scales::label_percent()) +
            theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
          
        } else{
          
          p <- serv_filtrado() %>% 
            filter(mencao %in% c("II", "MI", "MM", "MS", "SS")) %>% 
            droplevels() %>% 
            count(periodo, mencao) %>% 
            group_by(periodo) %>% mutate(prop = n/sum(n)) %>% 
            complete(mencao, fill = list(n = 0, prop = 0)) %>% 
            ggplot(aes(x = periodo, y = prop, group = mencao, color = mencao)) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nMenção: ', mencao,
                                        '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Porcentagem de Alunos") +
            scale_colour_manual(name="", values = c("#ef8a62", "#fddbc7", "#d1e5f0", "#66a9cf", "#2266ac")) + 
            scale_y_continuous(labels = scales::label_percent()) +
            theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
        }
        
      } else{
        
        if(input$serv_sr == TRUE & input$serv_tr == TRUE){
          
          p <- serv_filtrado() %>% 
            filter(mencao %in% c("SR", "II", "MI", "TJ", "TR", "MM", "MS", "SS")) %>% 
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR"))) %>% 
            droplevels() %>% 
            count(periodo, mencao) %>% 
            complete(mencao, fill = list(n = 0, prop = 0)) %>% 
            ggplot(aes(x = periodo, y = n, group = mencao, color = mencao)) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nMenção: ', mencao,
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Quantidade de Alunos") +
            scale_colour_manual(name="", values = c("#b2172b", "#ef8a62", "#fddbc7", "grey70", "#d1e5f0", "#66a9cf", "#2266ac")) + 
            theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))        
          
        } else if(input$serv_sr == FALSE & input$serv_tr == TRUE){  
          
          p <- serv_filtrado() %>% 
            filter(mencao %in% c("II", "MI", "TJ", "TR", "MM", "MS", "SS")) %>% 
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR"))) %>% 
            droplevels() %>% 
            count(periodo, mencao) %>% 
            complete(mencao, fill = list(n = 0, prop = 0)) %>% 
            ggplot(aes(x = periodo, y = n, group = mencao, color = mencao)) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nMenção: ', mencao,
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Quantidade de Alunos") +
            scale_colour_manual(name="", values = c("#ef8a62", "#fddbc7", "grey70", "#d1e5f0", "#66a9cf", "#2266ac")) + 
            theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))        
          
        } else if(input$serv_sr == TRUE & input$serv_tr == FALSE){
          
          p <- serv_filtrado() %>% 
            filter(mencao %in% c("SR", "II", "MI", "MM", "MS", "SS")) %>% 
            droplevels() %>% 
            count(periodo, mencao) %>% 
            complete(mencao, fill = list(n = 0, prop = 0)) %>% 
            ggplot(aes(x = periodo, y = n, group = mencao, color = mencao)) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nMenção: ', mencao,
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Quantidade de Alunos") +
            scale_colour_manual(name="", values = c("#b2172b", "#ef8a62", "#fddbc7", "#d1e5f0", "#66a9cf", "#2266ac")) + 
            theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))        
          
        } else{
          
          p <- serv_filtrado() %>% 
            filter(mencao %in% c("II", "MI", "MM", "MS", "SS")) %>% 
            droplevels() %>% 
            count(periodo, mencao) %>% 
            complete(mencao, fill = list(n = 0, prop = 0)) %>% 
            ggplot(aes(x = periodo, y = n, group = mencao, color = mencao)) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nMenção: ', mencao,
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Quantidade de Alunos") +
            scale_colour_manual(name="", values = c("#ef8a62", "#fddbc7", "#d1e5f0", "#66a9cf", "#2266ac")) + 
            theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))     
        }
      }
    })
    
    output$serv_aprov_hist <-renderPlotly({
      
      if(input$porcent_serv == TRUE){
        
        if(input$serv_sr == TRUE & input$serv_tr == TRUE){
          
          p <- serv_filtrado() %>% 
            group_by(periodo, resultado) %>% summarise(n = n()) %>% 
            mutate(prop = n/sum(n)) %>% 
            complete(resultado, fill = list(prop = 0)) %>% 
            ggplot(aes(x = periodo, y = prop, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")))) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nResultado: ', resultado,
                                        '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop)))) + 
            labs(x="Período", y="Porcentagem de Alunos") +
            scale_colour_manual(name="", values = c("#2266ac", "#b2172b", "grey70")) + 
            scale_y_continuous(labels = scales::label_percent()) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
          
        } else if(input$serv_sr == FALSE & input$serv_tr == TRUE){  
          
          p <- serv_filtrado() %>% 
            filter(!mencao %in% c("SR")) %>% 
            group_by(periodo, resultado) %>% summarise(n = n()) %>% 
            mutate(prop = n/sum(n)) %>% 
            complete(resultado, fill = list(prop = 0)) %>% 
            ggplot(aes(x = periodo, y = prop, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")))) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nResultado: ', resultado,
                                        '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop)))) + 
            labs(x="Período", y="Porcentagem de Alunos") +
            scale_colour_manual(name="", values = c("#2266ac", "#b2172b", "grey70")) + 
            scale_y_continuous(labels = scales::label_percent()) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
          
        } else if(input$serv_sr == TRUE & input$serv_tr == FALSE){
          
          p <- serv_filtrado() %>% 
            filter(!mencao %in% c("TR", "TJ")) %>% 
            droplevels() %>% 
            group_by(periodo, resultado) %>% summarise(n = n()) %>% 
            mutate(prop = n/sum(n)) %>% 
            complete(resultado, fill = list(prop = 0)) %>% 
            ggplot(aes(x = periodo, y = prop, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação")))) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nResultado: ', resultado,
                                        '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop)))) + 
            labs(x="Período", y="Porcentagem de Alunos") +
            scale_colour_manual(name="", values = c("#2266ac", "#b2172b")) + 
            scale_y_continuous(labels = scales::label_percent()) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
          
        } else{
          
          p <- serv_filtrado() %>% 
            filter(!mencao %in% c("SR", "TR", "TJ")) %>% 
            droplevels() %>% 
            group_by(periodo, resultado) %>% summarise(n = n()) %>% 
            mutate(prop = n/sum(n)) %>% 
            complete(resultado, fill = list(prop = 0)) %>% 
            ggplot(aes(x = periodo, y = prop, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação")))) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nResultado: ', resultado,
                                        '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop)))) + 
            labs(x="Período", y="Porcentagem de Alunos") +
            scale_colour_manual(name="", values = c("#2266ac", "#b2172b")) + 
            scale_y_continuous(labels = scales::label_percent()) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
        }
        
      } else{
        
        if(input$serv_sr == TRUE & input$serv_tr == TRUE){
          
          p <- serv_filtrado() %>% 
            group_by(periodo, resultado) %>% 
            mutate(n = n()) %>% 
            complete(resultado, fill = list(n = 0)) %>% 
            ggplot(aes(x = periodo, y = n, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")))) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nResultado: ', resultado,
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Quantidade de Alunos") +
            scale_colour_manual(name="", values = c("#2266ac", "#b2172b", "grey70")) + 
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))        
          
        } else if(input$serv_sr == FALSE & input$serv_tr == TRUE){  
          
          p <- serv_filtrado() %>% 
            filter(!mencao %in% c("SR")) %>% 
            group_by(periodo, resultado) %>% 
            mutate(n = n()) %>% 
            complete(resultado, fill = list(n = 0)) %>% 
            ggplot(aes(x = periodo, y = n, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")))) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nResultado: ', resultado,
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Quantidade de Alunos") +
            scale_colour_manual(name="", values = c("#2266ac", "#b2172b", "grey70")) + 
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))        
          
        } else if(input$serv_sr == TRUE & input$serv_tr == FALSE){
          
          p <- serv_filtrado() %>% 
            filter(!mencao %in% c("TR", "TJ")) %>%
            droplevels() %>% 
            group_by(periodo, resultado) %>% 
            mutate(n = n()) %>% 
            complete(resultado, fill = list(n = 0)) %>% 
            ggplot(aes(x = periodo, y = n, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")))) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nResultado: ', resultado,
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Quantidade de Alunos") +
            scale_colour_manual(name="", values = c("#2266ac", "#b2172b", "grey70")) + 
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))    
          
        } else{
          
          p <- serv_filtrado() %>% 
            filter(!mencao %in% c("SR", "TR", "TJ")) %>%
            droplevels() %>% 
            group_by(periodo, resultado) %>% 
            mutate(n = n()) %>% 
            complete(resultado, fill = list(n = 0)) %>% 
            ggplot(aes(x = periodo, y = n, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")))) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nResultado: ', resultado,
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Quantidade de Alunos") +
            scale_colour_manual(name="", values = c("#2266ac", "#b2172b", "grey70")) + 
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))    
        }
      }
    })

# Server - Página 4 Matemática -------------------------------------------
    
    mat_filtrado <- reactive({
      matematica %>%
        filter(
          conditional(!is.null(input$mat_disc), disciplina %in% input$mat_disc),
          conditional(TRUE, periodo >= input$mat_periodo[1] & periodo <= input$mat_periodo[2]),
          conditional(!is.null(input$mat_professor), professor %in% input$mat_professor),
          # conditional(!is.null(input$mat_horario), horario %in% input$mat_horario),
          conditional(!is.null(input$mat_turma), turma %in% input$mat_turma)
        )
    })
    
    mat_taxa_aprovacao <- reactive({
      (mat_filtrado() %>% 
         count(resultado) %>% 
         summarise(prop = n/sum(n), resultado = resultado) %>% 
         filter(resultado == "Aprovação"))$prop
    })
    
    mat_taxa_reprovacao <- reactive({
      (mat_filtrado() %>% 
         count(resultado) %>% 
         summarise(prop = n/sum(n), resultado = resultado) %>% 
         filter(resultado == "Reprovação"))$prop
    })
    
    output$mat_aprovacoes <- renderInfoBox({
      infoBox(
        "Aprovação",  label_percent(accuracy = 0.1, decimal.mark = ",")(mat_taxa_aprovacao()),
        color = "aqua", fill = TRUE,icon=icon("check")
      )
    })
    
    output$mat_reprovacoes <- renderInfoBox({
      infoBox(
        "Reprovação", label_percent(accuracy = 0.1, decimal.mark = ",")(mat_taxa_reprovacao()),
        color = "red", fill = TRUE,icon=icon("times")
      )
    })
    
    mat_disciplinas <- reactive(filter(matematica, disciplina %in% input$mat_disc))
    
    observe({
      if (!is.null(input$mat_disc)){
        updateSelectInput(session, "mat_professor", choices = sort(unique(mat_disciplinas()$professor)))
      } else {
        updateSelectInput(session, "mat_professor", choices = sort(professores_ativos))  
      }
    })
    
    # mat_professores <- reactive(filter(matematica, professor %in% input$mat_professor))
    # 
    # observe({
    #   if (!is.null(input$mat_professor)){
    #     updateSelectInput(session, "mat_disc", choices = sort(unique(mat_professores()$disciplina)))
    #   } else {
    #     updateSelectInput(session, "mat_disc", choices = sort(unique(matematica$disciplina)))  
    #   }
    # }) Erro que limpa as disciplinas depois que seleciona o professor
    
    observeEvent(input$mat_limpar, {
      updateSelectInput(session, "mat_disc", selected = "None")
      updateSliderTextInput(session, "mat_periodo", selected = c(as.character(min(matematica$periodo)), as.character(max(matematica$periodo))))
      updateSelectInput(session, "mat_professor", selected = "None")
      # updateSelectInput(session, "mat_horario", selected = "None")
      updateSelectInput(session, "mat_turma", selected = "None")
    })
    
    output$mat_mencoes <- renderPlotly({
      
      if(input$porcent_mat == TRUE){
        
        if(input$mat_sr == TRUE & input$mat_tr == TRUE){
          
          mat_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP")) %>%
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR")),
                   disciplina = fct_reorder(disciplina, mencao, function(.x) mean(.x %in% c("SR", "II", "MI", "TR")))) %>% 
            count(disciplina, mencao) %>% 
            group_by(disciplina) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~disciplina, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', mencao,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$mat_sr == FALSE & input$mat_tr == TRUE){
          
          mat_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "SR")) %>%
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR")),
                   disciplina = fct_reorder(disciplina, mencao, function(.x) mean(.x %in% c("II", "MI", "TR")))) %>% 
            count(disciplina, mencao) %>% 
            group_by(disciplina) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~disciplina, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', mencao,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$mat_sr == TRUE & input$mat_tr == FALSE){
          
          mat_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>%
            mutate(disciplina = fct_reorder(disciplina, mencao, function(.x) mean(.x %in% c("SR", "II", "MI")))) %>% 
            count(disciplina, mencao) %>% 
            group_by(disciplina) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~disciplina, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', mencao,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))   
          
        } else{
          
          mat_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "SR", "TJ", "TR")) %>%
            mutate(disciplina = fct_reorder(disciplina, mencao, function(.x) mean(.x %in% c("II", "MI")))) %>% 
            count(disciplina, mencao) %>% 
            group_by(disciplina) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~disciplina, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', mencao,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))          
        }
        
      } else{
        
        if(input$mat_sr == TRUE & input$mat_tr == TRUE){
          
          mat_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP")) %>%
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR")),
                   disciplina = fct_reorder(disciplina, mencao, function(.x) sum(.x %in% c("SR", "II", "MI", "TR")))) %>% 
            count(disciplina, mencao) %>% 
            plot_ly(type = "bar", x = ~n, y = ~disciplina, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', mencao,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$mat_sr == FALSE & input$mat_tr == TRUE){
          
          mat_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "SR")) %>%
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR")),
                   disciplina = fct_reorder(disciplina, mencao, function(.x) sum(.x %in% c("II", "MI", "TR")))) %>% 
            count(disciplina, mencao) %>% 
            plot_ly(type = "bar", x = ~n, y = ~disciplina, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', mencao,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$mat_sr == TRUE & input$mat_tr == FALSE){
          
          mat_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>%
            mutate(disciplina = fct_reorder(disciplina, mencao, function(.x) sum(.x %in% c("SR", "II", "MI")))) %>% 
            count(disciplina, mencao) %>% 
            plot_ly(type = "bar", x = ~n, y = ~disciplina, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', mencao,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))  
          
        } else{
          
          mat_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "TJ", "TR", "SR")) %>%
            mutate(disciplina = fct_reorder(disciplina, mencao, function(.x) sum(.x %in% c("II", "MI")))) %>% 
            count(disciplina, mencao) %>% 
            plot_ly(type = "bar", x = ~n, y = ~disciplina, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', mencao,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))  
          
        }
        
      }
    })
    
    output$mat_aprov <- renderPlotly({
      
      if(input$porcent_mat == TRUE){
        
        if(input$mat_sr == TRUE & input$mat_tr == TRUE){
          
          mat_filtrado() %>% 
            mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) mean(.x %in% c("Reprovação", "Trancamento")))) %>% 
            count(disciplina, resultado) %>% 
            group_by(disciplina) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~disciplina, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', resultado,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$mat_sr == FALSE & input$mat_tr == TRUE){  
          
          mat_filtrado() %>% 
            filter(!mencao %in% c("SR")) %>% 
            mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) mean(.x %in% c("Reprovação", "Trancamento")))) %>% 
            count(disciplina, resultado) %>% 
            group_by(disciplina) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~disciplina, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', resultado,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$mat_sr == TRUE & input$mat_tr == FALSE){
          
          mat_filtrado() %>% 
            filter(!mencao %in% c("TJ", "TR")) %>% 
            mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) mean(.x %in% c("Reprovação")))) %>% 
            count(disciplina, resultado) %>% 
            group_by(disciplina) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~disciplina, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', resultado,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else{
          
          mat_filtrado() %>% 
            filter(!mencao %in% c("TJ", "TR", "SR")) %>% 
            mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) mean(.x %in% c("Reprovação")))) %>% 
            count(disciplina, resultado) %>% 
            group_by(disciplina) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~disciplina, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', resultado,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
        }
        
      } else{
        
        if(input$mat_sr == TRUE & input$mat_tr == TRUE){
          
          mat_filtrado() %>% 
            mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) sum(.x %in% c("Reprovação", "Trancamento")))) %>% 
            count(disciplina, resultado) %>% 
            plot_ly(type = "bar", x = ~n, y = ~disciplina, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', resultado,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$mat_sr == FALSE & input$mat_tr == TRUE){  
          
          mat_filtrado() %>% 
            filter(!mencao %in% c("SR")) %>% 
            mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) sum(.x %in% c("Reprovação", "Trancamento")))) %>% 
            count(disciplina, resultado) %>% 
            plot_ly(type = "bar", x = ~n, y = ~disciplina, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', resultado,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$mat_sr == TRUE & input$mat_tr == FALSE){
          
          mat_filtrado() %>% 
            filter(!mencao %in% c("TJ", "TR")) %>% 
            mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) sum(.x %in% c("Reprovação")))) %>% 
            count(disciplina, resultado) %>% 
            plot_ly(type = "bar", x = ~n, y = ~disciplina, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', resultado,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else{
          
          mat_filtrado() %>% 
            filter(!mencao %in% c("TJ", "TR", "SR")) %>% 
            mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) sum(.x %in% c("Reprovação")))) %>% 
            count(disciplina, resultado) %>% 
            plot_ly(type = "bar", x = ~n, y = ~disciplina, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Disciplina: ', disciplina, 
                                                       '\nMenção: ', resultado,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
        }
        
      }
    })
    
    output$mat_mencoes_hist <-renderPlotly({
      
      if(input$porcent_mat == TRUE){
        
        if(input$mat_sr == TRUE & input$mat_tr == TRUE){
          
          p <- mat_filtrado() %>% 
            filter(mencao %in% c("SR", "II", "MI", "TJ", "TR", "MM", "MS", "SS")) %>% 
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR"))) %>% 
            droplevels() %>% 
            count(periodo, mencao) %>% 
            group_by(periodo) %>% mutate(prop = n/sum(n)) %>% 
            complete(mencao, fill = list(n = 0, prop = 0)) %>% 
            ggplot(aes(x = periodo, y = prop, group = mencao, color = mencao)) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nMenção: ', mencao,
                                        '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Porcentagem de Alunos") +
            scale_colour_manual(name="", values = c("#b2172b", "#ef8a62", "#fddbc7", "grey70", "#d1e5f0", "#66a9cf", "#2266ac")) + 
            scale_y_continuous(labels = scales::label_percent()) +
            theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
          
        } else if(input$mat_sr == FALSE & input$mat_tr == TRUE){  
          
          p <- mat_filtrado() %>% 
            filter(mencao %in% c("II", "MI", "TJ", "TR", "MM", "MS", "SS")) %>% 
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR"))) %>% 
            droplevels() %>% 
            count(periodo, mencao) %>% 
            group_by(periodo) %>% mutate(prop = n/sum(n)) %>% 
            complete(mencao, fill = list(n = 0, prop = 0)) %>% 
            ggplot(aes(x = periodo, y = prop, group = mencao, color = mencao)) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nMenção: ', mencao,
                                        '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Porcentagem de Alunos") +
            scale_colour_manual(name="", values = c("#ef8a62", "#fddbc7", "grey70", "#d1e5f0", "#66a9cf", "#2266ac")) + 
            scale_y_continuous(labels = scales::label_percent()) +
            theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
          
        } else if(input$mat_sr == TRUE & input$mat_tr == FALSE){
          
          p <- mat_filtrado() %>% 
            filter(mencao %in% c("SR", "II", "MI", "MM", "MS", "SS")) %>% 
            droplevels() %>% 
            count(periodo, mencao) %>% 
            group_by(periodo) %>% mutate(prop = n/sum(n)) %>% 
            complete(mencao, fill = list(n = 0, prop = 0)) %>% 
            ggplot(aes(x = periodo, y = prop, group = mencao, color = mencao)) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nMenção: ', mencao,
                                        '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Porcentagem de Alunos") +
            scale_colour_manual(name="", values = c("#b2172b", "#ef8a62", "#fddbc7", "#d1e5f0", "#66a9cf", "#2266ac")) + 
            scale_y_continuous(labels = scales::label_percent()) +
            theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
          
        } else{
          
          p <- mat_filtrado() %>% 
            filter(mencao %in% c("II", "MI", "MM", "MS", "SS")) %>% 
            droplevels() %>% 
            count(periodo, mencao) %>% 
            group_by(periodo) %>% mutate(prop = n/sum(n)) %>% 
            complete(mencao, fill = list(n = 0, prop = 0)) %>% 
            ggplot(aes(x = periodo, y = prop, group = mencao, color = mencao)) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nMenção: ', mencao,
                                        '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Porcentagem de Alunos") +
            scale_colour_manual(name="", values = c("#ef8a62", "#fddbc7", "#d1e5f0", "#66a9cf", "#2266ac")) + 
            scale_y_continuous(labels = scales::label_percent()) +
            theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
        }
        
      } else{
        
        if(input$mat_sr == TRUE & input$mat_tr == TRUE){
          
          p <- mat_filtrado() %>% 
            filter(mencao %in% c("SR", "II", "MI", "TJ", "TR", "MM", "MS", "SS")) %>% 
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR"))) %>% 
            droplevels() %>% 
            count(periodo, mencao) %>% 
            complete(mencao, fill = list(n = 0, prop = 0)) %>% 
            ggplot(aes(x = periodo, y = n, group = mencao, color = mencao)) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nMenção: ', mencao,
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Quantidade de Alunos") +
            scale_colour_manual(name="", values = c("#b2172b", "#ef8a62", "#fddbc7", "grey70", "#d1e5f0", "#66a9cf", "#2266ac")) + 
            theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))        
          
        } else if(input$mat_sr == FALSE & input$mat_tr == TRUE){  
          
          p <- mat_filtrado() %>% 
            filter(mencao %in% c("II", "MI", "TJ", "TR", "MM", "MS", "SS")) %>% 
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR"))) %>% 
            droplevels() %>% 
            count(periodo, mencao) %>% 
            complete(mencao, fill = list(n = 0, prop = 0)) %>% 
            ggplot(aes(x = periodo, y = n, group = mencao, color = mencao)) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nMenção: ', mencao,
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Quantidade de Alunos") +
            scale_colour_manual(name="", values = c("#ef8a62", "#fddbc7", "grey70", "#d1e5f0", "#66a9cf", "#2266ac")) + 
            theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))        
          
        } else if(input$mat_sr == TRUE & input$mat_tr == FALSE){
          
          p <- mat_filtrado() %>% 
            filter(mencao %in% c("SR", "II", "MI", "MM", "MS", "SS")) %>% 
            droplevels() %>% 
            count(periodo, mencao) %>% 
            complete(mencao, fill = list(n = 0, prop = 0)) %>% 
            ggplot(aes(x = periodo, y = n, group = mencao, color = mencao)) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nMenção: ', mencao,
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Quantidade de Alunos") +
            scale_colour_manual(name="", values = c("#b2172b", "#ef8a62", "#fddbc7", "#d1e5f0", "#66a9cf", "#2266ac")) + 
            theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))        
          
        } else{
          
          p <- mat_filtrado() %>% 
            filter(mencao %in% c("II", "MI", "MM", "MS", "SS")) %>% 
            droplevels() %>% 
            count(periodo, mencao) %>% 
            complete(mencao, fill = list(n = 0, prop = 0)) %>% 
            ggplot(aes(x = periodo, y = n, group = mencao, color = mencao)) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nMenção: ', mencao,
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Quantidade de Alunos") +
            scale_colour_manual(name="", values = c("#ef8a62", "#fddbc7", "#d1e5f0", "#66a9cf", "#2266ac")) + 
            theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))     
        }
      }
    })
    
    output$mat_aprov_hist <-renderPlotly({
      
      if(input$porcent_mat == TRUE){
        
        if(input$mat_sr == TRUE & input$mat_tr == TRUE){
          
          p <- mat_filtrado() %>% 
            group_by(periodo, resultado) %>% summarise(n = n()) %>% 
            mutate(prop = n/sum(n)) %>% 
            complete(resultado, fill = list(prop = 0)) %>% 
            ggplot(aes(x = periodo, y = prop, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")))) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nResultado: ', resultado,
                                        '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop)))) + 
            labs(x="Período", y="Porcentagem de Alunos") +
            scale_colour_manual(name="", values = c("#2266ac", "#b2172b", "grey70")) + 
            scale_y_continuous(labels = scales::label_percent()) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
          
        } else if(input$mat_sr == FALSE & input$mat_tr == TRUE){  
          
          p <- mat_filtrado() %>% 
            filter(!mencao %in% c("SR")) %>% 
            group_by(periodo, resultado) %>% summarise(n = n()) %>% 
            mutate(prop = n/sum(n)) %>% 
            complete(resultado, fill = list(prop = 0)) %>% 
            ggplot(aes(x = periodo, y = prop, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")))) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nResultado: ', resultado,
                                        '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop)))) + 
            labs(x="Período", y="Porcentagem de Alunos") +
            scale_colour_manual(name="", values = c("#2266ac", "#b2172b", "grey70")) + 
            scale_y_continuous(labels = scales::label_percent()) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
          
        } else if(input$mat_sr == TRUE & input$mat_tr == FALSE){
          
          p <- mat_filtrado() %>% 
            filter(!mencao %in% c("TR", "TJ")) %>% 
            droplevels() %>% 
            group_by(periodo, resultado) %>% summarise(n = n()) %>% 
            mutate(prop = n/sum(n)) %>% 
            complete(resultado, fill = list(prop = 0)) %>% 
            ggplot(aes(x = periodo, y = prop, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação")))) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nResultado: ', resultado,
                                        '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop)))) + 
            labs(x="Período", y="Porcentagem de Alunos") +
            scale_colour_manual(name="", values = c("#2266ac", "#b2172b")) + 
            scale_y_continuous(labels = scales::label_percent()) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
          
        } else{
          
          p <- mat_filtrado() %>% 
            filter(!mencao %in% c("SR", "TR", "TJ")) %>% 
            droplevels() %>% 
            group_by(periodo, resultado) %>% summarise(n = n()) %>% 
            mutate(prop = n/sum(n)) %>% 
            complete(resultado, fill = list(prop = 0)) %>% 
            ggplot(aes(x = periodo, y = prop, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação")))) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nResultado: ', resultado,
                                        '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop)))) + 
            labs(x="Período", y="Porcentagem de Alunos") +
            scale_colour_manual(name="", values = c("#2266ac", "#b2172b")) + 
            scale_y_continuous(labels = scales::label_percent()) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
        }
        
      } else{
        
        if(input$mat_sr == TRUE & input$mat_tr == TRUE){
          
          p <- mat_filtrado() %>% 
            group_by(periodo, resultado) %>% 
            mutate(n = n()) %>% 
            complete(resultado, fill = list(n = 0)) %>% 
            ggplot(aes(x = periodo, y = n, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")))) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nResultado: ', resultado,
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Quantidade de Alunos") +
            scale_colour_manual(name="", values = c("#2266ac", "#b2172b", "grey70")) + 
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))        
          
        } else if(input$mat_sr == FALSE & input$mat_tr == TRUE){  
          
          p <- mat_filtrado() %>% 
            filter(!mencao %in% c("SR")) %>% 
            group_by(periodo, resultado) %>% 
            mutate(n = n()) %>% 
            complete(resultado, fill = list(n = 0)) %>% 
            ggplot(aes(x = periodo, y = n, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")))) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nResultado: ', resultado,
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Quantidade de Alunos") +
            scale_colour_manual(name="", values = c("#2266ac", "#b2172b", "grey70")) + 
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))        
          
        } else if(input$mat_sr == TRUE & input$mat_tr == FALSE){
          
          p <- mat_filtrado() %>% 
            filter(!mencao %in% c("TR", "TJ")) %>%
            droplevels() %>% 
            group_by(periodo, resultado) %>% 
            mutate(n = n()) %>% 
            complete(resultado, fill = list(n = 0)) %>% 
            ggplot(aes(x = periodo, y = n, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")))) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nResultado: ', resultado,
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Quantidade de Alunos") +
            scale_colour_manual(name="", values = c("#2266ac", "#b2172b", "grey70")) + 
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))    
          
        } else{
          
          p <- mat_filtrado() %>% 
            filter(!mencao %in% c("SR", "TR", "TJ")) %>%
            droplevels() %>% 
            group_by(periodo, resultado) %>% 
            mutate(n = n()) %>% 
            complete(resultado, fill = list(n = 0)) %>% 
            ggplot(aes(x = periodo, y = n, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")))) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nResultado: ', resultado,
                                        '\nQuantidade: ', n))) + 
            labs(x="Período", y="Quantidade de Alunos") +
            scale_colour_manual(name="", values = c("#2266ac", "#b2172b", "grey70")) + 
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))    
        }
      }
    })
    
# Server - Página 5 Professores -------------------------------------------
    
    prof_disciplinas <- reactive(filter(historico, disciplina %in% input$prof_disc))
    
    observe({
      if (!is.null(input$prof_disc)){
        updateSelectInput(session, "prof_professor", choices = sort(unique(prof_disciplinas()$professor)))
      } else {
        updateSelectInput(session, "prof_professor", choices = sort(professores_ativos))  
      }
    })
    
    # prof_professores <- reactive(filter(historico, professor %in% input$prof_professor))
    # 
    # observe({
    #   if (!is.null(input$prof_professor)){
    #     updateSelectInput(session, "prof_disc", choices = sort(unique(prof_professores()$disciplina)))
    #   } else {
    #     updateSelectInput(session, "prof_disc", choices = sort(unique(historico$disciplina)))  
    #   }
    # }) Erro que limpa as disciplinas depois que seleciona o professor
    
    observeEvent(input$prof_limpar, {
      updateSelectInput(session, "prof_disc", selected = "None")
      updateSliderTextInput(session, "prof_periodo", selected = c(as.character(min(historico$periodo)), as.character(max(historico$periodo))))
      updateSelectInput(session, "prof_professor", selected = "None")
      # updateSelectInput(session, "prof_horario", selected = "None")
      updateSelectInput(session, "prof_turma", selected = "None")
    })
    
    hist_filtrado <- reactive({
      if(input$prof_ativos == FALSE){
        historico %>%
          filter(
            conditional(!is.null(input$prof_disc), disciplina %in% input$prof_disc),
            conditional(TRUE, periodo >= input$prof_periodo[1] & periodo <= input$prof_periodo[2]),
            conditional(!is.null(input$prof_professor), professor %in% input$prof_professor),
            # conditional(!is.null(input$prof_horario), horario %in% input$prof_horario),
            conditional(!is.null(input$prof_turma), turma %in% input$prof_turma)
          )
      } else{
        historico %>%
          filter(
            professor %in% professores_ativos,
            conditional(!is.null(input$prof_disc), disciplina %in% input$prof_disc),
            conditional(TRUE, periodo >= input$prof_periodo[1] & periodo <= input$prof_periodo[2]),
            conditional(!is.null(input$prof_professor), professor %in% input$prof_professor),
            # conditional(!is.null(input$prof_horario), horario %in% input$prof_horario),
            conditional(!is.null(input$prof_turma), turma %in% input$prof_turma)
          )
      } 
    })
    
    output$prof_mencoes <- renderPlotly({
      
      if(input$porcent_prof == TRUE){
        
        if(input$prof_sr == TRUE & input$prof_tr == TRUE){
          
          hist_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP")) %>%
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR")),
                   professor = fct_reorder(professor, mencao, function(.x) mean(.x %in% c("SR", "II", "MI", "TR")))) %>% 
            count(professor, mencao) %>% 
            group_by(professor) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~professor, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Professor: ', professor, 
                                                       '\nMenção: ', mencao,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$prof_sr == FALSE & input$prof_tr == TRUE){
          
          hist_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "SR")) %>%
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR")),
                   professor = fct_reorder(professor, mencao, function(.x) mean(.x %in% c("II", "MI", "TR")))) %>% 
            count(professor, mencao) %>% 
            group_by(professor) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~professor, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Professor: ', professor, 
                                                       '\nMenção: ', mencao,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$prof_sr == TRUE & input$prof_tr == FALSE){
          
          hist_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>%
            mutate(professor = fct_reorder(professor, mencao, function(.x) mean(.x %in% c("SR", "II", "MI")))) %>% 
            count(professor, mencao) %>% 
            group_by(professor) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~professor, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Professor: ', professor, 
                                                       '\nMenção: ', mencao,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))   
          
        } else{
          
          hist_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "SR", "TJ", "TR")) %>%
            mutate(professor = fct_reorder(professor, mencao, function(.x) mean(.x %in% c("II", "MI")))) %>% 
            count(professor, mencao) %>% 
            group_by(professor) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~professor, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Professor: ', professor, 
                                                       '\nMenção: ', mencao,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))          
        }
        
      } else{
        
        if(input$prof_sr == TRUE & input$prof_tr == TRUE){
          
          hist_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP")) %>%
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR")),
                   professor = fct_reorder(professor, mencao, function(.x) sum(.x %in% c("SR", "II", "MI", "TR")))) %>% 
            count(professor, mencao) %>% 
            plot_ly(type = "bar", x = ~n, y = ~professor, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Professor: ', professor, 
                                                       '\nMenção: ', mencao,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$prof_sr == FALSE & input$prof_tr == TRUE){
          
          hist_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "SR")) %>%
            mutate(mencao = fct_collapse(mencao, "TR" = c("TJ", "TR")),
                   professor = fct_reorder(professor, mencao, function(.x) sum(.x %in% c("II", "MI", "TR")))) %>% 
            count(professor, mencao) %>% 
            plot_ly(type = "bar", x = ~n, y = ~professor, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Professor: ', professor, 
                                                       '\nMenção: ', mencao,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$prof_sr == TRUE & input$prof_tr == FALSE){
          
          hist_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>%
            mutate(professor = fct_reorder(professor, mencao, function(.x) sum(.x %in% c("SR", "II", "MI")))) %>% 
            count(professor, mencao) %>% 
            plot_ly(type = "bar", x = ~n, y = ~professor, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Professor: ', professor, 
                                                       '\nMenção: ', mencao,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))  
          
        } else{
          
          hist_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "TJ", "TR", "SR")) %>%
            mutate(professor = fct_reorder(professor, mencao, function(.x) sum(.x %in% c("II", "MI")))) %>% 
            count(professor, mencao) %>% 
            plot_ly(type = "bar", x = ~n, y = ~professor, color = ~fct_rev(mencao), colors = c("#2266ac", "#66a9cf", "#d1e5f0", "grey70", "#fddbc7", "#ef8a62", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Professor: ', professor, 
                                                       '\nMenção: ', mencao,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))  
          
        }
        
      }
    })
    
    output$prof_aprov <- renderPlotly({
      
      if(input$porcent_prof == TRUE){
        
        if(input$prof_sr == TRUE & input$prof_tr == TRUE){
          
          hist_filtrado() %>% 
            mutate(professor = fct_reorder(professor, resultado, function(.x) mean(.x %in% c("Reprovação", "Trancamento")))) %>% 
            count(professor, resultado) %>% 
            group_by(professor) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~professor, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Professor: ', professor, 
                                                       '\nMenção: ', resultado,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$prof_sr == FALSE & input$prof_tr == TRUE){  
          
          hist_filtrado() %>% 
            filter(!mencao %in% c("SR")) %>% 
            mutate(professor = fct_reorder(professor, resultado, function(.x) mean(.x %in% c("Reprovação", "Trancamento")))) %>% 
            count(professor, resultado) %>% 
            group_by(professor) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~professor, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Professor: ', professor, 
                                                       '\nMenção: ', resultado,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$prof_sr == TRUE & input$prof_tr == FALSE){
          
          hist_filtrado() %>% 
            filter(!mencao %in% c("TJ", "TR")) %>% 
            mutate(professor = fct_reorder(professor, resultado, function(.x) mean(.x %in% c("Reprovação")))) %>% 
            count(professor, resultado) %>% 
            group_by(professor) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~professor, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Professor: ', professor, 
                                                       '\nMenção: ', resultado,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else{
          
          hist_filtrado() %>% 
            filter(!mencao %in% c("TJ", "TR", "SR")) %>% 
            mutate(professor = fct_reorder(professor, resultado, function(.x) mean(.x %in% c("Reprovação")))) %>% 
            count(professor, resultado) %>% 
            group_by(professor) %>% mutate(prop = n/sum(n)) %>% 
            plot_ly(type = "bar", x = ~prop, y = ~professor, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Professor: ', professor, 
                                                       '\nMenção: ', resultado,
                                                       '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop),
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Porcentagem de Alunos", tickformat = "%"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
        }
        
      } else{
        
        if(input$prof_sr == TRUE & input$prof_tr == TRUE){
          
          hist_filtrado() %>% 
            mutate(professor = fct_reorder(professor, resultado, function(.x) sum(.x %in% c("Reprovação", "Trancamento")))) %>% 
            count(professor, resultado) %>% 
            plot_ly(type = "bar", x = ~n, y = ~professor, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Professor: ', professor, 
                                                       '\nMenção: ', resultado,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$prof_sr == FALSE & input$prof_tr == TRUE){  
          
          hist_filtrado() %>% 
            filter(!mencao %in% c("SR")) %>% 
            mutate(professor = fct_reorder(professor, resultado, function(.x) sum(.x %in% c("Reprovação", "Trancamento")))) %>% 
            count(professor, resultado) %>% 
            plot_ly(type = "bar", x = ~n, y = ~professor, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Professor: ', professor, 
                                                       '\nMenção: ', resultado,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else if(input$prof_sr == TRUE & input$prof_tr == FALSE){
          
          hist_filtrado() %>% 
            filter(!mencao %in% c("TJ", "TR")) %>% 
            mutate(professor = fct_reorder(professor, resultado, function(.x) sum(.x %in% c("Reprovação")))) %>% 
            count(professor, resultado) %>% 
            plot_ly(type = "bar", x = ~n, y = ~professor, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Professor: ', professor, 
                                                       '\nMenção: ', resultado,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
          
        } else{
          
          hist_filtrado() %>% 
            filter(!mencao %in% c("TJ", "TR", "SR")) %>% 
            mutate(professor = fct_reorder(professor, resultado, function(.x) sum(.x %in% c("Reprovação")))) %>% 
            count(professor, resultado) %>% 
            plot_ly(type = "bar", x = ~n, y = ~professor, color = ~fct_rev(resultado), colors = c("#2266ac", "grey70", "#b2172b"),
                    hoverinfo = "text", text = ~paste0('Professor: ', professor, 
                                                       '\nMenção: ', resultado,
                                                       '\nQuantidade: ', n)) %>% 
            layout(barmode = "stack",
                   yaxis = list(title = "", autorange = "reversed"),
                   xaxis = list(title = "Quantidade de Alunos"),
                   margin = list(pad = 5),
                   legend = list(orientation = "h", traceorder = "normal", x = 0, y = 1.05))
        }
        
      }
    })
   
    alunos_por_professor_geral <- reactive({
      (hist_filtrado() %>% 
         group_by(professor, periodo) %>% 
         summarise(alunos_professor_periodo = length(nome)) %>% 
         summarise(alunos_por_professor = mean(alunos_professor_periodo)) %>% summarise(alunos_por_professor_geral = mean(alunos_por_professor)))[[1]]
    })
    
    output$prof_alunos_por_professor_geral <- renderInfoBox({
      infoBox(
        "Alunos por Professor", label_number(accuracy = 0.1, decimal.mark = ",")(alunos_por_professor_geral()),
        color = "light-blue", fill = TRUE,icon=icon("chalkboard-teacher")
      )
    })
    
    output$prof_alunos_por_prof <- renderPlotly({
      hist_filtrado() %>% 
        group_by(professor, periodo) %>% 
        summarise(alunos_professor_periodo = length(nome)) %>% # contar aluno duas vezes se faz duas matérias com o mesmo professor? n_distinct?
        summarise(alunos_por_professor = mean(alunos_professor_periodo)) %>% 
        mutate(professor = fct_reorder(professor, alunos_por_professor)) %>% 
        plot_ly(type = "scatter", mode = "markers", x = ~alunos_por_professor, y = ~professor, 
                color = I("#2266ac"), size = 15,
                hoverinfo = "text", text = ~paste0('Professor: ', professor, 
                                                   '\nAlunos por professor: ', label_number(accuracy = 0.1, decimal.mark = ",")(alunos_por_professor))) %>% 
        add_segments(y = ~professor, yend = ~professor, x = 0, xend= ~alunos_por_professor,
                     size = I(3)) %>%
        layout(yaxis = list(title = ""),
               xaxis = list(title = "Quantidade de Alunos por Professor"),
               margin = list(pad = 5),
               showlegend = FALSE)
      
    })
    
    output$prof_alunos_por_professor_hist <-renderPlotly({
          p <- hist_filtrado() %>% 
            group_by(periodo, professor) %>% 
            summarise(alunos_professor_periodo = length(nome)) %>%
            summarise(alunos_por_professor = mean(alunos_professor_periodo)) %>% 
            ggplot(aes(x = periodo, y = alunos_por_professor, group = 1)) + 
            geom_line(aes(text = paste0('Período: ', periodo, 
                                        '\nAlunos por professor: ', label_number(accuracy = 0.1, decimal.mark = ",")(alunos_por_professor))),
                      color = "#2266ac") + 
            labs(x="Período", y="Alunos por Professor") +
            theme_minimal() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          
          ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
    })
}

shinyApp(ui, server)
