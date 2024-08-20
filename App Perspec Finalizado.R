#################################
# Seção: Configurações Iniciais #
#################################

# Carregando bibliotecas a serem utilizadas
library(shiny)
library(tidyverse)
library(shinydashboard)
library(flexdashboard)
library(forcats)
library(randomForest)

# Baixando o banco de dados da web
download.file("https://www.gov.br/receitafederal/dados/arrecadacao-estado.csv",
              "arrecadacao_estado.csv", mode = "wb")
# Lendo o banco de dados baixado
df <- read.table("arrecadacao_estado.csv", sep=";", quote="\"")
# Setando os nomes das variáveis e excluindo eles da primeira linha do banco
nomes <- c("Ano", "Mes", "UF", "Imposto_Importacao", "Imposto_Exportacao", "IPI_Fumo",
           "IPI_Bebidas", "IPI_Automoveis", "IPI_Vinculado_Importacao", "IPI_Outros",
           "IRPF", "IRPJ_Entidades_Financeiras", "IRPJ_Demais_Empresas",
           "IRRF_Rendimentos_Trabalho", "IRRF_Rendimentos_Capital",
           "IRRF_Remessas_Exterior", "IRRF_Outros_Rendimentos",
           "Imposto_Operacoes_Financeiras", "Imposto_Territorial_Rural",
           "IPMF", "CPMF", "COFINS", "COFINS_Financeiras", "COFINS_Demais",
           "PIS_PASEP", "PIS_PASEP_Financeiras", "PIS_PASEP_Demais",
           "CSLL", "CSLL_Financeiras", "CSLL_Demais", "CIDE_Combustiveis_Nao_Dedutivel",
           "CIDE_Combustiveis", "Contribuicao_Plano_Seg_Soc_Servidores",
           "CPSSS", "Fundaf", "Refis", "Paes", "Retencao_Fonte_Lei_10833_Art_30",
           "Pagamento_Unificado", "Outras_Receitas_Administradas",
           "Demais_Receitas", "Receita_Previdenciaria",
           "Receita_Previdenciaria_Propria", "Receita_Previdenciaria_Demais",
           "Administradas_Por_Outros_Orgaos")
colnames(df) <- nomes
df_puro <- df[-c(1, nrow(df)),]
# Tratamento do banco para as aplicações do aplicativo
df_puro <- df_puro %>%
  # Corrigir caracteres Unicode
  mutate(Mes = iconv(Mes, from = "latin1", to = "UTF-8")) %>%
  # Substituir caracteres especiais
  mutate(Mes = gsub("MarÃ§o", "Marco", Mes, fixed = TRUE)) %>%
  # Substituir strings vazias por NA
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  # Remover vírgulas e pontos de todas as colunas exceto as três primeiras
  mutate(across(-c(2:3), ~ as.numeric(gsub("[.,]", "", .x)))) %>%
  # Unir variáveis sobre IRPJ em uma só
  mutate(IRPJ = IRPJ_Entidades_Financeiras + IRPJ_Demais_Empresas) %>%
  # Unir variáveis sobre IRRF em uma só
  mutate(IRRF = IRRF_Rendimentos_Trabalho + IRRF_Rendimentos_Capital +
           IRRF_Remessas_Exterior + IRRF_Outros_Rendimentos) %>%
  # Unir variáveis sobre COFINS em uma só
  mutate(COFINS_AUX = COFINS_Financeiras + COFINS_Demais) %>%
  mutate(COFINS = coalesce(COFINS_AUX, COFINS)) %>%
  # Unir variáveis sobre PIS_PASEP em uma só
  mutate(PIS_PASEP_AUX = PIS_PASEP_Financeiras + PIS_PASEP_Demais) %>%
  mutate(PIS_PASEP = coalesce(PIS_PASEP_AUX, PIS_PASEP)) %>%
  # Unir variáveis sobre CSLL em uma só
  mutate(CSLL_AUX = CSLL_Financeiras + CSLL_Demais) %>%
  mutate(CSLL = coalesce(CSLL_AUX, CSLL)) %>%
  # Unir variáveis sobre CIDE_Combustiveis em uma só
  mutate(CIDE_Combustiveis = coalesce(CIDE_Combustiveis, CIDE_Combustiveis_Nao_Dedutivel)) %>%
  # Unir variáveis sobre Receita_Previdenciaria em uma só
  mutate(Receita_Previdenciaria_aux = Receita_Previdenciaria_Propria + Receita_Previdenciaria_Demais) %>%
  mutate(Receita_Previdenciaria = coalesce(Receita_Previdenciaria_aux, Receita_Previdenciaria)) %>%
  # Selecionar apenas as variáveis que iremos utilizar
  select(-IRPJ_Entidades_Financeiras, -IRPJ_Demais_Empresas, -IRRF_Rendimentos_Trabalho,
         -IRRF_Rendimentos_Capital, -IRRF_Remessas_Exterior, -IRRF_Outros_Rendimentos,
         -IPMF, -CPMF, -COFINS_AUX, -COFINS_Financeiras, -COFINS_Demais, -PIS_PASEP_AUX,
         -PIS_PASEP_Financeiras, -PIS_PASEP_Demais, -CSLL_AUX, -CSLL_Financeiras,
         -CSLL_Demais, -CIDE_Combustiveis_Nao_Dedutivel, -Contribuicao_Plano_Seg_Soc_Servidores,
         -Fundaf, -Refis, -Paes, -Retencao_Fonte_Lei_10833_Art_30, -Pagamento_Unificado,
         -Outras_Receitas_Administradas, -Demais_Receitas, -Receita_Previdenciaria_Propria,
         -Receita_Previdenciaria_Demais, -Receita_Previdenciaria_aux, -Administradas_Por_Outros_Orgaos) %>% 
  # Pegar os dados até 2023 (somente anos completos)
  filter(Ano <= 2023)

# Banco de dados auxiliar filtrado para ajuste do modelo de floresta aleatória
dados_filtrados_modelos <- df_puro %>% group_by(Ano) %>%
  summarise(across(colnames(df_puro[,-c(1:3)]), sum, na.rm = TRUE)) %>% 
  # Adiciona colunas de lags 1 e 2 anos para cada imposto
  mutate(across(-Ano, ~ lag(.x, 1), .names = "{col}_ano_passado")) %>%
  mutate(across(-Ano, ~ lag(.x, 2), .names = "{col}_ano_retrasado")) %>% 
  # Remove linhas com NAs resultantes da criação das colunas de lags
  na.omit()



###############################
# Seção: Interface do Usuário #
###############################

# Definindo a interface do usuário com um layout de dashboard
ui <- dashboardPage(
  # Cabeçalho do dashboard
  dashboardHeader(title = "Arrecadação de Impostos no Brasil"),
  # Barra lateral do dashboard com o menu de navegação
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visão Geral da Pesquisa", tabName = "pesquisa", icon = icon("book")),
      menuItem("Análise Estadual", tabName = "estadual", icon = icon("map-location-dot")),
      menuItem("Análise por Impostos", tabName = "impostos", icon = icon("magnifying-glass-dollar")),
      menuItem("Importância de Variáveis", tabName = "previsoes", icon = icon("chart-line"))
      )
    ),
  # Corpo do dashboard com o conteúdo das abas
  dashboardBody(
    # Incluindo estilos e scripts customizados na página
    tags$head(
      tags$style(HTML('
        .main-header .logo {
          font-size: 18px;
          width: 320px;
        }
        .main-header .navbar {
          margin-left: 320px;
        }
        .custom-list li {
        font-size: 18px;
        }
        .explanation {
        font-size: 15px;
        }
        .lead {
        font-size: 16px;
        }
      ')),
      # Script JavaScript para fazer com que um botão clique automaticamente em outro
      tags$script(HTML("
      $(document).on('click', '#btn_pergunta1', function() {
        setTimeout(function() {
          $('#gerar_grafico1').click();
        }, 100);
      });
      $(document).on('click', '#btn_pergunta2', function() {
        setTimeout(function() {
          $('#gerar_grafico2').click();
        }, 100);
      });
      $(document).on('click', '#btn_pergunta3', function() {
        setTimeout(function() {
          $('#gerar_grafico3').click();
        }, 100);
      });
      $(document).on('click', '#btn_pergunta4', function() {
        setTimeout(function() {
          $('#gerar_grafico4').click();
        }, 100);
      });
      $(document).on('click', '#btn_pergunta5', function() {
        setTimeout(function() {
          $('#gerar_grafico5').click();
        }, 100);
      });
      $(document).on('click', '#btn_pergunta6', function() {
        setTimeout(function() {
          $('#gerar_grafico5').click();
        }, 100);
      });
    "))
      ),
    # Definindo o conteúdo de cada aba do dashboard
    tabItems(
      # Conteúdo aba 1
      tabItem(tabName = "pesquisa",
              fluidRow(
                column(width = 12, align = "center",
                       h2("Visão Geral da Pesquisa")
                       )
                ),
              fluidRow(
                column(width = 6,
                       h3(icon("pencil-alt"), "Autores:"),
                       # Informações sobre os autores com estilo customizado
                       div(style = "border: 2px solid #007bff; padding: 10px; margin-bottom: 10px; text-align: center;",
                           strong("Vinícius Morgado Rosa Vianna"), br(),
                           "Estudante de Graduação em Estatística"
                       ),
                       div(style = "border: 2px solid #007bff; padding: 10px; margin-bottom: 10px; text-align: center;",
                           strong("Pedro Lana Cordeiro"), br(),
                           "Estudante de Graduação em Estatística"
                       ),
                       div(style = "border: 2px solid #007bff; padding: 10px; text-align: center;",
                           strong("Murilo Rejani Franzotti"), br(),
                           "Estudante de Graduação em Estatística"
                       ),
                       br(),
                       h3(icon("database"), "Banco de Dados:"),
                       # Descrição do banco de dados utilizado
                       div(" Este banco de dados contém informações detalhadas sobre as receitas tributárias no Brasil, abrangendo um período extenso e variadas categorias de impostos e contribuições. As variáveis incluem o ano e o mês de referência, além das siglas das Unidades da Federação (UF) onde os tributos foram arrecadados. Os tributos registrados no banco de dados incluem impostos sobre importação e exportação, diferentes tipos de Imposto sobre Produtos Industrializados (IPI) aplicados a setores como fumo, bebidas, automóveis, e importações em geral, além de várias modalidades de Imposto de Renda (IR), incluindo para pessoas físicas, jurídicas, e retenções na fonte. Esses dados da arrecadação de impostos e contribuições federais são administrados pela Secretaria Especial da Receita Federal do Brasil (RFB).", style="font-size:16px"),
                       br(),
                       h3(icon("wrench"), "Metodologia:"),
                       # Descrição da metodologia utilizada no projeto
                       div(" O aplicativo foi exclusivamente desenvolvido por meio do software R, que é gratuito e possui diversas aplicações em Estatística e Ciência de Dados. O pacote ", tags$code("shiny"),
                           " e seus pacotes auxiliares como ", tags$code("shinydashboard"), " e ", tags$code("flexdashboard"), " foram utilizados para desenvolver o aplicativo em si, enquanto o ", tags$code("tidyverse"),
                           " e o ", tags$code("forcats"), " foram utilizados para limpar os dados e gerar as visualizações gráficas. Além disso, utilizamos o pacote ", tags$code("randomForest"), 
                           " para treinar os modelos de Florestas Aleatórias que foram utilizados.", style="font-size:16px"),
                       div("Consideramos adicionar botões para responder diretamente as perguntas de pesquisa que motivaram o trabalho, porém há uma gama muito grande de possibilidades de visualizações gráficas que o usuário pode explorar nas abas do aplicativo.", style="font-size:16px")
                       ),
                column(width = 6,
                       h3(icon("magnifying-glass"), "Perguntas de Pesquisa:"),
                       tags$ul(class = "custom-list",
                               # Lista de perguntas de pesquisa com explicações
                               tags$li("1 - Como a arrecadação de impostos federais variou entre os Estados do Brasil ao longo do tempo?"),
                               tags$p(class = "explanation", 
                                      "Visando entender o comportamento da arrecadação considerando todos os impostos, revelando padrões e tendências na distribuição dessa arrecadação pelos Estados."),
                               tags$li("2 - Qual é a participação do meu Estado na arrecadação total do Imposto de Renda da Pessoa Física em comparação com outros Estados?"),
                               tags$p(class = "explanation", 
                                      "Destacando este imposto que é um dos mais populares, buscando uma comparação da contribuição de cada Estado em relação à contribuição nacional."),
                               tags$li("3 - Como a sazonalidade afeta a arrecadação de impostos federais nos diferentes Estados?"),
                               tags$p(class = "explanation",
                                      "Analisando se existem padrões sazonais na arrecadação de impostos federais em diferentes Estados ao longo dos anos, identificando possíveis variações ao longo dos meses."),
                               tags$li("4 - Quais os impactos da introdução ou extinção de certos impostos na arrecadação total?"),
                               tags$p(class = "explanation", 
                                      "Buscando entender como um ou mais impostos contribuem para a arrecadação considerando todos os impostos."),
                               tags$li("5 - Como a arrecadação de impostos que afetam diretamente os consumidores, como o IPI sobre automóveis, bebidas, etc, variou nos últimos anos?"),
                               tags$p(class = "explanation", 
                                      "Destacando especialmente os impostos que afetam os consumidores, mas também podendo expandir para qualquer imposto em estudo."),
                               tags$li("6 - Qual o comportamento histórico das arrecadações de IRPF e IRPJ?"),
                               tags$p(class = "explanation",
                                      "Procurando entender esse comportamento para ter um indicativo de quais as tendências dos impostos entre Pessoa Física e Pessoa Jurídica para uma posterior ação no mercado de trabalho."),
                               tags$li("7 - Se quisermos prever a arrecadação anual de algum imposto específico, quais os impostos são mais importantes para essa previsão?"),
                               tags$p(class = "explanation",
                                      "Visando obter quais impostos são mais importantes em um modelo de previsão com um imposto selecionado como variável resposta.")
                               ),
                       )
                )
              ),
      # Conteúdo aba 2
      tabItem(tabName = "estadual",
              fluidPage(
                titlePanel("Análise Estadual"),
                sidebarLayout(
                  sidebarPanel(
                    # Input de seleção de múltiplos estados
                    checkboxGroupInput(inputId = "estados", 
                                       label = "Selecione os Estados que você deseja ver:",
                                       choices = c("Acre" = "AC", "Alagoas" = "AL", "Amapá" = "AP", "Amazonas" = "AM", "Bahia" = "BA", 
                                                   "Ceará" = "CE", "Distrito Federal" = "DF", "Espírito Santo" = "ES", "Goiás" = "GO", 
                                                   "Maranhão" = "MA", "Mato Grosso" = "MT", "Mato Grosso do Sul" = "MS", "Minas Gerais" = "MG", 
                                                   "Pará" = "PA", "Paraíba" = "PB", "Paraná" = "PR", "Pernambuco" = "PE", "Piauí" = "PI", 
                                                   "Rio de Janeiro" = "RJ", "Rio Grande do Norte" = "RN", "Rio Grande do Sul" = "RS", 
                                                   "Rondônia" = "RO", "Roraima" = "RR", "Santa Catarina" = "SC", "São Paulo" = "SP", 
                                                   "Sergipe" = "SE", "Tocantins" = "TO"),
                                       # Estados pré selecionados
                                       selected = c("PA", "BA", "DF", "SP", "RS"))
                    ),
                  mainPanel(
                    # Painel com difetentes abas
                    tabsetPanel(
                      # Aba 1
                      tabPanel("Arrecadação Total",
                               br(),
                               # Botão para gerar gráfico para a pergunta 1
                               actionButton("btn_pergunta1", "Gráfico para a pergunta 1"),
                               br(),
                               # Botão para gerar gráfico livre
                               actionButton("gerar_grafico1", "Gerar visualização gráfica com base nos Estados selecionados"),
                               br(), br(),
                               # Output gráfico
                               plotOutput("graf_arrecad_total")
                               ),
                      # Aba 2
                      tabPanel("Imposto de Renda de Pessoa Física",
                               br(),
                               # Botão para gerar gráfico para a pergunta 2
                               actionButton("btn_pergunta2", "Gráfico para a pergunta 2"),
                               br(),
                               # Botão para gerar gráfico livre
                               actionButton("gerar_grafico2", "Gerar visualização gráfica com base nos Estados selecionados"),
                               br(), br(),
                               # Output gráfico
                               plotOutput("graf_irpf")
                               ),
                      # Aba 3
                      tabPanel("Sazonalidade",
                               br(),
                               # Botão para gerar gráfico para a pergunta 3
                               actionButton("btn_pergunta3", "Gráfico para a pergunta 3"),
                               br(),
                               # Botão para gerar gráfico livre
                               actionButton("gerar_grafico3", "Gerar visualização gráfica com base nos Estados selecionados"),
                               br(), br(),
                               # Output gráfico
                               plotOutput("graf_sazon")
                               )
                      ),
                    )
                  )
                )
              ),
      # Conteúdo aba 3
      tabItem(tabName = "impostos",
              fluidPage(
                titlePanel("Análise por Impostos"),
                sidebarLayout(
                  sidebarPanel(
                    # Input de seleção de múltiplos impostos
                    checkboxGroupInput(inputId = "impostos", 
                                       label = "Selecione os impostos que você deseja ver:",
                                       choices = c("Imposto sobre Importação" = "Imposto_Importacao", 
                                                   "Imposto sobre Exportação" = "Imposto_Exportacao", 
                                                   "Imposto sobre Produtos Industrializados (IPI) - Fumo" = "IPI_Fumo", 
                                                   "Imposto sobre Produtos Industrializados (IPI) - Bebidas" = "IPI_Bebidas", 
                                                   "Imposto sobre Produtos Industrializados (IPI) - Automóveis" = "IPI_Automoveis", 
                                                   "Imposto sobre Produtos Industrializados (IPI) - Vinculado à Importação" = "IPI_Vinculado_Importacao", 
                                                   "Imposto sobre Produtos Industrializados (IPI) - Outros" = "IPI_Outros", 
                                                   "Imposto de Renda Pessoa Física (IRPF)" = "IRPF", 
                                                   "Imposto de Renda Pessoa Jurídica (IRPJ)" = "IRPJ", 
                                                   "Imposto de Renda Retido na Fonte (IRRF)" = "IRRF", 
                                                   "Imposto sobre Operações Financeiras" = "Imposto_Operacoes_Financeiras", 
                                                   "Imposto Territorial Rural" = "Imposto_Territorial_Rural", 
                                                   "Contribuição para o Financiamento da Seguridade Social (COFINS)" = "COFINS", 
                                                   "Programa de Integração Social e de Formação do Patrimônio do Servidor Público (PIS/PASEP)" = "PIS_PASEP", 
                                                   "Contribuição Social sobre o Lucro Líquido (CSLL)" = "CSLL", 
                                                   "Contribuição de Intervenção no Domínio Econômico (CIDE) - Combustíveis" = "CIDE_Combustiveis", 
                                                   "Contribuição para o Plano de Seguridade Social do Servidor Público (CPSSS)" = "CPSSS", 
                                                   "Receita Previdenciária" = "Receita_Previdenciaria"),
                                       # Impostos pré selecionados
                                       selected = c("IPI_Fumo", "IPI_Bebidas", "IPI_Automoveis", "IPI_Vinculado_Importacao", "IPI_Outros")),
                    ),
                  mainPanel(
                    # Painel com diferentes abas
                    tabsetPanel(
                      # Aba 1
                      tabPanel("Arrecadação Total",
                               br(),
                               # Botão para gerar gráfico para a pergunta 4
                               actionButton("btn_pergunta4", "Gráfico para a pergunta 4"),
                               br(),
                               # Botão para gerar gráfico livre
                               actionButton("gerar_grafico4", "Gerar visualização gráfica com base nos impostos selecionados"),
                               br(), br(),
                               # Output gráfico
                               plotOutput("graf_impostos_total"),
                               br(),
                               # Output de texto
                               div(textOutput("porcentagem"), style="font-size:18px")
                               ),
                      # Aba 2
                      tabPanel("Curvas de Arrecadação",
                               br(),
                               # Botão para gerar gráfico para a pergunta 5
                               actionButton("btn_pergunta5", "Gráfico para a pergunta 5"),
                               br(),
                               # Botão para gerar gráfico para a pergunta 6
                               actionButton("btn_pergunta6", "Gráfico para a pergunta 6"),
                               br(),
                               # Botão para gerar gráfico livre
                               actionButton("gerar_grafico5", "Gerar visualização gráfica com base nos impostos selecionados"),
                               br(), br(),
                               # Output gráfico
                               plotOutput("graf_curvas"),
                               br()
                               )
                      )
                    )
                  )
                )
              ),
      # Conteúdo aba 4
      tabItem(tabName = "previsoes",
              fluidPage(
                titlePanel("Importância de Variáveis via Modelos de Previsão"),
                sidebarLayout(
                  sidebarPanel(
                    # Input de seleção única do imposto (variável resposta)
                    radioButtons(inputId = "imposto", 
                                 label = "Selecione o imposto que você deseja estudar como variável resposta:",
                                 choices = c("Imposto sobre Importação" = "Imposto_Importacao", 
                                             "Imposto sobre Exportação" = "Imposto_Exportacao", 
                                             "Imposto sobre Produtos Industrializados (IPI) - Fumo" = "IPI_Fumo", 
                                             "Imposto sobre Produtos Industrializados (IPI) - Bebidas" = "IPI_Bebidas", 
                                             "Imposto sobre Produtos Industrializados (IPI) - Automóveis" = "IPI_Automoveis", 
                                             "Imposto sobre Produtos Industrializados (IPI) - Vinculado à Importação" = "IPI_Vinculado_Importacao", 
                                             "Imposto sobre Produtos Industrializados (IPI) - Outros" = "IPI_Outros", 
                                             "Imposto de Renda Pessoa Física (IRPF)" = "IRPF", 
                                             "Imposto de Renda Pessoa Jurídica (IRPJ)" = "IRPJ", 
                                             "Imposto de Renda Retido na Fonte (IRRF)" = "IRRF", 
                                             "Imposto sobre Operações Financeiras" = "Imposto_Operacoes_Financeiras", 
                                             "Imposto Territorial Rural" = "Imposto_Territorial_Rural", 
                                             "Contribuição para o Financiamento da Seguridade Social (COFINS)" = "COFINS", 
                                             "Programa de Integração Social e de Formação do Patrimônio do Servidor Público (PIS/PASEP)" = "PIS_PASEP", 
                                             "Contribuição Social sobre o Lucro Líquido (CSLL)" = "CSLL", 
                                             "Contribuição de Intervenção no Domínio Econômico (CIDE) - Combustíveis" = "CIDE_Combustiveis", 
                                             "Contribuição para o Plano de Seguridade Social do Servidor Público (CPSSS)" = "CPSSS", 
                                             "Receita Previdenciária" = "Receita_Previdenciaria"),
                                 # Imposto pré selecionado
                                 selected = c("IRPF")),
                    ),
                  mainPanel(
                    # Output de texto
                    div(textOutput("import"), style="font-size:18px"),
                    br(),
                    # Output gráfico
                    plotOutput("graf_import")
                    )
                  )
                )
              )
      )
    )
  )



###################
# Seção: Servidor #
###################

# Definindo o servidor com todos os processos backend
server <- function(input, output, session) {
  ### ABA 2 - GRÁFICO 1 - PERGUNTA 1
  # Após clique do botão da pergunta 1, atualiza a seleção de estados no input
  observeEvent(input$btn_pergunta1, {
    updateCheckboxGroupInput(session, "estados",
                             selected = c("PA", "BA", "DF", "SP", "RS"))
  })
  # Filtra e agrupa os dados para calcular a arrecadação total por estado e ano
  dados_filtrados_arrecad_total <- eventReactive(input$gerar_grafico1,
    df_puro %>% filter(UF %in% input$estados) %>% 
      group_by(Ano, UF) %>%
      summarise(arrecadacao_total = sum(across(colnames(df_puro[,-c(1:3)]), sum, na.rm = TRUE)))
  )
  # Renderiza o output gráfico
  output$graf_arrecad_total <- renderPlot({
    ggplot(dados_filtrados_arrecad_total(), aes(x = Ano, y = arrecadacao_total, color = UF, group = UF)) +
      geom_line() + geom_point(size = 2) +
      labs(title = paste("Arrecadação total de Impostos Federais (em R$) ","\n",
                         "por Estado ao longo dos anos"),
           x = "", y = "") +
      scale_color_discrete(name = "Estado") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 16)) +
      theme(axis.text = element_text(size = 13),
            title = element_text(size = 15),
            legend.text = element_text(size = 13),
            legend.position = "top",
            axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  ### ABA 2 - GRÁFICO 2 - PERGUNTA 2
  # Após clique do botão da pergunta 2, atualiza a seleção de estados no input
  observeEvent(input$btn_pergunta2, {
    updateCheckboxGroupInput(session, "estados",
                             selected = c("PA", "BA", "DF", "SP", "RS"))
  })
  # Filtra e calcula a participação percentual do IRPF por estado
  dados_filtrados_arrecad_irpf <- eventReactive(input$gerar_grafico2,
    df_puro %>% group_by(UF) %>% 
      summarise(total_irpf = sum(IRPF, na.rm = TRUE)) %>%
      mutate(participacao_percentual = (total_irpf / sum(total_irpf)) * 100) %>% 
      filter(UF %in% input$estados)
  )
  # Renderiza o output gráfico
  output$graf_irpf <- renderPlot({
    ggplot(dados_filtrados_arrecad_irpf(), aes(x = fct_reorder(UF, participacao_percentual, .desc = TRUE),
                                               y = participacao_percentual)) +
      geom_bar(stat = "identity", fill = "blue") +
      labs(title = paste("Participação percentual na arrecadação de IRPF por Estado ", "\n",
                         "em relação ao total nacional, 01/2000 a 12/2023"),
           x = "", y = "") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size=13),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            title = element_text(size = 15)) +
      geom_text(aes(y=participacao_percentual + 0.2,
                    label=paste0(round(participacao_percentual, 2), "%")),
                size = 5)
  })
  
  ### ABA 2 - GRÁFICO 3 - PERGUNTA 3
  # Após clique do botão da pergunta 3, atualiza a seleção de estados no input
  observeEvent(input$btn_pergunta3, {
    updateCheckboxGroupInput(session, "estados",
                             selected = c("PA", "BA", "DF", "SP", "RS"))
  })
  # Filtra e calcula a arrecadação total por estado e mês
  dados_filtrados_sazon <- eventReactive(input$gerar_grafico3,
    df_puro %>% filter(UF %in% input$estados) %>%
      group_by(Mes, UF) %>%
      mutate(Mes = factor(Mes, levels = c("Janeiro", "Fevereiro", "Março", "Abril", 
                                          "Maio", "Junho", "Julho", "Agosto", 
                                          "Setembro", "Outubro", "Novembro", "Dezembro"), ordered = TRUE)) %>%  
      summarise(arrecadacao_total = sum(across(colnames(df_puro[,-c(1:3)]), sum, na.rm = TRUE)))
  )
  # Renderiza o output gráfico
  output$graf_sazon <- renderPlot({
    ggplot(dados_filtrados_sazon(), aes(x = Mes, y = arrecadacao_total, color = UF, group = UF)) +
      geom_line() + geom_point(size = 2) +
      labs(title = paste("Arrecadação total de Impostos Federais (em R$) por ","\n",
                         "Estado ao longo dos meses, 01/2000 a 12/2023"),
           x = "", y = "") +
      scale_color_discrete(name = "Estado") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
      theme(axis.text = element_text(size = 13),
            title = element_text(size = 15),
            legend.text = element_text(size = 13),
            legend.position = "top",
            axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  ### ABA 3 - GRÁFICO 1 - PERGUNTA 4
  # Após clique do botão da pergunta 4, atualiza a seleção de impostos no input
  observeEvent(input$btn_pergunta4, {
    updateCheckboxGroupInput(session, "impostos",
                             selected = c("IRPF", "IRPJ", "IRRF"))
  })
  # Filtra e calcula a arrecadação total e filtrada para os impostos selecionados
  dados_filtrados_arrecad_impostos <- eventReactive(input$gerar_grafico4,
    df_puro %>% group_by(Ano) %>%
      summarise(total_completo = sum(across(colnames(df_puro[,-c(1:3)]), sum, na.rm = TRUE)),
                total_filtrado = sum(across(all_of(input$impostos), sum, na.rm = TRUE))) %>% 
      mutate(porcentagem = (sum(total_filtrado) / sum(total_completo)) * 100)
  )
  # Renderiza o output gráfico
  output$graf_impostos_total <- renderPlot({
    ggplot(dados_filtrados_arrecad_impostos()) +
      geom_line(aes(x = Ano, y = total_completo, color = "Arrecadação Total", group = 1)) +
      geom_point(aes(x = Ano, y = total_completo, color = "Arrecadação Total"), size = 2) +
      geom_line(aes(x = Ano, y = total_filtrado, color = "Arrecadação Filtrada", group = 1)) +
      geom_point(aes(x = Ano, y = total_filtrado, color = "Arrecadação Filtrada"), size = 2) +
      labs(title = paste("Comparação de arrecadação total e filtrada (em R$) ","\n",
                         "por alguns impostos ao longo dos anos"),
           x = "", y = "") +
      scale_color_manual(values = c("Arrecadação Total" = "blue", "Arrecadação Filtrada" = "red")) +
      scale_color_discrete(name = "Curvas") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 16)) +
      theme(axis.text = element_text(size = 13),
            title = element_text(size = 15),
            legend.text = element_text(size = 13),
            legend.position = "top",
            axis.text.x = element_text(angle = 90, hjust = 1))
  })
  # Renderiza o output de texto
  output$porcentagem <- renderText({
    paste0("A arrecadação dos impostos selecionados representa ", 
           round(dados_filtrados_arrecad_impostos()$porcentagem[1], 2), 
           "% da arrecadação total no período em análise.")
  })
  
  ### ABA 3 - GRÁFICO 2 - PERGUNTA 5
  # Após clique do botão da pergunta 5, atualiza a seleção de impostos no input
  observeEvent(input$btn_pergunta5, {
    updateCheckboxGroupInput(session, "impostos",
                             selected = c("IPI_Fumo", "IPI_Bebidas", "IPI_Automoveis", "IPI_Vinculado_Importacao", "IPI_Outros"))
  })
  # Filtra e calcula a arrecadação total por tipo de imposto
  dados_filtrados_curv_impostos <- eventReactive(input$gerar_grafico5,
    df_puro %>%
      select(Ano, all_of(input$impostos)) %>% group_by(Ano) %>%
      summarise(across(all_of(input$impostos), sum, na.rm = TRUE), .groups = 'drop') %>%
      pivot_longer(cols = all_of(input$impostos), names_to = "Imposto", values_to = "Arrecadacao") %>% 
      mutate(Ano = as.numeric(Ano))
  )
  # Renderiza o output gráfico
  output$graf_curvas <- renderPlot({
    ggplot(dados_filtrados_curv_impostos(), aes(x = Ano, y = Arrecadacao, color = Imposto)) +
      geom_line() + geom_point(size = 2) +
      labs(title = "Arrecadação (em R$) por imposto ao longo dos anos",
           x = "", y = "") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 16)) +
      theme(axis.text = element_text(size = 13),
            title = element_text(size = 15),
            legend.text = element_text(size = 13),
            legend.position = "top",
            axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  ### ABA 3 - GRÁFICO 2 - PERGUNTA 6
  # Após clique do botão da pergunta 6, atualiza a seleção de impostos no input
  observeEvent(input$btn_pergunta6, {
    updateCheckboxGroupInput(session, "impostos",
                             selected = c("IRPF", "IRPJ"))
  })
  
  ### ABA 4 - IMPORTANCIA
  modelo_forest <- reactive({
    # Semente para reprodutibilidade
    set.seed(123)
    # Identifica as covariaveis excluindo a resposta selecionada (imposto)
    covariates <- setdiff(names(dados_filtrados_modelos), c("Ano", input$imposto))
    # Define o parametro tunado para a floresta
    tuning.forest <- round(length(covariates)/3)
    # Ajusta o modelo de floresta com o parametro tunado
    forest <- randomForest(as.formula(paste(input$imposto, "~ .")), mtry = tuning.forest, importance = TRUE,
                 data = dados_filtrados_modelos[, c(covariates, input$imposto)])
    # Dataframe com todas as importancias de cada covariável do modelo
    importancia <- data.frame(Variable = rownames(importance(forest)),
                              IncNodePurity = importance(forest)[, "IncNodePurity"])
    # Selecionando as 5 maiores importancias
    importancia %>% 
      arrange(desc(IncNodePurity)) %>%
      head(5)
  })
  # Renderiza o output de texto
  output$import <- renderText({
    paste0("O modelo de Floresta Aleatória ajustado indica que os seguintes impostos são os mais importantes para a previsão da variável resposta ", 
           input$imposto, ":")
  })
  # Renderiza o output gráfico
  output$graf_import <- renderPlot({
    ggplot(modelo_forest(), aes(x = reorder(Variable, IncNodePurity), y = IncNodePurity)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      coord_flip() +
      labs(title = paste("Variáveis mais importantes para o modelo com ", "\n",
                         "a variável resposta", input$imposto),
           x = "", y = "") +
      theme(title = element_text(size = 15),
            axis.text = element_text(size = 13))
  })
}



#################################
# Seção: Execução do Aplicativo #
#################################

shinyApp(ui = ui, server = server)

