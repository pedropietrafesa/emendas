# 1. Carregar a biblioteca necessária
install.packages("writexl")
library(dplyr)
library(readxl)
library(writexl)
library(tidyverse)


# Carregar dados 
#emendas <- read_excel("Downloads/PNA_LOA_2023.xlsx", sheet = "2026")
                           
emendas_2023 <- read_excel("Downloads/dados_emendas/emendas2023.xlsx")

e2026<- read_excel("Downloads/dados_emendas/b2d23fb5-8508-42c5-bb02-436fcf46dce9.xlsx")


# 2. Criar a nova tabela agrupada
tabela_consolidada <- emendas %>%
  group_by(Autor, `Emenda (Número/Ano)`, `Funcional`) %>%
  summarise(
    Autorizado_Total = sum(Autorizado, na.rm = TRUE),
    Empenhado_Total = sum(Empenhado, na.rm = TRUE),
    Executado_Total = sum(`Despesa Executada`, na.rm = TRUE),
    RP_Inscrito_Total = sum(`RP Inscrito`, na.rm = TRUE),
    Pago_Total = sum(`Pago (inclui RP)`, na.rm = TRUE)
  ) %>%
  ungroup()

# 3. Visualizar o resultado
print(tabela_consolidada)

# extrair o resultado para excel
write_xlsx(tabela_consolidada, "Resultado_Emendas.xlsx")

## Juntar informações 2024

# preparar dados

# 1. Preparamos a tabela2 para ter apenas valores ÚNICOS por emenda
e2026_unica <- e2026 %>%
  select(`Emenda (Número/Ano)`, `Ação + Subtítulo`) %>% # Pega só o necessário
  distinct(`Emenda (Número/Ano)`, .keep_all = TRUE)    # Mantém apenas a 1ª vez que a emenda aparece


tabela_final <- emendas_2026 %>%
  inner_join(e2026_unica %>% select(`Emenda (Número/Ano)`, `Ação + Subtítulo`), 
             by = "Emenda (Número/Ano)")

# extrair o resultado para excel
write_xlsx(tabela_final, "emendas2026.xlsx")


## Criar colunas sobre localidade da emenda

tabela <- emendas_2023 %>%
  mutate(
    # 1. Tenta extrair o padrão "MUNICÍPIO DE ... - UF"
    Extração = str_match(`Ação + Subtítulo`, "MUNICÍPIO DE (.*?) - ([A-Z]{2})$"),
    
    # 2. Cria as colunas específicas
    Municipio = Extração[,2],
    UF = Extração[,3]
  ) %>%
  # 3. Se não achou município, tenta pegar apenas a Localidade no final (ex: NACIONAL ou UF sozinha)
  mutate(
    Localidade_Geral = ifelse(is.na(UF), 
                              str_extract(`Ação + Subtítulo`, "(?<=-)[^-]+$") %>% str_trim(), 
                              NA)
  )


# extrair o resultado para excel
write_xlsx(tabela, "emendas2026_1.xlsx")

# Organizar a coluna de localidade


library(tidyverse)

# 1. Dicionário de tradução para quando houver apenas a sigla na coluna UF
mapa_ufs <- c(
  "AC"="Acre", "AL"="Alagoas", "AP"="Amapá", "AM"="Maxonas", "BA"="Bahia",
  "CE"="Ceará", "DF"="Distrito Federal", "ES"="Espírito Santo", "GO"="Goiás",
  "MA"="Maranhão", "MT"="Mato Grosso", "MS"="Mato Grosso do Sul",
  "MG"="Minas Gerais", "PA"="Pará", "PB"="Paraíba", "PR"="Paraná",
  "PE"="Pernambuco", "PI"="Piauí", "RJ"="Rio de Janeiro",
  "RN"="Rio Grande do Norte", "RS"="Rio Grande do Sul", "RO"="Rondônia",
  "RR"="Roraima", "SC"="Santa Catarina", "SP"="São Paulo", "SE"="Sergipe",
  "TO"="Tocantins"
)


# 2. Transformação dos dados
tabela_final <- tabela %>%
  mutate(
    # --- Passo A: Identificar o Estado Beneficiado ---
    Nome_Estado_Completo = case_when(
      # 1. Se Localidade_Geral for NACIONAL
      str_detect(toupper(Localidade_Geral), "NACIONAL") ~ "Nacional",
      
      # 2. Se houver sigla na coluna UF, usamos ela como prioridade
      !is.na(UF) & UF %in% names(mapa_ufs) ~ mapa_ufs[UF],
      
      # 3. Se UF falhar, extraímos o nome do estado de Localidade_Geral
      # Removemos os termos "NO ESTADO DA ", "NO ESTADO DE ", "NO ESTADO DO "
      TRUE ~ str_to_title(
        str_remove_all(Localidade_Geral, "NO ESTADO D[AEO]\\s+|NO DISTRITO FEDERAL") %>% 
          str_trim()
      )
    ),
    
    # --- Passo B: Coluna 1 (Município/Tipo) ---
    `Localidade Beneficiada (Município/Tipo)` = case_when(
      str_detect(toupper(Localidade_Geral), "NACIONAL") ~ "Nacional",
      # Se a coluna Município tiver algum valor
      !is.na(Municipio) & Municipio != "" ~ str_to_title(Municipio),
      # Se não, o beneficiário é o Estado como um todo
      TRUE ~ "Estado"
    ),
    
    # --- Passo C: Coluna 2 (Estado/UF) ---
    # Ajuste final para o Distrito Federal que aparece de forma específica
    `Localidade Beneficiada (Estado/UF)` = case_when(
      str_detect(toupper(Localidade_Geral), "DISTRITO FEDERAL") ~ "Distrito Federal",
      TRUE ~ Nome_Estado_Completo
    )
  ) %>%
  # Seleciona apenas as colunas solicitadas
  select(`Localidade Beneficiada (Município/Tipo)`, `Localidade Beneficiada (Estado/UF)`)

# Visualização do resultado corrigido
print(tabela_final)

# extrair o resultado para excel
write_xlsx(tabela_final, "emendas2023_2.xlsx")

