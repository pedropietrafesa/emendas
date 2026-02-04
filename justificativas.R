install.packages("pdftools")
library(pdftools)
library(tidyverse)
library(writexl) # Opcional: para salvar em Excel

# 1. Lista completa de emendas baseada no seu arquivo
emendas_alvo <- c(
  "12180017","12830012","13310016","13380004","13380007","13380015","13380016","13380017",
  "13390001","13390010","13620003","14030002","14030003","14030004","14030005","14030006",
  "14070006","14080015","14080017","14110009","16190001","18800008","19550003","19600003",
  "19600007","19830012","19830014","19830015","19860007","20830003","20980009","22630010",
  "22630012","23880002","23880003","24680011","24880005","25170010","25200010","25340005",
  "25340011","25340014","26160001","26740001","27000008","27000009","27000012","27180007",
  "27240005","27330012","27500001","27640019","27640021","27740001","27760004","27760025",
  "28180012","28260012","28330002","28550021","28620003","28620004","28620016","28850005",
  "29140012","29140013","29250005","29250006","29250010","29730011","29790005","30360004",
  "30360005","30360023","30410009","30540004","30880001","31600012","32280005","32620010",
  "35300015","36110019","36910007","37030011","37100006","37100017","37350008","37540014",
  "37630001","37630002","37660010","37770006","37960013","38010006","38050002","38860006",
  "38920015","38920018","38940002","38990007","39000006","39160019","39170012","39190001",
  "39250010","39440017","39780005","39830007","39840002","39840014","39870002","39870011",
  "39920001","39920005","39920009","40110005","40110006","40290007","40290012","40360009",
  "40360011","40380009","40390003","40440006","40540024","40580006","40630008","40630009",
  "40630011","40630014","40640010","40640019","40650005","40680001","40700001","40790009",
  "40800001","40800003","40800004","40800009","40820001","40910006","40920009","41090006",
  "41090014","41100005","41100014","41280013","41300016","41300017","41320002","41320010",
  "41400006","41410001","41410002","41420005","41430006","41440012","41510011","41510012",
  "41510013","41550005","41550006","41710004","41710015","41740007","41740008","41740012",
  "41750012","41750018","41780013","41840006","42180022","90480004","90600007"
)

# 2. Carregar o PDF
caminho_pdf <- "/Users/pedropietrafesa/Downloads/emendas/P_5445_AV_LOA.pdf"
paginas_pdf <- pdf_text(caminho_pdf)

# 3. Processar e criar o Banco de Dados
base_justificativas <- map_df(paginas_pdf, function(texto_da_pagina) {
  
  # Extrai o número da emenda (ajuste o padrão se necessário)
  # O padrão \\d{8} busca os 8 dígitos que aparecem no topo
  num_emenda <- str_extract(texto_da_pagina, "\\d{8}")
  
  # Captura o bloco da Justificativa
  # Busca tudo entre a palavra JUSTIFICATIVA e o rodapé AUTOR DA EMENDA
  texto_justificativa <- str_extract(texto_da_pagina, "(?<=JUSTIFICATIVA\\n)(.|\\n)*?(?=AUTOR DA EMENDA)")
  
  # Organiza em uma linha de tabela se a emenda estiver na sua lista
  if (!is.na(num_emenda) && num_emenda %in% emendas_alvo) {
    tibble(
      Numero_Emenda = num_emenda,
      Justificativa_Completa = str_squish(texto_justificativa) # str_squish limpa quebras de linha extras
    )
  } else {
    NULL # Ignora páginas que não são das emendas escolhidas
  }
})

# 4. Visualizar o "Banco de Dados" criado
print(base_justificativas)

# 5. Opcional: Salvar em Excel para abrir no computador
write_xlsx(base_justificativas, "justificativas_emendas.xlsx")


# 6. Colar no excel as emendas de acordo com a ordem que está na coluna com os número das emendas
# =SEERRO(PROCX(E2;'[justificativas_emendas.xlsx]Sheet1'!A:A;'[justificativas_emendas.xlsx]Sheet1'!B:B);"")

