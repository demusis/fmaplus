library(readr)
library(lubridate)
library(openxlsx)

# Carga de dados
inmet <- read_delim("inmet Campo Verde_A912_2019.csv", 
                    delim = ";",
                    escape_double = FALSE, 
                    trim_ws = TRUE)

colnames(inmet)[19] <- 'P'
inmet$P <- as.numeric(gsub(",", ".", inmet$P))

colnames(inmet)[6] <- 'UR'
inmet$UR <- inmet$UR/10

colnames(inmet)[15] <- 'v'
inmet$v <- as.numeric(gsub(",", ".", inmet$v))

colnames(inmet)[2] <- 'hora'

# Exclui NAs
inmet <- inmet[!is.na(inmet$UR),]

# Converte texto para data 
inmet$Data <- as.Date(inmet$Data, format = "%d/%m/%Y")

# Extrai o mês e ano
inmet$mes <- month(inmet$Data)
inmet$ano <- year(inmet$Data)
inmet$dia <- day(inmet$Data)

# Seleciona dados para 13:00
inmet13 <- inmet[inmet$hora == '1300',]     
inmet13 <- inmet13[, c('dia', 'mes', 'ano', 'UR', 'P', 'v')]

# Agrupa dados por precipitação
inmetDia <- aggregate(P ~ ano + mes + dia, data = inmet13, FUN = sum)
inmet13 <- inmet13[, c('dia', 'mes', 'ano', 'UR', 'v')]

# Mesclar os dois dataframes
inmetMesc <- merge(inmet13, inmetDia, by = c('dia', 'mes', 'ano'))
inmetMesc <- inmetMesc[order(inmetMesc$ano, inmetMesc$mes, inmetMesc$dia),]

# Fator de propagação
fp <-function(v) {
  return (exp(0.04*v))
}

# Calcula o FMA
inmetMesc$FMA <- NA

for (i in 1:nrow(inmetMesc)) {
  # Primeiro Registro
  if (i == 1) {
    if (inmetMesc[i, 'P'] < 2.5) {
      inmetMesc[i, 'FMA'] <- 100/inmetMesc[i, 'UR']*fp(inmetMesc[i, 'v'])
    } else {
      if (inmetMesc[i, 'P'] > 12.9) {
        inmetMesc[i, 'FMA'] <- 0
      } else {
        inmetMesc[i, 'FMA'] <- NA
      }
    }
  } else {
    if (inmetMesc[i, 'P'] < 2.5) {
      inmetMesc[i, 'FMA'] <- 100/inmetMesc[i, 'UR']*fp(inmetMesc[i, 'v'] + inmetMesc[i-1, 'FMA'])
    } else {
      if (inmetMesc[i, 'P'] < 5.0) {
        inmetMesc[i, 'FMA'] <- 100/inmetMesc[i, 'UR']*fp(inmetMesc[i, 'v'] + .70*inmetMesc[i-1, 'FMA'])
      } else {
        if (inmetMesc[i, 'P'] < 10.0) {
          inmetMesc[i, 'FMA'] <- 100/inmetMesc[i, 'UR']*fp(inmetMesc[i, 'v'] + .40*inmetMesc[i-1, 'FMA'])
        } else {
          if (inmetMesc[i, 'P'] < 13.0) {
            inmetMesc[i, 'FMA'] <- 100/inmetMesc[i, 'UR']*fp(inmetMesc[i, 'v'] + 20 * inmetMesc[i-1, 'FMA'])
          } else {
            inmetMesc[i, 'FMA'] <- 0
          }
        }
      }
    }
  }
}

# Cria um objeto workbook
workbook <- createWorkbook()

# Adiciona uma nova planilha ao workbook
addWorksheet(workbook, "FMA+")

# Escreve o dataframe na planilha
writeData(workbook, "FMA+", inmetMesc)

# Salva o arquivo
saveWorkbook(workbook, "Resultados.xlsx", overwrite = TRUE)
