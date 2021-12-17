#' @name readFiles
#' @description
#' @param
#' @details
#' @export

dir <- "C:/Users/pedrosa/github/tomtom"

readFiles <- function(dir){

  files <- list.files(dir)
  files_table <- grep("{2}[0-9].dbf",files, value = T)
  files_shape <- grep("network.dbf",files, value = T)

  dbf_table <- list()
  for (i in files_table) {
    dbf_table[[i]] <- read.dbf(file.path(dir,i))
  }
  dbf_network <- read.dbf(file.path(dir,files_shape))
}




  library(foreign)
  library(dplyr)

  network <- read.dbf(file.path(dir,"network.dbf"))
  t_hora0 <- read.dbf(file.path(dir,"2020-01-10 to 2020-01-10_0_00-1_00.dbf"))
  t_hora1 <- read.dbf(file.path(dir,"2020-01-10 to 2020-01-10_1_00-2_00.dbf"))
  t_hora2 <- read.dbf("2.dbf")
  t_hora3 <- read.dbf("3.dbf")
  t_hora4 <- read.dbf("4.dbf")
  t_hora5 <- read.dbf("5.dbf")
  t_hora6 <- read.dbf("6.dbf")
  t_hora7 <- read.dbf("7.dbf")
  t_hora8 <- read.dbf("8.dbf")
  t_hora9 <- read.dbf("9.dbf")
  t_hora10 <- read.dbf("10.dbf")
  t_hora11 <- read.dbf("11.dbf")
  t_hora12 <- read.dbf("12.dbf")
  t_hora13 <- read.dbf("13.dbf")
  t_hora14 <- read.dbf("14.dbf")
  t_hora15 <- read.dbf("15.dbf")
  t_hora16 <- read.dbf("16.dbf")
  t_hora17 <- read.dbf("17.dbf")
  t_hora18 <- read.dbf("18.dbf")
  t_hora19 <- read.dbf("19.dbf")
  t_hora20 <- read.dbf("20.dbf")
  t_hora21 <- read.dbf("21.dbf")
  t_hora22 <- read.dbf("22.dbf")
  t_hora23 <- read.dbf("23.dbf")

  ##################################################################################################################################################
  colnames(t_hora0) <- c("Id", 2, 3, 4, "AvgSpeed", 6, 7, 8, "Hits", 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  t_hora0 <- select(t_hora0, -2, -3, -4, -6, -7, -8, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28)
  t_hora0 <- merge(t_hora0, network, by.x = "Id", by.y = "Id")
  t_hora0 <- select(t_hora0, -"FRC", -"SpeedLimit", -"StreetName")
  t_hora0$flux_veic <- t_hora0$Hits * 3.1293
 adsf

  ###################################################################################################################################################

  colnames(t_hora1) <- c("Id", 2, 3, 4, "AvgSpeed", 6, 7, 8, "Hits", 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  t_hora1 <- select(t_hora1, -2, -3, -4, -6, -7, -8, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28)
  t_hora1 <- merge(t_hora1, network, by.x = "Id", by.y = "Id")
  t_hora1 <- select(t_hora1, -"FRC", -"SpeedLimit", -"StreetName")
  t_hora1$flux_veic <- t_hora1$Hits * 3.1293
  t_hora1 <- t_hora1 %>% mutate(emiss_factor = ifelse(AvgSpeed < 50, 0.0928*AvgSpeed^2-9.2601*AvgSpeed+358.7,
                                                      ifelse(AvgSpeed >= 80, 0.0165*AvgSpeed^2-2.3481*AvgSpeed+211.68,130)))
  t_hora1 <- t_hora1 %>% mutate(emission = Length/1000 * flux_veic * emiss_factor * 0.1/100/0.1585)
  write.dbf(t_hora1, "t_hora1")
  #####################################################################################################################################################

  colnames(t_hora2) <- c("Id", 2, 3, 4, "AvgSpeed", 6, 7, 8, "Hits", 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  t_hora2 <- select(t_hora2, -2, -3, -4, -6, -7, -8, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28)
  t_hora2 <- merge(t_hora2, network, by.x = "Id", by.y = "Id")
  t_hora2 <- select(t_hora2, -"FRC", -"SpeedLimit", -"StreetName")
  t_hora2$flux_veic <- t_hora2$Hits * 3.1293
  t_hora2 <- t_hora2 %>% mutate(emiss_factor = ifelse(AvgSpeed < 50, 0.0928*AvgSpeed^2-9.2601*AvgSpeed+358.7,
                                                      ifelse(AvgSpeed >= 80, 0.0165*AvgSpeed^2-2.3481*AvgSpeed+211.68,130)))
  t_hora2 <- t_hora2 %>% mutate(emission = Length/1000 * flux_veic * emiss_factor * 0.1/100/0.1585)
  write.dbf(t_hora2, "t_hora2")
  ###################################################################################################################################################

  colnames(t_hora3) <- c("Id", 2, 3, 4, "AvgSpeed", 6, 7, 8, "Hits", 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  t_hora3 <- select(t_hora3, -2, -3, -4, -6, -7, -8, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28)
  t_hora3 <- merge(t_hora3, network, by.x = "Id", by.y = "Id")
  t_hora3 <- select(t_hora3, -"FRC", -"SpeedLimit", -"StreetName")
  t_hora3$flux_veic <- t_hora3$Hits * 3.1293
  t_hora3 <- t_hora3 %>% mutate(emiss_factor = ifelse(AvgSpeed < 50, 0.0928*AvgSpeed^2-9.2601*AvgSpeed+358.7,
                                                      ifelse(AvgSpeed >= 80, 0.0165*AvgSpeed^2-2.3481*AvgSpeed+211.68,130)))
  t_hora3 <- t_hora3 %>% mutate(emission = Length/1000 * flux_veic * emiss_factor * 0.1/100/0.1585)
  write.dbf(t_hora3, "t_hora3")
  ###################################################################################################################################################

  colnames(t_hora4) <- c("Id", 2, 3, 4, "AvgSpeed", 6, 7, 8, "Hits", 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  t_hora4 <- select(t_hora4, -2, -3, -4, -6, -7, -8, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28)
  t_hora4 <- merge(t_hora4, network, by.x = "Id", by.y = "Id")
  t_hora4 <- select(t_hora4, -"FRC", -"SpeedLimit", -"StreetName")
  t_hora4$flux_veic <- t_hora4$Hits * 3.1293
  t_hora4 <- t_hora4 %>% mutate(emiss_factor = ifelse(AvgSpeed < 50, 0.0928*AvgSpeed^2-9.2601*AvgSpeed+358.7,
                                                      ifelse(AvgSpeed >= 80, 0.0165*AvgSpeed^2-2.3481*AvgSpeed+211.68,130)))
  t_hora4 <- t_hora4 %>% mutate(emission = Length/1000 * flux_veic * emiss_factor * 0.1/100/0.1585)
  write.dbf(t_hora4, "t_hora4")
  ###################################################################################################################################################

  colnames(t_hora5) <- c("Id", 2, 3, 4, "AvgSpeed", 6, 7, 8, "Hits", 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  t_hora5 <- select(t_hora5, -2, -3, -4, -6, -7, -8, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28)
  t_hora5 <- merge(t_hora5, network, by.x = "Id", by.y = "Id")
  t_hora5 <- select(t_hora5, -"FRC", -"SpeedLimit", -"StreetName")
  t_hora5$flux_veic <- t_hora5$Hits * 3.1293
  t_hora5 <- t_hora5 %>% mutate(emiss_factor = ifelse(AvgSpeed < 50, 0.0928*AvgSpeed^2-9.2601*AvgSpeed+358.7,
                                                      ifelse(AvgSpeed >= 80, 0.0165*AvgSpeed^2-2.3481*AvgSpeed+211.68,130)))
  t_hora5 <- t_hora5 %>% mutate(emission = Length/1000 * flux_veic * emiss_factor * 0.1/100/0.1585)
  write.dbf(t_hora5, "t_hora5")
  ###################################################################################################################################################

  colnames(t_hora6) <- c("Id", 2, 3, 4, "AvgSpeed", 6, 7, 8, "Hits", 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  t_hora6 <- select(t_hora6, -2, -3, -4, -6, -7, -8, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28)
  t_hora6 <- merge(t_hora6, network, by.x = "Id", by.y = "Id")
  t_hora6 <- select(t_hora6, -"FRC", -"SpeedLimit", -"StreetName")
  t_hora6$flux_veic <- t_hora6$Hits * 3.1293
  t_hora6 <- t_hora6 %>% mutate(emiss_factor = ifelse(AvgSpeed < 50, 0.0928*AvgSpeed^2-9.2601*AvgSpeed+358.7,
                                                      ifelse(AvgSpeed >= 80, 0.0165*AvgSpeed^2-2.3481*AvgSpeed+211.68,130)))
  t_hora6 <- t_hora6 %>% mutate(emission = Length/1000 * flux_veic * emiss_factor * 0.1/100/0.1585)
  write.dbf(t_hora6, "t_hora6")
  ###################################################################################################################################################

  colnames(t_hora7) <- c("Id", 2, 3, 4, "AvgSpeed", 6, 7, 8, "Hits", 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  t_hora7 <- select(t_hora7, -2, -3, -4, -6, -7, -8, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28)
  t_hora7 <- merge(t_hora7, network, by.x = "Id", by.y = "Id")
  t_hora7 <- select(t_hora7, -"FRC", -"SpeedLimit", -"StreetName")
  t_hora7$flux_veic <- t_hora7$Hits * 3.1293
  t_hora7 <- t_hora7 %>% mutate(emiss_factor = ifelse(AvgSpeed < 50, 0.0928*AvgSpeed^2-9.2601*AvgSpeed+358.7,
                                                      ifelse(AvgSpeed >= 80, 0.0165*AvgSpeed^2-2.3481*AvgSpeed+211.68,130)))
  t_hora7 <- t_hora7 %>% mutate(emission = Length/1000 * flux_veic * emiss_factor * 0.1/100/0.1585)
  write.dbf(t_hora7, "t_hora7")
  ###################################################################################################################################################

  colnames(t_hora8) <- c("Id", 2, 3, 4, "AvgSpeed", 6, 7, 8, "Hits", 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  t_hora8 <- select(t_hora8, -2, -3, -4, -6, -7, -8, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28)
  t_hora8 <- merge(t_hora8, network, by.x = "Id", by.y = "Id")
  t_hora8 <- select(t_hora8, -"FRC", -"SpeedLimit", -"StreetName")
  t_hora8$flux_veic <- t_hora8$Hits * 3.1293
  t_hora8 <- t_hora8 %>% mutate(emiss_factor = ifelse(AvgSpeed < 50, 0.0928*AvgSpeed^2-9.2601*AvgSpeed+358.7,
                                                      ifelse(AvgSpeed >= 80, 0.0165*AvgSpeed^2-2.3481*AvgSpeed+211.68,130)))
  t_hora8 <- t_hora8 %>% mutate(emission = Length/1000 * flux_veic * emiss_factor * 0.1/100/0.1585)
  write.dbf(t_hora8, "t_hora8")
  ###################################################################################################################################################

  colnames(t_hora9) <- c("Id", 2, 3, 4, "AvgSpeed", 6, 7, 8, "Hits", 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  t_hora9 <- select(t_hora9, -2, -3, -4, -6, -7, -8, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28)
  t_hora9 <- merge(t_hora9, network, by.x = "Id", by.y = "Id")
  t_hora9 <- select(t_hora9, -"FRC", -"SpeedLimit", -"StreetName")
  t_hora9$flux_veic <- t_hora9$Hits * 3.1293
  t_hora9 <- t_hora9 %>% mutate(emiss_factor = ifelse(AvgSpeed < 50, 0.0928*AvgSpeed^2-9.2601*AvgSpeed+358.7,
                                                      ifelse(AvgSpeed >= 80, 0.0165*AvgSpeed^2-2.3481*AvgSpeed+211.68,130)))
  t_hora9 <- t_hora9 %>% mutate(emission = Length/1000 * flux_veic * emiss_factor * 0.1/100/0.1585)
  write.dbf(t_hora9, "t_hora9")
  ###################################################################################################################################################

  colnames(t_hora10) <- c("Id", 2, 3, 4, "AvgSpeed", 6, 7, 8, "Hits", 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  t_hora10 <- select(t_hora10, -2, -3, -4, -6, -7, -8, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28)
  t_hora10 <- merge(t_hora10, network, by.x = "Id", by.y = "Id")
  t_hora10 <- select(t_hora10, -"FRC", -"SpeedLimit", -"StreetName")
  t_hora10$flux_veic <- t_hora10$Hits * 3.1293
  t_hora10 <- t_hora10 %>% mutate(emiss_factor = ifelse(AvgSpeed < 50, 0.0928*AvgSpeed^2-9.2601*AvgSpeed+358.7,
                                                        ifelse(AvgSpeed >= 80, 0.0165*AvgSpeed^2-2.3481*AvgSpeed+211.68,130)))
  t_hora10 <- t_hora10 %>% mutate(emission = Length/1000 * flux_veic * emiss_factor * 0.1/100/0.1585)
  write.dbf(t_hora10, "t_hora10")
  ###################################################################################################################################################

  colnames(t_hora11) <- c("Id", 2, 3, 4, "AvgSpeed", 6, 7, 8, "Hits", 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  t_hora11 <- select(t_hora11, -2, -3, -4, -6, -7, -8, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28)
  t_hora11 <- merge(t_hora11, network, by.x = "Id", by.y = "Id")
  t_hora11 <- select(t_hora11, -"FRC", -"SpeedLimit", -"StreetName")
  t_hora11$flux_veic <- t_hora11$Hits * 3.1293
  t_hora11 <- t_hora11 %>% mutate(emiss_factor = ifelse(AvgSpeed < 50, 0.0928*AvgSpeed^2-9.2601*AvgSpeed+358.7,
                                                        ifelse(AvgSpeed >= 80, 0.0165*AvgSpeed^2-2.3481*AvgSpeed+211.68,130)))
  t_hora11 <- t_hora11 %>% mutate(emission = Length/1000 * flux_veic * emiss_factor * 0.1/100/0.1585)
  write.dbf(t_hora11, "t_hora11")
  ###################################################################################################################################################

  colnames(t_hora12) <- c("Id", 2, 3, 4, "AvgSpeed", 6, 7, 8, "Hits", 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  t_hora12 <- select(t_hora12, -2, -3, -4, -6, -7, -8, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28)
  t_hora12 <- merge(t_hora12, network, by.x = "Id", by.y = "Id")
  t_hora12 <- select(t_hora12, -"FRC", -"SpeedLimit", -"StreetName")
  t_hora12$flux_veic <- t_hora12$Hits * 3.1293
  t_hora12 <- t_hora12 %>% mutate(emiss_factor = ifelse(AvgSpeed < 50, 0.0928*AvgSpeed^2-9.2601*AvgSpeed+358.7,
                                                        ifelse(AvgSpeed >= 80, 0.0165*AvgSpeed^2-2.3481*AvgSpeed+211.68,130)))
  t_hora12 <- t_hora12 %>% mutate(emission = Length/1000 * flux_veic * emiss_factor * 0.1/100/0.1585)
  write.dbf(t_hora12, "t_hora12")
  ###################################################################################################################################################

  colnames(t_hora13) <- c("Id", 2, 3, 4, "AvgSpeed", 6, 7, 8, "Hits", 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  t_hora13 <- select(t_hora13, -2, -3, -4, -6, -7, -8, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28)
  t_hora13 <- merge(t_hora13, network, by.x = "Id", by.y = "Id")
  t_hora13 <- select(t_hora13, -"FRC", -"SpeedLimit", -"StreetName")
  t_hora13$flux_veic <- t_hora13$Hits * 3.1293
  t_hora13 <- t_hora13 %>% mutate(emiss_factor = ifelse(AvgSpeed < 50, 0.0928*AvgSpeed^2-9.2601*AvgSpeed+358.7,
                                                        ifelse(AvgSpeed >= 80, 0.0165*AvgSpeed^2-2.3481*AvgSpeed+211.68,130)))
  t_hora13 <- t_hora13 %>% mutate(emission = Length/1000 * flux_veic * emiss_factor * 0.1/100/0.1585)
  write.dbf(t_hora13, "t_hora13")
  ###################################################################################################################################################

  colnames(t_hora14) <- c("Id", 2, 3, 4, "AvgSpeed", 6, 7, 8, "Hits", 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  t_hora14 <- select(t_hora14, -2, -3, -4, -6, -7, -8, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28)
  t_hora14 <- merge(t_hora14, network, by.x = "Id", by.y = "Id")
  t_hora14 <- select(t_hora14, -"FRC", -"SpeedLimit", -"StreetName")
  t_hora14$flux_veic <- t_hora14$Hits * 3.1293
  t_hora14 <- t_hora14 %>% mutate(emiss_factor = ifelse(AvgSpeed < 50, 0.0928*AvgSpeed^2-9.2601*AvgSpeed+358.7,
                                                        ifelse(AvgSpeed >= 80, 0.0165*AvgSpeed^2-2.3481*AvgSpeed+211.68,130)))
  t_hora14 <- t_hora14 %>% mutate(emission = Length/1000 * flux_veic * emiss_factor * 0.1/100/0.1585)
  write.dbf(t_hora14, "t_hora14")
  ###################################################################################################################################################

  colnames(t_hora15) <- c("Id", 2, 3, 4, "AvgSpeed", 6, 7, 8, "Hits", 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  t_hora15 <- select(t_hora15, -2, -3, -4, -6, -7, -8, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28)
  t_hora15 <- merge(t_hora15, network, by.x = "Id", by.y = "Id")
  t_hora15 <- select(t_hora15, -"FRC", -"SpeedLimit", -"StreetName")
  t_hora15$flux_veic <- t_hora15$Hits * 3.1293
  t_hora15 <- t_hora15 %>% mutate(emiss_factor = ifelse(AvgSpeed < 50, 0.0928*AvgSpeed^2-9.2601*AvgSpeed+358.7,
                                                        ifelse(AvgSpeed >= 80, 0.0165*AvgSpeed^2-2.3481*AvgSpeed+211.68,130)))
  t_hora15 <- t_hora15 %>% mutate(emission = Length/1000 * flux_veic * emiss_factor * 0.1/100/0.1585)
  write.dbf(t_hora15, "t_hora15")
  ###################################################################################################################################################

  colnames(t_hora16) <- c("Id", 2, 3, 4, "AvgSpeed", 6, 7, 8, "Hits", 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  t_hora16 <- select(t_hora16, -2, -3, -4, -6, -7, -8, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28)
  t_hora16 <- merge(t_hora16, network, by.x = "Id", by.y = "Id")
  t_hora16 <- select(t_hora16, -"FRC", -"SpeedLimit", -"StreetName")
  t_hora16$flux_veic <- t_hora16$Hits * 3.1293
  t_hora16 <- t_hora16 %>% mutate(emiss_factor = ifelse(AvgSpeed < 50, 0.0928*AvgSpeed^2-9.2601*AvgSpeed+358.7,
                                                        ifelse(AvgSpeed >= 80, 0.0165*AvgSpeed^2-2.3481*AvgSpeed+211.68,130)))
  t_hora16 <- t_hora16 %>% mutate(emission = Length/1000 * flux_veic * emiss_factor * 0.1/100/0.1585)
  write.dbf(t_hora16, "t_hora16")
  ###################################################################################################################################################

  colnames(t_hora17) <- c("Id", 2, 3, 4, "AvgSpeed", 6, 7, 8, "Hits", 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  t_hora17 <- select(t_hora17, -2, -3, -4, -6, -7, -8, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28)
  t_hora17 <- merge(t_hora17, network, by.x = "Id", by.y = "Id")
  t_hora17 <- select(t_hora17, -"FRC", -"SpeedLimit", -"StreetName")
  t_hora17$flux_veic <- t_hora17$Hits * 3.1293
  t_hora17 <- t_hora17 %>% mutate(emiss_factor = ifelse(AvgSpeed < 50, 0.0928*AvgSpeed^2-9.2601*AvgSpeed+358.7,
                                                        ifelse(AvgSpeed >= 80, 0.0165*AvgSpeed^2-2.3481*AvgSpeed+211.68,130)))
  t_hora17 <- t_hora17 %>% mutate(emission = Length/1000 * flux_veic * emiss_factor * 0.1/100/0.1585)
  write.dbf(t_hora17, "t_hora17")
  ###################################################################################################################################################

  colnames(t_hora18) <- c("Id", 2, 3, 4, "AvgSpeed", 6, 7, 8, "Hits", 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  t_hora18 <- select(t_hora18, -2, -3, -4, -6, -7, -8, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28)
  t_hora18 <- merge(t_hora18, network, by.x = "Id", by.y = "Id")
  t_hora18 <- select(t_hora18, -"FRC", -"SpeedLimit", -"StreetName")
  t_hora18$flux_veic <- t_hora18$Hits * 3.1293
  t_hora18 <- t_hora18 %>% mutate(emiss_factor = ifelse(AvgSpeed < 50, 0.0928*AvgSpeed^2-9.2601*AvgSpeed+358.7,
                                                        ifelse(AvgSpeed >= 80, 0.0165*AvgSpeed^2-2.3481*AvgSpeed+211.68,130)))
  t_hora18 <- t_hora18 %>% mutate(emission = Length/1000 * flux_veic * emiss_factor * 0.1/100/0.1585)
  write.dbf(t_hora18, "t_hora18")
  ###################################################################################################################################################

  colnames(t_hora19) <- c("Id", 2, 3, 4, "AvgSpeed", 6, 7, 8, "Hits", 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  t_hora19 <- select(t_hora19, -2, -3, -4, -6, -7, -8, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28)
  t_hora19 <- merge(t_hora19, network, by.x = "Id", by.y = "Id")
  t_hora19 <- select(t_hora19, -"FRC", -"SpeedLimit", -"StreetName")
  t_hora19$flux_veic <- t_hora19$Hits * 3.1293
  t_hora19 <- t_hora19 %>% mutate(emiss_factor = ifelse(AvgSpeed < 50, 0.0928*AvgSpeed^2-9.2601*AvgSpeed+358.7,
                                                        ifelse(AvgSpeed >= 80, 0.0165*AvgSpeed^2-2.3481*AvgSpeed+211.68,130)))
  t_hora19 <- t_hora19 %>% mutate(emission = Length/1000 * flux_veic * emiss_factor * 0.1/100/0.1585)
  write.dbf(t_hora19, "t_hora19")
  ###################################################################################################################################################

  colnames(t_hora20) <- c("Id", 2, 3, 4, "AvgSpeed", 6, 7, 8, "Hits", 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  t_hora20 <- select(t_hora20, -2, -3, -4, -6, -7, -8, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28)
  t_hora20 <- merge(t_hora20, network, by.x = "Id", by.y = "Id")
  t_hora20 <- select(t_hora20, -"FRC", -"SpeedLimit", -"StreetName")
  t_hora20$flux_veic <- t_hora20$Hits * 3.1293
  t_hora20 <- t_hora20 %>% mutate(emiss_factor = ifelse(AvgSpeed < 50, 0.0928*AvgSpeed^2-9.2601*AvgSpeed+358.7,
                                                        ifelse(AvgSpeed >= 80, 0.0165*AvgSpeed^2-2.3481*AvgSpeed+211.68,130)))
  t_hora20 <- t_hora20 %>% mutate(emission = Length/1000 * flux_veic * emiss_factor * 0.1/100/0.1585)
  write.dbf(t_hora20, "t_hora20")
  ###################################################################################################################################################

  colnames(t_hora21) <- c("Id", 2, 3, 4, "AvgSpeed", 6, 7, 8, "Hits", 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  t_hora21 <- select(t_hora21, -2, -3, -4, -6, -7, -8, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28)
  t_hora21 <- merge(t_hora21, network, by.x = "Id", by.y = "Id")
  t_hora21 <- select(t_hora21, -"FRC", -"SpeedLimit", -"StreetName")
  t_hora21$flux_veic <- t_hora21$Hits * 3.1293
  t_hora21 <- t_hora21 %>% mutate(emiss_factor = ifelse(AvgSpeed < 50, 0.0928*AvgSpeed^2-9.2601*AvgSpeed+358.7,
                                                        ifelse(AvgSpeed >= 80, 0.0165*AvgSpeed^2-2.3481*AvgSpeed+211.68,130)))
  t_hora21 <- t_hora21 %>% mutate(emission = Length/1000 * flux_veic * emiss_factor * 0.1/100/0.1585)
  write.dbf(t_hora21, "t_hora21")
  ###################################################################################################################################################

  colnames(t_hora22) <- c("Id", 2, 3, 4, "AvgSpeed", 6, 7, 8, "Hits", 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  t_hora22 <- select(t_hora22, -2, -3, -4, -6, -7, -8, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28)
  t_hora22 <- merge(t_hora22, network, by.x = "Id", by.y = "Id")
  t_hora22 <- select(t_hora22, -"FRC", -"SpeedLimit", -"StreetName")
  t_hora22$flux_veic <- t_hora22$Hits * 3.1293
  t_hora22 <- t_hora22 %>% mutate(emiss_factor = ifelse(AvgSpeed < 50, 0.0928*AvgSpeed^2-9.2601*AvgSpeed+358.7,
                                                        ifelse(AvgSpeed >= 80, 0.0165*AvgSpeed^2-2.3481*AvgSpeed+211.68,130)))
  t_hora22 <- t_hora22 %>% mutate(emission = Length/1000 * flux_veic * emiss_factor * 0.1/100/0.1585)
  write.dbf(t_hora22, "t_hora22")
  ###################################################################################################################################################

  colnames(t_hora23) <- c("Id", 2, 3, 4, "AvgSpeed", 6, 7, 8, "Hits", 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  t_hora23 <- select(t_hora23, -2, -3, -4, -6, -7, -8, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28)
  t_hora23 <- merge(t_hora23, network, by.x = "Id", by.y = "Id")
  t_hora23 <- select(t_hora23, -"FRC", -"SpeedLimit", -"StreetName")
  t_hora23$flux_veic <- t_hora23$Hits * 3.1293
  t_hora23 <- t_hora23 %>% mutate(emiss_factor = ifelse(AvgSpeed < 50, 0.0928*AvgSpeed^2-9.2601*AvgSpeed+358.7,
                                                        ifelse(AvgSpeed >= 80, 0.0165*AvgSpeed^2-2.3481*AvgSpeed+211.68,130)))
  t_hora23 <- t_hora23 %>% mutate(emission = Length/1000 * flux_veic * emiss_factor * 0.1/100/0.1585)
  write.dbf(t_hora23, "t_hora23")
  #######################################


}
