#'Diseño en Parcelas Divididas
#'
#'Realiza un analisis para un diseño en parcelas divididas
#'
#'@param base de datos (data.frame) datos del experimento
#'@return Una comparacion de promedios
#'@export
tablaAov<- function(rendimiento, var, camp,fs,data){

  # Defino la variable respuesta y los tratamientos y bloques como factores
  y <- data[,rendimiento]
  var <- factor(data[,var])
  camp <- factor(data[,camp])
  fs<- factor(data[,fs])
  a <- nlevels(camp)
  b <- nlevels(var)
  c <- nlevels(fs)

  # Corrección para la media
  suma_total <- sum(y)
  C <- suma_total^2 /(a*b)

  # SC Total
  sc_total <- sum(y^2) - C
  gl_total <- a*b-1
  cm_total <- sc_total / gl_total

  # sc var
  sumasxvar <- tapply(y, INDEX = var, FUN = sum)
  n_Var <- tapply(y, INDEX = var, FUN = length)
  sc_var <- sum((sumasxvar^2 )/ (3*3))-C
  gl_var <- a-1
  cm_var <-   sc_var / gl_var

  # sc Campos
  sumasxcamp <- tapply(y, INDEX = camp, FUN = sum)
  n_camp <- tapply(y, INDEX = camp, FUN = length)
  sc_camp <- sum(sumasxcamp^2 / (3*3))-C
  gl_camp <- b-1
  cm_camp <- sc_camp/ gl_camp

  # SCfs
  sumasxfs <- tapply(y, INDEX = fs, FUN = sum)
  n_fs <- tapply(y, INDEX =fs, FUN = length)
  sc_fs <- sum((sumasxfs^2)/(3*3))-C
  gl_fs <- b-1
  cm_fs <- sc_fs/ gl_fs

  #SCEa
  sumasfsXvar<- tapply(y, INDEX = fs ,FUN = sum)
  n_fs- tapply(y, INDEX = fs, FUN = length)
  sc_fs <- sum((sumasxfs^2)/3-C-sc_fs-sc_Campo)
  gl_fs <- b-1
  cm_fs <- sc_fs/ gl_fs

  #SCEFV
  sumasxfs- tapply(y, INDEX = fs, FUN = sum)
  n_fs- tapply(y, INDEX = fs, FUN = length)
  sc_fs <- C-sc_fs-sc_camp
  gl_fs <- b-1
  cm_fs <- sc_fs/ gl_fs

  #SCEab
  SCEab<- sc_total-sumasxfs- sc_Campo- sc_Var-sc_fs

  #SC residuales
  sc_res <- sc_total - sc_Var - sc_campo- sc_fs
  gl_res <- (a-1)*(b-1)
  cm_res <- sc_total/ gl_total

  # Valores F
  F_Campo<- cm_Campo/ cm_res
  F_Var <- cm_Var/ cm_res
  F_fs<-cm_fs/cm_res
  # P-values

  p_value_var <- pf(F_Var, gl_Var , gl_res, lower.tail = FALSE)
  p_value_fs <- pf(F_fs, gl_fs, gl_res, lower.tail = FALSE)
  p_value_Campo <- pf(F_Campo, gl_Campo, gl_res, lower.tail = FALSE)

  # Creamos el dataframe
  tabla <- data.frame(FV = c("var", "camp", "fs"),
                      SC = c(sc_Var, sc_Campo, sc_fs),
                      GL = c(gl_Var, gl_Campo, gl_fs),
                      CM = c(cm_Var, cm_Campo, cm_fs),
                      F = c(F_Var, F_Campo, F_fs),
                      `Pr(>F)` = c(p_value_var, p_value_Campo, p_value_fs),
                      check.names = FALSE)
  rownames(tabla) <- NULL
  anava <- format(tabla)
  anava[is.na(tabla)]

  return(anava)
}

