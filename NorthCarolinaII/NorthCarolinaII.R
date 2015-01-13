carolinaII <-  function (set,male,female,replication,y,name.y) 
{
  model <- lm(y ~ set + replication %in% set + male %in% set + female %in% set + male:female %in% set)
  
  cat("Response(y): ", name.y, "\n\n")
  print(anova(model))
  cat("\nCV:", cv.model(model), "\tMean:", mean(y), "\n")
   
  #m <- length(levels(model$model$male))
  m0=aggregate(model$model[,1],list(A=model$model[,2],B=model$model[,4]),length)
  m=unique((aggregate(m0[,2],list(C=m0[,1]),length))[,2])
  #f <- length(levels(model$model$female))
  f0=aggregate(model$model[,1],list(A=model$model[,2],B=model$model[,5]),length)
  f=unique((aggregate(m0[,2],list(C=m0[,1]),length))[,2])
  s <- length(levels(model$model$set))
  r <- length(levels(model$model$replication))
  anva <- as.matrix(anova(model)) ## sacar los cm
  
  cat("\n","\n","Variance Components","\n")
  
  var.m <- (anva["set:male", "Mean Sq"] - anva["set:male:female", 
                                               "Mean Sq"])/(f * r);cat("\n","var.m: ",var.m,"\n")
  var.f <- (anva["set:female", "Mean Sq"] - anva["set:male:female", 
                                                 "Mean Sq"])/(m * r);cat("\n","var.f: ",var.f,"\n")
  var.mf <- (anva["set:male:female", "Mean Sq"] - anva["Residuals", 
                                                       "Mean Sq"])/r;cat("\n","var.mf: ",var.mf,"\n")
  var.e <- anva["Residuals", "Mean Sq"];cat("\n","var.e: ",var.e,"\n")
  
  var.A <- 4*var.m;cat("\n","var.A: ",var.A,"\n")
  var.D <- 6*var.mf ;cat("\n","var.D: ",var.D,"\n")
  var.h2 <- 4*var.m/(4*var.m + var.mf/f + var.e/(r*f));cat("\n","var.h2:",var.h2,"\n")
  
  
  #cat("\n","\n","Analysis of Variance Based on the Nesting","\n")
  
  cuadro <- matrix(NA,7,5)
  cuadro[,1] = c(anva[1:2,1],sum(anva[3:5,1]),anva[3:6,1]) 
  cuadro[,2] = c(anva[1:2,3],sum(anva[3:5,3]),anva[3:6,3])  
  cuadro[4,3] = var.e + r*var.mf + f*r*var.m
  cuadro[5,3] = var.e + r*var.mf + m*r*var.f
  cuadro[6,3] = var.e + r*var.mf
  cuadro[7,3] = var.e
  cuadro[4,4] = cuadro[4,3]/cuadro[6,3]
  cuadro[5,4] = cuadro[5,3]/cuadro[6,3]
  cuadro[6,4] = cuadro[6,3]/cuadro[7,3]
  cuadro[4,5] = 1-pf(cuadro[4,4],cuadro[4,1],cuadro[4,1])
  cuadro[5,5] = 1-pf(cuadro[5,4],cuadro[5,1],cuadro[5,1])
  cuadro[6,5] = 1-pf(cuadro[6,4],cuadro[6,1],cuadro[6,1])
  
  colnames(cuadro) = c("Df","Mean Sq","Expect Mean Sq","F value","Pr(>F)") 
  rownames(cuadro) = c("set","replication/set","fam/set","   male/set","   female/set","   male:female/set","Residuals") 
  
  # print(cuadro,na.print = "")
  
  var.Am <- 4 * var.m
  var.Af <- 4 * var.f
  var.D <- 4 * var.mf
  output <- list(var.Am = var.Am, var.Af = var.Af,var.D = var.D)
  return(output)
  
}

#formato del libro de campo  SET MALE INSTN REP VARIABLE
#fp <- file.choose()
#sname <- "nombre de la hoja excel del libro de campo"
#datos <- read.xlsx(datos,sname)
#MALE:MACHOS
#INSTN: hembras
#rep: repeticiones o bloques

# Ejemplo
# set <- as.factor(datos[,"SET"])
# male <- as.factor(datos[,"MALE"])
# female <- as.factor(datos[, "INSTN"])
# replication <- as.factor(datos[, "REP"])

#indice O posucion de la variable en el libro de campo
# y  <-  datos[,abb[i]]
# name.y <- lab[i] 
##name y:nombre de la variable
# output=carolinaII(set,male,female,replication,y,name.y)