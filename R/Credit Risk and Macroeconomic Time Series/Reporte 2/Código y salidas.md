> # Cargar los datos de nuevo
> datos <- read_xlsx("C:/Users/oscar/Desktop/Backup/Asuntos personales/UCR/2026/2026_I Cuatrimestre/Métodos Cuantitativos en Banca/Reporte/Reporte 2/Base de datos_SUGEF 1-05.xlsx")
> skim(datos)
── Data Summary ────────────────────────
                           Values
Name                       datos 
Number of rows             204   
Number of columns          5     
_______________________          
Column type frequency:           
  numeric                  4     
  POSIXct                  1     
________________________         
Group variables            None  

── Variable type: numeric ─────────────────────────────────────────────────────────────────────────────────────────────
  skim_variable n_missing complete_rate      mean       sd        p0       p25       p50       p75     p100 hist 
1 PD90                  1         0.995   0.00559  0.00189   0.00117   0.00395   0.00498   0.00736   0.0101 ▁▇▃▆▁
2 TPM                   0         1       4.86     2.67      0         2.94      5         6.5      10      ▅▃▇▂▃
3 FX                    0         1     557.      42.3     497.      521.      555.      580.      693.     ▆▇▃▂▁
4 INF_YOY               0         1       4.33     3.96     -3.28      1.58      4.02      5.80     16.3    ▃▇▆▂▁

── Variable type: POSIXct ─────────────────────────────────────────────────────────────────────────────────────────────
  skim_variable n_missing complete_rate min                 max                 median              n_unique
1 Fecha                 0             1 2007-01-01 00:00:00 2023-12-01 00:00:00 2015-06-16 00:00:00      204
> # 1 - Evaluación de supuestos del modelo de regresión lineal
> mod1_R2 <- lm(PD90 ~ TPM + FX + INF_YOY, data = datos)
> # Pruebas de supuestos del modelo de regresión del Reporte 1
> dwtest(mod1_R2)   # autocorrelación

	Durbin-Watson test

data:  mod1_R2
DW = 0.13381, p-value < 2.2e-16
alternative hypothesis: true autocorrelation is greater than 0

> bptest(mod1_R2)   # heterocedasticidad

	studentized Breusch-Pagan test

data:  mod1_R2
BP = 40.13, df = 3, p-value = 1e-08

> vif(mod1_R2)      # multicolinealidad
     TPM       FX  INF_YOY 
1.522468 1.014910 1.535737 
> # Gráfico de autocorrelación de residuos
> acf(resid(mod1_R2), main = "ACF de residuos del modelo lineal inicial", xlab = "Rezagos", ylab = "Autocorrelación",
+ col = "black", lwd = 2)
> # 2 - Evaluación de estacionariedad
> # Base de datos para series de tiempo, eliminando observaciones con NA
> datos_st <- na.omit(datos[, c("PD90", "TPM", "FX", "INF_YOY")])
> # Pruebas ADF
> adf.test(datos_st$PD90)

	Augmented Dickey-Fuller Test

data:  datos_st$PD90
Dickey-Fuller = -1.9983, Lag order = 5, p-value = 0.5768
alternative hypothesis: stationary

> adf.test(datos_st$TPM)

	Augmented Dickey-Fuller Test

data:  datos_st$TPM
Dickey-Fuller = -3.2591, Lag order = 5, p-value = 0.07954
alternative hypothesis: stationary

> adf.test(datos_st$FX)

	Augmented Dickey-Fuller Test

data:  datos_st$FX
Dickey-Fuller = -8.4579, Lag order = 5, p-value = 0.01
alternative hypothesis: stationary

Warning message:
In adf.test(datos_st$FX) : p-value smaller than printed p-value

> adf.test(datos_st$INF_YOY)

	Augmented Dickey-Fuller Test

data:  datos_st$INF_YOY
Dickey-Fuller = -3.8965, Lag order = 5, p-value = 0.01542
alternative hypothesis: stationary

> # 3 - Transformación de las variables no estacionarias
> # Diferencias
> dPD90 <- diff(datos_st$PD90)
> dTPM  <- diff(datos_st$TPM)
> # Base final para VAR
> datos_var <- na.omit(cbind(dPD90, dTPM, FX = datos_st$FX[-1], INF_YOY = datos_st$INF_YOY[-1]))
> # 4 - VAR
> modelo_var <- VAR(datos_var, p = 3, type = "const")
> summary(modelo_var)

VAR Estimation Results:
========================= 
Endogenous variables: dPD90, dTPM, FX, INF_YOY 
Deterministic variables: const 
Sample size: 199 
Log Likelihood: -117.782 
Roots of the characteristic polynomial:
0.917 0.8885 0.8885 0.7321 0.7179 0.5537 0.5537 0.4812 0.4812 0.4428 0.2738 0.2738
Call:
VAR(y = datos_var, p = 3, type = "const")


Estimation results for equation dPD90: 
====================================== 
dPD90 = dPD90.l1 + dTPM.l1 + FX.l1 + INF_YOY.l1 + dPD90.l2 + dTPM.l2 + FX.l2 + INF_YOY.l2 + dPD90.l3 + dTPM.l3 + FX.l3 + INF_YOY.l3 + const 

             Estimate Std. Error t value Pr(>|t|)   
dPD90.l1   -2.210e-01  7.290e-02  -3.031  0.00278 **
dTPM.l1     3.535e-05  5.435e-05   0.650  0.51620   
FX.l1       8.285e-07  1.107e-06   0.749  0.45498   
INF_YOY.l1  2.105e-05  6.336e-05   0.332  0.74012   
dPD90.l2   -1.896e-01  6.393e-02  -2.966  0.00341 **
dTPM.l2     6.982e-05  5.314e-05   1.314  0.19051   
FX.l2      -1.493e-07  1.284e-06  -0.116  0.90756   
INF_YOY.l2 -9.402e-05  1.071e-04  -0.878  0.38105   
dPD90.l3   -4.570e-02  6.241e-02  -0.732  0.46498   
dTPM.l3     4.790e-05  5.027e-05   0.953  0.34186   
FX.l3       1.027e-06  1.118e-06   0.918  0.35962   
INF_YOY.l3  7.953e-05  6.547e-05   1.215  0.22602   
const      -9.491e-04  6.835e-04  -1.389  0.16662   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


Residual standard error: 0.0005425 on 186 degrees of freedom
Multiple R-Squared: 0.08976,	Adjusted R-squared: 0.03103 
F-statistic: 1.528 on 12 and 186 DF,  p-value: 0.117 


Estimation results for equation dTPM: 
===================================== 
dTPM = dPD90.l1 + dTPM.l1 + FX.l1 + INF_YOY.l1 + dPD90.l2 + dTPM.l2 + FX.l2 + INF_YOY.l2 + dPD90.l3 + dTPM.l3 + FX.l3 + INF_YOY.l3 + const 

             Estimate Std. Error t value Pr(>|t|)    
dPD90.l1    1.454e+01  7.949e+01   0.183   0.8550    
dTPM.l1    -3.050e-01  5.926e-02  -5.146 6.72e-07 ***
FX.l1      -2.077e-04  1.207e-03  -0.172   0.8635    
INF_YOY.l1  2.912e-01  6.909e-02   4.215 3.89e-05 ***
dPD90.l2    7.388e+00  6.971e+01   0.106   0.9157    
dTPM.l2    -2.339e-01  5.794e-02  -4.036 7.93e-05 ***
FX.l2       1.380e-03  1.400e-03   0.986   0.3255    
INF_YOY.l2 -2.232e-01  1.168e-01  -1.912   0.0574 .  
dPD90.l3    1.342e+02  6.805e+01   1.972   0.0501 .  
dTPM.l3     4.794e-01  5.481e-02   8.747 1.30e-15 ***
FX.l3       1.244e-03  1.219e-03   1.020   0.3090    
INF_YOY.l3 -4.409e-02  7.139e-02  -0.618   0.5375    
const      -1.425e+00  7.453e-01  -1.912   0.0574 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


Residual standard error: 0.5916 on 186 degrees of freedom
Multiple R-Squared: 0.7193,	Adjusted R-squared: 0.7012 
F-statistic: 39.73 on 12 and 186 DF,  p-value: < 2.2e-16 


Estimation results for equation FX: 
=================================== 
FX = dPD90.l1 + dTPM.l1 + FX.l1 + INF_YOY.l1 + dPD90.l2 + dTPM.l2 + FX.l2 + INF_YOY.l2 + dPD90.l3 + dTPM.l3 + FX.l3 + INF_YOY.l3 + const 

             Estimate Std. Error t value Pr(>|t|)    
dPD90.l1    1.247e+03  4.767e+03   0.262  0.79399    
dTPM.l1    -4.301e-01  3.554e+00  -0.121  0.90380    
FX.l1       6.389e-01  7.236e-02   8.829 7.75e-16 ***
INF_YOY.l1 -5.955e+00  4.143e+00  -1.437  0.15230    
dPD90.l2   -3.020e+03  4.180e+03  -0.722  0.47101    
dTPM.l2     3.120e+00  3.475e+00   0.898  0.37042    
FX.l2      -2.283e-01  8.399e-02  -2.718  0.00719 ** 
INF_YOY.l2  6.074e+00  7.002e+00   0.867  0.38683    
dPD90.l3    7.791e+03  4.081e+03   1.909  0.05782 .  
dTPM.l3    -9.116e-01  3.287e+00  -0.277  0.78184    
FX.l3       1.344e-01  7.311e-02   1.839  0.06752 .  
INF_YOY.l3 -6.651e-01  4.281e+00  -0.155  0.87671    
const       2.557e+02  4.469e+01   5.722 4.15e-08 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


Residual standard error: 35.48 on 186 degrees of freedom
Multiple R-Squared: 0.345,	Adjusted R-squared: 0.3027 
F-statistic: 8.163 on 12 and 186 DF,  p-value: 3.014e-12 


Estimation results for equation INF_YOY: 
======================================== 
INF_YOY = dPD90.l1 + dTPM.l1 + FX.l1 + INF_YOY.l1 + dPD90.l2 + dTPM.l2 + FX.l2 + INF_YOY.l2 + dPD90.l3 + dTPM.l3 + FX.l3 + INF_YOY.l3 + const 

             Estimate Std. Error t value Pr(>|t|)    
dPD90.l1   -3.636e+01  8.467e+01  -0.429  0.66811    
dTPM.l1     1.275e-01  6.312e-02   2.021  0.04475 *  
FX.l1      -4.191e-04  1.285e-03  -0.326  0.74470    
INF_YOY.l1  1.376e+00  7.359e-02  18.704  < 2e-16 ***
dPD90.l2    2.905e+01  7.425e+01   0.391  0.69608    
dTPM.l2     1.053e-01  6.172e-02   1.706  0.08960 .  
FX.l2       1.821e-04  1.492e-03   0.122  0.90298    
INF_YOY.l2 -3.372e-01  1.244e-01  -2.712  0.00732 ** 
dPD90.l3   -1.052e+00  7.248e+01  -0.015  0.98844    
dTPM.l3     7.500e-02  5.838e-02   1.285  0.20051    
FX.l3       1.058e-03  1.298e-03   0.815  0.41624    
INF_YOY.l3 -7.577e-02  7.604e-02  -0.996  0.32034    
const      -3.346e-01  7.938e-01  -0.422  0.67387    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


Residual standard error: 0.6301 on 186 degrees of freedom
Multiple R-Squared: 0.976,	Adjusted R-squared: 0.9745 
F-statistic: 631.6 on 12 and 186 DF,  p-value: < 2.2e-16 



Covariance matrix of residuals:
             dPD90       dTPM         FX    INF_YOY
dPD90    2.943e-07  8.260e-06 -8.208e-04 -3.217e-05
dTPM     8.260e-06  3.499e-01 -7.837e-02  3.574e-02
FX      -8.208e-04 -7.837e-02  1.259e+03  1.044e+00
INF_YOY -3.217e-05  3.574e-02  1.044e+00  3.970e-01

Correlation matrix of residuals:
           dPD90      dTPM        FX  INF_YOY
dPD90    1.00000  0.025738 -0.042647 -0.09411
dTPM     0.02574  1.000000 -0.003734  0.09588
FX      -0.04265 -0.003734  1.000000  0.04671
INF_YOY -0.09411  0.095876  0.046714  1.00000
