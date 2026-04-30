> # 1 - Cargar datos y revisión
> datos <- read_xlsx("C:/Users/oscar/Desktop/Backup/Asuntos personales/UCR/2026/2026_I Cuatrimestre/Métodos Cuantitativos en Banca/Reporte/Reporte SUGEF 1-05/Base de datos_SUGEF 1-05.xlsx")
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
> 
> # 2 - Estructura con Data.Explorer y ver relaciones)
> DataExplorer::plot_missing(datos)
> # Excluyendo fecha
> DataExplorer::plot_scatterplot(datos[, c("PD90","TPM","FX","INF_YOY")], by = "PD90")
Warning message:
Removed 3 rows containing missing values or values outside the scale range (`geom_point()`). 
> DataExplorer::plot_histogram(datos[, c("PD90","TPM","FX","INF_YOY")])
> DataExplorer::plot_correlation(datos[, c("PD90", "TPM","FX","INF_YOY")])
Warning message:
Removed 6 rows containing missing values or values outside the scale range (`geom_text()`). 
> 
> # Verifiación de los datos para ver correlación, darse una idea no gráfica
> cor(datos[, c("PD90","TPM","FX","INF_YOY")], use="complete.obs")
                PD90         TPM           FX    INF_YOY
PD90     1.000000000 -0.02820309  0.001305555 -0.2969342
TPM     -0.028203094  1.00000000 -0.077664074  0.5857682
FX       0.001305555 -0.07766407  1.000000000 -0.1209111
INF_YOY -0.296934163  0.58576821 -0.120911052  1.0000000
> 
> # 3 - Modelo
> mod1 <- lm(PD90 ~ TPM + FX + INF_YOY, data=datos)
> summary(mod1)

Call:
lm(formula = PD90 ~ TPM + FX + INF_YOY, data = datos)

Residuals:
       Min         1Q     Median         3Q        Max 
-0.0026604 -0.0015078 -0.0003905  0.0013637  0.0045717 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  6.557e-03  1.699e-03   3.859 0.000154 ***
TPM          1.558e-04  5.765e-05   2.702 0.007486 ** 
FX          -1.495e-06  2.980e-06  -0.502 0.616500    
INF_YOY     -2.047e-04  3.913e-05  -5.232 4.24e-07 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.001781 on 199 degrees of freedom
  (1 observation deleted due to missingness)
Multiple R-squared:  0.1216,	Adjusted R-squared:  0.1084 
F-statistic: 9.184 on 3 and 199 DF,  p-value: 1.017e-05

> 
> # Variable muy significativa, la inflación, segunda, la TPM
> # Dado a eso, se tira el modelo solo explicando PD por la inflación y TPM
> mod_inf_TPM <- lm(PD90 ~ INF_YOY + TPM, data = datos)
> summary(mod_inf_TPM)

Call:
lm(formula = PD90 ~ INF_YOY + TPM, data = datos)

Residuals:
       Min         1Q     Median         3Q        Max 
-0.0026743 -0.0015248 -0.0004217  0.0013897  0.0045558 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  5.714e-03  2.588e-04  22.079  < 2e-16 ***
INF_YOY     -2.029e-04  3.889e-05  -5.217 4.52e-07 ***
TPM          1.560e-04  5.754e-05   2.711  0.00728 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.001777 on 200 degrees of freedom
  (1 observation deleted due to missingness)
Multiple R-squared:  0.1205,	Adjusted R-squared:  0.1117 
F-statistic:  13.7 on 2 and 200 DF,  p-value: 2.652e-06

> #  Explicado por ambas anteriores, se observa una misma significancia
> 
> # 4 - Plot para ver mod1
> par(mfrow=c(2,2))
> plot(mod1)
> par(mfrow=c(1,1))
> 
> # En el mod 1, la concentración de los datos sigue siendo dispersa, sin embargo, los residuos estandarizados se miran
> # en una buena relación, lo que puede determinar el mejor cumplimiento de los supuestos, y en el summary se observa
> # por el % de significancia, que es alta.
> 
> # Plot para ver el model de inflacion y TPM
> par(mfrow=c(2,2))
> plot(mod_inf_TPM)
> par(mfrow=c(1,1))
