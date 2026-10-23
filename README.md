# Pruebas de hipótesis, tablas de contingencia y análisis multivariado en R

Inferencia básica y análisis de datos categóricos: diferencia de medias, tablas de contingencia, análisis de correspondencias (ACS) y análisis de componentes principales (ACP).

## Qué encontrarás aquí

| Archivo | Qué hace |
|---|---|
| `scripts/prueba_diferencia_de_medias.R` | Prueba de hipótesis para la diferencia de medias entre dos muestras (`t.test`). |
| `scripts/tablas_de_contingencia.R` | Construcción y lectura de tablas de contingencia. |
| `scripts/analisis_correspondencias_ejemplo.R` | Análisis de correspondencias simple (`FactoMineR::CA`). |
| `scripts/analisis_correspondencias_tareas_hogar.R` | Análisis de correspondencias con la base `housetasks`: chi-cuadrado y mapa factorial. |
| `scripts/analisis_correspondencias_preferencias.R` | Ejemplo con datos construidos: preferencia por una hamburguesa nueva según grupos de edad. |
| `scripts/guia_acp.R` | Guía de análisis de componentes principales. |

## Cómo ejecutarlo

1. Descarga o clona este repositorio y abre RStudio.
2. Fija la carpeta del repositorio como directorio de trabajo (`Session > Set Working Directory`).
3. Los scripts leen los datos desde la carpeta `datos/`. Instala los paquetes que falten con `install.packages("nombre")`.

**Paquetes usados:** `FactoMineR`, `factoextra`, `ggplot2`.

## Datos

Los CSV incluidos son datos de ejemplo del ejercicio de correspondencias.

## Autoría

Código y análisis de Alejandra, con fines educativos y de portafolio. Más sobre mi trabajo en analítica de datos: [alejadata.pro](https://alejadata.pro/).
