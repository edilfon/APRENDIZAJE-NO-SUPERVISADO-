# Instalar paquetes si no los tienes
# install.packages(c("tidyverse", "caret", "class", "factoextra", "cluster", "dendextend", "ggdendro", "stringr", "readxl"))

library(tidyverse)
library(caret)
library(class)
library(factoextra)
library(cluster)
library(dendextend)
library(ggplot2)
library(ggdendro)
library(stringr)
library(readxl)

# ======================
# 1. CARGAR Y PREPARAR DATOS
# ======================

# Cargar datos desde Excel
datos <- read_excel("C:/Users/EDILFONSO/Downloads/Multas.xlsx")

# Verificar los nombres de las columnas
print("Columnas disponibles:")
print(colnames(datos))

# Limpiar nombres de columnas para evitar problemas con espacios y caracteres especiales
colnames(datos) <- c("Codigo_Falta", "Descripcion_Falta", "Tipo_Formato", 
                     "Nivel_Gravedad", "Anio_Infraccion", "Mes_Infraccion",
                     "Cant_Multas", "Importe_Impuesto", "Reincidencia_Impuesta")

# Explorar los datos
print("Primeras filas de los datos:")
print(head(datos))

print("Estructura de los datos:")
print(str(datos))

print("Resumen estadístico:")
print(summary(datos))

# Verificar valores faltantes antes del procesamiento
print("Valores faltantes por columna:")
print(sapply(datos, function(x) sum(is.na(x))))

# Preparar variables categóricas con manejo robusto de NAs
datos_procesados <- datos %>%
  # Filtrar filas que tengan valores críticos faltantes
  filter(!is.na(Tipo_Formato), 
         !is.na(Nivel_Gravedad),
         !is.na(Anio_Infraccion),
         !is.na(Mes_Infraccion),
         !is.na(Cant_Multas),
         !is.na(Importe_Impuesto)) %>%
  mutate(
    # Convertir variables categóricas a numéricas
    Tipo_Formato_num = as.numeric(factor(Tipo_Formato)),
    Nivel_Gravedad_num = case_when(
      str_detect(tolower(as.character(Nivel_Gravedad)), "leve") ~ 1,
      str_detect(tolower(as.character(Nivel_Gravedad)), "grave") ~ 2,
      str_detect(tolower(as.character(Nivel_Gravedad)), "muy grave") ~ 3,
      TRUE ~ as.numeric(factor(Nivel_Gravedad))
    ),
    # Crear categorías de meses por trimestres
    Trimestre = case_when(
      Mes_Infraccion %in% 1:3 ~ 1,
      Mes_Infraccion %in% 4:6 ~ 2,
      Mes_Infraccion %in% 7:9 ~ 3,
      Mes_Infraccion %in% 10:12 ~ 4,
      TRUE ~ 1
    ),
    # Reemplazar NAs en Reincidencia_Impuesta con 0
    Reincidencia_Impuesta = ifelse(is.na(Reincidencia_Impuesta), 0, Reincidencia_Impuesta),
    # Crear categorías de importes
    Categoria_Importe = case_when(
      Importe_Impuesto <= quantile(Importe_Impuesto, 0.33, na.rm = TRUE) ~ "Bajo",
      Importe_Impuesto <= quantile(Importe_Impuesto, 0.67, na.rm = TRUE) ~ "Medio",
      TRUE ~ "Alto"
    ),
    Categoria_Importe_num = as.numeric(factor(Categoria_Importe, levels = c("Bajo", "Medio", "Alto")))
  )

print("Filas después del filtrado:")
print(nrow(datos_procesados))

# Seleccionar variables para el análisis y verificar completitud
datos_modelo <- datos_procesados %>%
  select(Tipo_Formato_num, Nivel_Gravedad_num, Anio_Infraccion, Trimestre,
         Cant_Multas, Importe_Impuesto, Reincidencia_Impuesta, Categoria_Importe_num) %>%
  # Eliminar cualquier fila con NAs restantes
  drop_na()

# Verificar que no hay NAs en el dataset final
print("Verificación final de NAs:")
print(sapply(datos_modelo, function(x) sum(is.na(x))))
print(paste("Filas con datos completos:", nrow(datos_modelo)))

# Verificar que tenemos suficientes datos
if(nrow(datos_modelo) < 10) {
  stop("Error: Muy pocas observaciones después de limpiar los datos. Revisar el dataset original.")
}

# Crear variable objetivo basada en nivel de gravedad para clasificación
datos_modelo$Target_Gravedad <- factor(datos_modelo$Nivel_Gravedad_num)

print("Datos procesados para el modelo:")
print(head(datos_modelo))
print(paste("Número total de observaciones:", nrow(datos_modelo)))

# Escalar las variables predictoras y verificar
predictoras <- datos_modelo %>% select(-Target_Gravedad)
escaladas <- scale(predictoras)

# Verificar que no hay NAs después del escalado
print("NAs después del escalado:")
print(sum(is.na(escaladas)))

# Verificar dimensiones
print(paste("Dimensiones de datos escalados:", paste(dim(escaladas), collapse = " x ")))

# Si hay NAs después del escalado, investigar
if(sum(is.na(escaladas)) > 0) {
  print("Columnas con NAs después del escalado:")
  print(colnames(escaladas)[colSums(is.na(escaladas)) > 0])
  
  # Eliminar columnas que generen NAs (probablemente con varianza cero)
  escaladas <- escaladas[, colSums(is.na(escaladas)) == 0]
  print(paste("Nuevas dimensiones después de limpiar NAs:", paste(dim(escaladas), collapse = " x ")))
}

# ======================
# 2. ANÁLISIS DE CLASIFICACIÓN KNN
# ======================

# Separar en entrenamiento y prueba
set.seed(123)
trainIndex <- createDataPartition(datos_modelo$Target_Gravedad, p = .8, list = FALSE)
train_data <- escaladas[trainIndex, ]
test_data <- escaladas[-trainIndex, ]
train_labels <- datos_modelo$Target_Gravedad[trainIndex]
test_labels <- datos_modelo$Target_Gravedad[-trainIndex]

# Modelo KNN (k = 5)
modelo_knn <- knn(train = train_data, test = test_data, cl = train_labels, k = 5)

# Matriz de confusión
print("\n=== RESULTADOS CLASIFICACIÓN KNN ===")
print(confusionMatrix(modelo_knn, test_labels))

# ======================
# 3. ANÁLISIS DE CLUSTERING K-MEANS
# ======================

set.seed(123)
k3 <- kmeans(escaladas, centers = 3, nstart = 25)

# Visualizar clústeres K-means
print("\n=== CLUSTERING K-MEANS ===")
print(paste("Centros de los clústeres K-means:"))
print(k3$centers)

fviz_cluster(k3, data = escaladas, geom = "point", ellipse.type = "norm") +
  theme_minimal() +
  labs(title = "Clustering K-Means - Análisis de Infracciones",
       subtitle = "Variables: Tipo, Gravedad, Año, Trimestre, Cantidad, Importe, Reincidencia")

# Método del codo para K-means
wss_kmeans <- sapply(1:10, function(k) {
  kmeans(escaladas, k, nstart = 10)$tot.withinss
})

plot(1:10, wss_kmeans, type = "b", pch = 19, frame = FALSE,
     xlab = "Número de Clústeres (k)",
     ylab = "Suma de Cuadrados Intra-clúster",
     main = "Método del Codo K-Means - Infracciones")

# ======================
# 4. ANÁLISIS DE DISTANCIAS MÚLTIPLES
# ======================

print("\n=== COMPARACIÓN DE DIFERENTES DISTANCIAS ===")

# Calcular diferentes tipos de distancias
dist_euclidean <- dist(escaladas, method = "euclidean")
dist_manhattan <- dist(escaladas, method = "manhattan")
dist_maximum <- dist(escaladas, method = "maximum")
dist_canberra <- dist(escaladas, method = "canberra")
dist_minkowski <- dist(escaladas, method = "minkowski", p = 3)

print("1. Distancia Euclidiana (primeras 6x6):")
print(round(as.matrix(dist_euclidean)[1:6, 1:6], 3))

print("\n2. Distancia Manhattan (primeras 6x6):")
print(round(as.matrix(dist_manhattan)[1:6, 1:6], 3))

print("\n3. Distancia Máxima (primeras 6x6):")
print(round(as.matrix(dist_maximum)[1:6, 1:6], 3))

# Lista de distancias para análisis comparativo
distancias <- list(
  "Euclidiana" = dist_euclidean,
  "Manhattan" = dist_manhattan,
  "Máxima" = dist_maximum,
  "Canberra" = dist_canberra,
  "Minkowski(p=3)" = dist_minkowski
)

# ======================
# 5. CLUSTERING JERÁRQUICO - MÉTODO DEL VECINO MÁS CERCANO
# ======================

print("\n=== CLUSTERING JERÁRQUICO - VECINO MÁS CERCANO ===")

# Análisis con método single linkage (vecino más cercano)
cluster_single <- hclust(dist_euclidean, method = "single")

# Dendrograma básico
plot(cluster_single, 
     main = "Dendrograma - Método del Vecino Más Cercano",
     sub = "Distancia Euclidiana - Análisis de Infracciones",
     xlab = "Observaciones",
     ylab = "Distancia",
     cex = 0.6,
     hang = -1)

# Agregar rectángulos para diferentes números de clústeres
rect.hclust(cluster_single, k = 3, border = "red")
rect.hclust(cluster_single, k = 2, border = "blue")

# Dendrograma elegante con ggplot2
dend_data_single <- dendro_data(cluster_single)

ggplot() +
  geom_segment(data = dend_data_single$segments, 
               aes(x = x, y = y, xend = xend, yend = yend)) +
  theme_minimal() +
  labs(title = "Dendrograma - Análisis Jerárquico de Infracciones",
       subtitle = "Método: Vecino Más Cercano | Distancia: Euclidiana",
       x = "Observaciones",
       y = "Distancia")

# Análisis de clústeres con diferentes k
for(k in 2:4) {
  clusters_single <- cutree(cluster_single, k = k)
  cat(paste("\nClústeres Vecino Más Cercano para k =", k, ":\n"))
  print(table(clusters_single))
}

# ======================
# 6. CLUSTERING JERÁRQUICO - MÉTODO DE WARD
# ======================

print("\n=== CLUSTERING JERÁRQUICO - MÉTODO DE WARD ===")

# Análisis con método Ward
cluster_ward <- hclust(dist_euclidean, method = "ward.D2")

# Dendrograma Ward
plot(cluster_ward, 
     main = "Dendrograma - Método de Ward",
     sub = "Distancia Euclidiana al cuadrado - Análisis de Infracciones",
     xlab = "Observaciones",
     ylab = "Distancia",
     cex = 0.6,
     hang = -1)

# Agregar rectángulos
rect.hclust(cluster_ward, k = 3, border = "red")
rect.hclust(cluster_ward, k = 2, border = "blue")

# Dendrograma Ward elegante
dend_data_ward <- dendro_data(cluster_ward)

ggplot() +
  geom_segment(data = dend_data_ward$segments, 
               aes(x = x, y = y, xend = xend, yend = yend)) +
  theme_minimal() +
  labs(title = "Dendrograma Ward - Análisis de Infracciones",
       subtitle = "Método: Ward D2 | Distancia: Euclidiana al cuadrado",
       x = "Observaciones",
       y = "Distancia")

# Análisis de clústeres Ward con diferentes k
for(k in 2:4) {
  clusters_ward <- cutree(cluster_ward, k = k)
  cat(paste("\nClústeres Ward para k =", k, ":\n"))
  print(table(clusters_ward))
}

# ======================
# 7. COMPARACIÓN DE DENDROGRAMAS CON DIFERENTES MÉTODOS
# ======================

print("\n=== COMPARACIÓN DE MÉTODOS DE ENLACE ===")

# Crear layout para múltiples gráficos
par(mfrow = c(2, 3))

# Diferentes métodos de enlace
metodos <- c("single", "complete", "average", "ward.D2", "mcquitty", "centroid")
nombres_metodos <- c("Vecino Más Cercano", "Enlace Completo", "Enlace Promedio", 
                     "Ward", "McQuitty", "Centroide")

for(i in 1:length(metodos)) {
  cluster_temp <- hclust(dist_euclidean, method = metodos[i])
  
  plot(cluster_temp, 
       main = nombres_metodos[i],
       sub = "",
       xlab = "Observaciones",
       ylab = "Distancia",
       cex = 0.5,
       hang = -1)
}

# Restaurar layout normal
par(mfrow = c(1, 1))

# ======================
# 8. FUNCIÓN PARA ANALIZAR CUALQUIER DISTANCIA
# ======================

analizar_distancia_completa <- function(tipo_distancia = "euclidean", metodo_enlace = "ward.D2", p_value = 2) {
  
  cat(paste("\n=== ANÁLISIS CON DISTANCIA", toupper(tipo_distancia), "Y MÉTODO", toupper(metodo_enlace), "===\n"))
  
  # Calcular distancia según el tipo
  if(tipo_distancia == "minkowski") {
    dist_temp <- dist(escaladas, method = tipo_distancia, p = p_value)
    cat(paste("Parámetro p =", p_value, "\n"))
  } else {
    dist_temp <- dist(escaladas, method = tipo_distancia)
  }
  
  # Análisis de clúster
  cluster_temp <- hclust(dist_temp, method = metodo_enlace)
  
  # Mostrar estadísticas básicas
  cat(paste("Número de observaciones:", nrow(escaladas), "\n"))
  cat(paste("Altura máxima del dendrograma:", round(max(cluster_temp$height), 3), "\n"))
  
  # Mostrar clústeres para k=3
  clusters_temp <- cutree(cluster_temp, k = 3)
  cat("\nDistribución de clústeres (k=3):\n")
  print(table(clusters_temp))
  
  # Crear dendrograma
  plot(cluster_temp, 
       main = paste("Dendrograma -", stringr::str_to_title(tipo_distancia), "/", stringr::str_to_title(metodo_enlace)),
       sub = ifelse(tipo_distancia == "minkowski", paste("p =", p_value), ""),
       xlab = "Observaciones",
       ylab = "Distancia",
       cex = 0.6,
       hang = -1)
  rect.hclust(cluster_temp, k = 3, border = "red")
  
  return(list(distancia = dist_temp, cluster = cluster_temp, grupos = clusters_temp))
}

# ======================
# 9. EJEMPLOS DE ANÁLISIS CON DIFERENTES COMBINACIONES
# ======================

# Análisis con diferentes combinaciones de distancia y método
resultado_manhattan_ward <- analizar_distancia_completa("manhattan", "ward.D2")
resultado_maximum_single <- analizar_distancia_completa("maximum", "single")
resultado_canberra_complete <- analizar_distancia_completa("canberra", "complete")

# ======================
# 10. VISUALIZACIÓN DE CLÚSTERES EN ESPACIO BIDIMENSIONAL
# ======================

# PCA para visualización
pca_result <- prcomp(escaladas, center = TRUE, scale. = TRUE)

# Crear dataframe para visualización
datos_viz <- data.frame(
  PC1 = pca_result$x[,1],
  PC2 = pca_result$x[,2],
  Kmeans = as.factor(k3$cluster),
  Ward = as.factor(cutree(cluster_ward, k = 3)),
  Single = as.factor(cutree(cluster_single, k = 3)),
  Gravedad = datos_modelo$Target_Gravedad,
  Categoria_Importe = factor(datos_procesados$Categoria_Importe[complete.cases(datos_modelo)], 
                             levels = c("Bajo", "Medio", "Alto"))
)

# Gráfico comparativo de métodos de clustering
library(gridExtra)

p1 <- ggplot(datos_viz, aes(x = PC1, y = PC2, color = Kmeans)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(title = "K-Means", color = "Clúster") +
  scale_color_brewer(type = "qual", palette = "Set1")

p2 <- ggplot(datos_viz, aes(x = PC1, y = PC2, color = Ward)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Ward", color = "Clúster") +
  scale_color_brewer(type = "qual", palette = "Set2")

p3 <- ggplot(datos_viz, aes(x = PC1, y = PC2, color = Single)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Vecino Más Cercano", color = "Clúster") +
  scale_color_brewer(type = "qual", palette = "Set3")

p4 <- ggplot(datos_viz, aes(x = PC1, y = PC2, color = Gravedad)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Nivel de Gravedad Real", color = "Gravedad") +
  scale_color_manual(values = c("1" = "green", "2" = "orange", "3" = "red"))

grid.arrange(p1, p2, p3, p4, ncol = 2, top = "Comparación de Métodos de Clustering - Análisis de Infracciones")

# ======================
# 11. ANÁLISIS ESPECÍFICO PARA INFRACCIONES
# ======================

# Análisis por tipo de formato
print("\n=== ANÁLISIS POR TIPO DE FORMATO ===")
if(exists("datos_procesados")) {
  formato_analisis <- datos_procesados %>%
    group_by(Tipo_Formato) %>%
    summarise(
      n = n(),
      importe_promedio = mean(Importe_Impuesto, na.rm = TRUE),
      multas_promedio = mean(Cant_Multas, na.rm = TRUE),
      reincidencia_promedio = mean(Reincidencia_Impuesta, na.rm = TRUE),
      .groups = 'drop'
    )
  print(formato_analisis)
}

# Análisis temporal
print("\n=== ANÁLISIS TEMPORAL ===")
temporal_analisis <- datos_procesados %>%
  group_by(Anio_Infraccion, Trimestre) %>%
  summarise(
    n = n(),
    importe_total = sum(Importe_Impuesto, na.rm = TRUE),
    multas_total = sum(Cant_Multas, na.rm = TRUE),
    .groups = 'drop'
  )
print(temporal_analisis)

# ======================
# 12. RESULTADOS FINALES Y EXPORTACIÓN
# ======================

# Agregar todos los resultados de clustering al dataset
datos_modelo$Cluster_Kmeans <- k3$cluster
datos_modelo$Cluster_Ward <- cutree(cluster_ward, k = 3)
datos_modelo$Cluster_Single <- cutree(cluster_single, k = 3)
datos_modelo$PC1 <- pca_result$x[,1]
datos_modelo$PC2 <- pca_result$x[,2]

# Estadísticas descriptivas por clúster
print("\n=== ESTADÍSTICAS DESCRIPTIVAS POR CLÚSTER (K-MEANS) ===")
print(datos_modelo %>%
        group_by(Cluster_Kmeans) %>%
        summarise(
          n = n(),
          importe_promedio = mean(Importe_Impuesto),
          multas_promedio = mean(Cant_Multas),
          reincidencia_promedio = mean(Reincidencia_Impuesta),
          anio_promedio = mean(Anio_Infraccion),
          .groups = 'drop'
        ))

print("\n=== ESTADÍSTICAS DESCRIPTIVAS POR CLÚSTER (WARD) ===")
print(datos_modelo %>%
        group_by(Cluster_Ward) %>%
        summarise(
          n = n(),
          importe_promedio = mean(Importe_Impuesto),
          multas_promedio = mean(Cant_Multas),
          reincidencia_promedio = mean(Reincidencia_Impuesta),
          anio_promedio = mean(Anio_Infraccion),
          .groups = 'drop'
        ))

# Tabla de contingencia entre métodos
print("\n=== COMPARACIÓN ENTRE MÉTODOS DE CLUSTERING ===")
print("K-Means vs Ward:")
print(table(datos_modelo$Cluster_Kmeans, datos_modelo$Cluster_Ward))

print("\nK-Means vs Vecino Más Cercano:")
print(table(datos_modelo$Cluster_Kmeans, datos_modelo$Cluster_Single))

# Exportar resultados
write.csv(datos_modelo, "resultados_clustering_infracciones.csv", row.names = FALSE)

cat("\n=== ANÁLISIS COMPLETADO ===\n")
cat("Archivo exportado: resultados_clustering_infracciones.csv\n")
cat("Métodos aplicados: KNN, K-Means, Ward, Vecino Más Cercano\n")
cat("Distancias analizadas: Euclidiana, Manhattan, Máxima, Canberra, Minkowski\n")
cat("Variables analizadas: Tipo de Formato, Nivel de Gravedad, Año, Trimestre, Cantidad de Multas, Importe, Reincidencia\n")

# Información adicional sobre las variables
cat("\n=== INFORMACIÓN SOBRE LAS VARIABLES ===\n")
cat("Variables originales transformadas:\n")
cat("- Código falta: No utilizada en clustering (identificador)\n")
cat("- Descripción falta: No utilizada en clustering (texto)\n")
cat("- Tipo formato: Convertida a numérica\n")
cat("- Nivel de gravedad: Convertida a escala 1-3\n")
cat("- Año infracción: Utilizada directamente\n")
cat("- Mes infracción: Convertida a trimestres\n")
cat("- Cant. multas impuestas: Utilizada directamente\n")
cat("- Importe impuesto: Utilizada directamente + categorizada\n")
cat("- Reincidencia impuesta: Utilizada directamente\n")