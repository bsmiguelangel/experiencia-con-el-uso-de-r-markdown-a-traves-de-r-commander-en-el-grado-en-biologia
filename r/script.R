#### Paquetes requeridos ####

## install.packages("pacman")

pacman::p_load(readxl, ggplot2, graphics, wordcloud, tm, install = FALSE)

#### Carga de datos ####

rm(list = ls())
encuesta <- read_excel(file.path("data", "EncuestaRMD2025.xlsx"), col_names = TRUE)

encuesta$P0 <- factor(encuesta$P0, levels = c("Castellano", "Valenciano"))
encuesta$P1 <- factor(encuesta$P1, levels = c("BI1", "BI2", "AI1", "AI2"))
encuesta$P2 <- factor(encuesta$P2, levels = c("No", "Si"))
encuesta$P3 <- factor(encuesta$P3, levels = c("No", "Si"))
encuesta$P4 <- factor(encuesta$P4, levels = c("No", "Si"))
encuesta$P5 <- factor(encuesta$P5, levels = c("No", "Si"))
encuesta$P6 <- factor(encuesta$P6, levels = c("No", "Si"))
encuesta$P7 <- factor(encuesta$P7, levels = c("Muy facil", "Facil", "Normal", "Dificil", "Muy dificil"))
encuesta$P8 <- factor(encuesta$P8, levels = c("Facilitado mucho", "Facilitado", "Normal", "Dificultado", "Dificultado mucho"))
encuesta$P9 <- factor(encuesta$P9, levels = c("Muy satisfecho", "Satisfecho", "Normal", "Insatisfecho", "Muy insatisfecho"))
encuesta$P10 <- factor(encuesta$P10, levels = c("No", "Si"))
encuesta$P11 <- factor(encuesta$P11, levels = c("Documento de texto", "R Markdown"))
encuesta$P12 <- factor(encuesta$P12, levels = c("No", "Si"))

str(encuesta)

#### Ítem 1: ¿Cuál es tu Subgrupo de Prácticas? ####

df <- data.frame(grupo = rep(levels(encuesta$P0), each = 2),
                 subgrupo = levels(encuesta$P1),
                 frecuencia = as.numeric(table(encuesta$P1)))
df$porcentaje <- paste0("(", round(df$frecuencia/sum(df$frecuencia) * 100, 2), "%", ")")
df

p <- ggplot(data = df, aes(x = subgrupo, y = frecuencia, fill = grupo)) + 
  geom_bar(stat = "identity") +
  theme_bw() + labs(x = "Subgrupo", y = "Frecuencia", fill = "Grupo") +
  geom_text(aes(label = frecuencia), vjust = 2.0, color = "white", size = 3.5) +
  geom_text(aes(label = porcentaje), vjust = 4.0, color = "white", size = 3.5) +
  scale_fill_manual(values = c("Castellano" = "tomato", "Valenciano" = "steelblue"))
p

#### Ítems 2 y 3: ¿Conocías la existencia de R y R Markdown? ####

summary(encuesta[, c("P2", "P3")])

#### Ítem 4: ¿Has sentido frustración mientras aprendías R Markdown? ####

df4 <- data.frame(subgrupo = rep(levels(encuesta$P1), each = length(levels(encuesta$P4))),
                  respuesta = rep(levels(encuesta$P4), times = length(levels(encuesta$P1))),
                  frecuencia = as.numeric(unlist(by(encuesta$P4, encuesta$P1, table))))
df4$porcentaje <- paste0("(", round(as.numeric(unlist(by(encuesta$P4, encuesta$P1, function(x) {table(x)/sum(table(x))}))) * 100, 2), "%", ")")
df4

p <- ggplot(data = df4, aes(x = subgrupo, y = frecuencia, fill = respuesta)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  theme_bw() + labs(x = "Subgrupo", y = "Frecuencia", fill = "Respuesta") +
  geom_text(aes(label = frecuencia), vjust = 1.75, position = position_dodge(0.9), 
            color = "white", size = 2.75) +
  geom_text(aes(label = porcentaje), vjust = 3.5, position = position_dodge(0.9), 
            color = "white", size = 2.75) +
  scale_fill_manual(values = c("No" = "forestgreen", "Si" = "firebrick"))
p

#### Ítem 5: En caso afirmativo, ¿esa frustración ha terminado por desaparecer? ####

df5 <- data.frame(subgrupo = rep(levels(encuesta$P1), each = length(levels(encuesta$P5))),
                  respuesta = rep(levels(encuesta$P5), times = length(levels(encuesta$P1))),
                  frecuencia = as.numeric(unlist(by(encuesta$P5[which(encuesta$P4 == "Si")], encuesta$P1[which(encuesta$P4 == "Si")], table))))
df5$porcentaje <- paste0("(", round(as.numeric(unlist(by(encuesta$P5[which(encuesta$P4 == "Si")], encuesta$P1[which(encuesta$P4 == "Si")], function(x) {table(x)/sum(table(x))}))) * 100, 2), "%", ")")
df5

p <- ggplot(data = df5, aes(x = subgrupo, y = frecuencia, fill = respuesta)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  theme_bw() + labs(x = "Subgrupo", y = "Frecuencia", fill = "Respuesta") +
  geom_text(aes(label = frecuencia), vjust = 1.75, position = position_dodge(0.9), 
            color = "white", size = 2.75) +
  geom_text(aes(label = porcentaje), vjust = 3.5, position = position_dodge(0.9), 
            color = "white", size = 2.75) +
  scale_fill_manual(values = c("No" = "firebrick", "Si" = "forestgreen")) +
  scale_y_continuous(breaks = seq(0, 10, by = 2))
p

#### Ítem 6: ¿Consideras que R Markdown te ha facilitado la entrega de tareas? ####

df6 <- data.frame(subgrupo = rep(levels(encuesta$P1), each = length(levels(encuesta$P6))),
                  respuesta = rep(levels(encuesta$P6), times = length(levels(encuesta$P1))),
                  frecuencia = as.numeric(unlist(by(encuesta$P6, encuesta$P1, table))))
df6$porcentaje <- paste0("(", round(as.numeric(unlist(by(encuesta$P6, encuesta$P1, function(x) {table(x)/sum(table(x))}))) * 100, 2), "%", ")")
df6

p <- ggplot(data = df6, aes(x = subgrupo, y = frecuencia, fill = respuesta)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  theme_bw() + labs(x = "Subgrupo", y = "Frecuencia", fill = "Respuesta") +
  geom_text(aes(label = frecuencia), vjust = 1.75, position = position_dodge(0.9), 
            color = "white", size = 1.75) +
  geom_text(aes(label = porcentaje), vjust = 3.5, position = position_dodge(0.9), 
            color = "white", size = 1.75) +
  scale_fill_manual(values = c("No" = "firebrick", "Si" = "forestgreen"))
p

#### Ítem 7: ¿Cómo calificarías la dificultad de aprender R Markdown? ####

df7 <- data.frame(grupo = as.character(encuesta$P0),
                  subgrupo = as.character(encuesta$P1),
                  respuesta = as.numeric(encuesta$P7))
df7

p <- ggplot(data = subset(df7, !is.na(subgrupo)), aes(x = subgrupo, y = respuesta)) + 
  geom_dotplot(aes(fill = grupo), binaxis = "y", stackdir = "center", dotsize = 0.75) +
  theme_bw() + labs(x = "Subgrupo", y = "Respuesta", fill = "Grupo") +
  scale_fill_manual(values = c("Castellano" = "tomato", "Valenciano" = "steelblue")) + 
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  scale_y_continuous(breaks = 1:length(levels(encuesta$P7)),
                     labels = c("Muy facil (1)", "Facil (2)", "Normal (3)", "Dificil (4)", "Muy dificil (5)"))
p

#### Ítem 8: ¿R Markdown te ha facilitado comprender los análisis estadísticos? ####

df8 <- data.frame(grupo = as.character(encuesta$P0),
                  subgrupo = as.character(encuesta$P1),
                  respuesta = as.numeric(encuesta$P8))
df8

p <- ggplot(data = subset(df8, !is.na(subgrupo)), aes(x = subgrupo, y = respuesta)) + 
  geom_dotplot(aes(fill = grupo), binaxis = "y", stackdir = "center", dotsize = 0.75) +
  theme_bw() + labs(x = "Subgrupo", y = "Respuesta", fill = "Grupo") +
  scale_fill_manual(values = c("Castellano" = "tomato", "Valenciano" = "steelblue")) + 
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  scale_y_continuous(breaks = 1:length(levels(encuesta$P7)),
                     labels = c("Facil. mucho (1)", "Facilitado (2)", "Normal (3)", "Dificultado (4)", "Dific. mucho (5)"))
p

#### Ítem 9: ¿Cuál ha sido tu grado de satisfacción utilizando R Markdown? ####

df9 <- data.frame(grupo = as.character(encuesta$P0),
                  subgrupo = as.character(encuesta$P1),
                  respuesta = as.numeric(encuesta$P9))
df9

p <- ggplot(data = subset(df9, !is.na(subgrupo)), aes(x = subgrupo, y = respuesta)) + 
  geom_dotplot(aes(fill = grupo), binaxis = "y", stackdir = "center", dotsize = 0.75) +
  theme_bw() + labs(x = "Subgrupo", y = "Respuesta", fill = "Grupo") +
  scale_fill_manual(values = c("Castellano" = "tomato", "Valenciano" = "steelblue")) + 
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  scale_y_continuous(breaks = 1:length(levels(encuesta$P7)),
                     labels = c("Muy satisf. (1)", "Satisfecho (2)", "Normal (3)", "Insatisfecho (4)", "Muy insatisf. (5)"))
p

#### Ítem 10: ¿Consideras que R Markdown es una herramienta útil? ####

df10 <- data.frame(subgrupo = rep(levels(encuesta$P1), each = length(levels(encuesta$P10))),
                   respuesta = rep(levels(encuesta$P10), times = length(levels(encuesta$P1))),
                   frecuencia = as.numeric(unlist(by(encuesta$P10, encuesta$P1, table))))
df10$porcentaje <- paste0("(", round(as.numeric(unlist(by(encuesta$P10, encuesta$P1, function(x) {table(x)/sum(table(x))}))) * 100, 2), "%", ")")
df10

p <- ggplot(data = df10, aes(x = subgrupo, y = frecuencia, fill = respuesta)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  theme_bw() + labs(x = "Subgrupo", y = "Frecuencia", fill = "Respuesta") +
  geom_text(aes(label = frecuencia), vjust = 1.75, position = position_dodge(0.9), 
            color = "white", size = 1.75) +
  geom_text(aes(label = porcentaje), vjust = 3.5, position = position_dodge(0.9), 
            color = "white", size = 1.75) +
  scale_fill_manual(values = c("No" = "firebrick", "Si" = "forestgreen"))
p

#### Ítem 11: En el futuro, ¿recurrirías a Word o R Markdown? ####

levels(encuesta$P11) <- c("Word", "R Markdown")
df11 <- data.frame(subgrupo = rep(levels(encuesta$P1), each = length(levels(encuesta$P11))),
                   respuesta = rep(levels(encuesta$P11), times = length(levels(encuesta$P1))),
                   frecuencia = as.numeric(unlist(by(encuesta$P11, encuesta$P1, table))))
df11$porcentaje <- paste0("(", round(as.numeric(unlist(by(encuesta$P11, encuesta$P1, function(x) {table(x)/sum(table(x))}))) * 100, 2), "%", ")")
df11

p <- ggplot(data = df11, aes(x = subgrupo, y = frecuencia, fill = respuesta)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  theme_bw() + labs(x = "Subgrupo", y = "Frecuencia", fill = "Respuesta") +
  geom_text(aes(label = frecuencia), vjust = 1.75, position = position_dodge(0.9), 
            color = "white", size = 2.75) +
  geom_text(aes(label = porcentaje), vjust = 3.5, position = position_dodge(0.9), 
            color = "white", size = 2.75) +
  scale_fill_manual(values = c("Word" = "steelblue", "R Markdown" = "forestgreen")) +
  scale_y_continuous(breaks = seq(0, 10, by = 2))
p

#### Ítem 12: ¿Recomendarías el aprendizaje de R Markdown? ####

df12 <- data.frame(subgrupo = rep(levels(encuesta$P1), each = length(levels(encuesta$P12))),
                   respuesta = rep(levels(encuesta$P12), times = length(levels(encuesta$P1))),
                   frecuencia = as.numeric(unlist(by(encuesta$P12, encuesta$P1, table))))
df12$porcentaje <- paste0("(", round(as.numeric(unlist(by(encuesta$P12, encuesta$P1, function(x) {table(x)/sum(table(x))}))) * 100, 2), "%", ")")
df12

p <- ggplot(data = df12, aes(x = subgrupo, y = frecuencia, fill = respuesta)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  theme_bw() + labs(x = "Subgrupo", y = "Frecuencia", fill = "Respuesta") +
  geom_text(aes(label = frecuencia), vjust = 1.75, position = position_dodge(0.9), 
            color = "white", size = 1.75) +
  geom_text(aes(label = porcentaje), vjust = 3.5, position = position_dodge(0.9), 
            color = "white", size = 1.75) +
  scale_fill_manual(values = c("No" = "firebrick", "Si" = "forestgreen")) +
  scale_y_continuous(breaks = seq(0, 12, by = 3))
p

#### Ítem 13: Lo que más me ha gustado de R Markdown es... ####

# Vector de texto
encuesta$P13

text_vector <- c(
  "Las salidas que daba eran bastante intuitivas y fáciles de comprender",
  "Me gustan los colores de las gráficas",
  "Simplifica mucho hacer los cálculos y los test",
  "La facilidad para realizar los test",
  "Que tanto la instrucción como el resultado se copian al instante; es mucho más rápido que si hubiera que hacerlo en otro documento",
  "Poder generar informes de manera más rápida y sencilla",
  NA,
  "Creo que, si sabes usarlo bien, puede ser muy útil",
  NA,
  "Sobre todo, me gustan los gráficos, que son muy útiles",
  "Que puedas escribir texto y mantener el formato de programación, a diferencia de copiar, pegar o añadir imágenes en Word",
  "Me parece un programa muy intuitivo",
  "La versatilidad del programa para realizar análisis y redactar informes",
  "Me ha ayudado bastante a entender la teoría y los problemas de la asignatura",
  "Al principio, al ser nuevo, costó un poco adaptarse al lenguaje del programa y a todas sus opciones. Pero al final logré controlarlo relativamente bien y fue bastante interesante",
  "Facilidad para obtener análisis",
  "La forma en que los profesores lo habéis impartido (he asistido a clases de ambos grupos), cómo la teoría se aplica a la práctica y su utilidad para el examen",
  "Lo que más me ha gustado es su comodidad: desde R puedo ir haciendo el informe, añadiendo las salidas, y al final descargarlo para ver todos los pasos seguidos durante la práctica. Muy satisfecha",
  "Solo necesitas escribir en R; no hay que repetir todo, y las gráficas y el código aparecen automáticamente en R Markdown. No necesito copiar y pegar nada. También se puede copiar y pegar el código dentro de R. El fichero de R Markdown está muy bien organizado",
  "Me ha gustado todo",
  "Queda guardado y puedes ver la práctica que has hecho y abrirla en R para completarla o modificarla. Las gráficas aparecen directamente en el documento, todo es más rápido",
  "Es muy útil para integrar las explicaciones (teóricas como los contrastes de hipótesis) con las salidas de R (script), de forma que no tienes que hacer capturas de pantalla ni pegarlas en Word",
  NA,
  NA,
  NA,
  NA,
  NA,
  "La facilidad para generar gráficas a partir de los datos",
  NA,
  NA,
  "La facilidad de lectura",
  NA,
  NA,
  "La facilidad que ofrece para analizar datos estadísticos",
  NA,
  NA,
  "Nada",
  "Ver los diagramas, que son muy explicativos y útiles. También me ha gustado ver cómo un problema de clase se puede resolver con un programa informático",
  "Los gráficos",
  "Que puedes ver los gráficos, y eso es muy visual",
  "La facilidad para comprender los conceptos y los resultados",
  "El hecho de poder hacer informes de forma tan sencilla, incluyendo datos estadísticos y gráficas directamente desde el programa",
  NA,
  NA,
  "Es intuitivo",
  NA,
  NA
)

# Convierte a un corpus
corpus <- Corpus(VectorSource(text_vector))

# Limpieza básica del texto
corpus <- tm_map(corpus, removePunctuation)                    # elimina puntuación
corpus <- tm_map(corpus, content_transformer(tolower))         # a minúsculas
corpus <- tm_map(corpus, removeNumbers)                        # elimina números
corpus <- tm_map(corpus, stripWhitespace)                      # elimina muchos espacios
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))    # elimina stopwords en español

# wordcloud(corpus, scale = c(2, 1), min.freq = 3, colors = rainbow(30))

# Crear matriz de términos
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)

# Dibujar la nube
par(mar = c(1, 1, 1, 1))
wordcloud(names(word_freqs), scale = c(2, 0.75), word_freqs, min.freq = 2, 
          colors = rainbow(30))

#### Ítem 14: Lo que menos me ha gustado de R Markdown es... ####

# Vector de texto
encuesta$P14

text_vector <- c(
  "Algunos comandos eran complejos",
  "No me gustan las gráficas sin colores",
  "Que a veces no sabíamos generar informes o daba problemas o se quedaba pillado",
  "La dificultad para organizar el informe, ya que copiar y pegar y los tests se ponen al final",
  "Puede ser un poco lioso a la hora de ordenar el documento",
  "Que al realizar informes y pruebas estadísticas, los resultados quedaban en la parte inferior, aunque me parece mejor que Word",
  NA,
  "No siempre tenía claros los pasos y me atascaba en la tarea",
  NA,
  "Si no sabes qué test hacer, es un poco difícil",
  "Tener que revisar constantemente si lo que escribí se había generado o no; debería haber una opción que permitiera hacer esto más fácilmente",
  "A veces las opciones eran un poco confusas",
  "Al principio es frustrante entenderlo y los comandos no son intuitivos",
  "Es frustrante que haya atajos para ciertas funciones pero no para todas, porque aunque estén en la presentación, al no explicarnos cómo escribirlos ni cómo funciona el lenguaje de R (si admite ciertos signos, tildes, etc.), se hace complicado y casi siempre se requiere la ayuda del profesor, lo cual rompe la dinámica del ejercicio",
  NA,
  NA,
  "No encuentro nada negativo",
  NA,
  "En R es fácil perderse en el código y a veces es complicado cambiar lo que quieres que aparezca en el R Markdown",
  "No me gustó cuando tuve que usarlo en mi ordenador porque era diferente al de las prácticas",
  "Cuando escribes en una sola línea continua, a partir de cierta longitud no puedes ver el final de la frase, lo que dificulta ver los ejercicios sin generar el informe",
  "A nivel de estilo, no se ve todo el texto en la misma pantalla, tienes que desplazarte horizontalmente, y a veces marea porque no sabes si estás escribiendo o no ni qué. Tampoco puedes ver simultáneamente los datos a los que te refieres",
  NA,
  NA,
  NA,
  NA,
  NA,
  "La interfaz es poco intuitiva",
  NA,
  NA,
  "Los gráficos del examen",
  NA,
  NA,
  "A veces cuesta relacionar lo que pide el ejercicio con lo que hay que usar en R",
  NA,
  NA,
  "No se entiende",
  "A veces no funciona bien: no genera el informe o no se transcriben los datos del fichero",
  NA,
  "La dificultad para aprenderlo, ya que a veces no era muy intuitivo",
  "La dificultad que había a veces para encontrar el método a ejecutar en el programa",
  NA,
  NA,
  NA,
  "Creo que Excel es más útil, tanto para análisis estadísticos como para otras funciones",
  NA,
  NA
)

# Convierte a un corpus
corpus <- Corpus(VectorSource(text_vector))

# Limpieza básica del texto
corpus <- tm_map(corpus, removePunctuation)                    # elimina puntuación
corpus <- tm_map(corpus, content_transformer(tolower))         # a minúsculas
corpus <- tm_map(corpus, removeNumbers)                        # elimina números
corpus <- tm_map(corpus, stripWhitespace)                      # elimina muchos espacios
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))    # elimina stopwords en español
corpus <- tm_map(corpus, removeWords, c("veces"))

# Crear matriz de términos
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)

# Dibujar la nube
par(mar = c(1, 1, 1, 1))
wordcloud(names(word_freqs), scale = c(2, 0.75), word_freqs, min.freq = 2, 
          colors = rainbow(30))

#### Ítem 15: Lo que más me ha costado de R Markdown es... ####

# Vector de texto
encuesta$P15

text_vector <- c(
  "Entender dónde buscar cada tipo de test",
  "Trabajar con mi compañero",
  "Aprender a hacerlo todo desde el principio y saber dónde está cada cosa",
  "Entender la interfaz y los pasos a seguir",
  "Nada en particular",
  "Acostumbrarme al principio",
  NA,
  "Es un programa informático, lo cual me cuesta personalmente",
  NA,
  "Saber qué hacer con los datos; luego manejarlo en R Markdown es bastante fácil",
  "Poco, es un programa muy claro e interactivo",
  "Aprender qué disposición tiene cada opción",
  "Aprender los comandos",
  "Al principio seguir el ritmo del profesor, pero luego bien",
  NA,
  NA,
  "Alguna práctica en la que tenía ciertas dudas, pero una vez resueltas no resultaba costoso",
  "Al principio cuesta cogerle el truco, pero en la segunda práctica ya lo manejas al 100%",
  "Cambiar cosas dentro de R es un poco confuso a veces",
  NA,
  "A veces no tienes claro cuál es el archivo generado si has guardado más cosas, puedes equivocarte al subir el archivo a otro sitio",
  "Guardar el informe en un archivo válido para subirlo a la tarea",
  NA,
  NA,
  "Entender dónde tengo que mirar",
  "Saber qué tests aplicar en cada ejercicio",
  NA,
  "Interpretar los datos",
  NA,
  NA,
  "Encontrar algunas funciones e interpretar algunos resultados",
  NA,
  NA,
  "Saber qué opciones marcar en algunas herramientas para establecer ciertas condiciones al analizar los datos",
  NA,
  NA,
  "Todo",
  "Aprender cómo se hacen algunos procedimientos o cuándo hay que marcar algunas casillas como agrupar por grupos, o lo que sea. A veces también me costaba hacer ciertos procedimientos porque no entendía el razonamiento",
  "Ciertas funciones",
  "Buscar a dónde ir cuando el ejercicio pedía ciertos gráficos",
  "Encontrar el método a ejecutar dentro del programa",
  "Aprender a interpretar los datos, sobre todo de las tablas de contingencia",
  NA,
  NA,
  NA,
  NA,
  NA
)

# Convierte a un corpus
corpus <- Corpus(VectorSource(text_vector))

# Limpieza básica del texto
corpus <- tm_map(corpus, removePunctuation)                    # elimina puntuación
corpus <- tm_map(corpus, content_transformer(tolower))         # a minúsculas
corpus <- tm_map(corpus, removeNumbers)                        # elimina números
corpus <- tm_map(corpus, stripWhitespace)                      # elimina muchos espacios
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))    # elimina stopwords en español

# Crear matriz de términos
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)

# Dibujar la nube
par(mar = c(1, 1, 1, 1))
wordcloud(names(word_freqs), scale = c(2, 0.75), word_freqs, min.freq = 2, 
          colors = rainbow(30))

#### Ítem 16: ¿Cómo mejorarías las clases en el uso de R Markdown? ####

# Vector de texto
encuesta$P16

text_vector <- c(
  "No tengo ninguna aportación",
  "No sé",
  "Quizás hacerlas en grupos más pequeños para que sea más fácil seguir la clase",
  "Concretar más en cada parte",
  "Incluir en los guiones de las prácticas las claves necesarias para usarlo, como ordenarlo y luego guardarlo en ambos formatos, ya que solemos olvidar cómo hacerlo",
  "No sé, me han parecido adecuadas y comprensibles",
  NA,
  "Explicar todos los pasos a seguir de forma más clara",
  NA,
  NA,
  "Pocas cosas podrían ser mejorables; quizás tener una pestaña de tiempo real del informe",
  "Aprender en grupos más reducidos",
  "No lo sé, me han parecido bastante buenas, la verdad",
  "Haría ejercicios más completos, que permitieran usar más funciones del programa, y promovería el uso individual de la herramienta. No todo el mundo aprende al mismo ritmo ni se quedará con todo igual; hay días que es difícil seguir el ritmo y otros que se hacen muy lentos. También añadiría entregas opcionales para incentivar el trabajo",
  "En general, personalmente, las he visto bastante bien organizadas y fáciles de seguir",
  NA,
  "No considero que haya mejoras que hacer :)",
  NA,
  NA,
  "Ir más despacio y hacer las cosas con más detalle",
  "Pondría una guía con todas las cosas que se pueden hacer y cómo ordenar los apartados en el R Markdown",
  "Quizás pondría menos ejercicios (o más tiempo), ya que al final de las sesiones íbamos bastante agobiados por la falta de tiempo; no era capaz de atender a la resolución del ejercicio, hacerlo en R y escribirlo",
  NA,
  NA,
  "Un enfoque distinto en las clases",
  "Explicar un poco los ejercicios para saber de qué tipo de muestra se trata y qué tests hay que hacer",
  NA,
  "Centrar las explicaciones un poco más en la navegación por las distintas pestañas de la aplicación",
  NA,
  NA,
  "Todo bien",
  NA,
  NA,
  "Una mayor relación entre cómo será el examen y la metodología de las prácticas",
  "Explicar más detalladamente su funcionamiento",
  NA,
  "Utilizar un programa más fácil",
  "Quizás ir un poco más lento, ya que a veces algunas preguntas no quedaban claras porque había que hacer muchos ejercicios",
  NA,
  "Ir un poco más despacio, porque a veces me perdía",
  "No sabría decir",
  "Creo que están bien organizadas",
  NA,
  NA,
  NA,
  NA,
  NA
)

# Convierte a un corpus
corpus <- Corpus(VectorSource(text_vector))

# Limpieza básica del texto
corpus <- tm_map(corpus, removePunctuation)                    # elimina puntuación
corpus <- tm_map(corpus, content_transformer(tolower))         # a minúsculas
corpus <- tm_map(corpus, removeNumbers)                        # elimina números
corpus <- tm_map(corpus, stripWhitespace)                      # elimina muchos espacios
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))    # elimina stopwords en español

# Crear matriz de términos
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)

# Dibujar la nube
par(mar = c(1, 1, 1, 1))
wordcloud(names(word_freqs), scale = c(2, 0.75), word_freqs, min.freq = 2, 
          colors = rainbow(30))
