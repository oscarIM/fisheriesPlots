#' @title plot_act_extractiva
#' @description Función para hacer el gráfico del "multipanel"
#' @param datos dataframe de entrada. Formato: separado por tab. (Tiene que tener: coluna que identifique a las especies/recurso, caletas, meses, años. Los meses representan el desembarco en ese mes/año)
#' @param col_especies columna que tiene que el nombre de las especies/recursos
#' @param especie Vector con el nombre de la especie que se quiera incluir en el gráfico.
#' @param col_caletas columna que tiene que el nombre de las caletas que se quieran incluir en el gráfico
#' @param orden_caletas Vector con el nombre de las caletas que se quieran incluir en el gráfico en el orde que se quiera que aparezcan.
#' @param nombre_salida_prefix prefijo usado para el conjunto de gráficos a exportar
#' @param alto altura de la imágen
#' @param ancho ancho de la imágen
#' @return imágenes png
#' @import ggplot2
#' @import dplyr
#' @import stringr
#' @importFrom janitor clean_names
#' @importFrom lubridate myd month
#' @export plot_act_extractiva
#' @examples
#' \dontrun{
#' datos <- read_tsv("datos_act_extractiva.tsv", show_col_types = FALSE)
#' col_especie <- "Recurso"
#' especie <- "Sierra"
#' col_caleta <- "Localidad"
#' orden_caletas <- c("Mehuín", "Los Molinos", "Niebla", "Corral")
#' nombre_salida_prefix <- "plots_biobio_test"
#' ancho <- 8
#' alto <- 5
#' plot_act_extractiva(datos = datos, col_especie = col_especie, especie = especie, col_caleta = col_caleta, col_especie = col_especie, orden_caletas = orden_caletas, nombre_salida_prefix = nombre_salida_prefix, ancho = ancho, alto = alto)
#' }
plot_act_extractiva <- function(datos, col_especies, especie, col_caleta, orden_caletas,
                                nombre_salida_prefix, ancho, alto) {
  ## se asume que todos los meses son del mismo año
  options(scipen = 999)
  options(dplyr.summarise.inform = FALSE)
  # desembarque total, talla promedio, tiempo faena promedio tiempo viaje
  data <- datos %>%
    filter(.[[col_caleta]] %in% orden_caletas, .[[col_especie]] %in% especie) %>%
    clean_names()
  # cambio unidades#
  unidades <- unique(data$um)
  patterns <- paste0(unidades, collapse = "|")
  data <- data %>%
    mutate(
      um = str_replace(string = um, pattern = patterns, replacement = "n_ind"),
      fecha = mdy(fecha),
      mes = month(fecha, label = TRUE, abbr = TRUE),
      mes = str_to_upper(mes)
    )
  pos_month <- c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV", "DIC")
  # pos_indx <- pos_month %in% unique(data$mes)
  # pos_month <- pos_month[pos_indx]
  # grafico barra: Desembarco total por mes y por caleta
  col_caleta <- str_to_lower(col_caleta)
  d1 <- data %>%
    group_by(mes, across(all_of(col_caleta))) %>%
    summarise(total_desembarque = sum(desembarque, na.rm = T))
  p_desem_total <- ggplot(d1, aes(x = mes, y = total_desembarque, fill = d1[[col_caleta]])) +
    geom_bar(position = "stack", stat = "identity", width = 0.5, color = "black") +
    ylab("Desembarque Total \n (N° Individuos)") +
    xlab("Meses") +
    scale_y_continuous(n.breaks = 5) +
    scale_x_discrete(limits = pos_month) +
    theme_light() +
    guides(fill = guide_legend(title = "Caletas", ncol = 1)) +
    scale_fill_discrete(limits = orden_caletas)
  ggsave(filename = paste0(nombre_salida_prefix, "_desembarco_ind.png"), plot = p_desem_total, dpi = 300, height = alto, width = ancho)
  # grafico boxplot: Desembarco promedio por mes y por caleta#
  p_desemb_promedio <- ggplot(data, aes(fill = .data[[col_caleta]], y = desembarque, x = mes)) +
    geom_boxplot() +
    xlab("Meses") +
    ylab("Desembarque Promedio \n (N° Individuos)") +
    theme_bw() +
    guides(fill = guide_legend(title = "Caletas", ncol = 1)) +
    scale_x_discrete(limits = pos_month) +
    scale_fill_discrete(limits = orden_caletas)
  ggsave(filename = paste0(nombre_salida_prefix, "_desembarco_avg.png"), plot = p_desemb_promedio, dpi = 300, height = alto, width = ancho)
  # grafico boxplot: tiempo de viaje promedio
  data_tmp <- data %>%
    mutate(horas_viaje = as.numeric(horas_viaje)) %>%
    filter(!is.na(horas_viaje))
  p_tiempo_viaje_promedio <- ggplot(data_tmp, aes(fill = .data[[col_caleta]], y = horas_viaje, x = mes)) +
    geom_boxplot() +
    xlab("Meses") +
    ylab("Tiempo Promedio Viaje \n (Horas)") +
    theme_bw() +
    guides(fill = guide_legend(title = "Caletas", ncol = 1)) +
    scale_x_discrete(limits = pos_month) +
    scale_fill_discrete(limits = orden_caletas)
  ggsave(filename = paste0(nombre_salida_prefix, "_tiempo_viaje_avg.png"), plot = p_tiempo_viaje_promedio, dpi = 300, height = alto, width = ancho)
  # grafico boxplot: tiempo de faena promedio
  data_tmp <- data %>%
    mutate(horas_faena = as.numeric(horas_faena)) %>%
    filter(!is.na(horas_faena))
  p_tiempo_faena_promedio <- ggplot(data_tmp, aes(fill = .data[[col_caleta]], y = horas_faena, x = mes)) +
    geom_boxplot() +
    xlab("Meses") +
    ylab("Tiempo Promedio Faena \n (Horas)") +
    theme_bw() +
    guides(fill = guide_legend(title = "Caletas", ncol = 1)) +
    scale_x_discrete(limits = pos_month) +
    scale_fill_discrete(limits = orden_caletas)
  ggsave(filename = paste0(nombre_salida_prefix, "_tiempo_faena_avg.png"), plot = p_tiempo_faena_promedio, dpi = 300, height = alto, width = ancho)
}