#' @title plot_multipanel
#' @description Función para realizar gráfico multipanel
#' @param datos dataframe de entrada
#' @param dicc archivo diccionario de recursos, separado por "," XXX
#' @param caletas Vector con el nombre de las caletas que se quieran incluir en el gráfico. Si NULL, se usaran
#' @param col_especies columna que tiene que el nombre de las especies/recursos
#' @param especies_rm Vector con el nombre de las especies que se quieran eliminar del gráfico en el gráfico. Si NULL, se usaran todas las especies
#' @param ylab_text_sup Cadena de texto para el eje y del panel superior. Si NULL == "Desembarque total (Ton)"
#' @param xlab_text_sup  Cadena de texto para el eje x del panel superior. Si NULL == "Años"
#' @param ylab_text_inf Cadena de texto para el eje y del panel inferior. Si NULL == "Desembarque promedio (Ton)"
#' @param xlab_text_inf Cadena de texto para el eje x del panel inferior. Si NULL == "Meses"
#' @param n_pie Número de divisiones de la torta
#' @param n_ticks Número de divisiones ejes y
#' @param nombre_salida Nombre figura incluyendo la extensión ("XXXX.png")
#' @param dec_red Número de decimales para los gráficos de torta
#' @return imágenes png
#' @import ggplot2
#' @import dplyr
#' @import stringr
#' @importFrom janitor clean_names
#' @importFrom RColorBrewer brewer.pal
#' @importFrom gridExtra grid.arrange
#' @importFrom purrr map
#' @importFrom tidyr pivot_longer
#' @export plot_multipanel
#' @examples
#' \dontrun{
#' datos <- read_tsv("data.txt", show_col_types = FALSE)
#' dicc <- read_delim("diccionario_recursos.txt", delim = ";", show_col_types = FALSE)
#' caletas <- "SAN VICENTE"
#' col_especie <- "ESPECIE"
#' especies_rm <- "SARDINA COMUN"
#' ylab_text_sup <- "eje y"
#' xlab_text_sup <- "eje x"
#' ylab_text_inf <- "eje y"
#' xlab_text_inf <- "eje x"
#' ancho <- 12
#' alto <- 8
#' n_ticks <- 6
#' nombre_salida <- "plot.png"
#' n_pie <- 5
#' dec_red <- 2
#' plot_multipanel(datos = datos, dicc = dicc, caletas = caletas, especies_rm = especies_rm, col_especie = col_especie, ylab_text_sup = ylab_text_sup, xlab_text_sup = xlab_text_sup, ylab_text_inf = ylab_text_inf, xlab_text_inf = xlab_text_inf, ancho = ancho, alto = alto, nombre_salida = nombre_salida, n_tick = n_tick, n_pie = n_pie, dec_red = dec_red)
#' }
plot_multipanel <- function(datos, dicc, caletas = NULL, especies_rm = NULL, col_especie, ylab_text_sup, xlab_text_sup, ylab_text_inf, xlab_text_inf, ancho, alto, nombre_salida = "plot.png", n_ticks  = 6, n_pie = 5, dec_red = 2) {
  options(scipen = 999)
  # read data and dicc
  col_especie <- str_to_lower(col_especie)
  data <- clean_names(datos) %>%
    pivot_longer(cols = ene:dic, names_to = "Meses", values_to = "desembarque", values_drop_na = TRUE) %>%
    mutate(
      especie = str_to_upper(.[[col_especie]]),
      Meses = str_to_upper(Meses)
    )
  if (!missing(caletas)) {
    data <- data %>% filter(caleta %in% caletas)
    names <- data[[col_especie]]
  } else {
    names <- data[[col_especie]]
  }
  # cambiar a map cuando se pueda
  for (i in 1:nrow(dicc)) {
    names <- str_replace(string = names, pattern = dicc$original_names[i], replacement = dicc$fixed_names[i])
  }
  data[[col_especie]] <- names
  if (missing(especies_rm)) {
    data <- data
  } else {
    data <- data %>% filter(!.[[col_especie]] %in% especies_rm)
  }
  #### empezar los plots####
  #### plot A: total anual, apilado por tipo de recurso con todas las especies####
  col_tipo <- c("#33a02c", "#fdbf6f", "#1f78b4")
  names(col_tipo) <- c("Algas", "Invertebrados", "Peces")
  summ <- data %>%
    group_by(tipo, ano) %>%
    summarise(total_desembarque = sum(desembarque, na.rm = T))
  plot_a <- ggplot(summ, aes(fill = tipo, y = total_desembarque, x = as.factor(ano)), na.rm = T) +
    geom_bar(position = "stack", stat = "identity") +
    ylab(ylab_text_sup) +
    xlab(xlab_text_sup) +
    scale_y_continuous(n.breaks = n_ticks) +
    ggtitle(label = "Tipo Especies") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 11)) +
    scale_fill_manual(breaks = c( "Algas", "Invertebrados", "Peces"), values = alpha(col_tipo, 0.7)) +
    theme(legend.position = "top") +
    theme(legend.title = element_blank())
  #### plot B: pie plot for n resources to display: total. Como esta escrito actualmente, solo funciona para 8 recursos
  # fn para usar sobre cada tipo de recurso (data_pb#)
  plot_torta <- function(data) {
    summ <- data %>%
      group_by(across(all_of(col_especie))) %>%
      summarise(total_desembarque = sum(desembarque, na.rm = T)) %>%
      ungroup()
    summ_perct <- summ %>%
      mutate(perct = total_desembarque / sum(total_desembarque) * 100) %>%
      arrange(desc(perct)) %>%
      mutate(sum_cum = cumsum(perct))
    if (nrow(summ_perct) <= n_pie) {
      selected_sp <- summ_perct %>% pull(., all_of(col_especie))
    } else {
      selected_sp <- summ_perct %>%
        slice_head(n = n_pie) %>%
        pull(., all_of(col_especie))
    }
    summ <- summ %>%
      mutate(recurso = if_else(all_of(.[[col_especie]]) %in% selected_sp, .[[col_especie]], "OTROS")) %>%
      group_by(recurso) %>%
      summarise(total_desembarque = sum(total_desembarque, na.rm = T)) %>%
      arrange(desc(total_desembarque)) %>%
      ungroup()
    order <- summ$recurso
    summ <- summ %>% mutate(
      recurso = factor(recurso, levels = order),
      cumulative = rev(cumsum(rev(total_desembarque))),
      pos = total_desembarque / 2 + lead(cumulative, 1),
      pos = if_else(is.na(pos), total_desembarque / 2, pos),
      labels = paste0(round((total_desembarque / sum(total_desembarque)) * 100, dec_red), "%")
    )
    summ <- summ %>% mutate(recurso = paste0(recurso, " ", "(", labels, ")"))
    ot <- summ %>% filter(str_detect(recurso, "^OTROS"))
    if (nrow(ot) > 0) {
      not_ot <- summ %>% filter(!recurso == ot$recurso)
      summ <- bind_rows(not_ot, ot)
    } else {
      summ <- summ
    }
    order <- summ$recurso
    # evaluar si es necesesario agregar el gris a la categoria
    ### if(para la paleta de colores)
    if (unique(data$tipo == "Peces")) {
      if (any(grepl("otros", ignore.case = T, x = summ$recurso) == TRUE)) {
        cols <- brewer.pal(length(order), "Blues")
        cols <- rev(cols)
        names(cols) <- order
        cols[n_pie + 1] <- "gray50"
      } else {
        cols <- brewer.pal(length(order), "Blues")
        cols <- rev(cols)
        names(cols) <- order
      }
      title <- "PECES"
    }
    if (unique(data$tipo == "Algas")) {
      if (any(grepl("otros", ignore.case = T, x = summ$recurso) == TRUE)) {
        cols <- brewer.pal(length(order), "Greens")
        cols <- rev(cols)
        names(cols) <- order
        cols[n_pie + 1] <- "gray50"
      } else {
        cols <- brewer.pal(length(order), "Greens")
        cols <- rev(cols)
        names(cols) <- order
      }
      summ$recurso <- str_replace(summ$recurso, pattern = "OTROS", replacement = "OTRAS")
      title <- "ALGAS"
    }
    if (unique(data$tipo) == "Invertebrados") {
      if (any(grepl("otros", ignore.case = T, x = summ$recurso) == TRUE)) {
        cols <- brewer.pal(length(order), "Oranges")
        cols <- rev(cols)
        names(cols) <- order
        cols[n_pie + 1] <- "gray50"
      } else {
        cols <- brewer.pal(length(order), "Oranges")
        cols <- rev(cols)
        names(cols) <- order
      }
      title <- "INVERTEBRADOS"
    }
    # title agregar if para el title
    plot <- ggplot(summ, aes(x = "", y = total_desembarque, fill = fct_inorder(recurso))) +
      geom_col(width = 1, color = "white") +
      coord_polar("y", start = 0, direction = -1) +
      guides(fill = guide_legend(title = title, ncol = 1)) +
      scale_fill_manual(breaks = order, values = cols) +
      theme_void() +
      theme(legend.text = element_text(size = 11))
    plot
  }
  data_list <- data %>%
    group_by(tipo) %>%
    group_split() %>%
    setNames(map(., ~ unique(.[["tipo"]])))
  p_list <- map(data_list, ~ plot_torta(.))
  if (length(p_list) == 3) {
    layout_matrix <- matrix(c(1, 1, 2, 2, NA, 3, 3, NA), nrow = 2, byrow = TRUE)
  }
  if (length(p_list) == 2) {
    layout_matrix <- matrix(c(1, 1, 2, 2), nrow = 1, byrow = TRUE)
  }
  if (length(p_list) == 1) {
    layout_matrix <- matrix(c(1, 1), nrow = 1, byrow = TRUE)
  }
  plot_b <- grid.arrange(grobs = p_list, layout_matrix = layout_matrix)
  ##### plot C: desembarco promedio por medio por tipo de recurso#####
  summ_tipo_mensual <- data %>%
    group_by(tipo, Meses) %>%
    summarise(desembarque_promedio = mean(desembarque, na.rm = T)) %>%
    ungroup()
  col_tipo <- c("#33a02c", "#fdbf6f", "#1f78b4")
  names(col_tipo) <- c("Algas", "Invertebrados", "Peces")
  pos <- c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV", "DIC")
  plot_c <- ggplot(summ_tipo_mensual, aes(fill = tipo, y = desembarque_promedio, x = as.factor(Meses)), na.rm = T) +
    geom_bar(position = "stack", stat = "identity") +
    ylab(ylab_text_inf) +
    xlab(xlab_text_inf) +
    scale_y_continuous(n.breaks = n_ticks) +
    scale_x_discrete(limits = pos) +
    ggtitle(label = "Tipo Especies") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 11)) +
    scale_fill_manual(breaks = c( "Algas", "Invertebrados","Peces"), values = alpha(col_tipo, 0.7)) +
    theme(legend.position = "top") +
    theme(legend.title = element_blank())
  p_final <- grid.arrange(grobs = list(plot_a, plot_b, plot_c), nrow = 3, rel_heights = c(1, 1, 1), rel_widths = c(1, 1, 1), align = "v")
  ggsave(nombre_salida, p_final, units = "in", width = ancho, height =  alto, dpi = 300)
}
