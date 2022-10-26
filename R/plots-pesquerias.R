#' @title plot_multipanel
#' @description Función para hacer el gráfico del "multipanel"
#' @param datos dataframe de entrada. Formato: separado por tab. (Tiene que tener: coluna que identifique a las especies/recurso, caletas, meses, años. Los meses representan el desembarco en ese mes/año)
#' @param dicc archivo diccionario de recursos, separado por "," XXX
#' @param caletas Vector con el nombre de las caletas que se quieran incluir en el gráfico. Si NULL, se usaran todas
#' @param col_especies columna que tiene que el nombre de las especies/recursos
#' @param especies_rm Vector con el nombre de las especies que se quieran eliminar del gráfico en el gráfico. Si NULL, se usaran todas las especies
#' @param ylab_text_sup Cadena de texto para el eje y del panel superior. Si NULL == "Desembarque total (Ton)"
#' @param xlab_text_sup  Cadena de texto para el eje x del panel superior. Si NULL == "Años"
#' @param ylab_text_inf Cadena de texto para el eje y del panel inferior. Si NULL == "Desembarque promedio (Ton)"
#' @param xlab_text_inf Cadena de texto para el eje x del panel inferior. Si NULL == "Meses"
#' @param n_pie Número de divisiones de la torta
#' @param n_ticks Número de divisiones ejes y
#' @param alto altura de la imágen
#' @param ancho ancho de la imágen
#' @param nombre_salida Nombre figura incluyendo la extensión ("XXXX.png")
#' @param dec_red Número de decimales para los porcentajes de los gráficos de torta
#' @return imágenes png
#' @import ggplot2
#' @import dplyr
#' @import stringr
#' @importFrom janitor clean_names
#' @importFrom RColorBrewer brewer.pal
#' @importFrom purrr map
#' @importFrom tidyr pivot_longer
#' @importFrom forcats fct_inorder
#' @importFrom gridExtra grid.arrange
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
#' nombre_salida <- "test_plot_multipanel"
#' n_pie <- 5
#' dec_red <- 2
#' plot_multipanel(datos = datos, dicc = dicc, caletas = caletas, especies_rm = especies_rm, col_especie = col_especie, ylab_text_sup = ylab_text_sup, xlab_text_sup = xlab_text_sup, ylab_text_inf = ylab_text_inf, xlab_text_inf = xlab_text_inf, ancho = ancho, alto = alto, nombre_salida = nombre_salida, n_tick = n_tick, n_pie = n_pie, dec_red = dec_red)
#' }
plot_multipanel <- function(datos, dicc, caletas = NULL, especies_rm = NULL, col_especie, ylab_text_sup, xlab_text_sup, ylab_text_inf, xlab_text_inf, ancho, alto, nombre_salida = "plot.png", n_ticks = 6, n_pie = 5, dec_red = 2) {
  options(scipen = 999)
  options(dplyr.summarise.inform = FALSE)
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
    theme(legend.spacing.x = unit(0.35, 'cm'),plot.title = element_text(hjust = 0.5), text = element_text(size = 11)) +
    scale_fill_manual(breaks = c("Algas", "Invertebrados", "Peces"), values = alpha(col_tipo, 0.7)) +
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
      theme(
        legend.key.height = unit(0.2, "cm"),
        legend.key.width = unit(0.2, "cm"),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 7)
      )
  }
  data_list <- data %>%
    group_by(tipo) %>%
    group_split() %>%
    setNames(map(., ~ unique(.[["tipo"]])))
  p_list <- map(data_list, ~ plot_torta(.))
  if (length(p_list) == 3) {
    layout_matrix <- matrix(c(1, 1, 2, 2, NA, 3, 3, NA), nrow = 2, byrow = TRUE)
    # layout <- c(
    #  area(1, 1),
    #  area(1, 3),
    #  area(3, 2))
  }
  if (length(p_list) == 2) {
    layout_matrix <- matrix(c(1, 1, 2, 2), nrow = 1, byrow = TRUE)
    # layout <- c(
    #  area(2, 1),
    #  area(2, 3)
    # )
  }
  if (length(p_list) == 1) {
    layout_matrix <- matrix(c(1, 1), nrow = 1, byrow = TRUE)
    # layout <- c(
    #  area(2, 2)
    # )
  }
  # cambiar a patchwork: usar design en vez de layout_matrix:
  # plot_b <- wrap_plots(p_list)
  # plot_b <- plot_b + plot_layout(design = layout, widths = 1)
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
    theme(legend.spacing.x = unit(0.35, 'cm'),plot.title = element_text(hjust = 0.5), text = element_text(size = 11)) +
    scale_fill_manual(breaks = c("Algas", "Invertebrados", "Peces"), values = alpha(col_tipo, 0.7)) +
    theme(legend.position = "top") +
    theme(legend.title = element_blank())
  final <- grid.arrange(grobs = list(plot_a, plot_b, plot_c), nrow = 3, rel_heights = c(1, 1, 1), rel_widths = c(1, 1, 1), align = "v")
  dev.off()
  ggsave(filename = nombre_salida, plot = final, units = "in", width = ancho, height = alto, dpi = 300)
}
#' @title plot_tipo_embarcacion
#' @description Función para hacer el grafico del "tipo de embarcaciones"
#' @param datos dataframe de entrada. Formato: separado por tab (Tiene que tener las columnas: caletas/caleta_inscripcion, material,tipo)
#' @param caletas Vector con el nombre de las caletas que se quieran incluir en el gráfico. Si NULL, se usaran todas
#' @param col_caleta columna que tiene que el nombre de las caletas
#' @param n_ticks Número de divisiones ejes y
#' @param alto altura de la imágen
#' @param ancho ancho de la imágen
#' @param interv_potencia amplitud del intervalo para el subplot de potencia de las embarcaciones
#' @param nombre_salida Nombre figura incluyendo la extensión ("XXXX.png")
#' @return imágenes png
#' @import ggplot2
#' @import dplyr
#' @import stringr
#' @importFrom janitor clean_names
#' @importFrom patchwork plot_annotation plot_layout
#' @importFrom tidyr separate
#' @export plot_embarcaciones
#' @examples
#' \dontrun{
#' datos <- read_tsv("datos_tipo_embarcacion.txt", show_col_types = FALSE)
#' caletas <- "SAN VICENTE"
#' col_caleta <- "Caleta Inscripción"
#' n_ticks <- 6
#' interv_potencia <- 100
#' alto <- 6
#' ancho <- 8
#' nombre_salida <- "test_plot_embarcaciones.png"
#' plot_embarcaciones(datos = datos, caletas = caletas, col_caleta = col_caleta, n_ticks = n_ticks, alto = alto, ancho = ancho, nombre_salida = nombre_salida)
#' }
plot_embarcaciones <- function(datos, caletas = NULL, col_caleta, n_ticks = 6, alto, ancho, nombre_salida) {
  options(scipen = 999)
  options(dplyr.summarise.inform = FALSE)
  if (!missing(caletas)) {
    data <- datos %>% filter(.[[col_caleta]] %in% caletas)
  } else {
    data <- datos
  }
  data <- data %>%
    clean_names() %>%
    mutate(
      tipo = str_to_sentence(tipo),
      material = str_to_sentence(material)
    )
  #### plot tipo botes####
  summ_tipo <- data %>%
    group_by(tipo) %>%
    summarise(numero_embarcaciones = n())
  order_tipo <- c("Bote a motor", "Bote a remo o vela", "Lancha")
  cols_tipo <- c("#999999", "#E69F00", "#56B4E9")
  names(cols_tipo) <- order_tipo
  cols_tipo <- cols_tipo[c(summ_tipo$tipo)]
  order_tipo <- names(cols_tipo)
  p_tipo_botes <- ggplot(summ_tipo, aes(x = tipo, y = numero_embarcaciones, fill = tipo)) +
    geom_bar(stat = "identity", width = 0.5, color = cols_tipo) +
    ylab("N° Botes Inscritos") +
    xlab("Tipo de Embarcación") +
    theme_bw() +
    theme(legend.position = "none") +
    geom_text(aes(label = numero_embarcaciones, vjust = -0.2, size = 0.7)) +
    ylim(0, max(summ_tipo$numero_embarcaciones + 10)) +
    theme(text = element_text(size = 12)) +
    scale_fill_manual(breaks = order_tipo, values = cols_tipo)
  #### Material de construcción####
  summ_material <- data %>%
    group_by(material) %>%
    summarise(n = n())
  #### plot  material bote
  p_mater_botes <- ggplot(summ_material, aes(x = material, y = n, fill = material)) +
    geom_bar(stat = "identity", color = "black", width = 0.5) +
    ylab("N° Botes Inscritos") +
    xlab("Material de Construcción") +
    theme_bw() +
    theme(legend.position = "none") +
    geom_text(aes(label = n, vjust = -0.2, size = 0.7)) +
    ylim(0, max(summ_material$n + 10)) +
    scale_fill_manual(values = rep("Gray50", 3)) +
    theme(text = element_text(size = 12))
  # Potencia. datos no tienen potencia, por eso lo pintamos ..quizas cambiar los colores
  # summ_potencia  <- data %>% filter(tipo == "Bote a motor")
  name_potencia <- data %>%
    dplyr::select(starts_with("potencia")) %>%
    colnames(.)
  data_potencia <- data %>% select(all_of(name_potencia), tipo)
  summ_potencia <- data_potencia %>%
    mutate(intervalo = cut_interval(.[[name_potencia]], length = interv_potencia)) %>%
    group_by(tipo, intervalo) %>%
    summarise(cuenta = n())
  ## acortar con este ejemplo
  remove <- c("\\(", "\\)", "]", "\\[")
  summ_potencia$intervalo <- str_remove_all(summ_potencia$intervalo, paste(remove, collapse = "|"))
  summ_potencia$intervalo <- str_replace(summ_potencia$intervalo, ",", "-")
  # summ_potencia$intervalo <- factor(summ_potencia$intervalo, levels = summ_potencia$intervalo)
  summ_potencia <- summ_potencia %>%
    separate(col = intervalo, into = c("min", "max"), sep = "-", remove = FALSE) %>%
    mutate(
      min = as.numeric(min),
      max = as.numeric(max)
    ) %>%
    mutate(label = ifelse(max <= 200, intervalo, "200+"))
  n_cat <- unique(summ_potencia$label)
  summ_potencia <- summ_potencia %>%
    group_by(label, tipo) %>%
    summarise(n_total = sum(cuenta))
  summ_potencia$label <- factor(summ_potencia$label, levels = n_cat)
  ###### cambiar el tipo de grafico
  order_tipo <- c("Bote a motor", "Bote a remo o vela", "Lancha")
  cols_tipo <- c("#999999", "#E69F00", "#56B4E9")
  names(cols_tipo) <- order_tipo
  cols_tipo <- cols_tipo[c(summ_potencia$tipo)]
  order_tipo <- names(cols_tipo)
  p_potencia_botes <- ggplot(summ_potencia, aes(x = label, y = n_total, fill = tipo)) +
    geom_bar(position = "stack", stat = "identity", width = 0.5) +
    labs(x = "Categorias Potencia (HP)", y = "N° Botes Inscritos", fill = "Tipo Embarcación") +
    scale_y_continuous(n.breaks = n_ticks) +
    scale_fill_manual(breaks = c("Bote a motor", "Bote a remo o vela", "Lancha"), values = cols_tipo) +
    theme_light()
  # p_potencia_botes
  # Eslora Promedio
  name_eslora <- data %>%
    dplyr::select(starts_with("eslora")) %>%
    colnames(.)
  summ_eslora <- data %>%
    group_by(material) %>%
    summarise(mean = mean(.[[name_eslora]]), sd = sd(.[[name_eslora]]))
  p_eslora <- ggplot(summ_eslora, aes(x = material, y = mean, fill = material)) +
    ylab("Eslora Promedio (m)") +
    xlab("Material de Construcción") +
    geom_bar(stat = "identity", color = "black", position = position_dodge(), width = 0.5) +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2, position = position_dodge(.9)) +
    scale_fill_manual(values = rep("Gray50", 3)) +
    theme_bw() +
    theme(text = element_text(size = 12), legend.position = "none")
  tmp <- p_tipo_botes + p_mater_botes + p_potencia_botes + p_eslora + plot_layout(widths = c(1, 1))
  final <- tmp + plot_annotation(tag_levels = "a")
  ggsave(filename = nombre_salida, plot = final, width = ancho, height = alto, dpi = 300)
}
#' @title plot_torta_pescadores
#' @description Función para hacer el grafico de torta que categorias de pescador por caleta
#' @param datos dataframe de entrada. Formato: separado por tab (Tiene que tener las columnas: caletas/caleta_inscripcion, material,tipo)
#' @param caletas Vector con el nombre de las caletas que se quieran incluir en el gráfico. Si NULL, se usaran todas las presentes en el set  de datos
#' @param col_caleta columna que tiene que el nombre de las caletas
#' @param alto altura de la imágen
#' @param ancho ancho de la imágen
#' @param ncol numero de columnas de la imágen
#' @param nombre_salida Nombre figura incluyendo la extensión ("XXXX.png")
#' @return imágenes png
#' @import ggplot2
#' @import dplyr
#' @import stringr
#' @importFrom purrr map
#' @importFrom janitor clean_names
#' @importFrom patchwork wrap_plots
#' @importFrom tidyr pivot_longer
#' @export plot_torta_pescadores
#' @examples
#' \dontrun{
#' datos <- read_tsv("data_pescador.txt", show_col_types = FALSE)
#' col_caleta <- "Caleta"
#' caletas <- unique(datos$Caleta)[3:6]
#' alto <- 6
#' ancho <- 9
#' n_col <- 2
#' nombre_salida <- "test_plot_torta_caletas.png"
#' plot_torta_pescadores(datos = datos, caletas = caletas, col_caleta = col_caleta, alto = alto, ancho = ancho, n_col = n_col, nombre_salida = nombre_salida)
#' }
plot_torta_pescadores <- function(datos, col_caleta, caletas, ancho, alto, n_col, nombre_salida) {
  options(scipen = 999)
  options(dplyr.summarise.inform = FALSE)
  data <- datos %>%
    rename_all(., .funs = str_to_lower) %>%
    clean_names()
  col_caleta <- str_to_lower(col_caleta)
  data <- data %>% mutate(nombre_pescador = str_to_upper(nombre_pescador))

  if (missing(caletas)) {
    data <- data
  } else {
    data <- data %>% filter(.[[col_caleta]] %in% caletas)
  }
  data <- data %>%
    select(all_of(col_caleta), nombre_pescador, armador:pescador) %>%
    mutate_at(vars(armador:pescador), ~ replace(., is.na(.), 0)) %>%
    mutate(across(.fns = ~ replace(., . == "X", 1))) %>%
    group_by(across(all_of(col_caleta)), nombre_pescador) %>%
    summarise_at(vars(armador:pescador), max) %>%
    pivot_longer(cols = armador:pescador, names_to = "categoria_pescador", values_to = "n_categoria") %>%
    mutate(n_categoria = as.numeric(n_categoria))
  ## summary total
  summ <- data %>%
    group_by(across(all_of(col_caleta))) %>%
    summarise(total_pescadores = length(unique(nombre_pescador)))
  # summ for pie
  summ_torta <- data %>%
    group_by(across(all_of(col_caleta)), categoria_pescador) %>%
    summarise(total_tipo = sum(n_categoria, na.rm = T)) %>%
    mutate(
      perct = total_tipo / sum(total_tipo) * 100,
      label = paste0(round(perct, 2), " ", "%"),
      categoria_pescador = str_to_sentence(categoria_pescador),
      categoria_pescador_label = paste0(str_to_sentence(categoria_pescador), " ", "(", label, ")")
    )
  summ_torta <- left_join(summ_torta, summ, by = col_caleta)
  ## funcion para plotear 1 torta
  fn_plot_pie <- function(data) {
    cols <- c("#a5a5a5", "#fdbf2e", "#ea7d3c", "#4674c2")
    names(cols) <- data$categoria_pescador_label
    title <- paste0("Caleta", " ", unique(data[[col_caleta]]), " ", "(N =", " ", unique(data$total_pescadores), ")")
    plot_pie <- ggplot(data, aes(x = "", y = total_tipo, fill = fct_inorder(categoria_pescador_label))) +
      geom_col(width = 1, color = "white") +
      coord_polar("y", start = 0, direction = -1) +
      guides(fill = guide_legend(title = "Categoría de Pescador", ncol = 1)) +
      theme_void() +
      theme(legend.text = element_text(size = 11)) +
      scale_fill_manual(breaks = names(cols), values = cols) +
      ggtitle(label = title) +
      theme(plot.title = element_text(hjust = 0.5))
  }
  summ_torta_list <- summ_torta %>%
    group_by(across(all_of(col_caleta))) %>%
    group_split() %>%
    setNames(map(., ~ unique(.[[col_caleta]])))
  list_plot <- map(summ_torta_list, ~ fn_plot_pie(.))
  final <- wrap_plots(list_plot, ncol = n_col)
  ggsave(filename = nombre_salida, plot = final, width = ancho, height = alto, dpi = 300)
}
#' @title plot_barra_pescadores
#' @description Función para hacer el grafico de barra muestra el total de pescadores por caleta junto a la categoria de pescadores
#' @param datos dataframe de entrada. Formato: separado por tab (Tiene que tener las columnas: caletas/caleta_inscripcion, material,tipo)
#' @param caletas Vector con el nombre de las caletas que se quieran incluir en el gráfico. Si NULL, se usaran todas las presentes en el set  de datos
#' @param col_caleta columna que tiene que el nombre de las caletas
#' @param ylab_text Cadena de texto para el eje y del panel. Si NULL == "N° de Pescadores Inscritos"
#' @param xlab_text  Cadena de texto para el eje x. Si NULL == "Caletas"
#' @param alto altura de la imágen
#' @param ancho ancho de la imágen
#' @param n_ticks Número de divisiones ejes y
#' @param nombre_salida Nombre figura incluyendo la extensión ("XXXX.png")
#' @return imágenes png
#' @import ggplot2
#' @import dplyr
#' @import stringr
#' @importFrom janitor clean_names
#' @importFrom tidyr pivot_longer
#' @export plot_barra_pescadores
#' @examples
#' \dontrun{
#' datos <- read_tsv("data_pescador.txt", show_col_types = FALSE)
#' col_caleta <- "Caleta"
#' ylab
#' alto <- 6
#' ancho <- 9
#' n_ticks <- 6
#' nombre_salida <- "test_plot_barra_pescadores.png"
#' plot_barra_pescadores(datos = datos, col_caleta = col_caleta, alto = alto, ancho = ancho, n_ticks = n_ticks, nombre_salida = nombre_salida)
#' }
plot_barra_pescadores <- function(datos, caletas = NULL, col_caleta, ylab_text = "N° de Pescadores Inscritos", xlab_text = "Caletas", n_ticks, ancho, alto, nombre_salida) {
  options(scipen = 999)
  options(dplyr.summarise.inform = FALSE)
  data <- datos %>%
    rename_all(., .funs = str_to_lower) %>%
    clean_names()
  col_caleta <- str_to_lower(col_caleta)
  data <- data %>% mutate(nombre_pescador = str_to_upper(nombre_pescador))
  if (missing(caletas)) {
    data <- data
  } else {
    data <- data %>% filter(.[[col_caleta]] %in% caletas)
  }
  data <- data %>%
    select(all_of(col_caleta), nombre_pescador, armador:pescador) %>%
    mutate_at(vars(armador:pescador), ~ replace(., is.na(.), 0)) %>%
    mutate(across(.fns = ~ replace(., . == "X", 1))) %>%
    group_by(across(all_of(col_caleta)), nombre_pescador) %>%
    summarise_at(vars(armador:pescador), max) %>%
    pivot_longer(cols = armador:pescador, names_to = "categoria_pescador", values_to = "n_categoria") %>%
    mutate(n_categoria = as.numeric(n_categoria))
  ## plot
  summ <- data %>%
    group_by(across(all_of(col_caleta))) %>%
    summarise(total_tipo = length(unique(nombre_pescador)))
  plot_bar_pescadores <- ggplot(summ, aes(y = total_tipo, x = as.factor(.data[[col_caleta]])), label = total_tipo, na.rm = T) +
    geom_bar(position = "stack", stat = "identity") +
    ylab(ylab_text) +
    xlab(xlab_text) +
    scale_y_continuous(n.breaks = n_ticks) +
    ggtitle(label = "N° Pescadores por caleta") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 11)) +
    theme(legend.position = "top") +
    theme(legend.title = element_blank()) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  ggsave(filename = nombre_salida, plot = plot_bar_pescadores, width = ancho, height = alto, units = "in", dpi = 300)
}
