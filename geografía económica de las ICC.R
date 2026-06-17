#Geografía económica de las ICC
#---------------------------------------------------------------------

#-----------------------------------------------------------------------------
# PAQUETES

paquetes <- c( 
  "sf","dplyr","tidyr","readr","stringr","purrr","tibble",
  "ggplot2","viridis","RColorBrewer","scales", "spdep","sfdep",
  "spatstat.geom","spatstat.explore", "tmap","data.table",   
  "flextable","officer", "vcd","rstatix","stars", "raster", "tidyverse", "spatstat",
  "units"
) 

faltantes <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
if (length(faltantes) > 0) install.packages(faltantes, dependencies = TRUE) 
invisible(lapply(paquetes, library, character.only = TRUE))

select <- dplyr::select
tmap_mode("plot")

#-----------------------------------------------------------------------------
# FASE 1: CARTOGRAFÍA BASE Y CONFIGURACIONES ADMINISTRATIVAS
#-----------------------------------------------------------------------------

cat("\n[Fase 1] Cargando y proyectando cartografía oficial AGEB (CRS 6372)...\n")

ageb <- st_read("datos/ageb/2025_1_00_A.shp", quiet = TRUE) %>%
  mutate(
    ent = sprintf("%02d", as.integer(substr(CVEGEO, 1, 2))),
    mun = sprintf("%03d", as.integer(substr(CVEGEO, 3, 5))),
    ageb_id = CVEGEO
  ) %>%
  st_transform(6372)

config_nodos <- list(
  "AMG" = list(
    cve_ent = "14",
    cve_mun = c("039", "120", "098", "101", "097", "070", "044", "051", "124", "002")
  ),
  "Puerto Vallarta" = list(cve_ent = "14", cve_mun = c("067")),
  "Tijuana" = list(cve_ent = "02", cve_mun = c("004")),
  "Oaxaca" = list(cve_ent = "20", cve_mun = c("067"))
)

nom_mun_amg <- c("039", "120", "098", "101", "097", "070", "044", "051", "124", "002")

macro_economia_control <- data.frame(
  nodo_urbano = c("AMG", "Puerto Vallarta", "Tijuana", "Oaxaca"),
  total_establecimientos_ciudad = c(185340, 18420, 72150, 22100)
)

#-----------------------------------------------------------------------------
# FASE 2: TAXONOMÍAS DE CLASIFICACIÓN (CSCM)
#-----------------------------------------------------------------------------

cat("\n[Fase 2] Construyendo taxonomías CSCM...\n")

# 2.1 CSCM - ACTIVIDADES CARACTERÍSTICAS
icc_caracteristicas <- data.frame(
  codigo = c(
    "315224","323111","323119","323120","325992","333412","334220",
    "334310","334610","339911","339930","339991","511111","511112",
    "511121","511122","511131","511132","511141","511191","511192",
    "512110","512111","512112","512113","512120","512130","512190",
    "512230","512240","512250","512290","515110","515120","515210",
    "519110","519121","519130","519190","541310","541320","541340",
    "541370","541410","541420","541430","541490","541510","541810",
    "541840","541850","541920","541930","611611","611612","611631",
    "711111","711112","711131","711191","711211","711212","711311",
    "711312","711320","711410","711510","712111","712120","712131",
    "712190","812910","813230"
  ),
  sistema = "CSCM",
  dimension = "Características Simbólicas",
  macro_grupo = "Características",
  stringsAsFactors = FALSE
)

# 2.2 CSCM - ARTESANÍAS
icc_artesanias <- data.frame(
  codigo = c(
    "311340","311350","311423","311513","311613","311812","312142",
    "313111","314110","314120","314991","314992","314999","315192",
    "315222","315224","315229","315991","315999","316211","316219",
    "316991","316999","321910","321991","321992","322299","325999",
    "327111","327215","327219","327420","327991","332110","332212",
    "332510","332610","332999","335120","337120","339912","339913",
    "339914","339930","339991","339994","339999"
  ),
  sistema = "CSCM",
  dimension = "Artesanías y Manufactura Tradicional",
  macro_grupo = "Artesanías",
  stringsAsFactors = FALSE
)

# DIMENSIONES INTERNAS CSCM (tribble completo - se mantiene intacto)
dimensiones_cscm <- tribble(
  # MANUFACTURA CULTURAL
  ~codigo, ~dimension,
  "315224","Manufactura Cultural", "323111","Manufactura Cultural",
  "323119","Manufactura Cultural", "323120","Manufactura Cultural",
  "325992","Manufactura Cultural", "333412","Manufactura Cultural",
  "334220","Manufactura Cultural", "334310","Manufactura Cultural",
  "334610","Manufactura Cultural", "339911","Manufactura Cultural",
  "339930","Manufactura Cultural", "339991","Manufactura Cultural",
  # EDICION Y PUBLICACION
  "511111","Edición y Publicación", "511112","Edición y Publicación",
  "511121","Edición y Publicación", "511122","Edición y Publicación",
  "511131","Edición y Publicación", "511132","Edición y Publicación",
  "511141","Edición y Publicación", "511191","Edición y Publicación",
  "511192","Edición y Publicación",
  # AUDIOVISUAL Y SOFTWARE
  "512110","Audiovisual y Software", "512111","Audiovisual y Software",
  "512112","Audiovisual y Software", "512113","Audiovisual y Software",
  "512120","Audiovisual y Software", "512130","Audiovisual y Software",
  "512190","Audiovisual y Software", "512230","Audiovisual y Software",
  "512240","Audiovisual y Software", "512250","Audiovisual y Software",
  "512290","Audiovisual y Software",
  # RADIO Y TELEVISION
  "515110","Radio y Televisión", "515120","Radio y Televisión",
  "515210","Radio y Televisión",
  # INTERNET Y MEDIOS DIGITALES
  "519110","Internet y Medios Digitales", "519121","Internet y Medios Digitales",
  "519130","Internet y Medios Digitales", "519190","Internet y Medios Digitales",
  # DISEÑO Y PUBLICIDAD
  "541310","Diseño y Publicidad", "541320","Diseño y Publicidad",
  "541340","Diseño y Publicidad", "541370","Diseño y Publicidad",
  "541410","Diseño y Publicidad", "541420","Diseño y Publicidad",
  "541430","Diseño y Publicidad", "541490","Diseño y Publicidad",
  "541510","Diseño y Publicidad", "541810","Diseño y Publicidad",
  "541840","Diseño y Publicidad", "541850","Diseño y Publicidad",
  "541920","Diseño y Publicidad", "541930","Diseño y Publicidad",
  # EDUCACION ARTISTICA
  "611611","Educación Artística", "611612","Educación Artística",
  "611631","Educación Artística",
  # DEPORTES
  "711211","Deportes", "711212","Deportes",
  # ARTES ESCENICAS
  "711111","Artes Escénicas", "711112","Artes Escénicas",
  "711131","Artes Escénicas", "711191","Artes Escénicas",
  "711311","Artes Escénicas", "711312","Artes Escénicas",
  "711320","Artes Escénicas", "711410","Artes Escénicas",
  "711510","Artes Escénicas",
  # PATRIMONIO Y MUSEOS
  "712111","Patrimonio y Museos", "712120","Patrimonio y Museos",
  "712131","Patrimonio y Museos", "712190","Patrimonio y Museos",
  # FOTOGRAFIA
  "812910","Fotografía y Servicios Visuales",
  # ORGANIZACIONES CULTURALES
  "813230","Organizaciones Culturales",
  # ARTESANIAS ALIMENTARIAS
  "311340","Artesanías Alimentarias", "311350","Artesanías Alimentarias",
  "311423","Artesanías Alimentarias", "311513","Artesanías Alimentarias",
  "311613","Artesanías Alimentarias", "311812","Artesanías Alimentarias",
  "312142","Artesanías Alimentarias",
  # TEXTILES Y VESTIMENTA
  "313111","Textiles y Vestimenta", "314110","Textiles y Vestimenta",
  "314120","Textiles y Vestimenta", "314991","Textiles y Vestimenta",
  "314992","Textiles y Vestimenta", "314999","Textiles y Vestimenta",
  "315192","Textiles y Vestimenta", "315222","Textiles y Vestimenta",
  "315224","Textiles y Vestimenta", "315229","Textiles y Vestimenta",
  "315991","Textiles y Vestimenta", "315999","Textiles y Vestimenta",
  # CUERO Y CALZADO
  "316211","Cuero y Calzado", "316219","Cuero y Calzado",
  "316991","Cuero y Calzado", "316999","Cuero y Calzado",
  # MADERA Y PAPEL
  "321910","Madera y Papel", "321991","Madera y Papel",
  "321992","Madera y Papel", "322299","Madera y Papel",
  # CERAMICA VIDRIO PIEDRA
  "327111","Cerámica, Vidrio y Piedra", "327215","Cerámica, Vidrio y Piedra",
  "327219","Cerámica, Vidrio y Piedra", "327420","Cerámica, Vidrio y Piedra",
  "327991","Cerámica, Vidrio y Piedra",
  # METAL JOYERIA Y MANUFACTURA
  "325999","Metal, Joyería y Manufactura", "332110","Metal, Joyería y Manufactura",
  "332212","Metal, Joyería y Manufactura", "332510","Metal, Joyería y Manufactura",
  "332610","Metal, Joyería y Manufactura", "332999","Metal, Joyería y Manufactura",
  "335120","Metal, Joyería y Manufactura", "337120","Metal, Joyería y Manufactura",
  "339912","Metal, Joyería y Manufactura", "339913","Metal, Joyería y Manufactura",
  "339914","Metal, Joyería y Manufactura", "339930","Metal, Joyería y Manufactura",
  "339991","Metal, Joyería y Manufactura", "339994","Metal, Joyería y Manufactura",
  "339999","Metal, Joyería y Manufactura"
)

# Los códigos de Manufactura Cultural solo pertenecen a Características
catalogo_cscm <- bind_rows(
  icc_caracteristicas,
  icc_artesanias
)

catalogo_cscm <- catalogo_cscm %>%
  dplyr::mutate(
    macro_grupo = dplyr::if_else(
      codigo %in% dimensiones_cscm$codigo[dimensiones_cscm$dimension == "Manufactura Cultural"],
      "Características",
      macro_grupo
    )
  )

catalogo_completo <- bind_rows(catalogo_cscm)

# 2.15 UNIVERSO TOTAL DE CÓDIGOS
todos_codigos_estudio <- unique(catalogo_completo$codigo)

cat("   CSCM Características:", nrow(icc_caracteristicas), "códigos\n")
cat("   CSCM Artesanías:", nrow(icc_artesanias), "códigos\n")
cat("   Total SCIAN únicos:", length(todos_codigos_estudio), "\n")

#-----------------------------------------------------------------------------
# FASE 3: INTEGRACIÓN DENUE Y CLASIFICACIÓN TERRITORIAL
#-----------------------------------------------------------------------------

cat("\n[Fase 3] Integrando DENUE, AGEB y taxonomías CSCM...\n")

archivos_denue <- list.files("datos/denue", pattern = "denue_2026_\\d+\\.(csv|CSV)$", full.names = TRUE)
cat("   Archivos encontrados:", length(archivos_denue), "\n")

denue_unificado <- archivos_denue |>
  lapply(function(ruta) {
    encabezados <- names(fread(ruta, nrows = 0))
    
    col_codigo <- intersect(c("codigo_act","clase_act","actividad_codigo"), encabezados)[1]
    col_ent <- intersect(c("cve_ent","entidad","cve_entidad"), encabezados)[1]
    col_mun <- intersect(c("cve_mun","municipio","cve_municipio"), encabezados)[1]
    col_lat <- intersect(c("latitud","lat"), encabezados)[1]
    col_lon <- intersect(c("longitud","lon"), encabezados)[1]
    
    if (is.na(col_codigo) || is.na(col_ent) || is.na(col_mun) || is.na(col_lat) || is.na(col_lon)) {
      warning(paste("Columnas no encontradas en:", basename(ruta)))
      return(NULL)
    }
    
    dt <- fread(ruta, select = c(col_codigo, col_ent, col_mun, col_lat, col_lon), colClasses = "character")
    
    setnames(dt, old = c(col_codigo, col_ent, col_mun, col_lat, col_lon),
             new = c("codigo_act", "cve_ent", "cve_mun", "latitud", "longitud"))
    
    dt[, `:=`(
      codigo_act = as.character(codigo_act),
      cve_ent = sprintf("%02d", as.integer(cve_ent)),
      cve_mun = sprintf("%03d", as.integer(cve_mun)),
      latitud = as.numeric(latitud),
      longitud = as.numeric(longitud)
    )]
    
    dt[codigo_act %in% todos_codigos_estudio]
  }) |> bind_rows()

# Limpieza básica
denue_unificado <- denue_unificado %>%
  filter(!is.na(latitud), !is.na(longitud), latitud != 0, longitud != 0)
cat("   Registros creativos encontrados:", nrow(denue_unificado), "\n")

# Identificación de nodos de estudio
denue_unificado <- denue_unificado %>%
  mutate(nodo_urbano = case_when(
    cve_ent == config_nodos$AMG$cve_ent & cve_mun %in% config_nodos$AMG$cve_mun ~ "AMG",
    cve_ent == config_nodos$`Puerto Vallarta`$cve_ent & cve_mun %in% config_nodos$`Puerto Vallarta`$cve_mun ~ "Puerto Vallarta",
    cve_ent == config_nodos$Tijuana$cve_ent & cve_mun %in% config_nodos$Tijuana$cve_mun ~ "Tijuana",
    cve_ent == config_nodos$Oaxaca$cve_ent & cve_mun %in% config_nodos$Oaxaca$cve_mun ~ "Oaxaca",
    TRUE ~ NA_character_
  )) %>% filter(!is.na(nodo_urbano))
cat("   Registros dentro de los nodos:", nrow(denue_unificado), "\n")

# Municipio específico del AMG
nom_mun_amg_vec <- setNames(nom_mun_amg, names(nom_mun_amg))
denue_unificado <- denue_unificado %>%
  mutate(municipio_amg = case_when(nodo_urbano == "AMG" ~ nom_mun_amg_vec[cve_mun], TRUE ~ NA_character_))

# Conversión a objeto espacial
icc_sf_base <- st_as_sf(denue_unificado, coords = c("longitud","latitud"), crs = 4326, remove = FALSE) %>%
  st_transform(6372)

# Asignación de AGEB
icc_sf_base <- icc_sf_base %>%
  st_join(ageb[, c("ageb_id","geometry")], join = st_intersects) %>%
  filter(!is.na(ageb_id))
cat("   Registros con AGEB:", nrow(icc_sf_base), "\n")

# Desplazamiento controlado
icc_sf_base <- st_jitter(icc_sf_base, amount = 0.0001)

# BASE CSCM
icc_sf_cscm <- icc_sf_base %>%
  left_join(catalogo_cscm, by = c("codigo_act" = "codigo")) %>%
  filter(!is.na(macro_grupo))
cat("   Registros CSCM:", nrow(icc_sf_cscm), "\n")

icc_sf_cscm <- icc_sf_cscm %>%
  left_join(dimensiones_cscm, by = c("codigo_act" = "codigo"), relationship = "many-to-many") %>%
  mutate(dimension_macro = dimension.x, dimension = dimension.y) %>%
  dplyr::select(-dimension.x, -dimension.y)


# Resumen general
resumen_nodos <- icc_sf_base %>%
  st_drop_geometry() %>%
  count(nodo_urbano, name = "establecimientos") %>%
  arrange(desc(establecimientos))
print(resumen_nodos)

# Control de calidad
cat("\nControl de calidad:\n")
cat("   CSCM Características:", icc_sf_cscm %>% filter(macro_grupo == "Características") %>% nrow(), "\n")
cat("   CSCM Artesanías:", icc_sf_cscm %>% filter(macro_grupo == "Artesanías") %>% nrow(), "\n")
cat("   AGEB con actividad creativa:", icc_sf_base %>% st_drop_geometry() %>% distinct(ageb_id) %>% nrow(), "\n")

# Preparar ageb_nodos para análisis espacial
ageb_nodos <- ageb %>%
  dplyr::mutate(nodo_urbano = dplyr::case_when(
    ent == "14" & mun %in% config_nodos$AMG$cve_mun ~ "AMG",
    ent == "14" & mun == "067" ~ "Puerto Vallarta",
    ent == "02" & mun == "004" ~ "Tijuana",
    ent == "20" & mun == "067" ~ "Oaxaca",
    TRUE ~ NA_character_
  )) %>%
  dplyr::filter(!is.na(nodo_urbano)) %>%
  dplyr::rename(ciudad = nodo_urbano)

cat("\n[Fase 3 completada]\n")

#-----------------------------------------------------------------------------
# FASE 4: ESTADISTICA DESCRIPTIVA Y ESTRUCTURA DE LAS ICC
#-----------------------------------------------------------------------------

carpetas_salida <- c("salidas/tablas", "salidas/graficas")
lapply(carpetas_salida, dir.create, recursive = TRUE, showWarnings = FALSE)

cat("\n[Fase 4] Generando tablas descriptivas...\n")

tabla_nodos <- icc_sf_base %>%
  st_drop_geometry() %>%
  count(nodo_urbano, name = "establecimientos") %>%
  arrange(desc(establecimientos))
write.csv(tabla_nodos, "salidas/tablas/tabla_01_total_nodos.csv", row.names = FALSE)

tabla_cscm_nodo <- icc_sf_cscm %>%
  st_drop_geometry() %>%
  count(nodo_urbano, macro_grupo, name = "establecimientos") %>%
  group_by(nodo_urbano) %>%
  mutate(porcentaje = round(establecimientos / sum(establecimientos) * 100, 2)) %>%
  ungroup()
write.csv(tabla_cscm_nodo, "salidas/tablas/tabla_02_cscm_nodo.csv", row.names = FALSE)

tabla_cscm_total <- icc_sf_cscm %>%
  st_drop_geometry() %>%
  count(macro_grupo, name = "establecimientos") %>%
  mutate(porcentaje = round(establecimientos / sum(establecimientos) * 100, 2))
write.csv(tabla_cscm_total, "salidas/tablas/tabla_03_cscm_total.csv", row.names = FALSE)

tabla_amg <- icc_sf_base %>%
  st_drop_geometry() %>%
  filter(nodo_urbano == "AMG") %>%
  count(municipio_amg, name = "establecimientos") %>%
  arrange(desc(establecimientos))
write.csv(tabla_amg, "salidas/tablas/tabla_06_amg_municipios.csv", row.names = FALSE)

tabla_densidad <- tabla_nodos %>%
  left_join(macro_economia_control, by = "nodo_urbano") %>%
  mutate(densidad_10mil = round(establecimientos / total_establecimientos_ciudad * 10000, 2))
write.csv(tabla_densidad, "salidas/tablas/tabla_07_densidad.csv", row.names = FALSE)

cat("\n[Fase 4 completada]\n")

#-----------------------------------------------------------------------------
# FASE 5: VISUALIZACION ESTADISTICA
#-----------------------------------------------------------------------------

cat("\n[Fase 5] Generando gráficas descriptivas...\n")

png("salidas/graficas/figura_01_ cscm_total.png", width = 1800, height = 1200, res = 300)
ggplot(tabla_cscm_total, aes(x = reorder(macro_grupo, establecimientos), y = establecimientos)) +
  geom_col() + coord_flip() + theme_minimal() +
  labs(x = "", y = "Establecimientos", title = "Industrias culturales y creativas CSCM")
dev.off()

png("salidas/graficas/figura_02_cscm_nodos.png", width = 2200, height = 1400, res = 300)
ggplot(tabla_cscm_nodo, aes(x = nodo_urbano, y = establecimientos, fill = macro_grupo)) +
  geom_col(position = "stack") + theme_minimal()
dev.off()


cat("\n[Fase 5 completada]\n")

# ------------------------------------------------------------------------------
# FASE 6: PRUEBAS CHI-CUADRADA Y V DE CRAMER
# ------------------------------------------------------------------------------

cat("\n[Fase 6] Ejecutando pruebas de asociacion territorial...\n")

library(vcd)
library(rstatix)
library(tibble)

dir.create("salidas/estadisticos", recursive = TRUE, showWarnings = FALSE)

# ==========================================================
# 6.1 CSCM
# ====================================================

cat("\n--- CSCM ---\n")

tabla_chi_cscm <- icc_sf_cscm %>%
  st_drop_geometry() %>%
  count(nodo_urbano, macro_grupo) %>%
  pivot_wider(names_from = macro_grupo, values_from = n, values_fill = 0)

matriz_cscm <- tabla_chi_cscm %>%
  column_to_rownames("nodo_urbano") %>%
  as.matrix()

chi_cscm <- chisq.test(matriz_cscm)
cramer_cscm <- assocstats(matriz_cscm)
residuos_cscm <- chi_cscm$stdres %>% as.data.frame() %>% tibble::rownames_to_column("nodo_urbano")

# Exportacion CSCM
tabla_resultados_chi <- data.frame(
  Sistema = "CSCM",
  Chi_cuadrada = unname(chi_cscm$statistic),
  gl = unname(chi_cscm$parameter),
  p_value = chi_cscm$p.value,
  V_Cramer = cramer_cscm$cramer
)

write.csv(
  tabla_resultados_chi,
  "salidas/estadisticos/tabla_09_chi_cuadrada.csv",
  row.names = FALSE
)
write.csv(as.data.frame(matriz_cscm), "salidas/estadisticos/tabla_contingencia_CSCM.csv")
write.csv(residuos_cscm, "salidas/estadisticos/residuos_estandarizados_CSCM.csv", row.names = FALSE)

sink("salidas/estadisticos/chi_cuadrada_CSCM.txt")
cat("CHI-CUADRADA CSCM\n\n")
print(chi_cscm)
cat("\n\n")
cat("V DE CRAMER\n\n")
print(cramer_cscm)
sink()

cat("\nCSCM p-value:", chi_cscm$p.value)

cat("\n=====================================================\n")
cat("Fase 6 completada\n")
cat("=====================================================\n")

# ------------------------------------------------------------------------------
# FASE 8: DIMENSIONES DOMINANTES Y ESPECIALIZACIÓN TERRITORIAL
# ------------------------------------------------------------------------------

cat("\n[Fase 8] Analizando dimensiones dominantes...\n")

# ============
# DIRECTORIOS
# =================================

dir.create("salidas/fase8", recursive = TRUE, showWarnings = FALSE)
dir.create("salidas/fase8/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("salidas/fase8/graficas", recursive = TRUE, showWarnings = FALSE)
dir.create("salidas/fase8/mapas_LQ", recursive = TRUE, showWarnings = FALSE)

# ===================================================
# DIMENSIONES DOMINANTES
# ====================================================

dominantes_caracteristicas <- c("Manufactura Cultural", "Diseño y Publicidad", "Educación Artística", "Organizaciones Culturales")
dominantes_artesanias <- c("Artesanías Alimentarias", "Metal, Joyería y Manufactura", "Madera y Papel")

# ==================================================
# 8.1 TABLA 
totales_nodo <- icc_sf_cscm %>%
  st_drop_geometry() %>%
  count(
    nodo_urbano,
    name = "total_nodo"
  )

total_general <- icc_sf_cscm %>%
  st_drop_geometry() %>%
  nrow()
tabla_dimensiones <- icc_sf_cscm %>%
  st_drop_geometry() %>%
  count(
    nodo_urbano,
    macro_grupo,
    dimension,
    name = "establecimientos"
  )

totales_dimension <- tabla_dimensiones %>%
  group_by(
    dimension
  ) %>%
  summarise(
    total_dimension =
      sum(establecimientos),
    .groups = "drop"
  )
lq_dimensiones <- tabla_dimensiones %>%
  
  left_join(
    totales_nodo,
    by = "nodo_urbano"
  ) %>%
  
  left_join(
    totales_dimension,
    by = "dimension"
  ) %>%
  
  mutate(
    
    LQ =
      
      (establecimientos / total_nodo) /
      
      (total_dimension / total_general)
    
  )
lq_caracteristicas <- lq_dimensiones %>%
  filter(
    macro_grupo == "Características"
  )

write.csv(
  lq_caracteristicas,
  "salidas/fase8/tablas/lq_caracteristicas.csv",
  row.names = FALSE
)
lq_artesanias <- lq_dimensiones %>%
  filter(
    macro_grupo == "Artesanías"
  )

write.csv(
  lq_artesanias,
  "salidas/fase8/tablas/lq_artesanias.csv",
  row.names = FALSE
)
caracteristicas_dominantes <-
  
  lq_caracteristicas %>%
  
  group_by(
    nodo_urbano
  ) %>%
  
  slice_max(
    LQ,
    n = 1,
    with_ties = FALSE
  ) %>%
  
  ungroup()

write.csv(
  caracteristicas_dominantes,
  "salidas/fase8/tablas/caracteristicas_dominantes.csv",
  row.names = FALSE
)
artesanias_dominantes <-
  
  lq_artesanias %>%
  
  group_by(
    nodo_urbano
  ) %>%
  
  slice_max(
    LQ,
    n = 1,
    with_ties = FALSE
  ) %>%
  
  ungroup()

write.csv(
  artesanias_dominantes,
  "salidas/fase8/tablas/artesanias_dominantes.csv",
  row.names = FALSE
)
png(
  "salidas/fase8/graficas/lq_caracteristicas.png",
  width = 2400,
  height = 1600,
  res = 300
)

ggplot(
  lq_caracteristicas,
  aes(
    dimension,
    LQ,
    fill = nodo_urbano
  )
) +
  geom_col(position = "dodge") +
  geom_hline(
    yintercept = 1,
    linetype = 2
  ) +
  coord_flip() +
  theme_minimal()

dev.off()
png(
  "salidas/fase8/graficas/lq_artesanias.png",
  width = 2400,
  height = 1600,
  res = 300
)

ggplot(
  lq_artesanias,
  aes(
    dimension,
    LQ,
    fill = nodo_urbano
  )
) +
  geom_col(position = "dodge") +
  geom_hline(
    yintercept = 1,
    linetype = 2
  ) +
  coord_flip() +
  theme_minimal()

dev.off()

# ============================================================
# 8.7 MAPA DE ESPECIALIZACIÓN (LQ por dimensión) - Mapa de calor
# =======================================================

lq_completo <- bind_rows(lq_caracteristicas, lq_artesanias)

png("salidas/fase8/graficas/mapa_especializacion_LQ.png", width = 2800, height = 2000, res = 300)
ggplot(lq_completo, aes(x = nodo_urbano, y = dimension, fill = LQ)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "darkred", midpoint = 1, name = "LQ") +
  geom_text(aes(label = round(LQ, 2)), size = 3.5, color = "black") +
  theme_minimal() +
  labs(title = "Especialización territorial por dimensión creativa", subtitle = "Location Quotient (LQ) - Valores > 1 indican especialización", x = "Nodo urbano", y = "Dimensión") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())
dev.off()

# Mapa de burbujas
png("salidas/fase8/graficas/mapa_burbujas_LQ.png", width = 2800, height = 2000, res = 300)
ggplot(lq_completo, aes(x = nodo_urbano, y = LQ, size = LQ, color = dimension)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  scale_size_continuous(range = c(2, 14), name = "LQ") +
  scale_color_brewer(palette = "Set2", name = "Dimensión") +
  theme_minimal() +
  labs(title = "Especialización territorial por dimensión creativa", subtitle = "Línea discontinua representa LQ = 1", x = "Nodo urbano", y = "Location Quotient (LQ)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# Tabla de especialización destacada
tabla_especializacion <- lq_completo %>% filter(LQ > 1.2) %>% arrange(desc(LQ))
write.csv(tabla_especializacion, "salidas/fase8/tablas/especializacion_destacada.csv", row.names = FALSE)

# ==============================================================
# 8.8 MAPA TERRITORIAL DE ESPECIALIZACIÓN (LQ por AGEB)
# ===============================================================

cat("\n  Generando mapas de especialización territorial...\n")

# Crear límites de nodos por convex hull (desde puntos ICC)
crear_convex_hull <- function(nodo_nombre) {
  puntos <- icc_sf_base %>% filter(nodo_urbano == nodo_nombre)
  if (nrow(puntos) < 3) return(NULL)
  hull <- puntos %>% st_union() %>% st_convex_hull() %>% st_as_sf() %>% mutate(nodo_urbano = nodo_nombre)
  hull <- st_buffer(hull, dist = 0)
  return(hull)
}

amg_hull <- crear_convex_hull("AMG")
pv_hull <- crear_convex_hull("Puerto Vallarta")
tijuana_hull <- crear_convex_hull("Tijuana")
oaxaca_hull <- crear_convex_hull("Oaxaca")

nodos_completos <- bind_rows(amg_hull, pv_hull, tijuana_hull, oaxaca_hull) %>% st_transform(6372)

# Verificar áreas
cat("\n  Áreas de los nodos (convex hull - km2):\n")
nodos_completos %>%
  mutate(area_km2 = round(as.numeric(st_area(.)) / 1e6, 2)) %>%
  st_drop_geometry() %>%
  select(nodo_urbano, area_km2) %>%
  print()

# Preparar datos LQ por AGEB
dim_ejemplo <- "Manufactura Cultural"
puntos_dim <- icc_sf_cscm %>% filter(dimension == dim_ejemplo)
conteo_ageb <- puntos_dim %>% st_drop_geometry() %>% count(ageb_id, name = "n_establecimientos")
total_nodo <- icc_sf_cscm %>% st_drop_geometry() %>% filter(nodo_urbano %in% unique(puntos_dim$nodo_urbano)) %>% nrow()

ageb_lq <- ageb_nodos %>%
  filter(ciudad %in% unique(puntos_dim$nodo_urbano)) %>%
  left_join(conteo_ageb, by = "ageb_id") %>%
  mutate(n_establecimientos = replace_na(n_establecimientos, 0)) %>%
  group_by(ciudad) %>%
  mutate(LQ = (n_establecimientos / sum(n_establecimientos)) / (sum(n_establecimientos) / total_nodo)) %>%
  ungroup() %>%
  mutate(LQ_cat = case_when(
    LQ > 1.5 ~ "Alta especialización (>1.5)",
    LQ > 1 ~ "Especialización (1-1.5)",
    LQ > 0.5 ~ "Proporcional (0.5-1)",
    TRUE ~ "Baja especialización (<0.5)"
  ))

# Generar mapas por nodo
for (n in unique(puntos_dim$nodo_urbano)) {
  cat(paste("    Procesando:", n, "\n"))
  
  borde_nodo <- nodos_completos %>% filter(nodo_urbano == n)
  if (nrow(borde_nodo) == 0) next
  
  ageb_n <- ageb_lq %>% filter(ciudad == n)
  
  mapa <- tm_shape(borde_nodo) +
    tm_fill(col = "grey95") +
    tm_borders(col = "black", lwd = 1.5) +
    tm_shape(ageb_n) +
    tm_fill(col = "LQ_cat", palette = c("#d73027", "#fc8d59", "#fee090", "#e0f3f8"), title = "Especialización", alpha = 0.85) +
    tm_borders(col = "grey50", lwd = 0.3) +
    tm_scalebar(position = c("left", "bottom")) +
    tm_compass(position = c("right", "top")) +
    tm_title(paste(dim_ejemplo, "-", n)) +
    tm_layout(legend.outside = TRUE, legend.outside.position = "right", frame = FALSE, bg.color = "white")
  
  tmap_save(mapa, filename = paste0("salidas/fase8/mapas_LQ/mapa_LQ_", n, "_", gsub(" ", "_", dim_ejemplo), ".png"), 
            width = 20, height = 20, units = "cm", dpi = 300)
}

# Mapa resumen
mapa_resumen <- tm_shape(nodos_completos) +
  tm_fill(col = "lightblue", alpha = 0.4) +
  tm_borders(col = "black", lwd = 1) +
  tm_text(text = "nodo_urbano", size = 0.7, just = "center") +
  tm_title("Áreas de estudio por convex hull") +
  tm_layout(frame = FALSE, bg.color = "white", legend.show = FALSE)

tmap_save(mapa_resumen, filename = "salidas/fase8/mapas_LQ/mapa_resumen_convex_hull.png", width = 25, height = 20, units = "cm", dpi = 300)

cat("\n  Mapas guardados en salidas/fase8/mapas_LQ/\n")

#--------------
#Mapas características
for(n in unique(icc_sf_cscm$nodo_urbano)){
  
  datos <- icc_sf_cscm %>%
    filter(
      nodo_urbano == n,
      macro_grupo == "Características"
    )
  
  mapa <- tm_shape(datos) +
    tm_dots(
      col = "darkblue",
      size = 0.02
    ) +
    tm_layout(
      title = paste(
        "Actividades características -",
        n
      )
    )
  
  tmap_save(
    mapa,
    paste0(
      "salidas/fase8/mapas_LQ/caracteristicas_",
      gsub(" ","_",n),
      ".png"
    )
  )
  
}
#Mapas artesanías
for(n in unique(icc_sf_cscm$nodo_urbano)){
  
  datos <- icc_sf_cscm %>%
    filter(
      nodo_urbano == n,
      macro_grupo == "Artesanías"
    )
  
  mapa <- tm_shape(datos) +
    tm_dots(
      col = "darkred",
      size = 0.02
    ) +
    tm_layout(
      title = paste(
        "Actividades artesanales -",
        n
      )
    )
  
  tmap_save(
    mapa,
    paste0(
      "salidas/fase8/mapas_LQ/artesanias_",
      gsub(" ","_",n),
      ".png"
    )
  )
  
}
#Mapas dimensión dominante
dominantes_nodo <- bind_rows(
  caracteristicas_dominantes,
  artesanias_dominantes
)

for(i in 1:nrow(dominantes_nodo)){
  
  nodo_i <- dominantes_nodo$nodo_urbano[i]
  
  dim_i <- dominantes_nodo$dimension[i]
  
  datos <- icc_sf_cscm %>%
    filter(
      nodo_urbano == nodo_i,
      dimension == dim_i
    )
  
  mapa <- tm_shape(datos) +
    tm_dots(
      col = "goldenrod",
      size = 0.03
    ) +
    tm_layout(
      title = paste(
        dim_i,
        "-",
        nodo_i
      )
    )
  
  tmap_save(
    mapa,
    paste0(
      "salidas/fase8/mapas_LQ/dominante_",
      gsub(" ","_",nodo_i),
      ".png"
    )
  )
  
}

# =========================================================
# FIN FASE 8
# =============================================================

cat("\n===========================================\n")
cat("FASE 8 COMPLETADA\n")
cat("- Tablas: caracteristicas_dominantes.csv, artesanias_dominantes.csv, lq_*.csv, especializacion_destacada.csv\n")
cat("- Gráficas: lq_caracteristicas.png, lq_artesanias.png, mapa_especializacion_LQ.png, mapa_burbujas_LQ.png\n")
cat("- Mapas territoriales: mapas_LQ/mapa_LQ_*.png, mapa_resumen_convex_hull.png\n")
cat("- Chi-cuadrada: chi_caracteristicas.txt, chi_artesanias.txt\n")
cat("===========================================\n")

# =====================================================
# FASE 9
# CENSO ECONÓMICO 2024
# =====================================================

cat("\n[Fase 9] Integrando Censo Económico 2024...\n")

library(data.table)
library(dplyr)
library(tidyverse)

# -----------------------------------------------------
# CARGA DE ARCHIVOS
# -----------------------------------------------------

archivos_censo <- list.files(
  "datos/censo2024",
  pattern = "\\.csv$",
  full.names = TRUE
)

censo_total <- lapply(
  archivos_censo,
  fread
) %>%
  bind_rows()

# -----------------------------------------------------
# MUNICIPIOS DE ESTUDIO
# -----------------------------------------------------

municipios_estudio <- tribble(
  ~E03, ~E04, ~nodo_urbano,
  
  "14","039","AMG",
  "14","070","AMG",
  "14","097","AMG",
  "14","098","AMG",
  "14","101","AMG",
  "14","120","AMG",
  "14","117","AMG",
  "14","111","AMG",
  "14","041","AMG",
  
  "14","067","Puerto Vallarta",
  
  "20","067","Oaxaca",
  
  "02","004","Tijuana"
)

# -----------------------------------------------------
# FILTRADO DE MUNICIPIOS
# -----------------------------------------------------

censo_nodos <- censo_total %>%
  
  mutate(
    
    E03 = sprintf(
      "%02d",
      as.integer(E03)
    ),
    
    E04 = sprintf(
      "%03d",
      as.integer(E04)
    ),
    
    CLASE = as.character(CLASE)
    
  ) %>%
  
  inner_join(
    municipios_estudio,
    by = c("E03","E04")
  )

cat(
  "Registros Censo:",
  nrow(censo_nodos),
  "\n"
)

# -----------------------------------------------------
# CÓDIGOS CSCM
# -----------------------------------------------------

codigos_icc <- unique(
  catalogo_cscm$codigo
)

censo_icc <- censo_nodos %>%
  
  filter(
    CLASE %in% codigos_icc
  )

cat(
  "Registros ICC:",
  nrow(censo_icc),
  "\n"
)

# -----------------------------------------------------
# CLASIFICACIÓN CSCM
# -----------------------------------------------------

censo_icc <- censo_icc %>%
  
  left_join(
    
    catalogo_cscm %>%
      select(
        codigo,
        macro_grupo
      ),
    
    by = c(
      "CLASE" = "codigo"
    )
    
  ) %>%
  
  left_join(
    
    dimensiones_cscm,
    
    by = c(
      "CLASE" = "codigo"
    ),
    
    relationship = "many-to-many"
    
  )

# -----------------------------------------------------
# VARIABLES ECONÓMICAS
# -----------------------------------------------------

censo_icc <- censo_icc %>%
  
  mutate(
    
    establecimientos =
      as.numeric(UE),
    
    empleo =
      as.numeric(H001A),
    
    ingresos =
      as.numeric(M000A),
    
    valor_agregado =
      as.numeric(A131A)
    
  )

# -----------------------------------------------------
# CONTROL DE CALIDAD
# -----------------------------------------------------

cat(
  "\nMacrogrupos encontrados:\n"
)

print(
  table(
    censo_icc$macro_grupo,
    useNA = "ifany"
  )
)

cat(
  "\nDimensiones encontradas:\n"
)

print(
  length(
    unique(
      censo_icc$dimension
    )
  )
)

cat(
  "\nEstablecimientos:",
  sum(
    censo_icc$establecimientos,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Empleo:",
  sum(
    censo_icc$empleo,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Ingresos:",
  sum(
    censo_icc$ingresos,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Valor agregado:",
  sum(
    censo_icc$valor_agregado,
    na.rm = TRUE
  ),
  "\n"
)

cat("\n[Fase 9 completada]\n")

# =====================================================
# FASE 10 LQ ECONÓMICO CREATIVO POR MACROGRUPOS CSCM
# =====================================================

cat("\n[Fase 10] Calculando especialización económica...\n")

dir.create(
  "salidas/censo2024",
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------
# RESUMEN POR NODO
# ------------------------------------------

tabla_economica <- censo_icc %>%
  
  group_by(
    nodo_urbano
  ) %>%
  
  summarise(
    
    establecimientos =
      sum(
        establecimientos,
        na.rm = TRUE
      ),
    
    empleo =
      sum(
        empleo,
        na.rm = TRUE
      ),
    
    ingresos =
      sum(
        ingresos,
        na.rm = TRUE
      ),
    
    valor_agregado =
      sum(
        valor_agregado,
        na.rm = TRUE
      ),
    
    .groups = "drop"
    
  )

write.csv(
  tabla_economica,
  "salidas/censo2024/resumen_economico.csv",
  row.names = FALSE
)

# ------------------------------------------
# TABLA BASE POR MACROGRUPO
# ------------------------------------------

tabla_macro <-
  
  censo_icc %>%
  
  group_by(
    nodo_urbano,
    macro_grupo
  ) %>%
  
  summarise(
    
    establecimientos =
      sum(
        establecimientos,
        na.rm = TRUE
      ),
    
    empleo =
      sum(
        empleo,
        na.rm = TRUE
      ),
    
    valor_agregado =
      sum(
        valor_agregado,
        na.rm = TRUE
      ),
    
    .groups = "drop"
    
  )

# =====================================================
# LQ ESTABLECIMIENTOS
# =====================================================

totales_macro_est <-
  
  tabla_macro %>%
  
  group_by(
    macro_grupo
  ) %>%
  
  summarise(
    
    E_i =
      sum(
        establecimientos,
        na.rm = TRUE
      ),
    
    .groups = "drop"
    
  )

E_total_est <-
  
  sum(
    tabla_macro$establecimientos,
    na.rm = TRUE
  )

tabla_lq_est <-
  
  tabla_macro %>%
  
  left_join(
    totales_macro_est,
    by = "macro_grupo"
  ) %>%
  
  group_by(
    nodo_urbano
  ) %>%
  
  mutate(
    
    e_j =
      sum(
        establecimientos,
        na.rm = TRUE
      )
    
  ) %>%
  
  ungroup() %>%
  
  mutate(
    
    LQ_est =
      
      (establecimientos / e_j) /
      
      (E_i / E_total_est)
    
  )

# =====================================================
# LQ EMPLEO
# =====================================================

totales_macro_emp <-
  
  tabla_macro %>%
  
  group_by(
    macro_grupo
  ) %>%
  
  summarise(
    
    E_i =
      sum(
        empleo,
        na.rm = TRUE
      ),
    
    .groups = "drop"
    
  )

E_total_emp <-
  
  sum(
    tabla_macro$empleo,
    na.rm = TRUE
  )

tabla_lq_emp <-
  
  tabla_macro %>%
  
  left_join(
    totales_macro_emp,
    by = "macro_grupo"
  ) %>%
  
  group_by(
    nodo_urbano
  ) %>%
  
  mutate(
    
    e_j =
      sum(
        empleo,
        na.rm = TRUE
      )
    
  ) %>%
  
  ungroup() %>%
  
  mutate(
    
    LQ_empleo =
      
      (empleo / e_j) /
      
      (E_i / E_total_emp)
    
  )

# =====================================================
# LQ VALOR AGREGADO
# =====================================================

totales_macro_va <-
  
  tabla_macro %>%
  
  group_by(
    macro_grupo
  ) %>%
  
  summarise(
    
    E_i =
      sum(
        valor_agregado,
        na.rm = TRUE
      ),
    
    .groups = "drop"
    
  )

E_total_va <-
  
  sum(
    tabla_macro$valor_agregado,
    na.rm = TRUE
  )

tabla_lq_va <-
  
  tabla_macro %>%
  
  left_join(
    totales_macro_va,
    by = "macro_grupo"
  ) %>%
  
  group_by(
    nodo_urbano
  ) %>%
  
  mutate(
    
    e_j =
      sum(
        valor_agregado,
        na.rm = TRUE
      )
    
  ) %>%
  
  ungroup() %>%
  
  mutate(
    
    LQ_valor_agregado =
      
      (valor_agregado / e_j) /
      
      (E_i / E_total_va)
    
  )

# =====================================================
# EXPORTACIÓN
# =====================================================

write.csv(
  tabla_lq_est,
  "salidas/censo2024/lq_establecimientos.csv",
  row.names = FALSE
)

write.csv(
  tabla_lq_emp,
  "salidas/censo2024/lq_empleo.csv",
  row.names = FALSE
)

write.csv(
  tabla_lq_va,
  "salidas/censo2024/lq_valor_agregado.csv",
  row.names = FALSE
)
#--------------------------------
tabla4_lq_est <-
  
  tabla_lq_est %>%
  
  select(
    nodo_urbano,
    macro_grupo,
    establecimientos,
    LQ_est
  ) %>%
  
  arrange(
    macro_grupo,
    desc(LQ_est)
  )

write.csv(
  tabla4_lq_est,
  "salidas/censo2024/tabla_04_lq_establecimientos.csv",
  row.names = FALSE
)

tabla5_lq_emp <-
  
  tabla_lq_emp %>%
  
  select(
    nodo_urbano,
    macro_grupo,
    empleo,
    LQ_empleo
  ) %>%
  
  arrange(
    macro_grupo,
    desc(LQ_empleo)
  )

write.csv(
  tabla5_lq_emp,
  "salidas/censo2024/tabla_05_lq_empleo.csv",
  row.names = FALSE
)
tabla6_lq_va <-
  
  tabla_lq_va %>%
  
  select(
    nodo_urbano,
    macro_grupo,
    valor_agregado,
    LQ_valor_agregado
  ) %>%
  
  arrange(
    macro_grupo,
    desc(LQ_valor_agregado)
  )

write.csv(
  tabla6_lq_va,
  "salidas/censo2024/tabla_06_lq_valor_agregado.csv",
  row.names = FALSE
)

cat("[Fase 10 completada]\n")

# ==========================================================
# FASE 11  LQ POR DIMENSIONES CSCM
# ==========================================================

cat("\n[Fase 11] Especialización por dimensiones CSCM...\n")

dir.create(
  "salidas/fase11",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "salidas/fase11/tablas",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "salidas/fase11/graficas",
  recursive = TRUE,
  showWarnings = FALSE
)

tabla_dimensiones <- censo_icc %>%
  
  group_by(
    nodo_urbano,
    macro_grupo,
    dimension
  ) %>%
  
  summarise(
    
    establecimientos =
      sum(establecimientos, na.rm = TRUE),
    
    empleo =
      sum(empleo, na.rm = TRUE),
    
    valor_agregado =
      sum(valor_agregado, na.rm = TRUE),
    
    .groups = "drop"
    
  )

total_dimension_est <-
  
  tabla_dimensiones %>%
  
  group_by(
    dimension
  ) %>%
  
  summarise(
    total_est =
      sum(
        establecimientos
      )
  )

lq_est_dim <-
  
  tabla_dimensiones %>%
  
  left_join(
    total_dimension_est,
    by = "dimension"
  ) %>%
  
  group_by(
    nodo_urbano
  ) %>%
  
  mutate(
    
    total_nodo =
      sum(
        establecimientos
      ),
    
    total_dimension =
      sum(
        total_est
      )
    
  ) %>%
  
  ungroup() %>%
  
  mutate(
    
    LQ_est =
      
      (establecimientos / total_nodo) /
      
      (total_est / total_dimension)
    
  )

total_dimension_emp <-
  
  tabla_dimensiones %>%
  
  group_by(
    dimension
  ) %>%
  
  summarise(
    total_emp =
      sum(
        empleo,
        na.rm = TRUE
      )
  )

lq_emp_dim <-
  
  tabla_dimensiones %>%
  
  left_join(
    total_dimension_emp,
    by = "dimension"
  ) %>%
  
  group_by(
    nodo_urbano
  ) %>%
  
  mutate(
    
    total_nodo =
      sum(
        empleo,
        na.rm = TRUE
      ),
    
    total_dimension =
      sum(
        total_emp
      )
    
  ) %>%
  
  ungroup() %>%
  
  mutate(
    
    LQ_empleo =
      
      (empleo / total_nodo) /
      
      (total_emp / total_dimension)
    
  )

total_dimension_va <-
  
  tabla_dimensiones %>%
  
  group_by(
    dimension
  ) %>%
  
  summarise(
    total_va =
      sum(
        valor_agregado,
        na.rm = TRUE
      )
  )

lq_va_dim <-
  
  tabla_dimensiones %>%
  
  left_join(
    total_dimension_va,
    by = "dimension"
  ) %>%
  
  group_by(
    nodo_urbano
  ) %>%
  
  mutate(
    
    total_nodo =
      sum(
        valor_agregado,
        na.rm = TRUE
      ),
    
    total_dimension =
      sum(
        total_va
      )
    
  ) %>%
  
  ungroup() %>%
  
  mutate(
    
    LQ_valor_agregado =
      
      (valor_agregado / total_nodo) /
      
      (total_va / total_dimension)
    
  )

tabla_final_lq <-
  
  lq_est_dim %>%
  
  select(
    nodo_urbano,
    macro_grupo,
    dimension,
    establecimientos,
    LQ_est
  ) %>%
  
  left_join(
    
    lq_emp_dim %>%
      
      select(
        nodo_urbano,
        dimension,
        empleo,
        LQ_empleo
      ),
    
    by = c(
      "nodo_urbano",
      "dimension"
    )
    
  ) %>%
  
  left_join(
    
    lq_va_dim %>%
      
      select(
        nodo_urbano,
        dimension,
        valor_agregado,
        LQ_valor_agregado
      ),
    
    by = c(
      "nodo_urbano",
      "dimension"
    )
    
  )

write.csv(
  tabla_final_lq,
  "salidas/fase11/tablas/lq_dimensiones.csv",
  row.names = FALSE
)

dimensiones_dominantes <-
  
  tabla_final_lq %>%
  
  filter(
    LQ_valor_agregado > 1
  ) %>%
  
  arrange(
    nodo_urbano,
    desc(
      LQ_valor_agregado
    )
  )

write.csv(
  dimensiones_dominantes,
  "salidas/fase11/tablas/dimensiones_dominantes.csv",
  row.names = FALSE
)

write.csv(
  dimensiones_dominantes,
  "salidas/fase11/tablas/tabla_07_dimensiones_dominantes.csv",
  row.names = FALSE
)

# ==========================================================
# FIGURA 5
# DIMENSIONES CULTURALES DOMINANTES
# ==========================================================

figura5 <- dimensiones_dominantes %>%
  
  group_by(
    nodo_urbano
  ) %>%
  
  slice_max(
    order_by = LQ_valor_agregado,
    n = 1,
    with_ties = FALSE
  ) %>%
  
  ungroup()

write.csv(
  figura5,
  "salidas/fase11/tablas/tabla_07_dimensiones_dominantes.csv",
  row.names = FALSE
)

png(
  "salidas/fase11/graficas/figura_05_dimensiones_dominantes.png",
  width = 2400,
  height = 1600,
  res = 300
)

ggplot(
  figura5,
  aes(
    x = reorder(
      paste(nodo_urbano, dimension, sep = " - "),
      LQ_valor_agregado
    ),
    y = LQ_valor_agregado,
    fill = nodo_urbano
  )
) +
  
  geom_col() +
  
  geom_hline(
    yintercept = 1,
    linetype = "dashed"
  ) +
  
  coord_flip() +
  
  theme_minimal() +
  
  labs(
    title = "Dimensiones culturales dominantes por nodo urbano",
    subtitle = "Especialización medida mediante LQ de valor agregado",
    x = "",
    y = "LQ"
  ) +
  
  theme(
    legend.position = "none"
  )

dev.off()

tabla_08 <- dimensiones_dominantes %>%
  filter(macro_grupo == "Artesanías")

write.csv(
  tabla_08,
  "salidas/fase11/tablas/tabla_08_dimensiones_artesanales_dominantes.csv",
  row.names = FALSE
)

# ==========================================================
# FASE 12
# MAPAS DE ESPECIALIZACIÓN TERRITORIAL DESTACADA
# ==========================================================

cat("\n[Fase 12] Generando mapas territoriales finales...\n")

dir.create(
  "salidas/fase12",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "salidas/fase12/mapas",
  recursive = TRUE,
  showWarnings = FALSE
)

# ==========================================================
# CONTEO POR AGEB Y DIMENSIÓN
# ==========================================================

ageb_dimension <-
  
  icc_sf_cscm %>%
  
  st_drop_geometry() %>%
  
  count(
    nodo_urbano,
    ageb_id,
    dimension,
    name = "establecimientos"
  )

# ==========================================================
# DIMENSIÓN DOMINANTE POR AGEB
# ==========================================================

dimension_dominante_ageb <-
  
  ageb_dimension %>%
  
  group_by(
    nodo_urbano,
    ageb_id
  ) %>%
  
  slice_max(
    establecimientos,
    n = 1,
    with_ties = FALSE
  ) %>%
  
  ungroup()

# ==========================================================
# FILTRO DE ESPECIALIZACIÓN DESTACADA
# ==========================================================

umbral <-
  
  quantile(
    dimension_dominante_ageb$establecimientos,
    probs = 0.75,
    na.rm = TRUE
  )

dimension_dominante_ageb <-
  
  dimension_dominante_ageb %>%
  
  filter(
    establecimientos >= umbral
  )

# ==========================================================
# UNIÓN CON AGEB
# ==========================================================

mapa_ageb_dominante <-
  
  ageb_nodos %>%
  
  left_join(
    dimension_dominante_ageb,
    by = "ageb_id"
  )

# ==========================================================
# PALETA
# ==========================================================

paleta_dimensiones <- c(
  
  "Manufactura Cultural" = "#d73027",
  "Edición y Publicación" = "#fc8d59",
  "Audiovisual y Software" = "#fee08b",
  "Radio y Televisión" = "#ffffbf",
  "Internet y Medios Digitales" = "#d9ef8b",
  "Diseño y Publicidad" = "#91cf60",
  "Educación Artística" = "#1a9850",
  "Artes Escénicas" = "#66c2a5",
  "Patrimonio y Museos" = "#3288bd",
  "Fotografía y Servicios Visuales" = "#5e4fa2",
  
  "Artesanías Alimentarias" = "#8c510a",
  "Textiles y Vestimenta" = "#bf812d",
  "Cuero y Calzado" = "#dfc27d",
  "Madera y Papel" = "#80cdc1",
  "Cerámica, Vidrio y Piedra" = "#35978f",
  "Metal, Joyería y Manufactura" = "#01665e",
  
  "Organizaciones Culturales" = "#762a83",
  "Deportes" = "#af8dc3"
  
)

# ==========================================================
# MAPAS POR NODO
# ==========================================================

for(nodo in unique(mapa_ageb_dominante$ciudad)) {
  
  cat(
    "Procesando:",
    nodo,
    "\n"
  )
  
  mapa_nodo <-
    
    mapa_ageb_dominante %>%
    
    filter(
      ciudad == nodo
    )
  
  mapa <-
    
    tm_shape(mapa_nodo) +
    
    tm_polygons(
      col = "dimension",
      palette = paleta_dimensiones,
      border.col = "grey70",
      lwd = 0.3,
      title = "Dimensión dominante"
    ) +
    
    tm_layout(
      frame = FALSE,
      legend.outside = TRUE,
      legend.outside.position = "right",
      main.title = paste(
        "Especialización territorial destacada",
        nodo
      )
    ) +
    
    tm_compass(
      position = c("right","top")
    ) +
    
    tm_scalebar(
      position = c("left","bottom")
    )
  
  tmap_save(
    mapa,
    filename = paste0(
      "salidas/fase12/mapas/",
      gsub(" ","_", nodo),
      "_especializacion_destacada.png"
    ),
    width = 25,
    height = 20,
    units = "cm",
    dpi = 300
  )
  
}

cat("\n[Fase 12 completada]\n")