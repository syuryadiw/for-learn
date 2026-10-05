# PT. Citra Mulia Inti (CMI) | Sanggala Corridor Project ----
# Land Cover Rapid Survey Analysis (ODK Data) - T=0 ARR
# Syuryadi Wijaya

# 1. ENVIRONMENT SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  googlesheets4, 
  tidyverse, 
  viridis, 
  scales, 
  leaflet, 
  writexl, 
  ggplot2, 
  sf, 
  leaflet.extras
)

theme_set(
  theme_minimal() +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(color = "gray35", size = 10),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom"
    )
)
gs4_deauth() 

# 2. DATA CLEANING & LOADING BOUNDARY ----
# 2.1 Survey Data ----
sheet_url <- "https://docs.google.com/spreadsheets/d/1YRFFE9gAI-_hcQoIyjV2XiSDJ5983Xx33pFE9O6v9mA/edit?usp=sharing"
raw_data  <- read_sheet(sheet_url)

# 2.2 Boundary & LULC Data ----
# Batas CMI
sf_cmi_petak <- read.csv("Batas_CMI.csv") %>% 
  st_as_sf(wkt = "WKT", crs = 4326) %>% 
  st_make_valid()

sf_cmi_outline <- sf_cmi_petak %>% 
  st_union() %>% 
  st_make_valid()

# LULC 2025 (SHP)
sf_lulc_2025 <- st_read("LULC 2025/LULC_TutupanLahan25_20260414_v1.shp") %>%
  st_transform(4326) %>%
  st_make_valid()

# 2.3 Transformasi Data ----
clean_data <- raw_data %>%
  separate(Koordinat, into = c("Latitude", "Longitude"), sep = ",", remove = FALSE, extra = "drop") %>%
  mutate(
    across(c(Latitude, Longitude), ~ as.numeric(trimws(.))),
    across(c(Desa, `Land Cover`, `Status Lahan`), as.factor),
    
    # Cleaning Teks Vegetasi yang Diringkas
    Vegetasi_Clean = `Vegetasi Dominan` %>% 
      iconv(to = "ASCII//TRANSLIT", sub = "") %>% 
      str_trim() %>% 
      str_to_title() %>% 
      str_remove("(?i)^(pohon|pohan)(/batang)?\\s+"),
    
    # Standarisasi Penamaan
    Vegetasi_Clean = case_when(
      str_detect(Vegetasi_Clean, "(?i)sawit|kelapak")         ~ "Kelapa Sawit",
      str_detect(Vegetasi_Clean, "(?i)durian")                 ~ "Durian",
      str_detect(Vegetasi_Clean, "(?i)karet|karpet")           ~ "Karet",
      str_detect(Vegetasi_Clean, "(?i)tengkawang")            ~ "Tengkawang",
      str_detect(Vegetasi_Clean, "(?i)timau|temau")            ~ "Timau",
      str_detect(Vegetasi_Clean, "(?i)leban|laban")            ~ "Leban",
      str_detect(Vegetasi_Clean, "(?i)mentawa|bitawa|bintawa") ~ "Bintawa",
      str_detect(Vegetasi_Clean, "(?i)meruat|memuat")          ~ "Meruat",
      str_detect(Vegetasi_Clean, "(?i)beringin")              ~ "Beringin",
      Vegetasi_Clean %in% c("Tidak Ada", "Tidak Ketahui", 
                            "Tidak Di Ketahui", 
                            "Tidak Tau", 
                            "Nama Tidak Di Ketahuai", 
                            "Kosong", "A") ~ "Unidentified",
      TRUE ~ Vegetasi_Clean
    )
  )

# 3. SUMMARY TABLES ----
table_landcover <- clean_data %>%
  count(Desa, `Land Cover`, name = "Jumlah_Point") %>%
  pivot_wider(names_from = Desa, values_from = Jumlah_Point, values_fill = 0) %>%
  mutate(
    Total_Point = rowSums(across(where(is.numeric))),
    Persentase  = round((Total_Point / sum(Total_Point)) * 100, 1)
  ) %>%
  arrange(desc(Total_Point))

table_vegetation <- clean_data %>%
  filter(Vegetasi_Clean != "Unidentified") %>%
  count(Vegetasi_Clean, name = "Total Point") %>%
  arrange(desc(`Total Point`)) %>%
  mutate(No = row_number(), `Vegetasi Dominan` = Vegetasi_Clean) %>%
  select(No, `Vegetasi Dominan`, `Total Point`)

table_status_lahan <- clean_data %>%
  count(`Status Lahan`, name = "Total Point") %>%
  arrange(desc(`Total Point`))

# 4. VISUALIZATION ----
# 4.1 Plot Land Cover ----
plot_landcover <- clean_data %>%
  count(Desa, `Land Cover`) %>%
  ggplot(aes(x = fct_reorder(`Land Cover`, n, .fun = sum), 
             y = n, 
             fill = Desa)) +
  geom_col(position = "stack", 
           width = 0.7, 
           color = "white", 
           linewidth = 0.2) +
  geom_text(aes(label = ifelse(n > 0, n, "")), 
            position = position_stack(vjust = 0.5), 
            color = "white", 
            size = 3, 
            fontface = "bold") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  scale_fill_viridis_d(option = "plasma", 
                       begin = 0.2, 
                       end = 0.8) +
  labs(
    title    = "Distribusi Tutupan Lahan (Land Cover)", 
    subtitle = "Sanggala Corridor Project - Rapid Survey 2026", 
    x        = "Land Cover", 
    y        = "Total Sampling Points"
  ) +
  theme(
    axis.text.y  = element_text(size = 10, face = "bold", color = "gray20"),
    axis.text.x  = element_text(size = 9, color = "gray30"),
    axis.title.y = element_text(size = 10, face = "bold", angle = 0, hjust = 1, vjust = 0.5),
    panel.grid.major.y = element_blank(),
    plot.title   = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "gray40", size = 9)
  )
plot_landcover

# 4.2 Plot Top 10 Dominant Vegetation ----
plot_vegetation <- clean_data %>%
  filter(Vegetasi_Clean != "Unidentified") %>%
  count(Vegetasi_Clean, name = "n") %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(Vegetasi_Clean, n), 
             y = n, 
             fill = n)) +
  geom_col(width = 0.65, 
           show.legend = FALSE) +
  geom_text(aes(label = n), 
            hjust = -0.4, 
            size = 3.5, 
            fontface = "bold") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  scale_fill_viridis_c(option = "turbo", 
                       begin = 0.3, 
                       end = 0.8) +
  labs(
    title    = "Top 10 Vegetasi Dominan",
    subtitle = "Sanggala Corridor Project - Rapid Survey 2026",
    x        = "Jenis Vegetasi",
    y        = "Frekuensi Titik Sampling"
  ) +  
  theme(
    axis.text.y  = element_text(size = 10, face = "bold", color = "gray20"),
    axis.text.x  = element_text(size = 9, color = "gray30"),
    axis.title.y = element_text(size = 10, face = "bold", angle = 0, hjust = 1, vjust = 0.5),
    panel.grid.major.y = element_blank(),
    plot.title   = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "gray40", size = 9)
  )
plot_vegetation

# 4.3 Plot Status Pengelolaan Lahan ----
plot_status_lahan <- clean_data %>%
  count(`Status Lahan`, name = "n") %>%
  ggplot(aes(x = reorder(`Status Lahan`, n), 
             y = n, 
             fill = `Status Lahan`)) +
  geom_col(show.legend = FALSE, width = 0.65) +
  geom_text(aes(label = n), 
            hjust = -0.4, 
            fontface = "bold", 
            size = 4) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  scale_fill_viridis_d(option = "plasma", 
                       begin = 0.2, 
                       end = 0.8) +
  labs(
    title    = "Status Pengelolaan Lahan",
    subtitle = "Sanggala Corridor Project - Rapid Survey 2026",
    y        = "Total Sampling Points"
  ) +  
  theme(
    axis.text.y  = element_text(size = 10, face = "bold", color = "gray20"),
    axis.text.x  = element_text(size = 9, color = "gray30"),
    axis.title.y = element_text(size = 10, face = "bold", angle = 0, hjust = 1, vjust = 0.5),
    panel.grid.major.y = element_blank(),
    plot.title   = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "gray40", size = 9)
  )
plot_status_lahan

# 4.4 Interactive Spatial Map (Leaflet) ----
nama_kolom_lulc <- "Class"

warna_lulc <- c(
  "Building"                  = "#9E9E9E",
  "Cleared / Bare Land"       = "#D7CCC8",
  "Cleared for Oil Palm"      = "#FFCC80",
  "Forest"                    = "#228B22",
  "Mixed Dryland Agriculture" = "#8BC34A",
  "Newly-planted Oil Palm"    = "#FF9800",
  "Oil Palm"                  = "#D32F2F",
  "Old Shrubs"                = "#CDDC39",
  "Road"                      = "#424242",
  "Shrubs"                    = "#C0CA33",
  "Water"                     = "#29B6F6"
)

pal_lulc_shp <- colorFactor(
  palette = warna_lulc,
  domain  = sf_lulc_2025[[nama_kolom_lulc]],
  levels  = names(warna_lulc)
)

pal_map <- colorFactor(viridis_pal(
  option = "inferno", 
  begin  = 0.3,
  end    = 0.9)(length(unique(clean_data$`Land Cover`))), 
  clean_data$`Land Cover`)

peta_spasial <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satelit") %>%
  addProviderTiles(providers$OpenStreetMap, group = "Peta Jalan") %>%
  
  addPolygons(
    data        = sf_lulc_2025,
    fillColor   = ~pal_lulc_shp(get(nama_kolom_lulc)),
    color       = "white",
    weight      = 0.5,
    fillOpacity = 0.5,
    popup       = ~paste0("<b>Kategori LULC (2025):</b> ", get(nama_kolom_lulc)),
    group       = "Peta LULC (2025)" 
  ) %>%
  
  addPolygons(
    data        = sf_cmi_petak,
    color       = "#8FBC8F", 
    weight      = 1,
    popup       = ~paste0("<b>Petak:</b> ", 
                          PETAKKERJA, "<br><b>Blok:</b> ", 
                          BLOK, "<br><b>Luas:</b> ", 
                          round(LUAS, 2), " Ha"),
    group       = "Petak Kerja CMI"
  ) %>%
  
  addPolygons(
    data        = sf_cmi_outline,
    color       = "#E07A5F", 
    weight      = 3.5,
    fill        = FALSE,
    group       = "Batas Luar Konsesi"
  ) %>%
  
  addCircleMarkers(
    data        = clean_data,
    lng         = ~Longitude, 
    lat         = ~Latitude, 
    color       = ~pal_map(`Land Cover`),
    radius      = 5, 
    fillOpacity = 0.9,
    stroke      = TRUE, 
    weight      = 1,
    popup       = ~paste0("<b>Desa:</b> ", 
                          Desa, "<br><b>Land Cover:</b> ", 
                          `Land Cover`, "<br><b>Vegetasi:</b> ", 
                          Vegetasi_Clean, "<br><b>Status Lahan:</b> ", 
                          `Status Lahan`),
    group       = "Titik Survei"
  ) %>%
  
  addLegend("bottomright", 
            pal     = pal_map, 
            values  = clean_data$`Land Cover`, 
            title   = "Land Cover (Titik Survei)", 
            opacity = 0.9) %>%
  
  addLegend("bottomleft", 
            pal     = pal_lulc_shp, 
            values  = names(warna_lulc), 
            title   = "Kelas LULC (2025)", 
            opacity = 0.8) %>%
  
  addLayersControl(
    baseGroups    = c("Satelit", "Peta Jalan"), 
    overlayGroups = c("Peta LULC (2025)", "Batas Luar Konsesi", 
                      "Petak Kerja CMI", "Titik Survei"), 
    options       = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  addMeasure(primaryLengthUnit   = "meters", 
             primaryAreaUnit     = "hectares")
peta_spasial

# 5. EXPORT ----
# Pembuatan folder otomatis
sapply(c("Output/01 - Tabel", "Output/02 - Grafik"), 
       function(x) if(!dir.exists(x)) dir.create(x, recursive = TRUE))

# Export Tables
write_xlsx(table_landcover, "Output/01 - Tabel/Ringkasan_Landcover_2026.xlsx")
write_xlsx(table_vegetation, "Output/01 - Tabel/Checklist_Vegetasi_2026.xlsx")
write_xlsx(table_status_lahan, "Output/01 - Tabel/Status_Lahan_2026.xlsx")

# Export Plots
ggsave("Output/02 - Grafik/Landcover_Distribusi.png", 
       plot_landcover, width = 14, height = 8, dpi = 300)
ggsave("Output/02 - Grafik/Top_10_Vegetasi.png", 
       plot_vegetation, width = 14, height = 8, dpi = 300)
ggsave("Output/02 - Grafik/Status_Pengelolaan_Lahan.png", 
       plot_status_lahan, width = 14, height = 8, dpi = 300)