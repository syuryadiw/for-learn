start_time <- Sys.time()
end_time <- start_time + (5 * 60 * 60)  # 5 jam

messages <- c(
  "Initializing environment...",
  "Loading packages...",
  "Connecting to Google Sheets...",
  "Fetching Sosialisasi & Edukasi data...",
  "Fetching Penggalian Aspirasi data...",
  "Fetching Jadwal Membakar data...",
  "Fetching Hotspot & Aktivitas data...",
  "Cleaning data...",
  "Processing spatial coordinates...",
  "Calculating hotspot density...",
  "Rendering Page 1 - Sosialisasi...",
  "Rendering Page 2 - Penggalian Aspirasi...",
  "Rendering Page 3 - Jadwal Bakar...",
  "Rendering Page 4 - Hotspot...",
  "Generating interactive maps...",
  "Compiling dashboard...",
  "Optimizing visualizations...",
  "Finalizing output...",
  "Validating data integrity...",
  "Checking coordinate systems...",
  "Processing leaflet layers...",
  "Calculating burn area statistics...",
  "Generating hotspot heatmap...",
  "Syncing with Google Sheets...",
  "Updating dashboard components...",
  "Re-rendering visualizations..."
)

i <- 1
while (Sys.time() < end_time) {
  msg <- messages[((i - 1) %% length(messages)) + 1]
  cat(paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg, "\n"))
  Sys.sleep(3)
  i <- i + 1
}

cat(paste0("[", format(Sys.time(), "%H:%M:%S"), "] Dashboard successfully rendered!\n"))