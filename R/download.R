# Download freMTPL2freq (OpenML 41214) and freMTPL2sev (OpenML 41215) into data/raw as .rds.
files <- c(
  freq = "https://openml.org/data/v1/download/20649148/freMTPL2freq.arff",
  sev  = "https://openml.org/data/v1/download/20649149/freMTPL2sev.arff"
)
md5 <- c(freq = "f8875568bf0ca622929105197e2db613", sev = "24cc74449e3931cb1aad0d43a12e7a6e")
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
options(timeout = 600)
for (k in names(files)) {
  rds <- file.path("data/raw", paste0(k, ".rds"))
  if (file.exists(rds)) next
  arff <- file.path("data/raw", basename(files[[k]]))
  if (!file.exists(arff)) download.file(files[[k]], arff, mode = "wb")
  if (tools::md5sum(arff) != md5[[k]]) message("note: md5 of ", arff, " differs from OpenML metadata")
  saveRDS(foreign::read.arff(arff), rds)
  message("saved ", rds)
}
