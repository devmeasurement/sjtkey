# =============================================================================
# sjtkey (nama sementara): penskoran, evaluasi kunci, dan visualisasi SJT
# Script tunggal. Jalankan dari atas ke bawah (tombol "Source" di RStudio).
# Hanya memakai R dasar (tanpa library tambahan), kecuali bagian NRM opsional.
#
# STATUS: belum diuji di mesin saya (R tidak tersedia). Jika ada error, salin
# pesan errornya apa adanya.
# =============================================================================

# ---- 0. Pengaturan ----------------------------------------------------------
data_dir      <- NULL     # opsional: isi alamat folder data, mis. "~/Downloads"
PILIH_MANUAL  <- TRUE     # TRUE: jika file tidak ketemu, muncul jendela pilih file
RUN_NRM       <- FALSE    # TRUE untuk skor NRM (butuh paket mirt)
out_dir       <- "hasil_sjt"

# ---- 1. Fungsi analisis -----------------------------------------------------

build_key <- function(ratings, method = c("mean", "rank")) {
  method <- match.arg(method)
  stopifnot(length(dim(ratings)) == 3)
  m <- apply(ratings, c(2, 3), mean, na.rm = TRUE)          # butir x opsi
  if (method == "rank") m <- t(apply(m, 1, rank, ties.method = "average"))
  dimnames(m) <- list(paste0("item", seq_len(nrow(m))),
                      paste0("opt", seq_len(ncol(m))))
  m
}

expert_agreement <- function(ratings) {
  stopifnot(length(dim(ratings)) == 3, dim(ratings)[1] >= 2)
  m <- dim(ratings)[1]; n <- dim(ratings)[3]
  vapply(seq_len(dim(ratings)[2]), function(j) {
    R  <- t(apply(ratings[, j, ], 1, rank))                 # pakar x opsi
    Rj <- colSums(R)
    S  <- sum((Rj - mean(Rj))^2)
    12 * S / (m^2 * (n^3 - n))
  }, numeric(1))
}

score_sjt <- function(resp, key,
                      method = c("expert", "best", "consensus", "standardized")) {
  method <- match.arg(method)
  resp <- as.matrix(resp); key <- as.matrix(key)
  stopifnot(ncol(resp) == nrow(key),
            all(resp %in% c(seq_len(ncol(key)), NA)))
  k <- ncol(resp)
  W <- switch(method,
    expert       = key,
    best         = (key == apply(key, 1, max)) * 1,
    consensus    = t(sapply(seq_len(k), function(j) {
                      tab <- tabulate(resp[!is.na(resp[, j]), j], nbins = ncol(key))
                      tab / sum(tab)
                    })),
    standardized = t(apply(key, 1, function(w) {
                      s <- sd(w)
                      if (is.na(s) || s == 0) rep(0, length(w)) else (w - mean(w)) / s
                    }))
  )
  S <- matrix(NA_real_, nrow(resp), k, dimnames = list(NULL, colnames(resp)))
  for (j in seq_len(k)) S[, j] <- W[j, resp[, j]]
  list(item_scores = S, total = rowSums(S), weights = W, method = method)
}

cronbach_alpha <- function(S) {
  S <- as.matrix(S); S <- S[stats::complete.cases(S), , drop = FALSE]
  k <- ncol(S)
  k / (k - 1) * (1 - sum(apply(S, 2, var)) / var(rowSums(S)))
}

option_analysis <- function(resp, key, method = "expert") {
  resp <- as.matrix(resp); key <- as.matrix(key)
  sc <- score_sjt(resp, key, method)
  out <- vector("list", ncol(resp) * ncol(key)); idx <- 0
  for (j in seq_len(ncol(resp))) {
    rest <- sc$total - sc$item_scores[, j]
    for (o in seq_len(ncol(key))) {
      idx <- idx + 1
      pick <- resp[, j] == o
      out[[idx]] <- data.frame(
        item = colnames(resp)[j], option = o, weight = key[j, o],
        prop = mean(pick, na.rm = TRUE),
        mean_rest = if (sum(pick, na.rm = TRUE) > 1) mean(rest[pick], na.rm = TRUE) else NA_real_,
        r_pb = suppressWarnings(cor(as.numeric(pick), rest, use = "complete.obs"))
      )
    }
  }
  do.call(rbind, out)
}

key_check <- function(oa, rho_min = 0.5, prop_min = 0.05) {
  parts <- split(oa, factor(oa$item, levels = unique(oa$item)))
  res <- lapply(parts, function(d) {
    ok  <- !is.na(d$mean_rest)
    rho <- if (sum(ok) >= 3) suppressWarnings(
      cor(d$weight[ok], d$mean_rest[ok], method = "spearman")) else NA_real_
    data.frame(item = d$item[1], rho = round(rho, 2),
               min_prop = round(min(d$prop), 2),
               periksa = is.na(rho) || rho < rho_min || min(d$prop) < prop_min,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, unname(res))
  rownames(out) <- NULL
  out
}

compare_keys <- function(resp, key, criterion = NULL,
                         methods = c("expert", "best", "consensus", "standardized")) {
  sc  <- lapply(methods, function(m) score_sjt(resp, key, m)); names(sc) <- methods
  tot <- sapply(sc, function(s) s$total)
  out <- data.frame(metode = methods,
                    alpha  = round(unname(sapply(sc, function(s) cronbach_alpha(s$item_scores))), 3),
                    stringsAsFactors = FALSE)
  if (!is.null(criterion)) {
    out$r_kriteria <- round(unname(vapply(methods, function(m)
      cor(tot[, m], criterion, use = "pairwise.complete.obs"), numeric(1))), 3)
  }
  list(ringkasan = out,
       korelasi_skor = round(cor(tot, use = "pairwise.complete.obs"), 3),
       skor = tot)
}

# ---- 2. Fungsi pemuatan data (tahan banting) --------------------------------

dir_script <- tryCatch({
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- rstudioapi::getSourceEditorContext()$path
    if (nzchar(p)) dirname(p) else NA_character_
  } else NA_character_
}, error = function(e) NA_character_)

daftar_folder <- function() {
  dasar <- c(data_dir, getwd(), dir_script, "~/Downloads", "~/Desktop",
             "~/Documents", file.path("~", "Documents", "PENGEMBANGAN PAKET"))
  dasar <- unique(path.expand(dasar[!is.na(dasar) & nzchar(dasar)]))
  semua <- c(dasar, file.path(dasar, "data_bangkitan"),
             file.path(dasar, "data_bangkitan", "data_bangkitan"))
  semua[dir.exists(semua)]
}

# Cari "nama.csv" atau "nama (1).csv" (file hasil unduh ganda)
cari_file <- function(nama, folders) {
  pola <- paste0("^", nama, "( ?\\([0-9]+\\))?\\.csv$")
  for (f in folders) {
    hit <- list.files(f, pattern = pola, full.names = TRUE, ignore.case = TRUE)
    if (length(hit)) return(sort(hit)[1])
  }
  NA_character_
}

# Baca CSV dengan pemisah koma ATAU titik-koma, tahan BOM
baca_csv <- function(f) {
  l1  <- readLines(f, n = 1, warn = FALSE)
  sep <- if (grepl(";", l1, fixed = TRUE) && !grepl(",", l1, fixed = TRUE)) ";" else ","
  read.csv(f, sep = sep, stringsAsFactors = FALSE, check.names = FALSE,
           fileEncoding = "UTF-8-BOM")
}

# Data simulasi cadangan
sim_sjt <- function(n = 300, k = 12, seed = 1) {
  set.seed(seed)
  theta <- rnorm(n)
  key <- t(replicate(k, sample(4:1)))
  dimnames(key) <- list(paste0("item", 1:k), paste0("opt", 1:4))
  resp <- matrix(NA_integer_, n, k, dimnames = list(NULL, rownames(key)))
  for (j in 1:k) {
    w <- key[j, ]
    for (i in 1:n) {
      lg <- 0.8 * theta[i] * (w - 2.5)
      p  <- exp(lg) / sum(exp(lg))
      resp[i, j] <- sample.int(4, 1, prob = p)
    }
  }
  resp <- as.data.frame(resp)
  resp$skor_kriteria <- round(55 + 12 * (0.55 * theta + sqrt(1 - 0.55^2) * rnorm(n)))
  list(resp = resp, key = key)
}

# ---- 3. Muat data -----------------------------------------------------------
folders <- daftar_folder()
f_resp  <- cari_file("respons_sjt", folders)

if (is.na(f_resp) && PILIH_MANUAL && interactive()) {
  cat("\nFile data belum ditemukan otomatis.\n",
      "Pilih file 'respons_sjt.csv' pada jendela yang muncul...\n")
  f_resp <- tryCatch(file.choose(), error = function(e) NA_character_)
}

f_key <- f_rate <- NA_character_
if (!is.na(f_resp)) {
  folders2 <- c(dirname(f_resp), folders)
  f_key  <- cari_file("kunci_pakar", folders2)
  f_rate <- cari_file("rating_pakar_long", folders2)
}

if (!is.na(f_resp) && !is.na(f_key)) {
  cat("\nMemuat respons :", f_resp, "\nMemuat kunci   :", f_key, "\n")
  resp <- baca_csv(f_resp)
  kk   <- baca_csv(f_key)
  key  <- as.matrix(kk[, -1, drop = FALSE])
  rownames(key) <- as.character(kk[[1]])
  storage.mode(key) <- "numeric"
  items <- intersect(rownames(key), names(resp))
  if (length(items) == 0)
    items <- grep("^item[0-9]+$", names(resp), ignore.case = TRUE, value = TRUE)
  if (length(items) == 0)
    stop("Kolom butir tidak ditemukan. Nama kolom di respons_sjt.csv: ",
         paste(names(resp), collapse = ", "))
  key <- key[items, , drop = FALSE]
  resp[items] <- lapply(resp[items], as.integer)
  sumber <- "data bangkitan/CSV"
} else {
  cat("\n*** File CSV tidak ditemukan. Memakai data simulasi cadangan dari R. ***\n",
      "Lokasi kerja R sekarang:", getwd(), "\n",
      "Folder yang diperiksa:\n ", paste(folders, collapse = "\n  "), "\n")
  d <- sim_sjt(); resp <- d$resp; key <- d$key
  items <- rownames(key); f_rate <- NA_character_
  sumber <- "simulasi cadangan"
}

stopifnot(all(unlist(resp[items]) %in% c(seq_len(ncol(key)), NA)))
cat("\nSumber data:", sumber, "| Responden:", nrow(resp),
    "| Butir:", length(items), "| Opsi:", ncol(key), "\n")
print(head(resp[, c(items[1:min(4, length(items))])], 3))

# ---- 4. Pemeriksaan dasar ---------------------------------------------------
manual1 <- sum(key[cbind(seq_along(items), as.integer(unlist(resp[1, items])))])
fungsi1 <- score_sjt(resp[, items], key, "expert")$total[1]
cat("\nCek skor responden 1: manual =", manual1, "| fungsi =", fungsi1,
    ifelse(isTRUE(all.equal(manual1, fungsi1)), "(SAMA)", "(BEDA! periksa kode)"), "\n")

# ---- 5. Analisis ------------------------------------------------------------
kriteria <- if ("skor_kriteria" %in% names(resp)) resp$skor_kriteria else NULL
hasil <- compare_keys(resp[, items], key, criterion = kriteria)
cat("\n== Ringkasan metode skor ==\n");  print(hasil$ringkasan)
cat("\n== Korelasi antarmetode ==\n");   print(hasil$korelasi_skor)

oa  <- option_analysis(resp[, items], key)
kc5 <- key_check(oa)
kc9 <- key_check(oa, rho_min = 0.9)
cat("\n== Pemeriksaan kunci (default, rho_min = 0.5) ==\n"); print(kc5)
cat("\n== Pemeriksaan kunci (ketat, rho_min = 0.9) ==\n");   print(kc9)

ratings <- NULL; w_pakar <- NULL
if (!is.na(f_rate)) {
  rt <- baca_csv(f_rate)
  n_pakar <- length(unique(rt$pakar)); n_item <- length(unique(rt$item))
  n_opsi  <- length(unique(rt$opsi))
  # Baris file berurutan: pakar (terluar) > item > opsi (terdalam).
  ratings <- aperm(array(rt$rating, dim = c(n_opsi, n_item, n_pakar)), c(3, 2, 1))
  w_pakar <- expert_agreement(ratings)
  key_dr  <- build_key(ratings, "rank")
  cat("\n== Kesepakatan pakar (Kendall's W per butir) ==\n"); print(round(w_pakar, 2))
  cat("Kesamaan kunci dari rating vs kunci_pakar.csv:",
      round(mean(key_dr == key) * 100, 1),
      "% sel sama (selisih wajar bila ada skor kembar)\n")
}

# ---- 6. Fungsi visualisasi (R dasar) ----------------------------------------
pal_rank <- c("#2c7bb6", "#66c2a5", "#fdae61", "#d7191c")  # urutan 1 (terbaik) ... 4

plot_metode <- function(rs) {
  M <- rbind("Alpha" = rs$alpha)
  if ("r_kriteria" %in% names(rs)) M <- rbind(M, "Korelasi kriteria" = rs$r_kriteria)
  colnames(M) <- rs$metode
  barplot(M, beside = TRUE, col = c("#2c7bb6", "#fdae61")[seq_len(nrow(M))],
          ylim = c(min(0, M, na.rm = TRUE), max(1, M, na.rm = TRUE)),
          ylab = "Nilai", main = "Perbandingan metode skor",
          legend.text = rownames(M), args.legend = list(x = "bottomright", bty = "n"))
  abline(h = 0)
}

plot_cor_heat <- function(cm) {
  k <- ncol(cm)
  op <- par(mar = c(6, 7, 3, 2)); on.exit(par(op))
  image(1:k, 1:k, t(cm[k:1, , drop = FALSE]),
        col = colorRampPalette(c("#d7191c", "white", "#2c7bb6"))(100),
        zlim = c(-1, 1), axes = FALSE, xlab = "", ylab = "",
        main = "Korelasi antar metode skor")
  axis(1, at = 1:k, labels = colnames(cm), las = 2)
  axis(2, at = 1:k, labels = rev(rownames(cm)), las = 2)
  for (i in 1:k) for (j in 1:k) text(j, k - i + 1, sprintf("%.2f", cm[i, j]))
  box()
}

plot_total <- function(total) {
  hist(total, breaks = 20, col = "#abd9e9", border = "white", freq = FALSE,
       main = "Distribusi skor total (bobot pakar)", xlab = "Skor total")
  lines(density(total), lwd = 2, col = "#2c7bb6")
}

plot_kriteria <- function(total, kriteria) {
  r <- cor(total, kriteria, use = "pairwise.complete.obs")
  plot(total, kriteria, pch = 19, col = "#2c7bb680",
       xlab = "Skor SJT (bobot pakar)", ylab = "Skor kriteria eksternal",
       main = paste0("Validitas kriteria (r = ", sprintf("%.2f", r), ")"))
  abline(lm(kriteria ~ total), col = "#d7191c", lwd = 2)
}

plot_option_props <- function(oa) {
  its <- unique(oa$item)
  M <- sapply(its, function(it) {
    d <- oa[oa$item == it, ]
    d[order(-d$weight), "prop"]            # proporsi: opsi terbaik -> terburuk
  })
  colnames(M) <- its
  op <- par(mar = c(5, 4, 3, 9), xpd = TRUE); on.exit(par(op))
  barplot(M, col = pal_rank[seq_len(nrow(M))], las = 2, ylim = c(0, 1),
          ylab = "Proporsi pemilih", main = "Pilihan responden per butir",
          border = "white")
  legend("topright", inset = c(-0.2, 0), bty = "n", cex = 0.8,
         legend = c("Opsi terbaik (pakar)", "Opsi ke-2", "Opsi ke-3", "Opsi terburuk")[seq_len(nrow(M))],
         fill = pal_rank[seq_len(nrow(M))])
}

plot_key_check <- function(kc, rho_min = 0.5) {
  op <- par(mar = c(6, 4, 3, 1)); on.exit(par(op))
  cols <- ifelse(kc$periksa, "#d7191c", "#2c7bb6")
  plot(seq_len(nrow(kc)), kc$rho, ylim = c(-1, 1), pch = 19, cex = 1.3, col = cols,
       xaxt = "n", xlab = "", ylab = "Spearman rho (bobot pakar vs skor sisa)",
       main = "Pemeriksaan kunci per butir")
  axis(1, at = seq_len(nrow(kc)), labels = kc$item, las = 2, cex.axis = 0.8)
  abline(h = rho_min, lty = 2); abline(h = 0, col = "grey70")
  legend("bottomright", bty = "n", pch = 19, col = c("#2c7bb6", "#d7191c"),
         legend = c("Wajar", "Perlu diperiksa"))
}

plot_option_curves <- function(resp, key, items_plot, n_grup = 4) {
  resp <- as.matrix(resp); key <- as.matrix(key)
  sc <- score_sjt(resp, key, "expert")
  n_p <- length(items_plot)
  op <- par(mfrow = c(ceiling(n_p / 3), min(3, n_p)), mar = c(4, 4, 3, 1))
  on.exit(par(op))
  for (it in items_plot) {
    j    <- match(it, colnames(resp))
    rest <- sc$total - sc$item_scores[, j]
    br   <- unique(quantile(rest, probs = seq(0, 1, length.out = n_grup + 1), na.rm = TRUE))
    if (length(br) < 3) next
    grp  <- cut(rest, breaks = br, include.lowest = TRUE, labels = FALSE)
    ng   <- max(grp, na.rm = TRUE)
    P    <- sapply(seq_len(ncol(key)), function(o) tapply(resp[, j] == o, grp, mean))
    P    <- matrix(P, nrow = ng)
    cols <- pal_rank[rank(-key[j, ], ties.method = "first")]
    matplot(seq_len(ng), P, type = "b", lty = 1, pch = 16, col = cols,
            ylim = c(0, 1), xaxt = "n",
            xlab = "Kelompok skor sisa (rendah -> tinggi)",
            ylab = "Proporsi memilih", main = it)
    axis(1, at = seq_len(ng))
    legend("top", bty = "n", cex = 0.6, ncol = 2, lty = 1, pch = 16, col = cols,
           legend = paste0("opsi", seq_len(ncol(key)), " (b=", key[j, ], ")"))
  }
}

plot_w_pakar <- function(w, its) {
  op <- par(mar = c(6, 4, 3, 1)); on.exit(par(op))
  barplot(w, names.arg = its, las = 2, ylim = c(0, 1), col = "#66c2a5", border = "white",
          ylab = "Kendall's W", main = "Kesepakatan pakar per butir")
  abline(h = 0.5, lty = 2)
}

buat_grafik <- function() {
  plot_metode(hasil$ringkasan)
  plot_cor_heat(hasil$korelasi_skor)
  plot_total(hasil$skor[, "expert"])
  if (!is.null(kriteria)) plot_kriteria(hasil$skor[, "expert"], kriteria)
  plot_option_props(oa)
  plot_key_check(kc9, rho_min = 0.9)
  items_plot <- head(kc9$item[kc9$periksa], 6)
  if (length(items_plot) == 0) items_plot <- head(items, 6)
  plot_option_curves(resp[, items], key, items_plot)
  if (!is.null(w_pakar)) plot_w_pakar(w_pakar, items)
}

# ---- 7. Tampilkan dan simpan grafik -----------------------------------------
if (!dir.exists(out_dir)) dir.create(out_dir)

cat("\nMenggambar grafik... (di RStudio gunakan panah di panel Plots untuk berpindah)\n")
buat_grafik()                                  # tampil di layar

pdf(file.path(out_dir, "grafik_sjt.pdf"), width = 10, height = 7)
tryCatch(buat_grafik(), finally = dev.off())   # simpan semua halaman ke PDF

# ---- 8. Skor NRM (opsional) -------------------------------------------------
if (RUN_NRM) {
  if (!requireNamespace("mirt", quietly = TRUE)) install.packages("mirt")
  resp0 <- as.data.frame(lapply(resp[, items], function(x) x - 1L))   # kode 0..3
  mod   <- mirt::mirt(resp0, 1, itemtype = "nominal", verbose = FALSE)
  fs    <- mirt::fscores(mod, method = "EAP", full.scores.SE = TRUE)
  cat("\nReliabilitas empiris NRM:", round(mirt::empirical_rxx(fs), 3), "\n")
  cat("Korelasi skor NRM vs skor bobot pakar:",
      round(cor(fs[, 1], hasil$skor[, "expert"]), 3), "(negatif = arah terbalik)\n")
}

# ---- 9. Simpan tabel --------------------------------------------------------
write.csv(hasil$ringkasan, file.path(out_dir, "ringkasan_metode.csv"), row.names = FALSE)
write.csv(oa,   file.path(out_dir, "analisis_opsi.csv"),     row.names = FALSE)
write.csv(kc9,  file.path(out_dir, "pemeriksaan_kunci.csv"), row.names = FALSE)
cat("\nSelesai. Tabel dan grafik (grafik_sjt.pdf) tersimpan di:", normalizePath(out_dir), "\n")
