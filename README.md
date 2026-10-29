# sjtkey
**Aplikasi online:** https://devmeasurement.shinyapps.io/sjtkey

Penskoran, evaluasi kunci, dan visualisasi **Situational Judgement Test (SJT)** dengan R dasar.

## Fitur
- **Penyusunan kunci** dari rating pakar (`build_key`, metode mean atau rank)
- **Empat metode skor**: `expert`, `best`, `consensus`, `standardized` (`score_sjt`)
- **Reliabilitas**: Cronbach's alpha (`cronbach_alpha`)
- **Analisis opsi**: proporsi pemilih, korelasi point-biserial, skor sisa (`option_analysis`)
- **Pemeriksaan kunci** per butir dengan Spearman rho (`key_check`)
- **Kesepakatan pakar**: Kendall's W (`expert_agreement`)
- **Perbandingan metode** dan validitas kriteria (`compare_keys`)
- **Grafik** otomatis ke layar dan PDF
- **Skor NRM** opsional (butuh paket `mirt`)

## Format data
| File | Isi |
|---|---|
| `respons_sjt.csv` | Kolom `item1..itemK` berisi kode opsi terpilih (1..m); opsional `skor_kriteria` |
| `kunci_pakar.csv` | Kolom pertama nama butir, kolom berikutnya bobot tiap opsi |
| `rating_pakar_long.csv` | Kolom `pakar`, `item`, `opsi`, `rating` (opsional) |

## Cara pakai
Unduh `sjtkey.R`, buka di RStudio, letakkan file CSV di folder yang sama, lalu klik **Source**.
Hasil tersimpan di folder `hasil_sjt/`.

```r
source("sjtkey.R")
```

## Status
Versi awal.
