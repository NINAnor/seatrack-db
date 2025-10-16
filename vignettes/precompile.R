src_dir <- file.path("vignettes", "src")
main_dir <- "vignettes"
all_vignettes <- list.files(src_dir, ".orig")
for (vignette in all_vignettes) {
    print(vignette)
    knitr::knit(file.path(src_dir, vignette), output = file.path(main_dir, gsub(".orig", "", vignette)))
}
