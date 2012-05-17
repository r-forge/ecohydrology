files <- Sys.glob("*.exe")
libarch <- if (nzchar(R_ARCH)) paste('libs', R_ARCH, sep='') else 'libs'
dest <- file.path(R_PACKAGE_DIR, libarch)
dir.create(dest, recursive = TRUE, showWarnings = FALSE)
file.copy(files, dest, overwrite = TRUE)
