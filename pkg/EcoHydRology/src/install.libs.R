## Modified from Rserve
libarch <- if (nzchar(R_ARCH)) paste("libs", R_ARCH, sep='') else "libs"
dest <- file.path(R_PACKAGE_DIR, libarch)

files <- if (WINDOWS) "rswat2005.exe" else "rswat2005"
if (length(files)) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  file.copy(files, dest, overwrite = TRUE)
}

