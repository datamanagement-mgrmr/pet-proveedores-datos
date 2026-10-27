# =============================================================================
# Actualizar la aplicación en línea (Shinylive)
#
# Ejecútelo desde RStudio, con el proyecto pet-proveedores-datos abierto,
# cada vez que cambie app.R o el Catálogo de vocabulario en la carpeta
# "descargas". Después corra `quarto render` y suba los cambios con
# GitHub Desktop.
# =============================================================================

if (!requireNamespace("shinylive", quietly = TRUE)) install.packages("shinylive")

# 1. Copiar la versión vigente de la aplicación y del catálogo a la carpeta app/
file.copy("descargas/app.R", "app/app.R", overwrite = TRUE)
file.copy("descargas/H4_catalogo-vocabulario.xlsx", "app/H4_catalogo-vocabulario.xlsx", overwrite = TRUE)

# 2. Convertir la aplicación en una página web que corre en el navegador
unlink("app-web", recursive = TRUE)
shinylive::export(appdir = "app", destdir = "app-web")

message("Listo. Ahora corra `quarto render` en la Terminal y suba los cambios con GitHub Desktop.")
