# Kit de herramientas para proveedores de datos

Sitio del Plan Regional de Monitoreo de Especies Prioritarias del PET (MigraMar). Reúne el Manual para proveedores de datos, la Referencia metodológica y las herramientas descargables.

**Sitio publicado:** https://datamanagement-mgrmr.github.io/pet-proveedores-datos/

Este repositorio es público y no contiene datos de monitoreo: solo documentos y herramientas.

## Qué hay en cada carpeta

| Archivo o carpeta | Qué es | ¿Lo edito? |
|----|----|----|
| `index.qmd` | Bienvenida | Sí |
| `manual.qmd` | Manual para proveedores de datos (versión maestra) | Sí |
| `referencia.qmd` | Herramienta 1 · Referencia metodológica (versión maestra) | Sí |
| `herramientas.qmd` | Página de descargas | Sí |
| `_quarto.yml` | Configuración del libro: capítulos, logo, tema | Pocas veces |
| `styles.css` | Ajustes de estilo | Pocas veces |
| `images/` | Logo y figuras del Manual | Sí, al cambiar una figura |
| `descargas/` | Archivos que se descargan desde el sitio (Excel, app.R, código, presentación, Word) | Sí, reemplazando el archivo con el mismo nombre |
| `word/` | Genera el Word del Manual y de la Referencia a partir de los `.qmd` | No |
| `app/` | Copia de la aplicación para la versión en línea | No (la llena el script) |
| `app-web/` | La aplicación en línea (Shinylive) | No (la genera el script) |
| `scripts/actualizar_app.R` | Actualiza la aplicación en línea | Lo ejecuta cuando cambie la app |
| `docs/` | El sitio ya generado, que es lo que publica GitHub | No (lo genera `quarto render`) |

## Cómo actualizar el sitio

Abra `pet-proveedores-datos.Rproj` en RStudio y siga solo los pasos que apliquen:

1.  **Cambió un texto** del Manual, la Referencia u otra página: edite el `.qmd`. Para ver cómo queda mientras edita, escriba en la pestaña *Terminal* de RStudio: `quarto preview`.
2.  **Cambió el Manual o la Referencia:** regenere sus versiones en Word con `quarto render word`.
3.  **Cambió una herramienta** (Excel, app.R, catálogo, presentación): reemplace el archivo en `descargas/` con el mismo nombre.
4.  **Cambió app.R o el catálogo:** ejecute `scripts/actualizar_app.R` en RStudio (botón *Source*).
5.  **Siempre al final:** genere el sitio con `quarto render`.
6.  **Publique:** en GitHub Desktop escriba un resumen del cambio, haga clic en *Commit to main* y luego en *Push origin*. El sitio se actualiza en 1–2 minutos.

## Primera vez

1.  Instale Quarto desde https://quarto.org (Get Started).
2.  (Ya hecho) El usuario de GitHub del sitio es `datamanagement-mgrmr`.
3.  Ejecute `scripts/actualizar_app.R` para crear la aplicación en línea, y luego `quarto render word` y `quarto render`.
4.  Haga *Commit* y *Push* en GitHub Desktop.
5.  En github.com, abra el repositorio → *Settings* → *Pages* → *Deploy from a branch* → rama `main`, carpeta `/docs` → *Save*.
