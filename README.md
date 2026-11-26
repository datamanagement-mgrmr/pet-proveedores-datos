# Kit de herramientas para proveedores de datos

Sitio del Plan Regional de Monitoreo de Especies Prioritarias del PET (MigraMar). Reúne el Manual para proveedores de datos, la Referencia metodológica y las herramientas descargables.

**Sitio publicado:** https://datamanagement-mgrmr.github.io/pet-proveedores-datos/

Este repositorio es público y no contiene datos de monitoreo: solo documentos y herramientas.

## Regla de oro

Este repositorio es la **única fuente oficial** del Kit. Se edita aquí; las carpetas internas (OneDrive) solo guardan copias de versiones anteriores.

Hay tres tipos de archivos:

- **Maestros:** los que el equipo edita. Todo lo demás sale de ellos.
- **Generados:** los crea Quarto o un script a partir de los maestros. No se editan a mano: se sobrescriben.
- **Configuración:** se tocan pocas veces.

## Qué hay en cada carpeta

| Carpeta o archivo | Qué es | Tipo |
|----|----|----|
| `index.qmd`, `manual.qmd`, `referencia.qmd`, `herramientas.qmd` | Textos de las páginas del sitio (Bienvenida, Manual, Herramienta 1, Descargas) | Maestro |
| `descargas/` | Herramientas que descarga el proveedor. Casi todas son maestras (H2, H4, `app.R`, CSV de ejemplo, presentación); tres son generadas (ver abajo) | Maestro + generado |
| `images/` | Logos y figuras del Manual | Maestro |
| `codigo-ejemplo/` | Código de R de ejemplo (`.qmd`); genera su HTML en `descargas` | Maestro |
| `word/` | Genera los Word del Manual y de la Referencia en `descargas` | Configuración |
| `mantenimiento/actualizar_app.R` | Ayudante interno: prepara la aplicación en línea | Configuración |
| `_quarto.yml`, `styles.css` | Menú, logo y estilo del sitio | Configuración |
| `app/`, `app-web/` | Copia de la app y versión en línea (las llena `actualizar_app.R`) | Generado |
| `docs/` | El sitio ya generado, lo que publica GitHub (lo crea `quarto render`) | Generado |

En `descargas/` **no se editan a mano** estos tres, porque se generan:

- `Manual_proveedores_datos.docx` y `H1_referencia-metodologica.docx` → salen de `manual.qmd` y `referencia.qmd` con `quarto render word`.
- `codigo-R_calculo-indicadores-sinGAMM.html` → sale de `codigo-ejemplo/` con `quarto render codigo-ejemplo`.

## Cómo actualizar el sitio

Abra `pet-proveedores-datos.Rproj` en RStudio y siga solo los pasos que apliquen:

1.  **Cambió un texto** del Manual, la Referencia u otra página: edite el `.qmd`. Para ver cómo queda mientras edita, escriba en la pestaña *Terminal* de RStudio: `quarto preview`.
2.  **Cambió el Manual o la Referencia:** regenere sus versiones en Word con `quarto render word`.
3.  **Cambió una herramienta** (Excel, app.R, catálogo, presentación): reemplace el archivo en `descargas/` con el mismo nombre.
4.  **Cambió app.R o el catálogo:** ejecute `mantenimiento/actualizar_app.R` en RStudio (botón *Source*).
5.  **Cambió el código de R de ejemplo:** edite `codigo-ejemplo/codigo-R_calculo-indicadores-sinGAMM.qmd` y regenere su HTML con `quarto render codigo-ejemplo`.
6.  **Siempre al final:** genere el sitio con `quarto render`.
7.  **Publique:** en GitHub Desktop escriba un resumen del cambio, haga clic en *Commit to main* y luego en *Push origin*. El sitio se actualiza en 1–2 minutos.

La guía interna (Word) explica cada paso con detalle, qué revisar para que todo quede alineado y cómo llevar los pendientes con *Issues*.

## Primera vez

1.  Instale Quarto desde https://quarto.org (Get Started).
2.  (Ya hecho) El usuario de GitHub del sitio es `datamanagement-mgrmr`.
3.  Ejecute `mantenimiento/actualizar_app.R` para crear la aplicación en línea, y luego `quarto render word` y `quarto render`.
4.  Haga *Commit* y *Push* en GitHub Desktop.
5.  En github.com, abra el repositorio → *Settings* → *Pages* → *Deploy from a branch* → rama `main`, carpeta `/docs` → *Save*.
