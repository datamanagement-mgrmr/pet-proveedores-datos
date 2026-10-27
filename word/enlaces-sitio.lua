-- Convierte los enlaces internos del libro en enlaces al sitio publicado,
-- para que funcionen también dentro del Word.
local sitio = ""
function Meta(m)
  if m.sitio then sitio = pandoc.utils.stringify(m.sitio) end
end
function Link(el)
  local t = el.target
  if t:match("^https?:") or t:match("^mailto:") or t:match("^#") then return el end
  t = t:gsub("^%.%./", "")
  t = t:gsub("%.qmd", ".html")
  el.target = sitio .. t
  return el
end
return { { Meta = Meta }, { Link = Link } }
