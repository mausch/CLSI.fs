#r "bin/debug/CLSIClient.dll"

open CLSI

let token = "yourtoken"
let compile = compile "http://clsi.scribtex.com/clsi/compile"
let req = Request.make(token = token, root = "tesis.tex", resources = [
                        R("tesis.bib", fromFile "tesis.bib")
                        R("tesis.tex", fromFile "tesis.tex")
                        R("logo_fiuba_alta.jpg", fromUrl "http://github.com/mausch/Figment/raw/master/logo_fiuba_alta.jpg")
                        ])
compile req