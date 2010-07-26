module Xml

open System
open System.IO
open System.Text
open System.Xml

type Content =
    | Value of string
    | Children of Element seq

and Element = {
    tag: string
    attributes: (string * string) seq
    content: Content seq
}
    with
        static member make(tag, ?attributes, ?content) =
            {tag = tag; attributes = defaultArg attributes []; content = defaultArg content []}

let rec writeTo (xw: XmlWriter) (e: Element) =
    let writeAttr a = xw.WriteAttributeString(fst a, snd a)
    let writeContent content = 
        match content with
        | Value x -> xw.WriteString x
        | Children children -> children |> Seq.iter (writeTo xw)
    xw.WriteStartElement e.tag
    e.attributes |> Seq.iter writeAttr
    e.content |> Seq.iter writeContent
    xw.WriteEndElement()
    ()

let toBin (e: Element) =
    let utf8 = UTF8Encoding(false)
    use ms = new MemoryStream()
    use xw = new XmlTextWriter(ms, utf8)
    xw.WriteStartDocument()
    e |> writeTo xw
    xw.WriteEndDocument()
    xw.Flush()
    ms.Flush()
    ms.ToArray()