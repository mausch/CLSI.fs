module CLSI

open System
open System.IO
open System.Net
open Xml

//type ErrorType = ParseError | InvalidToken | UnknownCompiler | ImpossibleOutputFormat | InvalidPath | NoOutputProduced | Timeout

type Error = {
    etype: string
    message: string
}

type ResponseStatus =
    | Successful of Uri list
    | Failure of Error

type Response = {
    logs: Uri list
    status: ResponseStatus
}

type ResourceContent =
    | Content of string
    | Url of Uri

let fromFile f = 
    ResourceContent.Content(File.ReadAllText f)

let fromUrl u =
    ResourceContent.Url(Uri(u))

type Resource = {
    path: string
    modified: DateTime option
    content: ResourceContent
    }
    with
        static member make(path, content, ?modified) =
            { 
                Resource.path = path
                content = content
                modified = modified
            }

let R = Resource.make

type Options = {
    compiler: string
    outputFormat: string
}

type Request = {
    token: string
    name: string option
    root: string
    resources: Resource seq
    options: Options option
    }
    with
        static member make(token, root, resources, ?name, ?options) =
            {
                token = token
                root = root
                resources = resources
                name = name
                options = options
            }

let serializeRequest (r: Request) =
    let token = Some <| Element.make("token", content = [Content.Value r.token])
    let resourceToXml (res: Resource) =
        let mutable attr = ["path", res.path]
        let mutable content = []
        if res.modified.IsSome then
            attr <- ("modified", res.modified.Value.ToString("o"))::attr
        match res.content with
        | Url u -> attr <- ("url", u.ToString())::attr
        | Content c -> content <- [Content.Value c]
        Element.make("resource", attributes = attr, content = content)
    let resources = r.resources |> Seq.map resourceToXml
    let resourcesElem = Some <| Element.make("resources", attributes = ["root-resource-path", r.root], content = [Content.Children resources])
    let options =
        match r.options with
        | None -> None
        | Some o -> 
            let compiler = Element.make("compiler", content = [Content.Value o.compiler ])
            let outputFormat = Element.make("outputFormat", content = [Content.Value o.outputFormat])
            Some <| Element.make("options", content = [Content.Children [compiler; outputFormat]])
    let compileElements = [token; options; resourcesElem] |> Seq.choose id
    let xml = Element.make("compile", content = [Content.Children compileElements])
    toBin xml

let send (serverUrl: string) (r: Request) =
    let serverUri = Uri(serverUrl)
    let xml = serializeRequest r
    //File.WriteAllBytes("request.xml", xml)
    //Array.zeroCreate 0
    use web = new WebClient()
    web.UploadData(serverUri, xml)

let parseResponse (s: byte[]) =
    use ms = new MemoryStream(s)
    let parseSuccess (xml: XmlElement) =
        let uris = 
            xml.SelectNodes "output/file" 
            |> Seq.cast<XmlNode>
            |> Seq.map (fun e -> Uri(e.Attributes.["url"].InnerText))
            |> Seq.toList
        ResponseStatus.Successful uris
    let parseFailure (xml: XmlElement) =
        ResponseStatus.Failure {
            etype = (xml.SelectSingleNode "error/type").InnerText
            message = (xml.SelectSingleNode "error/message").InnerText
        }
    let xml = XmlDocument()
    xml.Load(ms)
    let root = xml.DocumentElement
    let status =
        match root.["status"].InnerText.ToLowerInvariant() with
        | "success" -> parseSuccess root
        | "failure" -> parseFailure root
        | x -> failwithf "Unknown status in response: %s" x
    let logs =
        root.SelectNodes "logs/file"
        |> Seq.cast<XmlNode>
        |> Seq.map (fun e -> Uri(e.Attributes.["url"].InnerText))
        |> Seq.toList

    {
        Response.status = status
        logs = logs
    }

let download (uris: Uri seq) =
    let downloadOne (uri: Uri) =
        async {
            let filename = uri.Segments.[uri.Segments.Length - 1]
            let request = WebRequest.Create uri
            use! response = request.AsyncGetResponse()
            use responseStream = response.GetResponseStream()
            use file = new FileStream(filename, FileMode.Create)
            responseStream.CopyTo file
            printfn "Downloaded %A" uri
            return filename
        }
    uris |> Seq.map downloadOne |> Async.Parallel |> Async.RunSynchronously

let compile (serverUrl: string) (r: Request) =
    let rawResponse = send serverUrl r
    let response = parseResponse rawResponse
    let frename dest src = File.Move(src, dest src)
    let prependRoot s = Path.GetFileNameWithoutExtension(r.root) + "-" + s
    let renameWithRoot = frename prependRoot
    let renameAllWithRoot = Seq.iter renameWithRoot
    download response.logs |> renameAllWithRoot
    match response.status with
    | Failure e -> printfn "Error %s: %s" e.etype e.message
    | Successful docs -> download docs |> renameAllWithRoot