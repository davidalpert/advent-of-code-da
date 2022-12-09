namespace AdventOfCode

open System.Net.NetworkInformation

module NoSpaceLeftOnDevice =

    open System
    open System.IO
    open System.Collections.Generic
    open AdventOfCode.Input
    open FParsec
    open FParsec.Pipes
    open FSharp.Data.UnitSystems.SI.UnitNames

    type File = { path: string; size: int }
    type Directory = { path: string }
    
    type FileSystemEntry =
    | File of File // full_path, size
    | Directory of Directory  // full_path
    with
        static member isFile e = match e with | File f -> Some f | Directory _ -> None
        static member isDirectory e = match e with | File _ -> None | Directory d -> Some d
    
    let asFile size path = File({ path = path; size = size })
    let asFileInDir (dir:string) size name = asFile size (Path.Join(dir, name))
    let asDirectory path = Directory({ path = path })
    let asDirectorInDir (dir:string) name = asDirectory (Path.Join(dir, name))
    
    module parser =
        let ws = spaces
        
        // trace helper to assist with debugging misbehaving parsers
        // let (<!>) (p: Parser<_,_>) (label) : Parser<_,_> =
        //   fun stream ->
        //     printfn "%A: Entering %s" stream.Position label
        //     let reply = p stream
        //     printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        //     reply
 
        // https://www.quanttec.com/fparsec/users-guide/parsing-with-user-state.html 
        type UserState = {
            pwd:string
        }
          with
             static member Default = { pwd ="/" }
        
        let cd path =
             match path with
             | "/" -> updateUserState (fun s -> { s with pwd = "/" })
             | ".." -> updateUserState (fun s -> { s with pwd = s.pwd |> Path.GetDirectoryName })
             | _ -> updateUserState (fun s -> { s with pwd = Path.Join(s.pwd, path) })
             // <!> "cd <" + path + ">"
             
        let pCD =
            (%% %"$ cd " ?- +.(restOfLine true) -%> auto
            >>= fun path stream ->
                cd path stream |> ignore
                Reply(Seq.empty))
            // <!> "cd"
        
        // dir a
        let pDirEntry: Parser<_,UserState> =
            (%% %"dir " ?- +.(restOfLine true) -%> auto)
            >>= fun name stream -> Reply(asDirectorInDir stream.UserState.pwd name)
            // <!> "<dir>"
            
        // 14848514 b.txt
        let pFileEntry: Parser<_, UserState> =
            ((%% +.pint32 ?- ws -- +.(restOfLine true) -%> auto)
             >>= fun (size, name) stream ->
                Reply(asFileInDir stream.UserState.pwd size name))
            // <!> "<file>"
        
        let pDirListItem =
            %[pDirEntry; pFileEntry] * qty[0..]
            // <!> "dir entry"
                
        // $ ls
        // dir a
        // 14848514 b.txt
        // 8504156 c.dat
        // dir d
        // $ ...
        let pLS =
            %% %"$ ls" ?- ws -- +.(pDirListItem * (qty.[1..] / '\n'))
            -%> Seq.concat
            // <!> "ls"
            
        let pCommand = %[pCD; pLS]
                
        let pTerminalOutput =
            %% +.(pCommand * qty[1..])
            -|> fun s ->
                let entries =
                    s
                    |> Seq.concat
                    |> Array.ofSeq
                    
                let dirs = entries |> Array.choose FileSystemEntry.isDirectory |> Seq.append [{path = "/"}]
                let files = entries |> Array.choose FileSystemEntry.isFile
                
                (dirs, files)
            
        let mustParse p (input:string) =
            match runParserOnString p UserState.Default "" input with
            | Success(r, _, _) -> r
            | Failure(errorMsg, _, _) -> failwith errorMsg
            
        let parseTerminalOutput (input:string) =
            mustParse pTerminalOutput input
    
    let directoriesWithSizes (input: string) =
        let dirs, files = input.Trim() |> parser.parseTerminalOutput
        
        dirs
        |> Seq.map (fun d ->
                let totalSize = files |> Array.filter (fun f -> f.path.StartsWith(d.path)) |> Array.sumBy (fun f -> f.size)
                (d.path, totalSize)
            )
        
    let directoriesWithSizesOfAtMost n (input:string) =
        input
        |> directoriesWithSizes
        |> Seq.filter (fun (_, size) -> size <= n)
        
    // part 2
    
    let totalSizeOf (fullPath:string) (dirsWithSize:seq<string*int>) =
        dirsWithSize
        |> Seq.find (fun (path,_) -> path = fullPath)
        |> snd
        
    let totalSizeOfSmallestDirectoryToFreeUpEnoughSpaceToRunUpdateOfSize n (input:string) =
        let dirsWithSize = input |> directoriesWithSizes
        
        let totalSpaceOnDrive = 70000000
        let sizeOfOutermostDir = dirsWithSize |> totalSizeOf "/"
        let availableSpace = totalSpaceOnDrive - sizeOfOutermostDir
        let minSpaceToFree = n - availableSpace
        
        dirsWithSize
        |> Seq.map snd
        |> Seq.sort
        |> Seq.find (fun s -> s >= minSpaceToFree)
        
        