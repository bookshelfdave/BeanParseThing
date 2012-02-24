#light
#r "FParsec.dll";;
#r "FParsecCS.dll";;
open FParsec

(* Just playing with FParsec *)


type UserState = unit 
type Parser<'t> = Parser<'t, UserState>

type BeanProp = { name:string; typename:string}

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let str s : Parser<_> = pstring s
let ws:Parser<_> = spaces
let str_ws s = pstring s .>> ws

let wp p = spaces >>. p .>> spaces


let bean = ws >>. str_ws "bean"
let theend = ws >>. (str_ws "end") 

let betweenParens p = str_ws "(" >>. (ws >>. p .>> ws) .>> str_ws ")"
let typedef = 
        let iops = IdentifierOptions()
        (identifier iops) |> betweenParens

let beanid : Parser<_>= 
        let idops = IdentifierOptions(isAsciiIdStart = isUpper)
        (identifier idops) .>> ws

let prop = pipe2  beanid typedef (fun id t -> {name=id; typename=t})
let props = many prop

let pkg = 
    let isAsciiIdStart    = fun c -> isAsciiLetter c 
    let isASciiIdContinue = fun c -> isAsciiLetter c || isDigit c || c = '_' || c = '.'
    let iops = IdentifierOptions( isAsciiIdStart = isAsciiIdStart,isAsciiIdContinue = isASciiIdContinue)    
    str_ws "package" >>. (identifier iops)
  
    

let beandef:Parser<_> = pipe4 bean beanid props theend (fun _ b p _ -> (b, p))

let beandefs = many beandef 

let pkgdef = pipe3 pkg beandefs eof (fun p b _ -> (p,b))



let genVar (varout:BeanProp) = 
        let vname = varout.name.ToLower() 
        printfn "\t\tprivate %s %s;" varout.typename vname
let genVars varsout = List.map genVar varsout

let genProp (propout:BeanProp) = 
            let vname = propout.name.ToLower() 
            printfn "\t\tpublic %s get%s() { return %s; }" propout.typename propout.name vname
            printfn "\t\tpublic %s set%s(%s val) } { return %s }" propout.typename propout.name vname

let genProps propsout = List.map genProp propsout
       
let genCon proplist beanName =    
    let ps = Array.map (fun (p:BeanProp) -> p.typename + " " + p.name.ToLower()) proplist  
    let cs = String.concat "," ps
    printfn "\t\tpublic %s(%s) {" beanName cs
    let sets = Array.map (fun (p:BeanProp) -> 
                            let pname = p.name.ToLower()
                            sprintf "\t\t\tthis.%s = %s;" pname pname) proplist 
    let pss = (String.concat "\n" sets)
    printfn "%s" pss
    printfn "\t\t}"

let genCons (proplist:BeanProp list) beanName = 
    let aproplist = List.toArray proplist
    for i in 0 .. aproplist.Length - 1 do
        (genCon aproplist.[0..i] beanName) |> ignore

let genBean (pkg, beanName, allprops) =
        printfn "package %s;" pkg
        printfn "public class %s {" beanName   
        (genVars allprops) |> ignore
        (printfn "\t\tpublic %s() {}" beanName) |> ignore                         
        (genCons allprops beanName) |> ignore   
        (genProps allprops) |> ignore
        printfn "}"

let genBeans (bd:string * (string * BeanProp list) list) =    
    match bd with
        | (pkg, beanList) ->
             for bean in beanList do
                match bean with 
                    | (bname, ps) -> genBean(pkg,bname,ps)
    
let parseBean str =
    match run pkgdef str with
        | Success(result, _, _)   -> (genBeans result) |> ignore
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

parseBean 
    "package 
        foo.bar.baz 
        bean MyBean 
            Foo(String) 
            Bar(int) 
            FooBar(double) 
        end 
        bean MyBean2 
            Bar(double) 
        end" 


