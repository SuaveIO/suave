module Suave.Data

open Microsoft.FSharp.Reflection

open System
open System.Data
open System.Data.Common

let eval<'T> (reader:DbDataReader) =
    let recordType = typeof<'T>
    if FSharpType.IsRecord(recordType) then 
        let fieldCount = FSharpType.GetRecordFields(recordType).Length
        let vals = Array.create<obj> fieldCount null
        ignore (reader.GetValues(vals))
        let dataObj = FSharpValue.MakeRecord(recordType,vals) :?> 'T
        dataObj
    else if FSharpType.IsTuple(recordType) then
            let fieldCount = FSharpType.GetTupleElements(recordType).Length
            let vals = Array.create<obj> fieldCount null
            ignore (reader.GetValues(vals))
            let dataObj = FSharpValue.MakeTuple(vals,recordType) :?> 'T
            dataObj
        else
           reader.GetValue(0) :?> 'T

let PrintfFormatProc (worker: string * obj list -> 'b)  (query: PrintfFormat<'a, _, _, 'b>) : 'a = 

    if not (FSharpType.IsFunction typeof<'a>) then 
        unbox (worker (query.Value, [])) 
    else 
        let rec getFlattenedFunctionElements (functionType: Type) = 
            let domain, range = FSharpType.GetFunctionElements functionType 
            if not (FSharpType.IsFunction range) 
                then domain::[range] 
                else domain::getFlattenedFunctionElements(range) 

        let types = getFlattenedFunctionElements typeof<'a> 

        let rec makeFunctionType (types: Type list) =
            match types with
            | [x;y] -> FSharpType.MakeFunctionType(x,y)
            | x::xs -> FSharpType.MakeFunctionType(x,makeFunctionType xs)
            | _ -> failwith "shouldn't happen"
        let rec proc (types: Type list) (values: obj list) (a: obj) : obj =
            let values = a::values
            match types with
            | [x;y] -> 
                let result = worker (query.Value, List.rev values)
                box result
            | x::y::z::xs -> 
                let cont = proc (y::z::xs) values
                let ft = makeFunctionType (y::z::xs)
                let cont = FSharpValue.MakeFunction(ft, cont)
                box cont
            | _ -> failwith "shouldn't happen"
        
        let handler = proc types []
        unbox (FSharpValue.MakeFunction(typeof<'a>, handler))

open System.Text.RegularExpressions

type SQuery<'a,'b,'c> = PrintfFormat<'a,'b, 'c, DbCommand>

type SQLBuilder(conn: DbConnection) =

    let sqlProcessor (sql: string, values: obj list) : DbCommand =
    
        let stripFormatting s =
            let i = ref -1
            let eval (rxMatch: Match) =
                incr i
                sprintf "@p%d" !i
            Regex.Replace(s, "%.", eval)
    
        let sql = stripFormatting sql
    
        let cmd = conn.CreateCommand()
        cmd.CommandText <- sql

        let createParam i (p: obj) =
            let param = cmd.CreateParameter()
            param.ParameterName <- sprintf "@p%d" i
            param.Value <- p
            cmd.Parameters.Add param |> ignore

        values |> Seq.iteri createParam

        cmd

    member b.Query (a) = PrintfFormatProc sqlProcessor a

    member b.Enum (a) = seq {
        let cmd :DbCommand = PrintfFormatProc sqlProcessor a
        use reader = cmd.ExecuteReader()
        while reader.Read() do
            yield (eval reader)
        }
    member b.Connection = conn

    member b.Bind (cmd:DbCommand, rest) =
        use reader = cmd.ExecuteReader(CommandBehavior.SingleRow)
        if reader.Read() then rest (Some(eval reader)) else None

    member b.For (cmd:DbCommand, rest) =
        use reader = cmd.ExecuteReader()
        while reader.Read() do
            rest (eval reader)

    member b.Return expr = expr

    member b.Combine (expr1, expr2) =
       expr1 ; expr2

    member b.Delay (expr) = expr ()

    member b.Zero () = ()

let sql conn = SQLBuilder(conn)

let executeNonQuery (cmd:DbCommand) =
    cmd.ExecuteNonQuery() |> ignore

let executeCommand cmd arg = cmd arg |> executeNonQuery 
