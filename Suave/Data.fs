module Suave.Data

open Microsoft.FSharp.Reflection

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

type SQLBuilder(conn: DbConnection) =
    member b.Connection = conn
    member b.Bind (expr, rest) =
        use cmd = conn.CreateCommand(CommandText = expr)
        use reader = cmd.ExecuteReader(CommandBehavior.SingleRow)
        if reader.Read() then rest (Some(eval reader)) else None
    member b.For (expr, rest) =
        use cmd = conn.CreateCommand(CommandText = expr)
        use reader = cmd.ExecuteReader()
        while reader.Read() do
            rest (eval reader)
    member b.Return expr = expr
    member b.Combine (expr1, expr2) =
       expr1 ; expr2
    member b.Delay (expr) = expr ()
    member b.Zero () = ()

let sql conn = SQLBuilder(conn)