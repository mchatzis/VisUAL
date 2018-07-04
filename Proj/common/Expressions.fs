//For dealing with expressions


//******************************************RYAN'S EXPRESSION CODE******************************************
module Expressions
//Relies on some of my helper functions
//Partially tested
    open Data
    open FlexOp2
    open System.Text.RegularExpressions 
    ///Used to create list of multiple types for Expression parsing
    type ExprType = Oper of string | Item of uint32 


    let (|Matches|_|) pattern input =
        if input = null then None
        else
            let m = Regex.Matches(input, pattern, RegexOptions.Compiled)
            if m.Count > 0 then Some ([ for x in m -> x.Value])
            else None

    ///Splits String by operators (+-*), labels (alphanumeric) and values
    let expSplit exp =  
        match exp with
        | Matches @"\w+|[+*-]|\D\w+" lst -> Ok lst
        | _ -> Error (sprintf "Not a valid expression")



    ///Checks string list contains only operators, values or defined symbols and evaluates them
    let checkValExp sym lst = 
        let operMatch x = match x with 
                            | "*" | "-" | "+" -> Ok( Oper x)
                            | Matches @"[ ]" i-> Error (sprintf "ERROR: Whitespace in expression") //Needs refining
                            //Currently, whitespace at beginnings of operands or between oprands e.g. DCD 50, 30  Throws an Error when it shouldn't
                            | Matches @"^[0][b|x][0-9]+$" i-> Ok (Item (uint32 i.[0])) //0bXXXXX or 0xXXXX
                            | Matches @"^[&][0-9]+$" i-> Ok (Item (uint32 (strReplAll i.[0] '&' "0x"))) //&XXXXX
                            | _ -> match System.UInt32.TryParse x with 
                                    | (true, i) -> Ok (Item i)
                                    | (false, _) -> match sym with
                                                    | Some i -> (mapSearchRes i x) |> Result.map Item 
                                                    | None -> Error (sprintf "ERROR: Symbol %A cannot be evaluated as no symbol table exists" x)

        lst |> List.map operMatch |> resListHandle 

    ///Left-Associative computation of result of a list containing operators (+-*) and values. 
    let evalExp exp = 
        let updateRunVal (curVal,state,newItem)  = match state with //Only called when state = one of these operators, so incomp. matching fine
                                                    | "*" -> uint32 (curVal * newItem)
                                                    | "-" -> uint32 (curVal - newItem)
                                                    | "+" -> uint32 (curVal + newItem)
                                                    | _ -> failwith "Should never fail, as comparing against a set of patterns used to call this function"                           

        let rec eval (runVal,st)  (lst: ExprType list) =
            match lst with
            | [] -> match st with 
                    | "" -> Error (sprintf "ERROR: Empty expression cannot be evaluated")
                    | "*" | "-" | "+" -> Error (sprintf "ERROR: Expression ends with invalid operator %A" st)
                    | "f" -> Ok (Option.get runVal)
                    | _ -> Error (sprintf "ERROR: Uncaught pattern %A at end of expression" st) //Should never run
            | x :: xs -> match (runVal,st, x) with
                            | (Some i, "f", Oper j) when List.exists (fun itm -> j= itm) ["*";"+";"-"] -> eval (Some i, j) xs
                            | (Some _, ("*" | "-" | "+"), Oper j) when List.exists (fun itm -> j= itm) ["*";"+";"-"] -> Error (sprintf "ERROR: Expression contains consecutive operators %A and %A" st x)                        
                            | (Some i, ("*" | "-" | "+"), Item j) ->  eval ( Some (updateRunVal (i,st, j)),  "f") xs
                            | (None, "", Oper j) when List.exists (fun itm -> j= itm) ["*";"+"] -> Error (sprintf "ERROR: Expression begins with invalid operator %A" j) //Expression can begin with negative
                            | (None, "", (Oper "-")) -> eval (Some (uint32 -1),"*") xs //Dealing with first item of expression allowed to be negative
                            | (None, "", Item j) -> eval (Some (j),"f") xs                            
                            | _ -> Error (sprintf "ERROR: Uncaught pattern in evalExp for %A" (runVal,st,x))
                                     
        exp |> eval (None,"")     


    ///Top level string expression parser. Requires table    
    let expParse tab exp=        
        exp 
        |> expSplit 
        |> Result.bind (checkValExp tab) 
        |> Result.bind evalExp 
        |> Result.bind checkLiteral

    //*********************************************************************************************************