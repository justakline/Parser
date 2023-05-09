
// A sample grammar
//
// sentence	: np vp np sentence_tail
// sentence_tail	: conj sentence | eos
// np       	: art adj_list noun pp
// adj_list 	: adj adj_tail | ε
// adj_tail 	: comma adj_list | ε     // comma as in “,”
// pp       	: prep np | ε
// vp       	: adv verb | verb

// Tokens
type Token =
    | Add_Op of string
    | Mult_Op of string
    | Rel_Op of string
    | Left_Par of string
    | Right_Par of string
    | Assign of string
    | Read of string
    | Write of string
    | If of string
    | Then of string // a comma is just a comma
    | Else of string
    | Fi of string
    | For of string // don't care what the conjunction is
    | To of string // don't care what the conjunction is
    | Do of string // don't care what the conjunction is
    | Done of string // don't care what the conjunction is
    | Step of string // don't care what the conjunction is
    | Equals of string
    | Id of string

    static member tokenFromLexeme str = // Function to get a token from a lexeme (String)
        match str with
        | "+"
        | "-" -> Add_Op str
        | "*"
        | "/" -> Mult_Op str
        | "<"
        | ">"
        | "==" -> Rel_Op str
        | "(" -> Left_Par str
        | ")" -> Right_Par str
        | ":=" -> Assign str
        | "read" -> Read str
        | "write" -> Write str
        | "if" -> If str
        | "then" -> Then str
        | "else" -> Else str
        | "fi" -> Fi str
        | "for" -> For str
        | "to" -> To str
        | "do" -> Do str
        | "done" -> Done str
        | "step" -> Step str
        | "=" -> Equals str
        | x -> Id x





let rec parse (theList: Token list) = program theList

//program ⟶ stmt_list
and program lst = lst |> stmt_list

// stmt_list ⟶ stmt     stmt_list | ε
//  stmt ⟶ assignment | read_stmt | write_stmt | for_loop | if_stmt    // so need to use these tokens
//Pipe to stmt then use the first of stmt_list, ie pattern match if it is a stmt, and if it is go to stmt_list else its empty
and stmt_list =
    function
    | xs ->
        let result = xs |> stmt

        result
        |> (function
        | Id x :: xs -> Id x :: xs |> stmt_list
        | Read x :: xs -> Read x :: xs |> stmt_list
        | Write x :: xs -> Write x :: xs |> stmt_list
        | If x :: xs -> If x :: xs |> stmt_list
        | xs -> xs) // Empty

//  stmt ⟶ assignment | read_stmt | write_stmt | for_loop | if_stmt    // so need to use these tokens
and stmt =
    function
    | Id x :: xs -> Id x :: xs |> assignment
    | Read x :: xs -> Read x :: xs |> read_stmt
    | Write x :: xs -> Write x :: xs |> write_stmt
    | For x :: xs -> For x :: xs |> for_loop
    | If x :: xs -> If x :: xs |> if_stmt
    | _ -> failwith "Unexpected pattern input while processing stmt. Expected id, read, write, for, or if"

// assignment ⟶ id := expr
and assignment =
    function
    | Id x :: Assign a :: xs -> xs |> expr
    | x :: xs -> failwithf $"Expected Id, but found: %A{x} at assignment"
    | [] -> failwith "assignment should not be empty"

// expr ⟶ term term_tai
and expr =
    function
    | xs -> xs |> term |> term_tail

// term ⟶ factor factor_tail
and term =
    function
    | xs -> xs |> factor |> factor_tail

//term_tail ⟶ add_op term term_tail | ε
and term_tail =
    function
    | Add_Op x :: xs -> xs |> term |> term_tail
    | xs -> xs //empty

// factor ⟶ ( expr ) | id
//Pattern match left parenth and pipe to expr, then pipe that result to a function to see if there is a right parenth
and factor =
    function
    | Left_Par x :: xs ->
        let result = xs |> expr

        result
        |> (function
        | Right_Par x :: xs -> xs
        | [] -> failwith "factor should have a right parenthesis"
        | _ -> failwith "Unexpected pattern in factor")
    | Id x :: xs -> xs
    | [] -> failwith "factor should not be empty"
    | _ -> failwith "Unexpected pattern in factor"

//factor_tail ⟶ mult_op factor factor_tail | ε
and factor_tail =
    function
    | Mult_Op x :: xs -> xs |> factor |> factor_tail
    | xs -> xs //Empty

//cond ⟶ expr rel_oper expr
and cond =
    function
    | xs ->
        let result = xs |> expr

        result
        |> (function
        | Rel_Op result :: xs -> xs |> expr
        | [] -> failwith "Unexpected pattern in cond"
        | _ -> failwith "Unexpected pattern in cond")

// read_stmt ⟶ read id
and read_stmt =
    function
    | Read x :: Id id :: xs -> xs
    | _ -> failwith "Unexpected pattern in read"



// write_stmt ⟶ write expr
and write_stmt =
    function
    | Write x :: xs -> xs |> expr
    | _ -> failwith "Unexpected pattern in write"

// if_stmt ⟶ if cond then stmt_list else_stmt
// Pattern match If, send to cond, then pattern match With then, and send to stmt_list and else_stmt
and if_stmt =
    function
    | If x :: xs ->
        let r1 = xs |> cond

        r1
        |> (function
        | Then y :: ys -> ys |> stmt_list |> else_stmt
        | _ -> failwith "Unexpected Pattern at the Then")

    | _ -> failwith "Unexpected Pattern at the if"

//else_stmt ⟶ else stmt_list fi | fi
and else_stmt =
    function
    | Fi x :: xs -> xs
    | Else x :: xs ->
        let result = xs |> stmt_list

        result
        |> (function
        | Fi y :: ys -> ys
        | _ -> failwith "Unexpected Pattern at else end of stmt")

    | _ -> failwith "Unexpected Pattern at else"

// for_loop ⟶ for id = id to id step_stmt do stmt_list done
//Pattern match from for to step_stmt, pipe into a function to pattern match do, then pipe into stmt_list and pattern match done
and for_loop =
    function
    | For x :: Id id1 :: Equals equals :: Id id2 :: To t :: Id id3 :: xs ->
        let r1 = xs |> step_stmt

        r1
        |> (function
        | Do d :: ds ->
            let r2 = ds |> stmt_list

            r2
            |> (function
            | Done y :: ys -> ys
            | _ -> failwith "Unexpected Pattern at Done of for_loop")
        | _ -> failwith "Unexpected Pattern at Do of for_loop")
    | _ -> failwith "Unexpected Pattern at For of for_loop"

and step_stmt =
    function
    | Step x :: Id id :: xs -> xs
    | Step x :: xs -> failwith "Unexpected Pattern at Step, did not provide id"
    | xs -> xs // empty





(* Begin Parsing Process *)
let startParsing (str: string) =
    // Split the String (which creates an Array) -> convert the Array to a List -> MAP the list of strings into a list of Tokens.
    // (Note, though arrays are a lot like lists, lists are a bit easier to use for the pattern matching.)
    let tokenList = str.Split ' ' |> Array.toList |> List.map Token.tokenFromLexeme

    // Display our list of tokens... just for fun.
    printfn $"The initial String: %s{str}"
    printfn $"The initial List: %A{tokenList}\n"

    // Work the magic...
    try
        let parsedList = program tokenList in
        printfn $"The Final List:\n\t%A{parsedList}\n"
        printfn $"The Sentence: \n\t%A{str}\nfollows the grammar"
    with Failure msg ->
        printfn $"Error: %s{msg}"
        System.Console.ReadLine() |> ignore




(* Get the user input and start parsing *)
// NOTE: To make the let assihnment be a function that accepts no parameters,
// an "empty tuple" must be accepted.
let promptAndGo () =
    (* TEST DATA *)
    // let userInput = "the fast , fast dog chases the fast cat ."
    // let userInput = "the noun chases the adj cat and the adj , adj , fast dog adv chases the cat prep a adj noun ."

    let userInput =
        printf "Enter a Program: "
        // A case where it's easier to use the .NET ReadLine as opposed to the more restrictive OCaml native variant.
        System.Console.ReadLine()

    in

    startParsing userInput


promptAndGo ()

