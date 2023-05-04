
(* Parser Sample *)

(* NOTE: This example uses generic OCaml compatable syntax where the syntax where possible, which is
 * in most places!  There *are* LIBRARY differences between F# and OCaml that necessitate slighly
 * different functions to be used for input and output.
 *
 * If using OCaml instead of F#, check out: https://learnxinyminutes.com/docs/ocaml/
*)


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
    | Rel of string
    | Left_Par of string
    | Right_Par of string
    | Assign of string
    | Read of string
    | Write of string
    | If of string
    | Then of string // a comma is just a comma
    | Else of string
    | Fi of string
    | For of string  // don't care what the conjunction is
    | To of string  // don't care what the conjunction is
    | Do of string  // don't care what the conjunction is
    | Done of string  // don't care what the conjunction is
    | Step of string  // don't care what the conjunction is
    | Id of string

    with static member tokenFromLexeme str = // Function to get a token from a lexeme (String)
            match str with
            | "+" | "-" -> Add_Op str
            | "*" | "/" -> Mult_Op str
            | "<" | ">" | "==" -> Rel str
            | "("  -> Left_Par str
            | ")"  -> Right_Par str
            | ":="  -> Assign str
            | "read"  -> Read str
            | "write"  -> Write str
            | "if"  -> If str
            | "then"  -> Then str
            | "else"  -> Else str
            | "fi"  -> Fi str
            | "for"  -> For str
            | "to"  -> To str
            | "do"  -> Do str
            | "done"  -> Done str
            | "step"  -> Step str
            | x -> Id x








// NOTES:
// The |> operator sends (pipes) the output of one function directly to the next one in line.
// "and" just allows multiple, mutually recursive functions to be defined under a single "let"


let rec parse (theList: Token list) = program theList

//program ⟶ stmt_list
and program lst = lst |> stmt_lst

// stmt_list ⟶ stmt stmt_list | ε
//  stmt ⟶ assignment | read_stmt | write_stmt | for_loop | if_stmt    // so need to use these tokens
and stmt_list = function
    


    xs -> xs |> stmt
    and ->
    | Id x :: xs -> xs stmt_list
    | Read x :: xs -> xs stmt_list
    | Write x :: xs -> xs stmt_list
    | If x :: xs -> xs if_stmt_list
    | xs -> xs

and stmt = function
    | Id x :: xs -> xs |> assignment 
    | Read x :: xs -> xs |> read_stmt
    | Write x :: xs -> xs |>write_stmt
    | If x :: xs -> xs |> if_stmt
    | [] -> failwith "Unexpected end of input while processing stmt Phrase."
// assignment ⟶ id := expr
and assignment = function
    | Id x :: Assign a :: xs -> xs |> expr
    | x :: xs -> failwithf $"Expected ID, but found: %A{x}"
    | [] -> failwith "assignment should not be empty"

// expr ⟶ term term_tai
// term ⟶ factor factor_tail 
// factor ⟶ ( expr ) | id  // So To find out the pattern mach is correct, I have to see if it fits this
and expr = function
    | Left_Par x :: xs -> xs |> expr -> Right_Par x::xs   
    

(* Begin Parsing Process *)
let startParsing (str:string) =
    // Split the String (which creates an Array) -> convert the Array to a List -> MAP the list of strings into a list of Tokens.
    // (Note, though arrays are a lot like lists, lists are a bit easier to use for the pattern matching.)
    let tokenList =
        str.Split ' ' |>
        Array.toList |>
        List.map Token.tokenFromLexeme

    // Display our list of tokens... just for fun.
    printfn $"The initial String: %s{str}"
    printfn $"The initial List: %A{tokenList}\n"

    // Work the magic...
    try
        let parsedList = program tokenList
        in printfn $"The Final List:\n\t%A{parsedList}"
    with
        Failure msg -> printfn $"Error: %s{msg}";  System.Console.ReadLine () |> ignore




(* Get the user input and start parsing *)
// NOTE: To make the let assihnment be a function that accepts no parameters,
// an "empty tuple" must be accepted.
let promptAndGo () =
    (* TEST DATA *)
    // let userInput = "the fast , fast dog chases the fast cat ."
    // let userInput = "the noun chases the adj cat and the adj , adj , fast dog adv chases the cat prep a adj noun ."

    let userInput =
        printf "Enter String: ";
        // A case where it's easier to use the .NET ReadLine as opposed to the more restrictive OCaml native variant.
        System.Console.ReadLine ()

    in startParsing userInput


// RUN INTERACTIVELY AS A SCRIPT
promptAndGo ()

// Uncomment the following to pause at the end if it's running in a terminal which dissapears upon running.
// System.Console.ReadKey(true) |> ignore
