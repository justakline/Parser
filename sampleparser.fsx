
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
    | Noun of string
    | Verb of string
    | Art of string
    | Adj of string
    | Adv of string
    | Prep of string
    | Comma // a comma is just a comma
    | Conj  // don't care what the conjunction is
    | EOS   // the period at the end of the sentance
    | UNKNOWN of string

    with static member tokenFromLexeme str = // Function to get a token from a lexeme (String)
            match str with
            | "," -> Comma
            | "dog" | "cat" -> Noun str
            | "a" | "an" | "the" -> Art str
            | "verb" | "chases" | "loves" -> Verb str
            | "adj" | "fast" -> Adj str
            | "adv" | "quickly" -> Adv str
            | "prep" -> Prep str
            | "." -> EOS
            | "and" -> Conj
            | x -> UNKNOWN x




////////////////////////////////////////////////////////////////////////////////////////////////////
//
// The following is but one of many possible structures. In fact F#/Ocaml has many
// features that make parsing complex grammars pretty easy... but... to understand those requires a
// much deeper understanding of the language than we have explored.  Unfortunately, the result is
// that the code will not be nearly as concise or elegant as it could otherwise be. However, if you
// which to explore the additional features of the language, feel free to explore!!!
//
//////////////////////////////////////////////////////////////////////////////////////////////////////

// NOTE: A Better code approach MIGHT BE to use "Active Patterns", but those are a little more
// difficult to understand, especially while still trying to grasp "static patterns".
// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/active-patterns
// https://fsharpforfunandprofit.com/posts/convenience-active-patterns/



// NOTES:
// The |> operator sends (pipes) the output of one function directly to the next one in line.
// "and" just allows multiple, mutually recursive functions to be defined under a single "let"


let rec parse (theList: Token list) = sentence theList

// sentence ::= np vp np sentence_tail
and  sentence lst =
    lst |> np |> vp |> np |> sentenceTail

and sentenceTail = function
    | Conj :: xs -> sentence xs
    | [EOS] -> printfn "Parse Successful"; []
    | EOS :: xs ->  failwith "End of sentence marker found, but not at end!"
    | x :: xs -> failwithf $"Expected EOS but found: %A{x}"
    | [] -> failwith "Unexpected end of input while processing EOS"


// np ::= art adjLst noun pp
and np = function
    | Art theArticle :: xs -> xs |> adjList |> noun |> pp
    | x :: xs -> failwithf $"Expected article, but found: %A{x}"
    | [] -> failwith "article should not be empty"


// adjLst ::= adj adj_tail | <empty>
and adjList = function
    | Adj x :: xs -> xs |> adjTail
               // <empty> means the rule is empty (not the list), if there is no adjective, then…
    | xs -> xs // just resolve to what was passed (instead of failing)


// adjTail ::= comma adjLisy | <empty>
and adjTail = function
    | Comma :: xs -> xs |> adjList
    | xs -> xs // just resolve to what was passed (instead of failing)


// pp ::= prep np | <empty>
and pp = function
    | Prep thePrep :: xs -> xs |> np
    | xs -> xs


// vp ::= adv verb | verb
and vp = function
    | Verb x :: xs -> xs
    | Adv x :: Verb y :: xs -> xs
    | x :: xs -> failwithf $"Expected Verb Phrase, but found: %A{x}"
    | [] -> failwith "Unexpected end of input while processing Verb Phrase."


// Process the noun (which should be at the head of the list once this method is reached.
// Note: This function could be (and probably should be) defined as a “Lambda”, and included
// in-line in the nounPhrase function above, but it’s just separated out here for clarity.
and noun = function
    | Noun n :: xs -> xs // It's just a noun
    | x -> failwithf $"Expected Noun, but found: %A{x}"


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
        let parsedList = sentence tokenList
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
