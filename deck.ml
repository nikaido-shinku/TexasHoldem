open Card

type t =  Card.t list

let empty = []

let is_empty = 
  (=) []

let deal d = 
  match d with 
  |h :: t -> Some h
  | _ -> None

let size = List.length



let shuffle d =
  failwith "unimplemented"





let mem deck rank suite = 
  failwith "unimplemented" 


let remove deck rank suit = 
  failwith "unimplemented" 

let insert card deck = 
  failwith "unimplemented" 