type value =
  | Bool   of bool
  | Int    of int
  | String of string
  | Null
  | Callable of string * (value list -> value)

and expr =
  | Value        of value
  | Sum          of expr * expr
  | Diff         of expr * expr
  | Frac         of expr * expr
  | Prod         of expr * expr
  | Greater      of expr * expr
  | Lesser       of expr * expr
  | GreaterEqual of expr * expr
  | LesserEqual  of expr * expr
  | Equal        of expr * expr
  | NotEqual     of expr * expr
  | Id           of string
  | Call         of expr * expr list
  | Assign       of string * expr

and stmt =
  | Print of expr
  | Declaration of string * expr
  | If of expr * stmt list * stmt list
  | Function of string * string list * stmt list
  | ExprStmt of expr


exception NotfoundError of string

type env = {outer: env option; bindings : (string, value) Hashtbl.t }

let make outer = { outer = outer; bindings = Hashtbl.create 7; }

let add env name value =  Hashtbl.add env.bindings name value

let rec replace env name value =
  match Hashtbl.mem env.bindings name with
  | true ->
     Hashtbl.replace env.bindings name value
  | false ->
     match env.outer with
     | None ->
        raise (NotfoundError (Format.sprintf "variable %s not declared" name))
     | Some t ->
        replace t name value

let rec lookup env name =
  match Hashtbl.mem env.bindings name with
  | true ->
     Hashtbl.find env.bindings name
  | false ->
     match env.outer with
     | None ->
        raise (NotfoundError (Format.sprintf "variable %s not declared" name))
     | Some t ->
        lookup t name
