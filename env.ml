type t =
  {
    bindings : (string, Ast.value) Hashtbl.t;
    outer: t option
  }

exception NotfoundError of string

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
