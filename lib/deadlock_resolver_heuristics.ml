open Types

let get_top_layer (lambdas: LambdaTagged.t list) : LambdaTagged.t list = 
  let rec do_get_top_layer (lambda: LambdaTagged.t) : LambdaTagged.t list =
    match lambda with
    | LList(eta, _)
    | LRepl(eta, _) as l ->
      [l]
    | LOrE(a, b)
    | LOrI(a, b)
    | LPar(a, b) ->
      (do_get_top_layer a)@(do_get_top_layer b)
    | LNil ->
      []
  in
  lambdas |>
  List.map do_get_top_layer
  |> List.flatten

let get_next_layer (lambdas: LambdaTagged.t list) : LambdaTagged.t list =
  let rec do_get_next_layer (lambda: LambdaTagged.t) : LambdaTagged.t list =
    match lambda with
    | LList(eta, l)
    | LRepl(eta, l) ->
      get_top_layer [l]
    | LOrE(a, b)
    | LOrI(a, b)
    | LPar(a, b) ->
      (do_get_next_layer a)@(do_get_next_layer b)
    | LNil ->
      []
  in
  lambdas |>
  List.map do_get_next_layer
  |> List.flatten

let get_top_eta lambda =
  match lambda with
  | LambdaTagged.LList(eta, _)
  | LRepl(eta, _) -> eta
  | _ -> failwith "This should not happen!"

module type Deadlock_resolver_heuristic =
  sig
    type score
    type eta_score = (EtaTagged.eta * score)
    val best_etas: LambdaTagged.t -> state list -> eta_score list
    val print_eta_score: Format.formatter -> eta_score list -> unit
  end


module Heuristic_by_layer: Deadlock_resolver_heuristic =
struct
  type score = (Eta.eta * int)
  type eta_score = (EtaTagged.eta * score)
  let get_Etas_by_layer lambda: (Eta.eta, int) Hashtbl.t = 
    let etas_by_layer = Hashtbl.create (0) in
    let add_or_replace_min (eta: Eta.eta) (layer: int) =
      match Hashtbl.find_opt etas_by_layer eta with
      | Some(old_layer) -> if layer < old_layer then Hashtbl.replace etas_by_layer eta layer
      | None -> Hashtbl.add etas_by_layer eta layer
    in
    let rec iterate_layers lambda layer =
      if lambda <> [] then (
        List.iter (
          fun lambda ->
            add_or_replace_min (etaTaggedToEta (get_top_eta lambda)) layer
        ) lambda;
        let new_lambda = get_next_layer lambda in
        iterate_layers new_lambda (layer+1)
      )
    in
    iterate_layers (get_top_layer lambda) 0;
    etas_by_layer

  (* Receives a deadlocked state.
     For each problematic action return a score (heuristic indicating how good a choice that ) *)
  let get_blocked_eta_score ((lambdas, ctx): state): eta_score list = 
    (* Hashmap of eta to layer number*)
    let etas_by_layer = get_Etas_by_layer lambdas in

    Format.fprintf debug_fmt "etas_by_layer:\n";
    Hashtbl.iter (
      fun eta layer -> Format.fprintf debug_fmt "- %a: %i\n" Eta.print_eta eta layer) etas_by_layer;

    get_top_layer lambdas
    |> List.map (
      fun lambda -> (
        let blocked_eta = get_top_eta lambda in
        let compl_eta_score: (Eta.eta * int) list =
          get_next_layer [lambda]
          (* List of compl etas of the etas directly bellow the blocked eta *)
          |> List.map (fun l -> l |> get_top_eta |> EtaTagged.compl_eta)
          (* make a pair of the compl_eta and the layer number where the compl_eta is located *)
          |> List.filter_map (
            fun compl_eta ->
              Hashtbl.find_opt etas_by_layer compl_eta
              |> Option.map ( fun (layer) -> (compl_eta, layer))
          )
        in

        (* Debug print *)
        List.iter (
          fun (compl_eta, layer) ->
            Format.fprintf debug_fmt "blocked: %a; compl_eta: %a; layer: %i\n"
              EtaTagged.print_eta_simple blocked_eta
              Eta.print_eta_simple compl_eta
              layer
        ) compl_eta_score;

        compl_eta_score
        |> List.map ( fun e -> (blocked_eta, e))
      )
    )
    |> List.flatten

  let sort_eta_score  (eta1, (_, layer1)) (eta2, (_, layer2)) = 
    layer1 - layer2

  let best_etas exp deadlocked_states: eta_score list =
    deadlocked_states
    |> List.map get_blocked_eta_score
    |> List.flatten
    |> List.sort sort_eta_score

  let print_eta_score fmt (eta_scores: eta_score list) =
  eta_scores
  |> List.sort (fun (eta1, (_, layer1)) (eta2, (_, layer2)) -> layer1 - layer2)
  |> List.iter (
      fun (blocked_eta, (compl_eta, layer)) -> (
        Format.fprintf fmt "blocked: %a; compl_eta: %a; layer: %i\n"
          EtaTagged.print_eta_simple blocked_eta
          Eta.print_eta_simple compl_eta
          layer
      );
    )
end

module Heuristic_NOP: Deadlock_resolver_heuristic =
struct
  type score = unit
  type eta_score = (EtaTagged.eta * score)

  let best_etas exp (deadlocked_states: state list): eta_score list =
    deadlocked_states
    |> List.map (fun (lambdas, _) -> lambdas)
    |> List.map get_top_layer
    |> List.flatten
    |> List.map get_top_eta
    |> List.map (fun eta -> (eta, ()))

  let print_eta_score fmt (eta_scores: eta_score list): unit =
  ()
end


module Heuristic_New: Deadlock_resolver_heuristic =
struct
  type eta_sequence = Eta.eta list
  module EtaTSequence =
  struct
    type t = EtaTagged.eta list
    let compare = compare
    let print fmt (sequence: t) =
      Format.fprintf fmt "[";
      sequence
      |> List.iter (Format.fprintf fmt "%a," EtaTagged.print_eta);
      Format.fprintf fmt "]";
  end
  module EtaTSequenceSet = Set.Make(EtaTSequence)

  type score = (EtaTSequence.t list)
  type eta_score = (EtaTagged.eta * score)
  let sort_eta_score (eta1, seq1) (eta2, seq2) =
    let res = (List.length seq2) - (List.length seq1) in
    if res = 0 then
      compare eta1 eta2
    else
      res

  let [@warning "-8"] rec find_eta_sequence
      eta_location_cache ((eta::etas_tl) as etas) visited (lambda: LambdaTagged.t): (EtaTSequence.t) list =
    match lambda with
    | LList(e, l)
    | LRepl(e, l) -> (
      if (EtaTaggedSet.mem e visited) then
        []
      else (
        let visited = EtaTaggedSet.add e visited in
        if eta = etaTaggedToEta e then (
          (* Found next eta in the sequence *)
          if etas_tl = [] then (
            [[e]] (* Found all etas in the sequence*)
          ) else (
            (* Still need to find the remaining *)
            find_eta_sequence eta_location_cache etas_tl visited l
            |> List.map (fun tl -> e::tl)
          )
        ) else (
          (* Is not the next eta in the sequence*)
          (find_eta_sequence eta_location_cache etas visited l)
          @
          (
            (* try follow a transitive dependecy *)
            Hashtbl.find_all eta_location_cache (EtaTagged.compl_eta e)
            |> List.map (find_eta_sequence eta_location_cache etas visited)
            |> List.flatten
          )
        )
      )
    )
    | LOrI(a, b)
    | LOrE(a, b)
    | LPar(a, b) ->
      (find_eta_sequence eta_location_cache etas visited a)
      @
      (find_eta_sequence eta_location_cache etas visited b)
    | LNil -> []

  let find_inverted lambda lambdas eta_location_cache =
    let first_eta = get_top_eta lambda in
    let second_etas =
      [lambda]
      |> get_next_layer
      |> List.map get_top_eta
    in
    let compl_eta_sequences_list: EtaTSequence.t list =
      second_etas
      |> List.map (
        fun (second_eta) ->
          let eta_seq = [EtaTagged.compl_eta second_eta; EtaTagged.compl_eta first_eta] in
          lambdas
          |> List.filter (fun l ->  l <> lambda) (* Filter itself *)
          |> List.map (find_eta_sequence eta_location_cache eta_seq (EtaTaggedSet.empty))
        |> List.flatten
      )
      |> List.flatten
    in
    (first_eta, compl_eta_sequences_list)
  
  let rec poppulate_eta_location_cache eta_location_cache (exp: LambdaTagged.t) = 
    match exp with
    | LRepl(a, l)
    | LList(a, l) -> 
      let a = etaTaggedToEta a in
      Hashtbl.add eta_location_cache a l;
      poppulate_eta_location_cache eta_location_cache l
    | LOrI(a, b)
    | LOrE(a, b)
    | LPar(a, b) ->
      poppulate_eta_location_cache eta_location_cache a;
      poppulate_eta_location_cache eta_location_cache b;
    | LNil -> ()


  let best_etas exp deadlocked_states: eta_score list =
    let eta_scores = Hashtbl.create 16 in
    deadlocked_states
    |> List.map (fun (lambdas, _) -> get_top_layer lambdas)
    |> List.map (fun (lambdas) -> (
      (* For each deadlocked state found, build the eta_location_cache for that state *)
      let eta_location_cache = Hashtbl.create 16 in
      List.iter (poppulate_eta_location_cache eta_location_cache) lambdas;
      (lambdas, eta_location_cache) 
    ))
    |> List.iter (fun (lambdas, eta_location_cache) -> (
      (* For each deadlocked state*)
      lambdas
      |> List.iter (
        fun lambda -> (
          (* For each lamdba in each deadlocked state, find *)
          let (first_eta, compl_eta_sequences_list) = find_inverted lambda lambdas eta_location_cache in
            (* Aggregate the results in a hast able, where the key is the first eta,
               and the value a Set of all the inverted sequences it found, without duplicates of course *)
            let list = Hashtbl.find_opt eta_scores first_eta in
            match list with
            | None -> Hashtbl.add eta_scores first_eta (EtaTSequenceSet.of_list compl_eta_sequences_list)
            | Some(seq) -> 
              Hashtbl.replace eta_scores first_eta
                (EtaTSequenceSet.union seq (EtaTSequenceSet.of_list compl_eta_sequences_list) )
          )
        )
    ));
    (* Transform the Hastable and Set to lists *)
    Hashtbl.to_seq eta_scores
    |> Seq.map (
      fun ((first_eta: EtaTagged.eta), eta_sequence_set) ->
        (first_eta, EtaTSequenceSet.elements eta_sequence_set)
    )
    |> List.of_seq
    |> List.sort sort_eta_score

  let print_eta_score fmt (eta_scores: eta_score list): unit =
    eta_scores
    |> List.sort sort_eta_score
    |> List.iter (
      fun (first_eta, sequences) -> (
        Format.fprintf fmt "%a (%d):" EtaTagged.print_eta first_eta (List.length sequences);
        sequences 
        |> List.iter (Format.fprintf fmt " %a" EtaTSequence.print);
        Format.fprintf fmt "\n"
      )
    )
end