module XML

open DCR
open D

type XmlNode = System.Xml.XmlNode
(*
let serialize G (stream : System.IO.TextWriter) = 
  if DCR.has_subprocesses G then 
    error 0036 "Not implemented: Serialization of sub-processes to XML."
  let settings = new XmlWriterSettings ()
  settings.Indent <- true
  use XmlWriter w = XmlWriter.Create(stream, settings)

*)


let deserializeGraph (doc : XmlNode) = 

    let value (node : XmlNode) = 
        node.Value 

    let select path = 
        doc.SelectNodes path  
        |> Seq.cast<XmlNode> 
        |> Seq.map value 

    let labels = 
        let keys = 
            select "dcrgraph/specification/resources/labelMappings/labelMapping/@eventId" 
        let vals = 
            select "dcrgraph/specification/resources/labelMappings/labelMapping/@labelId" 
        Seq.zip keys vals |> Map.ofSeq

    // Exformatics support labels for groups; we don't. We'll identify
    // groups by their labels; this means that we (a) have to abort here
    // if group labels are not distinct and (b) later will have to patch
    // up relations (which contain group-events, we want group-labels).

    let groupings = 
        let isLeaf (node : XmlNode) = node.["event"] = null 
        let rec loop (node : XmlNode) = 
            node.SelectNodes "event" 
            |> Seq.cast<XmlNode>
            |> Seq.fold (fun (p, evts) n -> 
                             let id = match n.Attributes.["id"] with 
                                      | null -> error 0020 "Attribute 'id' missing from 'event' element."
                                      | attr -> attr.Value
                             let title = 
                               match Map.tryFind id labels with
                               | Some t -> t
                               | None -> id  // Special case introduced for Exformatics/Dreyer dump, May '15. 
                             if isLeaf n then 
                                p, Set.add id evts
                             else
                                let p', evts' = loop n
                                let p'' = 
                                   (Some (id,title), 
                                    Set.union (Grouping.domain p') evts', 
                                    p')
                                   |> Set.singleton 
                                   |> Grouping.Partition
                                Grouping.merge p p'', Set.union evts' evts)
                        (Grouping.Partition Set.empty, Set.empty)

        match doc.SelectSingleNode "dcrgraph/specification/resources/events" with
        | null -> Grouping.Partition Set.empty
        | x -> x |> loop |> fst

    let groups = 
        Grouping.map' (fun (name, es, P) -> (Option.map snd name, es, P)) groupings
    let raw_groups = 
        Grouping.map' (fun (name, es, P) -> (Option.map fst name, es, P)) groupings
    let group_id_label = Grouping.names groupings 
    let group_id_map = group_id_label |> Map.ofSeq

    // Nested events are events, and thus labelled. We don't have labels, so
    // for usability reasons we elevate labels to identities. This means of 
    // course that we can't accept duplicate labels.
    group_id_label
    |> Seq.groupBy snd
    |> Seq.choose (fun (label, names) -> 
                    if Seq.length names > 1 
                    then Some label
                    else None)
    |> Seq.iter (fun name -> error 0017 "Duplicate group name '%s' in XML input" name)

    let convert_group_labels = 
        fun e -> match Map.tryFind e group_id_map with
                 | Some g -> g
                 | None -> e

    let isGroup e = Map.containsKey e group_id_map

    let mkrel p1 p2 = 
        Seq.zip (select p1) (select p2)
        // Convert group events to group labels
        |> Seq.map (fun (e, f) -> (convert_group_labels e, convert_group_labels f))
        |> Seq.groupBy fst
        |> Seq.map (fun (e, fs) -> e, Seq.map snd fs |> Set.ofSeq)
        |> Map.ofSeq

    let G = { 
        events = select "dcrgraph/specification/resources/events//event[not(event)]/@id" 
                 |> Set.ofSeq

        labels = labels 
                 |> Map.filter (fun e _ -> isGroup e |> not)
                 |> Map.map (fun _ v -> DCR.Neutral v)

        incl = mkrel "dcrgraph/specification/constraints/includes/include/@sourceId"
                     "dcrgraph/specification/constraints/includes/include/@targetId"
        excl = mkrel "dcrgraph/specification/constraints/excludes/exclude/@sourceId"
                     "dcrgraph/specification/constraints/excludes/exclude/@targetId"
        resp = mkrel "dcrgraph/specification/constraints/responses/response/@sourceId"
                     "dcrgraph/specification/constraints/responses/response/@targetId"
        cond_on = mkrel "dcrgraph/specification/constraints/conditions/condition/@targetId"
                        "dcrgraph/specification/constraints/conditions/condition/@sourceId"
        mile_on = mkrel "dcrgraph/specification/constraints/milestones/milestone/@targetId"
                        "dcrgraph/specification/constraints/milestones/milestone/@sourceId"

        exec = select "dcrgraph/runtime/marking/executed/event/@id" 
               |> Seq.filter (isGroup >> not) |> Set.ofSeq 
        insi = select "dcrgraph/runtime/marking/included/event/@id" 
               |> Seq.filter (isGroup >> not) |> Set.ofSeq 
        pend = select "dcrgraph/runtime/marking/pendingResponses/event/@id" 
               |> Seq.filter (isGroup >> not) |> Set.ofSeq 

        // Exformatics XML currently defines neither sub-processes nor local events.  
        local = Set.empty
        defs  = Map.empty

        groups = groups

        // TODO. XML has "expression" element, but I've only seen it empty. 
        conds = []

        tags = 
            let p (node : XmlNode) = 
                let e = node.SelectSingleNode "@id" |> value
                let roles = node.SelectNodes "./custom/roles/role" 
                            |> Seq.cast<XmlNode> 
                            |> Seq.map (fun n -> n.InnerText)
                            |> Seq.filter (System.String.IsNullOrWhiteSpace >> not)
                            |> Set.ofSeq
                if Seq.isEmpty roles 
                then (e, Map.empty)
                else (e, Map.add "role" roles Map.empty)
            doc.SelectNodes "dcrgraph/specification/resources/events//event[not(event)]"
             |> Seq.cast<XmlNode>
             |> Seq.map p
             |> Seq.filter (fst >> isGroup >> not)
             |> Map.ofSeq

        store = Map.empty // TODO: Import keys from exformatics store

        highlights = []
        text = ""

        // Exformatics don't have time yet. 
        t_ex = Map.empty
        t_re = Map.empty
        t_c = Map.empty
        t_r = Map.empty
    }

    // "groups" in exformatics parlance.
    let collections = 
      select "dcrgraph/specification/resources/custom/groups/group/text()" 
      |> Seq.map (fun coll -> 
        sprintf "dcrgraph/specification/resources/events//event[.//groups/group[text()='%s']]/@id" coll
        |> select
        |> Seq.collect (fun e -> 
          match Grouping.tryFindGroup raw_groups e with
          | None -> Set.singleton e
          | Some (_, es, _) -> Set.ofSeq es)
        |> fun elems -> coll, elems)
      |> Map.ofSeq

    G, collections
    

let deserialize (stream : System.IO.TextReader) = 
    try 
        let doc = new System.Xml.XmlDocument () 
        doc.Load stream
        deserializeGraph doc
    with
    | :? System.Xml.XmlException as e -> 
            error 0018 "XML parse error: %s" e.Message
    | :? System.NullReferenceException ->
            error 0021 "XML parse error: Something is missing."
