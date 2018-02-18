module CMMN

open D

let quote (e : string) = 
  e.Replace (' ', '_')

let id (e : string) = 
  e.GetHashCode()

        
let (%?) = DCR.(%?)


// Find e such that (e,f) \in rel.
let invert f rel = 
  Map.toSeq rel 
  |> Seq.filter (fun (e, fs) -> Set.contains f fs)
  |> Seq.map fst
  |> Set.ofSeq


let mayBecomeExcluded (G : DCR.dcr) e = 
  not (Set.contains e G.insi) || 
  not (Set.isEmpty (invert e G.excl))


let mayBecomePending (G : DCR.dcr) e = 
  Set.contains e G.pend || 
  not (Set.isEmpty (invert e G.resp))
  

let isCondition (G : DCR.dcr) e = 
  G.cond_on 
  |> Map.toSeq
  |> Seq.map snd
  |> Set.unionMany
  |> Set.contains e 



let preamble (G : DCR.dcr) title = 
  sprintf 
    <| """
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
  <cmmn:definitions id="%u"
    targetNamespace="http://cmmn.org" xmlns:cmmn="http://www.omg.org/spec/CMMN/20151109/MODEL"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:camunda="http://camunda.org/schema/1.0/cmmn">
    <cmmn:case id="%u">
      <cmmn:casePlanModel autoComplete="false" name="%s" id="%u">
"""
    <| G.GetHashCode()
    <| G.GetHashCode()
    <| title
    <| title.GetHashCode()


let backmatter = """
    </cmmn:casePlanModel>
  </cmmn:case>
</cmmn:definitions>
"""

let planItem name enter exit = 
  sprintf """
       <cmmn:planItem definitionRef="%s" id="%u">
         <cmmn:entryCriterion sentryRef="%s_%s_sentry" />
         <cmmn:exitCriterion sentryRef="%s_%s_sentry" />
       </cmmn:planItem>""" name (id name) name enter name exit


let planItems1 (G : DCR.dcr)  (e : string) = 
  seq {
    yield planItem e "enabled" "disabled"
    if mayBecomeExcluded G e then 
      yield planItem (e + "_inc") "plus" "minus"
    if mayBecomePending G e then 
      yield planItem (e + "_res") "plus" "minus"
    if isCondition G e then
      let e' = e + "_exec"
      yield sprintf """
       <cmmn:planItem definitionRef="%s" id="%u">
         <cmmn:entryCriterion sentryRef="%s_sentry" />
       </cmmn:planItem>""" e' (id e') e'
  }
  |> String.concat "\n"
    

let planItems (G : DCR.dcr) = 
  G.events 
  |> Seq.map (planItems1 G)
  |> String.concat "\n"

let sentryBodyOnEvent e = 
  let e = quote e
  sprintf """
            <cmmn:planItemOnPart sourceRef="%s">
              <cmmn:standardEvent>occur</cmmn:standardEvent>
            </cmmn:planItemOnPart>
            <cmmn:ifPart>
              <cmmn:condition>${true}</cmmn:condition>
            </cmmn:ifPart>""" e


let sentry id body = 
  if body = "" then 
    "" 
  else 
    sprintf """
       <cmmn:sentry id="%s">%s
       </cmmn:sentry>""" id body


let toDisjunction bodies = 
  match Seq.length bodies with 
  | 0 -> ""
  | 1 -> Seq.item 0 bodies
  | _ -> 
    bodies 
    |> Seq.map (fun body -> """
           <cmmn:disjunct>""" + body + """
           </cmmn:disjunct>""")
    |> String.concat ""
    |> fun txt -> """
          <cmmn:disjunction>""" + txt + """
          </cmmn:disjunction>"""


let incPlusSentry (G : DCR.dcr) e = 
  invert e G.incl
  |> Set.map sentryBodyOnEvent 
  |> toDisjunction
  |> sentry (quote e + "_inc_plus_sentry")
    

let incMinusSentry (G : DCR.dcr) e = 
  Set.difference (invert e G.excl) (invert e G.incl)
  |> Set.map sentryBodyOnEvent
  |> toDisjunction
  |> sentry (quote e + "_inc_minus_sentry")


let resPlusSentry (G : DCR.dcr) e = 
  let e = quote e
  sentry (e + "_res_plus_sentry") (sentryBodyOnEvent e)

  
let resMinusSentry (G : DCR.dcr) e = 
  invert e G.resp
  |> Set.map sentryBodyOnEvent
  |> toDisjunction
  |> sentry (quote e + "_res_minus_sentry")


let execSentry e =
  let e = quote e
  sentry (e + "_exec_sentry") (sentryBodyOnEvent e)
 

let enabledSentry (G : DCR.dcr) e = 
  seq {
    for f in G.mile_on %? e do 
      yield sprintf "(!%%%s || %%%s)" 
        (quote (f + "_inc_milestone")) 
        (quote (f + "_res_milestone")) 
    for f in G.cond_on %? e do 
      yield sprintf "(!%%%s || %%%s)" 
        (quote (f + "_inc_milestone")) 
        (quote (f + "_exec_milestone")) 
  }
  |> String.concat " && " 
  |> fun x -> if x = "" then "${true}" else x
  |> sprintf """       
         <cmmn:ifPart>
           <cmmn:condition>%s</cmmn:condition>
         </cmmn:ifPart>""" 
  |> sentry (e + "_enabled_sentry")
       

let sentries (G : DCR.dcr) = 
  G.events 
  |> Seq.map 
      (fun f -> 
         seq { 
           if mayBecomeExcluded G f then 
             yield incPlusSentry G f
             yield incMinusSentry G f
           if mayBecomePending G f then 
              yield resPlusSentry G f
              yield resMinusSentry G f
           yield execSentry f
           yield enabledSentry G f
         })
   |> Seq.map (String.concat "\n")
   |> String.concat "\n"
      
      
let milestone1 (e : string) = 
  sprintf """
       <cmmn:milestone name="%s" id="%u" />""" <| quote e <| (quote e).GetHashCode()


let tasks (G : DCR.dcr) = 
  G.events 
  |> Seq.map (fun e -> 
    seq { 
      yield sprintf """
       <cmmn:humanTask isBlocking="true" name="%s" id="%u">
         <cmmn:defaultControl />
       </cmmn:humanTask>""" <| quote e <| (quote e).GetHashCode()
      if mayBecomePending G e then 
        yield milestone1 (e + "_res_milestone")
      if mayBecomeExcluded G e then 
        yield milestone1 (e + "_inc_milestone")
      if isCondition G e then 
        yield milestone1 (e + "_exec_milestone")
  })
  |> Seq.map (String.concat "")
  |> String.concat "\n"


let cmmn (G : DCR.dcr) = 
  [ 
    planItems G
    "\n"
    sentries G
    "\n"
    tasks G
  ] 
  |> String.concat ""
  |> (+) (preamble G "cmmn-export")
  |> fun txt -> txt + backmatter
    
  

