module Swimlane

type style = 
    | Fill of string
type styles = List<style>

let emit_style = 
    let emit_one = function 
        | Fill color -> "fill:" + color
    Seq.map emit_one 
    >> Seq.append (Seq.singleton "stroke:black") 
    >> String.concat ";" 

type elem = 
    | Line of x1:int * y1:int * x2:int * y2:int
    | Rect of x:int * y:int * w:int * h:int * st:styles
    | Head of x:int * y:int 

let emit t = function 
    | Line (x1, y1, x2, y2) -> 
        fprintfn t "<line x1='%d' y1='%d' x2='%d' y2='%d' style='%s'/>"
                   x1 y1 x2 y2
                   (emit_style [])

    | Rect (x, y, w, h, ss) -> 
        fprintfn t "<rect x='%d' y='%d' width='%d' height='%d' style='%s'/>"
                   x y w h (emit_style ss)

    | Head (x, y) -> 
        fprintfn t """<polygon points='%d,%d %d,%d %d,%d' style='fill:black'/>"""
                   x y (x-10) (y+5) (x-10) (y-5)

let mk_swimlane_svg t evts role_of label_of =
    let roles = Seq.groupBy role_of evts |> Seq.cache

    // Constants
    let box_height = 60
    let box_margin = 30
    let stroke_width = 1
    let lane_header_width = 40
    let x_offset = 0
    let y_offset = 0
    let lane_height = box_height + 2 * box_margin - stroke_width
    let char_width = 8

    let no_lanes = Seq.length roles
    let no_spots = Seq.length evts

    let lane_of = 
        let m = 
            roles
            |> Seq.mapi (fun i (r, _) -> (r,i))
            |> Map.ofSeq
        role_of >> (fun r -> Map.find r m)

    let clip txt width = 
      let n = String.length txt
      let capacity = width / char_width
      if capacity < n then 
        txt.Substring (0, capacity-3) + "..."
      else
        txt

    let spot_width ev = 
      char_width * (4 + String.length (label_of ev))

    let lane_length = 
      lane_header_width 
        + (evts |> Seq.map spot_width |> Seq.sum)
        + (Seq.length evts + 1) * box_margin

    let emit = emit t
    let total_width = lane_length + x_offset + 1 
    let total_height = no_lanes * (box_height + 2 * box_margin) + y_offset + 10 + stroke_width

    fprintfn t "<svg width='%d' height='%d' viewBox='0 0 %d %d' xmlns='http://www.w3.org/2000/svg'>" total_width total_height total_width total_height 

              
    for i in 0 .. no_lanes-1 do
        let x = x_offset
        let y = y_offset + (i * lane_height)
        let labelx = x+15
        let labely = y + lane_height / 2
        Rect(x=x, y=y, w=lane_length, h=lane_height, st=[Fill "#e0f2f1"]) |> emit 
        fprintfn t """<text x='%d' y='%d' transform='rotate(90 %d %d)'
                            text-anchor='middle' class='svg-label'>%s</text>"""
                   labelx labely labelx labely (clip (Seq.item i roles |> fst) lane_height)

    let mutable last = None
    let mutable last_width = 0;
    for ev in evts do
        let y = lane_of ev * lane_height + y_offset + box_margin
        let width = spot_width ev
        let x = box_margin + 
                match last with
                | Some (x0, _) -> x0 + last_width 
                | None -> lane_header_width 
        Rect(x=x, y=y, w=width, h=box_height, st=[Fill "white"]) |> emit
        fprintfn t "<text x='%d' y='%d' text-anchor='middle' class='svg-label'>%s</text>"
           (x + width/2)
           (y + box_height/2 + 5)
           // (clip (label_of ev) (width/2))
           (label_of ev)

        match last with
        | None -> ()
        | Some (x0, y0) when y0 = y -> 
            Line(x1=x0+last_width, y1=y0+box_height/2,
                 x2=x, y2=y+box_height/2) |> emit
            Head(x=x, y=y+box_height/2) |> emit
        | Some (x0, y0) when y0 > y -> 
            Line(x1=x0+last_width/2, y1=y0,
                 x2=x0+last_width/2, y2=y+box_height/2) |> emit
            Line(x1=x0+last_width/2, y1=y+box_height/2,
                 x2=x, y2=y+box_height/2) |> emit
            Head(x=x, y=y+box_height/2) |> emit
        | Some (x0, y0) when y0 < y -> 
            Line(x1=x0+last_width/2, y1=y0+box_height,
                 x2=x0+last_width/2, y2=y+box_height/2) |> emit
            Line(x1=x0+last_width/2, y1=y+box_height/2,
                 x2=x, y2=y+box_height/2) |> emit
            Head(x=x, y=y+box_height/2) |> emit
        | _ -> ()
                       
        last <- Some (x, y)
        last_width <- width

    let w = lane_length + x_offset + 1 
    let h = no_lanes * (box_height + 2 * box_margin) + y_offset + 10 + stroke_width
    if not <| Seq.isEmpty evts then 
      let header_line_x = x_offset + lane_header_width
      Line(x1=header_line_x, y1=y_offset, 
           x2=header_line_x, y2=y_offset + lane_height * no_lanes) |> emit
    fprintfn t "</svg>"

(*
[<EntryPoint>]
let main _ = 
let evts = 
  [ "Organizer", "Seminar proposal is accepted"
  ; "Organizer", "Select participants"
  ; "Organizer", "Invite participants"
  ; "Organizer", "Finalize partcipant"
  ; "Dagstuhl staff", "Prepare the venue for the workshop"
  ; "Time", "Seminar day arrived"
  ; "Participant", "Attend the meetings and contribute"
  ; "Organizer", "Lead the meetings"
  ; "Organizer", "Organize social events"
  ; "Participant", "Suggest social events"
  ; "Participant", "Attend social events"
  ; "Dagstuhl staff", "Get ready the meeting rooms, meals and coffee"
  ; "Time", "Last day of the seminar arrived"
  ; "Participant", "Check out"
  ; "Participant", "Pay the expenses"
  ; "Organizer", "The seminar is finalized"
  ]
let role_of = fst
let label_of = snd
mk_swimlane_svg stdout evts role_of label_of
*)

            
