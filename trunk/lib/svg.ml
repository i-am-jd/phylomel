open Printf

let header width height =
    sprintf "<?xml version=\"1.0\" standalone=\"no\"?>
            <svg width=\"%d\" height=\"%d\" version=\"1.1\"
            baseProfile=\"full\"
            xmlns=\"http://www.w3.org/2000/svg\"
            xmlns:xlink=\"http://www.w3.org/1999/xlink\"
            xmlns:ev=\"http://www.w3.org/2001/xml-events\">\n"
	width height

(* let create print chan_op header = *)
(*     print header; *)
(*     { print = print; *)
(*       chan =  chan_op; } *)

(* let createToChan chan header = *)
(*     create (output_string chan) (Some chan) header *)

(* let createToFile file header = *)
(*     let chan = open_out file in *)
(*     createToChan chan header *)

(* let createToBuffer b header = *)
(*     create (Buffer.add_string b) None header *)

(* let finalise p = *)
(*     p.print "</svg>"; *)
(*     match p.chan with *)
(* 	| Some(chan) -> close_out chan *)
(* 	| _ -> () *)

let rgb x y z =
    sprintf "rgb(%f,%f,%f)" x y z

let put = IO.nwrite

let close out =
	put out "</svg>";
	IO.close_out out
	    
let putLine out ?(color="black") ?(width=2.) (x1,y1) (x2,y2) =
	put out
    (sprintf
	"\t<line x1=\"%.2f\" y1=\"%.2f\" x2=\"%.2f\" y2=\"%.2f\" style=\"stroke:%s;stroke-width:%f\"/>\n"
	x1 y1 x2 y2 color width)

let str_of_list =
	let soi = string_of_int in
	let rec print' acc = function
		| [x] -> acc ^ (soi x)
		| h::t ->
			  let acc = acc ^ (soi h) ^ ", " in
			  print' acc t
		| [] -> "" in
	print' ""

let putDottedLine out
	?(color="black")
	?(width=2.)
	?(dashes=[9;5])
	(x1,y1) (x2,y2) =
		put out
		(sprintf
		 "\t<line x1=\"%.2f\" y1=\"%.2f\" x2=\"%.2f\" y2=\"%.2f\" style=\"stroke:%s;stroke-width:%f;stroke-dasharray:%s\"/>\n"
		 x1 y1 x2 y2 color width (str_of_list dashes))
			
let put2Lines out ?(color="black") ?(width=2.) (x1,y1) (x2,y2) (x3,y3) =
	put out
    (sprintf
	"\t<polyline points=\"%.2f,%.2f %.2f,%.2f %.2f,%.2f\" style=\"fill:none;stroke:%s;stroke-width:%f\"/>\n"
	x1 y1 x2 y2 x3 y3 color width)
		
let put3Lines out ?(color="black") ?(width=2.) (x1,y1) (x2,y2) (x3,y3) (x4,y4) =
	put out
    (sprintf
	"\t<polyline points=\"%.2f,%.2f %.2f,%.2f %.2f,%.2f %.2f,%.2f\" style=\"fill:none;stroke:%s;stroke-width:%f\"/>\n"
	x1 y1 x2 y2 x3 y3 x4 y4 color width)

let putPoint out (x,y) =
	put out (sprintf "%.2f,%.2f " x y)
		
let putLines out ?(color="black") ?(width=2.) points =
    put out "\t<polyline points=\"";
    Array.iter (putPoint out) points;
    put out "\" style=\"fill:none;stroke:black;stroke-width:2\"/>\n"
	
let putPolygon out ?(fill="none") ?(stroke="black") ?(width=2.) points =
    put out "\t<polygon points=\"";
    Array.iter (putPoint out) points;
    put out (sprintf "\" style=\"fill:%s;stroke:%s;stroke-width:%f\"/>\n" fill stroke width)

let putCircle out ?(fill="none") ?(stroke="black") ?(width=1.) r (x,y) =
	put out
	(sprintf
	"<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"%s\" stroke=\"%s\" stroke-width=\"%f\" />\n" 
	x y r fill stroke width)

let putText out s ?(font="Arial") ?(size=10) ?(anchor="left") ?(fill="none") (x,y) =
	put out
    (sprintf
	"\t<text id=\"TextElement\" x=\"%.f\" y=\"%.f\" style=\"font-family: %s; font-size:%dpx; text-anchor: %s; fill:%s\"> %s </text>\n"
	x y font size anchor fill s)
				
let putLink out s ?(font="Arial") ?(size=10) ?(target="") ~link (x,y) =
	put out
    (sprintf
	"\t<a xlink:href=\"%s\" target=\"%s\"> <text id=\"TextElement\" x=\"%.2f\" y=\"%.2f\" style=\"font-family:%s;font-size:%dpx\"> %s </text> </a>\n"
	link target x y font size s)
