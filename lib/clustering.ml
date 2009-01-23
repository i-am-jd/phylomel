open Printf
open ExtLib
open Dendogram

(**
   Creation of UPGMA trees
*)

type tree = Dendogram.t
type leaves = Dendogram.leaf list

(* The leaves who come first in the list are the ones belonging to the
   subtrees with the greatest number of leaves. *)
let rec leaves_of_tree_acc acc = function
	| Leaf(l) -> l::acc
	| Node(n) ->
		  if leaves_nb n.t1 >= leaves_nb n.t2 then
			  let acc' = leaves_of_tree_acc acc n.t1 in
			  leaves_of_tree_acc acc' n.t2
		  else
			  let acc' = leaves_of_tree_acc acc n.t2 in
			  leaves_of_tree_acc acc' n.t1

let leaves_of_tree t =
	List.rev (leaves_of_tree_acc [] t)

(* UNPURE
   Fills the vertical position of each node in the tree.
   The position of the leaves are filled first.
   The position of a node is the mean of the positions
   of its two subtrees.
*)
let rec fill_tree_pos tree =
	let leaves = leaves_of_tree tree in

	(*Fills the positions of the leaves*)
	List.iteri (fun i leaf -> leaf.index <- i) leaves;

	(*Fills the position of the nodes*)
	let rec fill_node node = match node with
		| Leaf(l) ->
			float_of_int l.index
		| Node(n) ->
			let pos1 = fill_node n.t1 in
			let pos2 = fill_node n.t2 in
			n.pos <- (min pos1 pos2) +. (abs_float (pos2 -. pos1)) /. 2.;
			n.pos
	
	in ignore(fill_node tree);
	   leaves

let find_min_coords dynmat =
	let min_i = ref 0 in
	let min_j = ref 0 in
	let min_diff = ref infinity in
	let size = (DynArray.length dynmat) - 1 in
	for i=0 to size - 1 do
		for j=0 to i-1 do
			let found_diff = DynMat.get dynmat i j in
			if (found_diff <= !min_diff) then (
				min_diff := found_diff;
				min_i := i;
				min_j := j
			)
		done
	done;
	(!min_i, !min_j), !min_diff

(*
  Builds an upgma tree from a difference matrix
  see the wikipedia article on upgma
*)
let mk_upgma init_matrix genotype_size =

	let init_size = Array.length init_matrix in
	let matrix = DynMat.of_mat init_matrix in
	let nodes = DynArray.init init_size mk_leaf in
	
	while (DynArray.length nodes) > 1 do
		let min_a = ref 0 in
		let min_b = ref 0 in
		let min_diff = ref infinity in
		let size = DynArray.length matrix in
		
		for i=0 to size-1 do
			for j=0 to (i-1) do
				let found_diff = DynMat.get matrix i j in
				if (found_diff <= !min_diff) then (
					min_diff := found_diff;
					min_a := i;
					min_b := j
				)
			done
		done;
		(*
		  Doest not work, don't know why.
		let (min_a, min_b), min_diff = find_min_coords matrix in
		let size = DynArray.length matrix in
		*)
		
		(*0. Creates the new node from the 2 closest nodes*)
		let node_a = DynArray.get nodes !min_a in
		let node_b = DynArray.get nodes !min_b in
		let homology = homology !min_diff genotype_size in
		let new_node = mk_node node_a node_b homology in
		
		(*1. Generates the new line
		     (with the differences to the new node)*)
		let new_line = DynArray.create () in
		for j=0 to size-1 do
			if (j <> !min_a) && (j <> !min_b) then (
				let leaves_a = float_of_int (leaves_nb node_a) in
				let leaves_b = float_of_int (leaves_nb node_b) in
				let sum = leaves_a +. leaves_b in
				let diff_a = DynMat.get_diff matrix j !min_a in
				let diff_b = DynMat.get_diff matrix j !min_b in
				let new_diff = ( diff_a *. leaves_a +. diff_b *. leaves_b ) /. sum in
				
				DynArray.add new_line new_diff
			)
		done;
		
		(*2. Removes from the matrix the lines and columns corresponding to node_a and node_b*)
		DynMat.rem_col matrix !min_a;
		DynMat.rem_col matrix !min_b;
		DynArray.delete matrix !min_a;
		DynArray.delete matrix !min_b;
		
		(*3. Adds the new line*)
		DynArray.add matrix new_line;
		
		(*4. Removes the nodes node_a and node_b from the node dynarray*)
		DynArray.delete nodes !min_a;
		DynArray.delete nodes !min_b;
		
		(*5. Adds the new node to the node dynarray*)
		DynArray.add nodes (Node(new_node))
	done;

	(* Now the only element in the node dynarray is the tree *)
	let tree = DynArray.get nodes 0 in
	(*unpure, fills vertical positions of the nodes*)
	let leaves = fill_tree_pos tree in
	tree, leaves

let rec min_homology = function
	| Leaf(l) -> 100.
	| Node(n) ->
		min (min_homology n.t1) (min n.homology (min_homology n.t2))

type transform = float * float -> float * float

type link_info = string array * string

let write_genos out transform genos leaves links_target =
    let writeLeaf leaf =
		let genotype = genos.(leaf.value) in
		let geno_coords = transform (101., (float_of_int leaf.index)) in
		match links_target with
			| None ->
				  Svg.putText out genotype.Genotype.info geno_coords
			| Some(links,target) ->
				  let link = links.(leaf.value) in
				  Svg.putLink out genotype.Genotype.info ~link:link ~target:target geno_coords
    in List.iter writeLeaf leaves

(*
  SVG Header
  Supposes there is some javascript with an "Init" function further in the svg
*)		   
let header width height =
    sprintf "<?xml version=\"1.0\" standalone=\"no\"?>
            <svg width=\"%d\" height=\"%d\" version=\"1.1\"
            baseProfile=\"full\"
            xmlns=\"http://www.w3.org/2000/svg\"
            xmlns:xlink=\"http://www.w3.org/1999/xlink\"
            xmlns:ev=\"http://www.w3.org/2001/xml-events\"
            onload=\"Init( evt )\">\n"
	width height

(*
  Writing the svg
  Big ugly function that need documenting
*)
let write_svg_file genos links_target tree leaves svg_file =
    let height = int_of_float (50. +. float_of_int(leaves_nb tree) *. 25.) in
    let width = 1280 in
	let header = header width height in
	let svg = IO.output_channel (open_out svg_file) in

    
    let minH = min_homology tree in
    
    let trans (x,y) =
	(*( 9. +. 9. *. x,*)
	( 9. +. 9. *. (x -. minH) *. 100. /. (100. -. minH),
	  50. +. 25. *. y) in
    let transLegend (x,y) =  ( 9. +. 9. *. x, 10. +. 13. *. y) in
    let transText (x,y) = ( 9. +. 9. *. x, 54.5 +. 25. *. y) in
    
	(*let javascript =
		"//<![CDATA[\n\nvar svgRoot;\nvar toScale;\nvar toTranslate;\nvar myLine;\nvar myLegend;\n\nvar b_mouse_down = false;\nvar b_drag = false;\n\nvar last_pos = 909;\n\nfunction Init() {\n    svgRoot = document.documentElement;\n    toScale = document.getElementById(\"to_scale\");\n    toTranslate = document.getElementById(\"to_translate\");\n    myLine  = document.getElementById(\"line\");\n    myLegend = document.getElementById(\"legend\");\n}\n\nfunction getMouse(evt) {\n    var pos = svgRoot.createSVGPoint();\n    pos.x = evt.clientX;\n    pos.y = evt.clientY;\n    return pos;\n}\n\t\nfunction onMouseDown(evt) {\n    b_mouse_down = true;\n    if(isPointInVertLine(getMouse(evt), myLine)) {\n\tb_drag = true;\n    }\n}\n\t\nfunction onMouseMove(evt) {\n    if(b_drag) {\n\tdoUpdate(evt);\n    }\n}\n\t\nfunction onMouseUp(evt) {\n    var newP = getMouse(evt);\n    if(b_drag) {\n\ttoScale.setAttribute(\"transform\", \"matrix(\" + newP.x / 909 + \" 0 0 1 0 0)\");\n\ttoTranslate.setAttribute(\"transform\", \"translate(\" + (-(909 - newP.x)) + \" 0)\");\n\tfor (i=0; i<=10; i++) {\n\t    var text = document.getElementById(\"Legend\" + i);\n\t    x0 = text.getAttribute(\"x0\");\n\t    //text.setAttribute(\"x\",x0 * newP.x / 909);\n\t\ttext.setAttribute(\"x\",x0 * newP.x / last_pos);\n\t}\n    }\n    b_mouse_down = false;\n    b_drag = false;\n\tlast_pos = newP.x;\n}\n\t\nfunction doUpdate(evt) {\n    var newP = getMouse(evt);\n    myLine.setAttributeNS(null, \"x1\", newP.x );\n    myLine.setAttributeNS(null, \"x2\", newP.x );\n    \n}\n\t\nfunction isPointInVertLine(p,line) {\n    x = line.getAttribute(\"x1\");\n    return Math.abs(x - p.x) <= 4\n\t}\n\t\n// ]]></script>" in*)
	
	let javascript =
		"//<![CDATA[\n\nvar svgRoot;\nvar toScale;\nvar toTranslate;\nvar myLine;\nvar myLegend;\n\nvar b_mouse_down = false;\nvar b_drag = false;\n\nvar last_pos = 909;\n\nfunction Init() {\n    svgRoot = document.documentElement;\n    toScale = document.getElementById(\"to_scale\");\n    toTranslate = document.getElementById(\"to_translate\");\n    myLine  = document.getElementById(\"line\");\n    myLegend = document.getElementById(\"legend\");\n}\n\nfunction getMouse(evt) {\n    var pos = svgRoot.createSVGPoint();\n    pos.x = evt.clientX;\n    pos.y = evt.clientY;\n    return pos;\n}\n\t\nfunction onMouseDown(evt) {\n    b_mouse_down = true;\n    if(isPointInVertLine(getMouse(evt), myLine)) {\n\tb_drag = true;\n    }\n}\n\t\nfunction onMouseMove(evt) {\n    if(b_drag) {\n\tdoUpdate(evt);\n    }\n}\n\t\nfunction onMouseUp(evt) {\n    var newP = getMouse(evt);\n    if(b_drag) {\n\ttoScale.setAttribute(\"transform\", \"matrix(\" + newP.x / 909 + \" 0 0 1 0 0)\");\n\ttoTranslate.setAttribute(\"transform\", \"translate(\" + (-(909 - newP.x)) + \" 0)\");\n\tfor (i=0; i<=10; i++) {\n\t    var text = document.getElementById(\"Legend\" + i);\n\t    x0 = text.getAttribute(\"x0\");\n\t    //text.setAttribute(\"x\",x0 * newP.x / 909);\n\t\torig_pos = 9 + 9 * ((i*10) - 0.7);\n\t\ttext.setAttribute(\"x\",orig_pos * newP.x / 909);\n\t}\n    }\n    b_mouse_down = false;\n    b_drag = false;\n}\n\t\nfunction doUpdate(evt) {\n    var newP = getMouse(evt);\n    myLine.setAttributeNS(null, \"x1\", newP.x );\n    myLine.setAttributeNS(null, \"x2\", newP.x );\n    \n}\n\t\nfunction isPointInVertLine(p,line) {\n    x = line.getAttribute(\"x1\");\n    return Math.abs(x - p.x) <= 4\n\t}\n\t\n// ]]></script>" in
	
	Svg.put svg header;
	Svg.put svg "<script type=\"text/ecmascript\n\">";
    Svg.put svg javascript;
	
    (*SCALE GROUP*)
	
    Svg.put svg "<g id=\"to_scale\" transform=\"matrix(1 0 0 1 0 0)\">\n";

    (*Scale line*)
    Svg.putLine svg (transLegend (-.0.1,1.)) (transLegend (100.1,1.));
	
    (*Tree*)
    let rec write_aux node = match node with
	| Leaf(leaf) ->	()
	      
	| Node(node)->
	      let pos1 = get_pos node.t1 in
	      let pos2 = get_pos node.t2 in
	      let x = node.homology in
	      let x1 = get_homology node.t1 in
	      let x2 = get_homology node.t2 in
	      Svg.put3Lines svg (trans (x1,pos1)) (trans (x, pos1)) (trans (x, pos2)) (trans (x2, pos2));
	      
	      write_aux node.t1;
	      write_aux node.t2
    in write_aux tree;

    Svg.put svg "</g>\n";

    (*OTHERS*)
    
    (*10 20 .. 100*)
    for i=0 to 10 do
		(*let (x,y) = transLegend ((float_of_int (i*10))-.0.7, 2.) in*)
		let (x,y) = transLegend ((float_of_int (i*10)), 2.) in
		Svg.put svg
			(sprintf
				 "<text id=\"Legend%d\" x=\"%f\" y=\"%f\" x0=\"%f\" style=\"font-family:Arial;font-size:10px;text-anchor:middle\"> %d </text>"
				 i x y x (i*10))
			(*Svg.putText svg (sprintf "%d" (i*10)) (transLegend (x-.0.7,2.))*)
    done;

    (*Title*)
    Svg.putText svg "Homology percentage" ~size:12 (transLegend (5.,0.5));

    (*Draggable line*)
    Svg.put svg
	(sprintf
	     "<line id=\"line\" x1=\"909\" y1=\"23\" x2=\"909\" y2=\"%d\" style=\"stroke:black;stroke-width:1\" opacity=\"0.5\" />\n"
	     height);
	
    (* Canvas
	   big clickable area for interaction *)
    Svg.put svg 
	(sprintf 
	     "<rect id=\"canvas\" x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" opacity=\"0\"
		pointer-events=\"visible\"
		onmousedown=\"onMouseDown(evt)\"
		onmousemove=\"onMouseMove(evt)\"
		onmouseup=\"onMouseUp(evt)\"/>\n"
	     width height);

    (*GROUP TO TRANSLATE*)
    Svg.put svg "<g id=\"to_translate\" transform=\"translate(0 0)\">\n";
    
    (*Genotype names*)
    write_genos svg transText genos leaves links_target;
    
    (*Lines*)
    for i=0 to 20 do
	let x = float_of_int (i*5) in
	Svg.putLine svg (transLegend (x,1.)) (transLegend (x,1.2))
    done;
    
    Svg.put svg "</g>\n";
    
    Svg.close svg;
    width, height

