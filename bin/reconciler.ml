open GL

module RemoteAction = Brisk_reconciler.RemoteAction

open Brisk_reconciler

type hostElement =
  | DrawArea of GMisc.drawing_area
  | Container of GPack.box

type node = hostElement

let onStale = RemoteAction.create ()

let insertNode ~parent ~child ~position:int =
  let _ = (match (parent, child) with
  | (Container(box), Container(child)) -> let _ = GPack.vbox ~packing:box#add () in ()
  | (Container(box), DrawArea(child)) -> let _ = GMisc.drawing_area ~packing:box#add () in ()
  | _ -> ()
  ) in
  parent

let deleteNode ~parent ~child ~position:int = 
  let _ = (match (parent, child) with
  | (Container(box), Container(child)) -> let _ = GPack.vbox ~packing:box#add () in ()
  | _ -> ()
  ) in
  parent

let moveNode ~parent  ~child:_  ~from:_  ~to_:_  = parent
let markAsStale () = RemoteAction.send ~action:() onStale
let beginChanges () = ()
let commitChanges () = ()



let%nativeComponent vbox ~children () hooks = 
  let window = GWindow.window ~width:800 ~height:600 ~title:"Simple lablgtk program" () in ({
    make = (fun () -> let node = GPack.vbox ~packing:window#add () in Container(node));
    configureInstance = (fun ~isFirstRender -> fun node  -> node);
    children;
    insertNode;
    deleteNode;
    moveNode;
  }, hooks)
