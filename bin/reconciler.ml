open GL
open Brisk_reconciler

module Reconciler = 
  struct
    type hostElement =
      (* | DrawArea of GMisc.drawing_area *)
      | Container of GPack.box

    type node = hostElement
    module RemoteAction = Brisk_reconciler.RemoteAction
    
    let onStale = RemoteAction.create ()

    let insertNode ~parent(parent:node) ~child:(child:node) ~position:int =
      let _ = (match (parent, child) with
      | (Container(box), Container(child)) -> GPack.vbox ~packing:box#add()
      ) in
      parent

    let deleteNode ~parent(parent:node) ~child:(child:node) ~position:int = 
      let _ = (match (parent, child) with
      | (Container(box), Container(child)) -> GPack.vbox ~packing:box#add()
      ) in
      parent

    let moveNode ~parent  ~child:_  ~from:_  ~to_:_  = parent
    let markAsStale () = RemoteAction.send ~action:() onStale
    let beginChanges () = ()
    let commitChanges () = ()

  end

module LambdaReact = Brisk_reconciler.Make(Reconciler)