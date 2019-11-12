open Plplot
open Cairo
open GLFW
open GL

(* open Reconciler *)

(* external plset_cairo_context : Cairo.context -> unit = "ml_set_plplot_cairo_context" *)

let simple_example context =
  let xs = Array.init 21 (fun xi -> float xi -. 10.0) in
  let ys = Array.map (fun x -> x**2.0) xs in
  plinit ();
  (* plset_cairo_context context; *)
  plenv (-10.0) 10.0 0.0 100.0 0 0;
  plline xs ys;
  plend ();
  ()
  
let draw_surface () = 
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w:120 ~h:120 in
  let cr = Cairo.create surface in
  simple_example cr;  
  (* Examples are in 1.0 x 1.0 coordinate space
  Cairo.scale cr 120. 120.;


  (* Drawing code goes here *)
  Cairo.set_line_width cr 0.1;
  Cairo.set_source_rgb cr 1. 1. 1.;
  Cairo.rectangle cr 0.25 0.25 ~w:0.5 ~h:0.5;
  Cairo.stroke cr; *)
  let texid = glGenTexture() in
  glBindTexture BindTex.GL_TEXTURE_2D texid;
  let tex_w = 120 in
  let tex_h = 120 in
  let data = Bigarray.genarray_of_array1 (Cairo.Image.get_data8 surface) in
  glTexParameter TexParam.GL_TEXTURE_2D  (TexParam.GL_TEXTURE_MAG_FILTER  Mag.GL_NEAREST);
  glTexParameter TexParam.GL_TEXTURE_2D  (TexParam.GL_TEXTURE_MIN_FILTER  Min.GL_NEAREST);
  glTexImage2D 
    ~target:TexTarget.GL_TEXTURE_2D 
    ~level:0 
    ~internal_format:GL_RGBA
    ~width:tex_w 
    ~height:tex_h 
    ~format_:GL_RGBA
    ~type_:GL_UNSIGNED_BYTE 
    ~pixels:data;
  glEnable GL_TEXTURE_2D; 
  glBegin GL_QUADS;   
  glNormal3 (0.0) (0.0) (1.0);                          (* front face points out of the screen on z. *)
  glTexCoord2 0.0 0.0; glVertex3 (-1.0) (-1.0) ( 1.0);  (* Bottom Left Of The Texture and Quad *)
  glTexCoord2 1.0 0.0; glVertex3 ( 1.0) (-1.0) ( 1.0);  (* Bottom Right Of The Texture and Quad *)
  glTexCoord2 1.0 1.0; glVertex3 ( 1.0) ( 1.0) ( 1.0);  (* Top Right Of The Texture and Quad *)
  glTexCoord2 0.0 1.0; glVertex3 (-1.0) ( 1.0) ( 1.0);  (* Top Left Of The Texture and Quad *)


  glEnd();  
  ()

let processKey window = 
  if GLFW.getKey ~window:window ~key:Escape
  then GLFW.setWindowShouldClose window true

(* let () =
  GLFW.init ();
  at_exit GLFW.terminate;
  let window = GLFW.createWindow 800 600 "Hello World" () in
  GLFW.windowHint ~hint:ContextVersionMajor ~value:3;
  GLFW.windowHint ~hint:ContextVersionMinor ~value:3;
  GLFW.windowHint ~hint:OpenGLProfile ~value:CoreProfile;
  
  GLFW.makeContextCurrent (Some window);
  glViewport 0 0 800 600;
  glClearColor 0.2 0.3 0.3 1.0;
  

  while not (GLFW.windowShouldClose window) do
    processKey (window);
    
    glClear [GL_COLOR_BUFFER_BIT];
    draw_surface ();
    GLFW.swapBuffers window;
    GLFW.pollEvents ()
  done *)

open GMain
open GdkKeysyms

let pi2 = 8. *. atan 1.

let draw cr width height =
  let r = 0.25 *. width in
  set_source_rgba cr 0. 1. 0. 0.5;
  arc cr (0.5 *. width) (0.35 *. height) ~r ~a1:0. ~a2:pi2;
  fill cr;
  set_source_rgba cr 1. 0. 0. 0.5;
  arc cr (0.35 *. width) (0.65 *. height) ~r ~a1:0. ~a2:pi2;
  fill cr;
  set_source_rgba cr 0. 0. 1. 0.5;
  arc cr (0.65 *. width) (0.65 *. height) ~r ~a1:0. ~a2:pi2;
  fill cr;
;;

let expose drawing_area cr =
  let allocation = drawing_area#misc#allocation in
  draw cr (float allocation.Gtk.width) (float allocation.Gtk.height);
  true

let _ = GtkMain.Main.init ()

let main () =
  let window = GWindow.window ~width:800 ~height:600 ~title:"Simple lablgtk program" () in
  let vbox = GPack.vbox ~packing:window#add () in
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in
  let factory = new GMenu.factory file_menu ~accel_group in
  let _ = factory#add_separator () in
  let _ = factory#add_item "Quit" ~key:_Q ~callback:Main.quit in
  let _ = window#connect#destroy ~callback:Main.quit in
  window#show ();
  let d = GMisc.drawing_area ~packing:vbox#add () in
  ignore(d#misc#connect#draw ~callback:(expose d));
  Main.main ()

let () = main ()