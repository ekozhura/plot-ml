open Printf
open Plplot
open Cairo
open GLFW

let simple_example () =
  let xs = Array.init 21 (fun xi -> float xi -. 10.0) in
  let ys = Array.map (fun x -> x**2.0) xs in
  plinit ();
  plenv (-10.0) 10.0 0.0 100.0 0 0;
  plline xs ys;
  plend ();
  ()
  
let draw_surface () = 
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w:120 ~h:120 in
  let cr = Cairo.create surface in
  (* Examples are in 1.0 x 1.0 coordinate space *)
  Cairo.scale cr 120. 120.;

  (* Drawing code goes here *)
  Cairo.set_line_width cr 0.1;
  Cairo.set_source_rgb cr 0. 0. 0.;
  Cairo.rectangle cr 0.25 0.25 ~w:0.5 ~h:0.5;
  Cairo.stroke cr;
  Cairo.PNG.write surface "stroke.png"

(* let () = simple_example ();  
  draw_surface () *)

let processKey window = 
  if GLFW.getKey ~window:window ~key:Space
  then GLFW.setWindowShouldClose window true

let () =
  GLFW.init ();
  at_exit GLFW.terminate;
  let window = GLFW.createWindow 800 600 "Hello World" () in
  GLFW.windowHint ~hint:ContextVersionMajor ~value:3;
  GLFW.windowHint ~hint:ContextVersionMinor ~value:3;
  GLFW.windowHint ~hint:OpenGLProfile ~value:CoreProfile;

  GLFW.makeContextCurrent (Some window);

  while not (GLFW.windowShouldClose window) do
    processKey(window);
    GLFW.swapBuffers window;
    GLFW.pollEvents ()
  done

