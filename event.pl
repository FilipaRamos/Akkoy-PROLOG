 :- use_module(library(tcltk)).

 setup :-
         tk_new([name('SICStus+Tcl/Tk - Queens')], Tcl),
         tcl_eval(Tcl, 'source queens.tcl', _),
         tk_next_event(Tcl, Event),
         (   Event = next -> go(Tcl),
         ;   closedown(Tcl)
         ).
     
     closedown(Tcl) :-
         tcl_delete(Tcl).
     
     go(Tcl) :-
         tcl_eval(Tcl, 'clear_board', _),
         queens(8, Qs),
         show_solution(Qs, Tcl),
         tk_next_event(Tcl, Event),
         (   Event = next -> fail
         ;   closedown(Tcl)
         ).
     go(Tcl) :-
         tcl_eval(Tcl, 'disable_next', _),
         tcl_eval(Tcl, 'clear_board', _),
         tk_next_event(Tcl, _Event),
         closedown(Tcl).

button .next -text next -command {prolog_event  next}
     pack .next
     
     button .stop -text stop -command {prolog_event stop}
     pack .stop