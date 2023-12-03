
-define (tb_var, tb_var).

-define (assert(P, E), case P of true -> ok; _ -> throw({error, E}) end).

-define (assertmatch(Exp, P),
         begin
           put(?tb_var, Exp),
           case get(?tb_var) of
             P -> get(?tb_var);
             _ -> throw ({error, get(?tb_var)})
           end
         end
         ).

-define (assertmatch(Exp, P, Comment),
         begin
           put(?tb_var, Exp),
           case get(?tb_var) of
             P -> get(?tb_var);
             _ -> throw ({error, {Comment, get(?tb_var)}})
           end
         end
         ).

-define (assertnotmatch(Exp, P),
         begin
           put(?tb_var, Exp),
           case get(?tb_var) of
             P -> throw ({error, get(?tb_var)});
             _ -> get(?tb_var)
           end
         end
         ).

-define (assertnotmatch(Exp, P, Comment),
         begin
           put(?tb_var, Exp),
           case get(?tb_var) of
             P -> throw ({error, {Comment, get(?tb_var)}});
             _ -> get(?tb_var)
           end
         end
         ).