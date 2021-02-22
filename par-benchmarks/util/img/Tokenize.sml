(* Adapted from the MPL examples: https://github.com/MPLLang/mpl/tree/master/examples *)

structure Tokenize:
sig
  val tokenRanges: (char -> bool) -> char Array.array -> int * (int -> (int * int))

  val tokensSeq: (char -> bool) -> char Array.array -> (char ArraySlice.slice) Array.array

  val tokens: (char -> bool) -> char Array.array -> string Array.array
end =
struct
  structure A = Array
  structure AS = ArraySlice

  fun tokenRanges f s =
    let
      val n = A.length s
      fun check i =
        if (i = n) then not (f(A.sub(s,n-1)))
        else if (i = 0) then not (f(A.sub(s,0)))
        else let val i1 = f (A.sub(s,i))
                 val i2 = f (A.sub(s,i-1))
             in (i1 andalso not i2) orelse (i2 andalso not i1) end
      val ids =
          A.fromList (List.filter check (List.tabulate(n+1, fn i => i)))
      val count = (A.length ids) div 2
    in
      (count, fn i => (A.sub(ids,2*i), A.sub(ids,2*i+1)))
    end

  fun tokensSeq f s =
    let
      val (n, g) = tokenRanges f s
      fun token i =
        let
          val (lo, hi) = g i
        in
          AS.slice (s, lo, SOME (hi-lo))
        end
    in
      A.tabulate (n, token)
    end

  fun tokens f s =
    let
      val (n, g) = tokenRanges f s
      fun token i =
        let
          val (lo, hi) = g i
          val chars = AS.slice (s, lo, SOME (hi-lo))
        in
          CharVector.tabulate (AS.length chars, fn i => AS.sub(chars,i))
        end
    in
      A.tabulate (n, token)
    end
end
