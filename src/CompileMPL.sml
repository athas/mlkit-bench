structure CompileMPL : COMPILE =
  struct
    fun compile {env:(string*string)list,flags:string, src:string} =
      let val {base,ext} = OS.Path.splitBaseExt src
	  val target = base ^ "_mpl.exe"
	  fun comp src =
	    let val cmd = "mpl -output " ^ target ^ " " ^ flags ^ " " ^ src
                val env = String.concatWith " " (map (fn (k,v) => k ^ "=" ^ v) env)
                val cmd = env ^ " " ^ cmd
	    in  print ("Executing: '" ^ cmd ^ "'\n")
	      ; if OS.Process.isSuccess(OS.Process.system cmd) then SOME target
		else NONE
	    end
      in case ext
	   of SOME "sml" => comp src
	    | SOME "mlb" => comp src
	    | _ => NONE
      end

    fun version () =
        FileUtil.trimWS(FileUtil.systemOut "mpl")
        handle X => ("Failed to extract mpl version; " ^ exnMessage X)
  end
