class c = object
end

(*
class c = object (_ as selfpat-* as selfpat-1)  end
*)

class c1 = object (_)
end

class c2 = object (self)
end

class c3 = object (self as self')
end
