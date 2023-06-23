class C {
	a : Int;
	b : Bool;
	init(x : Int, y : Bool) : C {
           {
		a <- x;
		b <- y;
		self;
		
           }
	};
	
	check(x: Int): Bool{
	    case x+10 of
		n : Object => true;    
		n1 : Object => true;    
		m : Int => false;
	    esac   
	};
	
	get(x : Int, x : Bool) : Int{ (*redefine of formals *)
	    {
	        init(1,1);
	        init(1,1,1,1);
	        iinit(1,true);
	    }
	};
};

class B {
	a : Int;
	b : Bool;
	initB(x : Int, y : Bool) : B {
           {
		a <- x;
		b <- y;
		true < 1;
		1 <= true;
		y + x;
		y - x;
		y * x;
		y / x;
		Not x;
		x = y;
		~y;
		a;
           }
	};
};

class Commands{
    
    d_op(s: Stack):Bool{
        let
            cur:C <- new C,
            no:B <- new C,
            flag:Bool <- false,
            self:Bool <- true
        in
            {
                while(cur) loop{
                    cur.init(1,true);
                }
                pool;
                self;
            }        
    };
    
    a():Fffff{
    {
		a <- x;
		b <- y;
		a;
           }
    };
};

Class Main {
	  flag:Bool;
	  a:Bool <- 1; (* check attr_typecheck *)
	  top:String <- "a";
	main():C {
	 {
	  (new C).init(1,1);
	  (new C).init(1,true,3);
	  (new C).iinit(1,true);
	  flag.initB(1,true);
	  (new C);
	  if(top) then
            self
            else
            {
                top <- "b";
                self;
            }
            fi;
	 }
	};
};


class M{
    initB(x : Int, y : Bool) : C{
        {
        if(x) then
            {(new M);}
            else
            {
                top <- "b";
                (new M);
            }
            fi;
        }
    }; 
};