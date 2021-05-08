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
};

class Foo inherits Bar {};
class Bar inherits Bat {
      a: Str;
};
class Bat {
      a: Int <- 10;
      b: Str <- "Hello world";
      c: Bool <- False;
      (if c then a else b);
};

Class Main {
	main():C {
	 {
	  (new C).init(1,1);
	  (new C).init(1,true,3);
	  (new C).iinit(1,true);
	  (new C);
	 }
	};
};
