
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

 class A{
    a:Int<-5;
 };
 class B inherits A{
    b:Int<-6;
 };
 class C inherits B{
    c:Int<-7;
 };
 class D inherits C{
    d:Int<-8;
 };
  class Main {
   dc:D<-new D;
   x:Int<-1;
   y:Int<-2;
    main():Object {
       case dc of 
        a:A=>x+y;
        b:B=>x-y;
        c:C=>x+y;
       esac
      };
  
  };
  
  