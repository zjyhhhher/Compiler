
(*
 *  execute "coolc bad.cl" to see the error messages that the coolc parser
 *  generates
 *
 *  execute "myparser bad.cl" to see the error messages that your parser
 *  generates
 *)

(* no error *)
class A {
};

(* error:  b is not a type identifier *)
Class b inherits A {
};

(* error:  a is not a type identifier *)
Class C inherits a {
};

(* error:  keyword inherits is misspelled *)
Class D inherts A {
};


(* error:  closing brace is missing *)
Class E inherits A {
;

(* error: feature missing type *)
(* error: error of the expression in the block *)
class Main inherits IO {
   stk:Stack<--new Stack;
   command:Command<-new Command;
   flag:;
   a:kkk;
   !B:Int;
   d::Int;
   cmd:String;
   main() : Object {
       {
           a<--5;
           b<-6;
           cmd<-"Hello World!";
           c<-a/5;
           c<-a**b;
           c<-a-b--;//error in expression
           flag<-true;
           flag<-~flag;
           while(++flag)loop{//error in while expression
           out_string(">");
           cmd<--in_string();
           if(cmd="x")then
               flag<-false
           else
               if(cmd==="d")then//error if
                   command.command_d(stk)
               else
                   if(cmd="e")then
                       command.command_e(stk)
                   else
                       stk.push(cmd)
                   //missing fi
               fi
           fi;
           }
           pool;
       }
   };
};

(* error let binding *)
class Command inherits IO{
   command_d(stack:Stack):Object{
       let 
           current:Listnode<--stack.get_top(), a:IInt<--6, b:Int<-5
       in
           {
               while(not isvoid current)loop{
                   out_string(current.get_value().concat("\n"));
                   current<-current.get_next();
               }
               pool;
               self;
           }
   };
};
