(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)
 class Listnode{
   value:String;
   next:Listnode;
   gen_node(val:String,pre_top:Listnode):Listnode{
      {
         value<-val;
         next<-pre_top;
         self;
      }
   };
   get_next():Listnode{next};
   get_value():String{value};
};

class Stack inherits IO{
   top:Listnode;
   push(val:String):Stack{
      {
         top<-(new Listnode).gen_node(val,top);
         self;
      }
   };
   pop():Listnode{
      let
         pop_node:Listnode
      in
         if(isvoid top)then
         {
            out_string("The stack is empty!");
            top;
         }
         else
         {
            pop_node<-top;
            top<-top.get_next();
            pop_node;
         }
         fi
   };
   get_top():Listnode{top};
};

class Command inherits IO{
   command_d(stack:Stack):Object{
       let 
           current:Listnode<-stack.get_top()
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

   command_e(stack:Stack):Object{
       let
           opt:Listnode<-stack.get_top(),opt_value1:String,opt_value2:String,to_num:A2I<-new A2I
       in
       {
           if(isvoid opt)then
           {
               out_string("e command error: The stack is empty!");
               self;
           }
           else
           {   
               if(opt.get_value()="+")then
               {
                   stack.pop();
                   opt_value1<-stack.pop().get_value();
                   opt_value2<-stack.pop().get_value();
                   stack.push(to_num.i2a(to_num.a2i(opt_value1)+to_num.a2i(opt_value2)));
               }
               else 
                   if(opt.get_value()="s")then
                   {
                       stack.pop();
                       opt_value1<-stack.pop().get_value();
                       opt_value2<-stack.pop().get_value();
                       stack.push(opt_value1);
                       stack.push(opt_value2);
                   }
                   else
                       self
                   fi
               fi;
           }
           fi;
       }
   };
};


class A inherits Listnode{
    test():Object{
    self
    };
};

class Main inherits IO {
   stk:Stack<-new Stack;
   command:Command<-new Command;
   flag:Bool;
   a:Int;
   b:Int;
   c:Int;
   cmd:String;
   test:A<-new A;
   testb:Listnode<-new Listnode;
   main() : Object {
       {
           a<-5;
           b<-6;
           cmd<-"Hello World!";
           c<-a/5;
           c<-a*b;
           c<-a-b;
           flag<-true;
           flag<-~flag;
           while(flag)loop{
           out_string(">");
           cmd<-in_string();
           if(cmd="x")then
               flag<-false
           else
               if(cmd="d")then
                   command.command_d(stk)
               else
                   if(cmd="e")then
                       command.command_e(stk)
                   else
                       stk.push(cmd)
                   fi
               fi
           fi;
           }
           pool;
           case test.test() of
             testb:Listnode => test.test();
           esac;
       }
   };
};
