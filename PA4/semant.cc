
#include <cstdlib>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <type_traits>
#include <vector>
#include <unordered_map>
#include <functional>
#include <algorithm>
#include <queue>
#include <unordered_set>
#include "cool-tree.h"
#include "cool-tree.handcode.h"
#include "semant.h"
#include "utilities.h"

extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}

std::optional<std::pair<Formals,Symbol>> Enviro::getFuncSig(Symbol class_name,Symbol method_name){
    Symbol cur_class=class_name==SELF_TYPE?cur_class_:class_name;
    while(cur_class!=No_class){
        auto find_res1=method_signatures_.find(cur_class);
        if(find_res1==method_signatures_.end()){
            cur_class=inherit_graph_[cur_class];
            continue;
            //如果找不到这个类key，有可能类是子类，没有实现函数，所以要去父类里面找
        }
        auto find_res2=find_res1->second.find(method_name);
        if(find_res2==find_res1->second.end()){
            cur_class=inherit_graph_[cur_class];
            continue;
            //如果这个类里面找不到method，去父类里面找
        }
        return find_res2->second;
    }
    return {};
}

//检查class1是不是class2的子类
bool Enviro::checkSubClass(Symbol class1,Symbol class2){
    class1=class1==SELF_TYPE?cur_class_:class1;
    class2=class2==SELF_TYPE?cur_class_:class2;
    //why???No_type可能就是没有继承
    if(class1==No_type)return true;
    Symbol cur=class1;
    while(cur!=class2&&cur!=Object){
        cur=inherit_graph_[cur];
    }
    if(cur==class2)return true;
    else return false;
}

//找最小公共祖先
Symbol Enviro::getLca(const std::vector<Symbol>& classes){
    const int n=classes.size();
    std::vector<std::vector<Symbol>> paths(n);
    for(int i=0;i<n;i++){
        Symbol cur=classes[i]==SELF_TYPE?cur_class_:classes[i];
        while(cur!=Object){
            paths[i].push_back(cur);
            cur=inherit_graph_[cur];
        }
    }
    for(auto& x:paths){
        std::reverse(x.begin(),x.end());
    }
    Symbol ans=Object;
    for(int i=0;i<paths[0].size();++i){
        bool flag=true;
        Symbol cur=paths[0][i];
        for(int j=0;j<paths.size();j++){
            if(paths[j].size()<=i||paths[j][i]!=cur){
                flag=false;
                break;
            }
        }
        if(!flag)break;
        else ans=cur;
    }
    if(semant_debug){
        std::cerr<<"final lca!"<<std::endl;
        for(auto x:classes){
            std::cerr<<x<<"|";
        }
        std::cout<<std::endl;
        std::cout<<ans<<std::endl;
    }
    return ans;
}

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr), classes_(classes) {

    /* Fill this in */
    install_basic_classes();
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);
    graph_[Object]=No_class;
    graph_[IO]=Object;
    graph_[Int]=Object;
    graph_[Bool]=Object;
    graph_[Str]=Object;
    symbol_to_class_[Object]=dynamic_cast<class__class*>(Object_class);
    symbol_to_class_[IO]=dynamic_cast<class__class*>(IO_class);
    symbol_to_class_[Int]=dynamic_cast<class__class*>(Int_class);
    symbol_to_class_[Bool]=dynamic_cast<class__class*>(Bool_class);
    symbol_to_class_[Str]=dynamic_cast<class__class*>(Str_class);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 



/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */

void ClassTable::check_phase1(){
    if(classes_==nullptr){
        return;
    }
    install_user_classes();
    check_cycle();
    check_main();
}

void ClassTable::install_user_classes(){
    bool check=false;

    //检查基本类的重定义
    for(int i=classes_->first();classes_->more(i);i=classes_->next(i)){
        auto cur_class=dynamic_cast<class__class*>(classes_->nth(i));
        Symbol cur_class_name=cur_class->get_name();
        Symbol cur_class_parent=cur_class->get_parent();

        if(cur_class_name==SELF_TYPE||
           cur_class_name==Object||
           cur_class_name==IO||
           cur_class_name==Int||
           cur_class_name==Bool||
           cur_class_name==Str){
            semant_error(classes_->nth(i))<<"Redefinition of basic class "<<cur_class_name<<"."<<std::endl;
            check=true;
        }
    }

    //将userClass加入map中，并且检查重定义
    //unorderedmap的key是className，value是parentName
    for(int i=classes_->first();classes_->more(i);i=classes_->next(i)){
        auto cur_class = dynamic_cast<class__class*>(classes_->nth(i));
        Symbol cur_class_name=cur_class->get_name();
        Symbol cur_class_parent=cur_class->get_parent();
        if(graph_.find(cur_class_name)!=graph_.end() && 
           cur_class_name!=SELF_TYPE && 
           cur_class_name!=Object &&
           cur_class_name!=IO &&
           cur_class_name!=Int &&
           cur_class_name!=Bool &&
           cur_class_name!=Str){//如果不把这几个基本类单独区分，当重定义基本类的时候，就会Redifinition和这个一起报错
            semant_error(classes_->nth(i))<<"Class "<<cur_class_name<<" was previously defined." <<std::endl;
            check=true;
            //abort();
        }
        graph_[cur_class_name]=cur_class_parent;
    }

    //检查基本类有没有被继承
    for(int i=classes_->first();classes_->more(i);i=classes_->next(i)){
        auto cur_class=dynamic_cast<class__class*>(classes_->nth(i));
        Symbol cur_class_name=cur_class->get_name();
        Symbol cur_class_parent=cur_class->get_parent();
        if(cur_class_parent==SELF_TYPE||
           cur_class_parent==Str||
           cur_class_parent==Int||
           cur_class_parent==Bool){
            semant_error(classes_->nth(i))<<"Class "<<cur_class_name<<" cannot inherit class "\
            <<cur_class_parent<<"."<<std::endl;
            check=true;
            //abort();
        }
    }

    
     //检查父类是否定义
    for(int i=classes_->first();classes_->more(i);i=classes_->next(i)){
        auto cur_class = dynamic_cast<class__class*>(classes_->nth(i));
        Symbol cur_class_name = cur_class->get_name();
        Symbol cur_class_parent = cur_class->get_parent();
        if (cur_class_name == Object) continue;//Object没有父类
        if (graph_.find(cur_class_parent) == graph_.end()) {
            semant_error(classes_->nth(i)) << "Class "<< cur_class_name << " inherits from an undefined class "
                << cur_class_parent << "." << std::endl;
            check=true;
            //abort();
        }
    }
    
    if(check)abort();
    
    //将类的Symbol和实际的类对应起来
    for(int i=classes_->first();classes_->more(i);i=classes_->next(i)){
        auto cur_class=dynamic_cast<class__class*>(classes_->nth(i));
        symbol_to_class_[cur_class->get_name()]=cur_class;
    }
}

//拓扑排序检测是否有环
void ClassTable::check_cycle(){
    int all_class_num=graph_.size()+1;
    std::unordered_map<Symbol,int> degrees;
    std::vector<Symbol> alreadyOut;
    for(auto& [k,v]:graph_){
        degrees[v]++;
    }
    std::queue<Symbol> qe;
    for(auto& [k,v]:graph_){
        if(degrees.find(k)==degrees.end())qe.push(k);
    }

    int pop_class_num=0;
    while(!qe.empty()){
        auto cur=qe.front();
        qe.pop();
        pop_class_num++;
        if(cur==No_class)continue;
        degrees[graph_[cur]]--;
        if(degrees[graph_[cur]]==0)qe.push(graph_[cur]);
    }//弹出入度为0的节点，寻找新入度变成0的节点加入队列
    if(pop_class_num!=all_class_num){
        bool check;
       for(auto& [k,v]:degrees){
            check=false;
            if(k==Object || k==No_class)continue;
            if(v==0)continue;
            for(Symbol& al:alreadyOut){
                if(graph_[k]==al)
                {
                    semant_error(symbol_to_class_[k])<<"Class "<<k<<", or an ancestor of "
                    <<k<<", is involved in an inheritance cycle."<<std::endl;
                    semant_error(symbol_to_class_[al])<<"Class "<<al<<", or an ancestor of "
                    <<al<<", is involved in an inheritance cycle."<<std::endl;
                    check=true;
                }
            }
            if(check)continue;
            alreadyOut.push_back(k);
        }
        //如何获取成环的类名字
        abort();
    }
}

void ClassTable::check_main(){
    //检查是否包含Main类
    bool contain_main=false;
    for(int i=classes_->first();classes_->more(i);i=classes_->next(i)){
        auto cur_class=dynamic_cast<class__class*>(classes_->nth(i));
        Symbol cur_class_name=cur_class->get_name();
        if(cur_class_name==Main){
            contain_main=true;
            break;
        }
    }
    if(!contain_main){
        semant_error()<<"Class Main is not defined."<<std::endl;
        //abort();
    }
}

void ClassTable::buildGraphTopDown(){
    for(auto& [k,v]:graph_){
        if(k==Object)continue;
        graph_rev_[v].push_back(k);
    }
}

void ClassTable::collectAndCheckAllMethod(){
    SymTable<Symbol,std::pair<Formals,Symbol>> func_table;
    SymTable<Symbol,Symbol> attr_table;

    std::function<void(Symbol)> dfs=[&](Symbol cur_class){
        //每调用一遍dfs，进入一个新的作用域
        func_table.enterScope();
        attr_table.enterScope();
        class__class* t=symbol_to_class_[cur_class];
        Features features=t->get_features();
        for(int i=features->first();features->more(i);i=features->next(i)){
            if(features->nth(i)->getType()==FeatureType::METHOD_){
                method_class* cur_method=dynamic_cast<method_class*>(features->nth(i));
                Symbol method_name=cur_method->getName();
                Formals formals=cur_method->getFormals();
                Symbol ret_type=cur_method->getRetType();
                auto find_res=func_table.probe(method_name);
                if(find_res){
                    //如果不为空，说明method名字重复了
                    semant_error(getClass(cur_class).value()->get_filename(),cur_method)<<"define same method in this class"<<std::endl;
                    abort();
                }
                find_res=func_table.lookUp(method_name);
                if(find_res){
                    //找到了父类中的method
                    Formals old_formals=find_res.value().first;
                    Symbol old_ret_type=find_res.value().second;
                    if(old_formals->len()!=formals->len()){
                        semant_error(getClass(cur_class).value()->get_filename(),cur_method)<<"override with different arg number"<<std::endl;
                        abort();
                    }//如果formals的个数不一样
                    //参数个数一样，检查type
                    for(int j=0;j<old_formals->len();++j){
                        Formal old_formal=old_formals->nth(j);
                        Formal cur_formal=formals->nth(j);
                        if(!old_formal->equal(cur_formal)){
                            semant_error(getClass(cur_class).value()->get_filename(),cur_method)<<"override with different arg type"<<std::endl;
                            abort();
                        }
                    }
                    //检查返回值的类型
                    if(old_ret_type!=ret_type){
                        semant_error(getClass(cur_class).value()->get_filename(),cur_method)<<"override with different return type"<<std::endl;
                        abort();
                    }
                }else{
                    //父类都没有这个method
                    for(int j=formals->first();formals->more(j);j=formals->next(j)){
                        Formal cur_formal=formals->nth(j);
                        if(symbol_to_class_.find(cur_formal->getTypeDecl())==symbol_to_class_.end()){
                            semant_error(getClass(cur_class).value()->get_filename(), cur_method) << "cannot find the arg type: " << cur_formal->getTypeDecl() << std::endl;
                        }
                        //检查函数参数的类型是否正确
                    }
                    if(ret_type!=SELF_TYPE&&symbol_to_class_.find(ret_type) == symbol_to_class_.end()){
                        semant_error(getClass(cur_class).value()->get_filename(), cur_method) << "cannot find the the ret type: " << ret_type << std::endl;
                    }//检查返回值类型是否正确
                }
                for(int j=0;j<formals->len();j++){
                    Formal cur_formal=formals->nth(j);
                    if(semant_debug){
                        std::cerr<<cur_formal->getName()<<std::endl;
                    }
                    if(cur_formal->getName()==self){
                        semant_error(getClass(cur_class).value()->get_filename(), cur_method) << "formal arg cannot be self" << std::endl;
                    }
                }
                //检查都通过之后，将函数加到Envrio里面
                env_.addFuncSig(cur_class,method_name,formals,ret_type);
                //如果一个class里面没有method，那么在Env里面就没有这个className的key
                func_table.add(method_name,{formals,ret_type});
                if (semant_debug){
                    std::cerr << "Insert Func to Env: " << cur_class << "::" << method_name << std::endl;  
                }
            }else{
                attr_class* attr=dynamic_cast<attr_class*>(features->nth(i));
                if(attr->getName()==self){
                    semant_error(symbol_to_class_[cur_class]) << "\'self\' cannot be the name of attribute." << std::endl;
                    abort();
                }
                if(attr_table.lookUp(attr->getName())){
                    semant_error(symbol_to_class_[cur_class]) << "Attribute " << attr->getName() << " is an attribute of an inherited class." << std::endl;
                    abort();
                }
                attr_table.add(attr->getName(),attr->get_decl_type());
            }
        }
        for(auto child:graph_rev_[cur_class]){
            dfs(child);
        }
        func_table.exitScope();
        attr_table.exitScope();
    };
    dfs(Object);
}

void ClassTable::check_phase2(){
    env_=Enviro(graph_);
    buildGraphTopDown();
    if(semant_debug){
        std::cerr<<"build graph top to down end!"<<std::endl;
    }
    collectAndCheckAllMethod();
    if (semant_debug) {
        std::cerr << "Collect and check method end!" << std::endl;
    }
    getClass(Object).value()->typeCheck(*this,env_);
}

/***************************************接下来是类型检查************************************************/
void class__class::typeCheck(ClassTable& class_table,Enviro& env){
    env.newScope();
    env.setCurClass(name);
    env.addVar(self,name);//先把self加入到scope里面
    for(int i=features->first();features->more(i);i=features->next(i)){
        if(features->nth(i)->getType()==FeatureType::ATTR_){
            attr_class* cur_attr=dynamic_cast<attr_class*>(features->nth(i));
            env.addVar(cur_attr->getName(),cur_attr->getDeclType());
        }
    }//将每一个attr加入scope
    for(int i=features->first();features->more(i);i=features->next(i)){
        features->nth(i)->typeCheck(class_table,env);
    }//递归检查每一个feature类型
    std::vector<Symbol> child=class_table.getChild(name);
    //std::reverse(child.begin(),child.end());
    for(auto x: child){
        class_table.getClass(x).value()->typeCheck(class_table,env);
    }
    //检查每个子类
    //如果一个类的所有子类都检查完成，那么删除该类的作用域
    env.exitScope();
}

void method_class::typeCheck(ClassTable& class_table,Enviro& env){
    env.newScope();
    std::unordered_set<Symbol> se;
    for(int i=formals->first();formals->more(i);i=formals->next(i)){
        Formal cur_formal=formals->nth(i);
        if(se.find(cur_formal->getName())!=se.end()){
            class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(),this)<<"Formal parameter "<<cur_formal->getName()<<" is multiply defined."<<std::endl;
        }
        se.insert(cur_formal->getName());
        env.addVar(cur_formal->getName(),cur_formal->getTypeDecl());
    }
    Symbol expr_type=expr->typeCheck(class_table,env);//多态，调用的是子类具体expr的typeCheck
    if(return_type==SELF_TYPE && (expr_type!=SELF_TYPE && expr_type!=No_type) || !env.checkSubClass(expr_type,return_type)){
        if(return_type != SELF_TYPE && !class_table.getClass(return_type).has_value()){
            env.exitScope();
            return;
        }
        else class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(), this) << "Inferred return type " << expr_type << " of method "<< name << " does not" << " conform to declared return type " << return_type << "." << std::endl; // checked
    }
    env.exitScope();
}

void attr_class::typeCheck(ClassTable& class_table, Enviro& env){
    Symbol init_type=init->typeCheck(class_table,env);//检查初始化语句的type
    if(!env.checkSubClass(init_type,type_decl)){
        class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(), this) << "Inferred type " << init_type << " of initialization of attribute " << name << " does not conform to declared type " << type_decl << "." << std::endl;
    }
}

Symbol assign_class::typeCheck(ClassTable& class_table,Enviro& env){
    if(name==self){
        class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(), this) << "Cannot assign to self" << std::endl;
    }
    Symbol expr_type=expr->typeCheck(class_table,env);//检查expr的type
    auto decl_type=env.lookUp(name);//检查之前变量声明的type
    if(!decl_type){
        class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(),this)<<"Assign Expr: Symbol "<<name<<" is not defined."<<std::endl;
        set_type(Object);//设置assign expr的type
    }else{
        if(!env.checkSubClass(expr_type,decl_type.value())){
            class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(), this) << "Type " << expr_type << " of assigned expression does not conform to declared " << "type " << decl_type.value() << " of identifier " << name << std::endl;
            set_type(Object);
        }else{
            set_type(decl_type.value());
        }
    }
    return get_type();
}

Symbol bool_const_class::typeCheck(ClassTable& class_table,Enviro& env){
    set_type(Bool);
    return Bool;
}

Symbol int_const_class::typeCheck(ClassTable& class_table,Enviro& env){
    set_type(Int);
    return Int;
}

Symbol string_const_class::typeCheck(ClassTable& class_table,Enviro& env){
    set_type(Str);
    return Str;
}

Symbol new__class::typeCheck(ClassTable& class_table,Enviro& env){
    set_type(type_name);
    return get_type();
}

Symbol dispatch_class::typeCheck(ClassTable& class_table,Enviro& env){
    //dispatch_class格式：expression.name(actual)
    Symbol expr_type=expr->typeCheck(class_table,env);//得到前面expr的类型，就是调用函数所在的类
    auto find_result=env.getFuncSig(expr_type,name);
    if(!find_result){
        class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(), this) << "Dispatch to undefined method " << name << "."<< std::endl; //checked
        set_type(Object);
        return get_type();
    }
    Formals formals=find_result.value().first;
    Symbol ret_type=find_result.value().second;
    if(actual->len()!=formals->len()){
        class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(), this) << "Method " << name << " called with wrong number of arguments."<< std::endl; //checked
        set_type(Object);
        return get_type();
    }
    //actual就是函数调用括号中的东西，可以是一个变量、常量或者表达式
    std::vector<Symbol> actual_types;
    for(int i=0;i<actual->len();i++){
        actual_types.push_back(actual->nth(i)->typeCheck(class_table,env));    
    }//查看actual每一项的类型
    for(int i=0;i<actual_types.size();i++){
        if(!env.checkSubClass(actual_types[i],formals->nth(i)->getTypeDecl())){
            class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(), this) << "In call of method " << name << ", type " << actual_types[i] << " of parameter " << formals->nth(i)->getName() << " does not conform to declared type " << formals->nth(i)->getTypeDecl() << "." << std::endl;
            set_type(Object); // checked
            return get_type();
        }
    }
    if(ret_type==SELF_TYPE){//如果函数的ret_type是self，即函数所在的类，就是expr的类型
        set_type(expr_type);
    }else{
        set_type(ret_type);
    }
    return get_type();
}

Symbol static_dispatch_class::typeCheck(ClassTable& class_table,Enviro& env){
    //static_dispatch的格式：expr@typename.name(actual)
    Symbol expr_type=expr->typeCheck(class_table,env);
    if(!env.checkSubClass(expr_type,type_name)){
        class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(), this) << "Static dispatch error! type dismatch real: " << expr_type << " Expect: " << type_name << std::endl;
        set_type(Object);
        return get_type();
    }
    auto find_result=env.getFuncSig(expr_type,name);
    if(!find_result){
        class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(), this) << "Dispatch to undefined method " << name << "."<< std::endl;
        set_type(Object);
        return get_type();
    }
    Formals formals=find_result.value().first;
    Symbol ret_type=find_result.value().second;
    if(actual->len()!=formals->len()){
        class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(), this) << "Method " << name << " called with wrong number of arguments."<< std::endl;
        set_type(Object);
        return get_type();
    }
    //actual就是函数调用括号中的东西，可以是一个变量、常量或者表达式
    std::vector<Symbol> actual_types;
    for(int i=0;i<actual->len();i++){
        actual_types.push_back(actual->nth(i)->typeCheck(class_table,env));    
    }//查看actual每一项的类型
    for(int i=0;i<actual_types.size();i++){
        if(!env.checkSubClass(actual_types[i],formals->nth(i)->getTypeDecl())){
            class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(), this) << "In call of method " << name << ", type " << actual_types[i] << " of parameter " << formals->nth(i)->getName() << " does not conform to declared type " << formals->nth(i)->getTypeDecl() << "." << std::endl;
            set_type(Object);
            return get_type();
        }
    }
    if(ret_type==SELF_TYPE){//如果函数的ret_type是self，即函数所在的类，就是expr的类型
        set_type(expr_type);
    }else{
        set_type(ret_type);
    }
    return get_type();
}

Symbol cond_class::typeCheck(ClassTable& class_table,Enviro& env){
    //if(pred)then...else...
    Symbol pred_type=pred->typeCheck(class_table,env);
    if(pred_type!=Bool){
        class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(), this) << "Predicate of 'if' does not have type Bool." << std::endl;
    }
    Symbol then_expr_type=then_exp->typeCheck(class_table,env);
    Symbol else_expr_type=else_exp->typeCheck(class_table,env);
    Symbol type_lca = env.getLca({then_expr_type, else_expr_type});
    if((then_expr_type==SELF_TYPE || else_expr_type==SELF_TYPE) && env.checkSubClass(SELF_TYPE, type_lca) && env.checkSubClass(type_lca, SELF_TYPE)) set_type(SELF_TYPE);
    else set_type(type_lca);
    
    return get_type();////??????
}

Symbol block_class::typeCheck(ClassTable& class_table,  Enviro &env){
    Symbol ret_type=No_type;
    for(int i=body->first();body->more(i);i=body->next(i)){
        ret_type=body->nth(i)->typeCheck(class_table,env);
    }
    set_type(ret_type);
    return ret_type;
}//block的最后一个expr的type就是block的type

Symbol let_class::typeCheck(ClassTable& class_table,  Enviro &env){
    //let的格式：let identifier:type_decl<-init... in body 
    Symbol init_type=init->typeCheck(class_table,env);
    if (init_type != No_type && !env.checkSubClass(init_type, type_decl)) {
       class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(), this) << "Inferred type " << init_type << " of initialization of " << identifier << " does not conform to identifier's declared type " << type_decl << "." << std::endl;
    }
    env.newScope();
    if(identifier==self){
        class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(), this) << "\'self\' cannot be bound in a \'let\' expression." << std::endl;
    }else{
        env.addVar(identifier,type_decl);
    }
    Symbol ret_type=body->typeCheck(class_table,env);
    env.exitScope();
    set_type(ret_type);
    return get_type();
}

Symbol typcase_class::typeCheck(ClassTable& class_table,Enviro& env){
    //CASE expression OF case_list ESAC
    Symbol expr_type=expr->typeCheck(class_table,env);
    std::unordered_set<Symbol> se;
    for(int i=cases->first();cases->more(i);i=cases->next(i)){
        auto case_=dynamic_cast<branch_class*>(cases->nth(i));
        if(se.find(case_->getTypeDecl())!=se.end()){
            class_table.semant_error\
            (class_table.getClass(env.getCurClass()).value()->get_filename(), case_) \
            << "Duplicate branch " << case_->getTypeDecl() << " in case statement." << std::endl;
        }
        se.insert(case_->getTypeDecl());
    }
    std::vector<Symbol> barnch_types;
    for(int i=cases->first();cases->more(i);i=cases->next(i)){
        barnch_types.push_back(cases->nth(i)->typeCheck(class_table,env));
    }//这里的第n个case的类型指的是branch中后面expr的类型，和上面的类型不一样

    //把众多分支类型的最小公共祖先作为typcase的类型
    Symbol ret_type=env.getLca(barnch_types);
    set_type(ret_type);
    return get_type();
}

Symbol branch_class::typeCheck(ClassTable& class_table,Enviro& env){
    //OBJECTID ':' TYPEID DARROW expression ';'
    //name : type_decl DARROW expr;
    env.newScope();
    env.addVar(name,type_decl);
    Symbol ret_type=expr->typeCheck(class_table,env);
    env.exitScope();
    return ret_type;
}

Symbol loop_class::typeCheck(ClassTable& class_table,Enviro& env){
    //while expr loop expr pool
    Symbol pred_type=pred->typeCheck(class_table,env);
    if(pred_type!=Bool){
        class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(), this) << "Loop condition does not have type Bool." << std::endl;
    }
    Symbol expr_type=body->typeCheck(class_table,env);
    set_type(Object);
    return get_type();
}

Symbol isvoid_class::typeCheck(ClassTable& class_table,Enviro& env){
    //isvoid expr
    e1->typeCheck(class_table,env);
    set_type(Bool);
    return get_type();
}

Symbol comp_class::typeCheck(ClassTable& class_table,Enviro& env){
    //NOT expr
    Symbol tmp_type=e1->typeCheck(class_table,env);
    if(tmp_type!=Bool){
        class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(), this) << "Argument of 'not' has type " << tmp_type << " instead of Bool." << std::endl;
        set_type(Object);
    }else{
        set_type(Bool);
    }
    return get_type();
}

Symbol lt_class::typeCheck(ClassTable& class_table,  Enviro &env) {
    //e1 < e2
    Symbol e1_type = e1->typeCheck(class_table, env);
    Symbol e2_type = e2->typeCheck(class_table, env);
    if (e1_type != Int || e2_type != Int) {
        class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(), this) << "non-Int arguments: " << e1_type << " < " << e2_type << std::endl;
        set_type(Object);
    } else {
        set_type(Bool);
    }
    return get_type();
}

Symbol leq_class::typeCheck(ClassTable& class_table, Enviro &env) {
    Symbol e1_type = e1->typeCheck(class_table, env);
    Symbol e2_type = e2->typeCheck(class_table, env);
    if (e1_type != Int || e2_type != Int) {
        class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(), this) << "non-Int arguments: " << e1_type << " <= " << e2_type << std::endl;
        set_type(Object);
    } else {
        set_type(Bool);
    }
    return get_type();
}

Symbol neg_class::typeCheck(ClassTable& class_table,  Enviro &env) {
    //~e1
    Symbol e1_type = e1->typeCheck(class_table, env);
    if (e1_type != Int) {
        class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(), this) << "Argument of '~' has type " << e1_type << " instead of Bool." << std::endl;
        set_type(Object);
    } else {
        set_type(Int);
    }
    return get_type();

}

Symbol plus_class::typeCheck(ClassTable& class_table,  Enviro &env) {
    Symbol e1_type = e1->typeCheck(class_table, env);
    Symbol e2_type = e2->typeCheck(class_table, env);
    if (e1_type != Int || e2_type != Int) {
        class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(), this) << "non-Int arguments " << e1_type << " + " << e2_type << std::endl;
        set_type(Object);
    }  else {
        set_type(Int);
    }
    return get_type();
}

Symbol sub_class::typeCheck(ClassTable& class_table,  Enviro &env) {
    Symbol e1_type = e1->typeCheck(class_table, env);
    Symbol e2_type = e2->typeCheck(class_table, env);
    if (e1_type != Int || e2_type != Int) {
        class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(), this) << "non-Int arguments " << e1_type << " - " << e2_type << std::endl;
        set_type(Object);
    }  else {
        set_type(Int);
    }
    return get_type();
}

Symbol mul_class::typeCheck(ClassTable& class_table,  Enviro &env) {
    Symbol e1_type = e1->typeCheck(class_table, env);
    Symbol e2_type = e2->typeCheck(class_table, env);
    if (e1_type != Int || e2_type != Int) {
        class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(), this) << "non-Int arguments " << e1_type << " * " << e2_type << std::endl;
        set_type(Object);
    }  else {
        set_type(Int);
    }
    return get_type();
}

Symbol divide_class::typeCheck(ClassTable& class_table,  Enviro &env) {
    Symbol e1_type = e1->typeCheck(class_table, env);
    Symbol e2_type = e2->typeCheck(class_table, env);
    if (e1_type != Int || e2_type != Int) {
        class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(), this) << "non-Int arguments " << e1_type << " / " << e2_type << std::endl;
        set_type(Object);
    }  else {
        set_type(Int);
    }
    return get_type();
}

Symbol eq_class::typeCheck(ClassTable& class_table,  Enviro &env) {
    
    Symbol e1_type = e1->typeCheck(class_table, env);
    Symbol e2_type = e2->typeCheck(class_table, env);
    if (e1_type == Int || e1_type == Str || e1_type == Bool || e2_type == Int || e2_type == Str || e2_type == Bool) {
        if(e1_type != e2_type) {
            class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(), this) << "Illegal comparison with a basic type." << std::endl;
            set_type(Object);
        } else {
            set_type(Bool);
        }
    } else {
        set_type(Bool);
    }
    return get_type();
}

Symbol no_expr_class::typeCheck(ClassTable& class_table,  Enviro &env) {
    set_type(No_type);
    return get_type();
}//这是哪个？？？

Symbol object_class::typeCheck(ClassTable& class_table,  Enviro &env){
    if(name==self){
        set_type(SELF_TYPE);
    }else{
        auto find_result=env.lookUp(name);
        if(!find_result){
            class_table.semant_error(class_table.getClass(env.getCurClass()).value()->get_filename(), this) << "Undeclared identifier " << name << "." << std::endl;
            set_type(Object);
        }else{
            set_type(find_result.value());
        }
    }
    return get_type();
}




/*****************************************类型检查结束************************************************/

void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */
    //semantic analysis phase 1, check the inheritance graph
    classtable->check_phase1();
    //semantic analysis phase 2, check others
    classtable->check_phase2();

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}

