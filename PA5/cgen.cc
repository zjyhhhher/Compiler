
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************
#include <unordered_map>
#include <algorithm>
#include <vector>
#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
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

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;


 /***** Add dispatch information for class String ******/

      s << endl;                                              // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 

 /***** Add dispatch information for class Int ******/

      s << endl;                                          // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

 /***** Add dispatch information for class Bool ******/

      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}

void CgenClassTable::code_class_nameTab(){
  const int n=tag_to_class.size();
  str<<CLASSNAMETAB<<LABEL;
  for(int i=0;i<n;i++){
    str<<WORD;
    StringEntryP tt=stringtable.lookup_string(tag_to_class[i]->get_string());
    tt->code_ref(str);
    str<<endl;
  }
}

void CgenClassTable::code_class_objTab(){
  const int n=tag_to_class.size();
  str<<CLASSOBJTAB<<LABEL;
  for(int i=0;i<n;i++){
    str<<WORD;
    emit_protobj_ref(tag_to_class[i],str);str<<endl;
    str<<WORD;
    emit_init_ref(tag_to_class[i],str);str<<endl;
  }
}

void CgenClassTable::code_class_dispTab(){
  for(List<CgenNode>* l=nds;l;l=l->tl()){
    CgenNode* cur=l->hd();
    emit_disptable_ref(cur->get_name(),str);str<<LABEL;
    gene_dispTab_for_one_class(cur,str);
  }

  if(cgen_debug){
    std::cerr<<"the method offset of each class is: "<<std::endl;
    for(auto& [class_name,t]:class_method_offset){
      std::cerr<<class_name<<std::endl;
      for(auto& [method_name,idx]:t){
        std::cerr<<"\t"<<method_name<<"\t"<<idx<<std::endl;
      }
      std::cerr<<"========================"<<std::endl;
    }
  }
}

void CgenClassTable::gene_dispTab_for_one_class(CgenNodeP cur,std::ostream& str){
  std::vector<CgenNodeP> path;
  while(cur->get_name()!=Object){
    path.push_back(cur);
    cur=cur->get_parentnd();
  }
  path.push_back(cur);

  std::reverse(path.begin(),path.end());

  std::unordered_map<Symbol,int> method_idx;
  std::vector<std::pair<Symbol,Symbol>> methods;

  for(auto _class:path){
    Features features=_class->get_features();
    for(int i=features->first();features->more(i);i=features->next(i)){
      Feature feature=features->nth(i);
      if(feature->getType()==FeatureType::METHOD_){
        Symbol method_name=feature->get_name();
        if(method_idx.find(method_name)==method_idx.end()){
          //这个method第一次出现
          methods.push_back({_class->get_name(),method_name});
          method_idx[method_name]=methods.size()-1;
        }else{
          //继承父类的，重写函数
          methods[method_idx[method_name]]={_class->get_name(),method_name};
          //类修改为子类的名字，调用的时候调用子类的
        }
      }
    }
  }

  for(auto& [class_name,method_name]:methods){
    str<<WORD;
    emit_method_ref(class_name,method_name,str);
    str<<endl;
  }
  //填补class_method_offset<class_name,<method_name,index>>
  //methods[i]={class_name,method_name}
  auto& t=class_method_offset[path.back()->get_name()];//vector最后一个，也就是子类
  const int nn=methods.size();
  for(int i=0;i<nn;i++){
    t[methods[i].second]=i;
  }
}

void CgenClassTable::code_class_protObj(){
  //看stack.s
  //写protObj表，每一个类都以.word   -1开头
  for(List<CgenNode> *l=nds;l;l=l->tl()){
    str<<WORD<<-1<<endl;
    CgenNode* cur=l->hd();
    emit_protobj_ref(cur->get_name(),str);str<<LABEL;
    str<<WORD<<subclass_idrange[cur->get_name()].first<<endl;
    auto all_attrs=collect_all_attr(cur);
    //all_attr是一个vector，每一项是一个pair{name,type}
    if(cur->get_name()==Str){
      str<<WORD<<5<<endl;
      str<<WORD;emit_disptable_ref(cur->get_name(),str);str<<endl;
      str<<WORD;inttable.lookup_string("0")->code_ref(str);str<<endl;
      emit_string_constant(str,"");
    }else if(cur->get_name()==Int||cur->get_name()==Bool){
      str<<WORD<<4<<endl;
      str<<WORD;emit_disptable_ref(cur->get_name(),str);str<<endl;
      str<<WORD<<0<<endl;
    }else{
      str<<WORD<<DEFAULT_OBJFIELDS+all_attrs.size()<<endl;
      str<<WORD;emit_disptable_ref(cur->get_name(),str);str<<endl;
      for(auto& x:all_attrs){
        //如果type是int、str、bool，输出？？？
        if(x.second==Int||
           x.second==Str||
           x.second==Bool){
            str<<WORD;emit_protobj_ref(x.second,str);str<<endl;
            //这里感觉有问题！输出的是Str_protObj
            //但是实际应该是str_const*
        }else{
          str<<WORD<<0<<std::endl;
        }
      }
    }
  }
}

std::vector<std::pair<Symbol,Symbol>> CgenClassTable::collect_all_attr(CgenNodeP cur){
  std::vector<CgenNodeP> path;
  while(cur->get_name()!=Object){
    path.push_back(cur);
    cur=cur->get_parentnd();
  }
  path.push_back(cur);
  std::reverse(path.begin(),path.end());
  std::vector<std::pair<Symbol,Symbol>> all_attrs;

  for(auto& _class:path){
    Features features = _class->get_features();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
      Feature feature = features->nth(i);
      if (feature->getType() == FeatureType::ATTR_) {
        all_attrs.push_back({feature->get_name(), feature->get_type()});
      }
    }
  }

  auto& t = class_attr_offset[path.back()->get_name()];
  const int nn = all_attrs.size();
  for (int i = 0; i < nn; ++i) {
    t[all_attrs[i].first] = i;
  }

  return all_attrs;
}
/*
static void emit_method_begin(std::ostream& s){
  //???按照下面的end函数，难道不是函数有n个参数，SP就要-(n+3)*WORD_SIZE嘛？？？
  //addiu	$sp $sp -12
  emit_addiu(SP,SP,-12,s);
  //sw $fp 12($sp)
  emit_store(FP,3,SP,s);
  //sw $s0 8($sp)
  emit_store(SELF,2,SP,s);
  //sw $ra 4($sp)
  emit_store(RA,1,SP,s);
  //addiu $fp $sp 4
  emit_addiu(FP,SP,4,s);
  //move $s0 $a0
  emit_move(SELF,ACC,s);
}

static void emit_method_end(std::ostream& s,int num_args){
  //move $a0 $s0
  emit_move(ACC,SELF,s);
  //lw	$fp 12($sp)
  emit_load(FP,3,SP,s);
	//lw	$s0 8($sp)
  emit_load(SELF,2,SP,s);
	//lw	$ra 4($sp)
  emit_load(RA,1,SP,s);
	//addiu	$sp $sp 12
  emit_addiu(SP,SP,(3+num_args)*WORD_SIZE,s);
	//jr	$ra	
  emit_return(s);
}
*/

static void emit_method_begin(std::ostream& s) {
  emit_addiu(SP, SP, -12, s);
  // store fp
  emit_store(FP, 3, SP, s);
  // store ra
  emit_store(RA, 2, SP, s);
  // store SELF
  emit_store(SELF, 1, SP, s);
  // set fp
  emit_addiu(FP, SP, 12, s);
  // set SELF
  emit_move(SELF, ACC, s);
}

static void emit_method_end(std::ostream& s, int num_args) {
  // resotre register
  emit_load(SELF, 1, SP, s);
  emit_load(RA, 2, SP, s);
  emit_load(FP, 3, SP, s);

  // modify sp
  emit_addiu(SP, SP, (3 + num_args) * WORD_SIZE, s);
  emit_return(s);
}

void CgenClassTable::code_class_init(){
  for(List<CgenNode> *l=nds;l;l=l->tl()){
    CgenNodeP cur_class=l->hd();
    setCurClass(cur_class);
    auto& t=class_attr_offset[cur_class->get_name()];
    enterScope();
    for(auto& [name,offset]:t){
      addSymbol(name,BASE_LOC_TYPE::SELF_,offset+DEFAULT_OBJFIELDS);
    }

    emit_init_ref(cur_class->get_name(),str);str<<LABEL;
    emit_method_begin(str);

    if(cur_class->get_name()!=Object){
      CgenNodeP parent=cur_class->get_parentnd();
      emit_move(ACC, SELF, str);
      str<<JAL;emit_init_ref(parent->get_name(),str);str<<endl;
      emit_move(SELF, ACC, str);
    }
    //对于类中的每个new了的attr（类型是一个类），init中都会有这四句
    //la	$a0 Stack_protObj
	  //jal	Object.copy
	  //jal	Stack_init
	  //sw	$a0 12($s0)
    if(cur_class->get_name()!=Int && 
       cur_class->get_name()!=Str &&
       cur_class->get_name()!=Bool&&
       cur_class->get_name()!=Object){
      Features features=cur_class->get_features();
      for(int i=features->first();features->more(i);i=features->next(i)){
        Feature feature=features->nth(i);
        if(feature->getType()==FeatureType::ATTR_){
          feature->code(str,*this);
        }
      }
    }
    emit_method_end(str,0);
    exitScope();
  }
}

void CgenClassTable::code_class_method(){
  for(List<CgenNode>* l=nds;l;l=l->tl()){
    CgenNodeP cur_class=l->hd();
    setCurClass(cur_class);
    Symbol cur_class_name = cur_class->get_name();
    if (cur_class_name == Object ||
        cur_class_name == Str ||
        cur_class_name == IO ||
        cur_class_name == Int ||
        cur_class_name == Bool) continue;
    auto& t=class_attr_offset[cur_class->get_name()];
    enterScope();
    //开一个新的作用域，把这个类的成员变量和self都加到symbol_to_loc里面
    for(auto& [name,offset]:t){
      addSymbol(name,BASE_LOC_TYPE::SELF_,offset+DEFAULT_OBJFIELDS);
    }
    addSymbol(self,BASE_LOC_TYPE::SELF_,0);

    Features features=cur_class->get_features();
    for(int i=features->first();features->more(i);i=features->next(i)){
      Feature feature=features->nth(i);
      if(feature->getType()==FeatureType::METHOD_){
        emit_method_ref(cur_class->get_name(),feature->get_name(),str);str<<LABEL;
        feature->code(str,*this);
      }
    }
    exitScope();
  }
}

CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
  // stringclasstag = 0 /* Change to your String class tag here */;
  // intclasstag =    0 /* Change to your Int class tag here */;
  // boolclasstag =   0 /* Change to your Bool class tag here */;

  enterscope();
  if (cgen_debug) cout << "Building CgenClassTable" << endl;
  install_basic_classes();
  install_classes(classes);
  build_inheritance_tree();

  if(cgen_debug) cout<<"Set Class Tag"<<std::endl;
  set_class_tag();

  code();
  exitscope();
}

void CgenClassTable::set_class_tag(){
  if(cgen_debug)cout<<"OK!"<<std::endl;
  CgenNodeP root_class=root();
  if(cgen_debug)cout<<"YES!"<<std::endl;
  set_class_tag_internal(root_class,0);

  stringclasstag=subclass_idrange[idtable.lookup_string("String")].first;
  cout<<"YES1!"<<std::endl;
  intclasstag=subclass_idrange[idtable.lookup_string("Int")].first;
  cout<<"YES2!"<<std::endl;
  boolclasstag=subclass_idrange[idtable.lookup_string("Bool")].first;
  cout<<"YES3!"<<std::endl;

  if(cgen_debug)cout<<"YES!"<<std::endl;
  if (cgen_debug) {
    cout << "The class tag range is: " << std::endl;
    for (auto& [k, v] : subclass_idrange) {
      cout << k << " " << v.first << " " << v.second << endl;
    }
    cout << "The class depth in inherite tree" << std::endl;
    for (auto& [k, v] : class_depth) {
      cout << k << ' ' << v << std::endl;
    }
  }
}

void CgenClassTable::set_class_tag_internal(CgenNodeP cur,int cur_dep){
  //cur->get_name();
  if(cur==NULL)cout<<"YES!"<<std::endl;
  class_depth[cur->get_name()]=cur_dep;
  cout<<"YES!"<<std::endl;
  int begin_idx=tag_to_class.size();
  tag_to_class.push_back(cur->get_name());
  auto children=cur->get_children();
  for(List<CgenNode> *l=children;l;l=l->tl()){
    CgenNodeP cur_child=l->hd();
    set_class_tag_internal(cur_child,cur_dep+1);
  }
  int end_idx=tag_to_class.size()-1;
  subclass_idrange[cur->get_name()]={begin_idx,end_idx};
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
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
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
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
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}



void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//
  if (cgen_debug) cout << "coding class_nameTab" << endl;
  code_class_nameTab();

  if (cgen_debug) cout << "coding class_objTab" << endl;
  code_class_objTab();

  if (cgen_debug) cout << "coding class_dispTab" << endl;
  code_class_dispTab();

  if (cgen_debug) cout << "coding class_protObj" << endl;
  code_class_protObj();
/////////////////////////////////////////////////////////////////////////////

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...
  if (cgen_debug) cout << "coding class initializer" << endl;
  code_class_init();

  if (cgen_debug) cout << "coding class other method" << endl;
  code_class_method();
}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream &s, CgenClassTable& cgen_class) {
  //将expression的结果保存到$a0中
  expr->code(s,cgen_class);

  auto& symbol_to_loc=cgen_class.getSymbolToLoc();
  auto loc=symbol_to_loc.lookUp(name);
  if(loc.base_loc==BASE_LOC_TYPE::SELF_){
    //这个变量是class的成员变量
    emit_store(ACC,loc.offset,SELF,s);
  }else{
    //这个变量是函数参数或者let、branch的
    emit_store(ACC,loc.offset,FP,s);
  }
}

int CgenClassTable::get_method_offset(Symbol class_name,Symbol method_name){
  if(cgen_debug)cout<<class_name<<" "<<method_name<<" "<<endl;
  if(class_name==SELF_TYPE)class_name=cur_class->get_name();
  if(class_method_offset.find(class_name)==class_method_offset.end()){
    std::cerr<<"get_method_offset error! cannot find:"
             <<class_name<<"::"<<method_name<<std::endl;
  }
  if(cgen_debug)cout<<class_name<<" "<<method_name<<" "<<endl;
  auto& t=class_method_offset[class_name];
  if(t.find(method_name)==t.end())cout<<class_name<<" "<<method_name<<" "<<getCurClass()->get_name() <<endl;
  assert(t.find(method_name)!=t.end());
  return t[method_name];
}//得到函数在该类的dispTab中的偏移量

void static_dispatch_class::code(ostream &s,CgenClassTable& cgen_class) {
  //expr@typename.name(actual)
  for(int i=actual->first();actual->more(i);i=actual->next(i)){
    Expression expression=actual->nth(i);
    expression->code(s,cgen_class);
    emit_push(ACC,s);
  }
  //把每个参数依次进栈,进栈第一个，第二个...按顺序
  expr->code(s,cgen_class);
  int no_void_label=cgen_class.get_next_labelid();
  emit_bne(ACC,ZERO,no_void_label,s);
  //如果没有成功调用：
  //la $a0 str_const*
  //li $t1 24(line_no)
  //jal _dispatch_abort
  Symbol file_name=cgen_class.getCurClass()->get_filename();
  //la $a0
  emit_partial_load_address(ACC,s);
  //la $a0 str_const*
  stringtable.lookup_string(file_name->get_string())->code_ref(s);
  s<<endl;
  int line_no=get_line_number();
  //li $t1 line_no
  emit_load_imm(T1,line_no,s);
  //jal _dispatch_abort
  emit_jal("_dispatch_abort",s);

////////////////////////expr不是空，进行函数调用//////////////////////////////
  //成功调用，先打印label：
  emit_label_def(no_void_label,s);
  //li $t1 A_dispTab
  emit_partial_load_address(T1,s);
  emit_disptable_ref(type_name,s);
  s<<endl;
  
  if(cgen_debug)cout<<"s"<<" "<<type_name<<" "<<name<<cgen_class.getCurClass()->get_name()<< endl;
  
  //lw $t1 offset(&t1)找到这个method在dispTab中的offset
  int method_offset=cgen_class.get_method_offset(type_name,name);
  emit_load(T1,method_offset,T1,s);
  //jalr $t1
  emit_jalr(T1,s);
}

void dispatch_class::code(ostream &s,CgenClassTable& cgen_class) {
  //dispatch_class格式：expression.name(actual)
  for(int i=actual->first();actual->more(i);i=actual->next(i)){
    Expression expression=actual->nth(i);
    expression->code(s,cgen_class);
    emit_push(ACC,s);
  }
  //把每个参数依次进栈,进栈第一个，第二个...按顺序
  expr->code(s,cgen_class);
  int no_void_label=cgen_class.get_next_labelid();
  emit_bne(ACC,ZERO,no_void_label,s);

  //如果没有成功调用：
  //la $a0 str_const*
  //li $t1 24(line_no)
  //jal _dispatch_abort
  Symbol file_name=cgen_class.getCurClass()->get_filename();
  //la $a0
  emit_partial_load_address(ACC,s);
  //la $a0 str_const*
  stringtable.lookup_string(file_name->get_string())->code_ref(s);
  s<<endl;
  int line_no=get_line_number();
  //li $t1 line_no
  emit_load_imm(T1,line_no,s);
  //jal _dispatch_abort
  emit_jal("_dispatch_abort",s);

  //成功调用，先打印label：
  emit_label_def(no_void_label,s);

  //找到dispTab
  //a0里面是expression的计算结果，加载对应类的dispTab
  emit_load(T1,DISPTABLE_OFFSET,ACC,s);

  //lw $t1 offset(&t1)找到这个method在dispTab中的offset
  Symbol type_name=expr->get_type();
  if(cgen_debug)cout<<type_name<<" "<<name<<cgen_class.getCurClass()->get_name()<< endl;
  int method_offset=cgen_class.get_method_offset(type_name,name);
  emit_load(T1,method_offset,T1,s);
  //jalr $t1
  emit_jalr(T1,s);
}

void cond_class::code(ostream &s,CgenClassTable& cgen_class) {
  //if(pred)then...else...
  pred->code(s,cgen_class);
  emit_load_bool(T1,BoolConst(TRUE),s);

  int else_label=cgen_class.get_next_labelid();

  //使用equality_test，测试t1和t2里面的值是否有相同的类型和值
  //如果他们有相同类型和值，a0寄存器中的value被return
  //否则，a1寄存器中的value被return
  emit_move(T2,ACC,s);//把a0，刚才计算的pred的值给t2
  emit_load_bool(ACC,BoolConst(TRUE),s);//把true给a0
  emit_load_bool(A1,BoolConst(FALSE),s);//把false给a1
  emit_jal("equality_test",s);
  //所以，这种情况下应该是
  //如果t2的值是true，a0就是true，否则a0就是false
  //随后比较a0和a1的值，如果相等，说明t2的值是false，要去else执行
  emit_beq(ACC,A1,else_label,s);
///////////////then////////////////////////////
  then_exp->code(s,cgen_class);
  int end_label=cgen_class.get_next_labelid();
  //jump to end(then执行完之后)
  emit_branch(end_label,s);
///////////////else///////////////////////////
  emit_label_def(else_label,s);
  else_exp->code(s,cgen_class);
  emit_label_def(end_label,s);
}

void loop_class::code(ostream &s, CgenClassTable& cgen_class) {
  //while expr loop expr pool
  int test_idx=cgen_class.get_next_labelid();//循环体
  int exit_idx=cgen_class.get_next_labelid();//循环结束的下一句

  emit_label_def(test_idx,s);
  pred->code(s,cgen_class);//每次循环都要重新计算pred，看是否满足条件
  emit_load_bool(T1,BoolConst(TRUE),s);
  emit_bne(ACC,T1,exit_idx,s);
  body->code(s,cgen_class);
  emit_branch(test_idx,s);

  emit_label_def(exit_idx,s);
  emit_move(ACC,ZERO,s);
}

void typcase_class::code(ostream &s,CgenClassTable& cgen_class) {
  //CASE expression OF case_list ESAC
  expr->code(s,cgen_class);
  int end_label=cgen_class.get_next_labelid();
  int no_zero_label=cgen_class.get_next_labelid();
  int case_error1_label=cgen_class.get_next_labelid();

  emit_bne(ACC,ZERO,no_zero_label,s);//expr不是void跳转
  //错误处理：
  Symbol file_name=cgen_class.getCurClass()->get_filename();
  emit_partial_load_address(ACC,s);
  stringtable.lookup_string(file_name->get_string())->code_ref(s);
  s<<endl;
  int line_no=get_line_number();
  emit_load_imm(T1,line_no,s);
  emit_jal("_case_abort2",s);//call abort

  emit_label_def(no_zero_label,s);
  emit_load(T1,TAG_OFFSET,ACC,s);//把expression的计算结果类的tag放到t1中

  std::vector<Case> sorted_result;
  for(int i=cases->first();cases->more(i);i=cases->next(i)){
    sorted_result.push_back(cases->nth(i));
  }

  std::sort(sorted_result.begin(), sorted_result.end(), [&](Case& a, Case& b) -> bool {
    return cgen_class.get_depth(a->get_type_decl()) > cgen_class.get_depth(b->get_type_decl());
  });
  //深度从深到浅排序（即子类--->父类）
  const int n=sorted_result.size();
  for(int i=0;i<n;i++){
    Case cur_case=sorted_result[i];
    Symbol type_decl=cur_case->get_type_decl();
    auto [begin_idx, end_idx]=cgen_class.get_subclass_idrange(type_decl);
    int next_branch_idx=cgen_class.get_next_labelid();
    //得到当前case的type的子类tag范围

    emit_blti(T1,begin_idx,next_branch_idx,s);//如果t1<begin_idx
    emit_bgti(T1,end_idx,next_branch_idx,s);//如果t1>end_idx
    //说明t1不是当前case的type的子类，跳转到下一个case

    //匹配：进入当前case中：
    emit_push(ACC,s);//acc是expression的计算结果
    //进入case中：
    cgen_class.enterScope();
    int cur_stack_offset=cgen_class.push_new_var();
    cgen_class.addSymbol(cur_case->get_name(),BASE_LOC_TYPE::FP_,cur_stack_offset);

    cur_case->get_expr()->code(s,cgen_class);
    cgen_class.exitScope();
    
    emit_addiu(SP,SP,4,s);
    cgen_class.pop_new_var(1);
    emit_branch(end_label,s);
    emit_label_def(next_branch_idx,s);
  }

  //没有case可以匹配
  emit_label_def(case_error1_label,s);
  emit_jal("_case_abort",s);

  emit_label_def(end_label,s);
}

void block_class::code(ostream &s,CgenClassTable& cgen_class) {
  for(int i=body->first();body->more(i);i=body->next(i)){
    Expression expr=body->nth(i);
    expr->code(s,cgen_class);
  }
}

void let_class::code(ostream &s,CgenClassTable& cgen_class) {
  //let的格式：let identifier:type_decl<-init in body 
  //let identifier:type_decl in body
  //let identifier:type_decl, body(这里的body从identifier开始)
  //let identifier:type_decl<-init,body(同上，只不过第一个变量有了init)
  if(init->get_type()==NULL){//没初始化
    if(type_decl==Int || type_decl==Str || type_decl==Bool){
      emit_partial_load_address(ACC,s);
      emit_protobj_ref(type_decl,s);
      s<<endl;
      emit_jal("Object.copy",s);
    }else{
      emit_move(ACC,ZERO,s);
    }
  }else{//有初始化
    init->code(s,cgen_class);
  }
  //why?为什么这里要push a0
  emit_push(ACC,s);
  cgen_class.enterScope();
  int cur_stack_offset=cgen_class.push_new_var();
  cgen_class.addSymbol(identifier,BASE_LOC_TYPE::FP_,cur_stack_offset);
  body->code(s,cgen_class);
  cgen_class.exitScope();
  cgen_class.pop_new_var(1);
  emit_addiu(SP,SP,4,s);//恢复栈帧，弹出a0
}

static void load_two_int(std::ostream& s,CgenClassTable& cgen_class,Expression e1,Expression e2){
  e1->code(s,cgen_class);
  emit_push(ACC,s);
  //因为e2计算的时候也要把结果放到acc里
  //所以要先把acc放到栈上
  e2->code(s,cgen_class);
  emit_jal("Object.copy",s);

  emit_load(T1,1,SP,s);//把e1的结果放到t1里面
  emit_addiu(SP,SP,4,s);//平衡栈

  emit_load(T2,DEFAULT_OBJFIELDS,T1,s);//把e1的计算值放到T2里面
  //一个Int，Str，Bool的第四个位置放的value（classTag、length啥的）
  emit_load(T3,DEFAULT_OBJFIELDS,ACC,s);//把e2的计算结果值放到T3里面
}

void plus_class::code(ostream &s, CgenClassTable& cgen_class) {
  load_two_int(s,cgen_class,e1,e2);
  emit_add(T2,T2,T3,s);
  emit_store(T2,DEFAULT_OBJFIELDS,ACC,s);
  //在上面load_two_int里面，用object.copy新创建了一个object，地址为acc
  //所以把计算结果放到这个object的第四个位置
}

void sub_class::code(ostream &s, CgenClassTable& cgen_class) {
  load_two_int(s,cgen_class,e1,e2);
  emit_sub(T2,T2,T3,s);
  emit_store(T2,DEFAULT_OBJFIELDS,ACC,s);
}

void mul_class::code(ostream &s,CgenClassTable& cgen_class) {
  load_two_int(s,cgen_class,e1,e2);
  emit_mul(T2,T2,T3,s);
  emit_store(T2,DEFAULT_OBJFIELDS,ACC,s);
}

void divide_class::code(ostream &s,CgenClassTable& cgen_class) {
  load_two_int(s,cgen_class,e1,e2);
  emit_div(T2,T2,T3,s);
  emit_store(T2,DEFAULT_OBJFIELDS,ACC,s);
}

void neg_class::code(ostream &s,CgenClassTable& cgen_class) {
  e1->code(s,cgen_class);
  emit_jal("Object.copy",s);
  emit_load(T1,DEFAULT_OBJFIELDS,ACC,s);
  emit_neg(T1,T1,s);
  emit_store(T1,DEFAULT_OBJFIELDS,ACC,s);
}

void lt_class::code(ostream &s,CgenClassTable& cgen_class) {
  //e1 < e2，都是Int
  //如果就是e1<e2，acc里面是true；否则是false
  load_two_int(s,cgen_class,e1,e2);
  emit_load_bool(ACC,BoolConst(TRUE),s);

  int final_branch_id=cgen_class.get_next_labelid();
  emit_blt(T2,T3,final_branch_id,s);
  emit_load_bool(ACC,BoolConst(FALSE),s);
  emit_label_def(final_branch_id,s);
}

void eq_class::code(ostream &s,CgenClassTable &cgen_class) {
  //e1=e2
  //e1，e2可以是Bool、Str、Int，返回类型都是Bool
  e1->code(s,cgen_class);
  emit_push(ACC,s);
  e2->code(s,cgen_class);
  emit_move(T2,ACC,s);
  emit_load(T1,1,SP,s);
  emit_addiu(SP,SP,4,s);

  emit_load_bool(ACC,BoolConst(TRUE),s);
  emit_load_bool(A1,BoolConst(FALSE),s);

  int out_label=cgen_class.get_next_labelid();
  emit_beq(T1,T2,out_label,s);

  emit_jal("equality_test",s);
  emit_branch(out_label,s);
  emit_load_bool(ACC, BoolConst(FALSE), s);
  emit_label_def(out_label,s);
}

void leq_class::code(ostream &s,CgenClassTable& cgen_class) {
  //e1<=e2,e1和e2都是Int
  load_two_int(s,cgen_class,e1,e2);
  emit_load_bool(ACC,BoolConst(TRUE),s);
  int final_branch_id=cgen_class.get_next_labelid();
  emit_bleq(T2,T3,final_branch_id,s);

  emit_load_bool(ACC,BoolConst(FALSE),s);
  emit_label_def(final_branch_id,s);
}

void comp_class::code(ostream &s,CgenClassTable& cgen_class) {
  //NOT expr
  e1->code(s,cgen_class);
  emit_move(T1,ACC,s);
  emit_load_bool(T2,BoolConst(FALSE),s);
  emit_load_bool(ACC,BoolConst(TRUE),s);
  emit_load_bool(A1,BoolConst(FALSE),s);
  emit_jal("equality_test", s);
}

void int_const_class::code(ostream& s,CgenClassTable& cgen_class)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s,CgenClassTable& cgen_class)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s,CgenClassTable& cgen_class)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s,CgenClassTable& cgen_class) {
  //竟然可以new SELF_TTYPE
  //这里没懂totally！
  if(type_name==SELF_TYPE){
    emit_load(T1,0,ACC,s);
    emit_load_address(T2,"class_objTab",s);
    emit_load_imm(T3,8,s);
    emit_mul(T1,T1,T3,s);
    emit_add(T2,T2,T1,s);

    emit_load(ACC,0,T2,s);
    emit_jal("Object.copy",s);

    emit_load(T1,0,ACC,s);
    emit_load_address(T2,"class_objTab",s);
    emit_load_imm(T3,8,s);
    emit_mul(T1, T1, T3, s);
    emit_addiu(T1, T1, 4, s);
    emit_add(T2, T2, T1, s);
    // call init ;
    emit_load(A1, 0, T2, s);
    emit_jalr(A1, s);
  }else{
    emit_partial_load_address(ACC, s);
    Symbol real_type = type_name;
    emit_protobj_ref(real_type, s); s << endl;

    // copy the protObj
    emit_jal("Object.copy", s);

    // call the init funcion
    s << JAL; emit_init_ref(real_type, s); s << endl;
  }
}

void isvoid_class::code(ostream &s,CgenClassTable& cgen_class) {
  e1->code(s, cgen_class);
  emit_move(T1, ACC, s);
  emit_load_bool(ACC, BoolConst(TRUE), s);
  int final_idx = cgen_class.get_next_labelid();
  //比较t1和0，相等则isvoid为true
  emit_beqz(T1, final_idx, s);
  emit_load_bool(ACC, BoolConst(FALSE), s);
  emit_label_def(final_idx, s);
}

void no_expr_class::code(ostream &s,CgenClassTable& cgen_class) {

}

void object_class::code(ostream &s,CgenClassTable& cgen_class) {
  if(name==self){
    emit_move(ACC,SELF,s);
    return;
  }
  auto loc=cgen_class.getSymbolToLoc().lookUp(name);
  if(loc.base_loc==BASE_LOC_TYPE::SELF_){
    emit_load(ACC,loc.offset,SELF,s);
  }else{
    emit_load(ACC,loc.offset,FP,s);
  }
}

void attr_class::code(std::ostream &s, CgenClassTable& cgen_class){
  if(init->get_type()==NULL){
    return;//就是没有初始化这个attr，就什么指令也没有
  }
  //init分为new的，和直接用
  init->code(s,cgen_class);
  auto loc=cgen_class.getSymbolToLoc().lookUp(name);
  emit_store(ACC,loc.offset,SELF,s);
  emit_move(ACC,SELF,s);
  //gc
  emit_gc_assign(s);
}

void method_class::code(std::ostream& s,CgenClassTable& cgen_class){
  cgen_class.enterScope();
  std::vector<Symbol> args;
  for(int i=formals->first();formals->more(i);i=formals->next(i)){
    Formal formal=formals->nth(i);
    args.push_back(formal->get_name());
  }
  std::reverse(args.begin(),args.end());
  const int n=args.size();
  for(int i=0;i<n;i++){//why?为什么这里是FP，为什么这里是i+1
    cgen_class.addSymbol(args[i],BASE_LOC_TYPE::FP_,i+1);
  }//最后一个参数的offset是1，第一个参数的offset是n？
  //我猜：应该是参数都放在栈上，靠fp来索引
  //最后一个参数索引：fp-1*4e

  emit_method_begin(s);
  expr->code(s,cgen_class);
  emit_method_end(s,n);
  cgen_class.exitScope();
}
