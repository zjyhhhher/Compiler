/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%option noyywrap
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;
static int n_comment_layer;
int flag=0;

/*
 *  Add Your own definitions here
 */

%}

%Start COMMENT
%Start ONELINE_COMMENT
%Start STRING

/*
 * Define names for regular expressions here.
 */
CLASS           [Cc][Ll][Aa][Ss][Ss]
ELSE            [Ee][Ll][Ss][Ee]
IF              [Ii][Ff]
FI              [Ff][Ii]
IN              [Ii][Nn]
INHERITS        [Ii][Nn][Hh][Ee][Rr][Ii][Tt][Ss]
LET             [Ll][Ee][Tt]
LOOP            [Ll][Oo][Oo][Pp]
POOL            [Pp][Oo][Oo][Ll]
THEN            [Tt][Hh][Ee][Nn]
WHILE           [Ww][Hh][Ii][Ll][Ee]
CASE            [Cc][Aa][Ss][Ee]
ESAC            [Ee][Ss][Aa][Cc]
OF              [Oo][Ff]
DARROW          =>
NEW             [Nn][Ee][Ww]
ISVOID          [Ii][Ss][Vv][Oo][Ii][Dd]
INT_CONST       [0-9]+
BOOL_CONST      (t[Rr][Uu][Ee])|(f[Aa][Ll][Ss][Ee])
TYPEID          [A-Z][A-Za-z0-9_]*
OBJECTID        [a-z][A-Za-z0-9_]*
ASSIGN          <-
NOT             [Nn][Oo][Tt]
LE              <=
WHITE_SPACE     [ \t\f\r\v]+ 
/*这里不加\n是因为涉及到\n行数要增加，而使用正则表达式不能知道有几个换行*/
%%

 /*
  *  Nested comments
  */
<COMMENT>{
"(*" { if(flag==1){flag=0;curr_lineno++;}n_comment_layer++; }
[^\n(*)] {if(flag==1){flag=0;curr_lineno++;}}
"*)" {
  if(flag==1){flag=0;curr_lineno++;}
  n_comment_layer--;
  if(n_comment_layer==0)BEGIN 0;
}
[(*)] {if(flag==1){flag=0;curr_lineno++;}}
"\n" { if(flag==1){flag=0;curr_lineno++;}curr_lineno++; }
<<EOF>> {
  if(flag==1){flag=0;curr_lineno++;}
  yylval.error_msg="EOF in comment";
  BEGIN 0;
  return ERROR;
}
}

<STRING>{
<<EOF>> {
  if(flag==1){flag=0;curr_lineno++;}
  yylval.error_msg="EOF in string constant";
  BEGIN 0;
  return ERROR;
}
\n {
  if(flag==1){flag=0;curr_lineno++;}
  int n=yyleng;
  std::string str_const;
  for(int i=0;i<n;i++){
    if(yytext[i]=='\0'){
      if(i-1>=0){
      if(yytext[i-1]=='\\')
      yylval.error_msg="String contains escaped null character.";/*增加/后面null的情况*/
      else yylval.error_msg="String contains null character.";
      }
      else yylval.error_msg="String contains null character.";
      BEGIN 0;
      flag=1;
      return ERROR;
    }
  }
  std::string str=yytext;
  str=str.substr(0,str.size()-1);/*去掉末尾换行*/
  n=str.size();
  for(int i=0;i<n;i++){
    if(str[i]=='\\'){
      if(str[i+1]=='b')str_const+='\b';
      else if(str[i+1]=='t')str_const+='\t';
      else if(str[i+1]=='n')str_const+='\n';
      else if(str[i+1]=='f')str_const+='\f';
      else str_const+=str[i+1];
      ++i;
    }else str_const+=str[i];
  }
  if(str_const.size()<=MAX_STR_CONST){
    yylval.error_msg="Unterminated string constant";
    curr_lineno++;
    BEGIN 0;
    return ERROR;
  }
  else{yylval.error_msg="String constant too long";
  BEGIN 0;
  flag=1;
  return ERROR;
  }
}
[^\\"\""\n]* { if(flag==1){flag=0;curr_lineno++;}yymore(); }
\\\n {
  if(flag==1){flag=0;curr_lineno++;}
  curr_lineno++;
  yymore();
}

\\[^\n] { if(flag==1){flag=0;curr_lineno++;}yymore(); }
\\ { if(flag==1){flag=0;curr_lineno++;}yymore(); }
"\"" {
  if(flag==1){flag=0;curr_lineno++;}
  int n=yyleng;
  std::string str_const;
  for(int i=0;i<n;i++){
    if(yytext[i]=='\0'){
      if(i-1>=0){
      if(yytext[i-1]=='\\')
      yylval.error_msg="String contains escaped null character.";/*增加/后面null的情况*/
      else yylval.error_msg="String contains null character.";
      }
      else yylval.error_msg="String contains null character.";
      BEGIN 0;
      return ERROR;
    }
  }
  std::string str=yytext;
  str=str.substr(0,str.size()-1);/*去掉末尾引号*/
  n=str.size();
  for(int i=0;i<n;i++){
    if(str[i]=='\\'){
      if(str[i+1]=='b')str_const+='\b';
      else if(str[i+1]=='t')str_const+='\t';
      else if(str[i+1]=='n')str_const+='\n';
      else if(str[i+1]=='f')str_const+='\f';
      else str_const+=str[i+1];
      ++i;
    }else str_const+=str[i];
  }
  if(str_const.size()<=MAX_STR_CONST-1){
    cool_yylval.symbol=stringtable.add_string(const_cast<char*>(str_const.c_str()));
    /*通过const_cast运算符，能将const type转换为type*/
    BEGIN 0;
    return (STR_CONST);
  }else {
    cool_yylval.error_msg="String constant too long";
    BEGIN 0;
    return ERROR;
  }
}
}

<ONELINE_COMMENT>{
.* { if(flag==1){flag=0;curr_lineno++;}}
"\n" {
  if(flag==1){flag=0;curr_lineno++;}
  curr_lineno++;
  BEGIN 0;
}
}

<INITIAL>{
"--" {if(flag==1){flag=0;curr_lineno++;}BEGIN ONELINE_COMMENT;}
"(*" {
  if(flag==1){flag=0;curr_lineno++;}
  n_comment_layer++;
  BEGIN COMMENT;
}
"*)" {
  if(flag==1){flag=0;curr_lineno++;}
  yylval.error_msg="Unmatched *)";
  return ERROR;
}
"\"" {if(flag==1){flag=0;curr_lineno++;}BEGIN STRING;}
{WHITE_SPACE} {if(flag==1){flag=0;curr_lineno++;} }
"\n" {if(flag==1){flag=0;curr_lineno++;}curr_lineno++;}

 /*
  *  The multiple-character operators.
  */
{DARROW}		{ if(flag==1){flag=0;curr_lineno++;}return (DARROW); }
{LE} {if(flag==1){flag=0;curr_lineno++;}return (LE); }
{ASSIGN} {if(flag==1){flag=0;curr_lineno++;}return (ASSIGN); }
 /*
  * Single-character operators.
  */
"+" {if(flag==1){flag=0;curr_lineno++;}return static_cast<int>('+');}
"-" {if(flag==1){flag=0;curr_lineno++;}return static_cast<int>('-');}
"*" {if(flag==1){flag=0;curr_lineno++;}return static_cast<int>('*');}
"/" {if(flag==1){flag=0;curr_lineno++;}return static_cast<int>('/');}
"~" {if(flag==1){flag=0;curr_lineno++;}return static_cast<int>('~');}
"<" {if(flag==1){flag=0;curr_lineno++;}return static_cast<int>('<');}
"=" {if(flag==1){flag=0;curr_lineno++;}return static_cast<int>('=');}
"(" {if(flag==1){flag=0;curr_lineno++;}return static_cast<int>('(');}
")" {if(flag==1){flag=0;curr_lineno++;}return static_cast<int>(')');}
"{" {if(flag==1){flag=0;curr_lineno++;}return static_cast<int>('{');}
"}" {if(flag==1){flag=0;curr_lineno++;}return static_cast<int>('}');}
"," {if(flag==1){flag=0;curr_lineno++;}return static_cast<int>(',');}
";" {if(flag==1){flag=0;curr_lineno++;}return static_cast<int>(';');}
":" {if(flag==1){flag=0;curr_lineno++;}return static_cast<int>(':');}
"@" {if(flag==1){flag=0;curr_lineno++;}return static_cast<int>('@');}
"." {if(flag==1){flag=0;curr_lineno++;}return static_cast<int>('.');}

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
  
{CLASS} {if(flag==1){flag=0;curr_lineno++;}return (CLASS);}
{ELSE}  {if(flag==1){flag=0;curr_lineno++;}return (ELSE); }
{FI}    {if(flag==1){flag=0;curr_lineno++;}return (FI);}
{IF}    {if(flag==1){flag=0;curr_lineno++;}return (IF);}
{IN}    {if(flag==1){flag=0;curr_lineno++;}return (IN);}
{INHERITS} {if(flag==1){flag=0;curr_lineno++;}return (INHERITS);}
{LET}   {if(flag==1){flag=0;curr_lineno++;}return (LET);}
{LOOP}  {if(flag==1){flag=0;curr_lineno++;}return (LOOP);}
{POOL}  {if(flag==1){flag=0;curr_lineno++;}return (POOL);}
{THEN}  {if(flag==1){flag=0;curr_lineno++;}return (THEN);}
{WHILE}  {if(flag==1){flag=0;curr_lineno++;}return (WHILE);}
{CASE}  {if(flag==1){flag=0;curr_lineno++;}return (CASE);}
{ESAC}  {if(flag==1){flag=0;curr_lineno++;}return (ESAC);}
{OF}    {if(flag==1){flag=0;curr_lineno++;}return (OF);}
{NEW}   {if(flag==1){flag=0;curr_lineno++;}return (NEW);}
{ISVOID} {if(flag==1){flag=0;curr_lineno++;}return (ISVOID);}
{NOT}   {if(flag==1){flag=0;curr_lineno++;}return (NOT);}

{BOOL_CONST} {
  if(flag==1){flag=0;curr_lineno++;}
  if (yytext[0] == 't') cool_yylval.boolean = 1;
  else cool_yylval.boolean = 0;
  return (BOOL_CONST);
}
{INT_CONST} {
  if(flag==1){flag=0;curr_lineno++;}
  cool_yylval.symbol=inttable.add_string(yytext);
  return (INT_CONST);
}
{TYPEID} {
  if(flag==1){flag=0;curr_lineno++;}
  cool_yylval.symbol = idtable.add_string(yytext);
  return (TYPEID);
}
{OBJECTID} {
  if(flag==1){flag=0;curr_lineno++;}
  cool_yylval.symbol = idtable.add_string(yytext);
  return (OBJECTID);
}
. {
  if(flag==1){flag=0;curr_lineno++;}
  yylval.error_msg = yytext;
  return ERROR;
}


}
%%
