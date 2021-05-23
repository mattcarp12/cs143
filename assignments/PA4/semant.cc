

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

extern int semant_debug;
extern char *curr_filename;
ClassTable *e = new ClassTable();

void check() {
  if (e->errors())
  {
    cerr << "Compilation halted due to static semantic errors." << endl;
    exit(1);
  }
}

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
    arg = idtable.add_string("arg");
    arg2 = idtable.add_string("arg2");
    Bool = idtable.add_string("Bool");
    concat = idtable.add_string("concat");
    cool_abort = idtable.add_string("abort");
    copy = idtable.add_string("copy");
    Int = idtable.add_string("Int");
    in_int = idtable.add_string("in_int");
    in_string = idtable.add_string("in_string");
    IO = idtable.add_string("IO");
    length = idtable.add_string("length");
    Main = idtable.add_string("Main");
    main_meth = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any
    //   user-defined class.
    No_class = idtable.add_string("_no_class");
    No_type = idtable.add_string("_no_type");
    Object = idtable.add_string("Object");
    out_int = idtable.add_string("out_int");
    out_string = idtable.add_string("out_string");
    prim_slot = idtable.add_string("_prim_slot");
    self = idtable.add_string("self");
    SELF_TYPE = idtable.add_string("SELF_TYPE");
    Str = idtable.add_string("String");
    str_field = idtable.add_string("_str_field");
    substr = idtable.add_string("substr");
    type_name = idtable.add_string("type_name");
    val = idtable.add_string("_val");
}

ClassTable::ClassTable() : semant_errors(0), error_stream(cerr)
{
}



void install_basic_classes(SymbolTable<Symbol,Class__class> *gst)
{

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
        class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename);



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




    /* Add base classes to symbol table */
    gst->addid(Object,Object_class);
    gst->addid(Str,Str_class);
    gst->addid(Bool,Bool_class);
    gst->addid(Int,Int_class);
    gst->addid(IO,IO_class);
    gst->addid(SELF_TYPE,Object_class);
    
}

bool ClassTable::install_program_classes(Classes classes) {
  return TRUE;
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

ostream &ClassTable::semant_error(Class_ c)
{
    return semant_error(c->get_filename(), c);
}

ostream &ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream &ClassTable::semant_error()
{
    semant_errors++;
    return error_stream;
}




void program_class::def() {
  gst = new SymbolTable<Symbol,Class__class>();
  gst->enterscope();
  install_basic_classes(gst);

  for( int i = classes->first(); classes->more(i); i = classes->next(i)) { 
    Class_ c = classes->nth(i);
    Symbol cs = c->get_name();
    if (gst->probe(cs) != NULL) {
      e->semant_error(c);
      cerr << "Redefinition of basic class " << cs << "." << endl;
    }
    Symbol parent = c->get_parent();
    if (parent == Bool || parent == SELF_TYPE || parent == Str) {
      e->semant_error(c);
      cerr << "Class " << cs << " cannot inherit class " << parent << "." << endl;
    }
    gst->addid(cs,c);
  }
}

void class__class::def(SymbolTable<Symbol,Class__class> *gst) {
  /*

    The symbol table is to be constructed recursively.

    The base condition is the class's parent symbol table is already on the stack. 

    How to check?

    Ok so the built-in class scope trees are built. And I'm certain my parent class exists
    and there is no cycle. Sitting pretty. Now I just need to:
      - Find my parent class's node by probing global symbol table (gst)
      - Get my parent's scope
      - Push, assign, pop scope to myself.

    After all that is done, I've finally created my class scopes (mscope and oscope).

    Next I need to recursively call def() on each of my features.

    The feature will install itself in the class scope. The AST node will know whether to
    add itself to mscope or oscope.

   */

  if ( name == Object ) {
    gst->enterscope();
    mscope = *gst;
    gst->exitscope();
    gst->enterscope();
    oscope = *gst;
    gst->exitscope();
  } else {
    // Check if parent scope is created
    Class_ parent_class = gst->probe(parent);
    Class_ self_class = parent_class->oscope.probe(self);
    if (self_class == NULL) {
      parent_class->def(gst);
    }
    // if (parent_class == NULL) {
    //   parent_class->def(gst);
    // }
    // At this point we know the parent class's symbol table has been defined and populated
    parent_class->mscope.enterscope();
    mscope = parent_class->mscope;
    parent_class->mscope.exitscope();
    parent_class->oscope.enterscope();
    oscope = parent_class->oscope;
    parent_class->oscope.exitscope();
  }

  // Add builtin symbols
  oscope.addid(self, this);
  oscope.addid(SELF_TYPE, this);
  
  // Add features
  for (int j = features->first(); features->more(j); j = features->next(j)) {
    features->nth(j)->def(this);
  }
}

void method_class::def(Class_ parent) {
  Class_ method_type;
  if (return_type == SELF_TYPE) {
    method_type = parent->oscope.lookup(self);
  } else {
    // Check return type exists
    method_type = parent->mscope.lookup(return_type);
    if (method_type == NULL) {
      e->semant_error(parent);
      cerr << "Undefined return type " << return_type << " in method " << name << "." << endl;
    }
  }
  // Add self to owning class's mscope
  
  parent->mscope.addid(name, method_type);
  mscope = parent->mscope;
  
  // Push scope to oscope for method formals
  parent->oscope.enterscope();
  oscope = parent->oscope;
  parent->oscope.exitscope();
  for ( int i = formals->first(); formals->more(i); i = formals->next(i) ) {
    Formal f = formals->nth(i);
    if (f->get_name() == self) {
      e->semant_error(oscope.lookup(self));
      cerr << "'self' cannot be the name of a formal parameter." << endl;
    }
    if (f->get_type() == SELF_TYPE) {
      e->semant_error(oscope.lookup(self));
      cerr << "Formal parameter " << f->get_name() << " cannot have type SELF_TYPE." << endl;
    }
    formals->nth(i)->def(&oscope);
  }

  expr->def();
}

void attr_class::def(Class_ parent) {

  // Check symbol already defined (error)
  if ( parent->oscope.lookup(name) != NULL ) {
    e->semant_error(parent);
    cerr << "'" << name << "' cannot be the name of an attribute." << endl;
  }

  // Add myself to the class's symbol table
  Class_ attr_type = parent->oscope.lookup(type_decl);
  parent->oscope.addid(name, attr_type);
  oscope = parent->oscope;
  mscope = parent->mscope;
}

void formal_class::def(SymbolTable<Symbol,Class__class> *oscope) {
  Class_ formal_type = oscope->lookup(type_decl);
  oscope->addid(name, formal_type);
}

void branch_class::def() {}
void assign_class::def() {}
void static_dispatch_class::def() {}
void dispatch_class::def() {}
void cond_class::def() {}
void loop_class::def() {}
void typcase_class::def() {}
void block_class::def() {}
void let_class::def() {}
void plus_class::def() {}
void sub_class::def() {}
void mul_class::def() {}
void divide_class::def() {}
void neg_class::def() {}
void lt_class::def() {}
void eq_class::def() {}
void leq_class::def() {}
void comp_class::def() {}
void int_const_class::def() {}
void bool_const_class::def() {}
void string_const_class::def() {}
void new__class::def() {}
void isvoid_class::def() {}
void no_expr_class::def() {}
void object_class::def() {}










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
void program_class::semant(){
    initialize_constants();

    // Define class symbols in root of symbol tree
    this->def();

    /*

      Perform semantic analysis on class list:
        X Check for Main class
	X Check each parent class is defined
	X Check for inheritance cycle
	- Check no class inherits from Int, Bool, Str
	- Check no class is redefined

     */

    bool main = FALSE;
    for ( int i = classes->first(); classes->more(i); i = classes->next(i)) {
      Class_ c = classes->nth(i);
      Symbol cs = c->get_name();
      if (cs == Main) main = TRUE;
    }
    if (!main) {
      e->semant_error();
      cerr << "Class Main is not defined." << endl;
    }

        
  /* Make sure class inheriting from is defined in program */
  for( int i = classes->first(); classes->more(i); i = classes->next(i)) {
    Class_ c = classes->nth(i);
    Symbol parent_symbol = c->get_parent();
    Class_ parent_class = gst->lookup(parent_symbol);
    if (parent_class == NULL) {
      e->semant_error(c);  
      cerr << parent_symbol << ", parent class of " << c->get_name() << " , is undefined.\n"; 
    } check();
  }
  

  /* Check inheritance graph for cycles */
  for( int i = classes->first(); classes->more(i); i = classes->next(i)) {
    Class_ c = classes->nth(i);
    Symbol parent_symbol = c->get_parent();
    Classes class_stack = single_Classes(c);
    while(parent_symbol != Object) {
      /* Look to see if parent is already on the stack */
      for (int j = class_stack->first(); class_stack->more(j); j = class_stack->next(j)) {
    	if (parent_symbol == class_stack->nth(j)->get_name()) {
    	  e->semant_error(c);
          cerr << "Cycle detected in inheritance graph\n";
    	}
      } check();

      /* Otherwise add next inherited class to stack */
      Class_ parent_class = gst->probe(parent_symbol);
      class_stack = append_Classes(class_stack, single_Classes(parent_class));
      parent_symbol = parent_class->get_parent();
    }
  }

  check();


  /*

    At this point we know the class inheritance graph is well formed.
    We can carry on def'ing and ref'ing without a care in the world.
    First we need to manually construct the scope tree starting with 
    Object class and adding the other built-in classes.

   */

  gst->probe(Object)->def(gst);
  gst->probe(IO)->def(gst);
  gst->probe(Int)->def(gst);
  gst->probe(Bool)->def(gst);
  gst->probe(Str)->def(gst);
   
  for ( int i = classes->first(); classes->more(i); i = classes->next(i)) {
    Class_ c = classes->nth(i);
    c->def(gst);
  }
  //check();
     
  for( int i = classes->first(); classes->more(i); i = classes->next(i)) { 
    classes->nth(i)->semant();
  }
  check();

  dump_with_types(cout,0);

  /* some semantic analysis code may go here */
  check();

}


void class__class::semant() {
  /*

    Class semantic analysis:
    1. Create new symbol table to track variable names
    2. Check feature types exist in global symbol table
    3. Add attributes to symbol table
    4. Perform analysis on features

   */

  for ( int i = features->first(); features->more(i); i = features->next(i)) {
    Feature f = features->nth(i);
    f->semant();
  }

}

void method_class::semant() {
  /*

    Method semantic analysis:
    1. Push new scope to scope stack
    2. Evaluate formals and add to scope
    3. Evaluate expression
    4. Check expression type and method return type agree


   */
  
  // Evaluate formals
  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    Formal f = formals->nth(i);
    f->oscope = oscope;
    f->mscope = mscope;
    f->semant();
  }

  expr->oscope = oscope;
  expr->mscope = mscope;
  expr->semant();

}


void attr_class::semant() {

  //cout << "Attribute : " << name << endl;
  
  init->oscope = oscope;
  init->mscope = mscope;
  init->semant();
  // Check types agree

}


void formal_class::semant() {
 
}
 

void branch_class::semant() {

  expr->oscope = oscope;
  expr->mscope = mscope;
  expr->semant();
  // set_type();

}


void assign_class::semant() {

  //  cout << "Assigning to : " << name << endl;
  if (name == self) {
    Class_ cc = oscope.lookup(self);
    e->semant_error(cc);
    cerr << "Cannot assign to 'self'." << endl;
    return;
  }

  expr->oscope = oscope;
  expr->mscope = mscope;
  expr->semant();

  //cout << "Identifier type: " << oscope.lookup(name)->get_name() << endl;

  // Check type conformance
  Class_ id_type = oscope.lookup(name);
  if (id_type->get_name() != expr->get_type()) {
    e->semant_error(oscope.lookup(self));
    cerr << "Type " << expr->get_type() << " of assigned expression does not conform to declared type " << id_type->get_name() << " of identifier " << name << "." << endl;
  }
  // Set type to expression type
  set_type(expr->get_type());
  
}


void static_dispatch_class::semant() {

  expr->oscope = oscope;
  expr->mscope = mscope;
  expr->semant();
  // Check expression type is subtype of 'type_name'
  

  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    Expression exp = actual->nth(i);
    exp->oscope = oscope;
    exp->mscope = mscope;
    exp->semant();
  }

  // Lookup type for 'name'

  Class_ type = oscope.lookup(type_name);

  Class_ ret_type = type->mscope.lookup(name);

  set_type(ret_type->get_name());
  
}


void dispatch_class::semant() {
  expr->oscope = oscope;
  expr->mscope = mscope;
  expr->semant();

  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    Expression exp = actual->nth(i);
    exp->oscope = oscope;
    exp->mscope = mscope;
    exp->semant();
  }
  
      
  // Get expression type
  Class_ exp_type = oscope.lookup(expr->get_type());
  Symbol type_symbol;
  Class_ ret_type = exp_type->mscope.lookup(name);
  // Check if method is defined on expression's type
  if (ret_type == NULL) {
    e->semant_error(oscope.lookup(self));
    cerr << "Dispatch to undefined method " << name << "." << endl;
    type_symbol = Object;
  }else {
    type_symbol = ret_type->get_name();
  }
  //if (expr->get_type() == SELF_TYPE)
  //type_symbol = SELF_TYPE;
  set_type(type_symbol);    
}


void cond_class::semant() {
  pred->oscope = oscope;
  pred->mscope = mscope;
  pred->semant();
  // Check pred has type Bool
  if (pred->get_type() != Bool){
    Class_ parent = oscope.lookup(self);
    e->semant_error(parent);
    cerr << "If predicate must be type Bool" << endl;
  }

  then_exp->oscope = oscope;
  then_exp->mscope = mscope;
  then_exp->semant();

  else_exp->oscope = oscope;
  else_exp->mscope = mscope;
  else_exp->semant();

  // Set type to lub() of two above types
  set_type(Object);

}


void loop_class:: semant() {
  pred->oscope = oscope;
  pred->mscope = mscope;
  pred->semant();

  if (pred->get_type() != Bool) {
    Class_ parent = oscope.lookup(self);
    e->semant_error(parent);
    cerr << "Loop predicate must be type Bool" << endl;
  }

  body->oscope = oscope;
  body->mscope = mscope;
  body->semant();

  // Set type to Object always
  set_type(Object);

}


void typcase_class::semant() {
  expr->oscope = oscope;
  expr->mscope = mscope;
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    Case cas = cases->nth(i);
    cas->oscope = oscope;
    cas->mscope = mscope;
    cas->semant();
  }
  set_type(Object);
}


void block_class::semant() {
  Expression exp = NULL;
  int i = 0;
  for ( i = body->first(); body->more(i); i = body->next(i) ) {
    exp = body->nth(i);
    exp->oscope = oscope;
    exp->mscope = mscope;
    exp->semant();
  }
  set_type(exp->get_type());
}


void let_class::semant() {

  /*

    - Push new scope to stack
    - 
    - Add 'identifier' to scope
    - Assign scope to 'body' expression
    - Evaluate 'body' expression

   */

  init->oscope = oscope;
  init->mscope = mscope;
  init->semant();

  oscope.enterscope();
  body->oscope = oscope;
  oscope.exitscope();
  body->oscope.addid(identifier, oscope.lookup(type_decl));
  body->mscope = mscope;
  body->semant();

  set_type(body->get_type());

}


/*

  Arithmetic and Comparison Operators

 */


void plus_class::semant() {
  e1->oscope = oscope;
  e1->mscope = mscope;
  e1->semant();

  e2->oscope = oscope;
  e2->mscope = mscope;
  e2->semant();

  if (e1->get_type() == Int && e2->get_type() == Int) {
    set_type(Int);
  } else {
    Class_ parent = oscope.lookup(self);
    e->semant_error(parent);
    cerr << "Illegal arithmetic with a basic type." << endl;
  } 
}


void sub_class::semant() {
  e1->oscope = oscope;
  e1->mscope = mscope;
  e1->semant();

  e2->oscope = oscope;
  e2->mscope = mscope;
  e2->semant();

  if (e1->get_type() == Int && e2->get_type() == Int) {
    set_type(Int);
  } else {
    Class_ parent = oscope.lookup(self);
    e->semant_error(parent);
    cerr << "Illegal arithmetic with a basic type." << endl;
  } 
}


void mul_class::semant() {
  e1->oscope = oscope;
  e1->mscope = mscope;
  e1->semant();

  e2->oscope = oscope;
  e2->mscope = mscope;
  e2->semant();

  if (e1->get_type() == Int && e2->get_type() == Int) {
    set_type(Int);
  } else {
    Class_ parent = oscope.lookup(self);
    e->semant_error(parent);
    cerr << "Illegal arithmetic with a basic type." << endl;
  }
}


void divide_class::semant() {
  e1->oscope = oscope;
  e1->mscope = mscope;
  e1->semant();

  e2->oscope = oscope;
  e2->mscope = mscope;
  e2->semant();

  if (e1->get_type() == Int && e2->get_type() == Int) {
    set_type(Int);
  } else {
    Class_ parent = oscope.lookup(self);
    e->semant_error(parent);
    cerr << "Illegal arithmetic with a basic type." << endl;
  }
}


void neg_class::semant() {
  e1->oscope = oscope;
  e1->mscope = mscope;
  e1->semant();
  if (e1->get_type() == Int) {
    set_type(Int);
  } else {
    Class_ parent = oscope.lookup(self);
    e->semant_error(parent);
    cerr << "Illegal arithmetic with a basic type" << endl;
  }

}


void lt_class::semant() {
  
  e1->oscope = oscope;
  e1->mscope = mscope;
  e1->semant();

  e2->oscope = oscope;
  e2->mscope = mscope;
  e2->semant();

  if (e1->get_type() == Int && e2->get_type() == Int) {
    set_type(Bool);
  } else {
    Class_ parent = oscope.lookup(self);
    e->semant_error(parent);
    cerr << "Illegal comparison with a basic type." << endl;
  }
}


void eq_class::semant() {
  
  e1->oscope = oscope;
  e1->mscope = mscope;
  e1->semant();

  e2->oscope = oscope;
  e2->mscope = mscope;
  e2->semant();
  
  if ((e1->get_type() == Int && e2->get_type() != Int) ||
      (e2->get_type() == Int && e1->get_type() != Int) || 
      (e1->get_type() == Bool && e2->get_type() != Bool) || 
      (e2->get_type() == Bool && e1->get_type() != Bool) || 
      (e1->get_type() == Str && e2->get_type() != Str) ||
      (e2->get_type() == Str && e1->get_type() != Str)){
    Class_ parent = oscope.lookup(self);
    e->semant_error(parent);
    cerr << "Illegal comparison with a basic type." << endl;
  } else {
    set_type(Bool);
  }
}


void leq_class::semant() {
 
  e1->oscope = oscope;
  e1->mscope = mscope;
  e1->semant();

  e2->oscope = oscope;
  e2->mscope = mscope;
  e2->semant();

  if (e1->get_type() == Int && e2->get_type() == Int) {
    set_type(Bool);
  } else {
    Class_ parent = oscope.lookup(self);
    e->semant_error(parent);
    cerr << "Illegal comparison with a basic type." << endl;
    set_type(Bool);
  }
}


void comp_class::semant() {
  e1->semant();
  if (e1->get_type() != Bool) {
    Class_ parent = oscope.lookup(self);
    e->semant_error(parent);
    cerr << "Illegal complement operation on class: " << parent->get_name() << endl;
  }
  set_type(Bool);
}








/*

  Primitive Types

 */


void int_const_class::semant() {
  set_type(Int);
}


void bool_const_class::semant() {
  set_type(Bool);
}


void string_const_class::semant() {
  set_type(Str);
}


void new__class::semant() {
  if (type_name == SELF_TYPE)
    set_type(SELF_TYPE);
  else {
    // Check type exists
    Class_ type = oscope.lookup(type_name);
    if (type == NULL) {
      Class_ parent = oscope.lookup(self);
      e->semant_error(parent);
      cerr << "'new' used with undefined class " << type_name << "." << endl;
    }

    set_type(type_name);
    
  }
}


void isvoid_class::semant() {
  e1->oscope = oscope;
  e1->mscope = mscope;
  e1->semant();
  set_type(Bool);
}


void no_expr_class::semant() {
  set_type(No_type);
}


void object_class::semant() {
  // Look up symbol in scope, set to type
  // cout << "Object : " << name << endl;
  // oscope.dump();
  Class_ sc = oscope.lookup(name);
  if (sc == NULL) {
    e->semant_error(oscope.lookup(self));
    cerr << "Undeclared identifier " << name << "." << endl;
    set_type(Object);
  } else {
  // cout << "Class of object : " << name << " is : " << sc->get_name() << endl;
  if ( name == self ) 
    set_type(SELF_TYPE);
  else
    set_type(oscope.lookup(name)->get_name());
  }
}
