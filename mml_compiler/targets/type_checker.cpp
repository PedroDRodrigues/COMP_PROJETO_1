#include <string>
#include <sstream>
#include "targets/type_checker.h"
#include ".auto/all_nodes.h"  // automatically generated
#include <cdk/types/primitive_type.h>

#include <mml_parser.tab.h>

#define ASSERT_UNSPEC { if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC)) return; }

//---------------------------------------------------------------------------
//              AUXILIAR FUNCTIONS  
//---------------------------------------------------------------------------

static bool pointer_types_checker(std::shared_ptr<cdk::basic_type> lval_type, std::shared_ptr<cdk::basic_type> rval_type) {
  auto lt = lval_type;
  auto rt = rval_type;
  while (lt->name() == cdk::TYPE_POINTER && rt->name() == cdk::TYPE_POINTER && rt != nullptr) 
  {
    lt = cdk::reference_type::cast(lt)->referenced();
    rt = cdk::reference_type::cast(rt)->referenced();
  }
  //Return true if right value is null or both types are equal
  return rval_type == nullptr || lval_type->name() == rval_type->name();
}

static bool function_types_checker(std::shared_ptr<cdk::functional_type> lval_type, std::shared_ptr<cdk::functional_type> rval_type) {
  if (lval_type->output(0)->name() == cdk::TYPE_DOUBLE) 
  {
    if (!(((rval_type->output(0)->name() == cdk::TYPE_DOUBLE) || rval_type->output(0)->name() == cdk::TYPE_INT) )) 
    {
      return false;
    }
  } 
  else if ((lval_type->output(0)->name() != rval_type->output(0)->name())) 
  {
    return false;
  } 
  else if (lval_type->output(0)->name() == cdk::TYPE_POINTER) 
  {
    if (!(rval_type->output(0)->name() == cdk::TYPE_POINTER && pointer_types_checker(lval_type->output(0), rval_type->output(0)))) 
    {
      return false;
    }
  } 
  else if (lval_type->output(0)->name() == cdk::TYPE_FUNCTIONAL) 
  {
    if (!(rval_type->output(0)->name() == cdk::TYPE_FUNCTIONAL 
          && function_types_checker(cdk::functional_type::cast(lval_type->output(0)), cdk::functional_type::cast(rval_type->output(0))))) 
    {
      return false;
    }
  } 
  else if (lval_type->input_length() != rval_type->input_length()) 
  {
    return false;
  } 
  else {
    for (size_t i = 0; i < lval_type->input_length(); i++) 
    {
      if (rval_type->input(i)->name() == cdk::TYPE_DOUBLE) 
      {
        if ((!((lval_type->input(i)->name() == cdk::TYPE_INT) || (lval_type->input(i)->name() == cdk::TYPE_DOUBLE)))) 
        {
          return false;
        }
      } 
      else if (lval_type->input(i)->name() == cdk::TYPE_POINTER) 
      {
        if (!(rval_type->input(i)->name() == cdk::TYPE_POINTER && pointer_types_checker(lval_type->input(i), rval_type->input(i)))) {
          return false;
        }
      } 
      else if (lval_type->input(i)->name() == cdk::TYPE_FUNCTIONAL) 
      {
        if (!(rval_type->input(i)->name() == cdk::TYPE_FUNCTIONAL && function_types_checker(cdk::functional_type::cast(lval_type->input(i)), cdk::functional_type::cast(rval_type->input(i))))) 
        {
          return false;
        }
      } 
      else if ((lval_type->input(i)->name() != rval_type->input(i)->name())) 
      {
        return false;
      }
    }
  }

  return true;

}

//---------------------------------------------------------------------------

void mml::type_checker::do_sequence_node(cdk::sequence_node *const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++)
    node->node(i)->accept(this, lvl);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_nil_node(cdk::nil_node *const node, int lvl) {
 // EMPTY
}

void mml::type_checker::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void mml::type_checker::do_integer_node(cdk::integer_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_double_node(cdk::double_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
}

void mml::type_checker::do_string_node(cdk::string_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}

void mml::type_checker::do_null_node(mml::null_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::reference_type::create(4, nullptr));
}

// ---------------------------------------------------------------------------

void mml::type_checker::do_not_node(cdk::not_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (!node->argument()->is_typed(cdk::TYPE_INT)) 
  {
    throw std::string("wrong type in argument of not expression");
  }
  node->type(node->argument()->type());
}

void mml::type_checker::do_neg_node(cdk::neg_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (!(node->argument()->is_typed(cdk::TYPE_INT) || node->argument()->is_typed(cdk::TYPE_DOUBLE))) 
  {
    throw std::string("wrong type in argument of neg expression");
  }
  node->type(node->argument()->type());
}

void mml::type_checker::do_identity_node(mml::identity_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (!(node->argument()->is_typed(cdk::TYPE_INT) || node->argument()->is_typed(cdk::TYPE_DOUBLE))) 
  {
    throw std::string("wrong type in argument of identity expression");
  }
  node->type(node->argument()->type());
}

//---------------------------------------------------------------------------

void mml::type_checker::processBinaryExpression_PID(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);
  if (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) 
  {
    node->type(node->left()->type());
  } 
  else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_POINTER)) 
  { 
    node->type(node->right()->type());
  }
  else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_INT)) 
  {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
  else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) 
  {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  }
  else if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_DOUBLE)) 
  {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  }
  else if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) 
  {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  }
  else if (node->left()->is_typed(cdk::TYPE_UNSPEC) && node->right()->is_typed(cdk::TYPE_UNSPEC)) 
  {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
  else 
  {
    throw std::string("wrong types in arguments of binary expression PID");
  }
}

void mml::type_checker::processBinaryExpression_ID(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_INT)) 
  {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
  else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) 
  {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  }
  else if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_DOUBLE)) 
  {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  }
  else if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) 
  {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  }
  else if (node->left()->is_typed(cdk::TYPE_UNSPEC) && node->right()->is_typed(cdk::TYPE_UNSPEC)) 
  {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
  else 
  {
    throw std::string("wrong types in arguments of binary expression ID");
  }

}

void mml::type_checker::processBinaryExpression_I(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  
  node->left()->accept(this, lvl + 2);
  if (!node->left()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in left argument of binary expression");

  node->right()->accept(this, lvl + 2);
  if (!node->right()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in right argument of binary expression");

  // in MML, expressions are always int
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

//---------------------------------------------------------------------------

void mml::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
  processBinaryExpression_PID(node, lvl);
}

void mml::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) {
  processBinaryExpression_PID(node, lvl);
}

void mml::type_checker::do_mul_node(cdk::mul_node *const node, int lvl) {
  processBinaryExpression_ID(node, lvl);
}

void mml::type_checker::do_div_node(cdk::div_node *const node, int lvl) {
  processBinaryExpression_ID(node, lvl);
}

void mml::type_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
  processBinaryExpression_I(node, lvl);
}

//---------------------------------------------------------------------------

void mml::type_checker::processBinaryExpression_ALL(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  
  node->left()->accept(this, lvl + 2);
  if (!node->left()->is_typed(cdk::TYPE_INT)) 
  {
    throw std::string("wrong type in left argument of binary expression");
  }

  node->right()->accept(this, lvl + 2);
  if (!node->right()->is_typed(cdk::TYPE_INT)) 
  {
    throw std::string("wrong type in right argument of binary expression");
  }

  // in MML, expressions are always int
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::processBinaryExpression_EQUAL(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);
  if (node->left()->type() != node->right()->type()) 
  {
    throw std::string("types not equal in equal or not equal expression");
  }

  // in MML, expressions are always int
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

//---------------------------------------------------------------------------

void mml::type_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
  processBinaryExpression_ALL(node, lvl);
}

void mml::type_checker::do_le_node(cdk::le_node *const node, int lvl) {
  processBinaryExpression_ALL(node, lvl);
}

void mml::type_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
  processBinaryExpression_ALL(node, lvl);
}

void mml::type_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
  processBinaryExpression_ALL(node, lvl);
}

void mml::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
  processBinaryExpression_ALL(node, lvl);
}

void mml::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
  processBinaryExpression_ALL(node, lvl);
}

void mml::type_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
  processBinaryExpression_EQUAL(node, lvl);
}

void mml::type_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
  processBinaryExpression_EQUAL(node, lvl);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_variable_node(cdk::variable_node *const node, int lvl) {
  ASSERT_UNSPEC;
  const std::string &id = node->name();
  std::shared_ptr<mml::symbol> symbol = _symtab.find(id);
  if (symbol != nullptr) 
  {
    node->type(symbol->type());
  } 
  else 
  {
    throw id;
  }
}

void mml::type_checker::do_index_node(mml::index_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->base()->accept(this, lvl + 2);
  
  std::shared_ptr<cdk::reference_type> auxBaseType = cdk::reference_type::cast(node->base()->type());
  if (!node->base()->is_typed(cdk::TYPE_POINTER)) throw std::string("pointer expression expected in index left-value");

  node->index()->accept(this, lvl + 2);
  if (!node->index()->is_typed(cdk::TYPE_INT)) throw std::string("integer expression expected in left-value index");

  node->type(auxBaseType->referenced());
}

void mml::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  ASSERT_UNSPEC;
  try {
    node->lvalue()->accept(this, lvl);
    node->type(node->lvalue()->type());
  } catch (const std::string &id) {
    throw "undeclared variable '" + id + "'";
  }
}

void mml::type_checker::do_assignment_node(cdk::assignment_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->lvalue()->accept(this, lvl + 4);
  node->rvalue()->accept(this, lvl + 4);

  if (node->lvalue()->is_typed(cdk::TYPE_INT)) 
  {
    if (node->rvalue()->is_typed(cdk::TYPE_INT)) 
    {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) 
    {       
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } 
    else 
    {
      throw std::string("wrong assignment to integer");
    }
  } 
  else if (node->lvalue()->is_typed(cdk::TYPE_DOUBLE)) 
  {
    if (node->rvalue()->is_typed(cdk::TYPE_DOUBLE) || node->rvalue()->is_typed(cdk::TYPE_INT)) 
    {
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    } 
    else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) 
    {            
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
      node->rvalue()->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    } 
    else 
    {
      throw std::string("wrong assignment to real");
    }
  } 
  else if (node->lvalue()->is_typed(cdk::TYPE_STRING)) 
  {
    if (node->rvalue()->is_typed(cdk::TYPE_STRING)) 
    {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
    } 
    else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) 
    {           
      node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
      node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
    } 
    else 
    {
      throw std::string("wrong assignment to string");
    }
  } 
  else if (node->lvalue()->is_typed(cdk::TYPE_POINTER)) 
  {
    if (node->rvalue()->is_typed(cdk::TYPE_POINTER)) 
    {
      if (!(pointer_types_checker(node->lvalue()->type(), node->rvalue()->type()))) 
      {
        throw std::string("wrong assignment to pointer.");
      }
      node->type(node->rvalue()->type());
    } 
    else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) 
    {             
      node->type(cdk::primitive_type::create(4, cdk::TYPE_ERROR));
      node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_ERROR));
    } 
    else 
    {
      throw std::string("wrong assignment to pointer");
    }
  } 
  else if (node->lvalue()->is_typed(cdk::TYPE_FUNCTIONAL))
  {  
    if (node->rvalue()->is_typed(cdk::TYPE_FUNCTIONAL)) {
      if (!(function_types_checker(cdk::functional_type::cast(node->lvalue()->type()), 
            cdk::functional_type::cast(node->rvalue()->type())) || (node->rvalue()->is_typed(cdk::TYPE_POINTER) 
            && cdk::reference_type::cast(node->rvalue()->type())->referenced() == nullptr))) 
      {   
        throw std::string("wrong type for initializer (function expected).");
      }
      node->type(node->rvalue()->type());
    } 
    else if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) 
    {              
      node->type(cdk::primitive_type::create(4, cdk::TYPE_ERROR));
      node->rvalue()->type(cdk::primitive_type::create(4, cdk::TYPE_ERROR));
    } 
    else 
    {
      throw std::string("wrong assignment to function");
    }
  } 
  else 
  {
    throw std::string("wrong types in assignment");
  }
}

void mml::type_checker::do_declaration_node(mml::declaration_node *const node, int lvl) {  
  if (node->initializer() != nullptr) 
  {
    node->initializer()->accept(this, lvl + 2);
    if (node->type()) 
    {
      if (node->is_typed(cdk::TYPE_INT)) 
      {
        if (!node->initializer()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type for initializer (integer expected).");
      } 
      else if (node->is_typed(cdk::TYPE_DOUBLE)) 
      {
        if (!node->initializer()->is_typed(cdk::TYPE_INT) && !node->initializer()->is_typed(cdk::TYPE_DOUBLE)) 
        {
          throw std::string("wrong type for initializer (integer or double expected).");
        }
      } 
      else if (node->is_typed(cdk::TYPE_STRING)) 
      {
        if (!node->initializer()->is_typed(cdk::TYPE_STRING)) 
        {
          throw std::string("wrong type for initializer (string expected).");
        }
      } 
      else if (node->is_typed(cdk::TYPE_POINTER)) 
      {
        if (!(node->initializer()->is_typed(cdk::TYPE_POINTER) && 
          pointer_types_checker(node->type(), node->initializer()->type()))) 
        {
          throw std::string("wrong type for initializer (pointer expected).");
        }
      } 
      else if (node->is_typed(cdk::TYPE_FUNCTIONAL)) 
      {  // f = function or f = nullptr
        if (!((node->initializer()->is_typed(cdk::TYPE_FUNCTIONAL) && 
            function_types_checker(cdk::functional_type::cast(node->type()), cdk::functional_type::cast(node->initializer()->type())))
            || ((node->initializer()->is_typed(cdk::TYPE_POINTER) && cdk::reference_type::cast(node->initializer()->type())->referenced() == nullptr)))) 
        {
          throw std::string("wrong type for initializer (function expected).");
        }
      }
      else 
      { 
        throw std::string("unknown type for initializer."); 
      }
    } 
    else 
    {
      node->type(node->initializer()->type());
    }
  }

  std::string id;
  if (id == "_main") 
  {
    id = "._main";
  } 
  else 
  {
    id = node->identifier();
  }

  auto symbol = mml::make_symbol(node->type(), id, (bool)node->initializer(), node->qualifier());
  std::shared_ptr<mml::symbol> previous = _symtab.find_local(symbol->name());

  // Variable redeclaration
  if (previous) 
  { 
    if (previous->type()->name() == cdk::TYPE_POINTER && 
        symbol->type()->name() == cdk::TYPE_POINTER && 
        pointer_types_checker(previous->type(), symbol->type())) 
    {
      _symtab.replace(symbol->name(), symbol);
    } 
    else if (previous->type()->name() == cdk::TYPE_FUNCTIONAL && symbol->type()->name() == cdk::TYPE_FUNCTIONAL && 
        function_types_checker(cdk::functional_type::cast(previous->type()), cdk::functional_type::cast(symbol->type()))) 
    {
      _symtab.replace(symbol->name(), symbol);
    } 
    else if (previous->type()->name() == symbol->type()->name()) 
    {
      _symtab.replace(symbol->name(), symbol);
    } 
    else 
    {
      throw std::string(id + " was redefined.");
    }
  } 
  else 
  {
    _symtab.insert(id, symbol);
  } 

  _parent->set_new_symbol(symbol);

  if (node->qualifier() == tFOREIGN) 
  {
    symbol->set_foreign(true);
  }
}

//---------------------------------------------------------------------------

void mml::type_checker::do_evaluation_node(mml::evaluation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
}

void mml::type_checker::do_input_node(mml::input_node *const node, int lvl) {
  node->type(cdk::primitive_type::create(0, cdk::TYPE_UNSPEC));
}

void mml::type_checker::do_print_node(mml::print_node *const node, int lvl) {
  node->arguments()->accept(this, lvl + 2);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_address_of_node(mml::address_of_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->lvalue()->accept(this, lvl + 4);
  node->type(cdk::reference_type::create(4, node->lvalue()->type()));
}

void mml::type_checker::do_stack_alloc_node(mml::stack_alloc_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->argument()->accept(this, lvl + 4);
  if (!node->argument()->is_typed(cdk::TYPE_INT)) 
  {
    throw std::string("integer expression expected in allocation expression");
  }

  node->type(cdk::reference_type::create(4, cdk::primitive_type::create(8, cdk::TYPE_DOUBLE)));
}

//---------------------------------------------------------------------------
//Need to understand what this does and the types that condition can have
void mml::type_checker::do_while_node(mml::while_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (!node->condition()->is_typed(cdk::TYPE_INT)) 
    throw std::string("expected integer condition");
}

//---------------------------------------------------------------------------

void mml::type_checker::do_if_node(mml::if_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (!node->condition()->is_typed(cdk::TYPE_INT)) 
    throw std::string("expected integer condition");
}

void mml::type_checker::do_if_else_node(mml::if_else_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (!node->condition()->is_typed(cdk::TYPE_INT)) 
    throw std::string("expected integer condition");
}

//---------------------------------------------------------------------------

void mml::type_checker::do_sizeof_node(mml::sizeof_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->expression()->accept(this, lvl + 4);
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_function_call_node(mml::function_call_node *const node, int lvl) {
  ASSERT_UNSPEC;
  std::vector<std::shared_ptr<cdk::basic_type>> input_types;
  std::shared_ptr<cdk::basic_type> output_type;
  
  if (!node->identifier()) 
  {            
    // recursive call 
    auto symbol = _symtab.find("@", 1);
    
    if (symbol->is_main()) 
    {
      throw std::string("recursive call in main function");
    }
    if (symbol == nullptr) 
    {
      throw std::string("recursive call outside function");
    }

    input_types = cdk::functional_type::cast(symbol->type())->input()->components();
    output_type = cdk::functional_type::cast(symbol->type())->output(0);
  } 
  else 
  {
    node->identifier()->accept(this, lvl + 4);
    
    if (!(node->identifier()->type()->name() == cdk::TYPE_FUNCTIONAL)) 
    {
      throw std::string("expected function pointer on function call");
    }
    
    input_types = cdk::functional_type::cast(node->identifier()->type())->input()->components();
    output_type = cdk::functional_type::cast(node->identifier()->type())->output(0);
  }

  node->type(output_type);   

  if (node->arguments()->size() == input_types.size()) 
  {
    node->arguments()->accept(this, lvl + 4);
    
    for (size_t ax = 0; ax < node->arguments()->size(); ax++) 
    {
      cdk::expression_node *expression = dynamic_cast<cdk::expression_node *>(node->argument(ax));
      if (!expression) 
      {
        throw std::string("wrong type in write argument");
      }
            
      if (node->argument(ax)->is_typed(cdk::TYPE_POINTER) && input_types[ax]->name() == cdk::TYPE_POINTER) 
      {
        mml::null_node *null = dynamic_cast<mml::null_node *>(node->argument(ax));
        if (!null) 
        {
          pointer_types_checker(cdk::reference_type::cast(node->argument(ax)->type()), cdk::reference_type::cast(input_types[ax]));
        }
        continue;
      } 
      else if (node->argument(ax)->is_typed(cdk::TYPE_UNSPEC) && (input_types[ax]->name() == cdk::TYPE_INT || input_types[ax]->name() == cdk::TYPE_DOUBLE)) 
      {
        mml::input_node *input = dynamic_cast<mml::input_node *>(node->argument(ax));
        if (input) 
        {
            node->argument(ax)->type(input_types[ax]);
            continue;
        }
        else 
        {
          throw std::string("WARNING: unknown node with unspecified type");
        }
      } 
      else if(node->argument(ax)->is_typed(cdk::TYPE_UNSPEC) && input_types[ax]->name() == cdk::TYPE_POINTER) 
      {
          mml::stack_alloc_node *stack_alloc = dynamic_cast<mml::stack_alloc_node *>(node->argument(ax));
          if (stack_alloc) 
          {
              node->argument(ax)->type(input_types[ax]);
              continue;
          } 
          else 
          {
              throw std::string("WARNING: unknown node with unspecified type");
          }  
      } 
      else if (node->argument(ax)->type()->name() == input_types[ax]->name()) 
      {
        continue;
      } 
      else  if (input_types[ax]->name() == cdk::TYPE_DOUBLE && node->argument(ax)->is_typed(cdk::TYPE_INT)) 
      {
        continue;
      } 
      else 
          throw std::string("type mismatch for argument " + std::to_string(ax + 1) + ".");
    }
  } 
  else 
  {
    throw std::string("number of arguments in call (" + std::to_string(node->arguments()->size()) 
        + ") must match declaration (" + std::to_string(input_types.size()) + ").");
  }        
}

void mml::type_checker::do_function_definition_node(mml::function_definition_node *const node, int lvl) {
  if (node->main()) {
    auto cdkInt = cdk::primitive_type::create(4, cdk::TYPE_INT);
    auto mainfun = mml::make_symbol(cdk::functional_type::create(cdkInt), "_main", 0, tPRIVATE);
    mainfun->set_main(true);

    if (_symtab.find_local(mainfun->name())) 
    {
      _symtab.replace(mainfun->name(), mainfun);
    } 
    else 
    {
      if (!_symtab.insert(mainfun->name(), mainfun)) 
      {
        std::cerr << "ERROR INSERTING MAIN @" << std::endl;
        return;
      }
    }
    _parent->set_new_symbol(mainfun);
  } 
  else {
    std::vector<std::shared_ptr<cdk::basic_type>> input_types;
    for (size_t ax = 0; ax < node->arguments()->size(); ax++) 
    {
      input_types.push_back(node->argument(ax)->type());
    }
    node->type(cdk::functional_type::create(input_types, node->type()));

    auto function = mml::make_symbol(node->type(), "@", 0, tPRIVATE);

    if (_symtab.find_local(function->name())) 
    {
      _symtab.replace(function->name(), function);
    } 
    else 
    {
      if (!_symtab.insert(function->name(), function)) 
      {
        std::cerr << "ERROR INSERTING FUNCTION @" << std::endl;
        return;
      }
    }
    _parent->set_new_symbol(function);
  }
}
 
//---------------------------------------------------------------------------

void mml::type_checker::do_return_node(mml::return_node *const node, int lvl) {
  auto symbol = _symtab.find("@", 1);
  if (symbol == nullptr) 
  {
    symbol = _symtab.find("_main", 0);
    if (symbol == nullptr) 
    {
      throw std::string("return statement outside program block");
    } else {

      if(node->retval()) 
      {
        node->retval()->accept(this, lvl + 4);
      }

      if (!node->retval()->is_typed(cdk::TYPE_INT)) 
      {
        throw std::string("wrong type for program return expression (integer expected).");
      }

    }
  } 
  else {
    if (node->retval()) 
    {
      std::shared_ptr<cdk::functional_type> returnType = cdk::functional_type::cast(symbol->type());
      if (returnType->output() != nullptr && returnType->output(0)->name() == cdk::TYPE_VOID) 
      {
        throw std::string("return value specified for void function.");
      }

      node->retval()->accept(this, lvl + 4);

      if (returnType->output() != nullptr && returnType->output(0)->name() == cdk::TYPE_INT) 
      {
        if (!node->retval()->is_typed(cdk::TYPE_INT)) 
        {
          throw std::string("wrong type for return expression (integer expected).");
        }
      } 
      else if (returnType->output() != nullptr && returnType->output(0)->name() == cdk::TYPE_DOUBLE) 
      {
        if (!node->retval()->is_typed(cdk::TYPE_INT) && !node->retval()->is_typed(cdk::TYPE_DOUBLE)) 
        {
          throw std::string("wrong type for return expression (integer or double expected).");
        }
      } 
      else if (returnType->output() != nullptr && returnType->output(0)->name() == cdk::TYPE_STRING) 
      {
        if (!node->retval()->is_typed(cdk::TYPE_STRING)) 
        {
          throw std::string("wrong type for return expression (string expected).");
        }
      }
      else if (returnType->output() != nullptr && returnType->output(0)->name() == cdk::TYPE_POINTER) 
      {
        if (node->retval()->is_typed(cdk::TYPE_POINTER)) 
        {
          if (!(pointer_types_checker(returnType->output(0), node->retval()->type()))) 
          {
            throw std::string("wrong type for return expression (pointer expected).");
          }
        }
      }  
      else if (returnType->output() != nullptr && returnType->output(0)->name() == cdk::TYPE_FUNCTIONAL) 
      {
        node->retval()->accept(this, lvl + 4);
        if (node->retval()->is_typed(cdk::TYPE_FUNCTIONAL)) 
        {
          if (!(function_types_checker(cdk::functional_type::cast(returnType->output(0)), 
              cdk::functional_type::cast(node->retval()->type())) || (node->retval()->is_typed(cdk::TYPE_POINTER) && 
                cdk::reference_type::cast(node->retval()->type())->referenced() == nullptr))) 
          {
            throw std::string("wrong type for return expression (function expected).");
          }
        }
      } 
      else 
      {
        throw std::string("unknown type for return expression.");
      }
    }
  }
}

//---------------------------------------------------------------------------

void mml::type_checker::do_block_node(mml::block_node *const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void mml::type_checker::do_stop_node(mml::stop_node *const node, int lvl) {
  // EMPTY
}

void mml::type_checker::do_next_node(mml::next_node *const node, int lvl) {
  // EMPTY
}