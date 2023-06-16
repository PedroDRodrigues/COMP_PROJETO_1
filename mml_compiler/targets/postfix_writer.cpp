#include <string>
#include <sstream>
#include "targets/type_checker.h"
#include "targets/postfix_writer.h"
#include "targets/frame_size_calculator.h"
#include "targets/symbol.h"
#include ".auto/all_nodes.h"  // all_nodes.h is automatically generated

#include <mml_parser.tab.h>
//---------------------------------------------------------------------------
//     HELPER FUNCTIONS
//---------------------------------------------------------------------------

void mml::postfix_writer::do_init_expr(cdk::expression_node * const node, int lvl, std::shared_ptr<mml::symbol> const symbol) {
  //Expressions need to be separated by exprs in body of function and the other
  if (_inFunctionBody)  
  {
    node->accept(this, lvl);
    if (symbol->is_typed(cdk::TYPE_INT) || symbol->is_typed(cdk::TYPE_STRING) || 
        symbol->is_typed(cdk::TYPE_POINTER) || symbol->is_typed(cdk::TYPE_FUNCTIONAL)) {
      _pf.LOCAL(symbol->offset());
      _pf.STINT();
    } else if(symbol->is_typed(cdk::TYPE_DOUBLE)) {
      if (node->is_typed(cdk::TYPE_INT)) {
        _pf.I2D();
      }
      _pf.LOCAL(symbol->offset());
      _pf.STDOUBLE();
    } else {
      std::cerr << "UNKNOWN DECLARATION NODE TYPE" << std::endl;
      return;
    }
  }
  else {
    if (symbol->is_typed(cdk::TYPE_INT) || symbol->is_typed(cdk::TYPE_DOUBLE) || symbol->is_typed(cdk::TYPE_POINTER)) {
      _pf.DATA();
      _pf.ALIGN();
      _pf.LABEL(symbol->name());
      if (symbol->is_typed(cdk::TYPE_INT)) {
        node->accept(this, lvl);
      } else if (symbol->is_typed(cdk::TYPE_POINTER)) {
        node->accept(this, lvl);
      } else if (symbol->is_typed(cdk::TYPE_DOUBLE)) {
        if (node->is_typed(cdk::TYPE_DOUBLE)) {
          node->accept(this, lvl);
        }
        else if (node->is_typed(cdk::TYPE_INT)) {
          cdk::integer_node *dclini = dynamic_cast<cdk::integer_node*>(node);
          cdk::double_node ddi(dclini->lineno(), dclini->value());
          ddi.accept(this, lvl);
        } else {
          std::cerr << "BAD DECLARATION FOR DOUBLE VALUE" << std::endl;
        }
      }
    } else if (symbol->is_typed(cdk::TYPE_STRING)) {
        _pf.DATA();
        _pf.ALIGN();
        _pf.LABEL(symbol->name());
        node->accept(this, lvl);
    } else if (symbol->is_typed(cdk::TYPE_FUNCTIONAL)) {
      _functions_symbols.push_back(symbol);
      reset_new_symbol();
      node->accept(this, lvl);
      _pf.DATA();
      if (_functions_symbols.back()->qualifier() == tPUBLIC) {
        _pf.GLOBAL(_functions_symbols.back()->name(), _pf.OBJ());
      }
      _pf.ALIGN();
      _pf.LABEL(symbol->name());
      std::string label = _function_label;
      _function_label.clear();
      _pf.SADDR(label);
    } else {
      std::cerr << "UNEXPECTED INITIALIZER IN DECLARATION" << std::endl;
    }
  }

  _symbols_to_declare.erase(symbol->name());

}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_nil_node(cdk::nil_node * const node, int lvl) {
  // EMPTY
}

void mml::postfix_writer::do_data_node(cdk::data_node * const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_not_node(cdk::not_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
  _pf.INT(0);
  _pf.EQ();
}

void mml::postfix_writer::do_and_node(cdk::and_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  std::string lbl = mklbl(++_lbl);
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JZ(lbl);
  node->right()->accept(this, lvl + 2);
  _pf.AND();
  _pf.ALIGN();
  _pf.LABEL(lbl);
}

void mml::postfix_writer::do_or_node(cdk::or_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  std::string lbl = mklbl(++_lbl);
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JNZ(lbl); 
  node->right()->accept(this, lvl + 2);
  _pf.OR();
  _pf.ALIGN();
  _pf.LABEL(lbl);
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_sequence_node(cdk::sequence_node * const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl + 2);
  }
}

void mml::postfix_writer::do_block_node(mml::block_node *const node, int lvl) {
  std::set<std::string> symbols;
  _symtab.push(); 
  if (node->declarations()) { 
    node->declarations()->accept(this, lvl + 4);
  }
  if (node->instructions()) {
    node->instructions()->accept(this, lvl + 4);
  }
  _symtab.pop();
}

void mml::postfix_writer::do_declaration_node(mml::declaration_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  
  auto id = node->identifier();
  int offset = 0, typesize = node->type()->size();

  if (_inFunctionArgs) {
    offset = _offset;
    _offset += typesize;
  } else if (_inFunctionBody) {
    _offset -= typesize;
    offset = _offset;
  } else {
    offset = 0;
  }

  std::shared_ptr<mml::symbol> symbol = new_symbol();
  if (symbol) {
    symbol->set_offset(offset);
    reset_new_symbol();
  }

  // Insert new symbol name into uninitialized identifiers set
  if (!_inFunctionArgs && !_inFunctionBody) {
    _symbols_to_declare.insert(symbol->name());
  }
 
  // If there is an initializer, process it
  if (node->initializer()) {
    do_init_expr(node->initializer(), lvl + 4, symbol);
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_integer_node(cdk::integer_node * const node, int lvl) {
  if (_inFunctionBody) {
    _pf.INT(node->value()); 
  } else {
    _pf.SINT(node->value()); 
  }
}

void mml::postfix_writer::do_double_node(cdk::double_node * const node, int lvl) {
  std::string lbl = mklbl(++_lbl);
  if (_inFunctionBody) {
    _pf.CALL(lbl);
    _pf.TEXT();
    _pf.ALIGN();
    _pf.LABEL(lbl);
    _pf.START();
  }

  if (_inFunctionBody) {
    _pf.DOUBLE(node->value());
  } else {
    _pf.SDOUBLE(node->value());
  }

  if (_inFunctionBody) {
    _pf.STFVAL64();
    _pf.LEAVE();
    _pf.RET();
    _pf.TEXT(_return_labels.back());
    _pf.LDFVAL64();
  }
}

void mml::postfix_writer::do_string_node(cdk::string_node * const node, int lvl) {
  /* generate the string */
  std::string lbl = mklbl(++_lbl);
  _pf.RODATA(); // strings are DATA readonly
  _pf.ALIGN(); // make sure we are aligned
  _pf.LABEL(lbl); // give the string a name
  _pf.SSTRING(node->value()); // output string characters

  if (_inFunctionBody) {
    // local variable initializer
    _pf.TEXT(_return_labels.back()); 
    _pf.ADDR(lbl); 
  } else {
    // global variable initializer
    _pf.DATA(); 
    _pf.SADDR(lbl); 
  }

}

void mml::postfix_writer::do_null_node(mml::null_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (_inFunctionBody) {
    _pf.INT(0);
  } else {
    _pf.SINT(0);
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_neg_node(cdk::neg_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
  _pf.NEG(); // 2-complement
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_add_node(cdk::add_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
    _pf.MUL();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->left()->is_typed(cdk::TYPE_INT)) {
    auto referenced = cdk::reference_type::cast(node->right()->type())->referenced();
    if (referenced->name() == cdk::TYPE_DOUBLE) {
      _pf.INT(8);
    } else {
      _pf.INT(4);
    }
    _pf.MUL();
  }

  node->right()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
    _pf.MUL();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    auto referenced = cdk::reference_type::cast(node->left()->type())->referenced();
    if (referenced->name() == cdk::TYPE_DOUBLE) {
      _pf.INT(8);
    } else {
      _pf.INT(4);
    }
    _pf.MUL();
  } 
  
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DADD();
  } else {
    _pf.ADD();
  }
}

void mml::postfix_writer::do_sub_node(cdk::sub_node * const node, int lvl) {
  node->left()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->type()->name() == cdk::TYPE_POINTER && node->right()->type()->name() == cdk::TYPE_INT) {
    auto referenced = cdk::reference_type::cast(node->right()->type())->referenced();
    if (referenced->name() == cdk::TYPE_DOUBLE) {
      _pf.INT(8);
    } else {
      _pf.INT(4);
    }
  }

  node->right()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    auto referenced = cdk::reference_type::cast(node->right()->type())->referenced();
    if (referenced->name() == cdk::TYPE_DOUBLE) {
      _pf.INT(8);
    } else {
      _pf.INT(4);
    }
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DSUB();
  } else {
    _pf.SUB();
  }
}

void mml::postfix_writer::do_mul_node(cdk::mul_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DMUL();
  } else {
    _pf.MUL();
  }
}

void mml::postfix_writer::do_div_node(cdk::div_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DDIV();
  } else {
    _pf.DIV();
  }
}

void mml::postfix_writer::do_mod_node(cdk::mod_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.MOD();
}

void mml::postfix_writer::do_lt_node(cdk::lt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  node->right()->accept(this, lvl + 2);
  if (node->right()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  _pf.LT();
}

void mml::postfix_writer::do_le_node(cdk::le_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  node->right()->accept(this, lvl + 2);
  if (node->right()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  _pf.LE();
}

void mml::postfix_writer::do_ge_node(cdk::ge_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  node->right()->accept(this, lvl + 2);
  if (node->right()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  _pf.GE();
}

void mml::postfix_writer::do_gt_node(cdk::gt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  node->right()->accept(this, lvl + 2);
  if (node->right()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  _pf.GT();
}

void mml::postfix_writer::do_ne_node(cdk::ne_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl + 2);
  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  node->right()->accept(this, lvl + 2);
  if (node->right()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  _pf.NE();
}

void mml::postfix_writer::do_eq_node(cdk::eq_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  node->right()->accept(this, lvl);
  if (node->right()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }
  _pf.EQ();
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_sizeof_node(mml::sizeof_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->expression()->accept(this, lvl + 4);
  if (_inFunctionBody) {
    if (node->expression()->is_typed(cdk::TYPE_DOUBLE)) {
      _pf.INT(8);
    } else {
      _pf.INT(4);
    }
  } else {
    if (node->expression()->is_typed(cdk::TYPE_DOUBLE)) {
      _pf.SINT(8);
    } else {
      _pf.SINT(4);
    }
  }  
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_variable_node(cdk::variable_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  const std::string &id = node->name();
  auto symbol = _symtab.find(id);
 
  if (symbol->is_foreign()) {
    _extern_label = symbol->name();
  } else if (symbol->global()) {
    _pf.ADDR(symbol->name());
  } else {
    _pf.LOCAL(symbol->offset());
  }
}

void mml::postfix_writer::do_index_node(mml::index_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->base()->accept(this, lvl + 4);
  node->index()->accept(this, lvl + 4);
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.INT(8);
  }
  else {
    _pf.INT(4);
  }
  _pf.MUL();
  _pf.ADD();
}

void mml::postfix_writer::do_rvalue_node(cdk::rvalue_node * const node, int lvl) {

  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl + 4);
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.LDDOUBLE();
  } else {
    if (_extern_label.empty()) {
      _pf.LDINT();
    }
  }
}

void mml::postfix_writer::do_assignment_node(cdk::assignment_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->rvalue()->accept(this, lvl + 4);
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    if (node->rvalue()->is_typed(cdk::TYPE_INT)) {
      _pf.I2D();
    }
    _pf.DUP64();
  } else {
    _pf.DUP32();
  }

  node->lvalue()->accept(this, lvl + 4);
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.STDOUBLE();
  } else {
    _pf.STINT();
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_evaluation_node(mml::evaluation_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl + 2); 
  _pf.TRASH(node->argument()->type()->size());
}

void mml::postfix_writer::do_input_node(mml::input_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (node->is_typed(cdk::TYPE_INT)) {
    _externals_functions.insert("readi");
    _pf.CALL("readi");
    _pf.LDFVAL32();
  }
  else if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _externals_functions.insert("readd");
    _pf.CALL("readd");
    _pf.LDFVAL64();
  } else {
    std::cerr << "CANNOT READ INPUT TYPE" << std::endl;
  }
}

void mml::postfix_writer::do_print_node(mml::print_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  for (size_t ix = 0; ix < node->arguments()->size(); ix++) {
    auto arg = dynamic_cast<cdk::expression_node*>(node->arguments()->node(ix));
    arg->accept(this, lvl + 2);
    if (arg->is_typed(cdk::TYPE_INT)) {
      _externals_functions.insert("printi");
      _pf.CALL("printi");
      _pf.TRASH(4);
    }
    else if (arg->is_typed(cdk::TYPE_DOUBLE)) {
      _externals_functions.insert("printd");
      _pf.CALL("printd");
      _pf.TRASH(8);
    }
    else if (arg->is_typed(cdk::TYPE_STRING)) {
      _externals_functions.insert("prints");
      _pf.CALL("prints");
      _pf.TRASH(4);
    }
    else {
      std::cerr << "cannot print expression of unknown type" << std::endl;
      return;
    }
  }
  if (node->newline()) {
    _externals_functions.insert("println");
    _pf.CALL("println");
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_address_of_node(mml::address_of_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl + 4);
}

void mml::postfix_writer::do_stack_alloc_node(mml::stack_alloc_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  auto referenced = cdk::reference_type::cast(node->type())->referenced();
  node->argument()->accept(this, lvl + 4);
  _pf.INT(referenced->size());
  _pf.MUL();
  _pf.ALLOC(); 
  _pf.SP(); 
}

void mml::postfix_writer::do_identity_node(mml::identity_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl + 2);
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_while_node(mml::while_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  _whileCond.push(++_lbl);
  _whileEnd.push(++_lbl);
 
  _symtab.push(); 
  _pf.ALIGN();
  _pf.LABEL(mklbl(_whileCond.top()));
  node->condition()->accept(this, lvl + 2);
  _pf.JZ(mklbl(_whileEnd.top()));
  node->block()->accept(this, lvl + 2);
  _pf.JMP(mklbl(_whileCond.top()));
  _pf.ALIGN();
  _pf.LABEL(mklbl(_whileEnd.top()));
  _symtab.pop();
  
  _whileEnd.pop();
  _whileCond.pop();
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_if_node(mml::if_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1;
  node->condition()->accept(this, lvl + 4);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->block()->accept(this, lvl + 4);
  _pf.LABEL(mklbl(lbl1));
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_if_else_node(mml::if_else_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1, lbl2;
  node->condition()->accept(this, lvl +  4);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->thenblock()->accept(this, lvl + 4);
  _pf.JMP(mklbl(lbl2 = ++_lbl));
  _pf.LABEL(mklbl(lbl1));
  node->elseblock()->accept(this, lvl + 4);
  _pf.LABEL(mklbl(lbl1 = lbl2));
}

void mml::postfix_writer::do_function_call_node(mml::function_call_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  std::vector<std::shared_ptr<cdk::basic_type>> inputTypes;

  if (node->identifier()) {   // NON RECURSIVE CASE
    inputTypes = cdk::functional_type::cast(node->identifier()->type())->input()->components();
  } else {                     // RECURSIVE CASE
    auto actualFun = _functions_symbols.back();
    inputTypes = cdk::functional_type::cast(actualFun->type())->input()->components();
  }

  size_t argsSize = 0;
  if (node->arguments()) {
    for (int ix = node->arguments()->size() - 1; ix >= 0; --ix) {
      auto arg = dynamic_cast<cdk::expression_node*>(node->arguments()->node(ix));
      arg->accept(this, lvl + 4);
      if (arg->is_typed(cdk::TYPE_DOUBLE) && inputTypes.at(ix)->name() == cdk::TYPE_INT) {
        _pf.D2I();
        argsSize += 8;
      }
      else if (arg->is_typed(cdk::TYPE_INT) && inputTypes.at(ix)->name() == cdk::TYPE_DOUBLE) {
        _pf.I2D();
        argsSize += 8;
      }
      else if (arg->is_typed(cdk::TYPE_DOUBLE)) {
        argsSize += 8;
      }
      else {
        argsSize += 4;
      }      
    }
  }

  if (node->identifier()) {  // NON RECURSIVE CASE -> get address value of the pointer and jump to it
    _extern_label.clear();
    node->identifier()->accept(this, lvl + 4);
    if (!_extern_label.empty()) {
      _pf.CALL(_extern_label);
    } else {
      _pf.BRANCH();
    }
  } else {  // RECURSIVE CASE -> call last pushed function label
    _pf.CALL(_return_labels.back());
  }

  if (argsSize != 0) {
    _pf.TRASH(argsSize);
  }

  if (!node->is_typed(cdk::TYPE_VOID)) {
    if (node->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.LDFVAL64();
    } else {
        _pf.LDFVAL32();
    }
  }

  _extern_label.clear();

}

void mml::postfix_writer::do_function_definition_node(mml::function_definition_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (node->main()) {
    std::shared_ptr<mml::symbol> symbol;
    for (std::string name : _symbols_to_declare) {
      auto symbol = _symtab.find(name, 0);
      if (symbol->is_foreign()) {
        _externals_functions.insert(name);
      } else {
        _pf.BSS();
        _pf.ALIGN();
        _pf.LABEL(name);
        if (symbol->is_typed(cdk::TYPE_DOUBLE))
          _pf.SALLOC(8);    
        else
          _pf.SALLOC(4);
      }
    }

    _symbols_to_declare.clear();
    
    symbol = new_symbol();
    _functions_symbols.push_back(symbol);
    reset_new_symbol();

    _return_labels.push_back("_main");
    
    _symtab.push(); 
    _pf.TEXT(_return_labels.back());
    _pf.ALIGN();
    _pf.GLOBAL("_main", _pf.FUNC());
    _pf.LABEL("_main");

    frame_size_calculator lsc(_compiler, _symtab, symbol);

    _symtab.push();
    node->accept(&lsc, lvl);
    _symtab.pop();
  
    _pf.ENTER(lsc.localsize());

    bool oldReturnBool = _returnBool;
    _returnBool = false;
    _inFunctionBody = true; 
    if (node->block()) {
      node->block()->accept(this, lvl + 4);
    }
    _inFunctionBody = false;

    _symtab.pop();
    
    _return_labels.pop_back();
    if (!_returnBool) {
      if (node->type()->name() == cdk::TYPE_DOUBLE) {
        _pf.INT(0);
        _pf.STFVAL64();
      } 
      else {
        _pf.INT(0);
        _pf.STFVAL32();
      }
    }

    _pf.LEAVE();
    _pf.RET();

    _functions_symbols.pop_back();
    for (std::string ext : _externals_functions) {
      _pf.EXTERN(ext);
    }
    _externals_functions.clear();
    _returnBool = oldReturnBool;
  
  } else {
    bool publicFun = false; 
    std::string funName; 
    auto symbol = new_symbol();

    if (symbol) {
      _functions_symbols.push_back(symbol);
      reset_new_symbol();
    } 
    
    _offset = 8;
    _symtab.push();

    if(node->arguments()) {
      _inFunctionArgs = true;
      for (size_t ix = 0; ix < node-> arguments()->size(); ix++){
        cdk::basic_node *argument = node->arguments()->node(ix);
        if (!argument ) break;
        argument->accept(this, 0);
      }
      _inFunctionArgs = false;
    }

    std::string lbl = mklbl(++_lbl);
    _return_labels.push_back(lbl);
    _pf.TEXT(_return_labels.back());
    _pf.ALIGN();
    if (publicFun) {
      _pf.GLOBAL(funName, _pf.FUNC());
    }
    _pf.LABEL(lbl);
    frame_size_calculator lsc(_compiler, _symtab, symbol);

    _symtab.push();
    node->accept(&lsc, lvl);
    _symtab.pop();
    
    _pf.ENTER(lsc.localsize());

    _offset = 0;

    bool _wasInFunctionBody = _inFunctionBody;
    _inFunctionBody = true;
    if (node->block()) {
      node->block()->accept(this, lvl + 4);
    }
  
    _inFunctionBody = _wasInFunctionBody;

    _symtab.pop();
  
    if (!_returnBool) {
      _pf.LEAVE();
      _pf.RET();
    }

    _return_labels.pop_back();
    if (symbol) {
      _functions_symbols.pop_back();
    }

    if (_inFunctionBody) {
      _pf.TEXT(_return_labels.back());
      _pf.ADDR(lbl);
    } 

    _function_label = lbl;
    }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_return_node(mml::return_node * const node, int lvl) {
   ASSERT_SAFE_EXPRESSIONS;
  _returnBool = true;
  auto actualFun = _functions_symbols.back();
  std::shared_ptr<cdk::basic_type> outputType = cdk::functional_type::cast(actualFun->type())->output(0);

  if (outputType->name() != cdk::TYPE_VOID) {
    node->retval()->accept(this, lvl + 4);
    if (outputType->name() == cdk::TYPE_INT) {
      if (!actualFun->is_main()) {
        _pf.I2D();
        _pf.STFVAL64();
      } else {
        _pf.STFVAL32();
      }
    } else if (outputType->name() == cdk::TYPE_STRING) {
      _pf.STFVAL32();
    } 
    else if (outputType->name() == cdk::TYPE_POINTER || outputType->name() == cdk::TYPE_FUNCTIONAL) {
      if (node->retval()->type()->name() == cdk::TYPE_DOUBLE) {
        _pf.STFVAL64();  
      }
      else {
        _pf.STFVAL32();
      }
    } else if (outputType->name() == cdk::TYPE_DOUBLE) {
      if (node->retval()->type()->name() == cdk::TYPE_INT) {
        _pf.I2D();
      }
      _pf.STFVAL64();
    }
    else {
      std::cerr << "UNKNOWN RETURN TYPE" << std::endl;
    }
  } 

  _pf.LEAVE();
  _pf.RET();
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_stop_node(mml::stop_node * const node, int lvl) {
  if (_whileCond.size() != 0) {
    _pf.JMP(mklbl(_whileEnd.top()));
  } else {
    std::cerr << "STOP INSTRUCTION OUTSIDE CYCLE" << std::endl;
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_next_node(mml::next_node * const node, int lvl) {
  if (_whileCond.size() != 0) {
    _pf.JMP(mklbl(_whileCond.top()));
  } else {
    std::cerr << "AGAIN INSTRUCTION OUTSIDE CYCLE" << std::endl;
  }
}