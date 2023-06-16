#ifndef __MML_TARGETS_POSTFIX_WRITER_H__
#define __MML_TARGETS_POSTFIX_WRITER_H__

#include "targets/basic_ast_visitor.h"
#include "targets/symbol.h"
#include "cdk/symbol_table.h"
#include "cdk/types/functional_type.h"
#include <cdk/emitters/basic_postfix_emitter.h>

#include <set>
#include <sstream>
#include <vector>
#include <stack>

namespace mml {

  //!
  //! Traverse syntax tree and generate the corresponding assembly code.
  //!
  class postfix_writer: public basic_ast_visitor {
    cdk::symbol_table<mml::symbol> &_symtab;
    cdk::basic_postfix_emitter &_pf;
    int _lbl;
    bool _inFunctionBody;
    bool _inFunctionArgs;
    bool _returnBool;
    int _offset;
    std::vector<std::shared_ptr<mml::symbol>> _functions_symbols;
    std::vector<std::string> _return_labels;
    std::string _function_label;
    std::string _extern_label;
    std::set<std::string> _symbols_to_declare;
    std::set<std::string> _externals_functions;
    std::stack<int> _whileCond, _whileEnd;

  public:
    postfix_writer(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<mml::symbol> &symtab,
                   cdk::basic_postfix_emitter &pf) :
        basic_ast_visitor(compiler), _symtab(symtab), _pf(pf), _lbl(0),
        _inFunctionBody(false), _inFunctionArgs(false), _returnBool(false),  _offset(0) {
    }

  public:
    ~postfix_writer() {
      os().flush();
    }

  protected:
    void do_init_expr(cdk::expression_node * const node, int lvl, std::shared_ptr<mml::symbol> symbol);

  private:
    /** Method used to generate sequential labels. */
    inline std::string mklbl(int lbl) {
      std::ostringstream oss;
      if (lbl < 0)
        oss << ".L" << -lbl;
      else
        oss << "_L" << lbl;
      return oss.str();
    }

    std::string _functions_symbols_string() {
      std::ostringstream oss;
      oss << "<";
      for (size_t ix = 0; ix < _functions_symbols.size(); ix++) {
        oss << _functions_symbols[ix]->name() << ",";
      }
      oss << ">";
      return oss.str();
    }

    std::string _symbols_string() {
      std::ostringstream oss;
      oss << "<";
      for (std::string name : _symbols_to_declare) {
        oss << name << ",";
      }
      oss << ">";
      return oss.str();
    }

  public:
  // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h"       // automatically generated
#undef __IN_VISITOR_HEADER__
  // do not edit these lines: end

  };

} // mml

#endif
