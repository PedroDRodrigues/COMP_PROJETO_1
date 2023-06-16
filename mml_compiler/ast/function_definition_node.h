#ifndef __MML_AST_FUNCTION_DEFINITION_H__
#define __MML_AST_FUNCTION_DEFINITION_H__

#include <string>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>
#include "ast/block_node.h"
#include "cdk/types/functional_type.h"

namespace mml {

  class function_definition_node: public cdk::expression_node {
    cdk::sequence_node *_arguments;
    mml::block_node *_block;
    bool _main;

  public:
    function_definition_node(int lineno, std::shared_ptr<cdk::basic_type> funType, cdk::sequence_node *arguments, mml::block_node *block, bool main) : 
        cdk::expression_node(lineno), _arguments(arguments), _block(block), _main(main) {
      type(funType);
    }

    function_definition_node(int lineno, std::shared_ptr<cdk::basic_type> funType, mml::block_node *block, bool main) :
        cdk::expression_node(lineno), _arguments(new cdk::sequence_node(lineno)), _block(block), _main(main) {
      type(funType);
    }

  public:
    cdk::sequence_node* arguments() {
      return _arguments;
    }

    cdk::typed_node* argument(size_t ax) {
      return dynamic_cast<cdk::typed_node*>(_arguments->node(ax));
    }

    mml::block_node* block() {
      return _block;
    }

    bool main() {
      return _main;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_definition_node(this, level);
    }

  };

} // mml

#endif
